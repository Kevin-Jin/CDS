#' The function returns the deposits and swap rates for the day
#' input. The day input should be a weekday. If not, go to the most
#' recent weekday.
#'
#' Assume date and currency are in the same location.
#' 
#' @param date ideally a weekday. The rates for a trade date T are
#' published on T-1 weekday.
#' @param currency the three-letter currency code. As of now, it works
#' for USD, EUR, and JPY. The default is USD.
#' 
#' @return a list with two data frames. The first data frame contains
#' the rates based on the ISDA specifications; the second contains all
#' the dcc and calendar specifications of the curve.
#'
#' @export
#' @examples
#'
#' getRates(as.Date("2014-05-07"), currency = "USD")
#' 
getRates <- function(date = Sys.Date(), currency = "USD"){

    ## coerce into character and change to upper case
    stopifnot(toupper(as.character(currency)) %in% c( "USD", "GBP", "EUR",
"JPY", "CHF", "CAD" , "AUD", "NZD", "SGD", "HKD"))
    
    currency <- as.character(currency)
    date <- as.Date(date) - 1

    ## 0 is Sunday, 6 is Saturday
    dateWday <- as.POSIXlt(date)$wday
    ## change date to the most recent weekday if necessary
    date <- date - ifelse(dateWday == 0,
        2,
    ifelse(dateWday == 6,
        1,
    # else
        0
    ))
    
    dateInt <- as.numeric(format(date, "%Y%m%d"))
    ratesURLs <- paste("https://www.markit.com/news/InterestRates_",
                      currency, "_", dateInt, ".zip", sep ="")
    uniqueURLs <- unique(ratesURLs)
    indices <- unlist(lapply(ratesURLs, function(x) which(x == uniqueURLs)))
    rateInfos <- lapply(uniqueURLs, function(ratesURL) {
    tryCatch({
        xmlParsedIn <- .downloadRates(ratesURL)

        if (class(xmlParsedIn)[1] == "character"){
            return(xmlParsedIn)
        } else {
            
            rates <- xmlSApply(xmlParsedIn, function(x) xmlSApply(x, xmlValue))
            
            curveRates <- c(rates$deposits[names(rates$deposits) == "curvepoint"],
                            rates$swaps[names(rates$swaps) == "curvepoint"])
            
            if (length(curveRates) != 0) {
              # each element in curveRates is a smooshed string because we don't
              # want to recurse to a third level of xmlSApply in order to simplify
              # logic with calendars. this regex extracts tenor, maturitydate, and parrate
              df <- data.frame(matrix(str_match(curveRates, "^(\\d+[MY])(\\d{4}-\\d{2}-\\d{2})(\\d+\\.?\\d*|\\.\\d+)$")[, -1], ncol = 3))
              colnames(df) <- c("expiry", "matureDate", "rate")
              ratesDf <- cbind(df, type = c(rep("M", sum(names(rates$deposits) == "curvepoint")),
                                            rep("S", sum(names(rates$swaps) == "curvepoint"))))
            } else {
              # some holidays, e.g. MLK Jr day + 1, George Washington day + 0,
              # Independence Day + 1, Labor Day + 1, Columbus Day + 1,
              # Veterans Day + 1, Thanksgiving + 1 in 2005 and some in 2006 are
              # missing curve points
              ratesDf <- data.frame(expiry = NULL, matureDate = NULL, rate = NULL, type = NULL)
            }
            dccDf <- data.frame(effectiveDate = rates$effectiveasof[[1]],
                                badDayConvention = rates$baddayconvention,
                                mmDCC = rates$deposits[['daycountconvention']],
                                mmCalendars = rates$deposits[['calendars']],
                                # InterestRates_USD_20090415.xml and InterestRates_USD_20090414.xml
                                fixedDCC = rates$swaps[[if ('fixeddaycountconvention' %in% names(rates$swaps)) 'fixeddaycountconvention' else 'daycountconvention']],
                                floatDCC = rates$swaps[[if ('floatingdaycountconvention' %in% names(rates$swaps)) 'floatingdaycountconvention' else 'daycountconvention']],
                                fixedFreq = rates$swaps[['fixedpaymentfrequency']],
                                floatFreq = rates$swaps[['floatingpaymentfrequency']],
                                swapCalendars = rates$swaps[['calendars']])
            
            # InterestRates_USD_20071129.xml fat finger.
            # appears that 4 and 8 are transposed in .084648 since 15Y is 4.81% and 30Y is 4.92%
            if (as.character(dccDf$effectiveDate) == "2007-11-30")
              levels(ratesDf$rate)[ratesDf[as.character(ratesDf$expiry) == "20Y", "rate"]] <- 0.048648
            
            return(list(ratesDf, dccDf))
        }
    }, error = function(e) { print(paste(ratesURL, e, sep = ": ")) })
    })[indices]
    return(rateInfos)
}

# cluster = NULL means download each file sequentially. cluster = NA means
# use the default cluster, or create one if the default cluster does not exist.
# Otherwise, pass cluster to parLapply.
serializeRatesCsv <- function(dates = Sys.Date(), toFile = NULL, existingRates = NULL, cluster = if (length(dates) < 16) NULL else NA, currency = "USD") {
  uniqueDates <- unique(dates)
  indices <- unlist(lapply(dates, function(x) which(x == uniqueDates)))
  
  if (is.null(cluster)) {
    applyFun <- lapply
  } else {
    applyFun <- function(x, FUN) {
      library(parallel)
      makeOwnCluster <- is.na(cluster)
      if (makeOwnCluster)
        cluster <- makePSOCKcluster(min(length(uniqueDates), getOption("cl.cores", detectCores()) * 2))
      clusterExport(cluster, c("currency"), envir = environment())
      #clusterEvalQ(cluster, library(CDS))
      result <- parLapply(cluster, x, FUN)
      if (makeOwnCluster)
        stopCluster(cluster)
      return(result)
    }
  }
  
  df <- applyFun(uniqueDates, function(date) {
    ratesInfo <- getRates(date = date, currency = currency)
    stopifnot(length(ratesInfo) == 1)
    ratesInfo <- ratesInfo[[1]]
    if (class(ratesInfo) == "character"){
      warning(ratesInfo)
      types <- NA
      rates <- NA
      expiries <- NA
      
      mmDCC <- NA
      fixedSwapFreq <- NA
      floatSwapFreq <- NA
      fixedSwapDCC <- NA
      floatSwapDCC <- NA
      badDayConvZC <- NA
      holidays <- NA
    } else {
      types <- paste(as.character(ratesInfo[[1]]$type), collapse = "")
      rates <- paste(as.numeric(as.character(ratesInfo[[1]]$rate)), collapse = ";")
      expiries <- paste(as.character(ratesInfo[[1]]$expiry), collapse = ";")
      
      mmDCC <- as.character(ratesInfo[[2]]$mmDCC)
      fixedSwapFreq <- as.character(ratesInfo[[2]]$fixedFreq)
      floatSwapFreq <- as.character(ratesInfo[[2]]$floatFreq)
      fixedSwapDCC <- as.character(ratesInfo[[2]]$fixedDCC)
      floatSwapDCC <- as.character(ratesInfo[[2]]$floatDCC)
      badDayConvZC <- as.character(ratesInfo[[2]]$badDayConvention)
      holidays <- as.character(ratesInfo[[2]]$swapCalendars)
    }
    return(c(date = date, types = types, rates = rates, expiries = expiries, mmDCC = mmDCC, fixedSwapFreq = fixedSwapFreq, floatSwapFreq = floatSwapFreq, fixedSwapDCC = fixedSwapDCC, floatSwapDCC = floatSwapDCC, badDayConvZC = badDayConvZC, holidays = holidays))
  })[indices]
  
  df <- as.data.frame(matrix(unlist(df), byrow = TRUE, nrow = length(df), dimnames = list(NULL, names(df[[1]]))), stringsAsFactors = FALSE)
  df$date <- as.Date(as.numeric(df$date), origin = "1970-01-01")
  df <- rbind(existingRates, df)
  if (is.null(toFile)) {
    #return(paste(capture.output(write.csv(df, stdout(), row.names = FALSE, quote = TRUE)), collapse = "\n"))
	return(df)
  } else {
    write.csv(df, toFile, row.names = FALSE, quote = TRUE)
    return(df)
  }
}

deserializeRatesCsv <- function(fromFile) {
  if (!file.exists(fromFile))
    return(NULL)
  df <- read.csv(fromFile, stringsAsFactors = FALSE)
  df$date <- as.Date(df$date)
  return(df)
}
