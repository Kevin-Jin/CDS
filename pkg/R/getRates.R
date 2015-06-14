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
        xmlParsedIn <- .downloadRates(ratesURL)

        if (class(xmlParsedIn)[1] == "character"){
            return(xmlParsedIn)
        } else {
            
            rates <- xmlSApply(xmlParsedIn, function(x) xmlSApply(x, xmlValue))
            
            curveRates <- c(rates$deposits[names(rates$deposits) == "curvepoint"],
                            rates$swaps[names(rates$swaps) == "curvepoint"])
            
            
            df <- do.call(rbind, strsplit(curveRates, split = "[MY]", perl = TRUE))
            rownames(df) <- NULL
            df <- cbind(df, "Y")
            df[1: (max(which(df[,1] == 1)) - 1), 3] <- "M"
            
            ratesDf <- data.frame(expiry = paste(df[,1], df[,3], sep = ""),
                                  matureDate = substring(df[,2], 0, 10),
                                  rate = substring(df[,2], 11),
                                  type = c(rep("M", sum(names(rates$deposits) == "curvepoint")),
                                      rep("S", sum(names(rates$swaps) == "curvepoint"))))
            dccDf <- data.frame(effectiveDate = rates$effectiveasof[[1]],
                                badDayConvention = rates$baddayconvention,
                                mmDCC = rates$deposits[['daycountconvention']],
                                mmCalendars = rates$deposits[['calendars']],
                                fixedDCC = rates$swaps[['fixeddaycountconvention']],
                                floatDCC = rates$swaps[['floatingdaycountconvention']],
                                fixedFreq = rates$swaps[['fixedpaymentfrequency']],
                                floatFreq = rates$swaps[['floatingpaymentfrequency']],
                                swapCalendars = rates$swaps[['calendars']])
            
            return(list(ratesDf, dccDf))
        }
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
