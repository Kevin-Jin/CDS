#' Calculates the expected present value of 1bp paid on the premium leg until default or maturity, AKA risky duration.
#'
#' For all parSpread != coupon and isPriceClean == TRUE, we expect that the two following conditions are true:
#' RPV01(TDate, parSpread) exactly equals (upfront(TDate, parSpread, coupon, notional) / (parSpread - coupon) / notional * 1e4) up to a machine precision
#' RPV01(TDate, parSpread)  approximates  ((price(upfront(TDate, parSpread + 1, coupon, notional), notional) / price(upfront(TDate, parSpread, coupon, notional), notional) - 1) * -1e4)
RPV01 <- function(TDate,
                    baseDate = TDate,
                    currency = "USD",

                    zeroCurve = NULL,

                    types = NULL,
                    rates = NULL,
                    expiries = NULL,
                    mmDCC = "ACT/360",
                    fixedSwapFreq = "6M",
                    floatSwapFreq = "3M",
                    fixedSwapDCC = "30/360",
                    floatSwapDCC = "ACT/360",
                    badDayConvZC = "M",
                    holidays = "None",
                    
                    valueDate = NULL,
                    benchmarkDate = NULL,
                    startDate = NULL,
                    endDate = NULL,
                    stepinDate = NULL,
                    maturity = "5Y",
                    
                    dccCDS = "ACT/360",
                    freqCDS = "1Q",
                    stubCDS = "F",
                    badDayConvCDS = "F",
                    calendar = "None",
                    
                    parSpread,
                    recoveryRate = 0.4,
                    isPriceClean = FALSE,
                    payAccruedOnDefault = TRUE){
    # RPV01 doesn't depend on coupon, and premium leg PV function seems to dislike coupons other than 100
    coupon <- 100
    # saves us from having to divide premium leg PV by notional
    notional <- 1

    ratesDate <- baseDate
    cdsDates <- getDates(as.Date(TDate), maturity, startDate, endDate)

    baseDate <- .separateYMD(baseDate)
    today <- .separateYMD(TDate)
    valueDate <- .separateYMD(valueDate %??% cdsDates$valueDate)
    benchmarkDate <- .separateYMD(benchmarkDate %??% cdsDates$startDate)
    startDate <- .separateYMD(startDate %??% cdsDates$startDate)
    endDate <- .separateYMD(endDate %??% cdsDates$endDate)
    stepinDate <- .separateYMD(stepinDate %??% cdsDates$stepinDate)

    zeroCurveMatches <- match(ratesDate, zeroCurve$date)
    if (!any(is.na(zeroCurveMatches))) {
        zeroCurve <- zeroCurve[zeroCurveMatches, ]
        coalesce(types) <- zeroCurve$types
        coalesce(rates) <- as.numeric(unlist(strsplit(zeroCurve$rates, split = ";")))
        coalesce(expiries) <- unlist(strsplit(zeroCurve$expiries, split = ";"))
        coalesce(mmDCC) <- zeroCurve$mmDCC
        coalesce(fixedSwapFreq) <- zeroCurve$fixedSwapFreq
        coalesce(floatSwapFreq) <- zeroCurve$floatSwapFreq
        coalesce(fixedSwapDCC) <- zeroCurve$fixedSwapDCC
        coalesce(floatSwapDCC) <- zeroCurve$floatSwapDCC
        coalesce(badDayConvZC) <- zeroCurve$badDayConvZC
        coalesce(holidays) <- zeroCurve$holidays
    }
    if (is.null(types) || is.null(rates) || is.null(expiries)) {
        # want to concatenate to initially empty vectors in the loop below
        types <- NULL
        rates <- NULL
        expiries <- NULL
        mmDCC <- NULL
        fixedSwapFreq <- NULL
        floatSwapFreq <- NULL
        fixedSwapDCC <- NULL
        floatSwapDCC <- NULL
        badDayConvZC <- NULL
        holidays <- NULL
        
        ratesInfos <- getRates(date = ratesDate, currency = as.character(currency))
        for (i in 1:length(ratesInfos)) {
            ratesInfo <- ratesInfos[[i]]
            if (class(ratesInfo) == "character"){
                warning(ratesInfo)
                types <- c(types, NA)
                
                mmDCC <- c(mmDCC, NA)
                fixedSwapFreq <- c(fixedSwapFreq, NA)
                floatSwapFreq <- c(floatSwapFreq, NA)
                fixedSwapDCC <- c(fixedSwapDCC, NA)
                floatSwapDCC <- c(floatSwapDCC, NA)
                badDayConvZC <- c(badDayConvZC, NA)
                holidays <- c(holidays, NA)
            } else {
                types <- c(types, paste(as.character(ratesInfo[[1]]$type), collapse = ""))
                rates <- c(rates, as.numeric(as.character(ratesInfo[[1]]$rate)))
                expiries <- c(expiries, as.character(ratesInfo[[1]]$expiry))
                
                mmDCC <- c(mmDCC, as.character(ratesInfo[[2]]$mmDCC))
                fixedSwapFreq <- c(fixedSwapFreq, as.character(ratesInfo[[2]]$fixedFreq))
                floatSwapFreq <- c(floatSwapFreq, as.character(ratesInfo[[2]]$floatFreq))
                fixedSwapDCC <- c(fixedSwapDCC, as.character(ratesInfo[[2]]$fixedDCC))
                floatSwapDCC <- c(floatSwapDCC, as.character(ratesInfo[[2]]$floatDCC))
                badDayConvZC <- c(badDayConvZC, as.character(ratesInfo[[2]]$badDayConvention))
                holidays <- c(holidays, as.character(ratesInfo[[2]]$swapCalendars))
            }
        }
    }
    # vector length checks for parallel arrays
    correctLengths <- function() all.equal(length(rates), length(expiries), if (length(types) > 0) sum(nchar(types[!is.na(types)])) else 0)
    stopifnot(correctLengths())
    # we'll just recycle mmDCC, fixedSwapFreq, floatSwapFreq, fixedSwapDCC, floatSwapDCC, badDayConvZC, holidays
    # in the C code if they're not all equal in length to length(ratesDate), but it's usually an error if
    # their lengths are greater than length(ratesDate) or not some factor of length(ratesDate) because some values will
    # be unused, so we should let the user know
    numDates <- length(ratesDate)
    warnSuspectLengths <- function(otherVector, desc) {
        if (length(otherVector) > numDates) warning(paste("Passed in only", numDates, "dates for zero curve but", length(otherVector), "for", desc))
        else if (numDates %% length(otherVector) != 0) warning(paste("Passed in", numDates, "dates for zero curve but only", length(otherVector), "for", desc))
    }
    warnSuspectLengths(types, "instrument types")
    warnSuspectLengths(mmDCC, "instrument day count conventions")
    warnSuspectLengths(fixedSwapFreq, "fixed leg frequencies")
    warnSuspectLengths(floatSwapFreq, "fixed leg frequencies")
    warnSuspectLengths(fixedSwapDCC, "fixed leg day count conventions")
    warnSuspectLengths(floatSwapDCC, "float leg day count conventions")
    warnSuspectLengths(badDayConvZC, "bad day conventions")
    warnSuspectLengths(holidays, "holidays")
    
    .Call('calcRpv01',
          baseDate,
          types,
          rates,
          expiries,

          mmDCC,
          fixedSwapFreq,
          floatSwapFreq,
          fixedSwapDCC,
          floatSwapDCC,
          badDayConvZC,
          holidays,
          
          today,
          valueDate,
          benchmarkDate,
          startDate,
          endDate,
          stepinDate,
          
          dccCDS,
          freqCDS,
          stubCDS,
          badDayConvCDS,
          calendar,
          
          parSpread,
          coupon,
          recoveryRate,
          isPriceClean,
          payAccruedOnDefault,
          notional,
          PACKAGE = "CDS")

}
