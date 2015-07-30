#' Interpolate the spread of a constant maturity CDS contract over a set
#' of dates given the term structure of CDS spreads for a set of several
#' maturities on each of those dates. Useful for comparing prices of a
#' CDS index to those of the on-the-run single name contracts of its
#' constituents. This routine does not discount CDS prices to account
#' for loss of liquidity premium on IMM roll dates. Even without the
#' liquidity factor, prices calculated from synthetic spreads may not
#" average to CDS index levels because of differences in restructuring
#' clauses between underlying index contracts and single name contracts.
#'
#' @param parSpread term structure of CDS par spread in bps. Each
#' row name must be a rolling maturity counted in months. Use
#' rownames(parSpreads) <- getTenorMonths(rownames(parSpreads)) to
#' assist you in converting Y and M strings. Each column name must be
#' when the trade is executed, AKA TDate, in the form YYYY-MM-DD. Use
#' colnames(parSpreads) <- format(as.Date(colnames(parSpreads)), %Y-%m-%d)
#' to assist you in converting other date formats.
#' @param baseDate is the start date for the IR curve. Default is
#' colnames(parSpread). 
#' @param currency in which CDS is denominated. 
#' @param zeroCurve is a data frame that represents the LIBOR curve.
#' @param types is a string indicating the names of the instruments
#' used for the yield curve. 'M' means money market rate; 'S' is swap
#' rate.
#' @param rates is an array of numeric values indicating the rate of
#' each instrument.
#' @param expiries is an array of characters indicating the maturity
#' of each instrument.
#' @param mmDCC is the day count convention of the instruments.
#' @param fixedSwapFreq is the frequency of the fixed rate of swap
#' being paid.
#' @param floatSwapFreq is the frequency of the floating rate of swap
#' being paid.
#' @param fixedSwapDCC is the day count convention of the fixed leg.
#' @param floatSwapDCC is the day count convention of the floating leg.
#' @param badDayConvZC is a character indicating how non-business days
#' are converted.
#' @param holidays is an input for holiday files to adjust to business
#' days.
#' @param valueDate is the date for which the present value of the CDS
#' is calculated. aka cash-settle date. The default is T + 3.
#' @param startDate is when the CDS nomially starts in terms of
#' premium payments, i.e. the number of days in the first period (and
#' thus the amount of the first premium payment) is counted from this
#' date. aka accrual begin date.
#' @param endDate aka maturity date. This is when the contract expires
#' and protection ends. Any default after this date does not trigger a
#' payment.
#' @param stepinDate default is T + 1.
#' @param maturity constant maturity added to startDate, to use in
#' transforming spreads.
#' @param dccCDS day count convention of the CDS. Default is ACT/360.
#' @param freqCDS date interval of the CDS contract.
#' @param stubCDS is a character indicating the presence of a stub.
#' @param badDayConvCDS refers to the bay day conversion for the CDS
#' coupon payments. Default is "F", following.
#' @param calendar refers to any calendar adjustment for the CDS.
#' @param coupon quoted in bps. It specifies the payment amount from
#' the protection buyer to the seller on a regular basis. The default
#' is 100 bps.
#' @param recoveryRate in decimal. Default is 0.4.
#' @param payAccruedOnDefault is a partial payment of the premium made
#' to the protection seller in the event of a default. Default is
#' \code{TRUE}.
#' @return a numeric indicating the amount of upfront payments from a
#' protection buyer's perspective.
#' @export
#' 
#' @examples
#' upf <- upfront(baseDate = "2014-01-13", currency = "USD", TDate
#' = "2014-01-14", maturity = "5Y", dccCDS = "ACT/360", freqCDS = "Q",
#' stubCDS = "F", badDayConvCDS = "F", calendar = "None", parSpread =
#' 32, coupon = 100, recoveryRate = 0.4, isPriceClean = FALSE,
#' notional = 1e7)
#' 

rollingToConstantMaturity <- function(parSpread,
        baseDate = NULL,

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
        startDate = NULL,
        endDate = NULL,
        stepinDate = NULL,
        maturity = "5Y",
        
        dccCDS = "ACT/360",
        freqCDS = "1Q",
        stubCDS = "F",
        badDayConvCDS = "F",
        calendar = "None",
        
        coupon = 100,
        recoveryRate = 0.4,
        payAccruedOnDefault = TRUE){

    TDate = colnames(parSpread)
    ratesDate <- baseDate
    cdsDates <- getDates(TDate = as.Date(TDate), maturity = maturity, startDate = startDate)

    baseDate <- .separateYMD(baseDate)
    today <- .separateYMD(TDate)
    valueDate <- .separateYMD(valueDate %??% cdsDates$valueDate)
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
    numericRowNames <- function() suppressWarnings(!is.na(as.numeric(rownames(parSpread))))
    numericColNames <- function() suppressWarnings(!is.na(as.numeric(colnames(parSpread))))
    stopifnot(correctLengths())
    stopifnot(all(numericRowNames(), numericColNames()))
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
    
    .Call('calcConstantMaturity',
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
          payAccruedOnDefault,
          PACKAGE = "CDS")

}
