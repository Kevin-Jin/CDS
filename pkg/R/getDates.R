tenorToMonths <- function(tenor.str) {
  # already in the right format
  if (all(!is.na(suppressWarnings(as.numeric(tenor.str)))))
    return(tenor.str)
  
  apply(stringr::str_match(tenor.str, "^(\\d+)([MY])$")[, -1, drop = FALSE], 1, function(tenor) {
    if (!any(is.na(tenor))) {
      factor <- if (tenor[2] == "Y") 12 else 1
      as.numeric(tenor[1]) * factor
    } else {
      NA
    }
  })
}

getStartDate <- function(maturity, endDate) {
    if (is.null(maturity) || is.null(endDate))
        return(NULL)

    startDate <- as.POSIXlt(endDate)
    startDate$mon <- startDate$mon - tenorToMonths(maturity) - 3
    startDate <- as.Date(startDate)
}

#' The function gets SNAC (IMM) coupon dates for a CDS contract.
#' 
#' @param TDate the trade date
#' @param maturity maturity of the CDS contract. Default "5Y".
#' @return a date frame with step-in date (T+1), value date (T+3
#' business days), start date (accrual begin date), end date
#' (maturity), backstop date (T-60 day look back from which
#' 'protection' is effective), pen coupon date (second to last coupon
#' date)
#' @export
#' @examples
#' getDates(as.Date("2014-05-07"), maturity = "5Y")
#' 

getDates <- function(TDate, maturity = "5Y", startDate = NULL, endDate = NULL){

    ## check maturity. Has to be "6M" of "NY" where N is an integer
    duration <- gsub("[[:digit:]]", "", maturity)
    if (!(duration %in% c("M", "Y"))) stop ("Maturity must end with 'M' or 'Y'")
    length <- as.numeric(gsub("[^[:digit:]]", "", maturity))
    
    ## TDate T
    dateWday <- as.POSIXlt(TDate)$wday
    if (any(!(dateWday %in% c(1:5)))) stop("TDate must be a weekday")

    ## stepinDate T + 1 bus day
    stepinDate <- .adjNextBusDay(TDate + 1)

    ## valueDate T + 3 bus day
    valueDate <- .adjNextBusDay(.adjNextBusDay(stepinDate + 1) + 1)
    
    ## startDate accrual date
    coalesce(startDate) <- getStartDate(maturity, endDate) %??% .getFirstAccrualDate(TDate)

    ## firstcouponDate the next IMM date approx after
    ## startDate
    firstcouponDate <- as.POSIXlt(startDate)
    firstcouponDate$mon <- firstcouponDate$mon + 3
    firstcouponDate <- as.Date(firstcouponDate)

    ## endDate firstcouponDate + maturity. IMM dates. No adjustment.
    endDate <- as.POSIXlt(firstcouponDate)
    if (duration == "M"){
        endDate$mon <- endDate$mon + length
    } else {
        endDate$year <- endDate$year + length
    }
    endDate <- as.Date(endDate)
    
    ## pencouponDate T + maturity - 1 accrual interval
    pencouponDate <- as.POSIXlt(endDate)
    pencouponDate$mon <- pencouponDate$mon - 3
    pencouponDate <- as.Date(pencouponDate)
    
    ## backstopDate T - 60
    backstopDate <- TDate - 60
    
    ## adjust dates to business day after first using them
	## to calculate the other dates
    startDate <- .adjNextBusDay(startDate)
    firstcouponDate <- .adjNextBusDay(firstcouponDate)
    pencouponDate <- .adjNextBusDay(pencouponDate)

    return(data.frame(TDate, stepinDate, valueDate, startDate,
                      firstcouponDate, pencouponDate, endDate, 
                      backstopDate))
    
}
