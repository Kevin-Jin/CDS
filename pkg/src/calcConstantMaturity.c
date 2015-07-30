#include <R.h>
#define USE_RINTERNALS
#include <Rdefines.h>
#include <Rinternals.h>
#include "utilities.h"
#include "version.h"
#include "macros.h"
#include "convert.h"
#include "zerocurve.h"
#include "cxzerocurve.h"
#include "dateconv.h"
#include "date_sup.h"
#include "busday.h"
#include "ldate.h"
#include "cdsone.h"
#include "cds.h"
#include "cerror.h"
#include "rtbrent.h"
#include "tcurve.h"

#define NEW_ARRAY1(t,n)          (t *) JpmcdsMallocSafe(sizeof(t)*(n))
//override JP Morgan's ISDA CDS standard model cerror.c with R's error handling
#define JpmcdsErrMsg(...) warning(__VA_ARGS__)

// can we use getNextBusDate() ??
int adjNextBusDay(TMonthDayYear* today, TDate* temp) {
  long dayOfWeek;

  if (JpmcdsNormalizeMDY(today) != SUCCESS)
    return FAILURE;
  if (JpmcdsMDYToDate(today, temp) != SUCCESS)
    return FAILURE;
  if (JpmcdsDayOfWeek(*temp, &dayOfWeek) != SUCCESS)
    return FAILURE;

  switch (dayOfWeek) {
    case 0: // Sunday
      today->day += 1;
      break;
    case 6: // Saturday
      today->day += 2;
      break;
  }

  return SUCCESS;
}

int getContractEndDate(long year, long month, long day, int maturityMonths, TDate* result) {
  TMonthDayYear today;
  today.year = year;
  today.month = month;
  today.day = day;

  // getDates()#startDate <- .getFirstAccrualDate()
  if (today.month % 3 == 0) {
    if (today.day  < 20)
      today.month -= 3;
  } else {
    today.month -= today.month % 3;
  }
  today.day = 20;
  if (adjNextBusDay(&today, result) != SUCCESS)
    return FAILURE;

  // getDates()#firstcouponDate
  today.month += 3;
  if (adjNextBusDay(&today, result) != SUCCESS)
    return FAILURE;

  // getDates()#endDate
  today.month += maturityMonths;
  if (JpmcdsNormalizeMDY(&today) != SUCCESS)
    return FAILURE;
  if (JpmcdsMDYToDate(&today, result) != SUCCESS)
    return FAILURE;

  return SUCCESS;
}

/*
***************************************************************************
** Interpolate constant maturity spread.
***************************************************************************
*/

//EXPORT int JpmcdsCdsoneUpfrontCharge(cdsone.c)
SEXP calcConstantMaturity
(SEXP baseDate_input,  /* (I) Value date  for zero curve       */
 SEXP types, /* "MMMMMSSSSSSSSS"*/
 SEXP rates, /* rates[14] = {1e-9, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9,
		1e-9, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9};/\* (I)
		Array of swap rates *\/ */
 SEXP expiries,
 SEXP mmDCC,          /* (I) DCC of MM instruments            */

 SEXP fixedSwapFreq,   /* (I) Fixed leg freqency/interval               */
 SEXP floatSwapFreq,   /* (I) Floating leg freqency/interval            */
 SEXP fixedSwapDCC,    /* (I) DCC of fixed leg                 */
 SEXP floatSwapDCC,    /* (I) DCC of floating leg              */
 SEXP badDayConvZC, //'M'  badDayConv for zero curve
 SEXP holidays,//'None'

 // input for upfront charge calculation
 SEXP todayDate_input, /*today: T (Where T = trade date)*/
 SEXP valueDate_input, /* value date: T+3 Business Days*/
 SEXP startDate_input,/* start date for internal
				     ** clean spread bootstrapping;
				     ** accrual Begin Date  */
 SEXP endDate_input,/*  Maturity (Fixed) */
 SEXP stepinDate_input,  /* T + 1*/
 
 SEXP dccCDS, 			/* accruedDcc */
 SEXP ivlCDS,
 SEXP stubCDS,
 SEXP badDayConvCDS,
 SEXP calendar,

 SEXP parSpread,
 SEXP recoveryRate,
 SEXP payAccruedOnDefault_input) 

{
  //  static char routine[] = "JpmcdsCdsoneUpfrontCharge";

  // my vars
  int resultLen, numInstruments, i, j, k, gc_protected, baseDateLen,
    typesLen, ratesLen, expiriesLen, mmDCCLen,
    fixedSwapFreqLen, floatSwapFreqLen, fixedSwapDCCLen, floatSwapDCCLen, badDayConvZCLen, holidaysLen,
    valueDateLen, startDateLen, endDateLen, stepinDateLen,
    dccCDSLen, ivlCDSLen, stubCDSLen, badDayConvCDSLen, calendarLen,
    parSpreadLen, recoveryRateLen, payAccruedOnDefaultLen;
  TDate baseDate, today, startDate, endDate, stepinDate,valueDate;
  //TMonthDayYear tempMDY;
  int payAccruedOnDefault;
  SEXP syntheticSpread, is_na_at;
  TCurve **discCurve;
  char* pt_types;
  char* pt_holidays;
  char* pt_mmDCC;
  char* pt_fixedSwapDCC;
  char* pt_floatSwapDCC;
  char* pt_fixedSwapFreq;
  char* pt_floatSwapFreq;
  char* pt_dccCDS;
  char* pt_ivlCDS;
  char* pt_stubCDS;
  char* pt_calendar;
  char* pt_badDayConvCDS;

  // new
  char *pt_badDayConvZC;
  double *parSpread_for_upf, recoveryRate_for_upf, *syntheticSpreads, *result;

  static char *routine = "CalcUpfrontCharge";
  TDateInterval ivl;
  TStubMethod stub;
  long dcc;

  TDateInterval fixedSwapIvl_curve;
  TDateInterval floatSwapIvl_curve;
  long          fixedSwapDCC_curve;
  long          floatSwapDCC_curve;
  double        fixedSwapFreq_curve;
  double        floatSwapFreq_curve;

  long mmDCC_zc_main;
  static char  *routine_zc_main = "BuildExampleZeroCurve";

  TDate *dates_main;
  TDateInterval tmp;

  SEXP rownames, colnames;
  R_len_t nrow, numMaturities;
  int *monthsMaturity;
  TDate *spreadCurveDates;
  long year, month, day;

  typesLen = length(types);
  ratesLen = length(rates);
  expiriesLen = length(expiries);
  mmDCCLen = length(mmDCC);
  fixedSwapFreqLen = length(fixedSwapFreq);
  floatSwapFreqLen = length(floatSwapFreq);
  fixedSwapDCCLen = length(fixedSwapDCC);
  floatSwapDCCLen = length(floatSwapDCC);
  badDayConvZCLen = length(badDayConvZC);
  holidaysLen = length(holidays);

  baseDateLen = length(baseDate_input) / 3;
  //todayDateLen = length(todayDate_input) / 3;
  valueDateLen = length(valueDate_input) / 3;
  startDateLen = length(startDate_input) / 3;
  endDateLen = length(endDate_input) / 3;
  stepinDateLen = length(stepinDate_input) / 3;

  dccCDSLen = length(dccCDS);
  ivlCDSLen = length(ivlCDS);
  stubCDSLen = length(stubCDS);
  badDayConvCDSLen = length(badDayConvCDS);
  calendarLen = length(calendar);

  parSpreadLen = length(parSpread);
  recoveryRateLen = length(recoveryRate);
  payAccruedOnDefaultLen = length(payAccruedOnDefault_input);

  gc_protected = 0;
  rownames = VECTOR_ELT(getAttrib(parSpread, R_DimNamesSymbol), 0);
  colnames = VECTOR_ELT(getAttrib(parSpread, R_DimNamesSymbol), 1);
  nrow = INTEGER(getAttrib(parSpread, R_DimSymbol))[0];
  monthsMaturity = NEW_ARRAY1(int, nrow);
  spreadCurveDates = NEW_ARRAY1(TDate, nrow);
  resultLen = INTEGER(getAttrib(parSpread, R_DimSymbol))[1];
  result = NEW_ARRAY1(double, resultLen);
  for (i = 0; i < resultLen; ++i)
    result[i] = NA_REAL;

  baseDate_input = PROTECT(coerceVector(baseDate_input,INTSXP));
  gc_protected++;
  types = PROTECT(coerceVector(types, STRSXP));
  gc_protected++;
  rates = PROTECT(coerceVector(rates, REALSXP));
  gc_protected++;
  expiries = PROTECT(coerceVector(expiries, VECSXP));
  gc_protected++;
  mmDCC = PROTECT(coerceVector(mmDCC, STRSXP));
  gc_protected++;

  fixedSwapFreq = PROTECT(coerceVector(fixedSwapFreq, STRSXP));
  gc_protected++;
  floatSwapFreq = PROTECT(coerceVector(floatSwapFreq, STRSXP));
  gc_protected++;
  fixedSwapDCC = PROTECT(coerceVector(fixedSwapDCC, STRSXP));
  gc_protected++;
  floatSwapDCC = PROTECT(coerceVector(floatSwapDCC, STRSXP));
  gc_protected++;
  badDayConvZC = PROTECT(coerceVector(badDayConvZC, STRSXP));
  gc_protected++;
  holidays = PROTECT(coerceVector(holidays, STRSXP));
  gc_protected++;

  //todayDate_input = PROTECT(coerceVector(todayDate_input,INTSXP));
  //gc_protected++;
  valueDate_input = PROTECT(coerceVector(valueDate_input,INTSXP));
  gc_protected++;
  startDate_input = PROTECT(coerceVector(startDate_input,INTSXP));
  gc_protected++;
  endDate_input = PROTECT(coerceVector(endDate_input,INTSXP));
  gc_protected++;
  stepinDate_input = PROTECT(coerceVector(stepinDate_input,INTSXP));
  gc_protected++;

  dccCDS = PROTECT(coerceVector(dccCDS, STRSXP));
  gc_protected++;
  ivlCDS = PROTECT(coerceVector(ivlCDS, STRSXP));
  gc_protected++;
  stubCDS = PROTECT(coerceVector(stubCDS, STRSXP));
  gc_protected++;
  badDayConvCDS = PROTECT(coerceVector(badDayConvCDS, STRSXP));
  gc_protected++;
  calendar = PROTECT(coerceVector(calendar, STRSXP));
  gc_protected++;

  PROTECT(is_na_at = is_na(baseDate_input));
  gc_protected++;
  discCurve = NEW_ARRAY1(TCurve*, baseDateLen); // LIBOR discount curve

  if (LOGICAL(is_na_at)[baseDateLen])
    // vector is entirely NA
    goto done;
  for (j = 0; j < nrow; j++)
    if (sscanf(CHAR(STRING_ELT(rownames, j)), "%d", &monthsMaturity[j]) != 1)
      goto done;
  
  k = 0;
  for (i = 0; i < baseDateLen; i++)
  {
    if (LOGICAL(is_na_at)[0 * baseDateLen + i] || LOGICAL(is_na_at)[1 * baseDateLen + i] || LOGICAL(is_na_at)[2 * baseDateLen + i] || STRING_ELT(types, i % typesLen) == NA_STRING)
    {
      discCurve[i] = NULL;
      continue;
    }
    
    pt_mmDCC = (char *) CHAR(STRING_ELT(mmDCC, i % mmDCCLen));
    if (JpmcdsStringToDayCountConv(pt_mmDCC, &mmDCC_zc_main) != SUCCESS)
      goto done;
    
    pt_fixedSwapFreq = (char *) CHAR(STRING_ELT(fixedSwapFreq, i % fixedSwapFreqLen));
    if (JpmcdsStringToDateInterval(pt_fixedSwapFreq, routine_zc_main, &fixedSwapIvl_curve) != SUCCESS)
      goto done;
    if (JpmcdsDateIntervalToFreq(&fixedSwapIvl_curve, &fixedSwapFreq_curve) != SUCCESS)
      goto done;
    
    pt_floatSwapFreq = (char *) CHAR(STRING_ELT(floatSwapFreq, i % floatSwapFreqLen));
    if (JpmcdsStringToDateInterval(pt_floatSwapFreq, routine_zc_main, &floatSwapIvl_curve) != SUCCESS)
      goto done;
    if (JpmcdsDateIntervalToFreq(&floatSwapIvl_curve, &floatSwapFreq_curve) != SUCCESS)
      goto done;
	
    pt_fixedSwapDCC = (char *) CHAR(STRING_ELT(fixedSwapDCC, i % fixedSwapDCCLen));
    if (JpmcdsStringToDayCountConv(pt_fixedSwapDCC, &fixedSwapDCC_curve) != SUCCESS)
      goto done;
    
    pt_floatSwapDCC = (char *) CHAR(STRING_ELT(floatSwapDCC, i % floatSwapDCCLen));
    if (JpmcdsStringToDayCountConv(pt_floatSwapDCC, &floatSwapDCC_curve) != SUCCESS)
      goto done;
    
    pt_badDayConvZC = (char *) CHAR(STRING_ELT(badDayConvZC, i % badDayConvZCLen));
    pt_holidays = (char *) CHAR(STRING_ELT(holidays, i % holidaysLen));
    pt_types = (char *) CHAR(STRING_ELT(types, i % typesLen));
    
	baseDate = JpmcdsDate((long)INTEGER(baseDate_input)[0 * baseDateLen + i], 
			(long)INTEGER(baseDate_input)[1 * baseDateLen + i], 
			(long)INTEGER(baseDate_input)[2 * baseDateLen + i]);
    
    numInstruments = strlen(pt_types); // for zerocurve
    dates_main = NEW_ARRAY1(TDate, numInstruments);
    for (j = 0; j < numInstruments; j++)
    {
      // offsetting expiries by k shouldn't be problematic since we checked for
      // matching lengths on expiries and types in upfront.R
      if (JpmcdsStringToDateInterval(strdup(CHAR(asChar(VECTOR_ELT(expiries, j + k % expiriesLen)))), routine_zc_main, &tmp) != SUCCESS)
      {
          JpmcdsErrMsg ("%s: invalid interval for element[%d, %d].\n", routine_zc_main, i, j);
          FREE(dates_main);
          goto done;
      }
      
      if (JpmcdsDateFwdThenAdjust(baseDate, &tmp, JPMCDS_BAD_DAY_NONE, "None", dates_main+j) != SUCCESS)
      {
          JpmcdsErrMsg ("%s: invalid interval for element[%d, %d].\n", routine_zc_main, i, j);
          FREE(dates_main);
          goto done;
      }
    }

    // doing pointer arithmetic on REAL(rates) shouldn't be problematic since we checked for
    // matching lengths on rates and types in upfront.R
    if (numInstruments == 0)
      discCurve[i] = NULL;
    else
      discCurve[i] = JpmcdsBuildIRZeroCurve(baseDate,
				       pt_types,
				       dates_main,
				       REAL(rates) + k % ratesLen,
				       (long) numInstruments,
				       (long) mmDCC_zc_main,
				       (long) fixedSwapFreq_curve,
				       (long) floatSwapFreq_curve,
				       fixedSwapDCC_curve,
				       floatSwapDCC_curve,
				       (char) *pt_badDayConvZC,
				       pt_holidays);
    FREE(dates_main);
    k += numInstruments;

    if (discCurve[i] == NULL && numInstruments != 0) JpmcdsErrMsg("IR curve not available ... \n");
  }

  PROTECT(is_na_at = is_na(parSpread));
  gc_protected++;
  if (LOGICAL(is_na_at)[parSpreadLen])
    // vector is entirely NA
    goto done;

  parSpread_for_upf = REAL(parSpread);
  // bps to decimal
  for (int i = 0; i < parSpreadLen; i++)
    parSpread_for_upf[i] /= 10000.0; // FIXME: this mutates parSpread in the code surrounding the call in R
  for (i = 0; i < resultLen; ++i) {
      if (discCurve[i % baseDateLen] == NULL)
        continue;

      /*today = JpmcdsDate((long)INTEGER(todayDate_input)[0 * todayDateLen + i % todayDateLen], 
		     (long)INTEGER(todayDate_input)[1 * todayDateLen + i % todayDateLen], 
		     (long)INTEGER(todayDate_input)[2 * todayDateLen + i % todayDateLen]);*/
      valueDate = JpmcdsDate((long)INTEGER(valueDate_input)[0 * valueDateLen + i % valueDateLen], 
			 (long)INTEGER(valueDate_input)[1 * valueDateLen + i % valueDateLen], 
			 (long)INTEGER(valueDate_input)[2 * valueDateLen + i % valueDateLen]);
      startDate = JpmcdsDate((long)INTEGER(startDate_input)[0 * startDateLen + i % startDateLen], 
			     (long)INTEGER(startDate_input)[1 * startDateLen + i % startDateLen],
			     (long)INTEGER(startDate_input)[2 * startDateLen + i % startDateLen]);
      endDate = JpmcdsDate((long)INTEGER(endDate_input)[0 * endDateLen + i % endDateLen],
		       (long)INTEGER(endDate_input)[1 * endDateLen + i % endDateLen],
		       (long)INTEGER(endDate_input)[2 * endDateLen + i % endDateLen]);
      stepinDate = JpmcdsDate((long)INTEGER(stepinDate_input)[0 * stepinDateLen + i % stepinDateLen],
		       (long)INTEGER(stepinDate_input)[1 * stepinDateLen + i % stepinDateLen],
		       (long)INTEGER(stepinDate_input)[2 * stepinDateLen + i % stepinDateLen]);

      pt_dccCDS = (char *) CHAR(STRING_ELT(dccCDS, i % dccCDSLen));
      if (JpmcdsStringToDayCountConv(pt_dccCDS, &dcc) != SUCCESS)
        goto done;

      pt_ivlCDS = (char *) CHAR(STRING_ELT(ivlCDS, i % ivlCDSLen));
      if (JpmcdsStringToDateInterval(pt_ivlCDS, routine, &ivl) != SUCCESS)
        goto done;

      pt_stubCDS = (char *) CHAR(STRING_ELT(stubCDS, i % stubCDSLen));
      if (JpmcdsStringToStubMethod(pt_stubCDS, &stub) != SUCCESS)
        goto done;

      pt_calendar = (char *) CHAR(STRING_ELT(calendar, i % calendarLen));
      recoveryRate_for_upf = REAL(recoveryRate)[i % recoveryRateLen];
      payAccruedOnDefault = INTEGER(payAccruedOnDefault_input)[i % payAccruedOnDefaultLen];
      pt_badDayConvCDS = (char *) CHAR(STRING_ELT(badDayConvCDS, i % badDayConvCDSLen));

      // can we use JpmcdsStringToDate() ??
      if (sscanf((char *) CHAR(STRING_ELT(colnames, i)), "%04ld-%02ld-%02ld", &year, &month, &day) < 3)
        goto done;

      numMaturities = nrow;
      today = JpmcdsDate(year, month, day);
	  if (today == FAILURE)
        goto done;
      for (j = numMaturities - 1; j >= 0; --j) {
        // exclude maturities with NA
        // FIXME: this mutates parSpread in the code surrounding the call in R
		if (LOGICAL(is_na_at)[nrow * i + j]) {
          for (k = nrow * i + j; k < nrow * i + numMaturities - 1; k++)
            parSpread_for_upf[k] = parSpread_for_upf[k + 1];
          numMaturities--;
        }

        if (getContractEndDate(year, month, day, monthsMaturity[j], &spreadCurveDates[j]) != SUCCESS)
          goto done;
      }
      /*TCurve *curve = JpmcdsCleanSpreadCurve(
          today,
          discCurve[i % baseDateLen],
          startDate,
          stepinDate,
          valueDate,
          numMaturities,
          spreadCurveDates,
		  // R matrices are column-major, so below grabs contiguous column for the date
          &parSpread_for_upf[nrow * i],
          NULL,
          recoveryRate_for_upf,
          payAccruedOnDefault,
          &ivl,
          dcc,
          &stub,
          (char) *pt_badDayConvCDS,
          pt_calendar
      );*/
      TCurve *curve = JpmcdsMakeTCurve(
          today,
          spreadCurveDates,
		  // R matrices are column-major, so below grabs contiguous column for the date
          &parSpread_for_upf[nrow * i],
          numMaturities,
          JPMCDS_CONTINUOUS_BASIS,
          dcc
      );
      /*JpmcdsErrMsg("%d %d %d %d %d %d\n%s %s %s %s %s %s\n%f %f %f %f %f %f",
          monthsMaturity[0], monthsMaturity[1], monthsMaturity[2], monthsMaturity[3], monthsMaturity[4], monthsMaturity[5],
          JpmcdsFormatDate(spreadCurveDates[0]), JpmcdsFormatDate(spreadCurveDates[1]), JpmcdsFormatDate(spreadCurveDates[2]), JpmcdsFormatDate(spreadCurveDates[3]), JpmcdsFormatDate(spreadCurveDates[4]), JpmcdsFormatDate(spreadCurveDates[5]),
          parSpread_for_upf[nrow * i + 0], parSpread_for_upf[nrow * i + 1], parSpread_for_upf[nrow * i + 2], parSpread_for_upf[nrow * i + 3], parSpread_for_upf[nrow * i + 4], parSpread_for_upf[nrow * i + 5]
      );
      JpmcdsErrMsg("%d %s %s %s", numMaturities, JpmcdsFormatDate(today), JpmcdsFormatDate(startDate), JpmcdsFormatDate(endDate));*/
      if (curve == NULL)
        goto done;
      if (JpmcdsInterpRate(endDate, curve, JPMCDS_LINEAR_INTERP, &result[i]) != SUCCESS) {
        JpmcdsFreeTCurve(curve);
        goto done;
      }
      JpmcdsFreeTCurve(curve);
  }

 done:
    PROTECT(syntheticSpread = allocVector(REALSXP, resultLen));
    gc_protected++;
    syntheticSpreads = REAL(syntheticSpread);
    for (i = 0; i < resultLen; ++i)
      syntheticSpreads[i] = result[i] * 1e4;
    UNPROTECT(gc_protected);
    FREE(spreadCurveDates);
    FREE(monthsMaturity);
    for (i = 0; i < baseDateLen; i++)
      JpmcdsFreeTCurve(discCurve[i]);
    FREE(discCurve);
    FREE(result);
    return syntheticSpread;
}


