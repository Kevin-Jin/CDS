#include <R.h>
#define USE_RINTERNALS
#include <Rdefines.h>
#include <Rinternals.h>
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

SEXP is_na(SEXP x)
{
  int n = length(x);

  SEXP out = PROTECT(allocVector(LGLSXP, n + 1));
  int allNa = 1;

  for (int i = 0; i < n; i++) {
    switch(TYPEOF(x)) {
      case LGLSXP:
        allNa &= (LOGICAL(out)[i] = (LOGICAL(x)[i] == NA_LOGICAL));
        break;
      case INTSXP:
        allNa &= (LOGICAL(out)[i] = (INTEGER(x)[i] == NA_INTEGER));
        break;
      case REALSXP:
        allNa &= (LOGICAL(out)[i] = ISNA(REAL(x)[i]));
        break;
      case STRSXP:
        allNa &= (LOGICAL(out)[i] = (STRING_ELT(x, i) == NA_STRING));
        break;
      default:
        LOGICAL(out)[i] = NA_LOGICAL;
    }
  }
  LOGICAL(out)[n] = allNa;
  UNPROTECT(1);

  return out;
}

// https://github.com/Rdatatable/data.table/blob/master/src/assign.c
SEXP memrecycle(SEXP source, int len)
{
    int r = 0;
    SEXP target = PROTECT(allocVector(TYPEOF(source), MAX(len, 0)));
    int slen = MIN(length(source), len); // fix for 5647. when length(source) > len, slen must be len.
    if (len<1 || slen<1) {
        UNPROTECT(1);
        return target;
    }
    size_t size;
    switch (TYPEOF(target)) {
        case INTSXP :
        case LGLSXP :
            size = sizeof(int);
            break;
        case REALSXP :
            size = sizeof(double);
            break;
        case STRSXP :
            size = sizeof(SEXP *);
            for (; r<slen; r++)     // only one SET_STRING_ELT per RHS item is needed to set generations (overhead)
                SET_STRING_ELT(target, r, STRING_ELT(source, r));
            break;
        case VECSXP :
            size = sizeof(SEXP *);
            for (; r<slen; r++)
                SET_VECTOR_ELT(target, r, VECTOR_ELT(source, r));
                // TO DO: if := in future could ever change a list item's contents by reference, would need to duplicate at that point
            break;
        default :
            error("Unsupported type '%s'", type2char(TYPEOF(target)));
    }
    if (slen == 1) {  
        if (size==4) for (; r<len; r++)
            INTEGER(target)[r] = INTEGER(source)[0];   // copies pointer on 32bit
        else for (; r<len; r++)
            REAL(target)[r] = REAL(source)[0];         // copies pointer on 64bit
    } else if (slen<10) {    // 10 is just a guess for when memcpy is faster. Certainly memcpy is slower when slen==1, but that's the most common case by far so not high priority to discover the optimum here. TO DO: revisit
        if (size==4) for (; r<len; r++)
            INTEGER(target)[r] = INTEGER(source)[r%slen];
        else for (; r<len; r++)
            REAL(target)[r] = REAL(source)[r%slen];
    } else {
        for (r=r>0?1:0; r<(len/slen); r++) {   // if the first slen were done in the switch above, convert r=slen to r=1
            memcpy((char *)DATAPTR(target) + (r*slen)*size,
                   (char *)DATAPTR(source),
                   slen * size);
        }
        memcpy((char *)DATAPTR(target) + (r*slen)*size,
               (char *)DATAPTR(source),
               (len%slen) * size);
    }
    UNPROTECT(1);
    return target;
}

/*
***************************************************************************
** Calculate upfront charge.
***************************************************************************
*/

//EXPORT int JpmcdsCdsoneUpfrontCharge(cdsone.c)
SEXP calcUpfrontTest
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
 SEXP benchmarkDate_input,/* start date of benchmark CDS for internal
				     ** clean spread bootstrapping;
				     ** accrual Begin Date  */
 SEXP startDate_input,/* Accrual Begin Date */
 SEXP endDate_input,/*  Maturity (Fixed) */
 SEXP stepinDate_input,  /* T + 1*/
 
 SEXP dccCDS, 			/* accruedDcc */
 SEXP ivlCDS,
 SEXP stubCDS,
 SEXP badDayConvCDS,
 SEXP calendar,

 SEXP parSpread,
 SEXP couponRate,
 SEXP recoveryRate,
 SEXP isPriceClean_input,
 SEXP payAccruedOnDefault_input,
 SEXP notional) 

{
  //  static char routine[] = "JpmcdsCdsoneUpfrontCharge";

  // my vars
  int resultLen, numInstruments, i, j, k, gc_protected, baseDateLen,
    typesLen, mmDCCLen, fixedSwapFreqLen, floatSwapFreqLen, fixedSwapDCCLen, floatSwapDCCLen, badDayConvZCLen, holidaysLen,
	todayDateLen, valueDateLen, benchmarkDateLen, startDateLen, endDateLen, stepinDateLen,
    dccCDSLen, ivlCDSLen, stubCDSLen, badDayConvCDSLen, calendarLen,
    parSpreadLen, couponRateLen, recoveryRateLen, isPriceCleanLen, payAccruedOnDefaultLen, notionalLen;
  TDate baseDate, today, benchmarkDate, startDate, endDate, stepinDate,valueDate;
  int isPriceClean, payAccruedOnDefault;
  SEXP upfrontPayment, is_na_at;
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
  double *parSpread_for_upf, couponRate_for_upf, recoveryRate_for_upf, *upfrontPayments, *result;

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

  typesLen = length(types);
  mmDCCLen = length(mmDCC);
  fixedSwapFreqLen = length(fixedSwapFreq);
  floatSwapFreqLen = length(floatSwapFreq);
  fixedSwapDCCLen = length(fixedSwapDCC);
  floatSwapDCCLen = length(floatSwapDCC);
  badDayConvZCLen = length(badDayConvZC);
  holidaysLen = length(holidays);

  baseDateLen = length(baseDate_input) / 3;
  todayDateLen = length(todayDate_input) / 3;
  valueDateLen = length(valueDate_input) / 3;
  benchmarkDateLen = length(benchmarkDate_input) / 3;
  startDateLen = length(startDate_input) / 3;
  endDateLen = length(endDate_input) / 3;
  stepinDateLen = length(stepinDate_input) / 3;

  dccCDSLen = length(dccCDS);
  ivlCDSLen = length(ivlCDS);
  stubCDSLen = length(stubCDS);
  badDayConvCDSLen = length(badDayConvCDS);
  calendarLen = length(calendar);

  parSpreadLen = length(parSpread);
  couponRateLen = length(couponRate);
  recoveryRateLen = length(recoveryRate);
  isPriceCleanLen = length(isPriceClean_input);
  payAccruedOnDefaultLen = length(payAccruedOnDefault_input);
  notionalLen = length(notional);

  gc_protected = 0;
  resultLen = MAX(MAX(baseDateLen, parSpreadLen), couponRateLen);
  result = NEW_ARRAY1(double, resultLen);
  for (i = 0; i < resultLen; ++i)
    result[i] = NA_REAL;

  baseDate_input = coerceVector(baseDate_input,INTSXP);
  types = coerceVector(types, STRSXP);
  rates = coerceVector(rates, REALSXP);
  expiries = coerceVector(expiries, VECSXP);
  mmDCC = coerceVector(mmDCC, STRSXP);

  fixedSwapFreq = coerceVector(fixedSwapFreq, STRSXP);
  floatSwapFreq = coerceVector(floatSwapFreq, STRSXP);
  fixedSwapDCC = coerceVector(fixedSwapDCC, STRSXP);
  floatSwapDCC = coerceVector(floatSwapDCC, STRSXP);
  badDayConvZC = coerceVector(badDayConvZC, STRSXP);
  holidays = coerceVector(holidays, STRSXP);

  todayDate_input = coerceVector(todayDate_input,INTSXP);
  valueDate_input = coerceVector(valueDate_input,INTSXP);
  benchmarkDate_input = coerceVector(benchmarkDate_input,INTSXP);
  startDate_input = coerceVector(startDate_input,INTSXP);
  endDate_input = coerceVector(endDate_input,INTSXP);
  stepinDate_input = coerceVector(stepinDate_input,INTSXP);

  dccCDS = coerceVector(dccCDS, STRSXP);
  ivlCDS = coerceVector(ivlCDS, STRSXP);
  stubCDS = coerceVector(stubCDS, STRSXP);
  badDayConvCDS = coerceVector(badDayConvCDS, STRSXP);
  calendar = coerceVector(calendar, STRSXP);

  PROTECT(is_na_at = is_na(baseDate_input));
  gc_protected++;
  discCurve = NEW_ARRAY1(TCurve*, baseDateLen);
  if (LOGICAL(is_na_at)[baseDateLen])
    // vector is entirely NA
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
      if (JpmcdsStringToDateInterval(strdup(CHAR(asChar(VECTOR_ELT(expiries, j + k)))), routine_zc_main, &tmp) != SUCCESS)
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
    discCurve[i] = JpmcdsBuildIRZeroCurve(baseDate,
				       pt_types,
				       dates_main,
				       REAL(rates) + k,
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

    if (discCurve[i] == NULL) JpmcdsErrMsg("IR curve not available ... \n");
  }

  PROTECT(is_na_at = is_na(parSpread));
  gc_protected++;
  if (LOGICAL(is_na_at)[parSpreadLen])
    // vector is entirely NA
    goto done;

  parSpread_for_upf = REAL(parSpread);
  for (i = 0; i < resultLen; ++i) {
      if (LOGICAL(is_na_at)[i % parSpreadLen] || discCurve[i % baseDateLen] == NULL)
        continue;

      today = JpmcdsDate((long)INTEGER(todayDate_input)[0 * todayDateLen + i % todayDateLen], 
		     (long)INTEGER(todayDate_input)[1 * todayDateLen + i % todayDateLen], 
		     (long)INTEGER(todayDate_input)[2 * todayDateLen + i % todayDateLen]);
      valueDate = JpmcdsDate((long)INTEGER(valueDate_input)[0 * valueDateLen + i % valueDateLen], 
			 (long)INTEGER(valueDate_input)[1 * valueDateLen + i % valueDateLen], 
			 (long)INTEGER(valueDate_input)[2 * valueDateLen + i % valueDateLen]);
      benchmarkDate = JpmcdsDate((long)INTEGER(benchmarkDate_input)[0 * benchmarkDateLen + i % benchmarkDateLen], 
			     (long)INTEGER(benchmarkDate_input)[1 * benchmarkDateLen + i % benchmarkDateLen],
			     (long)INTEGER(benchmarkDate_input)[2 * benchmarkDateLen + i % benchmarkDateLen]);
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
      couponRate_for_upf = REAL(couponRate)[i % couponRateLen];
      recoveryRate_for_upf = REAL(recoveryRate)[i % recoveryRateLen];
      isPriceClean = INTEGER(isPriceClean_input)[i % isPriceCleanLen];
      payAccruedOnDefault = INTEGER(payAccruedOnDefault_input)[i % payAccruedOnDefaultLen];
      pt_badDayConvCDS = (char *) CHAR(STRING_ELT(badDayConvCDS, i % badDayConvCDSLen));

      if (JpmcdsCdsoneUpfrontCharge(today,
				  valueDate,
				  benchmarkDate,
				  stepinDate,
				  startDate,
				  endDate,
				  couponRate_for_upf / 10000.0,
				  payAccruedOnDefault, //TRUE,
				  &ivl,
				  &stub, 
				  dcc,
				  (char) *pt_badDayConvCDS,
				  pt_calendar,
				  discCurve[i % baseDateLen],
				  parSpread_for_upf[i % parSpreadLen]/10000.0, 
				  recoveryRate_for_upf,
				  isPriceClean,
				  &result[i]) != SUCCESS) 
        goto done;
  }

 done:
    PROTECT(upfrontPayment = allocVector(REALSXP, resultLen));
    gc_protected++;
    upfrontPayments = REAL(upfrontPayment);
    for (i = 0; i < resultLen; ++i)
      upfrontPayments[i] = result[i] * REAL(notional)[i % notionalLen];
    UNPROTECT(gc_protected);
    FREE(discCurve);
    FREE(result);
    return upfrontPayment;
}


