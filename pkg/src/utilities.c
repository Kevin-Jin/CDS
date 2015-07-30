#include "utilities.h"
#include "macros.h"


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
