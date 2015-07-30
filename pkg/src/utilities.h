#ifndef UTILITIES_H
#define UTILITIES_H

#include <R.h>
#define USE_RINTERNALS
#include <Rdefines.h>
#include <Rinternals.h>


SEXP is_na(SEXP x);

SEXP memrecycle(SEXP source, int len);

#endif    /* UTILITIES_H */
