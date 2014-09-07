// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// cChunk
List cChunk(NumericVector num, List coll);
RcppExport SEXP kea_cChunk(SEXP numSEXP, SEXP collSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< NumericVector >::type num(numSEXP );
        Rcpp::traits::input_parameter< List >::type coll(collSEXP );
        List __result = cChunk(num, coll);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// cDrop
List cDrop(NumericVector num, List coll);
RcppExport SEXP kea_cDrop(SEXP numSEXP, SEXP collSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< NumericVector >::type num(numSEXP );
        Rcpp::traits::input_parameter< List >::type coll(collSEXP );
        List __result = cDrop(num, coll);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// cPowerSetOf
List cPowerSetOf(List coll);
RcppExport SEXP kea_cPowerSetOf(SEXP collSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< List >::type coll(collSEXP );
        List __result = cPowerSetOf(coll);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// cTake
List cTake(NumericVector num, List coll);
RcppExport SEXP kea_cTake(SEXP numSEXP, SEXP collSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< NumericVector >::type num(numSEXP );
        Rcpp::traits::input_parameter< List >::type coll(collSEXP );
        List __result = cTake(num, coll);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
