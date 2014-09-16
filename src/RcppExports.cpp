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
// cImplode
CharacterVector cImplode(const CharacterVector str, const CharacterVector strs);
RcppExport SEXP kea_cImplode(SEXP strSEXP, SEXP strsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const CharacterVector >::type str(strSEXP );
        Rcpp::traits::input_parameter< const CharacterVector >::type strs(strsSEXP );
        CharacterVector __result = cImplode(str, strs);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// cIsIn
LogicalVector cIsIn(SEXP val, List coll);
RcppExport SEXP kea_cIsIn(SEXP valSEXP, SEXP collSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< SEXP >::type val(valSEXP );
        Rcpp::traits::input_parameter< List >::type coll(collSEXP );
        LogicalVector __result = cIsIn(val, coll);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// cJoin
List cJoin(const List colls);
RcppExport SEXP kea_cJoin(SEXP collsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const List >::type colls(collsSEXP );
        List __result = cJoin(colls);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// cNotIn
LogicalVector cNotIn(SEXP val, List coll);
RcppExport SEXP kea_cNotIn(SEXP valSEXP, SEXP collSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< SEXP >::type val(valSEXP );
        Rcpp::traits::input_parameter< List >::type coll(collSEXP );
        LogicalVector __result = cNotIn(val, coll);
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
// cRiffle
List cRiffle(SEXP val, List coll);
RcppExport SEXP kea_cRiffle(SEXP valSEXP, SEXP collSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< SEXP >::type val(valSEXP );
        Rcpp::traits::input_parameter< List >::type coll(collSEXP );
        List __result = cRiffle(val, coll);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// cSwap
List cSwap(const SEXP val1, const SEXP val2, List coll);
RcppExport SEXP kea_cSwap(SEXP val1SEXP, SEXP val2SEXP, SEXP collSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const SEXP >::type val1(val1SEXP );
        Rcpp::traits::input_parameter< const SEXP >::type val2(val2SEXP );
        Rcpp::traits::input_parameter< List >::type coll(collSEXP );
        List __result = cSwap(val1, val2, coll);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// cTabulate
List cTabulate(List coll);
RcppExport SEXP kea_cTabulate(SEXP collSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< List >::type coll(collSEXP );
        List __result = cTabulate(coll);
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
// cUnzipIndices
List cUnzipIndices(List coll);
RcppExport SEXP kea_cUnzipIndices(SEXP collSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< List >::type coll(collSEXP );
        List __result = cUnzipIndices(coll);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// cUnzipKeys
List cUnzipKeys(List coll);
RcppExport SEXP kea_cUnzipKeys(SEXP collSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< List >::type coll(collSEXP );
        List __result = cUnzipKeys(coll);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// cZip
List cZip(const List colls);
RcppExport SEXP kea_cZip(SEXP collsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const List >::type colls(collsSEXP );
        List __result = cZip(colls);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// cZipKeys
List cZipKeys(const List colls);
RcppExport SEXP kea_cZipKeys(SEXP collsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const List >::type colls(collsSEXP );
        List __result = cZipKeys(colls);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
