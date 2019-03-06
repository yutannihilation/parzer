// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// pz_parse_lat
NumericVector pz_parse_lat(CharacterVector x);
RcppExport SEXP _parzer_pz_parse_lat(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(pz_parse_lat(x));
    return rcpp_result_gen;
END_RCPP
}
// pz_parse_lon
NumericVector pz_parse_lon(CharacterVector x);
RcppExport SEXP _parzer_pz_parse_lon(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(pz_parse_lon(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_parzer_pz_parse_lat", (DL_FUNC) &_parzer_pz_parse_lat, 1},
    {"_parzer_pz_parse_lon", (DL_FUNC) &_parzer_pz_parse_lon, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_parzer(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
