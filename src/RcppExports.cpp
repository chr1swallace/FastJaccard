// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// jaccard_pair
double jaccard_pair(const CharacterVector x, const CharacterVector y);
RcppExport SEXP _FastJaccard_jaccard_pair(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CharacterVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const CharacterVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(jaccard_pair(x, y));
    return rcpp_result_gen;
END_RCPP
}
// denom_pair
double denom_pair(const CharacterVector x, const CharacterVector y);
RcppExport SEXP _FastJaccard_denom_pair(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CharacterVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const CharacterVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(denom_pair(x, y));
    return rcpp_result_gen;
END_RCPP
}
// overlap_pair
int overlap_pair(const CharacterVector x, const CharacterVector y);
RcppExport SEXP _FastJaccard_overlap_pair(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const CharacterVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const CharacterVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(overlap_pair(x, y));
    return rcpp_result_gen;
END_RCPP
}
// jaccard_lists
NumericMatrix jaccard_lists(const List A, const List B);
RcppExport SEXP _FastJaccard_jaccard_lists(SEXP ASEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type A(ASEXP);
    Rcpp::traits::input_parameter< const List >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(jaccard_lists(A, B));
    return rcpp_result_gen;
END_RCPP
}
// denom_lists
NumericMatrix denom_lists(const List A, const List B);
RcppExport SEXP _FastJaccard_denom_lists(SEXP ASEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type A(ASEXP);
    Rcpp::traits::input_parameter< const List >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(denom_lists(A, B));
    return rcpp_result_gen;
END_RCPP
}
// overlap_lists
NumericMatrix overlap_lists(const List A, const List B);
RcppExport SEXP _FastJaccard_overlap_lists(SEXP ASEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type A(ASEXP);
    Rcpp::traits::input_parameter< const List >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(overlap_lists(A, B));
    return rcpp_result_gen;
END_RCPP
}
// jaccard_symlist
NumericMatrix jaccard_symlist(const List A);
RcppExport SEXP _FastJaccard_jaccard_symlist(SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(jaccard_symlist(A));
    return rcpp_result_gen;
END_RCPP
}
// denom_symlist
NumericMatrix denom_symlist(const List A);
RcppExport SEXP _FastJaccard_denom_symlist(SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(denom_symlist(A));
    return rcpp_result_gen;
END_RCPP
}
// overlap_symlist
NumericMatrix overlap_symlist(const List A);
RcppExport SEXP _FastJaccard_overlap_symlist(SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(overlap_symlist(A));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_FastJaccard_jaccard_pair", (DL_FUNC) &_FastJaccard_jaccard_pair, 2},
    {"_FastJaccard_denom_pair", (DL_FUNC) &_FastJaccard_denom_pair, 2},
    {"_FastJaccard_overlap_pair", (DL_FUNC) &_FastJaccard_overlap_pair, 2},
    {"_FastJaccard_jaccard_lists", (DL_FUNC) &_FastJaccard_jaccard_lists, 2},
    {"_FastJaccard_denom_lists", (DL_FUNC) &_FastJaccard_denom_lists, 2},
    {"_FastJaccard_overlap_lists", (DL_FUNC) &_FastJaccard_overlap_lists, 2},
    {"_FastJaccard_jaccard_symlist", (DL_FUNC) &_FastJaccard_jaccard_symlist, 1},
    {"_FastJaccard_denom_symlist", (DL_FUNC) &_FastJaccard_denom_symlist, 1},
    {"_FastJaccard_overlap_symlist", (DL_FUNC) &_FastJaccard_overlap_symlist, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_FastJaccard(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
