// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/remify.h"
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// remifyCpp
Rcpp::List remifyCpp(Rcpp::DataFrame input_edgelist, Rcpp::RObject actors, Rcpp::RObject types, bool directed, bool ordinal, Rcpp::RObject origin, Rcpp::List omit_dyad, std::string model);
RcppExport SEXP _remify_remifyCpp(SEXP input_edgelistSEXP, SEXP actorsSEXP, SEXP typesSEXP, SEXP directedSEXP, SEXP ordinalSEXP, SEXP originSEXP, SEXP omit_dyadSEXP, SEXP modelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type input_edgelist(input_edgelistSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type actors(actorsSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type types(typesSEXP);
    Rcpp::traits::input_parameter< bool >::type directed(directedSEXP);
    Rcpp::traits::input_parameter< bool >::type ordinal(ordinalSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type origin(originSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type omit_dyad(omit_dyadSEXP);
    Rcpp::traits::input_parameter< std::string >::type model(modelSEXP);
    rcpp_result_gen = Rcpp::wrap(remifyCpp(input_edgelist, actors, types, directed, ordinal, origin, omit_dyad, model));
    return rcpp_result_gen;
END_RCPP
}
// check_process
Rcpp::List check_process(Rcpp::RObject time, Rcpp::RObject origin);
RcppExport SEXP _remify_check_process(SEXP timeSEXP, SEXP originSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::RObject >::type time(timeSEXP);
    Rcpp::traits::input_parameter< Rcpp::RObject >::type origin(originSEXP);
    rcpp_result_gen = Rcpp::wrap(check_process(time, origin));
    return rcpp_result_gen;
END_RCPP
}
// getDyadIndex
int getDyadIndex(double actor1, double actor2, double type, int N, bool directed);
static SEXP _remify_getDyadIndex_try(SEXP actor1SEXP, SEXP actor2SEXP, SEXP typeSEXP, SEXP NSEXP, SEXP directedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< double >::type actor1(actor1SEXP);
    Rcpp::traits::input_parameter< double >::type actor2(actor2SEXP);
    Rcpp::traits::input_parameter< double >::type type(typeSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< bool >::type directed(directedSEXP);
    rcpp_result_gen = Rcpp::wrap(getDyadIndex(actor1, actor2, type, N, directed));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _remify_getDyadIndex(SEXP actor1SEXP, SEXP actor2SEXP, SEXP typeSEXP, SEXP NSEXP, SEXP directedSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_remify_getDyadIndex_try(actor1SEXP, actor2SEXP, typeSEXP, NSEXP, directedSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// getDyadComposition
Rcpp::IntegerVector getDyadComposition(int d, int C, int N, int D);
static SEXP _remify_getDyadComposition_try(SEXP dSEXP, SEXP CSEXP, SEXP NSEXP, SEXP DSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< int >::type d(dSEXP);
    Rcpp::traits::input_parameter< int >::type C(CSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< int >::type D(DSEXP);
    rcpp_result_gen = Rcpp::wrap(getDyadComposition(d, C, N, D));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _remify_getDyadComposition(SEXP dSEXP, SEXP CSEXP, SEXP NSEXP, SEXP DSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_remify_getDyadComposition_try(dSEXP, CSEXP, NSEXP, DSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}

// validate (ensure exported C++ functions exist before calling them)
static int _remify_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("int(*getDyadIndex)(double,double,double,int,bool)");
        signatures.insert("Rcpp::IntegerVector(*getDyadComposition)(int,int,int,int)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _remify_RcppExport_registerCCallable() { 
    R_RegisterCCallable("remify", "_remify_getDyadIndex", (DL_FUNC)_remify_getDyadIndex_try);
    R_RegisterCCallable("remify", "_remify_getDyadComposition", (DL_FUNC)_remify_getDyadComposition_try);
    R_RegisterCCallable("remify", "_remify_RcppExport_validate", (DL_FUNC)_remify_RcppExport_validate);
    return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
    {"_remify_remifyCpp", (DL_FUNC) &_remify_remifyCpp, 8},
    {"_remify_check_process", (DL_FUNC) &_remify_check_process, 2},
    {"_remify_getDyadIndex", (DL_FUNC) &_remify_getDyadIndex, 5},
    {"_remify_getDyadComposition", (DL_FUNC) &_remify_getDyadComposition, 4},
    {"_remify_RcppExport_registerCCallable", (DL_FUNC) &_remify_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_remify(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
