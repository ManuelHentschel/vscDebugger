#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP c_is_promise(SEXP, SEXP);
extern SEXP c_promise_info(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"c_is_promise",   (DL_FUNC) &c_is_promise,   3},
    {"c_promise_info", (DL_FUNC) &c_promise_info, 2},
    {NULL, NULL, 0}
};

void R_init_vscDebugger(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
