#include "promise.h"

// Helper functions to get promise info depend on the R version (</>= 4.6.0)
#if R_VERSION < R_Version(4, 6, 0)

static Rboolean is_promise(SEXP sym, SEXP env) {
  SEXP object = findVar(sym, env);
  return TYPEOF(object) == PROMSXP && PRVALUE(object) == R_UnboundValue;
}

static SEXP get_promise_code(SEXP sym, SEXP env) {
  return PRCODE(findVar(sym, env));
}

static SEXP get_promise_environment(SEXP sym, SEXP env) {
  return PRENV(findVar(sym, env));
}

#else

/* for R >= 4.6.0, use API from:
 * https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Working-with-variable-bindings
 */

static Rboolean is_promise(SEXP sym, SEXP env) {
  return R_GetBindingType(sym, env) == R_BindingTypeDelayed;
}

static SEXP get_promise_code(SEXP sym, SEXP env) {
  return R_DelayedBindingExpression(sym, env);
}

static SEXP get_promise_environment(SEXP sym, SEXP env) {
  return R_DelayedBindingEnvironment(sym, env);
}

#endif

SEXP attribute_hidden c_is_promise(SEXP sym, SEXP env) {
  if (!isSymbol(sym)) {
    error("'sym' must be a symbol");
  }
  if (!isEnvironment(env)) {
    error("'env' must be an environment");
  }
  return ScalarLogical(is_promise(sym, env));
}

SEXP attribute_hidden c_promise_info(SEXP sym, SEXP env) {
  if (!isSymbol(sym)) {
    error("'sym' must be a symbol");
  }
  if (!isEnvironment(env)) {
    error("'env' must be an environment");
  }
  if (!is_promise(sym, env)) {
    error("The object is not a promise");
  }

  int len = 2;
  SEXP exp = get_promise_code(sym, env);
  SEXP promise_env = get_promise_environment(sym, env);

  /* allocate and populate list */
  SEXP ret = PROTECT(allocVector(VECSXP, len));
  SET_VECTOR_ELT(ret, 0, exp);
  SET_VECTOR_ELT(ret, 1, promise_env);

  /* create names */
  SEXP nms = PROTECT(allocVector(STRSXP, len));
  SET_STRING_ELT(nms, 0, mkChar("code"));
  SET_STRING_ELT(nms, 1, mkChar("environment"));

  /* assign names to list */
  setAttrib(ret, R_NamesSymbol, nms);

  /* cleanup and return */
  UNPROTECT(2);
  return ret;
}
