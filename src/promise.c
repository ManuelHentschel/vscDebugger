#include "promise.h"

static Rboolean is_promise(SEXP object, Rboolean strict) {
  Rboolean ret = TYPEOF(object) == PROMSXP;
  if (ret && strict) {
    ret = PRVALUE(object) == R_UnboundValue;
  }
  return ret;
}

SEXP attribute_hidden c_is_promise(SEXP sym, SEXP env, SEXP strict) {
  if (!isSymbol(sym)) {
    error("'sym' must be a symbol");
  }
  if (!isEnvironment(env)) {
    error("'env' must be an environment");
  }
  if (!IS_SCALAR(strict, LGLSXP)) {
    error("'strict' must be a boolean");
  }
  SEXP obj = findVar(sym, env);
  return ScalarLogical(is_promise(obj, LOGICAL(strict)[0]));
}

SEXP attribute_hidden c_promise_info(SEXP sym, SEXP env) {
  if (!isSymbol(sym)) {
    error("'sym' must be a symbol");
  }
  if (!isEnvironment(env)) {
    error("'env' must be an environment");
  }
  SEXP prom = findVar(sym, env);
  if (!is_promise(prom, FALSE)) {
    error("The object is not a promise");
  }
  SEXP val = PROTECT(PRVALUE(prom));
  Rboolean has_value = (val != R_UnboundValue);
  int len = has_value ? 4 : 3;

  /* allocate and populate list */
  SEXP ret = PROTECT(allocVector(VECSXP, len));
  SET_VECTOR_ELT(ret, 0, PRCODE(prom));
  SET_VECTOR_ELT(ret, 1, PRENV(prom));
  SET_VECTOR_ELT(ret, 2, ScalarLogical(has_value));
  if (has_value) SET_VECTOR_ELT(ret, 3, val);

  /* create names */
  SEXP nms = PROTECT(allocVector(STRSXP, len));
  SET_STRING_ELT(nms, 0, mkChar("code"));
  SET_STRING_ELT(nms, 1, mkChar("environment"));
  SET_STRING_ELT(nms, 2, mkChar("evaluated"));
  if (has_value) SET_STRING_ELT(nms, 3, mkChar("value"));

  /* assign names to list */
  setAttrib(ret, R_NamesSymbol, nms);

  /* cleanup and return */
  UNPROTECT(3);
  return ret;
}
