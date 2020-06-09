 
#ifndef VSCDEBUGGER_PROMISE_H_
#define VSCDEBUGGER_PROMISE_H_

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Visibility.h>
#include <R_ext/Error.h>

SEXP attribute_hidden c_is_promise(SEXP, SEXP, SEXP);
SEXP attribute_hidden c_promise_info(SEXP, SEXP);

#endif