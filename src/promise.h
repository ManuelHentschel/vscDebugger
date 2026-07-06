 
#ifndef VSCDEBUGGER_PROMISE_H_
#define VSCDEBUGGER_PROMISE_H_

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Visibility.h>
#include <R_ext/Error.h>
#include <Rversion.h>


#if R_VERSION < R_Version(4, 6, 0)
SEXP attribute_hidden c_is_promise(SEXP, SEXP, SEXP);
#else
SEXP attribute_hidden c_is_promise(SEXP, SEXP);
#endif

SEXP attribute_hidden c_promise_info(SEXP, SEXP);

#endif