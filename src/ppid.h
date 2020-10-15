 
#ifndef VSCDEBUGGER_PPID_H_
#define VSCDEBUGGER_PPID_H_

#include <R.h>
#include <Rinternals.h>

#if defined(unix) || defined(__unix__) || defined(__unix)
#include <unistd.h> // getppid
#define VSCDEBUGGER_PPID_GETPPID
#endif

SEXP c_get_ppid();

#endif