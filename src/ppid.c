
# include "ppid.h"

#if defined(VSCDEBUGGER_PPID_GETPPID)
// getppid() is only defined on unix like systems
SEXP c_get_ppid() {
    return ScalarInteger(getppid());
}
#else
// mostly windows systems
SEXP c_get_ppid() {
    return ScalarInteger(0);
}
#endif

