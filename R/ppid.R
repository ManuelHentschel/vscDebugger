
#' Get parent pid
#' 
#' Returns the pid of the parent process (on unix like systems) or 0 (on windows).
#' 
#' @useDynLib vscDebugger c_get_ppid
getPpid <- function(){
  .Call(c_get_ppid)
}
