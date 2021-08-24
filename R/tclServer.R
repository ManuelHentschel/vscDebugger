



startServer <- function(port, cb){
  cbFunc <- function(){
    tcltk::.Tcl('global line')
    tcltk::.Tcl('global sock')
    line <- tcltk::tclvalue('line')
    sock <- tcltk::tclvalue('sock')
    cat(line)
    cb(line, sock)
  }
  tclCb <- tcltk::.Tcl.callback(cbFunc)

# TODO: Copy contents into this file!
  tclFile <- file.path(find.package('vscDebugger'), 'server.tcl')
  setupCmd <- paste0(readLines(tclFile), collapse = '\n')

  tcltk::.Tcl(setupCmd)
  tcltk::.Tcl(paste0('proc handleLine {} {', tclCb, '}'))
  tcltk::.Tcl(paste0('set port ', port))
  tcltk::.Tcl('startServer')
  
  invisible(TRUE)
}

stopServer <- function(){
  tcltk::.Tcl('stopServer')
}


