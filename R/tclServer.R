


#' @export
.vsc.startTclServer <- function(
  port = session$dapPort,
  host = '127.0.0.1'
){
  cat('Starting server on port: ', port, '\n', sep='')
  session$dapPort <- port
  session$socketServer <- startServer(
    port = port,
    cb = .vsc.handleDap
  )
}


#' @export
.vsc.handleDap <- function(s, socket='tclSocket', ...){
  
  # SKIPPED_FRAMES <- 6
  SKIPPED_FRAMES <- 1

  registerEntryFrame(SKIPPED_FRAMES)

  session$svName <- socket
  s <- paste0(session$restOfWs, s)

  while(TRUE){
    # identify content-length
    m <- regexec('^Content-Length: (\\d+)\r?\n\r?\n', s)
    rm <- regmatches(s, m)

    if(length(rm[[1]]) == 0){
      session$restOfWs <- s
      break
    }

    contentLength <- as.numeric(rm[[1]][2])
    headerLength <- nchar(rm[[1]][1])

    if(nchar(s)<contentLength+headerLength){
      session$restOfWs <- s
      break
    }

    json <- substring(s, headerLength+1, headerLength + contentLength)
    s <- substring(s, headerLength+contentLength+1, length(s))

    .vsc.handleJson(json)
  }
  unregisterEntryFrame(SKIPPED_FRAMES)
  return('')
}


startServer <- function(port, cb){
  cbFunc <- function(){
    registerEntryFrame(skipCalls = 5)
    tcltk::.Tcl('global line')
    tcltk::.Tcl('global sock')
    line <- tcltk::tclvalue('line')
    sock <- tcltk::tclvalue('sock')
    cb(line, sock)
    # unregisterEntryFrame()
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


