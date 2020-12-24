





#' @export
.vsc.startWebsocket <- function(
  port = session$dapPort,
  host = '127.0.0.1'
){
  supportsIncompleteLines <- tryCatch(
    svSocket::supportsIncompleteLines(),
    error = function(...) FALSE
  )
  if(!supportsIncompleteLines){
    informAboutSvSocket()
    return(FALSE)
  }
  cat('Starting server on port: ', port, '\n', sep='')
  session$dapPort <- port
  session$socketServer <- svSocket::startSocketServer(
    port = port,
    procfun = tmpHandleDAP
  )
}


tmpHandleDAP <- function(s, socket, ...){

  SKIPPED_FRAMES <- 6

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



informAboutSvSocket <- function(){
  url <- 'https://github.com/ManuelHentschel/svSocket/tree/forVscDebugger'
  installCommand <- paste0(
    '\tremotes::install_github("',
    url,
    '")'
  )
  s <- paste(
    "\nThis functionality is experimental!",
    "It depends on a modified version of the R package svSocket.",
    "The modified version can be downloaded from GitHub:\n",
    installCommand,
    "\nThis package modification is only meant as a temporary workaround and will be replaced.\n",
    sep = '\n'
  )
  message(s)
  stop('To use this function install the modified version of svSocket!')
  invisible(NULL)
}
