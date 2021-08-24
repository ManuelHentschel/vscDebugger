





#' @export
.vsc.startWebsocket <- function(
  port = session$dapPort,
  host = '127.0.0.1'
){
  cat('Starting server on port: ', port, '\n', sep='')
  session$dapPort <- port
  session$socketServer <- startServer(
    port = port,
    cb = tmpHandleDAP
  )
}


tmpHandleDAP <- function(s, socket='tclSocket', ...){
  
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
