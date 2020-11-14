





#' @export
.vsc.startWebsocket <- function(
  port = session$dapPort,
  host = '127.0.0.1'
){
  cat('Starting server on port: ', port, '\n', sep='')
  session$dapPort <- port
  session$socketServer <- svSocket::startSocketServer(
    port = port,
    procfun = tmpHandleDAP
  )

  # s <- httpuv::startServer(host, port, list(
  #   onWSOpen = function(ws) {
  #     cat('Websocket has connection.')
  #     session$ws <- ws
  #     ws$onMessage(function(binary, message) {
  #       tmpHandleDAP(message)
  #     })
  #   },
  #   onWSMessage = function(msg) {
  #     print(msg)
  #   }
  # ))
}


tmpHandleDAP <- function(s, socket, ...){

  registerEntryFrame()

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
  unregisterEntryFrame()
  return('')
}
