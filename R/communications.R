
#' @export
.vsc.listenForDAP <- function(
  port = session$dapPort,
  host = session$dapHost,
  timeout = -1,
  server = TRUE
){
  registerEntryFrame()

  # choose/open socketConnection
  if(session$useJsonSocket){
    # sessions can only use one or the other function to receive commands
    stop('This Debug session receives messages via JSON socket.')
  } else if(is.null(session$dapSocketConnection)){
    # new session (also applies for disconnected sessions)
    logCat('Listening on ', host, ':', toString(port), '\n', sep='')
    conn <- socketConnection(
      host = host,
      port = port,
      server = server,
      open = "r+b",
      blocking = FALSE
    )
    session$dapPort <- port
    session$dapHost <- host
    session$dapSocketConnection <- conn
    session$useDapSocket <- TRUE
  } else{
    # resume running session
    port <- session$dapPort
    host <- session$dapHost
    conn <- session$dapSocketConnection
    logCat('Listening on ', host, ':', toString(port), '\n', sep='')
    if(!session$state$isPaused()){
      sendStoppedEvent('step')
    }
    session$previousOptions <- options(session$internalOptions)
  }

  # check if paused on function step or toplevel prompt
  if(session$state$isPaused()){
    logCat('Session already paused.')
  } else if(isCalledFromBrowser()){
    session$state$startPaused('step')
  } else{
    session$state$startPaused('toplevel')
  }

  # main loop
  t <- as.numeric(Sys.time())
  session$stopListeningOnPort <- FALSE
  while(!session$stopListeningOnPort){
    # wait for first character, regularly checking for timeout
    char <- readChar(conn, nchars=1)
    while(length(char)==0 && !session$stopListeningOnPort){
      if(timeout>=0 && as.numeric(Sys.time())-t > timeout){
        session$stopListeningOnPort <- TRUE
      } else{
        Sys.sleep(0.001)
      }
      char <- readChar(conn, nchars=1)
    }

    # read header, don't check timeout, don't Sys.sleep
    if(length(char)>0){
      header <- char
      while(!endsWith(header, '\r\n\r\n')){
        char <- readChar(conn, nchars=1)
        header <- paste0(header, char)
      }

      # identify content-length
      m <- regexec('Content-Length: (\\d+)\r\n', header)
      rm <- regmatches(header, m)
      contentLength <- as.numeric(rm[[1]][2])

      # read content in one go
      json <- readChar(conn, nchars=contentLength, useBytes = TRUE)
      Encoding(json) <- 'UTF-8'
      # json <- enc2native(json)

      # handle content
      .vsc.handleJson(json)

      # reset timer
      t <- as.numeric(Sys.time())
    }
  }
  logPrint('Stop listening on port')
  options(session$previousOptions)
  unregisterEntryFrame()
  invisible(NULL)
}

#' @export
.vsc.listen <- .vsc.listenForDAP

#' @export
.vsc.handleJson <- function(json){
  registerEntryFrame()
  obj <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  if(lget(obj, 'type', '') == 'request'){
    .vsc.dispatchRequest(obj)
    typeKnown <- TRUE
  } else{
    logCat('Unknown json: ', json, '\n')
    typeKnown <- FALSE
  }
  unregisterEntryFrame()
  invisible(typeKnown)
}


#' Sends a json to vsc
#'
#' Sends a json to vsc
#'
#' @param body The body of the message. Must be convertible to JSON. Usually named lists.
#' 
#' @keywords internal
sendToVsc <- function(body = "", useCustomSocket = FALSE) {
  json <- getJson(body)
  success <- TRUE
  if(session$socketServer){
    msg <- makeDapMessage(json, '')
    logCat('\nSending to socket ', session$svName, ':\n', msg, '\n', sep='')
    svSocket::sendSocketClients(msg, session$svName, serverport = session$dapPort)
    success <- TRUE #?
  } else if(useCustomSocket){
    if(session$useCustomSocket){
      base::cat(json, '\n', sep='', file=session$customSocketConnection)
      logCat('Sent json (custom): ', json, '\n', sep='')
    } else{
      logCat('Not using custom socket!\n')
      success <- FALSE
    }
  } else if(!is.null(session$jsonSocketConnection)){
    base::cat(json, '\n', sep='', file=session$jsonSocketConnection)
    logCat('Sent json (json): ', json, '\n', sep='')
  } else if(!is.null(session$dapSocketConnection)){
    msg <- makeDapMessage(json)
    # This step should not be necessary, but the debugger does not work otherwise:
    # msg <- paste0(msg, '\r\n\r\n')
    msg <- paste0(msg)
    base::cat(msg, file=session$dapSocketConnection)
    logCat('Sent json (dap): ', json, '\n', sep='')
  } else{
    success <- FALSE
    logCat('Unknown sendToVsc target!\n')
  }
  return(success)
}

makeDapMessage <- function(json, cr = '\r'){
  json <- enc2utf8(json)
  Encoding(json) <- 'native'
  contentLength <- nchar(json, type = 'bytes')
  # saveRDS(json, 'json0.rds')
  # saveRDS(json, 'json.rds')
  # cL2 <- nchar(json)
  # logCat('CL: ', contentLength, ' - ', cL2, '\n', sep='')
  dap <- paste0(
    'Content-Length: ',
    toString(contentLength),
    cr,
    '\n',
    cr,
    '\n',
    json
  )
  # saveRDS(dap, 'msg.rds')
  return(dap)
}

getJson <- function(body){
  body <- removeNonJsonElements(body)
  s <- jsonlite::toJSON(body, auto_unbox = TRUE, force = TRUE)
}

removeNonJsonElements <- function(v){
  if(is.list(v)){
    v <- lapply(v, removeNonJsonElements)
    # remove NULL entries
    for(i in rev(seq_along(v))){
      if(is.null(v[[i]])){
        v[i] <- NULL
      }
    }
    return(v)
  } else{
    if(is.vector(v)){
      return(v)
    } else{
      return(NULL)
    }
  }
}





# used for debugging
logPrint <- function(...){
  if(getOption('vsc.doLogPrint', FALSE)){
    base::print(...)
  }
}

# used for debugging
logCat <- function(...){
  if(getOption('vsc.doLogPrint', FALSE)){
    base::cat(...)
  }
}



closeConnections <- function(){
  session$stopListeningOnPort <- TRUE
  if(!is.null(session$sinkSocketConnection)){
    while(sink.number() > session$sinkNumber){
      sink(NULL)
    }
    try(close(session$sinkSocketConnection), silent=TRUE)
    session$sinkSocketConnection <- NULL
  }
  if(!is.null(session$jsonSocketConnection)){
    try(close(session$jsonSocketConnection), silent=TRUE)
    session$jsonSocketConnection <- NULL
  }
  if(!is.null(session$dapSocketConnection)){
    try(close(session$dapSocketConnection), silent=TRUE)
    session$dapSocketConnection <- NULL
  }
  if(!is.null(session$customSocketConnection)){
    try(close(session$customSocketConnection), silent=TRUE)
    session$customSocketConnection <- NULL
  }
}

