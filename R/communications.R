


# Can be used to dispatch requests via tcp socket instead of stdin
#' @export
.vsc.listenOnPort <- function(
  timeout=0,
  host='127.0.0.1',
  port=NULL,
  stopListening=FALSE
){
  if(is.null(port)){
    connection <- session$jsonSocketConnection
    host <- session$jsonHost
    port <- session$jsonPort
  } else{
    connection <- socketConnection(
      host = host,
      port = port,
      server = TRUE,
      open = "r+b"
    )
  }
  logCat('Start listening on ', host, ':', port, '\nTimeout: ', toString(timeout), '\n', sep='')
  session$stopListeningOnPort <- stopListening
  registerEntryFrame()
  t <- as.numeric(Sys.time())
  repeat{
    char <- readChar(connection, nchars=1)
    # char <- readChar(session$jsonSocketConnection, nchars=1)
    if(length(char)==0){
      if(timeout == 0 || (timeout > 0 && as.numeric(Sys.time()) - t > timeout)){
        break
      } else{
        Sys.sleep(0.01)
      }
    } else {
      if(char == '\n'){
        json <- session$restOfLine
        session$restOfLine <- ''
        logCat('Received json: ', json, '\n', sep='')
        .vsc.handleJson(json)
        if(session$stopListeningOnPort){
          break
        }
      } else{
        session$restOfLine <- paste0(session$restOfLine, char)
      }
      t <- as.numeric(Sys.time())
    }
  }
  logPrint('stop listening on port')
  unregisterEntryFrame()
}


#' @export
.vsc.listenForDAP <- function(
  host = session$dapHost,
  port = session$dapPort
){
  registerEntryFrame()
  if(is.null(session$dapSocketConnection)){
    conn <- socketConnection(
      host = host,
      port = port,
      server = TRUE,
      open = "r+b",
      blocking = FALSE
    )
    session$dapSocketConnection <- conn
  } else{
    conn <- session$dapSocketConnection
    sendStoppedEvent('step')
    session$state$startPaused('step')
    session$previousOptions <- options(session$internalOptions)
  }
  cat('Listening on ', host, ':', toString(port), '\n', sep='')
  session$stopListeningOnPort <- FALSE
  while(!session$stopListeningOnPort){
    header <- ''
    while(!endsWith(header, '\r\n\r\n')){
      char <- readChar(conn, nchars=1)
      if(length(char)==0){
        Sys.sleep(0.01)
      } else {
        header <- paste0(header, char)
      }
    }
    m <- regexec('Content-Length: (\\d+)\r\n', header)
    rm <- regmatches(header, m)
    contentLength <- as.numeric(rm[[1]][2])
    json <- readChar(conn, nchars=contentLength)
    # base::cat('Received json: ', json, '\n', sep='')
    .vsc.handleJson(json)
  }
  logPrint('stop listening on port')
  options(session$previousOptions)
  unregisterEntryFrame()
}



#' Sends a json to vsc
#'
#' Sends a json to vsc
#'
#' @param body The body of the message. Must be convertible to JSON. Usually named lists.
sendToVsc <- function(body = "") {
  j <- getJson(body)
  if(!is.null(session$jsonSocketConnection)){
    base::cat(j, '\n', sep='', file=session$jsonSocketConnection)
    logCat('Sent json: ', j, '\n', sep='')
    TRUE
  } else if(!is.null(session$dapSocketConnection)){
    contentLength <- nchar(j, type='bytes')
    msg <- paste0(
      'Content-Length: ',
      toString(contentLength),
      '\r\n\r\n',
      j
    )
    base::cat(msg, file=session$dapSocketConnection)
    # base::cat('Sent message:', msg, '\n')
    TRUE
  } else{
    FALSE
  }
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





#' Modified version of `base::cat()` for vsc
#'
#' Captures the output of `base::cat(...)` and sends it to vsc together with information about the sourcefile and line
#' @export
#' @param ... Arguments passed to base::cat()
#' @param skipCalls The number of calls to skip when reporting the calling file and line. Can be used e.g. inside log functions.
#' @return NULL (invisible)
.vsc.cat <- function(..., skipCalls=0, showSource=TRUE) {
  # TODO: consider correct environment for base::print(...)?
  # env <- sys.frame(-1)
  # ret <- capture.output(base::print(...), envir=env)

  if (session$state$isEvaluatingSilent()) {
    return(base::cat(...))
  }

  args <- list(...)
  if(identical(args$file, stderr())){
    args['file'] <- NULL
    category <- "stderr"
  } else if(is.null(args$file) || identical(args$file, '')){
    category <- "stdout"
  } else{
    return(base::cat(...))
  }

  ret <- capture.output({do.call(base::cat, args);base::cat("\n")})
  printToVsc(ret, skipCalls+1, category, showSource = showSource)
  invisible(NULL)
}

#' Modified version of `base::print()` for vsc
#'
#' Captures the output of `base::print(...)` and sends it to vsc together with information about the sourcefile and line
#' @export
#' @param ... Arguments passed to `base::cat()`
#' @param skipCalls The number of calls to skip when reporting the calling file and line. Can be used e.g. inside log functions.
#' @return `NULL` (invisible)
.vsc.print <- function(x, ..., skipCalls=0, showSource=TRUE) {
  # TODO: consider correct environment for base::print(...)?
  # env <- sys.frame(-1)
  # ret <- capture.output(base::print(...), envir=env)

  if (session$state$isEvaluatingSilent()) {
    return(base::print(x, ...))
  }
  ret <- capture.output(base::print(x, ...))
  ret <- c(ret, "")
  printToVsc(ret, skipCalls+1, showSource = showSource)
  invisible(x)
}

#' @export
.vsc.message <- function(..., domain = NULL, appendLF = TRUE, showSource=TRUE){
  args <- list(...)
  cond <- if (length(args) == 1L && inherits(args[[1L]], "condition")) {
    if (nargs() > 1L) {
      warning("additional arguments ignored in message()")
    }
    args[[1L]]
  } else {
    msg <- .makeMessage(..., domain = domain, appendLF = appendLF) # changed
    call <- sys.call()
    simpleMessage(msg, call)
  }
  defaultHandler <- function(c) {
    .vsc.cat(conditionMessage(c), file = stderr(), sep = "", skipCalls=5) # changed
  }
  withRestarts(
    {
      signalCondition(cond)
      defaultHandler(cond)
    },
    muffleMessage = function() NULL
  )
  invisible()
}

#' @export
.vsc.str <- function(object, ..., skipCalls=0, showSource=TRUE){
  args <- list(
    name = 'vscStrResult',
    rValue = list(object)
  )
  node <- session$rootNode$getEvalRootNode()$addChild(args)
  variable <- node$getContent()

  source <- getSource(sys.call(-skipCalls))
  line <- lget(source, 'line', 0)

  sendOutputEvent(
    output = "",
    category = "stdout",
    variablesReference = variable$variablesReference,
    source = source,
    line = line
  )

  invisible(NULL)
}


printToVsc <- function(ret, skipCalls=0, category="stdout", showSource=TRUE){
  output <- paste0(ret, collapse = "\n")

  if(showSource){
    source <- getSource(sys.call(-skipCalls))
    line <- lget(source, 'line', 0)
  } else{
    source <- NULL
    line <- NULL
  }

  sendOutputEvent(category, output = output, line=line, source=source)
}


# used for debugging
logPrint <- function(...){
  # base::print(...)
}

# used for debugging
logCat <- function(...){
  # base::cat(...)
}


