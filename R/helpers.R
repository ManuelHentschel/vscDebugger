
calledFromGlobal <- function() {
  # determine if a call was made from the command line (=global) or not
  # can be used inside other functions from this package

  # Make sure not to nest this line: (!!!)
  thisPackageEnv <- parent.env(environment())

  # check if the first call (stack frame) is to a funcion from this package:
  if (identical(parent.env(sys.frame(1)), thisPackageEnv)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
isPackageFrame <- function(env = parent.frame()) {
  while (!identical(env, emptyenv())) {
    if (identical(env, globalenv())) {
      return(FALSE)
    }
    env <- parent.env(env)
  }
  return(TRUE)
}


#' Modified version of `cat()` for vsc
#'
#' Captures the output of `cat(...)` and sends it to vsc together with information about the sourcefile and line
#' @export
#' @param ... Arguments passed to base::cat()
#' @param skipCalls The number of calls to skip when reporting the calling file and line. Can be used e.g. inside log functions.
#' @return NULL (invisible)
.vsc.cat <- function(..., skipCalls=0) {
  # TODO: consider correct environment for print(...)?
  # env <- sys.frame(-1)
  # ret <- capture.output(base::print(...), envir=env)

  if (session$isEvaluating || (!identical(list(...)$file, "") && !is.null(list(...)$file))) {
    # return(base::cat(...))
    return(base::cat(...))
  }
  ret <- capture.output(base::cat(...))
  printToVsc(ret, skipCalls+1)
  invisible(NULL)
}

#' Modified version of `print()` for vsc
#'
#' Captures the output of `print(...)` and sends it to vsc together with information about the sourcefile and line
#' @export
#' @param ... Arguments passed to `base::cat()`
#' @param skipCalls The number of calls to skip when reporting the calling file and line. Can be used e.g. inside log functions.
#' @return `NULL` (invisible)
.vsc.print <- function(x, ..., skipCalls=0) {
  # TODO: consider correct environment for print(...)?
  # env <- sys.frame(-1)
  # ret <- capture.output(base::print(...), envir=env)

  if (session$isEvaluating) {
    return(base::print(x, ...))
  }
  ret <- capture.output(base::print(x, ...))
  printToVsc(ret, skipCalls+1)
  invisible(x)
}

printToVsc <- function(ret, skipCalls=0){
  output <- paste(ret, "\n", sep = "", collapse = "\n")

  lineAndFile <- .vsc.getCallingLineAndFile(skipCalls = skipCalls+1, default = list(line=0, file=''))
  line <- lineAndFile$line
  file <- lineAndFile$path
  source <- list(path=file, name=basename(file))

  sendOutputEvent(category="stdout", output = output, line=line, source=source)
}


#' Send info about some vars to vsc
#'
#' Gathers info about the specified variablesReferences and sends them to vsc
#'
#' @export
#' @param refs A list of variableReferences, as specified in the scopes/previous variables
#' @param id The id of the message sent to vsc
#' @return None (The variable info, formatted as a nested named list is sent to vsc)
#'
.vsc.getVarLists <- function(refs, id = 0) {
  varLists <- makeVarLists(refs)
  .vsc.sendToVsc('variables', varLists, id)
}


#' Get Line and File info for a frameId
#' 
#' Get Line and File info for a frameId
#' 
#' @export
#' @param frameId The id of the frame as used by `sys.call()`
#' @param skipCalls Skip this number of frames (is substracted from `frameId`)
#' @param default The results defaults to this if an error is encountered
#' @return See `.vsc.getLineAndFile`
.vsc.getCallingLineAndFile <- function(frameId = 0, skipCalls = 0, default=NULL) {
  getSource(sys.call(frameId - skipCalls))
}


#' Send a message to vsc
#'
#' Sends a message (text) together with body and id to vsc
#'
#' @param message A string identifying the type of message
#' @param body The body of the message. Must be convertible to JSON. Usually named lists.
#' @param id The message id. Is usually provided in the function call from vsc.
#' @export
.vsc.sendToVsc <- function(message, body = "", id = 0) {
  if(session$useServer){
    j <- getJson(body)
    base::cat(j, '\n', sep='', file=session$serverConnection)
  } else {
    s <- .vsc.makeStringForVsc(message, body, id)
    base::cat(s)
  }
}

getJson <- function(body){
  body <- removeNonJsonElements(body)
  s <- jsonlite::toJSON(body, auto_unbox = TRUE, force = TRUE)
}


#' Prepare a message as string for vsc
#'
#' Prepare a message as string for vsc
#'
#' @param message A string identifying the type of message
#' @param body The body of the message. Must be convertible to JSON. Usually named lists.
#' @param id The message id. Is usually provided in the function call from vsc.
#' @return A (json) string that can be interpreted by vsc
.vsc.makeStringForVsc <- function(message, body = "", id = 0) {
  body <- removeNonJsonElements(body)
  l <- list(message = message, body = body, id = id)
  s <- jsonlite::toJSON(l, auto_unbox = TRUE, force = TRUE)
  r <- paste0(
    session$rStrings$delimiter0,
    s,
    session$rStrings$delimiter1,
    '\n'
  )
  return(r)
}

removeNonJsonElements <- function(v){
  if(is.list(v)){
    lapply(v, removeNonJsonElements)
  } else{
    if(is.vector(v)){
      v
    } else{
      ''
    }
  }
}


#' Error handler
#' 
#' Error handler used by vsc. Set with `options(error = .vsc.onError)`
#' 
#' @export
#' @param err The message to be sent to vsc. Defaults to `geterrmessage()`
.vsc.onError <- function(err=NULL) {
  session$isError <- TRUE
  if(is.null(err)){
    message <- geterrmessage()
  } else{
    attributes(err) <- list()
    message <- err
  }
  body <- list(message=message)
  # .vsc.sendToVsc('error', body)
  sendStoppedEvent('exception', description = 'Stopped on Exception', text = message)
  browser()
}

setErrorHandler <- function(useVscOnError = TRUE){
  if(useVscOnError){
    options(error = .vsc.onError)
  } else{
    options(error = traceback)
  }
}

#' Check if debugger is evaluating
#'
#' Returns `TRUE` iff an expression is being evaluated by the debugger during a breakpoint
#'
#' @export
#' @return Boolean indicating whether an expression is being evaluated
.vsc.isEvaluating <- function() {
  return(session$isEvaluating)
}



globalStepCallback <- function(...){
  registerEntryFrame()
  if(lget(session, 'ignoreNextCallback', FALSE)){
    session$ignoreNextCallback <- FALSE
  } else{
    if(calledFromGlobal()){
      session$isError <- FALSE
      setErrorHandler(session$breakOnErrorFromConsole)
      sendContinuedEvent()
      sendStoppedEvent(reason="step")
      .vsc.listenOnPort(timeout=2)
    } else{
      # do nothing?
    }
  }
  unregisterEntryFrame()
  TRUE
}