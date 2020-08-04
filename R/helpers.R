

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
  ret <- capture.output({base::cat(...);base::cat("\n")})
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
  ret <- c(ret, "")
  printToVsc(ret, skipCalls+1)
  invisible(x)
}

printToVsc <- function(ret, skipCalls=0){
  output <- paste0(ret, collapse = "\n")

  lineAndFile <- getSource(sys.call(-skipCalls))
  line <- lineAndFile$line
  file <- lineAndFile$path
  if(!identical(file, "")){
    source <- list(path=file, name=basename(file))
  } else{
    source <- NULL
  }

  sendOutputEvent(category="stdout", output = output, line=line, source=source)
}



#' Send a message to vsc
#'
#' Sends a message (text) together with body and id to vsc
#'
#' @param message A string identifying the type of message
#' @param body The body of the message. Must be convertible to JSON. Usually named lists.
#' @param id The message id. Is usually provided in the function call from vsc.
.vsc.sendToVsc <- function(body = "") {
  if(session$useJsonServer){
    j <- getJson(body)
    base::cat(j, '\n', sep='', file=session$jsonServerConnection)
  } else {
    s <- .vsc.makeStringForVsc(body)
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
#' @param body The body of the message. Must be convertible to JSON. Usually named lists.
#' @return A (json) string that can be interpreted by vsc
.vsc.makeStringForVsc <- function(body = "") {
  body <- removeNonJsonElements(body)
  s <- jsonlite::toJSON(body, auto_unbox = TRUE, force = TRUE)
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
  registerEntryFrame()
  session$isError <- TRUE
  if(is.null(err)){
    message <- geterrmessage()
  } else{
    attributes(err) <- list()
    message <- err
  }
  body <- list(message=message)
  sendStoppedEvent('exception', description = 'Stopped on Exception', text = message)
  browser()
  unregisterEntryFrame()
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
      # .vsc.listenOnPort(timeout=2)
    } else{
      # do nothing?
    }
  }
  unregisterEntryFrame()
  TRUE
}

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
