########################################################################
# Prep

#' Evaluate an expression and send result to vsc
#'
#' Evaluates an expression in a given frameId and sends the result to vsc
#'
#' @export
#' @param expr The espression to be evaluated
#' @param frameId The Id of the frame (as given by vsc)
#' @param id The Id of the message sent to vsc
#' @param assignToAns Whether to assign the result of the evaluation to .GlobalEnv$.ans
.vsc.evalInFrame <- function(expr, frameId, silent = TRUE, id = 0, assignToAns = TRUE, catchErrors = TRUE) {
  # evaluate calls that were made from top level cmd line in the .GlobalEnv
  if (session$debugGlobal && calledFromGlobal()) {
    env <- .GlobalEnv
  } else {
    frameIdR <- convertFrameId(vsc = frameId)
    if(is.null(frameIdR)){
      env <- .GlobalEnv
    } else{
      env <- sys.frame(frameIdR)
    }
  }

  # prepare settings
  tmpDebugGlobal <- session$debugGlobal
  session$debugGlobal <- FALSE

  if(silent){
    # prepare settings
    options(error=traceback)
    session$isEvaluating <- TRUE
    ts <- tracingState(FALSE)

    # eval
    valueAndVisible <- try(
      {
        b <- parse(text=expr)
        for(exp in b){
          cl <- call('withVisible', exp)
          capture.output(valueAndVisible <- eval(cl, envir=env))
        }
        valueAndVisible
      },
      silent = getOption('vsc.trySilent', default=TRUE)
    )
    if(class(valueAndVisible) == 'try-error'){
      valueAndVisible <- list(value=valueAndVisible, visible=FALSE)
    }

    # reset settings
    tracingState(ts)
    session$isEvaluating <- FALSE
    options(error = .vsc.onError)
  } else{
    # eval
    if (catchErrors) {
      valueAndVisible <- try(
        {
          b <- parse(text=expr)
          for(exp in b){
            cl <- call('withVisible', exp)
            valueAndVisible <- eval(cl, envir=env)
          }
          valueAndVisible
        },
        silent = FALSE
      )
    } else {
      b <- parse(text=expr)
      for(exp in b){
        cl <- call('withVisible', exp)
        valueAndVisible <- eval(cl, envir=env)
      }
    }
    if(class(valueAndVisible) == 'try-error'){
      valueAndVisible <- list(value=valueAndVisible, visible=FALSE)
    }
  }

  # reset settings
  session$debugGlobal <- tmpDebugGlobal


  # prepare and send result
  if(valueAndVisible$visible || silent){
    ret <- getVariableForEval(valueAndVisible$value)
  } else {
    ret <- getEmptyVariableForEval()
  }

  # value <- paste(value, sep = "", collapse = "\n")
  .vsc.sendToVsc('eval', ret, id = id)

  # return value for further handling
  if(assignToAns && !silent){
    .GlobalEnv$.ans <- valueAndVisible$value
  }
  invisible(valueAndVisible$value)
}

calledFromGlobal <- function() {
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


#' Modified version of \code{cat()} for vsc
#'
#' Captures the output of \code{cat(...)} and sends it to vsc together with information about the sourcefile and line
#' @export
#' @param ... Arguments passed to base::cat()
#' @return NULL (invisible)
.vsc.cat <- function(...) {
  # TODO: consider correct environment for print(...)?
  # env <- sys.frame(-1)
  # ret <- capture.output(base::print(...), envir=env)

  if (session$isEvaluating || !identical(list(...)$file, "")) {
    # return(base::cat(...))
    return(base::cat(...))
  }
  ret <- capture.output(base::cat(...))
  output <- paste(ret, sep = "", collapse = "\n")

  line <- .vsc.getLineNumber(sys.call())
  frame <- parent.frame()
  call <- sys.call(-1)
  file <- .vsc.getFileName(call, frame)
  # output <- capture.output(base::print(...))
  .vsc.sendToVsc('print', list(output = output, file = file, line = line))
  invisible(NULL)
}

#' Modified version of \code{print()} for vsc
#'
#' Captures the output of \code{print(...)} and sends it to vsc together with information about the sourcefile and line
#' @export
#' @param ... Arguments passed to \code{base::cat()}
#' @return \code{NULL} (invisible)
.vsc.print <- function(x, ...) {
  # TODO: consider correct environment for print(...)?
  # env <- sys.frame(-1)
  # ret <- capture.output(base::print(...), envir=env)

  if (session$isEvaluating) {
    return(base::print(x, ...))
  }
  ret <- capture.output(base::print(x, ...))
  output <- paste(ret, sep = "", collapse = "\n")

  line <- .vsc.getLineNumber(sys.call())
  frame <- parent.frame()
  call <- sys.call(-1)
  file <- .vsc.getFileName(call, frame)
  # output <- capture.output(base::print(...))
  .vsc.sendToVsc('print', list(output = output, file = file, line = line))
  invisible(x)
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


#' Get Filename corresponding to a call in a given frame
#'
#' Get Filename corresponding to a call in a given frame
#'
#' @param call A function call (usually from the stack)
#' @param frame The corresponding frame
#' @return The filename
.vsc.getFileName <- function(call, frame) {
  fullPath <- try({
    if (identical(call[[1]], call('::', quote(vscDebugger), quote(.vsc.debugSource)))) {
      fileName <- call[[2]]
      fullPath <- normalizePath(fileName)
    } else {
      fileName <- getSrcFilename(eval(call[[1]], envir = frame))
      dirName <- getSrcDirectory(eval(call[[1]], envir = frame))
      dirName <- suppressWarnings(normalizePath(dirName, winslash = '/'))
      fullPath <- file.path(dirName, fileName)
      fullPath <- suppressWarnings(normalizePath(fullPath, winslash = '\\'))
      fullPath <- toString(fullPath)
    }
  }, silent = getOption('vsc.trySilent', default=TRUE))
  if (class(fullPath) == 'try-error') {
    fullPath <- ''
  }
  return(fullPath)
}


getLineAndFile <- function(call){
  ret <- tryCatch({
    srcref <- attr(call, 'srcref')
    srcfile <- attr(srcref, 'srcfile')
    srcbody <- paste0(srcfile$lines, collapse='\n')
    isFile <- srcfile$isFile

    ret <- list(
      wd = srcfile$wd,
      filename = srcfile$filename,
      line = srcref[1],
      endLine = srcref[3],
      column = srcref[2],
      endColumn = srcref[4],
      srcbody = srcbody,
      isFile = isFile
  )}, error=function(e) NULL)
  return(ret)
}

getCallingLineAndFile <- function(frameId = 0, skipCalls = 0) {
  call <- sys.call(frameId - skipCalls)
  return(getLineAndFile(call))
}


#' Get Line-number corresponding to a call in a given frame
#'
#' Get Line-number corresponding to a call in a given frame
#'
#' @param call A function call (usually from the stack)
#' @param frame The corresponding frame
#' @return The line-numer
.vsc.getLineNumber <- function(call) {
  lineAndFile <- getLineAndFile(call)
  ret <- lineAndFile$line
  return(ret)
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
  s <- .vsc.makeStringForVsc(message, body, id)
  # base::cat(s)
  base::cat(s)
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
  .vsc.delimiter0 <- '<v\\s\\c>' # hardcoded to avoid triggering vsc when e.g. stack is sent
  .vsc.delimiter1 <- '</v\\s\\c>' # should probably be solved more elegantly
  l <- list(message = message, body = body, id = id)
  s <- jsonlite::toJSON(l, auto_unbox = TRUE, force = TRUE)
  r <- paste0(
    .vsc.delimiter0,
    s,
    .vsc.delimiter1,
    '\n'
  )
  return(r)
}





#' Runs the \code{main()} function
#'
#' @description
#' HAS CHANGED! Following info not up to date!
#' Looks for a function \code{main()} in the global environment and runs it
#' Runs main() inside this function, hence \code{parent.frame()} and \code{parent.env()} etc. will behave differently
#'
#' @export
#' @param overWritePrint Whether to overwrite \code{base::print} with a version that sends output to vsc
#' @param overWriteCat Whether to overwrite \code{base::cat} with a version that sends output to vsc
.vsc.prepGlobalEnv <- function(overwritePrint = TRUE, overwriteCat = TRUE, overwriteSource = TRUE, findMain = TRUE, mainFunction = 'main', debugGlobal = FALSE) {

  session$debugGlobal <- debugGlobal

  options(prompt = "<#v\\s\\c>\n")
  options(continue = "<##v\\s\\c>\n")
  options(browserNLdisabled = TRUE)

  suppressPackageStartupMessages(loadNamespace("pryr"))

  attachList <- list()

  if (overwritePrint) {
    attachList$print <- .vsc.print
  }

  if (overwriteCat) {
    attachList$cat <- .vsc.cat
  }

  if (overwriteSource) {
    attachList$source <- .vsc.debugSource
  }

  attach(attachList, name = "tools:vscDebugger", warn.conflicts = FALSE)

  session$isEvaluating <- FALSE

  options(error = .vsc.onError)
  .vsc.sendToVsc('go')
}

#' @export
.vsc.lookForMain <- function(mainFunction='main'){
  foundMain <- FALSE
  # look for main():
  if(mainFunction %in% ls(globalenv())){
    if(typeof(get(mainFunction, envir=globalenv())) == 'closure'){
      foundMain <- TRUE
    }
  }
  # report back to vsc:
  if(foundMain){
    .vsc.sendToVsc('callMain')
  } else{
    .vsc.sendToVsc('noMain')
  }
  return(invisible(foundMain))
}

.vsc.onError <- function(err=NULL) {
  if(is.null(err)){
    message <- geterrmessage()
  } else{
    attributes(err) <- list()
    message <- err
  }
  body <- list(message=message)
  .vsc.sendToVsc('error', body)
  browser()
}


#' Check if debugger is evaluating
#'
#' Returns \code{TRUE} iff an expression is being evaluated by the debugger during a breakpoint
#'
#' @export
#' @return Boolean indicating whether an expression is being evaluated
.vsc.isEvaluating <- function() {
  return(session$isEvaluating)
}


#' Check if R should stop on breakpoint
#'
#' Returns \code{FALSE} iff an expression is being evaluated by the debugger during a (different) breakpoint, else TRUE
#'
#' @export
#' @return Boolean indicating whether R should stop on breakpoints
.vsc.stopOnBreakpoint <- function() {
  return(!session$isEvaluating)
}

#' Same as base::cat
#'
#' Same as base::cat
#' @export
.vsc.baseCat <- base::cat

#' Same as base::print
#'
#' Same as base::print
#' @export
.vsc.basePrint <- base::print
