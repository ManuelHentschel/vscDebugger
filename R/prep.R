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
#' @param catchErrors Whether to catch errors or let them be handled by `options(error = ...)`
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
    if(inherits(valueAndVisible, 'try-error')){
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
    if(inherits(valueAndVisible, 'try-error')){
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
  output <- paste(ret, sep = "", collapse = "\n")

  lineAndFile <- .vsc.getCallingLineAndFile(skipCalls = skipCalls+1, default = list(line=0, file=''))
  line <- lineAndFile$line
  file <- lineAndFile$file

  .vsc.sendToVsc('print', list(output = output, file = file, line = line))
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
  output <- paste(ret, sep = "", collapse = "\n")

  lineAndFile <- .vsc.getCallingLineAndFile(skipCalls = skipCalls+1, default = list(line=0, file=''))
  line <- lineAndFile$line
  file <- lineAndFile$file

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




#' Get Line and File info for a call
#' 
#' Get Line and File info for a call
#' 
#' @export
#' @param call The call for which to find line and file info
#' @param default The results defaults to this if an error is encountered
.vsc.getLineAndFile <- function(call, default=NULL){
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
  )}, error=function(e) default)
  return(ret)
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
  ret <- .vsc.getLineAndFile(sys.call(frameId - skipCalls), default)
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





#' Prep the r session for vsc
#'
#' @description
#' Preps the r session for runing the r debugger in vscode and reports back to vsc
#'
#' @export
#' @param overWritePrint Whether to overwrite `base::print` with a version that sends output to vsc
#' @param overWriteCat Whether to overwrite `base::cat` with a version that sends output to vsc
#' @param overwriteSource Whether to overwrite `base::source` with a version that supports breakpoints
#' @param findMain Whether to look for a main function
#' @param mainFunction Name of the main function
#' @param allowGlobalDebugging Whether to allow debugging after finishing a main function/script
#' @param rStrings List of unique strings used to communicate with vsc
#' @param id id of the response sent to vsc
.vsc.prepGlobalEnv <- function(
  overwritePrint = TRUE,
  overwriteCat = TRUE,
  overwriteSource = TRUE,
  findMain = TRUE,
  mainFunction = 'main',
  allowGlobalDebugging = FALSE,
  rStrings = NULL,
  id = 0
) {

  session$debugGlobal <- allowGlobalDebugging

  if(!is.null(rStrings)){
    session$rStrings <- rStrings
  } else{
    session$rStrings <- list(
      delimiter0 = '<v\\s\\c>',
      delimiter1 = '</v\\s\\c>',
      prompt = '<#v\\s\\c>', #actual prompt is followed by a newline to make easier to identify
      continue = '<##v\\s\\c>', #actual prompt is followed by a newline to make easier to identify
      startup = '<v\\s\\c\\R\\STARTUP>',
      libraryNotFound = '<v\\s\\c\\LIBRARY\\NOT\\FOUND>',
      packageName = 'vscDebugger',
      append = ' ### <v\\s\\c\\COMMAND>'
    )
  }

  options(prompt = paste0(session$rStrings$prompt, '\n'))
  options(continue = paste0(session$rStrings$continue, '\n'))
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
  .vsc.sendToVsc('go', id=id)
}

#' Look for a main function
#' 
#' Looks for main function and reports back to vsc
#' 
#' @export
#' @param mainFunction Name of the main function, usually "`main`"
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

#' Error handler
#' 
#' Error handler used by vsc. Set with `options(error = .vsc.onError)`
#' 
#' @export
#' @param err The message to be sent to vsc. Defaults to `geterrmessage()`
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
#' Returns `TRUE` iff an expression is being evaluated by the debugger during a breakpoint
#'
#' @export
#' @return Boolean indicating whether an expression is being evaluated
.vsc.isEvaluating <- function() {
  return(session$isEvaluating)
}

