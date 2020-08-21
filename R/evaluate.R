
evaluateRequest <- function(response, args, request){
  # args
  expr <- lget(args, 'expression', '')
  frameId <- lget(args, 'frameId', 0)
  context <- lget(args, 'context', '')

  if(context == 'clipboard'){
    response$success <- FALSE
    sendResponse(response)
    return(invisible(NULL))
  }

  if(getOption("vsc.ignoreCrInEval", TRUE)){
    expr <- gsub("\r", "", expr)
  }

  silent <- (context == 'watch')
  assignToAns <- getOption('vsc.assignToAns', TRUE)
  catchErrors <- !(session$breakOnErrorFromConsole)
  deactivateTracing <- isCalledFromBrowser() || silent

  valueAndVisible <- .vsc.evalInFrame(
    expr,
    frameId,
    silent = silent,
    id = 0,
    assignToAns = assignToAns,
    catchErrors = catchErrors,
    deactivateTracing = deactivateTracing
  )

  if(
    valueAndVisible$visible
    && identical(class(valueAndVisible$value), 'help_files_with_topic')
  ){
    valueAndVisible$visible <- FALSE
    print(valueAndVisible$value)
  }

  if(valueAndVisible$visible || context == 'watch'){
    args <- list(
      name = 'evalResult',
      rValue = valueAndVisible$value
    )
    node <- session$rootNode$getEvalRootNode()$addChild(args)
    variable <- node$getContent()

    body <- list(
      result = variable$value,
      type = variable$type,
      variablesReference = variable$variablesReference
    )
  } else{
    body <- list(
      result = "",
      type = "",
      variablesReference = 0
    )
  }

  response$body <- body

  sendResponse(response)
}



#' Evaluate an expression and send result to vsc
#'
#' Evaluates an expression in a given frameId and sends the result to vsc
#'
#' @param expr The espression to be evaluated
#' @param frameId The Id of the frame (as given by vsc)
#' @param id The Id of the message sent to vsc
#' @param assignToAns Whether to assign the result of the evaluation to .GlobalEnv$.ans
#' @param catchErrors Whether to catch errors or let them be handled by `options(error = ...)`
.vsc.evalInFrame <- function(expr, frameId, silent = TRUE, id = 0, assignToAns = TRUE, catchErrors = TRUE, deactivateTracing = silent) {
  registerEntryFrame()
  # evaluate calls that were made from top level cmd line in the .GlobalEnv
  if (!isCalledFromBrowser()) {
    env <- .GlobalEnv
  } else {
    frameIdR <- convertFrameId(vsc = frameId)
    if(is.null(frameIdR)){
      env <- .GlobalEnv
    } else{
      env <- sys.frame(frameIdR)
    }
  }

  registerLaunchFrame(8)

  if(deactivateTracing){
    ts <- eval(quote(tracingState(FALSE)), envir=env)
    # ts <- tracingState(FALSE)
    sendCustomEvent('continueOnBrowserPrompt', list(value=TRUE))
  }

  if(silent){
    # prepare settings
    setErrorHandler(FALSE)
    session$isEvaluating <- TRUE

    # eval
    valueAndVisible <- list(value=NULL, visible=FALSE)
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
    session$isEvaluating <- FALSE
    setErrorHandler(session$breakOnErrorFromConsole)
  } else{
    # eval
    valueAndVisible <- list(value=NULL, visible=FALSE)
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
      setErrorHandler(TRUE)
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

  if(deactivateTracing){
    tracingState(ts)
    sendCustomEvent('continueOnBrowserPrompt', list(value=FALSE))
  }

  unregisterLaunchFrame()

  # assign to .ans
  if(assignToAns && !silent){
    .GlobalEnv$.ans <- valueAndVisible$value
  }
  
  unregisterEntryFrame()
  return(valueAndVisible)
}
