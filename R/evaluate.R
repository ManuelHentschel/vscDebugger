
evaluateRequest <- function(response, args, request){
  # args
  expr <- lget(args, 'expression', '')
  frameId <- lget(args, 'frameId', 0)
  context <- lget(args, 'context', '')

  # do not evaluate clipboard context
  # -> uses the already known string representation from the stack tree
  if(context == 'clipboard' || expr == ''){
    response$success <- FALSE
    sendResponse(response)
    return(invisible(NULL))
  }

  # carriage returns are added e.g. by vscode on windows
  # cause errors in R
  if(getOption("vsc.ignoreCrInEval", TRUE)){
    expr <- gsub("\r", "", expr)
  }

  # determine the correct environment
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

  # store response. is sent if evaluation results in an error
  session$pendingEvalResponses <- c(list(response), session$pendingEvalResponses)

  # call actual 'workhorse' function
  isWatch <- (context == 'watch')
  valueAndVisible <- evalInEnv(
    expr = expr,
    env = env,
    showParseErrors = !isWatch,
    showOutput = !isWatch,
    deactivateTracing = isWatch || session$state$isError(),
    catchErrors = isWatch || session$state$isError() || !session$breakOnErrorFromConsole,
    showErrors = !isWatch
  )

  # remove response
  session$pendingEvalResponses[1] <- NULL

  # use print function if specified
  # (e.g. base::print for arrays, rather than structured eval response)
  if(valueAndVisible$visible && context != 'watch'){
    infos <- .vsc.applyVarInfos(valueAndVisible$value, infos = 'printFunc')
    printFunc <- infos$printFunc
    if(is.function(printFunc)){
      printFunc(valueAndVisible$value)
      valueAndVisible$visible <- FALSE
    } else if(identical(printFunc, FALSE)){
      valueAndVisible$visible <- FALSE
    }
  }

  # prepare response body 
  if(valueAndVisible$visible || context == 'watch'){
    # store result to stackTree
    args <- list(
      name = 'evalResult',
      rValue = valueAndVisible$value
    )
    node <- session$rootNode$getEvalRootNode()$addChild(args)
    variable <- node$getContent()

    response$body <- list(
      result = variable$value,
      type = variable$type,
      variablesReference = variable$variablesReference
    )
  } else{
    response$body <- list(
      result = "",
      type = "",
      variablesReference = 0
    )
  }

  if(!isWatch && session$assignToAns){
    assign('.ans', valueAndVisible$value, envir = globalenv())
  }

  sendResponse(response)
}


# helper function that does the work for eval requests
evalInEnv <- function(
  expr,
  env,
  showParseErrors,
  showOutput,
  deactivateTracing,
  catchErrors,
  showErrors,
  useBody = FALSE,
  body = NULL
){
  # check input
  if(deactivateTracing && !catchErrors) stop('invalid args')
  # other invalid combinations are silently ignored/treated unexpectedly

  # parse expr
  if(!useBody){
    body <- try(
      parse(text=expr),
      silent = !showParseErrors
    )
    if(inherits(body, 'try-error')){
      return(list(
        value = body,
        visible = FALSE,
        isError = TRUE,
        errorType = 'parseError'
      ))
    }
  }

  # return early if body is empty
  if(length(body) == 0){
    valueAndVisible <- list(
      value = NULL,
      visible = FALSE
    )
    return(valueAndVisible)
  }

  # change state
  prevState <- session$state$startRunning('eval', evalSilent = !showOutput)

  # deactivate tracing
  if(deactivateTracing){
    ts <- eval(quote(tracingState(FALSE)), envir=env)
    sendWriteToStdinEvent('c', when='browserPrompt', count=-1)
  }
    
  # eval
  if(catchErrors && !showOutput){
    registerLaunchFrame(8)
    # wrap in try(), withVisible(), capture.output()
    valueAndVisible <- try(
      {
        for(exp in body){
          cl <- call('withVisible', exp)
          capture.output(valueAndVisible <- eval(cl, envir=env))
        }
        valueAndVisible
      },
      silent = getOption('vsc.trySilent', default=TRUE)
    )
    unregisterLaunchFrame()
  } else if(catchErrors && showOutput){
    registerLaunchFrame(8)
    # wrap in try(), withVisible()
    valueAndVisible <- try(
      {
        for(exp in body){
          cl <- call('withVisible', exp)
          valueAndVisible <- eval(cl, envir=env)
        }
        valueAndVisible
      },
      silent = FALSE
    )
    unregisterLaunchFrame()
  } else{
    # wrap in withVisible()
    registerLaunchFrame(2)
    for(exp in body){
      cl <- call('withVisible', exp)
      valueAndVisible <- eval(cl, envir=env)
    }
    unregisterLaunchFrame()
  }

  # restore tracing
  if(deactivateTracing){
    eval(substitute(tracingState(ts), list(ts=ts)), envir=env)
    sendWriteToStdinEvent('', when='browserPrompt', count=0)
  }

  # restore state
  session$state$revert(prevState)
  
  # handle error caught by try()
  if(inherits(valueAndVisible, 'try-error') && catchErrors){
    valueAndVisible <- list(
      value=valueAndVisible,
      visible=FALSE,
      isError=TRUE,
      errorType='evalError'
    )
  }

  return(valueAndVisible)
}
