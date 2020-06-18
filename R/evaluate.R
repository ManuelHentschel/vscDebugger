
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

  silent <- (context == 'watch')
  assignToAns <- session$assignToAns
  catchErrors <- !(session$breakOnErrorFromConsole)

  valueAndVisible <- .vsc.evalInFrame(
    expr,
    frameId,
    silent = silent,
    id = 0,
    assignToAns = assignToAns,
    catchErrors = catchErrors
  )

  if(valueAndVisible$visible || context == 'watch'){
    variableArgs <- list(
      nodeType = 'Variable',
      minVar = list(
        name = 'evalResult',
        rValue = valueAndVisible$value
      )
    )
    nodeId <- session$tree$storeToNewNode(list(contentArgs = variableArgs), session$rootNode)
    variable <- session$tree$getContent(nodeId)
    storeVarRef(node = nodeId, varRef = variable$variablesReference)

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
#' @export
#' @param expr The espression to be evaluated
#' @param frameId The Id of the frame (as given by vsc)
#' @param id The Id of the message sent to vsc
#' @param assignToAns Whether to assign the result of the evaluation to .GlobalEnv$.ans
#' @param catchErrors Whether to catch errors or let them be handled by `options(error = ...)`
.vsc.evalInFrame <- function(expr, frameId, silent = TRUE, id = 0, assignToAns = TRUE, catchErrors = TRUE) {
  # evaluate calls that were made from top level cmd line in the .GlobalEnv
  if (session$allowGlobalDebugging && calledFromGlobal()) {
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
  tmpallowGlobalDebugging <- session$allowGlobalDebugging
  session$allowGlobalDebugging <- FALSE

  if(silent){
    # prepare settings
    setErrorHandler(FALSE)
    session$isEvaluating <- TRUE
    ts <- tracingState(FALSE)

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
    tracingState(ts)
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

  # reset settings
  session$allowGlobalDebugging <- tmpallowGlobalDebugging
  setErrorHandler(session$breakOnErrorFromConsole)

  # assign to .ans
  if(assignToAns && !silent){
    .GlobalEnv$.ans <- valueAndVisible$value
  }
  
  return(valueAndVisible)
}
