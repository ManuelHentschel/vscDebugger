


setVariableRequest <- function(response, args, request){
  varRef <- lget(args, "variablesReference", 0)
  name <- lget(args, "name", "")
  value <- lget(args, "value", "")
  if(value == ""){
    response$success <- FALSE
  } else{
    args$findBy <- 'nameAndVarRef'

    node <- session$rootNode$findChildNode(args)
    if(is.null(node)){
      base::cat("<did not find node>", file=stderr())
      response$success <- FALSE
      sendResponse(response)
      return(NULL)
    }
    ancestorNodes <- node$getAncestors(TRUE)

    variables <- lapply(
      ancestorNodes,
      function(node) node$getContent(list(includeSetInfo=TRUE))
    )

    setInfos <- lapply(variables, function(variable) {
      setInfo <- variable$setInfo
      if(is.null(setInfo$setter)){
        setInfo$setter <- variable$setter
      }
      setInfo
    })

    successAndRValue <- setVar(setInfos, value)

    if(successAndRValue$success){
      node$updateValue(successAndRValue$rValue)
      response$body <- node$getContent()
    } else{
      response$success <- FALSE
      if(is.null(successAndRValue$reason)){
        base::cat("<Changing the variable value was unsuccessful>\n", file = stderr())
      } else{
        base::cat(successAndRValue$reason, file = stderr())
      }
    }
  }
  ret <- sendResponse(response)
  sendInvalidatedEvent(list('variables')) # update other variables to cover side effects
  invisible(ret)
}


setVar <- function(setInfos, valueString){
  sv <- substituteSetInfos(setInfos)
  target <- sv$expression
  env <- sv$environment
  rValue <- NULL
  reason <- NULL
  if(is.null(target) || is.null(env)){
    success <- FALSE
    reason <- "<No set-info available>\n"
  } else{
    err <- try({
      tmpTracingState <- tracingState(FALSE)
      rValue <- eval(parse(text=valueString), envir=env)
      cl <- as.call(list(`<-`, target, rValue))
      eval(cl)
      tracingState(tmpTracingState)
    })
    success <- !inherits(err, "try-error")
  }
  return(list(
    success = success,
    reason = reason,
    rValue = rValue
  ))
}

substituteSetInfos <- function(setInfos){
  if(length(setInfos) == 0){
    return(NULL)
  }
  env <- setInfos[[1]]$environment
  expr <- setInfos[[1]]$expression
  setter <- setInfos[[1]]$setter
  if(!is.null(env) && !is.null(setter)){
    expr <- setter
  } else if(!is.null(setter)){
    parent <- substituteSetInfos(setInfos[-1])
    if(!is.null(parent$expression) && !is.null(parent$environment)){
      env <- parent$environment
      expr <- do.call('substitute', list(setter, list(parent = parent$expression)))
    }
  } else{
    expr <- NULL
    env <- NULL
  }
  ret <- list(
    expression = expr,
    environment = env
  )
  return(ret)
}

