


setVariableRequest <- function(response, args, request){
  #args:
  varRef <- lget(args, "variablesReference", 0)
  name <- lget(args, "name", "")
  value <- lget(args, "value", "")
  if(value == ""){
    response$success <- FALSE
  } else{
    nodeId <- getNodeIdByVarRefAndName(varRef, name)
    ancestorIds <- session$tree$getAncestorIds(nodeId)
    variables <- session$tree$getContents(c(nodeId, ancestorIds))

    setInfos <- lapply(variables, function(variable) {
      setInfo <- variable$setInfo
      if(is.null(setInfo$setter)){
        setInfo$setter <- variable$setter
      }
      setInfo
    })

    successAndRValue <- setVar(setInfos, value)

    if(successAndRValue$success){
      newVariable <- updateVariableValue(nodeId, successAndRValue$rValue)
      response$body <- newVariable
    } else{
      response$success <- FALSE
      if(is.null(successAndRValue$reason)){
        cat("Changing the variable value was not successful.\n", file = stderr())
      } else{
        cat(successAndRValue$reason, file = stderr())
      }
    }
  }
  sendResponse(response)
}



setVar <- function(setInfos, valueString){
  sv <- substituteSetInfos(setInfos)
  target <- sv$expression
  env <- sv$environment
  rValue <- NULL
  reason <- NULL
  if(is.null(target) || is.null(env)){
    success <- FALSE
    reason <- "No set-info available.\n"
  } else{
    err <- try({
      rValue <- eval(parse(text=valueString), envir=env)
      cl <- as.call(list(`<-`, target, rValue))
      eval(cl)
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

substituteSetter <- function(setter, parent){
  do.call('substitute', list(setter, list(v = parent)))
}

