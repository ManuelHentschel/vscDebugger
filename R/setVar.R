


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
      .vsc.cat("<did not find node>", file=stderr(), showSource = FALSE)
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
        .vsc.cat("<Variable not set: internal error>\n", file = stderr(), showSource = FALSE)
      } else{
        .vsc.cat(successAndRValue$reason, file = stderr(), showSource = FALSE)
      }
    }
  }
  ret <- sendResponse(response)
  if(getOption('vsc.sendInvalidatedEventAfterSetVar', TRUE)){
    sendInvalidatedEvent(list('variables')) # update other variables to cover side effects
  }
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
    reason <- "<Variable not set: No set-info available>\n"
  } else{
    valueAndVisible <- evalInEnv(
      expr = valueString,
      env = env,
      showParseErrors = TRUE,
      showOutput = TRUE,
      deactivateTracing = TRUE,
      catchErrors = TRUE,
      showErrors = TRUE
    )
    if(lget(valueAndVisible, 'isError', FALSE)){
      success <- FALSE
      reason <- '<Variable not set: Error during evaluation of new value>\n'
    } else{
      rValue <- valueAndVisible$value
      cl <- as.call(list(`<-`, target, substitute(quote(rValue), list(rValue=rValue))))
      body <- list(cl)
      valueAndVisible2 <- evalInEnv(
        useBody = TRUE,
        body = body,
        env = environment(),
        showOutput = TRUE,
        deactivateTracing = TRUE,
        catchErrors = TRUE,
        showErrors = TRUE
      )
      success <- !lget(valueAndVisible2, 'isError', FALSE)
      reason <- ''
    }
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
      expr <- do.call(substitute, list(setter, list(parent = parent$expression)))
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

