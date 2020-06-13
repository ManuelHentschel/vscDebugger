

sendResponse <- function(response){
  .vsc.sendToVsc(message = 'response', body = response)
}

prepareResponse <- function(request){
  response <- list(
    seq = 0,
    type = 'response',
    request_seq = request$seq,
    command = request$command,
    success = TRUE
  )
  return(response)
}

#' @export
.vsc.dispatchRequest <- function(request){
  response <- prepareResponse(request)
  command <- lget(request, 'command', '')
  args <- lget(request, 'arguments', list())
  if(command == 'stackTrace'){
    stackTraceRequest(response, args, request)
  } else if(command == 'stackTrace'){
    stackTraceRequest(response, args, request)
  } else if(command == 'scopes'){
    scopesRequest(response, args, request)
  } else if(command == 'variables'){
    variablesRequest(response, args, request)
  } else if(command == 'setVariable'){
    setVariableRequest(response, args, request)
  } else if(command == 'setExpression'){
    setExpressionRequest(response, args, request)
  } else if(command == 'evaluate'){
    evaluateRequest(response, args, request)
  } else {
    # ignore
  }
}

