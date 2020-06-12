




# session$stackNode <- 1

# session$frameIds <- list(
#     vsc = list(),
#     R = list(),
#     node = list()
# )

# session$varRefs <- list(
#     varRef = list(),
#     node = list()
# )


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


sourceRequest <- function(response, args, request){

}

stackTraceRequest <- function(response, args, request){
  # args: none relevant

  # do stuff:
  stackNode <- .vsc.buildNewStack(topFrame = parent.frame(2))
  session$stackNode <- stackNode
  frameNodes <- getChildrenIds(stackNode)
  stackFrames <- getContents(frameNodes)

  # make sure the frameIds are linked to the corresponding nodeIds
  storeFrameIds(stackFrames, frameNodes)

  # return:
  response[['body']] <- list(
      stackFrames = stackFrames
  )

  sendResponse(response)
}


scopesRequest <- function(response, args, request){
  # args:
  frameIdVsc <- args$frameId

  # do stuff:
  frameNode <- getNodeId(vsc = frameIdVsc)
  scopeNodes <- getChildrenIds(frameNode)
  scopes <- getContents(scopeNodes)

  # make sure the variableReferences are linked to the corresponding nodeIds
  storeVarRefs(scopes, scopeNodes)

  # return:
  response[['body']] <- list(
    scopes = scopes
  )

  sendResponse(response)
}

variablesRequest <- function(response, args, request){
  # args:
  varRef <- args$variablesReference

  # do stuff:
  nodeId <- getNodeId(varRef = varRef)

  variableNodes <- getChildrenIds(nodeId)
  variables <- getContents(variableNodes)

  # make sure the variableReferences are linked to the corresponding nodeIds
  storeVarRefs(variables, variableNodes)

  # return:

  response[['body']] <- list(variables = variables)
  
  sendResponse(response)
}

setVariableRequest <- function(response, args, request){
    
}

setExpressionRequest <- function(response, args, request){
    
}

evaluateRequest <- function(response, args, request){
    
}

completionsRequest <- function(response, args, request){
    
}

readMemoryRequest <- function(response, args, request){
    
}
