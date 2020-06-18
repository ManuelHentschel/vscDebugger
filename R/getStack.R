

stackTraceRequest <- function(response, args, request){
  # args: none relevant

  # do stuff:
  tree <- session$tree

  oldStackNode <- lget(session, 'stackNode', 0)
  if(oldStackNode>0){
    tree$deleteNode(oldStackNode)
  }

  stackNode <- .vsc.buildNewStack(topFrame = parent.frame(2))
  session$stackNode <- stackNode
  frameNodes <- tree$getChildrenIds(stackNode)
  stackFrames <- tree$getContents(frameNodes)

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
  tree <- session$tree
  frameNode <- getNodeId(vsc = frameIdVsc)
  scopeNodes <- tree$getChildrenIds(frameNode, refresh=TRUE)
  scopes <- tree$getContents(scopeNodes)

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
  tree <- session$tree
  nodeId <- getNodeId(varRef = varRef)

  variableNodes <- tree$getChildrenIds(nodeId, refresh=TRUE)
  variables <- tree$getContents(variableNodes)

  names(variables) <- NULL

  # make sure the variableReferences are linked to the corresponding nodeIds
  storeVarRefs(variables, variableNodes)

  # return:

  response[['body']] <- list(variables = variables)
  
  sendResponse(response)
}
