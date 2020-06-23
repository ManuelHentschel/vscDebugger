

stackTraceRequest <- function(response, args, request){
  # args: none relevant

  # do stuff:
  tree <- session$tree

  oldStackNode <- lget(session, 'stackNode', 0)
  if(oldStackNode>0){
    tree$deleteNode(oldStackNode)
  }

  stackNode <- .vsc.buildNewStack()
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

  scopes <- lapply(scopeNodes, function(nodeId){
    scope <- tree$getContent(nodeId)
    childCount <- length(tree$getChildrenIds(nodeId))
    scope$indexedVariables  <- childCount
    scope$namedVariables<- 0
    scope$expensive <- FALSE
    scope
  })

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
  start <- lget(args, 'start', 0)
  count <- lget(args, 'count', 0)

  # do stuff:
  tree <- session$tree
  nodeId <- getNodeId(varRef = varRef)

  variableNodes <- tree$getChildrenIds(nodeId, refresh=TRUE)

  if(count>0){
    ind <- (start+1):(start+count)
    variableNodes <- variableNodes[ind]
  }

  variables <- tree$getContents(variableNodes)

  variables <- lapply(variableNodes, function(nodeId){
    variable <- tree$getContent(nodeId)
    childCount <- length(tree$getChildrenIds(nodeId))
    variable$indexedVariables  <- childCount
    variable$namedVariables<- 0
    variable$expensive <- FALSE
    variable
  })

  names(variables) <- NULL

  # make sure the variableReferences are linked to the corresponding nodeIds
  storeVarRefs(variables, variableNodes)

  # return:

  response[['body']] <- list(variables = variables)
  
  sendResponse(response)
}


getChildCount <- function(nodeId){
  childCount <- length(getChildrenIds(nodeId))
}
