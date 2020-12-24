

stackTraceRequest <- function(response, args, request){
  # args:
  startFrame <- lget(args, 'startFrame', 0)
  levels <- lget(args, 'levels', -1)

  topFrameId <- getTopFrameId()
  skipFromBottom <- getSkipFromBottom()
  if(getOption('vsc.showInternalFrames', FALSE)){
    frameIdsR <- seq2(topFrameId, (skipFromBottom + 1), -1) # vsc considers frames in the opposite order!
  } else{
    frameIdsR <- rev(getExternalFrames())
  }

  stackArgs <- list(
    nodeType = 'Stack',
    frameIdsR = frameIdsR,
    topFrameId = topFrameId,
    skipFromTop = 0,
    skipFromBottom = skipFromBottom,
    isError = FALSE,
    forceDummyStack = FALSE,
    dummyFile = '',
    refresh = (startFrame == 0)
  )
  stackNode <- session$rootNode$getStackNode(stackArgs)

  if(levels>=0){
    frameIds <- startFrame:(startFrame+levels)
    frameNodes <- stackNode$getChildren(list(frameIdsVsc = frameIds))
  } else{
    frameNodes <- stackNode$getChildren()
  }

  stackFrames <- lapply(frameNodes, function(node) node$getContent())

  # return:
  response[['body']] <- list(
    stackFrames = stackFrames,
    totalFrames = max(length(frameIdsR), 1)
  )
  sendResponse(response)
}



scopesRequest <- function(response, args, request){
  # args:
  frameIdVsc <- args$frameId

  stackNode <- session$rootNode$getStackNode()
  frameNode <- stackNode$getChildren(args)[[1]]
  scopeNodes <- frameNode$getChildren(list(refresh=TRUE))

  scopes <- lapply(scopeNodes, function(node) node$getContent())

  # return:
  response[['body']] <- list(
    scopes = scopes
  )

  sendResponse(response)
}

variablesRequest <- function(response, args, request){
  # args:
  varRef <- args$variablesReference

  variableNode <- session$rootNode$findChildNode(list(
    findBy = 'varRef',
    varRef = varRef
  ))

  if(is.null(variableNode)){
    logPrint("Variable not found. This is not supposed to happen.")
    response$success <- FALSE
    sendResponse(response)
    return(NULL)
  }

  variable <- variableNode$getContent()
  childrenNodes <- variableNode$getChildren(args)
  variables <- lapply(childrenNodes, function(node) node$getContent())
  response[['body']] <- list(variables = variables)
  sendResponse(response)
}

