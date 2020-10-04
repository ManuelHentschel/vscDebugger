

stackTraceRequest <- function(response, args, request){
  topFrameId <- getTopFrameId()
  skipFromBottom <- getSkipFromBottom()

  stackArgs <- list(
    nodeType = 'Stack',
    topFrameId = topFrameId,
    skipFromTop = 0,
    skipFromBottom = skipFromBottom,
    isError = FALSE,
    forceDummyStack = FALSE,
    dummyFile = '',
    refresh = TRUE
  )
  stackNode <- session$rootNode$getStackNode(stackArgs)

  frameNodes <- stackNode$getChildren()

  stackFrames <- lapply(frameNodes, function(node) node$getContent())

  # return:
  response[['body']] <- list(
    stackFrames = stackFrames
  )
  sendResponse(response)
}



scopesRequest <- function(response, args, request){
  # args:
  frameIdVsc <- args$frameId

  stackNode <- session$rootNode$getStackNode()
  frameNode <- stackNode$getChildren(args)
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

