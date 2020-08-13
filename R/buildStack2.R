

makeRootNode <- function(){
  node <- makeNode(
    nodeDescription = "Root"
  )
}

makeStackNode <- function(makeStackArgs, makeFramesArgs, parent){
  node <- makeNode(
    parent = parent,
    nodeType = "Stack",
    contentArgs = makeStackArgs,
    childrenArgs = makeFramesArgs
  )
}
makeVariableNode <- function(makeVariableArgs, parent){
  node <- makeNode(
    parent = parent,
    nodeType = "Variable",
    contentArgs = makeVariableArgs
  )
  local(envir=node,{

    environment()
  })
}


makeContent <- function(args){
  return(list())
}

makeChildren <- function(args){
  node <- makeNode()
  return(list(node))
}

makeStack <- function(args){

}
makeFrames <- function(args){

  # read args
  topFrameId <- lget(args, 'topFrameId', sys.nframe()-1)
  skipFromTop <- lget(args, 'skipFromTop', 0)
  skipFromBottom <- lget(args, 'skipFromBottom', 0)
  forceDummyStack <- lget(args, 'forceDummyStack', FALSE)
  dummyFile <- lget(args, 'dummyFile', '')
  parent <- lget(args, 'parent', NULL)

  # do stuff

  if(topFrameId <= 0 || forceDummyStack){
    forceDummyStack <- TRUE
    frameIdsR <- c(0)
    frameIdsVsc <- c(0)
  } else{
    frameIdsR <- seq2((topFrameId - skipFromTop), (skipFromBottom + 1), -1) # vsc considers frames in the opposite order!
    frameIdsVsc <- seq_along(frameIdsR) - 1
  }

  makeFrameNode <- function(frameIdR, frameIdVsc){
    makeNode(
      parent = parent,
      nodeType = "Frame",
      childrenArgs = 
    )
  }
}
makeScopes <- function(args){}
makeVariables <- function(args){}

makeStack <- function(args){}
makeFrame <- function(args){}
makeScope <- function(args){}
makeVariable <- function(args){}


makeNode <- function(
  parent = NULL,
  nodeType = "Meta",
  nodeDescription = "",
  children = NULL,
  childrenArgs = NULL,
  childIndices = NULL,
  content = NULL,
  contentArgs = NULL,
) {
  node <- local(
    this <- environment()

    parent <- parent
    nodeType <- nodeType
    children <- children
    childrenArgs <- childrenArgs
    childIndices <- childIndices
    content <- content
    contentArgs <- contentArgs

    getChildren <- function(args){
      refresh <- lget(args, 'refresh', FALSE)
      if(refresh || is.null(this$children)){
        this$childrenArgs$parent <- this
        this$children <- this$makeChildren(this$childrenArgs)
      }
      return(this$children)
    }

    getContent <- function(args){
      refresh <- lget(args, 'refresh', FALSE)
      if(refresh || is.null(this$content)){
        this$content <- this$makeContent(this$contentArgs)
      }
      return(this$content)
    }

    getParent <- function(){
      this$parent
    }

    getAncestors <- function(includeThisNode = TRUE){
      if(is.null(this$parent)){
        ancestors <- list()
      } else{
        ancestors <- this$parent$getAncestors(includeThisNode = TRUE)
      }
      if(includeThisNode){
        ancestors <- c(list(this), ancestors)
      }
      return(ancestors)
    }

    
    environment()
  )
}
