


#' @export
.vsc.buildNewStack <- function(topFrame = parent.frame()){

  id0 <- session$rootNode
  tree <- session$tree

  stackArgs <- list(
    nodeType = 'Stack',
    topFrame = topFrame,
    skipFromTop = 0,
    skipFromBottom = 0,
    isError = FALSE,
    forceDummyStack = FALSE,
    dummyFile = '',

    contentProducesChildren = TRUE
  )
  nodeArgs = list(
    contentArgs = stackArgs
  )
  idStack <- tree$storeToNewNode(nodeArgs, id0)
  return(idStack)
}


childrenFunction <- function(args){
  # cat('\nChildrenArgs:\n')
  # print(args)
  nodeType <- lget(args, 'nodeType', '')
  if(nodeType == 'Frame'){
    ret <- gatherFrames(args)
    childType <- 'Scope'
  } else if(nodeType == 'Scope'){
    ret <- gatherScopes(args)
    childType <- 'Variable'
  } else if(nodeType == 'Variable'){
    ret <- gatherVariables(args)
    childType <- 'Variable'
  } else {
    stop("nodeType must be specified for children")
      # error?
  }

  # cat('Adding nodetype to children...')

  for(i in seq_along(ret)){
    if(!is.null(ret[[i]]$childrenArgs)){
      ret[[i]]$childrenArgs$nodeType <- childType
    }
    if(!is.null(ret[[i]]$contentArgs)){
      ret[[i]]$contentArgs$nodeType <- nodeType
    }
  }
  # cat('\nResult:\n')
  # print(ret)
  # cat('\n')
  ret
}


contentFunction <- function(args){
  # cat('\nContentArgs:\n')
  # print(args)
  # cat('\n')
  nodeType <- lget(args, 'nodeType', '')
  if(nodeType == 'Stack'){
    ret <- buildStack(args)
    childType <- 'Frame'
  } else if(nodeType == 'Frame'){
    ret <- buildFrame(args)
    childType <- 'Scope'
  } else if(nodeType == 'Scope'){
    ret <- buildVariable(args)
    childType <- 'Variable'
  } else if(nodeType == 'Variable'){
    ret <- buildVariable(args)
    childType <- 'Variable'
  } else {
    stop("nodeType must be specified for content")
  }

  # cat('Adding nodetype to content...')
  ret$contentContent$nodeType <- nodeType
  if(!is.null(ret$childrenArgs)){
    ret$childrenArgs$nodeType <- childType
  }
  # cat('\nResult:\n')
  # print(ret)
  # cat('\n')
  ret
}



# declare function buildStack(args: stackArgs): Stack;
# interface stackArgs extends ContentArgs {
# }
buildStack <- function(args){
  # read args
  topFrame <- lget(args, 'topFrame', globalenv())
  skipFromTop <- lget(args, 'skipFromTop', 0)
  skipFromBottom <- lget(args, 'skipFromBottom', 0)
  isError <- lget(args, 'isError', FALSE)
  forceDummyStack <- lget(args, 'forceDummyStack', FALSE)
  dummyFile <- lget(args, "dummyFile", '')

  # nothing to do

  # return
  stack <- list(
    # totalFrames = 0 # done later/not required
  )

  framesArgs <- args # StackArgs extends FramesArgs
  framesArgs$nodeType <- "Frame"

  list(
    contentContent = stack,
    childrenArgs = framesArgs
  )
}

# declare function buildFrame(args: FrameArgs): StackFrame;
# interface FrameArgs extends ContentArgs {
#     frameIdR: number;
#     frameIdVsc: number;
#     firstenv: REnvironment;
#     dummyFile?: string;
# }
buildFrame <- function(args){
  # read args
  frameIdR <- lget(args, 'frameIdR', list(0))
  frameIdVsc <- lget(args, 'frameIdVsc', list(0))
  dummyFile <- lget(args, 'dummyFile', '')
  isDummyFrame <- lget(args, 'isDummyFrame', FALSE)

  # do stuff
  lastenv <- globalenv()

  if(isDummyFrame){
    firstenv <- globalenv()
    name <- "Global Workspace"
    presentationHint <- 'label'
    source <- NULL
    line <- 0
    column <- 0
    endLine <- NULL
    endColumn <- NULL
  } else{
    call <- sys.call(frameIdR)
    firstenv <- sys.frame(frameIdR)
    name <- getFrameName(call)
    presentationHint <- 'normal'
    source <- getSource(sys.call(frameIdR+1), frameIdR+1)
    line <- source$line
    column <- source$column
    endLine <- source$endLine
    endColumn <- source$endColumn + 1
  }
  ###

  # return
  frame <- list(
    id = frameIdVsc,
    name = name,
    presentationHint = presentationHint,
    frameIdR = frameIdR
  )
  frame$source <- source
  frame$line <- line
  frame$column <- column
  frame$endLine <- endLine
  frame$endColumn <- endColumn

  scopesArgs <- list(
    firstenv = firstenv,
    lastenv = lastenv
  )

  list(
    contentContent = frame,
    childrenArgs = scopesArgs
  )
}

# declare function buildVariable(args: VariableArgs): Variable;
# interface VariableArgs extends ContentArgs {
#     rValue: RValue;
#     name: string;
# }
buildVariable <- function(args){
  # read args
  minVar <- lget(args, 'minVar', NULL)

  name <- lget(minVar, 'name', '')
  rValue <- lget(minVar, 'rValue', NULL)
  setter <- lget(minVar, 'setter', NULL)
  setInfo <- lget(minVar, 'setInfo', NULL)

  # do stuff
  infos <- .vsc.applyVarInfos(
    rValue,
    c(
      'toString',
      'type',
      'evaluateName',
      'hasChildren'
    )
  )

  hasChildren <- infos$hasChildren

  if(hasChildren){
    variablesReference <- getNewVarRef()
  } else{
    variablesReference <- 0
  }
  ###

  # return
  variable <- list(
    name = name,

    value = infos$toString,
    type = infos$type,
    evaluateName = infos$evaluateName,
    hasChildren = infos$hasChildren,

    setter = setter,
    setInfo = setInfo,

    rValue = rValue,
    variablesReference = variablesReference # is later matched with nodeId
  )

  variablesArgs = list(
    rValue = rValue
  )

  list(
    contentContent = variable,
    childrenArgs = variablesArgs
  )
}


# declare function gatherFrames(args: FramesArgs): {
#     contentArgs: FrameArgs
#     childrenArgs: ScopesArgs
# }[]
# interface FramesArgs extends ChildrenArgs {
#     topFrame: REnvironment;
#     skipFromTop?: number;
#     skipFromBottom?: number;
#     isError: boolean;
#     forceDummyStack?: boolean;
#     dummyFile?: string;
# }
# interface FrameArgs extends ContentArgs {
#     frameIdR: number;
#     frameIdVsc: number;
#     firstenv: REnvironment;
#     dummyFile?: string;
# }
# interface ScopesArgs extends ChildrenArgs {
#     firstenv: REnvironment;
#     lastenv: REnvironment;
# }
gatherFrames <- function(args){

  # read args
  topFrame <- lget(args, 'topFrame', globalenv())
  skipFromTop <- lget(args, 'skipFromTop', 0)
  skipFromBottom <- lget(args, 'skipFromBottom', 0)
  isError <- lget(args, 'isError', FALSE)
  forceDummyStack <- lget(args, 'forceDummyStack', FALSE)
  dummyFile <- lget(args, 'dummyFile', '')

  isError <- lget(session, 'isError', FALSE)

  # do stuff
  if (isError) {
    skipFromTop = skipFromTop + 1
  }

  nFrames <- getNFrames(topFrame)
  if(nFrames == 0 || forceDummyStack){
    forceDummyStack <- TRUE
    frameIdsR <- c(0)
    frameIdsVsc <- c(0)
  } else{
    frameIdsR <- seq2((nFrames - skipFromTop), (skipFromBottom + 1), -1) # vsc considers frames in the opposite order!
    frameIdsVsc <- seq2(length(frameIdsR)) - 1
  }

  # return
  makeNodeArgs <- function(frameIdR, frameIdVsc){
    frameArgs <- list(
      frameIdR = frameIdR,
      frameIdVsc = frameIdVsc,
      dummyFile = dummyFile,
      isDummyFrame = forceDummyStack
    )
    list(
      contentArgs = frameArgs
    )
  }

  nodeArgs <- mapply(
    makeNodeArgs,
    frameIdsR,
    frameIdsVsc,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
}

# declare function gatherScopes(args: ScopesArgs): {
#     contentArgs: ScopeArgs[];
# }
# interface ScopesArgs extends ChildrenArgs {
#     firstenv: REnvironment;
#     lastenv: REnvironment;
# }
gatherScopes <- function(args){
  # read args
  firstenv <- lget(args, 'firstenv', globalenv())
  lastenv <- lget(args, 'lastenv', globalenv())

  # do stuff
  scopeEnvs <- getScopeEnvs(firstenv, lastenv)
  ###

  # return
  nodeArgs <- lapply(scopeEnvs, function(env){
    list(
      contentArgs = list( # interface ScopeArgs
        minVar = list(
          rValue = env,
          name = format(env)
        )
      )
    )
  })
}

# declare function gatherVariables(args: VariablesArgs): {
#     contentArgs: VariableArgs;
# }[]
gatherVariables <- function(args){
  # print('gathering variables...')
  # read args
  rValue <- lget(args, 'rValue', NULL)

  # do stuff
  infos <- .vsc.applyVarInfos(
    rValue,
    infos = c('childVars', 'internalAttributes'),
    stackingInfos = 'customAttributes'
  )

  childVariables <- c(
    infos$childVars,
    infos$internalAttributes
  )

  customAttributes <- unlist(infos$customAttributes, recursive = FALSE)
  if(length(customAttributes)>0){
    childVariables <- c(
      childVariables,
      customAttributes
    )
  }


  # return
  makeNodeArgs <- function(v){
    variableArgs <- list(
      minVar = v
    )
    list(
      contentArgs = variableArgs
    )
  }

  # return
  nodeArgs <- lapply(childVariables, makeNodeArgs)
}