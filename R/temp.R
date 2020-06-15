



NO_buildStack <- function(args){
  # clearVarLists() # TODO: replace

  topFrame <- parent.frame(2) # skip frames from within vscDebugger
  skipFromTop <- 0
  skipFromBottom <- 0

  nFrames <- getNFrames(topFrame)
  frameIdsR <- seq2((nFrames - skipFromTop), (skipFromBottom + 1), -1) # vsc considers frames in the opposite order!
  frameIdsVsc <- seq2(length(frameIdsR)) - 1
  frames <- mapply(
    getStackFrame, frameIdsR, frameIdsVsc,
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  )
}


NO_buildDummyStack <- function(response, args){
  # clearVarLists() # TODO: replace
  
  file <- lget(args, "dummyFile", '')

  frames <- list(list(
    firstenv = globalenv(),
    variablesReference = getNewVarRef(),
    parent = getRootNode(),
    frameIdR = 0,
    id = 0,
    line = 0,
    column = 0
  ))

  response$body <- list(
    stackFrames = frames
  )
   .vsc.sendToVsc('response', response)
}

getRootNode <- function(){
  list(
    variablesReference = 0,
    name = 'Root',
    depth = 0
  )
}


NO_getStackFrame <- function(frameIdR, frameIdVsc){
  firstenv <- sys.frame(frameIdR)
  call <- sys.call(frameIdR)
  name <- getFrameName(call)
  lineAndFile <- getLineAndFile(call, frameIdR)

  tmpFrame <- list(firstenv = firstenv)
  class(tmpFrame) <- '.vsc.frame'
  variablesReference <- getVarRefForVar(tmpFrame)


  ret <- list(
    id = frameIdVsc,
    name = name,
    source = lineAndFile,
    line = lineAndFile$line,
    column = lineAndFile$column,
    endLine = lineAndFile$endLine,
    endColumn = lineAndFile$endColumn,
    firstenv = firstenv
  )
}


NO_getLineAndFile <- function(call = 0, frameIdR = 0, fallback = list()) {
  if(is.null(call)){
    call <- sys.call(frameIdR)
  }

  srcref <- attr(call, 'srcref')
  srcfile <- attr(srcref, 'srcfile')
  
  if(is.null(srcfile)){
    return(fallback)
  }

  path <- srcfile$filename
  name <- basename(path)
  dir <- dirname(path)
  path <- file.path(dir, name, fsep='/')

  if(path != normalizePath(path, winslash = "/", mustWork = FALSE)){
    # turn relative path into absolute path
    path <- file.path(wd, path)
  }

  srcbody <- paste0(srcfile$lines, collapse='\n')
  isFile <- file.exists(path)

  if(isFile){
    sourceReference <- 0
  } else{
    sourceReference <- frameIdR + 1
  }

  ret <- list(
    name = name,
    path = path,
    sourceReference = sourceReference,
    line = srcref[1],
    endLine = srcref[3],
    column = srcref[2],
    endColumn = srcref[4],
    srcbody = srcbody,
    isFile = isFile
  )
}




###############################





NO_getVariable <- function(valueR, parentId, nodeId, nodeType){
  content <- list(
    # meta info
    nodeId = nodeId,
    parentId = parentId,
    children = list(),
    nodeType = nodeType,


    # info
    valueR = valueR,
    
    name = name(valueR),
    evaluateName = evalname(valueR),
    presentationHint = presHint(valueR),





  )
}
