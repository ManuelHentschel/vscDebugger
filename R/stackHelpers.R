



####### BUILD STACK

#' Gather info about a frame's source code
#' 
#' Gathers and returns a named list containing info about the name, path, and content of the source file of a frame
#' 
#' @param frameIdR A frame id (as used by R) that can be passed to sys.call
#' @return A named list containing info about the source file
getSource <- function(call, frameIdR = 0) {
  if(is.null(call)){
    call <- sys.call(frameIdR)
  }

  srcref <- attr(call, 'srcref')
  srcfile <- attr(srcref, 'srcfile')

  originalSrcfile <- srcfile$original

  if(!is.null(originalSrcfile)){
    srcfile <- originalSrcfile
  }

  if(is.null(srcfile)){
    return(getEmptySource())
  }

  wd <- srcfile$wd
  path <- srcfile$filename
  name <- basename(path)
  path <- normalizePathInWd(path, winslash = "/", mustWork = FALSE, wd = wd)

  srcbody <- paste0(srcfile$lines, '## end of source body', collapse='\n')
  isFile <- file.exists(path)

  if(isFile){
    sourceReference <- 0
  } else{
    sourceReference <- frameIdR + 1
    path <- ''
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

normalizePathInWd <- function(path, winslash="\\", mustWork=NA, wd=NULL){
  if(is.null(wd)){
    ret <- normalizePath(path, winslash, mustWork)
  } else{
    ret <- tryCatch(
      {
        tmpwd <- setwd(wd)
        ret <- normalizePath(path, winslash, mustWork)
        setwd(tmpwd)
        ret
      },
      error = function(e) path
    )
  }
  ret
}

getEmptySource <- function(){
  list(
    name = '',
    path = '',
    sourceReference = 0,
    line = 0,
    endLine = 0,
    column = 0,
    endColumn = 0,
    srcbody = '',
    isFile = FALSE
  )
}


#' Get the frame name of a given call
#'
#' Get the frame name of a given call
getFrameName <- function(call) {
  name <- varToStringWithCaptureOutput(call)
  # name <- substr(name, 1, 16)
  maxChars <- 300
  if (nchar(name) > maxChars) {
    name <- paste0(substr(name, 1, maxChars - 3), '...')
  }
  return(name)
}



getNewVarRef <- function(){
  session$varRef <- session$varRef + 1
}


#' Get the number of frames
#'
#' Get the number of frames
#'
#' @param topFrame Consider only frames below this frame
getNFrames <- function(topFrame) {
  nFrames <- sys.nframe()
  while (!identical(sys.frame(nFrames), topFrame) && !identical(sys.frame(nFrames), .GlobalEnv)) {
    nFrames <- nFrames - 1
  }
  return(nFrames)
}


#' Get the scopes corresponding to a frame
#' 
#' Gets the scopes corresponding to a frame
#' 
#' @param firstenv The top environment of the current frame
#' @param lastenv The last environment to be considered. By default .GlobalEnv, use emptyenv() to consider package environments
getScopeEnvs <- function(firstenv = parent.frame(), lastenv = .GlobalEnv) {
  env <- firstenv
  scopes <- list(env)
  while (!identical(env, lastenv) && !identical(env, emptyenv())) {
    env <- parent.env(env)
    scopes[[length(scopes) + 1]] <- env
  }
  return(scopes)
}




######### GET STACK


storeFrameId <- function(node, R, vsc){
    session$frameIds <- list(
        node = append(session$frameIds$node, node),
        R = append(session$frameIds$R, R),
        vsc = append(session$frameIds$vsc, vsc)
    )
}

storeFrameIds <- function(frames, frameNodes){
  mapply(
    function(frameNode, frame){
      storeFrameId(
          node = frameNode,
          R = frame[['frameIdR']],
          vsc = frame[['id']]
      )
    },
    frameNodes,
    frames
  )

}

storeVarRef <- function(varRef, node){
  session$varRefs <- list(
      varRef = append(session$varRefs$varRef, list(varRef)),
      node = append(session$varRefs$node, list(node))
  )
}

storeVarRefs <- function(variables, variableNodes){
  mapply(
    function(node, variable){
      storeVarRef(
        node = node,
        varRef = variable$variablesReference
      )
    },
    variableNodes,
    variables
  )

}


getNodeId <- function(R = NULL, vsc = NULL, varRef=NULL){
  if(!is.null(varRef)){
    ind <- which(varRef == session$varRefs$varRef)
    if(length(ind)>0){
      return(session$varRefs$node[[ind[1]]])
    } else{
      return(0)
    }
  } else if (is.null(vsc) && is.null(R)) {
    return(0)
  } else if (is.null(vsc)) {
    ind <- which(R == session$frameIds$R)
    if (length(ind) > 0) {
      return(session$frameIds$node[[ind[1]]])
    } else {
      return(0)
    }
  } else {
    ind <- which(vsc == session$frameIds$vsc)
    if (length(ind) > 0) {
      return(session$frameIds$node[[ind[1]]])
    } else {
      return(0)
    }
  }
}




