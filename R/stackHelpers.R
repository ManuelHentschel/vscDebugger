

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
  filename <- lget(srcfile, 'filename', '')

  if(filename == '' && !is.null(originalSrcfile)){
    srcfile <- originalSrcfile
  }

  if(is.null(srcfile)){
    return(getEmptySource())
  }

  wd <- srcfile$wd
  path <- srcfile$filename
  name <- basename(path)
  if(path != ''){
    path <- normalizePathInWd(path, winslash = "/", mustWork = FALSE, wd = wd)
  }

  srcbody <- paste0(srcfile$lines, collapse='\n')
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
        ret
      },
      error = function(e) path,
      finally = function(...) setwd(tmpwd)
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

varToStringWithCaptureOutput <- function(v) {
  # dirty way to convert anything to string
  # should be avoided!
  # TODO: replace with proper use of format(...)?
  ret <- try({
    paste0(capture.output(v), collapse = '\n')
  }, silent = getOption('vsc.trySilent', default=TRUE))
  if (inherits(ret, 'try-error')) {
    ret <- '???'
  }
  return(ret)
}




# getNewVarRef <- function(increment=TRUE){
#   if(increment){
#     session$varRef <- session$varRef + 1
#   }
#   session$varRef
# }


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


fixNames <- function(childVars){
  names <- lapply(childVars, function(var) var$name)
  inds <- which(duplicated(names) | names == "")
  while(length(inds)>0){
    for(ind in inds){
      newName <- paste0(names[[ind]], "<", ind, ">")
      childVars[[ind]]$name <- newName
      names[[ind]] <- newName
    }
    inds <- which(duplicated(names))
  }
  return(childVars)
}
