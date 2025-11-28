

sourceRequest <- function(response, args, request){
  # Get source reference from args (two possible locations)
  ref <- lget(args, 'sourceReference', 0)
  if(ref==0){
    srcref <- lget(args$source, 'sourceReference', 0)
  }

  # Get content matching the ref
  content <- getStoredSourceContent(ref)
  if(content == ''){
    response$success <- FALSE
  } else{
    response$body <- list(
      content = content,
      mimeType = NULL
    )
    response$success <- TRUE
  }
  sendResponse(response)
}

locationsRequest <- function(response, args, request){
  ref <- lget(args, 'locationReference', 0)
  locationInfo <- getStoredLocationInfo(ref)
  if(is.null(locationInfo)){
    response$success <- FALSE
  } else{
    response$body <- locationInfo # Matches the required structure
    response$success <- TRUE
  }
  sendResponse(response)
}

getStoredLocationInfo <- function(ref){
  if(ref <= 0 || ref > length(session$locations)){
    return(NULL)
  }
  return(session$locations[[ref]])
}

getLocationReference <- function(srcref){
  locationInfo <- getLocationInfo(srcref)
  ref <- storeLocationInfo(locationInfo)
  return(ref)
}

storeLocationInfo <- function(locationInfo){
  if(is.null(locationInfo)){
    return(0)
  }
  ref <- length(session$locations) + 1
  session$locations[[ref]] <- locationInfo
  return(ref)
}

#' Gather info about a frame's source code
#' 
#' Gathers and returns a named list containing info about the name, path, and content of the source file of a frame
#' 
#' @param call call object as returned by `sys.call`
#' @param frameIdR A frame id (as used by R) that can be passed to `sys.call`
#' @return NULL or a LocationInfo object, matching entries for LocationsResponse.body and OutputEvent.body
#' 
#' @keywords internal
getLocationInfoForCall <- function(call, frameIdR = 0) {
  # get call if not provided
  if(is.null(call)){
    call <- sys.call(frameIdR)
  }

  # retrieve source information from call
  srcref <- attr(call, 'srcref')
  return(getLocationInfo(srcref))
}

#' Get location info from srcref attribute
#'
#' @return NULL or a LocationInfo object, matching entries for LocationsResponse.body and OutputEvent.body
#'
#' @keywords internal
getLocationInfo <- function(srcref){
  srcfile <- attr(srcref, 'srcfile')

  originalSrcfile <- srcfile$original
  filename <- lget(srcfile, 'filename', '')

  if(filename == '' && !is.null(originalSrcfile)){
    srcfile <- originalSrcfile
  }

  # return empty source if no source information found
  if(is.null(srcfile)){
    if(!is.null(srcref)){
      logPrint('found srcref but not srcfile!')
      logPrint(srcref)
    } else{
      logPrint('no srcfile!')
    }
    return(NULL)
  }

  # handle source information
  ret <- list(
    line = srcref[1],
    endLine = srcref[3],
    column = srcref[2],
    endColumn = srcref[4]
  )

  # validate path
  wd <- lget(srcfile, 'wd', '')
  path <- lget(srcfile, 'filename', '')
  if(path != ''){
    path <- normalizePathInWd(path, winslash = "/", mustWork = FALSE, wd = wd)
  }

  source <- list(
    name = basename(path)
  )
  if(file.exists(path)){
    source$name <- basename(path)
    source$path <- path
  } else{
    # Store content and assign reference
    content <- paste0(srcfile$lines, collapse='\n')
    if(identical(content, '')){
      return(NULL)
    }
    source$sourceReference <- storeSourceContent(content)
    # Make name if necessary
    if(source$name == ''){
      source$name <- strsplit(content, ' ')[[1]][1]
    }
  }
  ret$source <- source

  return(ret)
}

storeSourceContent <- function(content){
  # If content already stored, return existing ref
  for(i in seq_along(session$sourceContents)){
    if(identical(session$sourceContents[[i]], content)){
      return(i)
    }
  }
  # Store new content
  new_ref <- length(session$sourceContents) + 1
  session$sourceContents[[new_ref]] <- content
  return(new_ref)
}

getStoredSourceContent <- function(ref){
  if(ref <= 0 || ref > length(session$sourceContents)){
    return('')
  }
  return(session$sourceContents[[ref]])
}

normalizePathInWd <- function(path, winslash="\\", mustWork=FALSE, wd=NULL){
  if(is.null(wd)){
    ret <- normalizePath(path, winslash, mustWork)
  } else{
    ret <- tryCatch(
      {
        tmpwd <- setwd(wd)
        on.exit(setwd(tmpwd))
        normalizePath(path, winslash, mustWork)
      },
      error = function(e) path
    )
  }
  ret
}

#' Get the frame name of a given call
#'
#' Get the frame name of a given call
#' @keywords internal
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




#' Get the scopes corresponding to a frame
#' 
#' Gets the scopes corresponding to a frame
#' 
#' @param firstenv The top environment of the current frame
#' @param lastenv The last environment to be considered. By default .GlobalEnv, use emptyenv() to consider package environments
#' 
#' @keywords internal
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
  # used to make sure there are not duplicate variable names in a scope/parent var
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
