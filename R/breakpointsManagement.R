
setExceptionBreakPointsRequest <- function(response, args, request){
  filters <- lget(args, 'filters', list())
  session$breakOnErrorFromConsole <- ('fromEval' %in% filters)
  session$breakOnErrorFromFile <- ('fromFile' %in% filters)
  sendResponse(response)
}


setBreakpointsRequest <- function(response, args, request){

  # convert args to internal representation
  newSourceBreakpoints <- requestArgsToSourceBreakpoints(args)

  # update breakpoints
  oldSourceBreakpoints <- storeSourceBreakpoints(newSourceBreakpoints)

  # set breakpoints
  if(session$state$isStarted()){
    newSourceBreakpoints <- setSourceBreakpoints(newSourceBreakpoints, oldSourceBreakpoints)
  }

  # response
  response$body <- list(
    breakpoints = newSourceBreakpoints$breakpoints
  )
  sendResponse(response)
}


setStoredBreakpoints <- function(envs=NULL){
  session$sourceBreakpointsList <- lapply(
    session$sourceBreakpointsList,
    setSourceBreakpoints,
    envs=envs
  )
}

setSourceBreakpoints <- function(sbps, oldSbps=NULL, envs=NULL){
  if(!session$noDebug){
    includeAllPackages <- session$setBreakpointsInPackages
    if(is.null(envs)){
      # get additional envs
      inNormalEnvs <- TRUE
      tmp <- getBreakpointEnvs()
      additionalEnvs <- tmp$packageEnvs
      stackFrames <- tmp$stackFrames
    } else{
      inNormalEnvs <- FALSE
      additionalEnvs <- envs
      stackFrames <- list()
    }

    # remove old bps
    if(!is.null(oldSbps)){
      setBreakpoints(
        oldSbps,
        unsetBreakpoints = TRUE,
        includeAllPackages = includeAllPackages,
        additionalEnvs = additionalEnvs,
        stackFrames = stackFrames,
        inNormalEnvs = inNormalEnvs
      )
    }

    # set new bps
    sbps <- setBreakpoints(
      sbps,
      unsetBreakpoints = FALSE,
      includeAllPackages = includeAllPackages,
      additionalEnvs = additionalEnvs,
      stackFrames = stackFrames,
      inNormalEnvs = inNormalEnvs
    )
  }
  return(sbps)
}

getBreakpointEnvs <- function(){
  pkgNamespaces <- lapply(session$debuggedPackages, getNamespace)
  pkgExports <- lapply(session$debuggedPackages, function(pkg) {
    as.environment(paste0('package:', pkg))
  })
  packageEnvs <- c(pkgNamespaces, pkgExports)
  for(env in session$breakpointEnvironments){
    # check if env is still attached?
    packageEnvs <- c(packageEnvs, list(env))
  }
  if(getOption('vsc.setBreakpointsInStack', TRUE)){
    externalFrames <- getExternalFrames()
    stackFrames <- lapply(externalFrames, sys.frame)
  } else{
    stackFrames <- list()
  }
  return(list(
    packageEnvs = packageEnvs,
    stackFrames = stackFrames
  ))
}

storeBreakpointEnv <- function(...){
  for(env in list(...)){
    session$breakpointEnvironments <- c(
      session$breakpointEnvironments,
      list(env)
    )
  }
}


storeSourceBreakpoints <- function(sbps){
  for(i in rev(seq_along(session$sourceBreakpointsList))){
    oldSbps <- session$sourceBreakpointsList[[i]]
    if(oldSbps$source$path == sbps$source$path){
      session$sourceBreakpointsList[[i]] <- sbps
      return(oldSbps)
    }
  }
  # add sbps and return (mock) entry without breakpoints:
  session$sourceBreakpointsList <- c(session$sourceBreakpointsList, list(sbps))
  sbps$breakpoints <- list()
  return(sbps)
}

getSourceBreakpoints <- function(path){
  for(sbps in session$sourceBreakpointsList){
    if(sbps$source$path == path){
      return(sbps)
    }
  }
  return(list(
    source = list(path=path),
    breakpoints = list()
  ))
}


requestArgsToSourceBreakpoints <- function(args){
  path <- normalizePath(lget(args$source, 'path', ''), mustWork=FALSE)
  args$source$path <- path

  sourceBreakpointToInternalBreakpoint <- function(bp){
    bp$id <- getNewBreakpointId()
    bp$verified <- FALSE
    bp$attempted <- FALSE
    bp$requestedLine <- bp$line
    bp$source <- args$source
    return(bp)
  }

  args$breakpoints <- lapply(args$breakpoints, sourceBreakpointToInternalBreakpoint)
  return(args)
}


getNewBreakpointId <- function(){
  session$breakpointId <- session$breakpointId + 1 # returns the new value of session$breakpoint
}
