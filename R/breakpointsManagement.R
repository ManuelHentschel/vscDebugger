
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


setStoredBreakpoints <- function(){
  session$sourceBreakpointsList <- lapply(
    session$sourceBreakpointsList,
    setSourceBreakpoints
  )
}

setSourceBreakpoints <- function(sbps, oldSbps=NULL){
  if(!session$noDebug){
    # get options
    includeAllPackages <- session$setBreakpointsInPackages
    additionalEnvs <- lapply(session$debuggedPackages, getNamespace)
    if(getOption('vsc.setBreakpointsInStack', TRUE)){
      externalFrames <- getExternalFrames()
      stackFrames <- lapply(externalFrames, sys.frame)
    } else{
      stackFrames <- list()
    }


    # remove old bps
    if(!is.null(oldSbps)){
      setBreakpoints(
        oldSbps,
        unsetBreakpoints = TRUE,
        includeAllPackages = includeAllPackages,
        additionalEnvs = additionalEnvs,
        stackFrames = stackFrames
      )
    }

    # set new bps
    sbps <- setBreakpoints(
      sbps,
      unsetBreakpoints = FALSE,
      includeAllPackages = includeAllPackages,
      additionalEnvs = additionalEnvs,
      stackFrames = stackFrames
    )
  }
  return(sbps)
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
