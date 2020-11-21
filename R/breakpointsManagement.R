
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
    if(is.null(envs)){
      envs <- getBreakpointEnvs()
    }

    # remove old bps
    if(!is.null(oldSbps)){
      setBreakpoints(
        oldSbps,
        unsetBreakpoints = TRUE,
        envs = envs
      )
    }

    # set new bps
    sbps <- setBreakpoints(
      sbps,
      unsetBreakpoints = FALSE,
      envs = envs
    )
  }
  return(sbps)
}

getBreakpointEnvs <- function(){
  # always serach globalenv:
  globalEnv <- list(globalenv())

  # search (non default) packages, if specified:
  packageEnvs <- list()
  if(session$setBreakpointsInPackages){
    env <- parent.env(globalenv())
    # default packages (base, utils, ...) -> ignore:
    defaultPackages <- getOption('defaultPackages')

    # go down the environment hierarchy and check/add each environment:
    while(!identical(env, emptyenv())){
      # check if env is the package environment of a default package:
      envName <- attr(env, 'name')
      if(!is.null(envName)){
        envName <- sub('package:', '', envName)
        if(envName %in% defaultPackages){
          break
        }
      }
      # add package to list and continue to parent.env
      packageEnvs <- c(packageEnvs, list(env))
      env <- parent.env(env)
    }
  }

  # explicitly add namespaces and exports of debugged packages:
  pkgNamespaces <- list()
  pkgExports <- list()
  for(pkg in session$debuggedPackages){
    try({
      env <- getNamespace(pkg)
      pkgNamespaces <- c(pkgNamespaces, list(env))
    })
    try({
      env <- as.environment(paste0('package:', pkg))
      pkgExports <- c(pkgExports, list(env))
    })
  }
  packageEnvs <- c(packageEnvs, pkgNamespaces, pkgExports)

  # add manually stored environments
  bpEnvs <- list()
  for(env in session$breakpointEnvironments){
    # check if env is still attached?
    bpEnvs <- c(bpEnvs, list(env))
  }

  # add environments from stack
  if(getOption('vsc.setBreakpointsInStack', TRUE)){
    externalFrames <- getExternalFrames()
    stackFrames <- lapply(externalFrames, sys.frame)
  } else{
    stackFrames <- list()
  }

  envs <- unique(c(globalEnv, packageEnvs, stackFrames, bpEnvs))
  return(envs)
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
