


setBreakpointsRequest <- function(response, args, request){

  fileBreakpoints <- requestArgsToFileBreakpoints(args)

  addOrUpdateFileBreakpoints(fileBreakpoints)

  response$body <- list(
    breakpoints = fileBreakpoints$breakpoints
  )

  sendResponse(response)
}


requestArgsToFileBreakpoints <- function(args){
  # not supported: args$lines
  path <- normalizePath(lget(args$source, 'path', ''))
  args$source$path <- path
  args$breakpoints <- lapply(args$breakpoints, sourceBreakpointToInternalBreakpoint, args$source)
  return(args)
}

sourceBreakpointToInternalBreakpoint <- function(bp, source){
  bp$id <- getNewBreakpointId()
  bp$verified <- FALSE
  bp$attempted <- FALSE
  bp$requestedLine <- bp$line
  bp$source <- source
  return(bp)
}


getNewBreakpointId <- function(){
  session$breakpointId <- session$breakpointId + 1 # returns the new value of session$breakpoint
}


addOrUpdateFileBreakpoints <- function(fileBreakpoints){
  path <- lget(fileBreakpoints$source, 'path', '')
  if(path == '') return(invisible(NULL))

  # remove previous fileBreakpoints:
  for(i in rev(seq_along(session$fileBreakpoints))){
    fbp <- session$fileBreakpoints[[i]]
    if(lget(fbp$source, 'path', '') == path){
      session$fileBreakpoints[[i]] <- NULL
    }
  }

  session$fileBreakpoints <- append(session$fileBreakpoints, list(fileBreakpoints))
  invisible(NULL)
}


getFileBreakpoints <- function(path){
  path <- normalizePath(path)
  for(fbp in session$fileBreakpoints){
    if(fbp$source$path == path){
      return(fbp)
    }
  }
  return(NULL)
}

#' @export
.vsc.getBreakpoints <- function(path){
  fbp <- getFileBreakpoints(path)
  breakpoints <- fbp$breakpoints
}

getRequestedBreakpointLines <- function(path){
  fbp <- getFileBreakpoints(path)
  lines <- lapply(fbp$breakpoints, function(bp) bp$requestedLine)
}

#' @export
.vsc.getBreakpointLines <- function(path, getActualLines = FALSE){
  if(getActualLines){
    fbp <- getFileBreakpoints(path)
    lines <- lapply(fbp$breakpoints, function(bp) bp$line)
  } else{
    lines <- getRequestedBreakpointLines(path)
  }
}


#' @export
.vsc.setStoredBreakpoints <- function() {
  for (fbp in session$fileBreakpoints){
    file <- lget(fbp$source, 'path', '')
    if(file != ''){
      bps <- lget(fbp, 'breakpoints', list())
      includePackageScopes <- lget(session, 'includePackageScopes', FALSE)
      .vsc.setBreakpoints(file, bps, includePackageScopes = includePackageScopes)
    }
  }
}


#' @export
.vsc.getAllBreakpoints <- function() {
  return(session$fileBreakpoints)
}
