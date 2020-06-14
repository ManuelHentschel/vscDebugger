

# Funtions to manage breakpoints from inside the R package
# Is necessary e.g. to use .vsc.debugSource() without specifying the breaklines on each call
# Is probably a bit over-complilcated for the current use cases.
# Might be necessary in more complex cases:
# - Adding/removing individual breakpoints during debugging (without resetting all other bps)
# - Verifying breakpoints during runtime (after function definition etc.)
# - Conditional breakpoints?
# - Setting/Getting breakpoints by line-range

# The breakpoints are actually set by .vsc.setBreakpoints() in ./breakpoints.R

# Structure of breakpoints is:
# interface srcBreakpoint {
#     file: string;
#     breakpoints: breakpoint[];
#     includePackages: boolean;
# }
# interface breakpoint {
#     requestedLine?: number;
#     line?: number; //ignore if verified==false
#     maxOffset?: number;
#     id?: number;
#     attempted: boolean; //default false
#     verified: boolean; //default false
#     message?: string;
#     rFunction?: rFunction; //only in R: function that contains the bp
#     rAt?: number[][]; //only in R: step that contains the bp
# }


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
  for (sbp in session$srcBreakpoints) {
    sbp$bps <- .vsc.setBreakpoints(sbp$file, sbp$breakpoints, includePackages = sbp$includePackages)
  }
}


# #' @export
# .vsc.getBreakpointLines <- function(file, getActualLines = FALSE) {
#   file <- normalizePath(file)
#   bps <- .vsc.getBreakpoints(file)
#   if (getActualLines) {
#     lines <- summarizeLists(bps)$line
#   } else {
#     lines <- summarizeLists(bps)$requestedLine
#   }
#   return(lines)
# }

#' @export
.vsc.getAllBreakpoints <- function() {
  return(session$fileBreakpoints)
}

# #' @export
# .vsc.getBreakpoints <- function(file) {
#   file <- normalizePath(file)
#   allBps <- session$srcBreakpoints
#   matchingBps <- allBps[which(lapply(allBps, function(sbp) sbp$file) == file)]
#   if (length(matchingBps) > 0) {
#     sbp <- mergeSrcBreakpoints(matchingBps)
#     bps <- sbp[[1]]$breakpoints
#   } else {
#     bps <- list()
#   }
#   return(bps)
# }

# #' @export
# .vsc.addBreakpoints <- function(file = '', lines = list(), maxOffset = 0, ids = NULL, includePackages = FALSE) {
#   file <- normalizePath(file)
#   if (!is.list(lines)) {
#     lines <- as.list(lines)
#   }
#   if (length(ids) == 0) {
#     ids <- list(0)
#   }
#   if (length(ids) == 1) {
#     ids <- lapply(lines, function(x) ids[[1]])
#   }
#   bps <- mapply(function(line, id) list(
#     requestedLine = line,
#     id = id,
#     maxOffset = maxOffset,
#     attempted = FALSE,
#     verified = FALSE
#   ), lines, ids, SIMPLIFY = FALSE, USE.NAMES = FALSE)
#   sbp <- list(
#     file = file,
#     breakpoints = bps,
#     includePackages = includePackages
#   )
#   .vsc.addBreakpoint(sbp)
# }

# #' @export
# .vsc.addBreakpoint <- function(sbp = NULL, file = NULL, line = NULL, maxOffset = NULL, id = NULL, message = NULL, includePackages = NULL) {
#   if(!is.null(file)){
#     file <- normalizePath(file)
#   }
#   if (length(sbp) == 0) {
#     sbp <- list()
#   }
#   bp <- sbp$breakpoints[[1]]
#   if (is.null(bp)) {
#     bp <- list()
#   }
#   if (!is.null(file)) sbp$file <- file

#   if (!is.null(line)) bp$requestedLine <- line
#   if (!is.null(maxOffset)) bp$maxOffset <- maxOffset
#   if (!is.null(id)) bp$id <- id
#   if (!is.null(message)) bp$message <- message
#   if (is.null(bp$attempted)) bp$attempted <- FALSE
#   if (is.null(bp$verified)) bp$verified <- FALSE

#   sbp$breakpoints[[1]] <- bp

#   addSrcBreakpoint(sbp)
#   session$srcBreakpoints <- mergeSrcBreakpoints(session$srcBreakpoints)
# }


# #' @export
# .vsc.clearAllBreakpoints <- function() {
#   session$srcBreakpoints <- list()
# }

# #' @export
# .vsc.clearBreakpointsByFile <- function(file = '') {
#   file <- normalizePath(file)
#   whichBreakpoints <- which(lapply(session$srcBreakpoints, function(bp) bp$file) == file)
#   session$srcBreakpoints[whichBreakpoints] <- NULL
# }

# addSrcBreakpoints <- function(sbps = list()) {
#   session$srcBreakpoints <- c(session$srcBreakpoints, sbps)
# }

# addSrcBreakpoint <- function(sbp = NULL) {
#   addSrcBreakpoints(list(sbp))
# }

# mergeSrcBreakpoints <- function(sbps) {
#   sbpList <- lGroupBy(sbps, item = 'file')
#   mergedBps <- lapply(sbpList, mergeSrcBreakpointList)
#   sbps <- mergedBps
#   return(sbps)
# }

# mergeSrcBreakpointList <- function(sbpList) {
#   if (length(sbpList) == 0) {
#     return(sbpList)
#   }
#   bps <- lapply(sbpList, function(sbp) sbp$breakpoints)
#   bps <- unlist(bps, recursive = FALSE)
#   bps <- unique(bps)
#   sbp <- sbpList[[1]]
#   sbp$breakpoints <- bps
#   return(sbp)
# }
