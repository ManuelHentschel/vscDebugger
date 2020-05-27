

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


.packageEnv$breakpoints <- list()


#' @export
.vsc.setStoredBreakpoints <- function(){
    for(sbp in .packageEnv$breakpoints){
        sbp$bps <- .vsc.setBreakpoints(sbp$file, sbp$breakpoints, includePackages = sbp$includePackages)
    }
}


#' @export
.vsc.getBreakpointLines <- function(file, getActualLines = FALSE){
    bps <- .vsc.getBreakpoints(file)
    if(getActualLines){
        lines <- summarizeLists(bps)$line
    } else{
        lines <- summarizeLists(bps)$requestedLine
    }
    return(lines)
}

#' @export
.vsc.getAllBreakpoints <- function(){
    return(.packageEnv$breakpoints)
}

#' @export
.vsc.getBreakpoints <- function(file){
    allBps <- .packageEnv$breakpoints
    matchingBps <- allBps[which(lapply(allBps, function(sbp) sbp$file)==file)]
    if(length(matchingBps)>0){
        sbp <- mergeSrcBreakpoints(matchingBps)
        bps <- sbp[[1]]$breakpoints
    } else{
        bps <- list()
    }
    return(bps)
}

#' @export
.vsc.addBreakpoints <- function(file='', lines=list(), maxOffset=0, ids=NULL, includePackages=FALSE){
    if(!is.list(lines)){
        lines <- as.list(lines)
    }
    if(length(ids)==0){
        ids <- list(0)
    }
    if(length(ids)==1){
        ids <- lapply(lines, function(x) ids[[1]])
    }
    bps <- mapply(function(line, id) list(
        requestedLine=line,
        id = id,
        maxOffset = maxOffset,
        attempted = FALSE,
        verified = FALSE
    ), lines, ids, SIMPLIFY=FALSE, USE.NAMES=FALSE)
    sbp <- list(
        file = file,
        breakpoints = bps,
        includePackages = includePackages
    )
    .vsc.addBreakpoint(sbp)
}

#' @export
.vsc.addBreakpoint <- function(sbp=NULL, file=NULL, line=NULL, maxOffset=NULL, id=NULL, message=NULL, includePackages=NULL){
    if(length(sbp)==0){
        sbp <- list()
    }
    bp <- sbp$breakpoints[[1]]
    if(is.null(bp)){
        bp <- list()
    }
    if(!is.null(file)) sbp$file <- file

    if(!is.null(line)) bp$requestedLine <- line
    if(!is.null(maxOffset)) bp$maxOffset <- maxOffset
    if(!is.null(id)) bp$id <- id
    if(!is.null(message)) bp$message <- message
    if(is.null(bp$attempted)) bp$attempted <- FALSE
    if(is.null(bp$verified)) bp$verified <- FALSE

    sbp$breakpoints[[1]] <- bp

    addSrcBreakpoint(sbp)
    .packageEnv$breakpoints <- mergeSrcBreakpoints(.packageEnv$breakpoints)
}


#' @export
.vsc.clearAllBreakpoints <- function(){
    .packageEnv$breakpoints <- list()
}

#' @export
.vsc.clearBreakpointsByFile <- function(file=''){
    whichBreakpoints <- which(lapply(.packageEnv$breakpoints, function(bp) bp$file)==file)
    .packageEnv$breakpoints[whichBreakpoints] <- NULL
}

addSrcBreakpoints <- function(sbps=list()){
    .packageEnv$breakpoints <- c(.packageEnv$breakpoints, sbps)
}

addSrcBreakpoint <- function(sbp=NULL){
    addSrcBreakpoints(list(sbp))
}

mergeSrcBreakpoints <- function(sbps){
    sbpList <- lGroupBy(sbps, item='file')
    mergedBps <- lapply(sbpList, mergeSrcBreakpointList)
    sbps <- mergedBps
    return(sbps)
}

mergeSrcBreakpointList <- function(sbpList){
    if(length(sbpList)==0){
        return(sbpList)
    }
    bps <- lapply(sbpList, function(sbp) sbp$breakpoints)
    bps <- unlist(bps, recursive = FALSE)
    bps <- unique(bps)
    sbp <- sbpList[[1]]
    sbp$breakpoints <- bps
    return(sbp)
}


