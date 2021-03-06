

# Is used to avoid showing internal frames in the stack tree
registerEntryFrame <- function(skipCalls=0, entryFrames = NULL){
  if(is.null(entryFrames)){
    parentFrame <- sys.nframe()-1
    ef <- c(session$entryFrames, parentFrame - skipCalls)
  } else{
    ef <- entryFrames
  }
  session$entryFrames <- unique(sort(ef))
  invisible(session$entryFrames)
}

# Is used to avoid showing internal frames in the stack tree
unregisterEntryFrame <- function(skipCalls=0, all=FALSE){
  ret <- session$entryFrames
  n <- sys.nframe() - 1 - skipCalls
  unregisterFrame(n, all)
  invisible(ret)
}

registerLaunchFrame <- function(skipCalls=0, launchFrames = NULL){
  if(is.null(launchFrames)){
    parentFrame <- sys.nframe()-1
    lf <- c(session$launchFrames, parentFrame + skipCalls)
  } else{
    lf <- launchFrames
  }
  session$launchFrames <- unique(sort(lf))
  invisible(session$launchFrames)
}

unregisterLaunchFrame <- function(skipCalls=0, all=FALSE){
  ret <- session$launchFrames
  n <- sys.nframe() - 1 - skipCalls
  unregisterFrame(n, all)
  invisible(ret)
}

unregisterFrame <- function(upto=sys.nframe()-1, all=FALSE){
  lf <- session$launchFrames
  ef <- session$entryFrames
  if(all){
    lf <- c()
    ef <- c()
  } else{
    lf <- lf[lf < upto]
    ef <- ef[ef < upto]
  }
  session$launchFrames <- unique(sort(lf))
  session$entryFrames <- unique(sort(ef))
}

getSkipFromBottom <- function(){
  if(getOption('vsc.showInternalFrames', FALSE)){
    return(0)
  }
  suppressWarnings({
    lf <- min(session$launchFrames)
    ef <- min(session$entryFrames)
  })
  if(ef == 1 && lf < Inf){
    return(lf)
  } else{
    return(0)
  }
}

getTopFrameId <- function(){
  suppressWarnings({
    lf <- max(session$launchFrames)
    efs <- session$entryFrames
    ef <- max(efs)
  })
  if(ef > lf){
    while((ef - 1) %in% efs && ef > lf){
      ef <- ef - 1
    }
    if(ef>0){
      ef <- ef - 1
    }
  } else{
    ef <- sys.nframe() - 1
  }
  return(ef)
}

getExternalFrames <- function(){
  efs <- session$entryFrames
  lfs <- session$launchFrames

  n <- sys.nframe() - 1
  externalFrames <- logical(n)
  isExternal <- TRUE
  for(i in 1:n){
    # frame stays external, unless it's an entryframe
    isExternal <- isExternal && !(i %in% efs)

    # store isExternal
    externalFrames[i] <- isExternal

    # frame stays external or gets launched (-> external)
    isExternal <- isExternal || (i %in% lfs)
  }

  externalFrames <- which(externalFrames)

  return(externalFrames)
}


#' Converts the frame id
#'
#' Converts the frame id from R to vsc or viceversa
#'
#' @param vsc The frame id as used by vsc
#' @param R The frame id as used by R
#' @return The frame id as used by the other program
#' 
#' @keywords internal
convertFrameId <- function(vsc = NULL, R = NULL) {
  if (is.null(vsc) && is.null(R)) {
    return(NULL)
  } else if (is.null(vsc)) {
    frame <- session$rootNode$getStackNode()$getChildren(list(frameIdR=R))[[1]]
    return(frame$frameIdVsc)
  } else {
    frame <- session$rootNode$getStackNode()$getChildren(list(frameId=vsc))[[1]]
    return(frame$frameIdR)
  }
}
