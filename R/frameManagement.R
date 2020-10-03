

# Is used to avoid showing internal frames in the stack tree
registerEntryFrame <- function(skipCalls=0, entryFrames = NULL){
  if(is.null(entryFrames)){
    parentFrame <- sys.nframe()-1
    session$entryFrames <- c(session$entryFrames, parentFrame - skipCalls)
  } else{
    session$entryFrames <- entryFrames
  }
  invisible(session$entryFrames)
}

# Is used to avoid showing internal frames in the stack tree
unregisterEntryFrame <- function(all=FALSE){
  ret <- session$entryFrames
  n <- sys.nframe() - 1
  unregisterFrame(n, all)
  invisible(ret)
}

registerLaunchFrame <- function(skipCalls=0, launchFrames = NULL){
  if(is.null(launchFrames)){
    parentFrame <- sys.nframe()-1
    session$launchFrames <- c(session$launchFrames, parentFrame + skipCalls)
  } else{
    session$launchFrames <- launchFrames
  }
  invisible(session$launchFrames)
}

unregisterLaunchFrame <- function(all=FALSE){
  ret <- session$launchFrames
  n <- sys.nframe() - 1
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
  session$launchFrames <- lf
  session$entryFrames <- ef
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


#' Converts the frame id
#'
#' Converts the frame id form R to vsc or viceversa
#'
#' @param vsc The frame id as used by vsc
#' @param R The frame id as used by R
#' @return The frame id as used by the other program
convertFrameId <- function(vsc = NULL, R = NULL) {
  if (is.null(vsc) && is.null(R)) {
    return(NULL)
  } else if (is.null(vsc)) {
    frame <- session$rootNode$getStackNode()$getChildren(list(frameIdR=R))
    return(frame$frameIdVsc)
  } else {
    frame <- session$rootNode$getStackNode()$getChildren(list(frameId=vsc))
    return(frame$frameIdR)
  }
}
