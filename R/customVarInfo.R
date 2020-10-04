
# TODO:
# User facing functions in this file need to be updated to match the new entries in VarInfo!

.vsc.applyVarInfos <- function(
  v,
  infos = character(0),
  stackingInfos = character(0),
  verbose = getOption('vsc.verboseVarInfos', FALSE),
  ind = NULL
) {
  # check args
  if(is.null(stackingInfos)) {
    stackingInfos <- c()
  } else if(is.list(stackingInfos)) {
    stackingInfos <- unlist(stackingInfos)
  }
  if(is.null(infos)) {
    infos <- c()
  } else if(is.list(infos)) {
    infos <- unlist(infos)
  }
  force(v)

  missingInfos <- c(infos, stackingInfos)
  names(missingInfos) <- missingInfos
  isStacking <- c(logical(length(infos)), !logical(length(stackingInfos)))
  names(isStacking) <- missingInfos
  ret <- list()

  if(verbose){
    logCat('\n\n\nApplying VarInfos to:\n')
    logPrint(v)
    logCat("\nMissing Infos:\n")
    logPrint(missingInfos)
    logCat("IsStacking:\n")
    logPrint(isStacking)
  }

  for (varInfo in session$varInfos) {
    # find missing infos that are supplied by this varInfo:
    matching <- intersect(missingInfos, names(varInfo))
    if(verbose) logCat("Checking ", varInfo$name, "...\n", sep="")
    applies <- toAtomicBoolean(varInfo$doesApply(v)) # safe conversion to atomic boolean
    if(applies){
      if(verbose) logCat("applies!\n")
      for(info in matching){
        # get and (if function) apply info:
        tmp <- varInfo[[info]]
        if(is.function(tmp)){
          valueAndError <- tryCatch(
            if(info=='childVars'){
              list(
                value = tmp(v, ind),
                isError = FALSE
              )
            } else{
              list(
                value = tmp(v),
                isError = FALSE
              )
            },
            error = function(e) {
              if(verbose){
                logPrint('error:')
                logPrint(e)
              }
              list(
                value = NULL,
                isError = TRUE
              )
            }
          )
          if(valueAndError$isError){
            next
          } else{
            tmp <- valueAndError$value
          }
        }
        if(verbose){
          logCat(info, " gives: ", sep="")
          logPrint(tmp)
        }
        # append or store result:
        if(is.null(tmp)){
          # ignore result
        } else if(isStacking[info]){
          ret[[info]] <- append(ret[[info]], list(tmp))
          # keep looking...
        } else{
          ret[[info]] <- tmp
          # remove from missing infos:
          ind <- which(missingInfos == info)
          missingInfos <- missingInfos[-ind]
          isStacking <- isStacking[-ind]
        }
      }
      if(length(missingInfos) == 0){
        break
      }
    }
  }
  ret
}

toAtomicBoolean <- function(v, ...){
  # outputs TRUE or FALSE
  # for nice input equivalent to `as.logical(v)[[1]]`
  # catches all errors, suppresses all warnings
  # able to handle any number of inputs, only considers 1st, returns FALSE if no arguments
  # able to handle e.g. NULL, c(), integer(0), list(), NA, NaN (all of these return FALSE)
  # if input throws an error and is lazy -> evaluated withing tryCatch -> returns FALSE
  suppressWarnings(
    tryCatch(
      if(as.logical(v)[[1]]){
        TRUE
      } else{
        FALSE
      },
      error = function(e) FALSE
    )
  )
}



.vsc.resetVarInfo <- function() {
  session$varInfos <- getDefaultVarInfos()
}

.vsc.clearVarInfo <- function() {
  session$varInfos <- list()
}

.vsc.addVarInfo <- function(
  name = '',
  doesApply = NULL,
  childVars = NULL,
  customAttributes = NULL,
  hasChildren = NULL,
  toString = NULL,
  shortType = NULL,
  longType = NULL,
  includeAttributes = NULL,
  varInfo = list(),
  position = 1
) {
  # start with empty varInfo if none given
  if (is.null(varInfo)) {
    varInfo <- list()
  }

  # check if there is a position given in varInfo
  if (!is.null(varInfo$position)){
    position <- varInfo$position
  }

  if (position < 0) {
    # negative positions count from the end, -1 = last position
    position <- length(session$varInfos) + 1 + position
  } else if (position > 0) {
    position <- position - 1 # position 1 == insert after 0
  }

  # add entries to varInfo (entires already in varInfo will be overwritten)
  varInfo$name <- name
  varInfo$doesApply <- doesApply
  varInfo$childVars <- childVars
  varInfo$customAttributes <- customAttributes
  varInfo$hasChildren <- hasChildren
  varInfo$toString <- toString
  varInfo$shortType <- shortType
  varInfo$longType <- longType
  varInfo$includeAttributes <- includeAttributes

  session$varInfos <- append(session$varInfos, list(varInfo), position)
}

.vsc.removeVarInfo <- function(position = 1) {
  if (position < 0) {
    position <- length(session$varInfos) + 1 + position
  }
  session$varInfos[position] <- NULL
}

.vsc.listVarInfo <- function(position = NULL) {
  if (is.null(position)) {
    position <- seq_along(session$varInfos)
  }
  varInfos <- .vsc.getAllVarInfos()
  varInfos <- lapply(position, function(pos) varInfos[pos])
  varInfos <- unlist(varInfos, recursive = FALSE)
  return(varInfos)
}

.vsc.getVarInfo <- function(positionOrName = NULL){
  if(is.null(positionOrName)){
    return(NULL)
  } else if(is.list(positionOrName)){
    return(.vsc.listVarInfo(positionOrName))
  } else if(is.vector(positionOrName) && length(positionOrName)>1){
    return(.vsc.listVarInfo(as.list(positionOrName)))
  }

  varInfos <- .vsc.getAllVarInfos()
  varInfo <- varInfos[[positionOrName]]
  return(varInfo)
}

.vsc.getAllVarInfos <- function(){
  varInfos <- session$varInfos
  varInfos <- lapply(seq_along(varInfos), function(i){
    vI <- varInfos[[i]]
    vI$position <- i
    vI
  })
  names(varInfos) <- lapply(varInfos, function(vI){
    if(is.null(vI$name)){
      paste0('varInfo', vI$pos)
    } else{
      vI$name
    }
  })
  return(varInfos)
}

applyTestVar <- function(varInfo, testVar){
  for(i in seq_along(varInfo)){
    if(is.function(varInfo[[i]])){
      varInfo[i] <- list(try(varInfo[[i]](testVar), silent=TRUE))
    }
  }
  return(varInfo)
}
