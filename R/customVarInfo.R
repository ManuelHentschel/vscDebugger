

# type rValue = any;
# type NULL = undefined;
# interface namedList {names: string[], values: rValue[]};
# interface varInfo {
#     name: string;
#     doesApply: ((v: rValue) => boolean);
#     childVars: ((v:rValue) => namedList)|namedList|NULL;
#     customAttributes: ((v:rValue) => namedList)|namedList|NULL;
#     internalAttributes: ((v:rValue) => namedList)|namedList|NULL;
#     hasChildren: ((v:rValue) => boolean)|boolean|NULL;
#     toString: ((v:rValue) => string)|string|NULL;
#     shortType: ((v:rValue) => string)|string|NULL;
#     longType: ((v:rValue) => string)|string|NULL;
#     includeAttributes: ((v:rValue) => boolean)|boolean|NULL;
#     evaluateName: ((v:rValue) => string)|string|NULL;
# }
# type varInfos = varInfo[];

# if childVars(v)$names==NULL, use names(childVars(v)$value) if given




#' @export
.vsc.getCustomInfo <- function(v, info, default = NULL, onError = NULL, append = FALSE, appendNested = FALSE) {
  # checks the entries in session$varInfos for a matching entry
  # returns the requested info if available
  # info can be a string from the list:
  #     childVars
  #     customAttributes
  #     internalAttributes
  #     hasChildren
  #     toString
  #     shortType
  #     longType
  #     includeAttributes
  #     evaluateName

  # make sure default is a list, if append==TRUE
  if(append && !is.list(default)){
    ret <- list()
    ret[[1]] <- default # does nothing if default==NULL
  } else{
    ret <- default
  }

  try({
    # loop through varInfos
    for (varInfo in session$varInfos) {
      # check if varInfo provides the required info
      if (!is.null(varInfo[[info]])) {
        # check if varInfo applies to v
        applies <- tryCatch(
          applies <- varInfo$doesApply(v)[[1]],
          error = function(e) FALSE
        )
        if (applies){
          if (is.function(varInfo[[info]])) {
            # apply function to v
            ret2 <- varInfo[[info]](v)
          } else {
            # ...or return (constant) value
            ret2 <- varInfo[[info]]
          }

          if (appendNested) {
            # append nested. Used if e.g.:
            # ret = list(values=list(1,2,3), names=list('a', 'b', 'c'))
            # ret2 = list(values=list(4), names=list('d'))
            ret <- appendNested(ret, ret2)
          } else if (append){
            # append to results and continue looking
            ret <- append(ret, ret2)
          } else{
            # return only this result
            ret <- ret2
            break
          }
        }
      }
    }
  }, silent = getOption('vsc.trySilent', default=TRUE))
  if (inherits(ret, 'try-error')) {
    return(onError)
  } else {
    return(ret)
  }
}


#' @export
.vsc.resetVarInfo <- function() {
  session$varInfos <- getDefaultVarInfos()
}

#' @export
.vsc.clearVarInfo <- function() {
  session$varInfos <- list()
}

#' @export
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

#' @export
.vsc.removeVarInfo <- function(position = 1) {
  if (position < 0) {
    position <- length(session$varInfos) + 1 + position
  }
  session$varInfos[position] <- NULL
}

#' @export
.vsc.listVarInfo <- function(position = NULL) {
  if (is.null(position)) {
    position <- seq2(session$varInfos)
  }
  varInfos <- .vsc.getAllVarInfos()
  varInfos <- lapply(position, function(pos) varInfos[pos])
  varInfos <- unlist(varInfos, recursive = FALSE)
  return(varInfos)
}

#' @export
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

#' @export
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

#' @export
.vsc.checkVarInfo <- function(varInfo, testCase, verbose=TRUE) {
  err <- list()
  warn <- list()
  remark <- list()


  mustNotBeNull <- list('doesApply', 'name')
  shouldBeFunction <- list('doesApply', 'customAttributes', 'childVars', 'toString')

  retMustBeString <- list('name', 'shortType', 'longType', 'toString')
  retMustBeList <- list('childVars', 'customAttributes')
  retMustBeBoolean <- list('hasChildren', 'doesApply')


  for(name in mustNotBeNull){
    if(is.null(varInfo[[name]])){
      err <- c(err, paste0(name, ' must not be NULL'))
    }
  }

  for(name in shouldBeFunction){
    if(!is.null(varInfo[[name]]) && !is.function(varInfo[[name]])){
      warn <- c(warn, paste0(name, ' should be a function'))
    }
  }

  if (missing(testCase)){
    warn <- c(warn, 'No test variable supplied. Could not check return values.')
    results <- NULL
  } else{
    results <- applyTestVar(varInfo, testCase)
      if(!identical(results$doesApply, TRUE)){
        warn <- c(warn, 'doesApply should return TRUE for the test case.')
      }
    for(i in seq_along(varInfo)){
      name <- names(varInfo)[i]
      entry <- varInfo[[i]]
      ret <- results[[i]]
      if(inherits(ret, 'try-error')){
        err <- c(err, paste0(name, ' causes an error.'))
      } else if(name %in% retMustBeBoolean && !(is.character(ret) && is.atomic(ret))){
        err <- c(err, paste0(name, ' must return (or be) an atomic logical value.'))
      } else if(name %in% retMustBeList && !is.list(ret)){
        err <- c(err, paste0(name, ' must return (or be) a list. Do not use NULL instead of list().'))
      } else if(name %in% retMustBeString && !(is.logical(ret) && is.atomic(ret))){
        err <- c(err, paste0(name, ' must return (or be) an atomic character vector.'))
      }
    }
  }

  if(verbose){
    cat('ERRORS:\n')
    for(er in err){
      cat(' - ', er, '\n')
    }
    cat('\nWARNINGS:\n')
    for(wa in warn){
      cat(' - ', wa, '\n')
    }
    cat('\nREMARKS:\n')
    for(rem in remark){
      cat(' - ', rem, '\n')
    }
  }

  return(list(err=err, warn=warn, remark=remark, results=results))
}
