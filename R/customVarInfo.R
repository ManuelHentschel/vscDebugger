

# type rValue = any;
# type NULL = undefined;
# interface namedList {names: string[], values: rValue[]};
# interface varInfo {
#     name: string;
#     doesApply: ((v: rValue) => boolean);
#     childVars: ((v:rValue) => namedList)|namedList|NULL;
#     customAttributes: ((v:rValue) => namedList)|namedList|NULL;
#     hasChildren: ((v:rValue) => boolean)|boolean|NULL;
#     toString: ((v:rValue) => string)|string|NULL;
#     shortType: ((v:rValue) => string)|string|NULL;
#     longType: ((v:rValue) => string)|string|NULL;
#     includeAttributes: ((v:rValue) => boolean)|boolean|NULL;
# }
# type varInfos = varInfo[];

# if childVars(v)$names==NULL, use names(childVars(v)$value) if given



# TODO: clean up and create sensible varInfos!!!!!


#' @export
.vsc.resetVarInfo <- function() {
  session$varInfo <- defaultVarInfo
}

#' @export
.vsc.clearVarInfo <- function() {
  session$varInfo <- list()
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
    position <- length(session$varInfo) + 1 + position
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
  varInfo$includeAttributes <- includeAttribute

  session$varInfo <- append(session$varInfo, varInfo, position)
}

#' @export
.vsc.removeVarInfo <- function(position = 1) {
  if (position < 0) {
    position <- length(session$varInfo) + 1 + position
  }
  session$varInfo[position] <- NULL
}

#' @export
.vsc.listVarInfo <- function(position = NULL) {
  if (is.null(position)) {
    position <- seq2(session$varInfo)
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
  varInfos <- session$varInfo
  varInfos <- lapply(seq(1, length(varInfos)), function(i){
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
  for(i in seq(length(varInfo))){
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
    for(i in seq(length(varInfo))){
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
