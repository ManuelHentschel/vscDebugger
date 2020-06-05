

# type rValue = any;
# type NULL = undefined;
# interface namedList {names: string[], values: rValue[]};
# interface varInfo {
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
      } else if(name %in% retMustBeBoolean && !isAtomicBoolean(ret)){
        err <- c(err, paste0(name, ' must return (or be) an atomic logical value.'))
      } else if(name %in% retMustBeList && !is.list(ret)){
        err <- c(err, paste0(name, ' must return (or be) a list. Do not use NULL instead of list().'))
      } else if(name %in% retMustBeString && !isAtomicString(ret)){
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

isAtomicBoolean <- function(v) is.logical(v) && is.atomic(v)
isAtomicString <- function(v) is.character(v) && is.atomic(v)


getVarsInEnv <- function(env, all.names = TRUE) {
  names <- ls(env, all.names = all.names)
  vars <- lapply(names, getVarInEnv, env)
  return(list(
    values = vars,
    names = names
  ))
}

defaultVarInfo <- list(
  # NULL
  list(
    name = 'NULL',
    doesApply = is.null,
    childVars = list(),
    hasChildren = FALSE,
    shortType = '',
    longType = 'NULL',
    toString = 'NULL'
  ),
  # promise (custom type)
  list(
    name = 'Promise',
    doesApply = function(v) inherits(v, '.vsc.promise'),
    childVars = list(),
    shortType = '',
    longType = 'promise',
    toString = function(v) v$promiseCode,
    customAttributes = function(v) {
      if (getOption('vsc.previewPromises', default = FALSE)) {
        ret <- list(
          names = list('__promiseEnv', '__currentValue'),
          values = list(v$promiseEnv, eval(v$promiseExpr, envir = v$promiseEnv))
        )
      } else {
        ret <- list(
          names = list('__promiseEnv'),
          values = list(v$promiseEnv)
        )
      }
      return(ret)
    },
    hasChildren = TRUE,
    includeAttributes = FALSE
  ),
  # .Random.seed (TEMPORARY FIX)
  list(
    name = '.Random.seed',
    doesApply = function(v) !is.null(v) && identical(v, get0(".Random.seed", globalenv())),
    childVars = list(),
    hasChildren = FALSE
    # toString = 'c(KW:$%&...)'
  ),
  # environment
  list(
    name = 'Environment',
    doesApply = is.environment,
    childVars = getVarsInEnv,
    toString = format,
    hasChildren = function(v) length(ls(v, all.names = TRUE)) > 0
  ),
  # data.frame
  list(
    name = 'Data.frame',
    doesApply = is.data.frame,
    childVars = function(v) list(values = as.list(v)),
    hasChildren = TRUE,
    shortType = 'data.frame',
    longType = 'data.frame'
  ),
  # factor
  list(
    name = 'Factor',
    doesApply = is.factor,
    childVars = function(v) list(values = format(v)),
    hasChildren = function(v) length(v) > 0,
    shortType = 'factor',
    longType = 'factor'
  ),
  # matrix row
  list(
    name = 'MatrixRow',
    doesApply = function(v) inherits(v, '.vsc.matrixRow'),
    includeAttributes = FALSE,
    customAttributes = list(),
    toString = function(v){
      attributes(v) <- list()
      paste0(utils::capture.output(utils::str(v, max.level = 0, give.attr = FALSE)), collapse = "\n")
    }
  ),
  # matrix
  list(
    name = 'Matrix',
    doesApply = is.matrix,
    childVars = function(v) {
      getRow <- function(i) {
        row <- v[i, ]
        if (is.null(names(row))) {
          names(row) <- vapply(seq_len(ncol(v)), function(j) paste0('[', i, ',', j, ']'), character(1))
        }
        class(row) <- '.vsc.matrixRow'
        return(row)
      }
      if (ncol(v)==1){
        vars <- as.list(v)
        names <- rownames(v)
        if (is.null(names)) {
          names <- lapply(seq_len(nrow(v)), function(i) paste0('[', i, ',1]'))
        }
      } else{
        vars <- lapply(seq2(nrow(v)), getRow)
        names <- rownames(v)
        if (is.null(names)) {
          names <- lapply(seq_len(nrow(v)), function(i) paste0('[', i, ',]'))
        }
      }
      return(list(
        names = names,
        values = vars
      ))
    },
    hasChildren = TRUE,
    shortType = function(v) {
      paste0('matrix[', nrow(v), ',', ncol(v), ']')
    },
    'matrix',
    longType = 'matrix'
  ),
  # list
  list(
    name = 'List',
    doesApply = is.list,
    childVars = function(v) {
      values <- as.list(v)
      if (is.null(names(v))) {
        names <- lapply(seq2(values), function(s) paste0('[[', s, ']]'))
      } else {
        names <- NULL
      }
      list(values = values, names = names)
    },
    hasChildren = TRUE,
    shortType = 'list',
    longType = 'list'
  ),
  # vector
  list(
    name = 'Vector',
    doesApply = function(v) {
      attributes(v) <- NULL
      is.vector(v) && length(v) > 1
    },
    childVars = function(v) {
      values <- as.list(v)
      if (is.null(names(values))) {
        names <- lapply(seq2(values), function(s) paste0('[', s, ']'))
      } else {
        names <- NULL
      }
      list(values = as.list(v), names = names)
    },
    hasChildren = TRUE,
    shortType = 'c',
    longType = 'vector'
  ),
  # language: name, call, expression, name
  list(
    name = 'Language',
    doesApply = function(v) is.language(v),
    childVars = function(v) {
      if (is.name(v) || is.symbol(v)) {
        return(list())
      } else {
        return(list(values = as.list(v)))
      }
    },
    hasChildren = function(v) !(is.name(v) || is.symbol(v)),
    shortType = '',
    longType = 'language',
    toString = function(v) {
      if (is.symbol(v)) {
        ret <- toString(v)
      } else {
        ret <- paste0(format(v), collapse = '\n')
      }
      ret
    }
  ),
  # S4
  list(
    name = 'S4',
    doesApply = isS4,
    childVars = function(v) {
      names <- slotNames(v)
      values <- lapply(names, function(s) slot(v, s))
      list(values = values, names = names)
    },
    hasChildren = TRUE,
    shortType = 'S4',
    longType = 'S4',
    includeAttributes = FALSE,
    customAttributes = function(v) {
      attrs <- attributes(v)
      slots <- slotNames(v)
      nonslots <- setdiff(names(attrs), slots)
      list(
        names = as.list(paste0("_", nonslots)),
        values = attrs[nonslots]
      )
    }
  ),
  # non-standard class
  list(
    name = 'NonStandardClass',
    doesApply = function(v) {
      'class' %in% names(attributes(v)) && !is.environment(v) && !isS4(v)
    },
    customAttributes = function(v) {
      return(list(
        names = list('__unclass()'),
        values = list(unclass(v))
      ))
    },
    hasChildren = TRUE
  ),
  # function
  list(
    name = 'Function',
    doesApply = is.function,
    customAttributes = function(v) {
      return(list(
        names = list('__body()'),
        values = list(body(v))
      ))
    },
    hasChildren = TRUE,
    shortType = '',
    longType = 'function',
    toString = function(v) {
      paste0(format(v), collapse = '\n')
    }
  ),
  # scalar
  list(
    name = 'Scalar',
    doesApply = function(v) is.atomic(v) && length(v) == 1 && is.null(attributes(v)),
    hasChildren = FALSE,
    toString = function(v) paste(deparse(v), collapse = '\n', sep = ';')
  ),
  # default case
  list(
    name = 'Default',
    doesApply = function(v) TRUE,
    childVars = list(),
    shortType = '',
    longType = function(v) typeof(v),
    includeAttributes = TRUE,
    hasChildren = function(v) !is.null(attributes(v)),
    toString = function(v) {
      paste0(utils::capture.output(utils::str(v, max.level = 0, give.attr = FALSE)), collapse = "\n")
    }
  )
)
