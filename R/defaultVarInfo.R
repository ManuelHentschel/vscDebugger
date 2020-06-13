
# helper functions for the default varInfos:

getRow <- function(v, i) {
  row <- v[i, ]
  names(row) <- colnames(v)
  if (is.null(names(row))) {
    names(row) <- getIndices(v, row = i)
  }
  class(row) <- c('.vsc.matrixRow', '.vsc.internalClass')
  return(row)
}
getCol <- function(v, j) {
  col <- v[, j]
  names(col) <- rownames(v)
  if (is.null(names(col))) {
    names(col) <- getIndices(v, col = j)
  }
  # matrixRow can be used here as well!
  class(col) <- c('.vsc.matrixRow', '.vsc.internalClass')
  return(col)
}
getIndices <- function(v, row = NULL, col = NULL) {
  if (is.null(row) && is.null(col)) {
    m <- expand.grid(nrow(v), ncol(v))
    names <- mapply(function(i, j) paste0('[', i, ',', j, ']'), m[[1]], m[[2]])
  } else if (is.null(row)) {
    names <- lapply(seq_len(nrow(v)), function(i) paste0('[', i, ',', col, ']'))
  } else if (is.null(col)) {
    names <- lapply(seq_len(ncol(v)), function(j) paste0('[', row, ',', j, ']'))
  } else {
    names <- list(paste0('[', row, ',', col, ']'))
  }
}


# Get default list of varInfos to handle most cases

getDefaultVarInfos <- function() {
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
      toString = function(v) paste0(format(v$code), collapse = "; "),
      internalAttributes = function(v) {
        ret <- list(
          list(
            name = '__promiseEnv',
            rValue = v$environment
          )
        )
        if (getOption('vsc.previewPromises', default = FALSE)) {
          ret <- append(ret, list(
            list(
              name = '__currentValue',
              eval(v$code, envir=v$environment)
            )
          ))
        }
      },
      hasChildren = TRUE,
      includeAttributes = FALSE
    ),
    # Active binding
    list(
      name = 'ActiveBinding',
      doesApply = function(v) inherits(v, '.vsc.activeBinding'),
      longType = 'active binding',
      hasChildren = TRUE,
      toString = 'Active binding',
      includeAttributes = FALSE, # TODO: Can active bindings have non-active attributes?
      childVars = list(),
      internalAttributes = function(v) {
        list(
          list(
            name = 'bindingFunction',
            rValue = v$bindingFunction
          )
        )
      }
    ),
    # Ellipses entry (custom type)
    list(
      name = 'EllipsesEntry',
      doesApply = function(v) inherits(v, '.vsc.ellipsesEntry'),
      includeAttributes = FALSE,
      internalAttributes = function(v) list(
        list(
          name = '__dotEnvironment',
          rValue = v$dotEnv
        )
      ),
      childVars = list(),
      longType = 'ellipses entry',
      shortType = '',
      toString = function(v) paste0(format(v$dotExpr), collapse = '\n')
    ),
    # Ellipses (custom type)
    list(
      name = 'Ellipses',
      doesApply = function(v) inherits(v, '.vsc.ellipses'),
      childVars = function(v) {
        lapply(v, function(vv) {
          class(vv) <- c('.vsc.ellipsesEntry', '.vsc.internalClass')
          list(
            rValue = vv,
            name = '<Ellipses Entry Name???>'
          )
        })
      },
      longType = 'ellipses',
      includeAttributes = FALSE,
      internalAttributes = list()
    ),
    # info variable (info by the debugger if there was an error etc.)
    list(
      name = 'InfoVar',
      doesApply = function(v) inherits(v, '.vsc.infoVar'),
      childVars = list(),
      shortType = '',
      longType = function(v) v$type,
      hasChildren = FALSE,
      toString = function(v) v$text,
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
      childVars = function(v) {
        vars <- getVarsInEnv(v)
        lapply(vars, function(var){
          list(
            name = var$name,
            rValue = var$value,
            setInfo = list(environment=v, expression=as.symbol(var$name))
          )
        })
      },
      toString = format,
      hasChildren = function(v) length(ls(v, all.names = TRUE)) > 0
    ),
    # data.frame
    list(
      name = 'Data.frame',
      doesApply = is.data.frame,
      shortType = 'data.frame',
      longType = 'data.frame'
      # rest is handled by 'Matrix'
    ),
    # factor
    list(
      name = 'Factor',
      doesApply = is.factor,
      childVars = function(v) {
        ret <- list(rValue = format(v))
        if (is.null(names(ret$value))) {
          ret$name <- paste0('[', seq_along(ret$value), ']')
        } else{
          ret$name <- names(ret$value)
        }
        unsummarizeLists(ret)
      },
      hasChildren = function(v) length(v) > 0,
      shortType = 'factor',
      longType = 'factor'
    ),
    # matrix row
    list(
      name = 'MatrixRow',
      doesApply = function(v) inherits(v, '.vsc.matrixRow'),
      includeAttributes = FALSE,
      internalAttributes = list(),
      toString = function(v) {
        attributes(v) <- list()
        paste0(utils::capture.output(utils::str(v, max.level = 0, give.attr = FALSE)), collapse = "\n")
      }
    ),
    # matrix
    list(
      name = 'Matrix',
      doesApply = function(v) is.matrix(v) || is.data.frame(v), # data.frame specific info is handled above
      childVars = function(v) {
        if (getOption('vsc.matricesByRow', TRUE)) {
          if (ncol(v) == 1) {
            vars <- as.list(v)
            names <- rownames(v)
            if (is.null(names)) {
              names <- getIndices(v, col = 1)
            }
            setters <- lapply(seq_along(vars), function(s){
              substitute(.vsc.parent[s,1])
            })
          } else {
            vars <- lapply(seq2(nrow(v)), getRow, v = v)
            names <- rownames(v)
            if (is.null(names)) {
              names <- getIndices(v, col = '')
            }
            setters <- lapply(seq_along(vars), function(s){
              substitute(.vsc.parent[s,])
            })
          }
        } else { # by column
          if (nrow(v) == 1) {
            vars <- as.list(v)
            names <- colnames(v)
            if (is.null(names)) {
              names <- getIndices(v, row = 1)
            }
            setters <- lapply(seq_along(vars), function(s){
              substitute(.vsc.parent[1,s])
            })
          } else {
            vars <- lapply(seq2(ncol(v)), getCol, v = v)
            names <- colnames(v)
            if (is.null(names)) {
              names <- getIndices(v, row = '')
            }
            setters <- lapply(seq_along(vars), function(s){
              substitute(.vsc.parent[,s])
            })
          }
        }
        unsummarizeLists(list(
          name = names,
          rValue = vars,
          setter = setters
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
        names <- names(v)
        lapply(seq_along(v), function(s){
          name <- names[s]
          if(is.null(name)){
            name <- paste0('[[', s, ']]')
          }
          list(
            rValue = v[[s]],
            name = name,
            setter = substitute(.vsc.parent[[s]])
          )
        })
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
        names <- names(v)
        lapply(seq_along(v), function(s){
          name <- names[s]
          if(is.null(name)){
            name <- paste0('[', s, ']')
          }
          list(
            rValue = v[s],
            name = name,
            setter = substitute(.vsc.parent[s])
          )
        })
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
          return(unsummarizeLists(list(
            rValue = as.list(v),
            name = format(seq_along(as.list(v)))
          )))
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
        unsummarizeLists(list(rValue = values, name = names))
      },
      hasChildren = TRUE,
      shortType = 'S4',
      longType = 'S4',
      includeAttributes = FALSE,
      internalAttributes = function(v) {
        attrs <- attributes(v)
        slots <- slotNames(v)
        nonslots <- setdiff(names(attrs), slots)
        unsummarizeLists(list(
          name = as.list(paste0("_", nonslots)),
          rValue = attrs[nonslots]
        ))
      }
    ),
    # non-standard class
    list(
      name = 'NonStandardClass',
      doesApply = function(v) {
        'class' %in% names(attributes(v)) && !is.environment(v) && !isS4(v) && !inherits(v, '.vsc.internalClass')
      },
      customAttributes = function(v) {
        tryCatch(
          list(
            list(
              name = '__unclass()',
              rValue = unclass(v)
            )
          ),
          error = function(e) list()
        )
      },
      hasChildren = TRUE
    ),
    # function
    list(
      name = 'Function',
      doesApply = is.function,
      customAttributes = function(v) {
        list(
          list(
            name = '__body()',
            rValue = body(v)
          )
        )
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
      type = function(v) typeof(v),
      includeAttributes = TRUE,
      customAttributes = list(),
      internalAttributes = list(),
      hasChildren = function(v) !is.null(attributes(v)),
      toString = function(v) {
        paste0(utils::capture.output(utils::str(v, max.level = 0, give.attr = FALSE)), collapse = "\n")
      },
      evaluateName = function(v) paste0(deparse(v), collapse = '\n')
    )
  )

  return(defaultVarInfo)
}
