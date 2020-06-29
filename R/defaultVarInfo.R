
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
      internalAttributes = list(),
      childVars = function(v) {
        list(
          list(
            name = 'bindingFunction',
            rValue = v$bindingFunction
          )
        )
      }
    ),
    # Ellipsis (custom type)
    list(
      name = 'Ellipsis',
      doesApply = function(v) inherits(v, '.vsc.ellipsis'),
      longType = 'ellipsis',
      toString = '<Ellipsis Arguments>',
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
        ret <- lapply(vars, function(var){
          name <- var$name
          setter <- substitute(env[[name]], list(name=name))
          l <- list(
            name = name,
            rValue = var$value,
            setter = setter,
            setInfo = list(environment=v, setter = setter)
          )
        })
        ret
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
        if (is.null(names(v))) {
          names <- paste0('[', seq_along(v), ']')
        } else{
          names <- names(ret$value)
        }
        if(getOption('vsc.convertFactorEntries', FALSE)){
          rValues <- as.list(format(v))
        } else if(length(v)>1){
          rValues <- as.list(v)
        } else{
          rValues <- list()
          names <- list()
        }
        ret <- mapply(
          function(vv, n) list(rValue=vv, name=n),
          rValues,
          names,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      },
      hasChildren = function(v) {
        if( length(v) > 1 || getOption('vsc.showAttributes', TRUE)){
          TRUE
        } else{
          NULL
        }
      },
      shortType = 'factor',
      longType = 'factor'
    ),
    # matrix row
    list(
      name = 'MatrixRow',
      doesApply = function(v) inherits(v, '.vsc.matrixRow'),
      includeAttributes = FALSE,
      internalAttributes = list(),
      customAttributes = list(),
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
        byRow <- (
          is.matrix(v) && getOption('vsc.matricesByRow', TRUE) || 
          is.data.frame(v) && getOption('vsc.dataFramesByRow', FALSE) 
        )
        if (byRow) {
          if (ncol(v) == 1) {
            vars <- as.list(v)
            names <- rownames(v)
            if (is.null(names)) {
              names <- getIndices(v, col = 1)
            }
            setters <- lapply(seq_along(vars), function(s){
              substitute(parent[s,1], list(s=s))
            })
          } else {
            vars <- lapply(seq2(nrow(v)), getRow, v = v)
            names <- rownames(v)
            if (is.null(names)) {
              names <- getIndices(v, col = '')
            }
            setters <- lapply(seq_along(vars), function(s){
              substitute(parent[s,], list(s=s))
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
              substitute(parent[1,s], list(s=s))
            })
          } else {
            vars <- lapply(seq2(ncol(v)), getCol, v = v)
            names <- colnames(v)
            if (is.null(names)) {
              names <- getIndices(v, row = '')
            }
            setters <- lapply(seq_along(vars), function(s){
              substitute(parent[,s], list(s=s))
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
            setter = substitute(parent[[s]], list(s=s))
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
        if(is.factor(v)){
          FALSE
        } else{
          attributes(v) <- NULL
          is.vector(v) && length(v) > 1
        }
      },
      childVars = function(v) {
        names <- names(v)
        lapply(seq_along(v), function(s){
          name <- names[s]
          if(is.null(name)){
            name <- paste0('[', s, ']')
          }
          rValue <- v[s]
          attributes(rValue) <- NULL
          list(
            rValue = rValue,
            name = name,
            setter = substitute(parent[s], list(s=s))
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
      hasChildren = function(v){
        if(getOption('vsc.showCustomAttributes')){
          TRUE
        } else{
          NULL
        }
      }
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
      hasChildren = function(v){
        if(getOption('vsc.showCustomAttributes', TRUE)){
          TRUE
        } else{
          getOption('vsc.showAttributes', TRUE) && !is.null(attributes(v))
        }
      },
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
      toString = function(v) {
        if(is.numeric(v) || is.logical(v) || is.character(v)){
          names(v) <- NULL
        }
        paste(deparse(v), collapse = '\n', sep = ';')
      }
    ),
    # named scalar
    list(
      name = 'NamedScalar',
      doesApply = function(v){
        is.atomic(v) && length(v) == 1 && identical(names(attributes(v)), c("names")) &&
        (is.numeric(v) || is.logical(v) || is.character(v))
      },
      toString = function(v) {
        names(v) <- NULL
        paste(deparse(v), collapse = '\n', sep = ';')
      }
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
      internalAttributes = function(v) {
        attr <- attributes(v)
        names <- names(attr)
        mapply(
          function(a, n){
            list(
              name = paste0("_", n),
              rValue = a,
              setter = substitute(attr(parent, name), list(name=n))
            )
          },
          attr,
          names,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      },
      customAttributes = list(),
      hasChildren = function(v) {
        if(length(v)>1){
          return(TRUE)
        } else if(getOption('vsc.showAttributes') && !is.null(attributes(v))){
          return(TRUE)
        } else {
          childVars <- .vsc.applyVarInfos(v, infos='childVars')[[1]]
          if(length(childVars)>0){
            return(TRUE)
          } else if(getOption('vsc.showCustomAttributes')){
            customAttributes <- .vsc.applyVarInfos(v, stackingInfos = 'customAttributes')[[1]]
            customAttributes <- unlist(customAttributes, recursive = FALSE)
            return(length(customAttributes)>0)
          } else{
            return(FALSE)
          }
        }
      },
      toString = function(v) {
        paste0(utils::capture.output(utils::str(v, max.level = 0, give.attr = FALSE)), collapse = "\n")
      },
      evaluateName = function(v) paste0(deparse(v), collapse = '\n')
    )
  )

  return(defaultVarInfo)
}
