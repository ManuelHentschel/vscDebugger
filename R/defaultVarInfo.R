
# Get default list of varInfos to handle most cases

getDefaultVarInfos <- function() {
  defaultVarInfo <- list(
    # NULL
    list(
      name = 'NULL',
      doesApply = is.null,
      childVars = list(),
      nChildVars = 0,
      type = 'NULL',
      toString = 'NULL'
    ),
    # promise (custom type)
    list(
      name = 'Promise',
      doesApply = function(v) inherits(v, '.vsc.promise'),
      childVars = list(),
      nChildVars = 0,
      type = 'promise',
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
        ret
      }
    ),
    # Active binding
    list(
      name = 'ActiveBinding',
      doesApply = function(v) inherits(v, '.vsc.activeBinding'),
      type = 'active binding',
      toString = 'Active binding',
      internalAttributes = list(),
      childVars = function(v, ind=NULL) {
        list(
          list(
            name = 'bindingFunction',
            rValue = v$bindingFunction
          )
        )
      },
      nChildVars = 1
    ),
    # Ellipsis (custom type)
    list(
      name = 'Ellipsis',
      doesApply = function(v) inherits(v, '.vsc.ellipsis'),
      type = 'ellipsis',
      toString = '<Ellipsis Arguments>',
      internalAttributes = list()
    ),
    # info variable (info by the debugger if there was an error etc.)
    list(
      name = 'InfoVar',
      doesApply = function(v) inherits(v, '.vsc.infoVar'),
      childVars = list(),
      internalAttributes = list(),
      nChildVars = 0,
      type = function(v) v$type,
      toString = function(v) v$text
    ),
    # .Random.seed (TEMPORARY FIX)
    list(
      name = '.Random.seed',
      doesApply = function(v) !is.null(v) && identical(v, get0(".Random.seed", globalenv())),
      childVars = list(),
      nChildVars = 0
    ),
    # R6 (TODO: display public/private properties etc.)
    list(
      name = 'R6',
      doesApply = function(v) inherits(v, 'R6'),
      type = 'R6'
    ),
    # environment
    list(
      name = 'Environment',
      type = 'environment',
      doesApply = is.environment,
      childVars = function(v, ind=NULL) {
        vars <- getVarsInEnv(v, ind)
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
      nChildVars = function(v){
        length(ls(v, all.names = TRUE))
      },
      toString = format
    ),
    # factor
    list(
      name = 'Factor',
      doesApply = is.factor,
      childVars = function(v, ind=NULL) {
        len0 <- length(v)
        if(is.null(ind)){
          ind <- seq_along(v)
        } else{
          v <- v[ind]
        }
        if (is.null(names(v))) {
          names <- paste0('[', ind, ']')
        } else{
          names <- names(ret$value)
        }
        if(getOption('vsc.convertFactorEntries', FALSE)){
          rValues <- as.list(format(v))
        } else if(len0>1){
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
      nChildVars = function(v){
        length(v)
      },
      type = 'factor'
    ),
    # subArray
    list(
      name = 'SubArray',
      doesApply = function(v) inherits(v, '.vsc.subArray'),
      childVars = function(v, ind=NULL){
        subArrays <- arrayDimToList(v)
        childVars <- lapply(subArrays, function(sa){
          list(
            rValue = sa,
            name = attr(sa, '.vsc.name'),
            setter = attr(sa, '.vsc.setter')
          )
        })
      },
      nChildVars = function(v){
        arrayDimToList(v, onlyNChildVars = TRUE)
      },
      internalAttributes = list(),
      toString = function(v) {
        if(is.null(dim(v))){
          toString(v)
        } else{
          dimV <- dim(v)
          attributes(v) <- list()
          dim(v) <- dimV
          paste0(utils::capture.output(utils::str(v, max.level = 0, give.attr = FALSE)), collapse = "\n")
        }
      },
      type = 'subArray'
    ),
    # data.frame
    list(
      name = 'Data.frame',
      doesApply = is.data.frame,
      type = 'data.frame'
      # rest is handled by 'Array'
    ),
    # matrix
    list(
      name = 'Matrix',
      doesApply = is.matrix,
      type = 'matrix'
      # rest is handled by 'Array'
    ),
    # array
    list(
      name = 'Array',
      doesApply = function(v) is.array(v) || is.data.frame(v),
      childVars = function(v, ind=NULL){
        subArrays <- arrayDimToList(v, ind=ind)
        childVars <- lapply(subArrays, function(sa){
          list(
            rValue = sa,
            name = attr(sa, '.vsc.name'),
            setter = attr(sa, '.vsc.setter')
          )
        })
      },
      nChildVars = function(v){
        arrayDimToList(v, onlyNChildVars = TRUE)
      },
      toString = function(v){
        if(is.null(dim(v))){
          toString(v)
        } else{
          dimV <- dim(v)
          attributes(v) <- list()
          dim(v) <- dimV
          paste0(utils::capture.output(utils::str(v, max.level = 0, give.attr = FALSE)), collapse = "\n")
        }
      },
      type = 'array'
    ),
    # list
    list(
      name = 'List',
      doesApply = is.list,
      childVars = function(v, ind=NULL) {
        if(is.null(ind)){
          ind <- seq_along(v)
        } else{
          v <- v[ind]
        }
        names <- names(v)
        lapply(seq_along(v), function(s){
          name <- names[s]
          index <- ind[s]
          if(is.null(name)){
            name <- paste0('[[', index, ']]')
          }
          list(
            rValue = v[[s]],
            name = name,
            setter = substitute(parent[[s]], list(s=index))
          )
        })
      },
      nChildVars = function(v){
        length(v)
      },
      type = 'list'
    ),
    # vector
    list(
      name = 'Vector',
      doesApply = function(v) {
        if(is.environment(v)){
          FALSE
        } else if(is.factor(v)){
          FALSE
        } else{
          attributes(v) <- NULL
          is.vector(v) && length(v) > 1
        }
      },
      childVars = function(v, ind=NULL) {
        if(is.null(ind)){
          ind <- seq_along(v)
        } else{
          v <- v[ind]
        }
        names <- names(v)
        lapply(seq_along(v), function(s){
          name <- names[s]
          index <- ind[s]
          if(is.null(name)){
            name <- paste0('[', index, ']')
          }
          rValue <- v[s]
          attributes(rValue) <- NULL
          list(
            rValue = rValue,
            name = name,
            setter = substitute(parent[s], list(s=index))
          )
        })
      },
      nChildVars = function(v){
        length(v)
      },
      type = 'vector'
    ),
    # language: name, call, expression, name
    list(
      name = 'Language',
      doesApply = function(v) is.language(v),
      childVars = function(v, ind=NULL) {
        if (is.name(v) || is.symbol(v)) {
          return(list())
        } else {
          return(unsummarizeLists(list(
            rValue = as.list(v),
            name = format(seq_along(as.list(v)))
          )))
        }
      },
      nChildVars = function(v){
        if(is.name(v) || is.symbol(v)){
          0
        } else{
          length(as.list(v))
        }
      },
      type = 'language',
      toString = function(v) {
        if (is.symbol(v)) {
          ret <- toString(v)
        } else {
          ret <- paste0(format(v), collapse = '\n')
        }
        ret
      }
    ),
    # S4 class representation
    list(
      name = 'S4 class representation',
      doesApply = function(v) inherits(v, 'classRepresentation') && isS4(v),
      childVars = list(),
      nChildVars = 0,
      type = 'S4 class representation'
    ),
    # S4 (instance or class representation. Child vars of S4 class representation handled above.)
    list(
      name = 'S4',
      doesApply = isS4,
      childVars = function(v, ind=NULL) {
        names <- slotNames(v)
        values <- lapply(names, function(s) slot(v, s))
        unsummarizeLists(list(rValue = values, name = names))
      },
      nChildVars = function(v){
        length(slotNames(v))
      },
      type = 'S4',
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
      type = 'function',
      toString = function(v) {
        paste0(format(v), collapse = '\n')
      }
    ),
    # scalar
    list(
      name = 'Scalar',
      doesApply = function(v) is.atomic(v) && length(v) == 1 && is.null(attributes(v)),
      toString = function(v) {
        if(is.numeric(v) || is.logical(v) || is.character(v)){
          names(v) <- NULL
        }
        paste(deparse(v), collapse = '\n', sep = ';')
      },
      childVars = list(),
      nChildVars = 0
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
      nChildVars = 0,
      type = function(v) typeof(v),
      internalAttributes = function(v) {
        if(getOption('vsc.groupAttributes', FALSE)){
          return(list())
        }
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
      customAttributes = function(v){
        if(getOption('vsc.groupAttributes', FALSE) && !inherits(v, '.vsc.internalClass')){
          rValue <- attributes(v)
          class(rValue) <- c('.vsc.attributeList', '.vsc.internalClass')
          ret <- list(
            name = "__attributes()",
            rValue = rValue,
            setter = quote(attributes(parent))
          )
          list(ret)
        } else{
          list()
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
