
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
    # promsie (custom type, click-to-eval)
    list(
      name = 'Promise',
      type = 'promise',
      toString = function(v) paste0(
          '`',
          paste0(format(v$code), collapse = '; '),
          '` in ',
          format(v$environment)
      ),
      doesApply = function(v) inherits(v, '.vsc.promise'),
      nChildVars = 1,
      childVars = function(v, ind){
          list(list(
              rValue = eval(v$code, envir=v$environment),
              name = '<VALUE>'
          ))
      },
      internalAttributes = list(),
      presentationHint = list(lazy = TRUE)
    ),
    # promise details (custom type, includes environment and code)
    list(
      name = 'PromiseDetails',
      doesApply = function(v) inherits(v, '.vsc.promiseDetails'),
      childVars = list(),
      nChildVars = 0,
      type = 'promise details',
      toString = function(v) paste0(format(v$code), collapse = "; "),
      internalAttributes = function(v) {
        # Change class to render `__promiseValue` as simple promise
        class(v) = c(".vsc.promise", ".vsc.internalClass")
        ret <- list(
          list(
            name = '__promiseEnv',
            rValue = v$environment
          ),
          list(
            name = '__promiseCode',
            rValue = v$code
          ),
          list(
            name = '__promiseValue',
            rValue = v
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
      evaluateName = '.Random.seed',
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
    # help file
    list(
      name = 'HelpFile',
      doesApply = function(v){
        identical(class(v), 'help_files_with_topic') ||
        identical(class(v), 'hsearch')
      },
      printFunc = function(v) {
        base::print
      }
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
          names <- names(v)
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
        ret <- length(v)
        if(ret==1){
          ret <- 0
        }
        ret
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
      evaluateName = function(v) {
        attributes(v) <- NULL
        v <- drop(v)
        deparseUnlessTooLarge(v)
      },
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
      type = 'array',
      printFunc = function(v){
        if(getOption('vsc.printArrays', FALSE)){
          base::print
        } else{
          NULL
        }
      }
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
          if(is.null(ind)){
            ind <- seq_along(as.list(v))
          }
          lapply(ind, function(i){
            list(
              rValue = v[[i]],
              name = format(i),
              setter = substitute(parent[[i]], list(i=i))
            )
          })
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
        return(lapply(names, function(name) list(
          name = name,
          rValue = slot(v, name)
        )))
      },
      nChildVars = function(v){
        length(slotNames(v))
      },
      type = 'S4',
      internalAttributes = function(v) {
        if(getOption('vsc.groupAttributes', FALSE)){
          return(list())
        }
        attrs <- attributes(v)
        slots <- slotNames(v)
        nonslots <- setdiff(names(attrs), slots)
        return(lapply(nonslots, function(ns) list(
          name = paste0("_", ns),
          rValue = attrs[ns]
        )))
      }
    ),
    # non-standard class
    list(
      name = 'NonStandardClass',
      doesApply = function(v) {
        'class' %in% names(attributes(v)) && !is.environment(v) && !isS4(v) && !inherits(v, '.vsc.internalClass')
      },
      customAttributes = function(v) {
        if(getOption('vsc.showUnclass', TRUE)){
          tryCatch(
            list(
              list(
                name = '__unclass()',
                rValue = unclass(v),
                setter = quote(.vsc.unclass(parent))
              )
            ),
            error = function(e) list()
          )
        } else{
          list()
        }
      }
    ),
    # function
    list(
      name = 'Function',
      doesApply = is.function,
      customAttributes = function(v) {
        if(getOption('vsc.showFunctionBody', FALSE)){
          list(
            list(
              name = '__body()',
              rValue = body(v),
              setter = quote(body(parent))
            )
          )
        }
      },
      type = 'function',
      toString = function(v) {
        paste0(format(v), collapse = '\n')
      }
    ),
    # something with source info (usually functions)
    list(
      name = 'WithSource',
      doesApply = function(v) {(
        isTRUE(getOption('vsc.linkSrcrefLocations', TRUE))
        && "srcref" %in% names(attributes(v))
      )},
      locationReference = function(v){
        srcref <- attr(v, 'srcref')
        locref <- getLocationReference(srcref)
        return(locref)
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
        if(is.integer(v) && getOption('vsc.showIntegerAsNumber', TRUE)){
          v <- as.numeric(v)
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
    # internal variable, deparse normally not
    list(
      name = 'InternalVar',
      doesApply = function(v) inherits(v, '.vsc.internalClass'),
      evaluateName = '<No expression available>'
    ),
    # default case
    list(
      name = 'Default',
      doesApply = function(v) TRUE,
      childVars = list(),
      nChildVars = 0,
      type = function(v) typeof(v),
      presentationHint = function(v) NULL,
      internalAttributes = function(v) {
        if(getOption('vsc.groupAttributes', FALSE)){
          return(list())
        }
        attr <- attributes(v)
        if(getOption('vsc.linkSrcrefLocations', TRUE) && 'srcref' %in% names(attr)){
          attr <- attr[setdiff(names(attr), 'srcref')]
        }
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
        if(getOption('vsc.groupAttributes', FALSE) && !inherits(v, '.vsc.internalClass') && getOption('vsc.showAttributes', TRUE)){
          attr <- attributes(v)
          if(getOption('vsc.linkSrcrefLocations', TRUE) && 'srcref' %in% names(attr)){
            attr <- attr[setdiff(names(attr), 'srcref')]
          }
          class(attr) <- c('.vsc.attributeList', '.vsc.internalClass')
          ret <- list(
            name = "__attributes()",
            rValue = attr,
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
      evaluateName = function(v) {
        deparseUnlessTooLarge(v)
      },
      printFunc = function(v) {
        if(identical(getOption('vsc.printEvalResponses', FALSE), TRUE)){
          base::print
        } else{
          TRUE
        }
      },
      locationReference = 0
    )
  )

  return(defaultVarInfo)
}
