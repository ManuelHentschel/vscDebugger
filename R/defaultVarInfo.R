
# helper functions for the default varInfos:

getRow <- function(v, i) {
    row <- v[i, ]
    names(row) <- colnames(v)
    if (is.null(names(row))) {
        # names(row) <- vapply(seq_len(ncol(v)), function(j) paste0('[', i, ',', j, ']'), character(1))
        names(row) <- getIndices(v, row=i)
    }
    class(row) <- '.vsc.matrixRow'
    return(row)
}
getCol <- function(v, j) {
    col <- v[, j]
    names(col) <- rownames(v)
    if (is.null(names(col))) {
        # names(col) <- vapply(seq_len(ncol(v)), function(j) paste0('[', j, ',', i, ']'), character(1))
        names(col) <- getIndices(v, col=j)
    }
    # matrixRow can be used here as well!
    class(col) <- '.vsc.matrixRow'
    return(col)
}
getIndices <- function(v, row=NULL, col=NULL) {
    if(is.null(row) && is.null(col)){
        m <- expand.grid(nrow(v), ncol(v))
        names <- mapply(function(i,j) paste0('[', i, ',', j, ']'), m[[1]], m[[2]])
    } else if(is.null(row)){
        names <- lapply(seq_len(nrow(v)), function(i) paste0('[' ,i, ',', col, ']'))
    } else if(is.null(col)){
        names <- lapply(seq_len(ncol(v)), function(j) paste0('[', row, ',', j, ']'))
    } else{
        names <- list(paste0('[', row, ',', col, ']'))
    }
}


# Get default list of varInfos to handle most cases

getDefaultVarInfos <- function(){
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
    # Active binding
    list(
        name = 'ActiveBinding',
        doesApply = function(v) inherits(v, '.vsc.activeBinding'),
        longType = 'active binding',
        hasChildren = TRUE,
        toString = 'Active binding',
        includeAttributes = FALSE, # TODO: Can active bindings have non-active attributes?
        childVars = list(),
        customAttributes = function(v){
            list(
                names = list('bindingFunction'),
                values = list(v$bindingFunction)
            )
        }
    ),
    # Ellipses entry (custom type)
    list(
        name = 'EllipsesEntry',
        doesApply = function(v) inherits(v, '.vsc.ellipsesEntry'),
        includeAttributes = FALSE,
        customAttributes = function(v) list(
            names = list('__dotEnvironment'),
            values = list(v$dotEnv)
        ),
        childVars = list(),
        longType = 'ellipses entry',
        shortType = '',
        toString = function(v) paste0(format(v$dotExpr), collapse='\n')
    ),
    # Ellipses (custom type)
    list(
        name = 'Ellipses',
        doesApply = function(v) inherits(v, '.vsc.ellipses'),
        childVars = function(v){
            values <- lapply(v, function(vv) {
                class(vv) <- '.vsc.ellipsesEntry'
                vv
            })
            list(values=values)
        },
        longType = 'ellipses',
        includeAttributes = FALSE,
        customAttributes = list()
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
        childVars = getVarsInEnv,
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
        doesApply = function(v) is.matrix(v) || is.data.frame(v), # data.frame specific info is handled above
        childVars = function(v) {
            if(getOption('vsc.matricesByRow', TRUE)){
                if (ncol(v)==1){
                    vars <- as.list(v)
                    names <- rownames(v)
                    if (is.null(names)) {
                        names <- getIndices(v, col=1)
                    }
                } else{
                    vars <- lapply(seq2(nrow(v)), getRow, v=v)
                    names <- rownames(v)
                    if (is.null(names)) {
                        names <- getIndices(v, col='')
                    }
                }
            } else{ # by column
                if (nrow(v)==1){
                    vars <- as.list(v)
                    names <- colnames(v)
                    if (is.null(names)) {
                        names <- getIndices(v, row=1)
                    }
                } else{
                    vars <- lapply(seq2(ncol(v)), getCol, v=v)
                    names <- colnames(v)
                    if (is.null(names)) {
                        names <- getIndices(v, row='')
                    }
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

    return(defaultVarInfo)
}