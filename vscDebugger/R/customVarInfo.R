

# type rValue = any;
# type NULL = undefined;
# type namedList = {names: string[], values: rValue[]};
# type varInfo = {
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
.vsc.resetVarInfo <- function(){
    .packageEnv$varInfo <- defaultVarInfo
}

#' @export
.vsc.clearVarInfo <- function(){
    .packageEnv$varInfo <- list()
}

#' @export
.vsc.addVarInfo <- function(
    doesApply = NULL,
    childVars = NULL,
    customAttributes = NULL,
    hasChildren = NULL,
    toString = NULL,
    shortType = NULL,
    longType = NULL,
    includeAttributes = NULL,
    varInfo = NULL,
    position=1
){
    if(position<0){
        # negative positions count from the end, -1 = last position
        position <- length(.packageEnv$varInfo) + 1 + position
    } else if(position>0){
        position <- position - 1
    }
    # start with empty varInfo if none given
    if(is.null(varInfo)){
        varInfo <- list()
    }
    # add entries to varInfo (entires already in varInfo will be overwritten)
    varInfo$doesApply <- doesApply
    varInfo$childVars <- childVars
    varInfo$customAttributes <- customAttributes
    varInfo$hasChildren <- hasChildren
    varInfo$toString <- toString
    varInfo$shortType <- shortType
    varInfo$longType <- longType
    varInfo$includeAttributes <- includeAttribute

    .packageEnv$varInfo <- append(.packageEnv$varInfo, varInfo, position)
}

#' @export
.vsc.removeVarInfo <- function(position=1){
    if(position<0){
        position <- length(.packageEnv$varInfo) + 1 + position
    }
    .packageEnv$varInfo[position] <- NULL
}

#' @export
.vsc.listVarInfo <- function(position=NULL){
    if(is.null(position)){
        position <- seq2(.packageEnv$varInfo)
    }
    return(.packageEnv$varInfo[position])
}

.vsc.checkVarInfo <- function(varInfo, testCase=NULL){
    warn <- list()
    err <- list()
    # doesApply:
    if(is.null(varInfo$doesApply)){
        err <- c(err, 'doesApply must not be NULL')
    } else if(!is.function(varInfo$doesApply)){
        warn <- c(warn, 'doesApply should be a function')
    }

    # customAttributes:
    # hasChildren:
    # toString:
    # shortType:
    # longType:
    # includeAttributes:

    # TODO: check everything else, apply testCase if supplied
    return(TRUE)
}


getVarsInEnv <- function(env, all.names=TRUE){
    names <- ls(env, all.names=all.names)
    vars <- lapply(names, getVarInEnv, env)
    return(list(
        values = vars,
        names = names
    ))
}

defaultVarInfo <- list(
    # NULL
    list(
        doesApply = is.null,
        childVars = list(),
        hasChildren = FALSE,
        shortType = '',
        longType = 'NULL',
        toString = 'NULL'
    ),
    # promise (custom type)
    list(
        doesApply = function(v) class(v) == ".vsc.promise",
        childVars = list(),
        shortType = '',
        longType = 'promise',
        toString = function(v) v$promiseCode,
        customAttributes = function(v){
            list(
                names = list('__promiseEnv', '__currentValue'),
                values = list(v$promiseEnv, eval(v$promiseExpr, envir=v$promiseEnv))
            )
        },
        hasChildren = TRUE,
        includeAttributes = FALSE
    ),
    # environment
    list(
        doesApply = is.environment,
        childVars = getVarsInEnv,
        toString = format,
        hasChildren = function(v) length(ls(v, all.names=TRUE))>0
    ),
    # data.frame
    list(
        doesApply = is.data.frame,
        childVars = function(v) list(values = as.list(v)),
        hasChildren = TRUE,
        shortType = 'data.frame',
        longType = 'data.frame'
    ),
    # factor
    list(
        doesApply = is.factor,
        childVars = function(v) list(values = format(v)),
        hasChildren = function(v) length(v)>0,
        shortType = 'factor',
        longType = 'factor'
    ),
    # list
    list(
        doesApply = is.list,
        childVars = function(v){
            values <- as.list(v)
            if(is.null(names(v))){
                names <- lapply(seq2(values), function(s) paste0('[[',s,']]'))
            } else{
                names <- NULL
            }
            list(values=values, names=names)
        },
        hasChildren = TRUE,
        shortType = 'list',
        longType = 'list'
    ),
    # vector
    list(
        doesApply = function(v){
            attributes(v) <- NULL
            is.vector(v) && length(v)>1
        },
        childVars = function(v){
            values <- as.list(v)
            if(is.null(names(values))){
                names <- lapply(seq2(values), function(s){paste0('[',s,']')})
            } else{
                names <- NULL
            }
            list(values = as.list(v), names=names)
        },
        hasChildren = TRUE,
        shortType = 'c',
        longType = 'vector'
    ),
    # language: name, call, expression, name
    list(
        doesApply = function(v) is.language(v),
        childVars = function(v) list(values = as.list(v)),
        hasChildren = TRUE,
        shortType = '',
        longType = 'language',
        toString = function(v){
            if(is.symbol(v)){
                ret <- toString(v)
            } else{
                ret <- paste0(format(v), collapse='\n')
            }
            ret
        }
    ),
    # matrix
    list(
        doesApply = is.matrix,
        childVars = function(v){
            vars <- lapply(seq2(nrow(v)), function(i) v[i,])
            names(vars) <- rownames(v)
            return(list(
                names = rownames(v),
                values = vars
            ))
        },
        hasChildren = TRUE, 
        shortType = 'matrix',
        longType = 'matrix'
    ),
    # string
    list(
        doesApply = is.character,
        longType = 'string',
        hasChildren = FALSE
    ),
    # non-standard class
    list(
        doesApply = function(v){
            return('class' %in% names(attributes(v)) && !is.environment(v))
        },
        customAttributes = function(v){
            return(list(
                names = list('__unclass()'),
                values = list(unclass(v))
            ))
        },
        hasChildren = TRUE
    ),
    # function
    list(
        doesApply = is.function,
        customAttributes = function(v){
            return(list(
                names = list('__body()'),
                values = list(body(v))
            ))
        },
        hasChildren = TRUE,
        shortType = '',
        longType = 'function',
        toString = function(v){
            paste(format(v), collapse='\n')
        }
    ),
    # default case
    list(
        doesApply = function(v) TRUE,
        childVars = list(),
        shortType = '',
        longType = function(v) typeof(v),
        includeAttributes = TRUE,
        hasChildren = FALSE,
        toString = function(v) paste0(format(v), collapse=',')
    )
)
