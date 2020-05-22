

# type rValue = any;
# type NULL = undefined;

# type namedList = {names: string[], values: rValue[]};

# type varInfo = {
#     doesApply: ((v: rValue) => boolean);
#     childVars: ((v:rValue) => namedList)|NULL;
#     customAttributes: ((v:rValue) => namedList)|NULL;
#     toString: ((v:rValue) => string)|NULL;
#     shortType: ((v:rValue) => string)|NULL;
#     longType: ((v:rValue) => string)|NULL;
#     customNumbering: ((v:rValue) => {left: string, right: string} | string[])|NULL
# }

# type varInfos = varInfo[];


defaultVarInfo <- list(
    list(
        doesApply = function(v){
            return('class' %in% names(attributes(v)) && !is.environment(v))
        },
        customAttributes = function(v){
            return(list(
                names = list('__unclass()'),
                values = list(unclass(v))
            ))
        }
    ),
    list(
        doesApply = function(v){
            return(is.function(v))
        },
        customAttributes = function(v){
            return(list(
                names = list('__body()'),
                values = list(body(v))
            ))
        }
    )
)

#' @export
.vsc.resetVarInfo <- function(){
    .packageEnv$varInfo <- defaultVarInfo
}

#' @export
.vsc.addVarInfo <- function(..., position=1){
    if(position<0){
        position <- length(.packageEnv$varInfo) + 1 + position
    } else if(position>0){
        position <- position - 1
    }
    .packageEnv$varInfo <- append(.packageEnv$varInfo, list(...), position)
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