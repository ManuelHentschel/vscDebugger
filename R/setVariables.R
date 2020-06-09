


#' @export
.vsc.setVariable <- function(variablesReference, name, value){
  varList <- getVarListsEntry(variablesReference)
  setExpression <- NULL
  setEnvironment <- NULL
  for(var in varList$variables){
    if(var$name == name){
      setExpression <- var$setExpression
      setEnvironment <- var$setEnvironment
    }
  }
  if(is.null(setExpression) || is.null(setEnvironment)){
    print('Setting not possible')
    return(FALSE)
  }

  valueR <- eval(parse(text=value), envir = setEnvironment)
  
  cl <- call('<-', setExpression, valueR)
  eval(cl, envir=setEnvironment)

  invisible(TRUE)
}


