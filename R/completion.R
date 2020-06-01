constants <- c("TRUE", "FALSE", "NULL",
  "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_",
  "Inf", "NaN")

getSymbolsFromAttachedPackages <- function(text) {
  pkgs <- getAttachedPackages()
  symbols <- lapply(pkgs, function(pkg) {
    ns <- getNamespace(pkg)
    exported <- getNamespaceExports(ns)
    exported[startsWith(exported, text)]
  })
  unlist(symbols, use.names = FALSE)
}


getAttachedPackages <- function() {
  pkgs <- search()
  pkgs <- pkgs[startsWith(pkgs, "package:")]
  pkgs <- gsub("package:", "", pkgs, fixed = TRUE)
  return(pkgs)
}

getInstalledPackages <- function() {
  .packages(all.available = TRUE)
}

#' @export
.vsc.getCompletion <- function(frameIdVsc, text, column=0, line=1, id=0, onlyGlobalEnv=FALSE){
  if(column>1){
    text <- substring(text, 1, column-1)
  }
  if(onlyGlobalEnv){
    firstenv <- globalenv()
  } else{
    frameId <- convertFrameId(vsc = frameIdVsc)
    firstenv <- sys.frame(frameId)
  }
  lastenv <- globalenv()
  envs <- getScopeEnvs(firstenv=firstenv, lastenv=lastenv)

  pattern0 <- "(\\$|\\[\\[|\\[|:::|::|:)$"
  ind <- regexpr(pattern0, text)
  if(ind!=-1){
    text1 <- substring(text = text, first = 1, last = ind-1)
    text2 <- substring(text = text, first = ind)
  } else{
    text1 <- text
    text2 <- ""
  }

  var <- getLastVar(text1)

  if(var=="" && text2!=""){
    # only "$", "[", or "[[" --> no matches
    matches <- list()
  } else if(text2==""){
    # find all matching variable names
    pattern = paste0("^", var)
    pkgs <- getInstalledPackages()
    pkgCompletion <- lapply(pkgs, function(s) paste0(s, '::'))
    matches <- c(
      constants,
      lapply(envs, ls, all.names = TRUE, pattern = pattern, sorted = FALSE),
      pkgCompletion,
      getSymbolsFromAttachedPackages(var)
    )
    matches <- unlist(matches)
  } else{
    # find all children of the last variable
    matches <- getNameList(var, text2, envs)
  }

  targets <- lapply(matches, function(s) list(label=s))

  .vsc.sendToVsc('completion', targets, id)
}

#' @export
getLastVar <- function(text){
  pattern1 <- "((?:[a-zA-Z]|\\.[a-zA-Z_])[a-zA-Z\\._0-9]*|\\.)$" # matches the beggining of the last valid variable name
  ind <- regexpr(pattern1, text)
  if(ind == -1){
    return("")
  } else{
    return(substring(text, ind))
  }
}


#' @export
getNameList <- function(var, delimiter, envs){
  names <- list()
  if(delimiter %in% c('[', '[[', '$')){
    for(env in envs){
      if(var %in% ls(env, sorted=FALSE)){
        if(isPromise(var, env)){
          names <- getNameListForPromise(var,env)
        } else{
          names <- names(get(var, envir=env))
        }
        break
      }
    }
    if(delimiter %in% c('[', '[[')){
      names <- lapply(names, function(s) paste0('"', s, '"'))
    }
  } else if(delimiter %in% c('::', ':')){
    ns <- getNamespace(var)
    names <- getNamespaceExports(ns)
    if(delimiter == ':'){
      names <- lapply(names, function(s) paste0(':', s))
    }
  } else if(delimiter == ':::'){
    ns <- getNamespace(var)
    names <- ls(ns)
  }
  names <- as.list(names)
  return(names)
}


#' @export
getNameListForPromise <- function(var, env){
  promise <- getPromiseVar(var, env)
  val <- eval(promise$promiseExpr, promise$promiseEnv)
  names <- names(val)
  return(names)
}
