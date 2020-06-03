# type CompletionItemType = 'method' | 'function' | 'constructor' | 'field' | 'variable' |
#   'class' | 'interface' | 'module' | 'property' | 'unit' | 'value' | 'enum' |
#   'keyword' | 'snippet' | 'text' | 'color' | 'file' | 'reference' | 'customcolor';

constants <- c("TRUE", "FALSE", "NULL",
  "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_",
  "Inf", "NaN")

getLazyDataFromNamespace <- function(ns) {
  lazydata <- ns$.__NAMESPACE__.$lazydata
  if (length(lazydata)) {
    ls(lazydata)
  } else {
    character()
  }
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
.vsc.getCompletion <- function(frameIdVsc, text, column = 0, line = 1, id = 0, onlyGlobalEnv = FALSE) {
  if (column > 1) {
    text <- substring(text, 1, column - 1)
  }
  if (onlyGlobalEnv) {
    firstenv <- globalenv()
  } else {
    frameId <- convertFrameId(vsc = frameIdVsc)
    firstenv <- sys.frame(frameId)
  }
  lastenv <- globalenv()
  envs <- getScopeEnvs(firstenv = firstenv, lastenv = lastenv)

  pattern0 <- "(\\$|\\[\\[|\\[|:::|::|:)$"
  ind <- regexpr(pattern0, text)
  if (ind != -1) {
    text1 <- substring(text = text, first = 1, last = ind - 1)
    text2 <- substring(text = text, first = ind)
  } else {
    text1 <- text
    text2 <- ""
  }

  var <- getLastVar(text1)

  if (var == "" && text2 != "") {
    # only "$", "[", or "[[" --> no matches
    targets <- list()
  } else if (text2 == "") {
    const_targets <- lapply(constants[startsWith(constants, var)], function(s) list(
      label = s,
      type = 'value'
    ))

    pkgs <- getInstalledPackages()
    pkgs_targets <- lapply(pkgs[startsWith(pkgs, var)], function(s) list(
      label = paste0(s, '::'),
      type = 'module'
    ))

    pattern = paste0("^", var)
    env_targets <- lapply(envs, function(env) {
      names <- ls(env, all.names = TRUE, pattern = pattern, sorted = FALSE)
      lapply(names, function(s) list(
        label = s,
        type = if (isPromise(s, env)) 'variable' else if (is.function(env[[s]])) 'function' else 'variable'
      ))
    })
    env_targets <- unlist(env_targets, recursive = FALSE, use.names = FALSE)

    att_pkgs <- getAttachedPackages()
    att_targets <- lapply(att_pkgs, function(pkg) {
      ns <- getNamespace(pkg)
      exports <- getNamespaceExports(ns)
      lazydata <- getLazyDataFromNamespace(ns)
      c(
        lapply(exports[startsWith(exports, var)], function(s) list(
          label = s,
          type = if (is.function(ns[[s]])) 'function' else 'field'
        )),
        lapply(lazydata[startsWith(lazydata, var)], function(s) list(
          label = s,
          type = 'field'
        ))
      )
    })
    att_targets <- unlist(att_targets, recursive = FALSE, use.names = FALSE)

    targets <- c(const_targets, pkgs_targets, env_targets, att_targets)
  } else {
    # find all children of the last variable
    targets <- getCompletionList(var, text2, envs)
  }

  .vsc.sendToVsc('completion', targets, id)
}

#' @export
getLastVar <- function(text) {
  pattern1 <- "((?:[a-zA-Z]|\\.[a-zA-Z_])[a-zA-Z\\._0-9]*|\\.)$" # matches the beggining of the last valid variable name
  ind <- regexpr(pattern1, text)
  if (ind == -1) {
    return("")
  } else {
    return(substring(text, ind))
  }
}

getCompletionList <- function(var, accessor, envs) {
  targets <- list()
  if (accessor %in% c('[', '[[', '$')) {
    for (env in envs) {
      if (exists(var, env, inherits = FALSE)) {
        if (isPromise(var, env)) {
          if (getOption('vsc.previewPromises')){
            promise <- getPromiseVar(var, env)
            obj <- eval(promise$promiseExpr, promise$promiseEnv)
          } else {
            obj <- NULL
          }
        } else {
          obj <- env[[var]]
        }

        use_bracket <- accessor %in% c('[', '[[')
        use_dollar <- accessor == '$' && is.recursive(obj)

        if ((use_bracket || use_dollar)) {
          names <- names(obj)
          get_label <- if (use_bracket) function(s) paste0('"', s, '"') else identity
          if (is.environment(obj)) {
            targets <- lapply(names, function(s) list(
              label = get_label(s),
              type = if (isPromise(s, obj)) 'variable' else if (is.function(obj[[s]])) 'function' else 'variable'
            ))
          } else if (is.list(obj)) {
            targets <- lapply(names, function(s) list(
              label = get_label(s),
              type = if (is.function(obj[[s]])) 'function' else 'variable'
            ))
          } else {
            targets <- lapply(names, function(s) list(
              label = get_label(s),
              type = 'variable'
            ))
          }
        }
        break
      }
    }
  } else if (accessor == '::') {
    ns <- getNamespace(var)
    exports <- getNamespaceExports(ns)
    lazydata <- getLazyDataFromNamespace(ns)
    targets <- c(
      lapply(exports, function(s) list(
        label = s,
        type = if (is.function(ns[[s]])) 'function' else 'field'
      )),
      lapply(lazydata, function(s) list(
        label = s,
        type = 'field'
      ))
    )
  } else if (accessor == ':::') {
    ns <- getNamespace(var)
    names <- ls(ns)
    targets <- lapply(names, function(s) list(
      label = s,
      type = if (is.function(ns[[s]])) 'function' else 'field'
    ))
  }
  targets
}
