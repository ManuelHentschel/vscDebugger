

#' @export
.vsc.browser <- function(){
  print("My browser!")
  eval.parent(quote(browser()))
}


#' @export
.vsc.debugSourceBrowser <- function(){
  # print("My debug source broooowser!")
  # print(sys.calls())
  print(tracingState())
  eval.parent(quote(browser()))
}

