

# #' @export
# .vsc.browser <- function(){
#   print("My browser!")
#   print(tracingState())
#   print(sys.calls())
#   eval.parent(quote(browser()))
# }


# #' @export
# .vsc.debugSourceBrowser <- function(){
#   # print("My debug source broooowser!")
#   # print(sys.calls())
#   print(tracingState())
#   eval.parent(quote(browser()))
# }


#' @export
.vsc.preBreakpoint <- function(){
  if(tracingState()){
    # set some state in session?
    # print('pre breakpoint!!!')
    # request `n`
    sendWriteToStdinEvent('n')
    # send breakpoint event
    sendStoppedEvent('breakpoint')
  }
}
