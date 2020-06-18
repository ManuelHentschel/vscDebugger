# tryCatch(
#     devtools::uninstall(),
#     error = function(e) print('Not installed')
# )
# remove.packages('vscDebugger')

devtools::document()
# devtools::install()
# devtools::install(args = c('--no-lock'), reload = FALSE, quick = TRUE)
devtools::install(args = c('--no-staged-install', '--with-keep.source', '--no-exec'))
