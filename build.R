devtools::document()
devtools::install(args = c('--library=lib', '--no-lock'), reload = FALSE)
devtools::install(args = c('--library=../VSCode-R-Debugger/test/R/lib', '--no-lock'), reload = FALSE)
