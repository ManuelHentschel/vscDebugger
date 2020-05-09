# change multiline prompt
options(continue='... ')

# add ./lib to path
if(dir.exists('lib')){
    .libPaths(c('lib', .libPaths()))
}

# overwrite q()
q <- function (save="no", ...) {
  quit(save=save, ...)
}

