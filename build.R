# Build File
#
# Documents the package using roxygen
# Two build modes:
# - slow: make sure that binaries are built and source references retained
# - quick: quick reinstall, might not work after major changes (especially to C code)

args <- commandArgs(trailingOnly = TRUE)


if('quick' %in% args){
  # update documentation
  devtools::document()

  # quick install only
  # usually enough, e.g. when only changing some R code
  devtools::install(
    dependencies = FALSE,
    quick=TRUE,
    keep_source=TRUE,
    args="--no-byte-compile"
  )
} else{
  # This workflow seems to work on windows to reliably build the 
  # binaries and keep the source info of the debugger:

  # remove old files
  suppressWarnings({
    file.remove('src/init.o')
    file.remove('src/ppid.o')
    file.remove('src/promise.o')
    file.remove('src/vscDebugger.dll')
    file.remove('src/vscDebugger.so')
  })

  # update documentation
  devtools::document()

  # normal install first to build binaries
  devtools::install(
    dependencies = FALSE,
    quick = FALSE,
    build = TRUE,
    force = TRUE,
    args="--no-byte-compile"
  )

  # quick install to properly keep source references
  devtools::install(
    dependencies = FALSE,
    keep_source = TRUE,
    quick = TRUE
  )
}
