# Build File
#
# Documents the package using roxygen
# Two build modes:
# - slow: make sure that binaries are built and source references retained
# - quick: quick reinstall, might not work after major changes (especially to C code)

args <- commandArgs(trailingOnly = TRUE)

devtools::document()

if('quick' %in% args){
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

  # normal install first to build binaries
  devtools::install(
    dependencies = FALSE,
    args="--no-byte-compile"
  )

  # quick install to properly keep source references
  devtools::install(
    dependencies = FALSE,
    keep_source = TRUE,
    quick = TRUE
  )
}
