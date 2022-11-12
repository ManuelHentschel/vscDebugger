# Build File
#
# Documents the package using roxygen
# Two build modes:
# - slow: make sure that binaries are built and source references retained
# - quick: quick reinstall, might not work after major changes (especially to C code)

args <- commandArgs(trailingOnly = TRUE)


if('docOnly' %in% args){
  # update documentation
  devtools::document()
  
  devtools::build_vignettes(
    dependencies = FALSE,
    upgrade = FALSE,
    install = FALSE
  )

  # only update reference pages
  pkgdown::build_site(
    lazy = TRUE,
    devel = TRUE,
    preview = FALSE
  )
  system('sed -i "s/href=\'\\./href=\'dot-/g" docs/**/*.html')
} else if('quick' %in% args){
  
  try({
    libpath <- installed.packages()['vscDebugger', 'LibPath']
    trunc <- file.path(libpath, 'vscDebugger', 'libs')
    f1 <- file(file.path(trunc, 'i386', 'vscDebugger.dll'), open='r')
    f2 <- file(file.path(trunc, 'x64', 'vscDebugger.dll'), open='r')
    remove.packages('vscDebugger')
  })

  # update documentation
  devtools::document()

  # quick install only
  # usually enough, e.g. when only changing some R code
  devtools::install(
    dependencies = FALSE,
    quick=TRUE,
    keep_source=TRUE,
    args=c("--no-byte-compile", "--no-staged-install", "--no-libs")
  )
  
  # pkgdown::build_site(
  #   install = FALSE,
  #   lazy = TRUE,
  #   devel = TRUE,
  #   preview = FALSE
  # )
  # system('sed -i "s/href=\'\\./href=\'dot-/g" docs/**/*.html')
} else{
  # This workflow seems to work on windows to reliably build the 
  # binaries and keep the source info of the debugger:

  # update documentation
  devtools::document()

  # normal install first to build binaries
  devtools::install(
    dependencies = FALSE,
    upgrade = 'never',
    quick = FALSE,
    build = TRUE,
    force = TRUE,
    args = c(
      "--preclean",
      "--clean"
    ),
    build_vignettes = TRUE
  )

  pkgdown::build_site(
    install = FALSE,
    preview = FALSE
  )
  system('sed -i "s/href=\'\\./href=\'dot-/g" docs/**/*.html')
}
