# vscDebugger

This package provides support for the VS Code extension
**[R Debugger](https://github.com/ManuelHentschel/VSCode-R-Debugger)**.

## Installation

The package can be installed from the
[Releases](https://github.com/ManuelHentschel/vscDebugger/releases/latest)
site.
(Experimental) binaries for windows and macOS are provided as well.
Should these not work, you can compile and install the package using e.g.
[devtools](https://cran.r-project.org/web/packages/devtools/index.html)
and [RTools](https://cran.r-project.org/bin/windows/Rtools/).

Depending on the installation you might need to manually install the dependencies, as well
(currently `R6` and `jsonlite`):
```r
install.packages(c('R6', 'jsonlite', 'svSocket'))
```

## Usage
Detailed information can be found [here](https://manuelhentschel.github.io/vscDebugger/).
