# vscDebugger

<a href="https://manuelhentschel.r-universe.dev"><img src="https://manuelhentschel.r-universe.dev/badges/vscDebugger" class="img-fluid" alt="R-universe status badge"></a>

This package provides support for the VS Code extension
**[R Debugger](https://github.com/ManuelHentschel/VSCode-R-Debugger)**.

## Installation

**vscDebugger** is not yet on CRAN, but can be installed from R-universe.

```r
install.packages("vscDebugger", repos = "https://manuelhentschel.r-universe.dev")
```

Alternatively, users can install a source release from GitHub
[here](https://github.com/ManuelHentschel/vscDebugger/releases/latest).
However, note that this may require manual compilation of the
underlying C++ code (e.g., using
[RTools](https://cran.r-project.org/bin/windows/Rtools/) on Windows), as well
manual installation of the package dependencies (currently `R6`, `jsonlite`,
and `tcltk`).

## Usage
Detailed information can be found [here](https://manuelhentschel.github.io/vscDebugger/).
