R --file=preparePackage.R --silent

R CMD build vscDebugger

R CMD INSTALL vscDebugger --no-lock