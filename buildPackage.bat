"C:\Program Files\R\R-3.6.3\bin\R.exe" --file=preparePackage.R --silent

@IF %ERRORLEVEL% NEQ 0 (
    @echo Aborting build: preparePackage.R returned errorcode %ERRORLEVEL%
    @goto EOF
)

"C:\Program Files\R\R-3.6.3\bin\R.exe" CMD build vscDebugger

@IF %ERRORLEVEL% NEQ 0 (
    @echo Aborting build: build command returned errorcode %ERRORLEVEL%
    @goto EOF
)

"C:\Program Files\R\R-3.6.3\bin\R.exe" CMD INSTALL vscDebugger*.tar.gz --library=lib --no-lock

@IF %ERRORLEVEL% NEQ 0 (
    @echo Aborting build: local install command returned errorcode %ERRORLEVEL%
    @goto EOF
)

"C:\Program Files\R\R-3.6.3\bin\R.exe" CMD INSTALL vscDebugger*.tar.gz --library=..\vscode-R-test\lib --no-lock

:EOF