"C:\Program Files\R\R-3.6.3\bin\R.exe" --silent -e "devtools::document()"

@IF %ERRORLEVEL% NEQ 0 (
    @echo Aborting build: devtools::document() returned errorcode %ERRORLEVEL%
    @goto EOF
)

"C:\Program Files\R\R-3.6.3\bin\R.exe" -e "devtools::install(args = c('--library=lib', '--no-lock'))"

@IF %ERRORLEVEL% NEQ 0 (
    @echo Aborting build: local install command returned errorcode %ERRORLEVEL%
    @goto EOF
)

"C:\Program Files\R\R-3.6.3\bin\R.exe" -e "devtools::install(args = c('--library=..\\vscode-R-test\\lib', '--no-lock'))"

:EOF
