@echo off
setlocal
cd /d "%~dp0\banff-app"

set R_HOME=%~dp0\runtime\win\R
set R_LIBS_USER=%~dp0\runtime\win\library
set R_LIBS=%~dp0\runtime\win\library

"%~dp0\runtime\win\R\bin\Rscript.exe" --vanilla -e "lib<-normalizePath('../runtime/win/library',winslash='/'); .libPaths(lib); setwd('.'); shiny::runApp('.',host='127.0.0.1',port=3939,launch.browser=TRUE)"
