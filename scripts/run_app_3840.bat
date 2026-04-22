@echo off
setlocal EnableExtensions

set "SCRIPT_DIR=%~dp0"
for %%I in ("%SCRIPT_DIR%..") do set "REPO_ROOT=%%~fI"

if "%SK_ANA_HOST%"=="" (
  set "SK_ANA_HOST=127.0.0.1"
)
if "%PORT%"=="" set "PORT=3840"

cd /d "%REPO_ROOT%"
echo Preparing SK-Ana launch from isolated R_skana environment ^(preferred http://%SK_ANA_HOST%:%PORT%^)
"%SCRIPT_DIR%r_skana.bat" Rscript "%SCRIPT_DIR%run_app_3840.R"
exit /b %errorlevel%
