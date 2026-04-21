@echo off
setlocal

set "SCRIPT_DIR=%~dp0"
for %%I in ("%SCRIPT_DIR%..") do set "REPO_ROOT=%%~fI"

if "%HOST%"=="" set "HOST=127.0.0.1"
if "%PORT%"=="" set "PORT=3840"

where Rscript >nul 2>nul
if errorlevel 1 (
  echo Rscript not found in PATH.
  exit /b 127
)

cd /d "%REPO_ROOT%"
echo Launching SK-Ana from %REPO_ROOT% on http://%HOST%:%PORT%
Rscript "%SCRIPT_DIR%run_app_3840.R"

