@echo off
setlocal EnableExtensions

set "SCRIPT_DIR=%~dp0"
for %%I in ("%SCRIPT_DIR%..") do set "REPO_ROOT=%%~fI"
set "MAMBA_ROOT_PREFIX=%REPO_ROOT%\.R_skana\micromamba"
set "MICROMAMBA_BIN=%REPO_ROOT%\.R_skana\tools\micromamba\micromamba.exe"

call "%SCRIPT_DIR%setup_r_skana.bat"
if errorlevel 1 exit /b %errorlevel%

set "SK_ANA_RENV_ROOT=%REPO_ROOT%\.R_skana\renv"
set "SK_ANA_PROJECT_ROOT=%REPO_ROOT%"
set "RENV_PATHS_ROOT=%SK_ANA_RENV_ROOT%"
set "RENV_PATHS_LIBRARY=%SK_ANA_RENV_ROOT%\library"
set "RENV_PATHS_CACHE=%SK_ANA_RENV_ROOT%\cache"
set "RENV_PATHS_SANDBOX=%SK_ANA_RENV_ROOT%\sandbox"
set "RENV_CONFIG_SANDBOX_ENABLED=FALSE"
set "RENV_CONFIG_SHIMS_ENABLED=FALSE"
cd /d "%REPO_ROOT%"

if "%~1"=="" (
  "%MICROMAMBA_BIN%" run -r "%MAMBA_ROOT_PREFIX%" -n R_skana R
  exit /b %errorlevel%
)

"%MICROMAMBA_BIN%" run -r "%MAMBA_ROOT_PREFIX%" -n R_skana %*
exit /b %errorlevel%
