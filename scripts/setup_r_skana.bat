@echo off
setlocal EnableExtensions

set "SCRIPT_DIR=%~dp0"
for %%I in ("%SCRIPT_DIR%..") do set "REPO_ROOT=%%~fI"
set "R_SKANA_DIR=%REPO_ROOT%\.R_skana"
set "TOOLS_DIR=%R_SKANA_DIR%\tools"
set "MICROMAMBA_DIR=%TOOLS_DIR%\micromamba"
set "MICROMAMBA_BIN=%MICROMAMBA_DIR%\micromamba.exe"
set "MAMBA_ROOT_PREFIX=%R_SKANA_DIR%\micromamba"
set "ENV_FILE=%SCRIPT_DIR%r_skana.environment.yml"
set "ENV_RSCRIPT=%MAMBA_ROOT_PREFIX%\envs\R_skana\Scripts\Rscript.exe"
set "ENV_STAMP_FILE=%R_SKANA_DIR%\base-environment.stamp"
set "VENDOR_DIR=%SCRIPT_DIR%vendor\windows"
set "VENDOR_MICROMAMBA=%VENDOR_DIR%\micromamba.exe"
if "%SK_ANA_MICROMAMBA_VERSION%"=="" (
  set "MICROMAMBA_RELEASE_URL=https://github.com/mamba-org/micromamba-releases/releases/latest/download/micromamba-win-64"
) else (
  set "MICROMAMBA_RELEASE_URL=https://github.com/mamba-org/micromamba-releases/releases/download/%SK_ANA_MICROMAMBA_VERSION%/micromamba-win-64"
)

call :ensure_dir "%R_SKANA_DIR%"
if errorlevel 1 exit /b %errorlevel%

call :ensure_dir "%TOOLS_DIR%"
if errorlevel 1 exit /b %errorlevel%

echo.
echo ==> Preparing isolated R_skana runtime
echo     Repository root: %REPO_ROOT%

call :ensure_micromamba
if errorlevel 1 exit /b %errorlevel%

call :check_existing_env
if not errorlevel 1 (
  echo.
  echo ==> R_skana setup complete
  exit /b 0
)

call :ensure_base_env
if errorlevel 1 exit /b %errorlevel%

call :ensure_project_packages
echo.
echo ==> R_skana setup complete
exit /b %errorlevel%

:check_existing_env
if not exist "%ENV_RSCRIPT%" exit /b 1

call :env_definition_changed
if not errorlevel 1 (
  echo     Base environment definition changed. Refreshing the R_skana environment.
  exit /b 1
)

echo.
echo ==> Checking existing R_skana environment
pushd "%REPO_ROOT%"
"%MICROMAMBA_BIN%" run -r "%MAMBA_ROOT_PREFIX%" -n R_skana Rscript "%SCRIPT_DIR%setup_r_skana.R"
set "STATUS=%errorlevel%"
popd
if "%STATUS%"=="0" exit /b 0
echo     Existing environment check failed. Refreshing the base environment.
exit /b 1

:env_definition_changed
if not exist "%ENV_STAMP_FILE%" exit /b 0
for /f %%I in ('powershell -NoProfile -Command "(Get-FileHash -Algorithm SHA256 ''%ENV_FILE%'').Hash"') do set "ENV_FILE_HASH=%%I"
set /p STORED_ENV_HASH=<"%ENV_STAMP_FILE%"
if /I not "%ENV_FILE_HASH%"=="%STORED_ENV_HASH%" exit /b 0
exit /b 1

:write_env_stamp
for /f %%I in ('powershell -NoProfile -Command "(Get-FileHash -Algorithm SHA256 ''%ENV_FILE%'').Hash"') do set "ENV_FILE_HASH=%%I"
>"%ENV_STAMP_FILE%" echo %ENV_FILE_HASH%
exit /b 0

:ensure_micromamba
if exist "%MICROMAMBA_BIN%" (
  echo.
  echo ==> Using existing micromamba
  echo     %MICROMAMBA_BIN%
  goto :eof
)

call :ensure_dir "%MICROMAMBA_DIR%"
if errorlevel 1 exit /b %errorlevel%

if defined SK_ANA_MICROMAMBA_EXE (
  if exist "%SK_ANA_MICROMAMBA_EXE%" (
    echo.
    echo ==> Copying micromamba from SK_ANA_MICROMAMBA_EXE
    echo     %SK_ANA_MICROMAMBA_EXE%
    copy /y "%SK_ANA_MICROMAMBA_EXE%" "%MICROMAMBA_BIN%" >nul
    if exist "%MICROMAMBA_BIN%" goto :eof
  )
)

if exist "%VENDOR_MICROMAMBA%" (
  echo.
  echo ==> Copying vendored micromamba
  echo     %VENDOR_MICROMAMBA%
  copy /y "%VENDOR_MICROMAMBA%" "%MICROMAMBA_BIN%" >nul
  if exist "%MICROMAMBA_BIN%" goto :eof
)

echo.
echo ==> Downloading micromamba
call :download_micromamba
if errorlevel 1 (
  echo.
  echo Failed to provision micromamba in user space.
  echo.
  echo This script does not require administrator rights, but it does require one of:
  echo   1. outbound HTTPS access to micro.mamba.pm, or
  echo   2. a pre-approved micromamba.exe file
  echo.
  echo To run on locked-down Windows machines, use one of these options:
  echo   - set SK_ANA_MICROMAMBA_EXE to a local micromamba.exe path before running
  echo   - place micromamba.exe in scripts\vendor\windows\micromamba.exe
  echo.
  exit /b 1
)
goto :eof

:ensure_dir
if exist "%~1\" exit /b 0
mkdir "%~1" >nul 2>nul
if exist "%~1\" exit /b 0
echo Failed to create directory: %~1
exit /b 1

:download_micromamba
powershell -NoProfile -ExecutionPolicy Bypass -Command ^
  "$ErrorActionPreference='Stop';" ^
  "New-Item -ItemType Directory -Path (Split-Path '%MICROMAMBA_BIN%') -Force | Out-Null;" ^
  "Invoke-WebRequest -Uri '%MICROMAMBA_RELEASE_URL%' -OutFile '%MICROMAMBA_BIN%';"
if not errorlevel 1 exit /b 0

where curl.exe >nul 2>nul
if errorlevel 1 exit /b 1

powershell -NoProfile -ExecutionPolicy Bypass -Command ^
  "$ErrorActionPreference='Stop';" ^
  "New-Item -ItemType Directory -Path (Split-Path '%MICROMAMBA_BIN%') -Force | Out-Null;" ^
  "& curl.exe -L '%MICROMAMBA_RELEASE_URL%' -o '%MICROMAMBA_BIN%' | Out-Null;"
if errorlevel 1 exit /b 1
exit /b 0

:ensure_base_env
if exist "%ENV_RSCRIPT%" (
  echo.
  echo ==> Updating existing R_skana environment
  "%MICROMAMBA_BIN%" env update -y -r "%MAMBA_ROOT_PREFIX%" -n R_skana -f "%ENV_FILE%" --prune
  if errorlevel 1 exit /b %errorlevel%
  call :write_env_stamp
  exit /b %errorlevel%
)

echo.
echo ==> Creating R_skana environment
"%MICROMAMBA_BIN%" create -y -r "%MAMBA_ROOT_PREFIX%" --override-channels -c conda-forge -f "%ENV_FILE%"
if errorlevel 1 exit /b %errorlevel%
call :write_env_stamp
exit /b %errorlevel%

:ensure_project_packages
echo.
echo ==> Installing SK-Ana R packages
pushd "%REPO_ROOT%"
"%MICROMAMBA_BIN%" run -r "%MAMBA_ROOT_PREFIX%" -n R_skana Rscript "%SCRIPT_DIR%setup_r_skana.R"
set "STATUS=%errorlevel%"
popd
exit /b %STATUS%
