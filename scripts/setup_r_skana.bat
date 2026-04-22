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
set "VENDOR_DIR=%SCRIPT_DIR%vendor\windows"
set "VENDOR_MICROMAMBA=%VENDOR_DIR%\micromamba.exe"

if not exist "%TOOLS_DIR%" mkdir "%TOOLS_DIR%"

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

echo.
echo ==> Checking existing R_skana environment
pushd "%REPO_ROOT%"
"%MICROMAMBA_BIN%" run -r "%MAMBA_ROOT_PREFIX%" -n R_skana Rscript "%SCRIPT_DIR%setup_r_skana.R"
set "STATUS=%errorlevel%"
popd
if "%STATUS%"=="0" exit /b 0
echo     Existing environment check failed. Refreshing the base environment.
exit /b 1

:ensure_micromamba
if exist "%MICROMAMBA_BIN%" (
  echo.
  echo ==> Using existing micromamba
  echo     %MICROMAMBA_BIN%
  goto :eof
)

if exist "%MICROMAMBA_DIR%" rmdir /s /q "%MICROMAMBA_DIR%"
mkdir "%MICROMAMBA_DIR%"

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

:download_micromamba
powershell -NoProfile -ExecutionPolicy Bypass -Command ^
  "$ErrorActionPreference='Stop';" ^
  "$tmp = Join-Path $env:TEMP ('micromamba-' + [guid]::NewGuid().ToString());" ^
  "New-Item -ItemType Directory -Path $tmp | Out-Null;" ^
  "$archive = Join-Path $tmp 'micromamba.tar.bz2';" ^
  "Invoke-WebRequest -Uri 'https://micro.mamba.pm/api/micromamba/win-64/latest' -OutFile $archive;" ^
  "tar -xf $archive -C $tmp;" ^
  "$src = Join-Path $tmp 'Library\bin\micromamba.exe';" ^
  "Move-Item -Force $src '%MICROMAMBA_BIN%';" ^
  "Remove-Item -LiteralPath $tmp -Recurse -Force;"
if not errorlevel 1 exit /b 0

where curl.exe >nul 2>nul
if errorlevel 1 exit /b 1

powershell -NoProfile -ExecutionPolicy Bypass -Command ^
  "$ErrorActionPreference='Stop';" ^
  "$tmp = Join-Path $env:TEMP ('micromamba-' + [guid]::NewGuid().ToString());" ^
  "New-Item -ItemType Directory -Path $tmp | Out-Null;" ^
  "$archive = Join-Path $tmp 'micromamba.tar.bz2';" ^
  "& curl.exe -L 'https://micro.mamba.pm/api/micromamba/win-64/latest' -o $archive | Out-Null;" ^
  "tar -xf $archive -C $tmp;" ^
  "$src = Join-Path $tmp 'Library\bin\micromamba.exe';" ^
  "Move-Item -Force $src '%MICROMAMBA_BIN%';" ^
  "Remove-Item -LiteralPath $tmp -Recurse -Force;"
if errorlevel 1 exit /b 1
exit /b 0

:ensure_base_env
if exist "%ENV_RSCRIPT%" (
  echo.
  echo ==> Updating existing R_skana environment
  "%MICROMAMBA_BIN%" install -y -r "%MAMBA_ROOT_PREFIX%" -n R_skana --override-channels -c conda-forge -f "%ENV_FILE%"
  exit /b %errorlevel%
)

echo.
echo ==> Creating R_skana environment
"%MICROMAMBA_BIN%" create -y -r "%MAMBA_ROOT_PREFIX%" --override-channels -c conda-forge -f "%ENV_FILE%"
exit /b %errorlevel%

:ensure_project_packages
echo.
echo ==> Installing SK-Ana R packages
pushd "%REPO_ROOT%"
"%MICROMAMBA_BIN%" run -r "%MAMBA_ROOT_PREFIX%" -n R_skana Rscript "%SCRIPT_DIR%setup_r_skana.R"
set "STATUS=%errorlevel%"
popd
exit /b %STATUS%
