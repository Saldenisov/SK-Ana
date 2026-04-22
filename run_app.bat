@echo off
setlocal EnableExtensions

set "SCRIPT_DIR=%~dp0"
for %%I in ("%SCRIPT_DIR%") do set "SCRIPT_DIR=%%~fI"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
set "ORIGINAL_CWD=%CD%"
if "%SK_ANA_REPO_URL%"=="" set "SK_ANA_REPO_URL=https://github.com/Saldenisov/SK-Ana.git"
if "%SK_ANA_BRANCH%"=="" set "SK_ANA_BRANCH=master"
if "%SK_ANA_BOOTSTRAP_DIR%"=="" set "SK_ANA_BOOTSTRAP_DIR=SK-Ana"
set "PORTABLE_GIT_DIR=%SCRIPT_DIR%\.bootstrap-tools\git"
set "PORTABLE_GIT_CMD=%PORTABLE_GIT_DIR%\cmd\git.exe"
set "LAUNCH_MODE=standalone"

call :resolve_repo_root
if errorlevel 1 exit /b %errorlevel%

echo.
echo ==> Preparing SK-Ana launcher
echo     Script location: %SCRIPT_DIR%
echo     Target repo: %REPO_ROOT%
if /I "%LAUNCH_MODE%"=="git_checkout" echo     Detected a git checkout. Using it directly.
if /I "%LAUNCH_MODE%"=="snapshot" (
  echo     Detected a snapshot copy without .git.
  echo     Initializing git in this extracted folder and synchronizing from origin/%SK_ANA_BRANCH%
)
if /I "%LAUNCH_MODE%"=="standalone" echo     Detected a standalone launcher.

if /I not "%LAUNCH_MODE%"=="git_checkout" (
  call :bootstrap_repo_if_needed
  if errorlevel 1 exit /b %errorlevel%
)

call :update_repo_if_possible
if errorlevel 1 exit /b %errorlevel%

if /I not "%REPO_ROOT%"=="%SCRIPT_DIR%" (
  if exist "%REPO_ROOT%\run_app.bat" (
    echo     Handing off to %REPO_ROOT%\run_app.bat
    call "%REPO_ROOT%\run_app.bat" %*
  ) else (
    echo     Handing off directly to %REPO_ROOT%\scripts\run_app_3840.bat
    call "%REPO_ROOT%\scripts\run_app_3840.bat" %*
  )
  exit /b %errorlevel%
)

if not exist "%REPO_ROOT%\.R_skana\micromamba\envs\R_skana\Scripts\Rscript.exe" (
  echo     First launch detected. SK-Ana will install the isolated R_skana runtime and required packages before starting.
)

call "%REPO_ROOT%\scripts\run_app_3840.bat" %*
exit /b %errorlevel%

:looks_like_repo_root
if exist "%~1\app.R" if exist "%~1\scripts\run_app_3840.bat" exit /b 0
exit /b 1

:resolve_repo_root
for %%P in ("%SCRIPT_DIR%" "%ORIGINAL_CWD%" "%SCRIPT_DIR%\%SK_ANA_BOOTSTRAP_DIR%" "%ORIGINAL_CWD%\%SK_ANA_BOOTSTRAP_DIR%") do (
  call :looks_like_repo_root "%%~fP"
  if not errorlevel 1 (
    if exist "%%~fP\.git\" (
      set "LAUNCH_MODE=git_checkout"
      set "REPO_ROOT=%%~fP"
      exit /b 0
    )
  )
)

for %%P in ("%SCRIPT_DIR%" "%ORIGINAL_CWD%" "%SCRIPT_DIR%\%SK_ANA_BOOTSTRAP_DIR%" "%ORIGINAL_CWD%\%SK_ANA_BOOTSTRAP_DIR%") do (
  call :looks_like_repo_root "%%~fP"
  if not errorlevel 1 (
    set "LAUNCH_MODE=snapshot"
    set "REPO_ROOT=%%~fP"
    exit /b 0
  )
)

set "LAUNCH_MODE=standalone"
set "REPO_ROOT=%SCRIPT_DIR%\%SK_ANA_BOOTSTRAP_DIR%"
exit /b 0

:have_git
where git >nul 2>nul
if not errorlevel 1 exit /b 0
if exist "%PORTABLE_GIT_CMD%" exit /b 0
exit /b 1

:set_git_command
where git >nul 2>nul
if not errorlevel 1 (
  set "GIT_CMD=git"
  exit /b 0
)

if exist "%PORTABLE_GIT_CMD%" (
  set "GIT_CMD=%PORTABLE_GIT_CMD%"
  exit /b 0
)

exit /b 1

:ensure_portable_git
call :have_git
if not errorlevel 1 (
  call :set_git_command
  exit /b 0
)

echo.
echo ==> Downloading portable Git for Windows
echo     This stays in user space and does not require administrator rights.

mkdir "%SCRIPT_DIR%\.bootstrap-tools" 2>nul
if exist "%PORTABLE_GIT_DIR%" rmdir /s /q "%PORTABLE_GIT_DIR%"
mkdir "%PORTABLE_GIT_DIR%"

powershell -NoProfile -ExecutionPolicy Bypass -Command ^
  "$ErrorActionPreference='Stop';" ^
  "$tmp = Join-Path $env:TEMP ('mingit-' + [guid]::NewGuid().ToString());" ^
  "New-Item -ItemType Directory -Path $tmp | Out-Null;" ^
  "$archive = Join-Path $tmp 'MinGit.zip';" ^
  "Invoke-WebRequest -Uri 'https://github.com/git-for-windows/git/releases/latest/download/MinGit-64-bit.zip' -OutFile $archive;" ^
  "Expand-Archive -Path $archive -DestinationPath '%PORTABLE_GIT_DIR%' -Force;" ^
  "Remove-Item -LiteralPath $tmp -Recurse -Force;"
if errorlevel 1 (
  echo Failed to download portable Git.
  exit /b 1
)

call :set_git_command
exit /b %errorlevel%

:repo_is_dirty
call :set_git_command
if errorlevel 1 exit /b 1

"%GIT_CMD%" -C "%~1" status --porcelain 2>nul | findstr /r "." >nul
if errorlevel 1 exit /b 1
exit /b 0

:update_repo_if_possible
if not exist "%REPO_ROOT%\.git" exit /b 0

call :ensure_portable_git
if errorlevel 1 (
  echo Git is required to update the local checkout, but installation failed.
  exit /b 1
)

call :set_git_command
if errorlevel 1 exit /b 1

echo.
echo ==> Synchronizing SK-Ana checkout
echo     Discarding local tracked changes and resetting to origin/%SK_ANA_BRANCH%
"%GIT_CMD%" -C "%REPO_ROOT%" fetch --depth 1 origin "%SK_ANA_BRANCH%"
if errorlevel 1 exit /b %errorlevel%
"%GIT_CMD%" -C "%REPO_ROOT%" reset --hard "origin/%SK_ANA_BRANCH%"
if errorlevel 1 exit /b %errorlevel%
"%GIT_CMD%" -C "%REPO_ROOT%" clean -fd
exit /b %errorlevel%

:clone_repo
call :ensure_portable_git
if errorlevel 1 exit /b %errorlevel%

echo.
echo ==> Cloning SK-Ana from GitHub
"%GIT_CMD%" clone --depth 1 --branch "%SK_ANA_BRANCH%" "%SK_ANA_REPO_URL%" "%REPO_ROOT%"
exit /b %errorlevel%

:download_repo_archive
echo.
echo ==> Downloading SK-Ana source archive

powershell -NoProfile -ExecutionPolicy Bypass -Command ^
  "$ErrorActionPreference='Stop';" ^
  "$tmp = Join-Path $env:TEMP ('sk-ana-' + [guid]::NewGuid().ToString());" ^
  "New-Item -ItemType Directory -Path $tmp | Out-Null;" ^
  "$archive = Join-Path $tmp 'SK-Ana.zip';" ^
  "$repo = '%SK_ANA_REPO_URL%'.Replace('https://github.com/', '').Replace('.git', '');" ^
  "$url = 'https://github.com/' + $repo + '/archive/refs/heads/%SK_ANA_BRANCH%.zip';" ^
  "Invoke-WebRequest -Uri $url -OutFile $archive;" ^
  "Expand-Archive -Path $archive -DestinationPath $tmp -Force;" ^
  "$source = Get-ChildItem -Path $tmp -Directory | Where-Object { $_.Name -like 'SK-Ana-*' } | Select-Object -First 1;" ^
  "if (-not $source) { throw 'Could not unpack SK-Ana archive.' }" ^
  "if (Test-Path '%REPO_ROOT%') { Remove-Item -LiteralPath '%REPO_ROOT%' -Recurse -Force }" ^
  "Move-Item -LiteralPath $source.FullName -Destination '%REPO_ROOT%';" ^
  "Remove-Item -LiteralPath $tmp -Recurse -Force;"
exit /b %errorlevel%

:bootstrap_repo_if_needed
call :looks_like_repo_root "%REPO_ROOT%"
if not errorlevel 1 (
  call :initialize_snapshot_repo
  exit /b %errorlevel%
)

if exist "%REPO_ROOT%" (
  if not exist "%REPO_ROOT%\" (
    echo Bootstrap target exists but is not a directory: %REPO_ROOT%
    exit /b 1
  )
)

call :ensure_portable_git
if errorlevel 1 (
  echo Git is required to bootstrap the repository, but installation failed.
  exit /b 1
)

call :clone_repo
exit /b %errorlevel%

:initialize_snapshot_repo
call :ensure_portable_git
if errorlevel 1 (
  echo Git is required to initialize the local snapshot checkout, but installation failed.
  exit /b 1
)

call :set_git_command
if errorlevel 1 exit /b 1

echo.
echo ==> Initializing git checkout in the extracted SK-Ana folder
"%GIT_CMD%" -C "%REPO_ROOT%" init
if errorlevel 1 exit /b %errorlevel%
"%GIT_CMD%" -C "%REPO_ROOT%" remote remove origin >nul 2>nul
"%GIT_CMD%" -C "%REPO_ROOT%" remote add origin "%SK_ANA_REPO_URL%"
if errorlevel 1 exit /b %errorlevel%

call :update_repo_if_possible
exit /b %errorlevel%
