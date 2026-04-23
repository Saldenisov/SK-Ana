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
set "PORTABLE_GIT_RELEASE_API=https://api.github.com/repos/git-for-windows/git/releases/latest"

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

call :ensure_dir "%SCRIPT_DIR%\.bootstrap-tools"
if errorlevel 1 exit /b %errorlevel%
if exist "%PORTABLE_GIT_DIR%" rmdir /s /q "%PORTABLE_GIT_DIR%"
call :ensure_dir "%PORTABLE_GIT_DIR%"
if errorlevel 1 exit /b %errorlevel%

set "TMP_GIT_DIR=%TEMP%\mingit-%RANDOM%%RANDOM%"
set "TMP_GIT_ARCHIVE=%TMP_GIT_DIR%\MinGit.zip"
set "PORTABLE_GIT_URL="
call :ensure_dir "%TMP_GIT_DIR%"
if errorlevel 1 exit /b %errorlevel%

call :resolve_portable_git_url
if errorlevel 1 goto :portable_git_failed

call :download_url_to_file "%PORTABLE_GIT_URL%" "%TMP_GIT_ARCHIVE%"
if errorlevel 1 goto :portable_git_failed

call :extract_zip_to_dir "%TMP_GIT_ARCHIVE%" "%PORTABLE_GIT_DIR%"
if errorlevel 1 goto :portable_git_failed

if exist "%TMP_GIT_DIR%" rmdir /s /q "%TMP_GIT_DIR%"
if errorlevel 1 (
  echo Failed to download portable Git.
  exit /b 1
)

call :set_git_command
exit /b %errorlevel%

:portable_git_failed
if exist "%TMP_GIT_DIR%" rmdir /s /q "%TMP_GIT_DIR%"
echo Failed to download portable Git.
exit /b 1

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

set "TMP_REPO_DIR=%TEMP%\sk-ana-%RANDOM%%RANDOM%"
set "TMP_REPO_ARCHIVE=%TMP_REPO_DIR%\SK-Ana.zip"
set "TMP_REPO_EXTRACT=%TMP_REPO_DIR%\extract"
set "REPO_ARCHIVE_URL=%SK_ANA_REPO_URL%"
set "REPO_ARCHIVE_URL=%REPO_ARCHIVE_URL:https://github.com/=%"
set "REPO_ARCHIVE_URL=%REPO_ARCHIVE_URL:.git=%"
set "REPO_ARCHIVE_URL=https://github.com/%REPO_ARCHIVE_URL%/archive/refs/heads/%SK_ANA_BRANCH%.zip"

call :ensure_dir "%TMP_REPO_DIR%"
if errorlevel 1 exit /b %errorlevel%
call :ensure_dir "%TMP_REPO_EXTRACT%"
if errorlevel 1 exit /b %errorlevel%

call :download_url_to_file "%REPO_ARCHIVE_URL%" "%TMP_REPO_ARCHIVE%"
if errorlevel 1 goto :download_repo_archive_failed

call :extract_zip_to_dir "%TMP_REPO_ARCHIVE%" "%TMP_REPO_EXTRACT%"
if errorlevel 1 goto :download_repo_archive_failed

for /d %%D in ("%TMP_REPO_EXTRACT%\SK-Ana-*") do (
  set "REPO_ARCHIVE_SOURCE=%%~fD"
  goto :download_repo_archive_source_found
)

echo Could not unpack SK-Ana archive.
goto :download_repo_archive_failed

:download_repo_archive_source_found
if exist "%REPO_ROOT%" rmdir /s /q "%REPO_ROOT%"
move "%REPO_ARCHIVE_SOURCE%" "%REPO_ROOT%" >nul
if errorlevel 1 goto :download_repo_archive_failed
if exist "%TMP_REPO_DIR%" rmdir /s /q "%TMP_REPO_DIR%"
exit /b %errorlevel%

:download_repo_archive_failed
if exist "%TMP_REPO_DIR%" rmdir /s /q "%TMP_REPO_DIR%"
exit /b 1

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

:ensure_dir
if exist "%~1\" exit /b 0
mkdir "%~1" >nul 2>nul
if exist "%~1\" exit /b 0
echo Failed to create directory: %~1
exit /b 1

:download_url_to_file
set "DOWNLOAD_URL=%~1"
set "DOWNLOAD_TARGET=%~2"

powershell -NoProfile -ExecutionPolicy Bypass -Command ^
  "$ErrorActionPreference='Stop';" ^
  "[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12 -bor [Net.SecurityProtocolType]::Tls11 -bor [Net.SecurityProtocolType]::Tls;" ^
  "$client = New-Object System.Net.WebClient;" ^
  "$client.Headers.Add('User-Agent', 'SK-Ana bootstrap');" ^
  "$client.DownloadFile('%DOWNLOAD_URL%', '%DOWNLOAD_TARGET%');"
if not errorlevel 1 exit /b 0

where curl.exe >nul 2>nul
if errorlevel 1 exit /b 1

curl.exe -L --fail --retry 3 -H "User-Agent: SK-Ana bootstrap" "%DOWNLOAD_URL%" -o "%DOWNLOAD_TARGET%"
if errorlevel 1 exit /b 1
exit /b 0

:resolve_portable_git_url
for /f "usebackq delims=" %%I in (`powershell -NoProfile -ExecutionPolicy Bypass -Command ^
  "$ErrorActionPreference='Stop';" ^
  "[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12 -bor [Net.SecurityProtocolType]::Tls11 -bor [Net.SecurityProtocolType]::Tls;" ^
  "$client = New-Object System.Net.WebClient;" ^
  "$client.Headers.Add('User-Agent', 'SK-Ana bootstrap');" ^
  "$json = $client.DownloadString('%PORTABLE_GIT_RELEASE_API%');" ^
  "$m = [regex]::Match($json, '\"browser_download_url\"\\s*:\\s*\"(?<url>[^\"]*MinGit-[^\"]*-64-bit\\.zip)\"');" ^
  "if (-not $m.Success) { throw 'Could not resolve MinGit asset URL.' }" ^
  "$url = $m.Groups['url'].Value.Replace('\/', '/');" ^
  "[Console]::WriteLine($url);"`) do (
  set "PORTABLE_GIT_URL=%%I"
  goto :resolve_portable_git_url_done
)

where curl.exe >nul 2>nul
if errorlevel 1 exit /b 1

for /f "usebackq tokens=* delims=" %%I in (`curl.exe -fsSL -H "User-Agent: SK-Ana bootstrap" "%PORTABLE_GIT_RELEASE_API%" ^| findstr /R /C:"browser_download_url.*MinGit-.*-64-bit\\.zip"`) do (
  set "PORTABLE_GIT_JSON_LINE=%%I"
  call :extract_json_url PORTABLE_GIT_JSON_LINE PORTABLE_GIT_URL
  if defined PORTABLE_GIT_URL goto :resolve_portable_git_url_done
)

exit /b 1

:resolve_portable_git_url_done
if not defined PORTABLE_GIT_URL exit /b 1
exit /b 0

:extract_json_url
setlocal EnableDelayedExpansion
set "JSON_LINE=!%~1!"
for /f "tokens=2 delims=:" %%A in ("!JSON_LINE!") do (
  set "JSON_REST=%%A"
)
if not defined JSON_REST exit /b 1
set "JSON_REST=!JSON_LINE:*://=!"
set "JSON_REST=https://!JSON_REST!"
for /f "delims=" %%A in ("!JSON_REST!") do set "JSON_REST=%%~A"
set "JSON_REST=!JSON_REST:,=!"
set "JSON_REST=!JSON_REST:"=!"
endlocal & set "%~2=%JSON_REST%"
exit /b 0

:extract_zip_to_dir
set "ZIP_SOURCE=%~1"
set "ZIP_DEST=%~2"

powershell -NoProfile -ExecutionPolicy Bypass -Command ^
  "$ErrorActionPreference='Stop';" ^
  "Add-Type -AssemblyName System.IO.Compression.FileSystem;" ^
  "[System.IO.Compression.ZipFile]::ExtractToDirectory('%ZIP_SOURCE%', '%ZIP_DEST%');"
if not errorlevel 1 exit /b 0

where tar.exe >nul 2>nul
if errorlevel 1 exit /b 1

tar.exe -xf "%ZIP_SOURCE%" -C "%ZIP_DEST%"
if errorlevel 1 exit /b 1
exit /b 0
