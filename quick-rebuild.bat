@echo off
REM ========================================
REM SK-Ana Quick Docker Rebuild
REM ========================================
REM Fast rebuild for code changes (uses cache)

echo.
echo ========================================
echo SK-Ana Quick Rebuild
echo ========================================
echo.

set IMAGE_NAME=skana
set CONTAINER_NAME=skana
set PORT=3840

REM Stop and remove container
echo Stopping existing container...
docker stop %CONTAINER_NAME% >nul 2>&1
docker rm %CONTAINER_NAME% >nul 2>&1
echo.

REM Quick rebuild (uses cache)
echo Building image (using cache)...
docker build -t %IMAGE_NAME%:latest .
if errorlevel 1 (
    echo Build failed!
    pause
    exit /b 1
)
echo.

REM Run container
echo Starting container...
docker run -d -p %PORT%:3840 --name %CONTAINER_NAME% -e PORT=3840 %IMAGE_NAME%:latest
echo.
echo + SK-Ana is ready at http://localhost:%PORT%
echo.
pause
