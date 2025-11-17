@echo off
REM ========================================
REM SK-Ana Docker Rebuild and Run Script
REM ========================================
REM This script rebuilds the Docker image after code changes
REM and runs a fresh container

echo.
echo ========================================
echo SK-Ana Docker Rebuild and Run
echo ========================================
echo.

REM Configuration
set IMAGE_NAME=skana
set IMAGE_TAG=latest
set CONTAINER_NAME=skana
set PORT=3840
set DOCKERFILE=Dockerfile

REM Step 1: Check if Docker is running
echo [1/5] Checking Docker daemon...
docker info >nul 2>&1
if errorlevel 1 (
    echo   X Error: Docker daemon is not running
    echo   Please start Docker Desktop and try again
    pause
    exit /b 1
)
echo   + Docker is running
echo.

REM Step 2: Stop and remove existing container if it exists
echo [2/5] Checking for existing container...
docker ps -a --filter "name=%CONTAINER_NAME%" --format "{{.Names}}" | findstr /C:"%CONTAINER_NAME%" >nul 2>&1
if not errorlevel 1 (
    echo   Found existing container: %CONTAINER_NAME%
    echo   Stopping container...
    docker stop %CONTAINER_NAME% >nul 2>&1
    echo   Removing container...
    docker rm %CONTAINER_NAME% >nul 2>&1
    echo   + Container removed
) else (
    echo   + No existing container found
)
echo.

REM Step 3: Remove old image (optional, forces fresh build)
echo [3/5] Removing old image (if exists)...
docker rmi %IMAGE_NAME%:%IMAGE_TAG% >nul 2>&1
echo   + Old image removed (if it existed)
echo.

REM Step 4: Build the Docker image
echo [4/5] Building Docker image...
echo   Image: %IMAGE_NAME%:%IMAGE_TAG%
echo   Dockerfile: %DOCKERFILE%
echo   This may take a few minutes...
echo.

docker build -t %IMAGE_NAME%:%IMAGE_TAG% -f %DOCKERFILE% .
if errorlevel 1 (
    echo.
    echo   X Error: Failed to build image
    pause
    exit /b 1
)
echo.
echo   + Image built successfully
echo.

REM Step 5: Run the container
echo [5/5] Starting container...
echo   Container name: %CONTAINER_NAME%
echo   Port mapping: %PORT% (local) -^> 3840 (container)
echo.

docker run -d -p %PORT%:3840 --name %CONTAINER_NAME% -e PORT=3840 %IMAGE_NAME%:%IMAGE_TAG%
if errorlevel 1 (
    echo   X Error: Failed to start container
    pause
    exit /b 1
)
echo   + Container started successfully
echo.

REM Wait a moment and verify
echo Verifying container...
timeout /t 3 /nobreak >nul
docker ps --filter "name=%CONTAINER_NAME%" --format "{{.Names}}" | findstr /C:"%CONTAINER_NAME%" >nul 2>&1
if not errorlevel 1 (
    echo   + Container is running
) else (
    echo   X Container is not running
    echo   Checking logs...
    docker logs %CONTAINER_NAME%
    pause
    exit /b 1
)

echo.
echo ========================================
echo + SK-Ana is ready!
echo ========================================
echo.
echo Access the application at:
echo   http://localhost:%PORT%
echo.
echo Useful commands:
echo   View logs:     docker logs -f %CONTAINER_NAME%
echo   Stop:          docker stop %CONTAINER_NAME%
echo   Restart:       docker restart %CONTAINER_NAME%
echo   Remove:        docker rm %CONTAINER_NAME%
echo.
echo Press any key to view live logs (Ctrl+C to exit)...
pause >nul
docker logs -f %CONTAINER_NAME%
