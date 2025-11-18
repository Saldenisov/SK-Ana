@echo off
echo Checking if repository exists...
docker manifest inspect saldenisov/skana:latest >nul 2>&1
if %errorlevel% neq 0 (
    echo Repository saldenisov/skana does not exist or is not accessible.
    echo Attempting to push anyway - Docker Hub will create it if you have permissions...
)
docker push saldenisov/skana:latest
if %errorlevel% equ 0 (
    echo Successfully pushed to saldenisov/skana:latest
) else (
    echo Failed to push. Make sure you are logged in and have permissions.
    echo Run: docker login
)
