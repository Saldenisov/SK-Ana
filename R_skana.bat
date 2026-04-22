@echo off
setlocal
call "%~dp0scripts\r_skana.bat" %*
exit /b %errorlevel%
