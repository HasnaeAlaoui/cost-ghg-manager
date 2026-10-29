@echo off
setlocal
title Cost ^& GHG Manager
set "SERVER=server-windows-amd64.exe"
if /I "%PROCESSOR_ARCHITECTURE%"=="ARM64" set "SERVER=server-windows-arm64.exe"
if /I "%PROCESSOR_ARCHITEW6432%"=="ARM64" set "SERVER=server-windows-arm64.exe"
"%~dp0vendor\launcher\%SERVER%"
if errorlevel 1 (
    echo The local server could not start. Keep all extracted files together.
    echo If security software blocked it, contact your instructor.
    pause
    exit /b 1
)
endlocal
