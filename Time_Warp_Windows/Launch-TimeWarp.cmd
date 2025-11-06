@echo off
setlocal

REM Launch Time Warp IDE via Python

set SCRIPT_DIR=%~dp0
set REPO_ROOT=%SCRIPT_DIR%..\

where python >nul 2>nul
if errorlevel 1 (
  echo Python not found on PATH. Please install Python 3.10+.
  exit /b 1
)

pushd "%REPO_ROOT%"
python Time_Warp.py %*
set EXITCODE=%ERRORLEVEL%
popd

exit /b %EXITCODE%
