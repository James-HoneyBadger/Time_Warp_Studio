@echo off
setlocal
REM Launch DOSBox using the config in this folder, mounting this folder as C:
set HERE=%~dp0
pushd "%HERE%"
where dosbox >nul 2>&1
if %ERRORLEVEL% EQU 0 (
  set DOSBOX=dosbox
) else (
  if exist "C:\Program Files (x86)\DOSBox-0.74-3\DOSBox.exe" set DOSBOX="C:\Program Files (x86)\DOSBox-0.74-3\DOSBox.exe"
)
if "%DOSBOX%"=="" (
  echo Could not find DOSBox on PATH. Install DOSBox or update this script to point to your installation.
  popd
  exit /b 1
)
%DOSBOX% -noconsole -conf ".\dosbox-timewarp.conf"
popd
endlocal
