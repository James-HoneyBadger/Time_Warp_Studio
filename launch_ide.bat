@echo off
REM ============================================================================
REM              TIME WARP IDE - LAUNCH SCRIPT (WINDOWS)
REM ============================================================================
REM This script sets up and launches the Time Warp IDE with Python
REM It automatically:
REM  • Creates a virtual environment if needed
REM  • Installs/updates all required dependencies
REM  • Activates the virtual environment
REM  • Launches the Python IDE
REM
REM Usage: launch_ide.bat
REM        launch_ide.bat --reinstall  (force reinstall of dependencies)
REM        launch_ide.bat --dev        (install dev dependencies too)
REM ============================================================================

setlocal enabledelayedexpansion

REM Configuration
set "SCRIPT_DIR=%~dp0"
set "PYTHON_DIR=%SCRIPT_DIR%Platforms\Python"
set "VENV_DIR=%PYTHON_DIR%\.venv"
set "IDE_SCRIPT=%PYTHON_DIR%\time_warp_ide.py"
set "REQUIREMENTS_FILE=%PYTHON_DIR%\requirements.txt"
set "REQUIREMENTS_DEV_FILE=%PYTHON_DIR%\requirements-dev.txt"

REM Flags
set "REINSTALL=false"
set "INSTALL_DEV=false"

REM Parse command line arguments
:parse_args
if "%1"=="" goto args_done
if "%1"=="--reinstall" (
    set "REINSTALL=true"
    shift
    goto parse_args
)
if "%1"=="--dev" (
    set "INSTALL_DEV=true"
    shift
    goto parse_args
)
shift
goto parse_args

:args_done
echo.
echo ============================================================
echo          TIME WARP IDE - LAUNCHER (Windows)
echo ============================================================
echo.

REM Check if Python is installed
echo Checking Python installation...
python --version >nul 2>&1
if errorlevel 1 (
    echo.
    echo ERROR: Python is not installed or not in PATH
    echo Please install Python 3.8 or later from https://python.org
    echo.
    pause
    exit /b 1
)

for /f "tokens=2" %%i in ('python --version 2^>^&1') do set "PYTHON_VERSION=%%i"
echo [OK] Found Python %PYTHON_VERSION%
echo.

REM Remove venv if reinstall flag is set
if "%REINSTALL%"=="true" (
    echo Removing existing virtual environment...
    if exist "%VENV_DIR%" rmdir /s /q "%VENV_DIR%"
    echo.
)

REM Create virtual environment if it doesn't exist
if not exist "%VENV_DIR%" (
    echo Creating virtual environment...
    python -m venv "%VENV_DIR%"
    echo [OK] Virtual environment created
    echo.
)

REM Activate virtual environment
echo Activating virtual environment...
call "%VENV_DIR%\Scripts\activate.bat"
echo [OK] Virtual environment activated
echo.

REM Upgrade pip, setuptools, wheel
echo Upgrading pip and build tools...
python -m pip install --upgrade pip setuptools wheel >nul 2>&1
echo [OK] Build tools upgraded
echo.

REM Install main requirements
echo Installing Python dependencies...
echo From: %REQUIREMENTS_FILE%
pip install -r "%REQUIREMENTS_FILE%" >nul 2>&1
if errorlevel 1 (
    echo ERROR: Failed to install dependencies
    pause
    exit /b 1
)
echo [OK] Dependencies installed
echo.

REM Install dev requirements if requested
if "%INSTALL_DEV%"=="true" (
    echo Installing development dependencies...
    if exist "%REQUIREMENTS_DEV_FILE%" (
        pip install -r "%REQUIREMENTS_DEV_FILE%" >nul 2>&1
        echo [OK] Development dependencies installed
    ) else (
        echo WARNING: Development requirements file not found
    )
    echo.
)

REM Verify IDE script exists
if not exist "%IDE_SCRIPT%" (
    echo ERROR: IDE script not found: %IDE_SCRIPT%
    pause
    exit /b 1
)

REM Launch the IDE
echo.
echo ============================================================
echo              LAUNCHING TIME WARP IDE...
echo ============================================================
echo.
echo Starting IDE...
python --version
echo Environment: %VENV_DIR%
echo.

REM Launch the IDE with Python
python "%IDE_SCRIPT%"

REM If we get here, the IDE was closed
echo.
echo IDE closed
pause
