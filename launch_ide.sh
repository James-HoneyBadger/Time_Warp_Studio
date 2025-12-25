#!/bin/bash

################################################################################
#                     TIME WARP IDE - LAUNCH SCRIPT                           #
#                                                                              #
# This script sets up and launches the Time Warp IDE with Python              #
# It automatically:                                                           #
#  • Creates a virtual environment if needed                                  #
#  • Installs/updates all required dependencies                               #
#  • Activates the virtual environment                                        #
#  • Launches the Python IDE                                                  #
#                                                                              #
# Usage: ./launch_ide.sh                                                      #
#        ./launch_ide.sh --reinstall  (force reinstall of dependencies)       #
#        ./launch_ide.sh --dev        (install dev dependencies too)          #
#                                                                              #
################################################################################

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_DIR="$SCRIPT_DIR/Platforms/Python"
VENV_DIR="$PYTHON_DIR/.venv"
IDE_SCRIPT="$PYTHON_DIR/time_warp_ide.py"
REQUIREMENTS_FILE="$PYTHON_DIR/requirements.txt"
REQUIREMENTS_DEV_FILE="$PYTHON_DIR/requirements-dev.txt"

# Flags
REINSTALL=false
INSTALL_DEV=false

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --reinstall)
            REINSTALL=true
            shift
            ;;
        --dev)
            INSTALL_DEV=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

echo -e "${BLUE}╔════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║          TIME WARP IDE - LAUNCHER                     ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════╝${NC}"
echo ""

# Check if Python is installed
echo -e "${BLUE}➤${NC} Checking Python installation..."
if ! command -v python3 &> /dev/null; then
    echo -e "${RED}✗ Python 3 is not installed${NC}"
    echo "  Please install Python 3.8 or later from https://python.org"
    exit 1
fi

PYTHON_VERSION=$(python3 --version 2>&1 | awk '{print $2}')
echo -e "${GREEN}✓${NC} Found Python $PYTHON_VERSION"
echo ""

# Remove venv if reinstall flag is set
if [ "$REINSTALL" = true ]; then
    echo -e "${YELLOW}⚠ Removing existing virtual environment...${NC}"
    rm -rf "$VENV_DIR"
    echo ""
fi

# Create virtual environment if it doesn't exist
if [ ! -d "$VENV_DIR" ]; then
    echo -e "${BLUE}➤${NC} Creating virtual environment..."
    python3 -m venv "$VENV_DIR"
    echo -e "${GREEN}✓${NC} Virtual environment created"
    echo ""
fi

# Activate virtual environment
echo -e "${BLUE}➤${NC} Activating virtual environment..."
source "$VENV_DIR/bin/activate"
echo -e "${GREEN}✓${NC} Virtual environment activated"
echo ""

# Upgrade pip, setuptools, wheel
echo -e "${BLUE}➤${NC} Upgrading pip and build tools..."
python3 -m pip install --upgrade pip setuptools wheel > /dev/null 2>&1
echo -e "${GREEN}✓${NC} Build tools upgraded"
echo ""

# Install main requirements
echo -e "${BLUE}➤${NC} Installing Python dependencies..."
echo "  From: $REQUIREMENTS_FILE"
pip install -r "$REQUIREMENTS_FILE" > /dev/null 2>&1
echo -e "${GREEN}✓${NC} Dependencies installed"
echo ""

# Install dev requirements if requested
if [ "$INSTALL_DEV" = true ]; then
    echo -e "${BLUE}➤${NC} Installing development dependencies..."
    echo "  From: $REQUIREMENTS_DEV_FILE"
    if [ -f "$REQUIREMENTS_DEV_FILE" ]; then
        pip install -r "$REQUIREMENTS_DEV_FILE" > /dev/null 2>&1
        echo -e "${GREEN}✓${NC} Development dependencies installed"
    else
        echo -e "${YELLOW}⚠${NC} Development requirements file not found"
    fi
    echo ""
fi

# Verify IDE script exists
if [ ! -f "$IDE_SCRIPT" ]; then
    echo -e "${RED}✗ IDE script not found: $IDE_SCRIPT${NC}"
    exit 1
fi

# Launch the IDE
echo -e "${BLUE}╔════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║              LAUNCHING TIME WARP IDE...               ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════╝${NC}"
echo ""
echo -e "${GREEN}Starting IDE...${NC}"
echo "Python: $(python3 --version 2>&1)"
echo "Environment: $VENV_DIR"
echo ""

# Launch the IDE with Python
python3 "$IDE_SCRIPT"

# If we get here, the IDE was closed
echo ""
echo -e "${BLUE}IDE closed${NC}"
