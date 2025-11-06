#!/bin/bash
# Time Warp IDE - Installation Test Suite
# Verifies that Time Warp is correctly installed and working

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}╔════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║${NC}  Time Warp IDE - Installation Test Suite"
echo -e "${BLUE}╚════════════════════════════════════════════════╝${NC}"
echo ""

ERRORS=0
WARNINGS=0

# Test function
test_command() {
    local cmd="$1"
    local desc="$2"
    
    echo -n "Testing $desc... "
    if command -v "$cmd" &> /dev/null; then
        echo -e "${GREEN}✓${NC}"
        return 0
    else
        echo -e "${RED}✗${NC}"
        ((ERRORS++))
        return 1
    fi
}

test_file() {
    local file="$1"
    local desc="$2"
    
    echo -n "Checking $desc... "
    if [ -f "$file" ] || [ -d "$file" ]; then
        echo -e "${GREEN}✓${NC}"
        return 0
    else
        echo -e "${YELLOW}⚠${NC}"
        ((WARNINGS++))
        return 1
    fi
}

# Test PATH
echo -e "${BLUE}=== PATH Configuration ===${NC}"
test_command "timewarp" "timewarp command"
test_command "timewarp-python" "timewarp-python command"
test_command "timewarp-rust" "timewarp-rust command"
echo ""

# Test files
echo -e "${BLUE}=== Installation Files ===${NC}"
test_file "$HOME/.local/share/timewarp" "Installation directory"
test_file "$HOME/.local/share/timewarp/examples" "Examples directory"
test_file "$HOME/.local/share/timewarp/Time_Warp_Python" "Python implementation"
test_file "$HOME/.local/share/timewarp/Time_Warp_Rust" "Rust implementation"
test_file "$HOME/.local/share/timewarp/venv" "Python virtual environment"
echo ""

# Test executables
echo -e "${BLUE}=== Executables ===${NC}"
test_file "$HOME/.local/bin/timewarp" "Default launcher"
test_file "$HOME/.local/bin/timewarp-python" "Python launcher"
test_file "$HOME/.local/bin/timewarp-rust" "Rust launcher"
echo ""

# Test desktop integration
echo -e "${BLUE}=== Desktop Integration ===${NC}"
test_file "$HOME/.local/share/applications/timewarp-python.desktop" "Python desktop entry"
test_file "$HOME/.local/share/applications/timewarp-rust.desktop" "Rust desktop entry"
test_file "$HOME/.local/share/icons/hicolor/48x48/apps/timewarp.png" "Icon (48x48)"
test_file "$HOME/.local/share/icons/hicolor/256x256/apps/timewarp.png" "Icon (256x256)"
echo ""

# Test documentation
echo -e "${BLUE}=== Documentation ===${NC}"
test_file "$HOME/.local/share/doc/timewarp" "Documentation directory"
test_file "$HOME/.local/share/doc/timewarp/docs/USER_GUIDE.md" "User Guide"
test_file "$HOME/.local/share/doc/timewarp/README.md" "Main README"
echo ""

# Test examples
echo -e "${BLUE}=== Example Programs ===${NC}"
EXAMPLE_COUNT=$(ls "$HOME/.local/share/timewarp/examples/"*.{pilot,bas,logo,tc} 2>/dev/null | wc -l)
echo "Found $EXAMPLE_COUNT example programs"
if [ $EXAMPLE_COUNT -gt 0 ]; then
    echo -e "${GREEN}✓${NC} Examples installed"
else
    echo -e "${RED}✗${NC} No examples found"
    ((ERRORS++))
fi
echo ""

# Test Python version
echo -e "${BLUE}=== Python Environment ===${NC}"
if [ -f "$HOME/.local/share/timewarp/venv/bin/python" ]; then
    PY_VERSION=$("$HOME/.local/share/timewarp/venv/bin/python" --version 2>&1)
    echo "Python: $PY_VERSION"
    
    echo -n "PySide6... "
    if "$HOME/.local/share/timewarp/venv/bin/python" -c "import PySide6" 2>/dev/null; then
        echo -e "${GREEN}✓${NC}"
    else
        echo -e "${RED}✗${NC}"
        ((ERRORS++))
    fi
    
    echo -n "Pillow... "
    if "$HOME/.local/share/timewarp/venv/bin/python" -c "import PIL" 2>/dev/null; then
        echo -e "${GREEN}✓${NC}"
    else
        echo -e "${YELLOW}⚠${NC} (optional)"
        ((WARNINGS++))
    fi
else
    echo -e "${RED}✗${NC} Python virtual environment not found"
    ((ERRORS++))
fi
echo ""

# Test Rust version
echo -e "${BLUE}=== Rust Build ===${NC}"
RUST_BIN="$HOME/.local/share/timewarp/Time_Warp_Rust/target/release/time-warp"
if [ -f "$RUST_BIN" ]; then
    echo -e "${GREEN}✓${NC} Rust binary built"
    RUST_SIZE=$(du -h "$RUST_BIN" | cut -f1)
    echo "Binary size: $RUST_SIZE"
else
    echo -e "${RED}✗${NC} Rust binary not found"
    ((ERRORS++))
fi
echo ""

# Summary
echo -e "${BLUE}╔════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║${NC}  Test Summary"
echo -e "${BLUE}╚════════════════════════════════════════════════╝${NC}"
echo ""

if [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
    echo -e "${GREEN}✅ All tests passed!${NC}"
    echo ""
    echo "Time Warp IDE is correctly installed and ready to use."
    echo ""
    echo "Try it out:"
    echo "  ${BLUE}timewarp${NC}"
    echo ""
    exit 0
elif [ $ERRORS -eq 0 ]; then
    echo -e "${YELLOW}⚠ Tests completed with $WARNINGS warnings${NC}"
    echo ""
    echo "Time Warp IDE should work but some optional components are missing."
    echo ""
    exit 0
else
    echo -e "${RED}❌ Tests failed with $ERRORS errors and $WARNINGS warnings${NC}"
    echo ""
    echo "Some components are missing or incorrectly installed."
    echo "Try reinstalling: ./install-user.sh"
    echo ""
    exit 1
fi
