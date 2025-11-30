#!/usr/bin/env bash
#
# Time Warp IDE - Debian System Installation Script
# Installs Time Warp IDE system-wide with desktop integration
#
# Usage:
#   sudo ./install.sh                    # Install system-wide
#   sudo ./install.sh --uninstall        # Remove installation
#

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Installation directories
INSTALL_DIR="/opt/timewarp"
BIN_DIR="/usr/local/bin"
DESKTOP_DIR="/usr/share/applications"
ICON_DIR="/usr/share/icons/hicolor"
DOC_DIR="/usr/share/doc/timewarp"

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Installation options
UNINSTALL=false
        --help|-h)
            echo "Time Warp IDE Installation Script"
            echo ""
            echo "Usage:"
            echo "  sudo ./install.sh                 Install Time Warp IDE system-wide"
            echo "  sudo ./install.sh --uninstall     Remove Time Warp IDE"
            echo "  sudo ./install.sh --help          Show this help message"
            exit 0
            ;;
        *)
            echo -e "${RED}❌ Unknown option: $arg${NC}"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# Check if running as root
if [[ $EUID -ne 0 ]]; then
    echo -e "${RED}❌ This script must be run as root (use sudo)${NC}"
    exit 1
fi

# Function to print colored messages
print_info() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

print_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

print_error() {
    echo -e "${RED}❌ $1${NC}"
}

print_header() {
    echo ""
    echo -e "${BLUE}╔════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║${NC}  $1"
    echo -e "${BLUE}╚════════════════════════════════════════════════╝${NC}"
    echo ""
}

# Uninstall function
uninstall() {
    print_header "Uninstalling Time Warp IDE"
    
    # Remove binaries
    print_info "Removing binaries..."
    rm -f "$BIN_DIR/timewarp"
    rm -f "$BIN_DIR/timewarp-python"
    
    # Remove desktop files
    print_info "Removing desktop entries..."
    rm -f "$DESKTOP_DIR/timewarp-python.desktop"
    
    # Remove icons
    print_info "Removing icons..."
    rm -f "$ICON_DIR"/*/apps/timewarp.png
    
    # Remove installation directory
    print_info "Removing installation files..."
    rm -rf "$INSTALL_DIR"
    
    # Remove documentation
    rm -rf "$DOC_DIR"
    
    # Update desktop database
    if command -v update-desktop-database &> /dev/null; then
        update-desktop-database "$DESKTOP_DIR" 2>/dev/null || true
    fi
    
    print_success "Time Warp IDE has been uninstalled"
}

# Install Python dependencies
install_python_deps() {
    print_info "Installing Python dependencies..."
    
    # Check Python version
    if command -v python3 &> /dev/null; then
        PY_VERSION=$(python3 --version | awk '{print $2}')
        print_info "Found Python $PY_VERSION"
        
        # Install system packages
        print_info "Installing system packages..."
        apt-get update -qq
        apt-get install -y -qq python3-pip python3-venv &> /dev/null || {
            print_warning "Could not install python3-pip and python3-venv automatically"
            print_info "Please install manually: sudo apt-get install python3-pip python3-venv"
        }
        
        # Create virtual environment in installation directory
        print_info "Creating virtual environment..."
        python3 -m venv "$INSTALL_DIR/venv"
        
        # Install Python packages in virtual environment
        print_info "Installing PySide6 and dependencies..."
        "$INSTALL_DIR/venv/bin/pip" install --quiet --upgrade pip
        "$INSTALL_DIR/venv/bin/pip" install --quiet PySide6 pillow
        
        print_success "Python dependencies installed"
    else
        print_error "Python 3 not found. Please install Python 3.8 or higher."
        exit 1
    fi
}

# Create application icon
create_icon() {
    print_info "Creating application icon..."
    
    # Create icon directories if they don't exist
    mkdir -p "$ICON_DIR/48x48/apps"
    mkdir -p "$ICON_DIR/64x64/apps"
    mkdir -p "$ICON_DIR/128x128/apps"
    mkdir -p "$ICON_DIR/256x256/apps"
    
    # Create a simple SVG icon and convert to PNG (requires imagemagick)
    if command -v convert &> /dev/null; then
        # Create SVG
        cat > /tmp/timewarp-icon.svg << 'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<svg width="256" height="256" xmlns="http://www.w3.org/2000/svg">
  <defs>
    <linearGradient id="grad" x1="0%" y1="0%" x2="100%" y2="100%">
      <stop offset="0%" style="stop-color:#4A90E2;stop-opacity:1" />
      <stop offset="100%" style="stop-color:#7B68EE;stop-opacity:1" />
    </linearGradient>
  </defs>
  <rect width="256" height="256" rx="32" fill="url(#grad)"/>
  <text x="128" y="140" font-family="monospace" font-size="120" font-weight="bold" 
        text-anchor="middle" fill="white">TW</text>
  <path d="M 40 200 Q 128 180 216 200" stroke="white" stroke-width="8" 
        fill="none" stroke-linecap="round"/>
</svg>
EOF
        
        # Convert to multiple sizes
        convert /tmp/timewarp-icon.svg -resize 48x48 "$ICON_DIR/48x48/apps/timewarp.png" 2>/dev/null || true
        convert /tmp/timewarp-icon.svg -resize 64x64 "$ICON_DIR/64x64/apps/timewarp.png" 2>/dev/null || true
        convert /tmp/timewarp-icon.svg -resize 128x128 "$ICON_DIR/128x128/apps/timewarp.png" 2>/dev/null || true
        convert /tmp/timewarp-icon.svg -resize 256x256 "$ICON_DIR/256x256/apps/timewarp.png" 2>/dev/null || true
        
        rm /tmp/timewarp-icon.svg
        print_success "Icon created"
    else
        print_warning "ImageMagick not found - skipping icon creation"
        print_info "Install with: sudo apt-get install imagemagick"
    fi
}

# Create desktop entry for Time Warp IDE
create_desktop() {
    print_info "Creating desktop entry..."
    
    cat > "$DESKTOP_DIR/timewarp.desktop" << EOF
[Desktop Entry]
Version=1.0
Type=Application
Name=Time Warp IDE
GenericName=Educational Programming Environment
Comment=Multi-language programming environment for BASIC, PILOT, and Logo
Exec=$BIN_DIR/timewarp %F
Icon=timewarp
Terminal=false
Categories=Development;Education;IDE;
MimeType=text/x-pilot;text/x-basic;text/x-logo;
Keywords=programming;education;basic;pilot;logo;turtle;
StartupNotify=true
EOF
    
    chmod 644 "$DESKTOP_DIR/timewarp.desktop"
    print_success "Desktop entry created"
}

# Create launcher scripts
create_launchers() {
    print_info "Creating launcher scripts..."
    
    cat > "$BIN_DIR/timewarp" << EOF
#!/bin/bash
# Time Warp IDE launcher
exec "$INSTALL_DIR/venv/bin/python" "$INSTALL_DIR/time_warp_ide.py" "\$@"
EOF
    chmod 755 "$BIN_DIR/timewarp"
    print_success "Launcher created"
}

# Copy files to installation directory
install_files() {
    print_info "Copying files to $INSTALL_DIR..."
    
    # Create installation directory
    mkdir -p "$INSTALL_DIR"
    
    # Copy Python implementation (now at root level)
    print_info "Installing Python implementation..."
    cp -r "$SCRIPT_DIR/platforms/python/time_warp" "$INSTALL_DIR/"
    # cp -r "$SCRIPT_DIR/run_time_warp.py" "$INSTALL_DIR/" # Not present?
    cp -r "$SCRIPT_DIR/platforms/python/time_warp_ide.py" "$INSTALL_DIR/"
    cp -r "$SCRIPT_DIR/examples" "$INSTALL_DIR/"
    
    # Copy documentation
    print_info "Installing documentation..."
    mkdir -p "$DOC_DIR"
    cp -r "$SCRIPT_DIR/docs" "$DOC_DIR/"
    cp "$SCRIPT_DIR/README.md" "$DOC_DIR/"
    cp "$SCRIPT_DIR/CODE_OF_CONDUCT.md" "$DOC_DIR/" 2>/dev/null || true
    
    # Set permissions
    chmod -R 755 "$INSTALL_DIR"
    
    print_success "Files installed"
}

# Main installation
install() {
    print_header "Installing Time Warp IDE v4.0.0"
    
    print_info "Installation directory: $INSTALL_DIR"
    echo ""
    
    # Install Python version
    print_header "Python Implementation Setup"
    install_python_deps
    
    # Copy files
    print_header "Installing Files"
    install_files
    
    # Create icon
    print_header "Desktop Integration"
    create_icon
    
    # Create desktop entries
    create_desktop
    
    # Create launchers
    create_launchers
    
    # Update desktop database
    print_info "Updating desktop database..."
    if command -v update-desktop-database &> /dev/null; then
        update-desktop-database "$DESKTOP_DIR" 2>/dev/null || true
    fi
    
    if command -v gtk-update-icon-cache &> /dev/null; then
        gtk-update-icon-cache -f -t "$ICON_DIR" 2>/dev/null || true
    fi
    
    # Print success message
    print_header "Installation Complete!"
    
    echo -e "${GREEN}Time Warp IDE has been successfully installed!${NC}"
    echo ""
    echo "Launch methods:"
    echo "  • Command line: ${BLUE}timewarp${NC}"
    echo "  • Desktop menu: Look for 'Time Warp IDE' in Education or Development"
    echo ""
    echo "Documentation: $DOC_DIR"
    echo "Examples: $INSTALL_DIR/examples"
    echo ""
    echo "Quick test:"
    echo "  ${BLUE}timewarp $INSTALL_DIR/examples/logo_star.logo${NC}"
    echo ""
    print_info "To uninstall: sudo $0 --uninstall"
}

# Main execution
if $UNINSTALL; then
    uninstall
else
    install
fi

print_success "Done!"
