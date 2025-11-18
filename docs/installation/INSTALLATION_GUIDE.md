# 🚀 Time Warp IDE - Installation Guide

**Complete Platform-Specific Installation & Deployment Manual**

This comprehensive guide covers installation procedures, system requirements, troubleshooting, and deployment strategies for all Time Warp IDE implementations across different platforms and educational environments.

---

## 📋 Quick Installation Matrix

| Platform | Best For | Installation Time | Difficulty | Network Required |
|----------|----------|------------------|------------|------------------|
| 🌐 **Web** | Chromebooks, Quick Start | **Instant** | ⭐ Beginner | Initial only |
| 🐍 **Python** | Schools, Raspberry Pi | **2-5 minutes** | ⭐⭐ Easy | Yes |
| 🚀 **Rust** | Performance, Advanced | **5-15 minutes** | ⭐⭐⭐ Moderate | Build: Yes, Run: No |
| 🪟 **Windows** | Enterprise, IT Deployment | **10-30 minutes** | ⭐⭐⭐ Moderate | Yes |
| 💾 **DOS** | Retro Computing, History | **5-10 minutes** | ⭐⭐⭐⭐ Advanced | No |
| 🍎 **Apple** | iPad/Mac Classrooms | **5-10 minutes** | ⭐⭐ Easy | App Store: Yes |

---

## 🌐 Web Implementation - Zero Install

### **🎯 Perfect For**
- Chromebook classrooms and 1:1 device programs
- Quick demonstrations and immediate access
- Remote learning and mobile devices
- Environments with restricted software installation

### **⚡ Instant Access Methods**

#### **Method 1: Direct Browser Access**
```bash
# Simply open in any modern browser:
https://timewarp.edu/ide
# Or local deployment:
file:///path/to/Time_Warp_Web/index.html
```

#### **Method 2: Offline Progressive Web App**
1. **Visit the web app** in Chrome, Safari, or Edge
2. **Install PWA** - Click browser's "Install" prompt or "Add to Home Screen"
3. **Works Offline** - All features available without internet after first visit
4. **Auto-Updates** - Automatically receives new features when online

#### **Method 3: School Network Deployment**
```bash
# Deploy to school web server
sudo cp -r Time_Warp_Web/* /var/www/html/timewarp/
# Configure NGINX or Apache virtual host
# Students access via: http://school-server/timewarp/
```

### **📱 Mobile & Tablet Optimization**

#### **iPad/Tablet Setup**
- **Safari/Chrome**: Full desktop-like experience with touch gestures
- **Split View**: Code editor on left, canvas on right
- **Touch Controls**: Pinch-to-zoom, swipe navigation
- **Virtual Keyboard**: Auto-complete and symbol shortcuts

#### **Phone Optimization**  
- **Portrait Mode**: Stacked interface with swipe between code/canvas
- **Landscape Mode**: Side-by-side layout like desktop
- **Touch Gestures**: Tap to edit, swipe to navigate sections
- **Responsive Design**: Adapts to all screen sizes automatically

### **🔧 Browser Requirements & Compatibility**

| Browser | Version | Support Level | Notes |
|---------|---------|---------------|--------|
| **Chrome** | 80+ | ✅ Full | Recommended, best performance |
| **Safari** | 13+ | ✅ Full | Excellent on iOS/macOS |
| **Firefox** | 75+ | ✅ Full | Great cross-platform choice |
| **Edge** | 80+ | ✅ Full | Windows integration |
| **Opera** | 70+ | ⚠️ Mostly | Some advanced features limited |

### **🎓 Educational Deployment Strategies**

#### **Chromebook Classroom Setup**
```javascript
// Add to Chrome management console for district deployment
{
  "homepage": "https://school.edu/timewarp",
  "managed_bookmarks": [
    {"name": "Time Warp IDE", "url": "https://school.edu/timewarp"}
  ],
  "force_installed_apps": ["timewarp-pwa-id"]
}
```

#### **Shared Computer Configuration**
- **Kiosk Mode**: Lock browsers to Time Warp URL for dedicated programming stations
- **User Isolation**: LocalStorage keeps student work separate per login
- **Reset Scripts**: Clear sessions between users with admin shortcuts
- **Network Policies**: Allow Time Warp domain while restricting others

### **❌ Troubleshooting Web Version**

#### **Performance Issues**
```javascript
// Check browser performance
console.log("Available Memory:", navigator.deviceMemory);
console.log("CPU Cores:", navigator.hardwareConcurrency);
// Reduce graphics quality if hardware is limited
```

#### **Storage Problems** 
```javascript
// Check LocalStorage availability
try {
  localStorage.setItem('test', 'data');
  localStorage.removeItem('test');
  console.log('LocalStorage: Available');
} catch(e) {
  console.log('LocalStorage: Blocked - check browser settings');
}
```

#### **Common Issues & Solutions**
- **Programs Don't Save**: Check if cookies/localStorage enabled
- **Graphics Don't Display**: Ensure Canvas support, update browser  
- **Slow Performance**: Close other tabs, check available RAM
- **Touch Issues**: Enable touch events in browser developer settings

---

## 🐍 Python Implementation - Universal Access

### **🎯 Perfect For**
- Educational institutions with diverse hardware
- Raspberry Pi and maker spaces  
- Cross-platform compatibility requirements
- Environments where students can explore source code

### **⚙️ System Requirements**

#### **Minimum Requirements**
- **Python**: 3.8 or newer (3.10+ recommended)
- **RAM**: 2GB available (4GB+ for graphics-intensive projects)
- **Storage**: 500MB for full installation with examples
- **Display**: 1024×768 minimum (1920×1080+ recommended)

#### **Supported Platforms**
- **Windows**: 10/11 (x64, ARM64)
- **macOS**: 10.15+ (Intel and Apple Silicon)
- **Linux**: Ubuntu 18.04+, Debian 10+, CentOS 8+, Arch, Fedora
- **Raspberry Pi**: Raspberry Pi OS (Bullseye+), Ubuntu for Pi
- **Chrome OS**: Linux container support required

### **🚀 Installation Methods**

#### **Method 1: Quick Install (Recommended)**
```bash
# One-command installation
curl -fsSL https://get.timewarp.edu/python | python3

# Or with wget
wget -qO- https://get.timewarp.edu/python | python3

# Manual verification
python3 -c "import sys; print(f'Python {sys.version} ready')"
pip3 install --upgrade timewarp-ide
timewarp-ide
```

#### **Method 2: From Source (Development)**
```bash
# Clone and install in development mode
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp/Time_Warp_Python

# Create virtual environment (recommended)
python3 -m venv venv
source venv/bin/activate  # Linux/Mac
# or: venv\Scripts\activate  # Windows

# Install dependencies
pip install -r requirements.txt
pip install -e .

# Launch
python time_warp_ide.py
```

#### **Method 3: System Package Installation**
```bash
# Ubuntu/Debian
sudo apt update
sudo apt install python3-pip python3-tk
pip3 install timewarp-ide

# CentOS/RHEL/Fedora  
sudo dnf install python3-pip python3-tkinter
pip3 install timewarp-ide

# macOS with Homebrew
brew install python-tk
pip3 install timewarp-ide

# Arch Linux
sudo pacman -S python-pip tk
pip install timewarp-ide
```

### **🥧 Raspberry Pi Deployment**

#### **Raspberry Pi OS Setup**
```bash
# Update system
sudo apt update && sudo apt upgrade -y

# Install dependencies
sudo apt install python3-pip python3-pyside6 python3-pillow -y

# Install Time Warp
pip3 install timewarp-ide

# Create desktop shortcut
cat > ~/Desktop/TimeWarp.desktop << 'EOF'
[Desktop Entry]
Version=1.0
Type=Application  
Name=Time Warp IDE
Comment=Educational Programming Environment
Exec=python3 -m timewarp_ide
Icon=timewarp
Terminal=false
Categories=Education;Development;
EOF

chmod +x ~/Desktop/TimeWarp.desktop
```

#### **Performance Optimization for Pi**
```bash
# Increase GPU memory split for better graphics
echo "gpu_mem=128" | sudo tee -a /boot/config.txt

# Enable hardware acceleration  
echo "dtoverlay=vc4-kms-v3d" | sudo tee -a /boot/config.txt

# Optimize Python for Pi
echo "export PYTHONOPTIMIZE=1" >> ~/.bashrc
echo "export PYTHONDONTWRITEBYTECODE=1" >> ~/.bashrc

sudo reboot
```

### **🏫 School Network Installation**

#### **Shared Network Drive Setup**
```bash
# Install to shared location accessible by all students
sudo mkdir -p /opt/timewarp
sudo git clone https://github.com/James-HoneyBadger/Time_Warp.git /opt/timewarp
cd /opt/timewarp/Time_Warp_Python

# Install system-wide
sudo pip3 install -e .

# Create launcher script
sudo tee /usr/local/bin/timewarp << 'EOF'
#!/bin/bash
cd /opt/timewarp/Time_Warp_Python
python3 time_warp_ide.py "$@"
EOF
sudo chmod +x /usr/local/bin/timewarp

# Students can now run: timewarp
```

#### **User Profile Management**
```python
# Configure user-specific settings directory
import os
from pathlib import Path

# Student work directory
STUDENT_DIR = Path.home() / "TimeWarp_Projects" 
STUDENT_DIR.mkdir(exist_ok=True)

# Separate settings per user
CONFIG_DIR = Path.home() / ".config" / "timewarp"
CONFIG_DIR.mkdir(parents=True, exist_ok=True)
```

### **❌ Troubleshooting Python Version**

#### **Dependency Issues**
```bash
# Check Python version
python3 --version  # Should be 3.8+

# Check PySide6 installation  
python3 -c "import PySide6; print('PySide6 OK')"

# Fix common issues
pip3 install --upgrade pip setuptools wheel
pip3 install --force-reinstall PySide6

# Alternative Qt backend if PySide6 fails
pip3 install PyQt6
export QT_API=pyqt6
```

#### **Graphics/Display Problems**
```bash
# Check display connection
echo $DISPLAY  # Should show :0 or :1

# Test X11 forwarding (SSH)
ssh -X username@hostname
xeyes  # Should show graphical eyes

# Wayland compatibility
export QT_QPA_PLATFORM=wayland
# or force X11: export QT_QPA_PLATFORM=xcb
```

#### **Permission Errors**
```bash  
# Fix pip permissions
python3 -m pip install --user timewarp-ide

# Create user bin directory
mkdir -p ~/.local/bin
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

#### **Virtual Environment Issues**
```bash
# Recreate clean environment
rm -rf venv
python3 -m venv venv --clear
source venv/bin/activate
pip install --upgrade pip
pip install timewarp-ide
```

---

## 🚀 Rust Implementation - Native Performance

### **🎯 Perfect For**
- Performance-critical computational projects
- Advanced programming courses and computer science
- Single-binary deployment without dependencies
- Cross-platform development environments

### **⚙️ System Requirements**

#### **Runtime Requirements** *(for pre-built binaries)*
- **RAM**: 1GB available (2GB+ for large projects)
- **Storage**: 50MB for executable + 200MB for examples
- **Graphics**: OpenGL 3.0+ or equivalent (for turtle graphics)
- **CPU**: Any 64-bit processor (x86_64, ARM64)

#### **Build Requirements** *(for compilation from source)*
- **Rust**: 1.70.0 or newer (latest stable recommended)  
- **RAM**: 4GB available (8GB+ recommended for compilation)
- **Storage**: 2GB for Rust toolchain + dependencies
- **Network**: Required for initial dependency download

### **🚀 Installation Methods**

#### **Method 1: Pre-built Binaries (Fastest)**
```bash
# Download latest release for your platform
curl -L https://github.com/James-HoneyBadger/Time_Warp/releases/latest/download/time-warp-$(uname -s)-$(uname -m).tar.gz | tar xz

# Move to system location
sudo mv time-warp /usr/local/bin/
chmod +x /usr/local/bin/time-warp

# Verify installation
time-warp --version
```

#### **Method 2: Cargo Install (Rust Package Manager)**
```bash
# Install Rust if not present
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env

# Install Time Warp from crates.io
cargo install timewarp-ide

# Or install from git repository  
cargo install --git https://github.com/James-HoneyBadger/Time_Warp.git --bin time-warp
```

#### **Method 3: Build from Source (Development)**
```bash
# Clone repository
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp/Time_Warp_Rust

# Build debug version (faster compile)
cargo build
./target/debug/time-warp

# Build optimized release
cargo build --release
./target/release/time-warp
```

### **🏗️ Development Setup**

#### **VS Code Integration**
```json
// .vscode/settings.json
{
    "rust-analyzer.cargo.features": ["all"],
    "rust-analyzer.checkOnSave.command": "clippy",
    "rust-analyzer.procMacro.enable": true,
    "files.associations": {
        "*.spt": "templecode"
    }
}
```

#### **IntelliJ IDEA / CLion Setup**
```bash
# Install Rust plugin
# Configure cargo integration
# Set up run configurations for examples
cargo run --bin time-warp -- examples/basic_example.spt
```

### **🎯 Performance Optimization**

#### **Compile-Time Optimizations**
```toml
# Cargo.toml profile optimizations
[profile.release]
opt-level = 3
lto = true
codegen-units = 1
panic = "abort"
strip = true

[profile.dev]
opt-level = 1
debug = true
```

#### **Runtime Performance Tuning**  
```bash
# Set optimal thread count
export RAYON_NUM_THREADS=$(nproc)

# Enable graphics acceleration
export WGPU_BACKEND=vulkan  # or dx12, metal, gl

# Increase stack size for deep recursion
export RUST_MIN_STACK=8388608
```

### **🏫 Classroom Deployment**

#### **Network Installation Script**
```bash
#!/bin/bash
# deploy-timewarp-rust.sh - Deploy to multiple machines

MACHINES=("student1.local" "student2.local" "student3.local")
BINARY_URL="https://github.com/James-HoneyBadger/Time_Warp/releases/latest/download/time-warp-linux-x86_64.tar.gz"

for machine in "${MACHINES[@]}"; do
    echo "Deploying to $machine..."
    ssh "$machine" "
        curl -L $BINARY_URL | sudo tar xz -C /usr/local/bin/
        sudo chmod +x /usr/local/bin/time-warp
        time-warp --version
    "
done
```

#### **Docker Container Deployment**
```dockerfile
# Dockerfile for containerized deployment
FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y curl ca-certificates
COPY time-warp /usr/local/bin/
EXPOSE 8080
CMD ["time-warp", "--web-server", "--port", "8080"]
```

### **❌ Troubleshooting Rust Version**

#### **Compilation Issues**
```bash
# Update Rust toolchain
rustup update stable
rustup default stable

# Clear cargo cache
cargo clean
rm -rf ~/.cargo/registry/cache
rm -rf ~/.cargo/git/db

# Rebuild with verbose output
cargo build --verbose --release
```

#### **Runtime Graphics Problems**
```bash
# Check graphics drivers
lspci | grep -i vga
glxinfo | grep -i vendor

# Alternative graphics backends
WGPU_BACKEND=gl time-warp      # OpenGL fallback
WGPU_BACKEND=vulkan time-warp  # Vulkan (if available)

# Software rendering (slow but compatible)  
LIBGL_ALWAYS_SOFTWARE=1 time-warp
```

#### **Performance Issues**
```bash
# Profile performance
cargo install flamegraph
cargo flamegraph --bin time-warp -- examples/complex_example.spt

# Check system resources
htop
iostat -x 1

# Optimize binary size
cargo build --release
strip target/release/time-warp
upx --best target/release/time-warp  # Optional compression
```

---

## 🪟 Windows Implementation - Enterprise Deployment

### **🎯 Perfect For**
- District-wide educational technology rollouts
- Windows-centric school IT environments  
- Active Directory integrated classrooms
- Enterprise compliance and security requirements

### **⚙️ System Requirements**

#### **Minimum Requirements**
- **OS**: Windows 10 version 1809+ or Windows 11
- **RAM**: 4GB available (8GB+ recommended for multiple users)  
- **Storage**: 2GB for full installation with examples
- **Display**: 1366×768 minimum (1920×1080+ recommended)
- **Network**: Required for initial download and updates

#### **Administrative Requirements**
- **Local Admin Rights**: For MSI installation and system integration
- **Group Policy Access**: For district-wide configuration management
- **Windows Installer**: Version 5.0+ (included in Windows 10+)
- **PowerShell**: 5.1+ for deployment automation

### **🚀 Installation Methods**

#### **Method 1: MSI Package (Enterprise Recommended)**
```powershell
# Download and install MSI package
$url = "https://github.com/James-HoneyBadger/Time_Warp/releases/latest/download/TimewarpIDE-Setup.msi"
$output = "$env:TEMP\TimewarpIDE-Setup.msi"
Invoke-WebRequest -Uri $url -OutFile $output

# Silent installation
Start-Process msiexec -ArgumentList "/i `"$output`" /quiet /norestart" -Wait

# Verify installation
Get-ItemProperty HKLM:\Software\Microsoft\Windows\CurrentVersion\Uninstall\* | Where-Object {$_.DisplayName -eq "Time Warp IDE"}
```

#### **Method 2: PowerShell Deployment Script**
```powershell  
# TimewarpDeploy.ps1 - Automated deployment script
param(
    [string[]]$ComputerNames = @("localhost"),
    [string]$Version = "latest",
    [switch]$CreateShortcuts = $true
)

foreach ($computer in $ComputerNames) {
    Write-Host "Installing Time Warp IDE on $computer..." -ForegroundColor Green
    
    Invoke-Command -ComputerName $computer -ScriptBlock {
        # Download installer
        $url = "https://releases.timewarp.edu/TimewarpIDE-Setup.msi"
        $installer = "$env:TEMP\TimewarpIDE-Setup.msi"
        (New-Object Net.WebClient).DownloadFile($url, $installer)
        
        # Install with logging
        $logFile = "$env:TEMP\TimewarpIDE-Install.log"
        Start-Process msiexec -ArgumentList "/i `"$installer`" /quiet /L*v `"$logFile`"" -Wait
        
        # Create desktop shortcuts if requested
        if ($using:CreateShortcuts) {
            $shell = New-Object -ComObject WScript.Shell
            $shortcut = $shell.CreateShortcut("$env:PUBLIC\Desktop\Time Warp IDE.lnk")
            $shortcut.TargetPath = "${env:ProgramFiles}\Time Warp IDE\TimewarpIDE.exe"
            $shortcut.Save()
        }
        
        Write-Host "Installation completed on $computer" -ForegroundColor Green
    }
}
```

#### **Method 3: Group Policy Software Installation**
```powershell
# Configure Group Policy for automatic deployment
# 1. Copy MSI to network share accessible by all computers
$networkPath = "\\domain.local\software\TimewarpIDE-Setup.msi"
Copy-Item "TimewarpIDE-Setup.msi" -Destination $networkPath

# 2. Create GPO for software installation
New-GPO -Name "Deploy Time Warp IDE" | New-GPLink -Target "OU=StudentComputers,DC=domain,DC=local"

# 3. Configure software installation policy
# Use Group Policy Management Console (GPMC) to assign MSI package
```

### **🏫 Active Directory Integration**

#### **User and Computer Configuration**
```powershell
# Create security groups for Time Warp access
New-ADGroup -Name "TimewarpIDE-Users" -GroupScope Global -Path "OU=Educational,DC=school,DC=edu"
New-ADGroup -Name "TimewarpIDE-Admins" -GroupScope Global -Path "OU=Staff,DC=school,DC=edu"

# Add users to appropriate groups
Add-ADGroupMember -Identity "TimewarpIDE-Users" -Members "StudentOU"
Add-ADGroupMember -Identity "TimewarpIDE-Admins" -Members "TeacherOU"

# Configure folder redirection for student projects
$gpoName = "Timewarp User Settings"
New-GPO -Name $gpoName
Set-GPRegistryValue -Name $gpoName -Key "HKCU\Software\Timewarp\Settings" -ValueName "ProjectsFolder" -Value "%USERPROFILE%\Documents\TimewarpProjects"
```

#### **Network Drive Configuration**
```powershell
# Map shared drive for student projects
$scriptBlock = @'
if (Test-Path "\\server\StudentProjects\%USERNAME%") {
    New-PSDrive -Name "H" -PSProvider FileSystem -Root "\\server\StudentProjects\%USERNAME%" -Persist
    [Environment]::SetEnvironmentVariable("TIMEWARP_PROJECTS", "H:\", "User")
}
'@

# Add to logon script via Group Policy
Set-GPRegistryValue -Name "Student Logon Settings" -Key "HKCU\Software\Microsoft\Windows\CurrentVersion\Run" -ValueName "MapTimewarpDrive" -Value "powershell.exe -WindowStyle Hidden -Command `"$scriptBlock`""
```

### **🔒 Security and Compliance**

#### **Application Control Policies**
```powershell
# Configure Windows Defender Application Control
$policyXml = @"
<?xml version="1.0" encoding="utf-8"?>
<SiPolicy xmlns="urn:schemas-microsoft-com:sipolicy">
  <Rules>
    <Rule>
      <Option>Enabled:Unsigned System Integrity Policy</Option>
    </Rule>
  </Rules>
  <FileRules>
    <Allow ID="ID_ALLOW_TIMEWARP" FriendlyName="Time Warp IDE" 
           FileName="TimewarpIDE.exe" 
           InternalName="TimewarpIDE" 
           ProductName="Time Warp IDE" 
           MinimumFileVersion="3.0.0.0" />
  </FileRules>
</SiPolicy>
"@

$policyXml | Out-File -FilePath "TimewarpPolicy.xml"
ConvertFrom-CIPolicy -XmlFilePath "TimewarpPolicy.xml" -BinaryFilePath "TimewarpPolicy.bin"
```

#### **Network Security Configuration**
```powershell
# Configure Windows Firewall rules for Time Warp
New-NetFirewallRule -DisplayName "Time Warp IDE Inbound" -Direction Inbound -Protocol TCP -LocalPort 8080 -Action Allow -Group "Educational Software"
New-NetFirewallRule -DisplayName "Time Warp IDE Outbound" -Direction Outbound -Protocol TCP -RemotePort 80,443 -Action Allow -Group "Educational Software"

# Block unnecessary network access
New-NetFirewallRule -DisplayName "Block Timewarp External" -Direction Outbound -Program "${env:ProgramFiles}\Time Warp IDE\TimewarpIDE.exe" -Action Block -RemoteAddress Internet
```

### **📊 Monitoring and Management**

#### **Usage Monitoring with Event Logs**
```powershell
# Create custom event log for Time Warp usage
New-EventLog -LogName "TimewarpIDE" -Source "Application"

# Monitor usage events
$filter = @{
    LogName = 'TimewarpIDE'
    StartTime = (Get-Date).AddDays(-7)
}
Get-WinEvent -FilterHashtable $filter | Format-Table TimeCreated, Id, LevelDisplayName, Message -AutoSize
```

#### **Performance Monitoring**
```powershell
# Monitor Time Warp performance across network
$computers = Get-ADComputer -Filter {OperatingSystem -like "*Windows*"} | Select-Object -ExpandProperty Name

Invoke-Command -ComputerName $computers -ScriptBlock {
    Get-Process -Name "TimewarpIDE" -ErrorAction SilentlyContinue | 
    Select-Object ProcessName, CPU, WorkingSet, @{Name="Computer";Expression={$env:COMPUTERNAME}}
} | Sort-Object WorkingSet -Descending | Format-Table -AutoSize
```

### **❌ Troubleshooting Windows Version**

#### **MSI Installation Issues**
```powershell
# Check MSI installation logs
$logPath = "$env:TEMP\TimewarpIDE-Install.log"
if (Test-Path $logPath) {
    Select-String "Error|Failed|Exception" $logPath | Format-List
}

# Repair installation
$productCode = Get-WmiObject -Class Win32_Product | Where-Object {$_.Name -eq "Time Warp IDE"} | Select-Object -ExpandProperty IdentifyingNumber
Start-Process msiexec -ArgumentList "/fa $productCode" -Wait

# Complete reinstall
msiexec /x $productCode /quiet
msiexec /i "TimewarpIDE-Setup.msi" /quiet /L*v "$env:TEMP\TimewarpIDE-Reinstall.log"
```

#### **Group Policy Problems**
```powershell
# Force Group Policy update
gpupdate /force

# Check Group Policy application
gpresult /h GPReport.html /f
Invoke-Item GPReport.html

# Verify software installation policy
Get-GPOReport -Name "Deploy Time Warp IDE" -ReportType Html -Path "GPOReport.html"
```

#### **Network Drive Issues**
```powershell
# Test network connectivity
Test-NetConnection -ComputerName "fileserver.school.edu" -Port 445

# Check SMB shares
Get-SmbShare -CimSession "fileserver.school.edu"

# Reset drive mappings
Get-PSDrive -PSProvider FileSystem | Where-Object {$_.Name -eq "H"} | Remove-PSDrive -Force
New-PSDrive -Name "H" -PSProvider FileSystem -Root "\\server\StudentProjects\$env:USERNAME" -Persist
```

---

## 💾 DOS Implementation - Retro Computing

### **🎯 Perfect For**
- Computer history and evolution education
- Vintage hardware preservation projects
- Constraint-based programming challenges  
- Authentic retro computing experiences

### **⚙️ System Requirements**

#### **Minimum Hardware** *(Original DOS Era)*
- **CPU**: Intel 8086 (16-bit) or compatible
- **RAM**: 640KB conventional memory
- **Storage**: 360KB floppy disk or 10MB hard drive
- **Display**: CGA, EGA, VGA, or compatible text-mode display
- **DOS**: MS-DOS 3.3+ or compatible (PC-DOS, FreeDOS, etc.)

#### **Recommended Modern Setup**
- **Emulator**: DOSBox 0.74+ or 86Box for authentic experience
- **Host System**: Any modern computer (Windows, Mac, Linux)
- **Storage**: 50MB for DOS environment + Time Warp + examples
- **Display**: Any modern monitor (emulator handles scaling)

### **🚀 Installation Methods**

#### **Method 1: DOSBox (Modern Computers)**
```bash
# Install DOSBox on your modern system

# Windows
choco install dosbox
# or download from: https://www.dosbox.com/

# macOS
brew install dosbox
# or use: brew install --cask dosbox

# Ubuntu/Debian
sudo apt install dosbox

# Fedora/CentOS
sudo dnf install dosbox

# Arch Linux  
sudo pacman -S dosbox
```

```dosini
# Create dosbox.conf configuration
[autoexec]
# Mount Time Warp directory
mount c: /path/to/Time_Warp_DOS
c:
cd \
# Auto-launch Time Warp
timewarp.exe
```

#### **Method 2: Physical DOS Machine**
```bash
# Copy files to DOS-formatted floppy disk
# From Linux/Windows with DOS utilities:
mcopy timewarp.exe a:
mcopy examples\*.spt a:

# Or create bootable DOS disk with FreeDOS
dd if=freedos.img of=/dev/sdb bs=1M
mount /dev/sdb1 /mnt/dos
cp Time_Warp_DOS/* /mnt/dos/
umount /mnt/dos
```

#### **Method 3: Build from Source (DOS Development)**
```bash
# Using OpenWatcom on Windows
set WATCOM=C:\WATCOM
set PATH=%WATCOM%\BINNT;%PATH%
cd Time_Warp_DOS\build
wcl386 -fe=timewarp.exe ..\src\timewarp.c

# Using DJGPP in DOS/DOSBox
cd Time_Warp_DOS\build  
gcc -o timewarp.exe ..\src\timewarp.c
```

### **🏛️ Educational Setup for Computer History**

#### **Authentic DOS Environment**
```dosini
# dosbox.conf for historical accuracy
[sdl]
fullscreen=true
fulldouble=false
fullresolution=640x480

[render]  
aspect=true
scaler=normal2x

[cpu]
core=normal
cputype=286
cycles=2000

[dos]
xms=false
ems=false
umb=false

[autoexec]
@echo off
echo Welcome to 1985 Computing!
echo Today you'll learn programming like it was 40 years ago
mount c: /home/class/dos_lab
c:
```

#### **Classroom Exercise Structure**
```batch
REM setup_class.bat - Prepare student environment
@echo off
cls
echo ==========================================
echo    TIME WARP DOS - COMPUTER HISTORY LAB
echo ==========================================
echo.
echo Today we're using:
echo - MS-DOS 6.22 (Released 1994)  
echo - 640KB RAM (Same as original IBM PC)
echo - Text-mode only (No mouse, no graphics)
echo.
echo Commands you need to know:
echo   dir        - List files
echo   cd dirname - Change directory  
echo   type file  - View text file
echo   timewarp   - Start Time Warp IDE
echo.
pause
```

### **🎓 Constraint-Based Learning Exercises**

#### **Memory Management Lessons**
```c
// Demonstrate DOS memory constraints
#include <dos.h>
#include <stdio.h>

void show_memory_info() {
    union REGS regs;
    
    // Get conventional memory
    regs.h.ah = 0x48;
    regs.x.bx = 0xFFFF;  // Request impossible amount
    int86(0x21, &regs, &regs);
    
    printf("Available conventional memory: %u KB\n", 
           regs.x.bx * 16 / 1024);
}
```

#### **Text-Mode Graphics Programming**
```basic
REM Demonstrate character-based "graphics"
10 CLS
20 FOR Y = 1 TO 25
30 FOR X = 1 TO 80
40 IF X = 1 OR X = 80 OR Y = 1 OR Y = 25 THEN LOCATE Y, X: PRINT "*"
50 NEXT X
60 NEXT Y
70 LOCATE 12, 40: PRINT "DOS Graphics!"
```

### **⚙️ Advanced DOS Features**

#### **Batch File Automation**
```batch
REM timewarp.bat - Enhanced launcher with error checking
@echo off
cls

REM Check if Time Warp exists
if not exist timewarp.exe goto notfound

REM Check for examples directory  
if not exist examples\nul mkdir examples

REM Launch with error handling
echo Starting Time Warp IDE...
timewarp.exe %1
if errorlevel 1 goto error

echo Time Warp exited normally
goto end

:notfound
echo ERROR: timewarp.exe not found!
echo Please ensure Time Warp is properly installed.
goto end

:error  
echo ERROR: Time Warp encountered a problem (Exit code %errorlevel%)
echo Check your program for syntax errors.

:end
pause
```

#### **Memory-Resident Programming**
```c
// TSR (Terminate and Stay Resident) helper for Time Warp
#include <dos.h>
#include <stdio.h>

void interrupt (*old_handler)();

void interrupt new_handler() {
    // Handle special key combinations for Time Warp
    // Alt+T = Quick launch Time Warp
    if (peek(0x40, 0x17) & 0x08) {  // Alt key pressed
        if (peek(0x40, 0x1A) == 0x14) {  // 'T' key
            system("timewarp.exe");
        }
    }
    old_handler();  // Chain to original handler
}

int main() {
    old_handler = getvect(0x09);  // Keyboard interrupt
    setvect(0x09, new_handler);
    keep(0, 1024);  // Stay resident with 1KB memory
    return 0;
}
```

### **❌ Troubleshooting DOS Version**

#### **DOSBox Configuration Issues**
```dosini
# Common DOSBox fixes

[dosbox]
memsize=16          # Increase if programs need more memory

[cpu]  
cycles=10000        # Increase for faster execution
                    # Decrease if audio stutters

[mixer]
rate=22050          # Lower sample rate for compatibility
prebuffer=100       # Increase if audio skips
```

#### **File Transfer Problems**
```bash
# Mount host directory in DOSBox
mount d: /path/to/host/files -t floppy

# Copy files between host and DOS
copy c:\timewarp.exe d:\backup\
copy d:\newprog.spt c:\examples\

# Convert line endings for DOS
unix2dos file.txt  # Linux/Mac to DOS
dos2unix file.txt  # DOS to Linux/Mac
```

#### **Compilation Errors**
```bash
# Check compiler paths
echo %WATCOM%
echo %PATH%

# Verify DOS extender
dos4gw.exe
# Should display version info if properly installed

# Debug linker issues
wcl386 -v -fe=debug.exe timewarp.c
# Verbose output shows linking process
```

---

## 🍎 Apple Implementation - iOS & macOS

### **🎯 Perfect For**
- iPad-first classroom environments
- Apple School Manager deployments
- Creative coding with Apple Pencil integration
- Universal app experiences across Apple devices

### **⚙️ System Requirements**

#### **macOS Requirements**
- **macOS**: 12.0 Monterey or later (13.0+ recommended)
- **RAM**: 4GB available (8GB+ for multiple instances)
- **Storage**: 1GB for app + examples + user projects
- **Display**: 1440×900 minimum (Retina displays recommended)
- **Processor**: Intel or Apple Silicon (Universal Binary)

#### **iOS/iPadOS Requirements**
- **iOS**: 15.0 or later (16.0+ recommended for latest features)
- **iPadOS**: 15.0+ (17.0+ for enhanced multitasking)
- **RAM**: 3GB+ available (iPad Pro recommended for complex projects)
- **Storage**: 500MB for app installation + user project space
- **Accessories**: Apple Pencil compatible for drawing features

### **🚀 Installation Methods**

#### **Method 1: App Store (End Users)**
```bash
# Search for "Time Warp IDE" in App Store
# Or use direct link (when available):
# https://apps.apple.com/app/time-warp-ide/id123456789

# Command line installation (macOS)
mas install 123456789  # Requires mas-cli: brew install mas
```

#### **Method 2: Apple School Manager (Education)**
```json
{
  "app_assignment": {
    "app_id": "123456789",
    "bundle_id": "org.honey-badger.timewarp",
    "assignment_type": "device_based",
    "management": {
      "remove_app_when_mdm_removed": false,
      "prevent_backup": false,
      "prevent_data_backup": true
    },
    "deployment_target": {
      "groups": ["grade_6_ipads", "computer_lab_macs"]
    }
  }
}
```

#### **Method 3: TestFlight Beta**
```bash
# Join beta program (requires invitation)
# Install TestFlight app from App Store
# Use invitation link provided by school IT administrator
# https://testflight.apple.com/join/ABC123XYZ
```

#### **Method 4: Xcode Development** 
```bash
# Build from source (requires Apple Developer account)
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp/Time_Warp_Apple
open TimeWarp.xcworkspace

# Configure signing in Xcode
# Build and run on device or simulator
```

### **📱 iPad Classroom Configuration**

#### **Shared iPad Setup**
```json
{
  "shared_ipad": {
    "maximum_users_per_device": 25,
    "quota_per_user_gb": 2,
    "user_session_timeout_minutes": 30,
    "apps": {
      "time_warp_ide": {
        "auto_launch_on_login": false,
        "data_separation": true,
        "managed_settings": {
          "project_auto_save": true,
          "cloud_sync_enabled": false,
          "examples_read_only": true
        }
      }
    }
  }
}
```

#### **Apple Classroom Integration**
```swift
// Enable Classroom app monitoring
import ClassKit

class TimewarpActivity: CLSActivity {
    override init() {
        super.init()
        
        self.title = "Programming with TempleCode"
        self.type = .handout
        self.topic = CLSTopic.computerScienceAndEngineering
        
        // Define progress tracking
        self.progressReportingCapabilities = [.duration, .binary, .score]
    }
    
    func reportProgress(programLines: Int, errors: Int) {
        let context = CLSContext(type: .handout, identifier: "templecode-lesson", title: self.title)
        context.displayOrder = 1
        
        let activity = CLSActivity()
        activity.progress = Double(programLines) / 100.0  // Normalize to 0-1
        
        context.becomeActive()
        // Progress automatically synced to teacher's Classroom app
    }
}
```

### **✏️ Apple Pencil Integration**

#### **Handwriting to Code Conversion**
```swift
import PencilKit
import Vision

class CodeScribbleRecognizer {
    func convertHandwritingToCode(_ drawing: PKDrawing) {
        let image = drawing.image(from: drawing.bounds, scale: 1.0)
        
        let request = VNRecognizeTextRequest { (request, error) in
            guard let observations = request.results as? [VNRecognizedTextObservation] else { return }
            
            let recognizedStrings = observations.compactMap { observation in
                observation.topCandidates(1).first?.string
            }
            
            // Convert natural language to TempleCode
            let code = self.naturalLanguageToTempleCode(recognizedStrings)
            self.insertCodeIntoEditor(code)
        }
        
        request.recognitionLevel = .accurate
        request.usesLanguageCorrection = true
        
        let handler = VNImageRequestHandler(cgImage: image.cgImage!)
        try? handler.perform([request])
    }
    
    func naturalLanguageToTempleCode(_ text: [String]) -> String {
        // Convert phrases like "draw a square" to actual TempleCode
        var code = ""
        
        for phrase in text {
            if phrase.lowercased().contains("square") {
                code += """
                REPEAT 4 [
                    FORWARD 50
                    RIGHT 90
                ]
                """
            } else if phrase.lowercased().contains("circle") {
                code += """
                REPEAT 360 [
                    FORWARD 1
                    RIGHT 1
                ]
                """
            }
            // Add more pattern recognition
        }
        
        return code
    }
}
```

#### **Touch and Gesture Controls**
```swift
// iPad-optimized code editor with touch gestures
class TouchCodeEditor: UITextView {
    
    override func awakeFromNib() {
        super.awakeFromNib()
        setupTouchGestures()
    }
    
    func setupTouchGestures() {
        // Two-finger swipe to indent/unindent
        let swipeRight = UISwipeGestureRecognizer(target: self, action: #selector(indentCode))
        swipeRight.numberOfTouchesRequired = 2
        swipeRight.direction = .right
        addGestureRecognizer(swipeRight)
        
        let swipeLeft = UISwipeGestureRecognizer(target: self, action: #selector(unindentCode))  
        swipeLeft.numberOfTouchesRequired = 2
        swipeLeft.direction = .left
        addGestureRecognizer(swipeLeft)
        
        // Three-finger tap for code completion
        let threeFingerTap = UITapGestureRecognizer(target: self, action: #selector(showCodeCompletion))
        threeFingerTap.numberOfTouchesRequired = 3
        addGestureRecognizer(threeFingerTap)
    }
    
    @objc func indentCode() {
        // Indent selected lines
        guard let selectedRange = selectedTextRange else { return }
        let selectedText = text(in: selectedRange) ?? ""
        let indentedText = selectedText.components(separatedBy: .newlines)
            .map { "    " + $0 }
            .joined(separator: "\n")
        replace(selectedRange, withText: indentedText)
    }
}
```

### **☁️ iCloud and Continuity Features**

#### **Document-Based App with iCloud**
```swift
import SwiftUI

@main
struct TimewarpApp: App {
    var body: some Scene {
        DocumentGroup(newDocument: TimewarpDocument()) { file in
            ContentView(document: file.$document)
        }
        .commands {
            // Add custom menu items for macOS
            CommandGroup(replacing: .help) {
                Button("Time Warp Help") {
                    showHelp()
                }
                .keyboardShortcut("?", modifiers: .command)
            }
        }
    }
}

class TimewarpDocument: ReferenceFileDocument {
    static var readableContentTypes: [UTType] { [.templecode] }
    static var writableContentTypes: [UTType] { [.templecode] }
    
    @Published var code: String = ""
    
    // Automatic iCloud sync through NSDocument
    func snapshot(contentType: UTType) throws -> Data {
        code.data(using: .utf8) ?? Data()
    }
    
    func fileWrapper(snapshot: Data, configuration: WriteConfiguration) throws -> FileWrapper {
        FileWrapper(regularFileWithContents: snapshot)
    }
}
```

#### **Handoff Between Devices**
```swift
// Enable Handoff to continue work on different Apple devices
class HandoffManager: NSObject, NSUserActivityDelegate {
    
    func createUserActivity(for code: String, line: Int) -> NSUserActivity {
        let activity = NSUserActivity(activityType: "org.honey-badger.timewarp.editing")
        activity.title = "Programming in Time Warp"
        activity.userInfo = [
            "code": code,
            "current_line": line,
            "timestamp": Date()
        ]
        activity.isEligibleForHandoff = true
        activity.isEligibleForSearch = true
        return activity
    }
    
    func handleHandoff(_ userActivity: NSUserActivity) {
        guard let userInfo = userActivity.userInfo,
              let code = userInfo["code"] as? String,
              let line = userInfo["current_line"] as? Int else { return }
        
        // Restore editing session on new device
        loadCode(code)
        scrollToLine(line)
        showContinuityNotification()
    }
}
```

### **🏫 Educational Features**

#### **Schoolwork Integration**
```swift
import ClassKit

class SchoolworkReporting {
    func setupTimewarpActivities() {
        // Define learning activities for teachers to assign
        let basicProgramming = CLSActivity()
        basicProgramming.title = "Basic Programming Concepts"
        basicProgramming.type = .lesson
        
        let turtleGraphics = CLSActivity()
        turtleGraphics.title = "Turtle Graphics Art"
        turtleGraphics.type = .assignment
        
        // Teachers can assign these through Schoolwork app
        // Students see assignments in their Schoolwork app
        // Progress automatically reported to teacher dashboard
    }
    
    func reportAssignmentCompletion(assignmentId: String, score: Double) {
        let context = CLSContext(type: .assignment, 
                               identifier: assignmentId, 
                               title: "TempleCode Programming")
        
        let activity = CLSActivity()
        activity.progress = score
        activity.addProgressRange(from: 0.0, to: 1.0)
        
        context.becomeActive()
        
        // Score appears in teacher's grade book automatically
    }
}
```

#### **Accessibility Support**
```swift
// Full VoiceOver and accessibility support
class AccessibleCodeEditor: UITextView {
    
    override func awakeFromNib() {
        super.awakeFromNib()
        setupAccessibility()
    }
    
    func setupAccessibility() {
        // VoiceOver support for code editing
        accessibilityLabel = "Code Editor"
        accessibilityHint = "Double tap to edit your program"
        accessibilityTraits = .allowsDirectInteraction
        
        // Custom VoiceOver actions
        accessibilityCustomActions = [
            UIAccessibilityCustomAction(name: "Run Program") { _ in
                self.runProgram()
                return true
            },
            UIAccessibilityCustomAction(name: "Check Syntax") { _ in
                self.checkSyntax()
                return true
            }
        ]
    }
    
    // Announce code structure for screen readers
    func announceCodeStructure() {
        let lines = text.components(separatedBy: .newlines)
        let structure = analyzeCodeStructure(lines)
        
        UIAccessibility.post(notification: .announcement, 
                           argument: "Your program has \(structure.loops) loops and \(structure.conditionals) if statements")
    }
}
```

### **❌ Troubleshooting Apple Version**

#### **App Store and TestFlight Issues**
```bash
# Check app installation status
xcrun simctl list devices available
xcrun simctl install booted /path/to/TimeWarp.app

# Debug TestFlight installation
# In Settings > General > VPN & Device Management
# Verify developer profile is trusted

# Check app permissions
# Settings > Privacy & Security > Time Warp IDE
# Ensure all required permissions are granted
```

#### **iCloud Sync Problems**
```swift
// Debug iCloud document sync
import CloudKit

func checkiCloudStatus() {
    CKContainer.default().accountStatus { status, error in
        switch status {
        case .available:
            print("iCloud available")
        case .noAccount:
            print("No iCloud account signed in")
        case .restricted:
            print("iCloud restricted (parental controls)")
        case .couldNotDetermine:
            print("iCloud status unknown")
        @unknown default:
            print("Unknown iCloud status")
        }
    }
}

// Force document sync
func forceiCloudSync() {
    FileManager.default.startDownloadingUbiquitousItem(at: documentURL)
    
    // Monitor sync progress
    var isDownloaded = false
    try? (documentURL as NSURL).getResourceValue(&isDownloaded, 
                                               forKey: .ubiquitousItemDownloadingStatusKey)
    print("Document synced: \(isDownloaded)")
}
```

#### **Apple Pencil Recognition Problems**
```swift
// Diagnose Apple Pencil issues
import PencilKit

func checkApplePencilSupport() {
    if PKToolPicker.shared(for: self).isVisible {
        print("Apple Pencil tools available")
    }
    
    // Check Scribble support
    if UIScribbleInteraction.isPencilInputExpected {
        print("Scribble handwriting supported")
    }
    
    // Monitor pencil interactions
    let scribbleInteraction = UIScribbleInteraction(delegate: self)
    view.addInteraction(scribbleInteraction)
}

// Handle handwriting conversion errors
extension ViewController: UIScribbleInteractionDelegate {
    func scribbleInteraction(_ interaction: UIScribbleInteraction, 
                           shouldBeginAt location: CGPoint) -> Bool {
        // Only allow scribble in code editor area
        return codeEditorView.frame.contains(location)
    }
}
```

---

## 🎯 Conclusion

This comprehensive installation guide provides detailed procedures for deploying Time Warp IDE across all platforms and educational environments. From instant web access to enterprise Windows rollouts, each implementation is designed to meet specific educational needs while maintaining the unified TempleCode learning experience.

### **📊 Installation Summary**

| Implementation | Setup Time | Difficulty | Network Requirement | Best Use Case |
|---------------|------------|------------|-------------------|---------------|
| **🌐 Web** | Instant | ⭐ | Initial only | Quick access, Chromebooks |
| **🐍 Python** | 2-5 min | ⭐⭐ | Yes | Universal compatibility |
| **🚀 Rust** | 5-15 min | ⭐⭐⭐ | Build only | High performance |
| **🪟 Windows** | 10-30 min | ⭐⭐⭐ | Yes | Enterprise deployment |
| **💾 DOS** | 5-10 min | ⭐⭐⭐⭐ | No | Computer history |
| **🍎 Apple** | 5-10 min | ⭐⭐ | App Store | iPad classrooms |

### **🔗 Next Steps**

- **[📖 User Guide](USER_GUIDE.md)** - Learn to use Time Warp after installation
- **[🍎 Teacher Guide](TEACHER_GUIDE.md)** - Classroom deployment strategies  
- **[📚 Student Lesson Book](STUDENT_LESSON_BOOK.md)** - Start learning programming
- **[⚙️ Technical Reference](TECHNICAL_REFERENCE.md)** - Advanced configuration and development

---

<div align="center">

**🚀 Time Warp IDE Installation Guide**

*Education-first deployment across all platforms*

✅ **Zero to Programming in Minutes** • 🏫 **Classroom Ready** • 🔧 **IT Friendly**

</div>