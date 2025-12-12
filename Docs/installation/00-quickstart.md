# Installation Quick Start

Get Time Warp IDE running on your system in minutes!

---

## Choose Your Operating System

- **[Linux](#linux-quick-install)** – Debian, Ubuntu, Arch, Fedora
- **[macOS](#macos-quick-install)** – Apple computers (Intel and Apple Silicon)
- **[Windows](#windows-quick-install)** – Windows 10/11 (7+ supported)

---

## Linux Quick Install

### Debian/Ubuntu

```bash
# Install Python 3.10+ if needed
sudo apt update
sudo apt install python3 python3-pip python3-venv

# Clone repository
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp_Studio/Platforms/Python

# Create virtual environment
python3 -m venv .venv
source .venv/bin/activate

# Install dependencies and run
pip install -r requirements.txt
python time_warp_ide.py
```

### Arch Linux

```bash
sudo pacman -S python python-pip
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp_Studio/Platforms/Python
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
python time_warp_ide.py
```

### Fedora

```bash
sudo dnf install python3 python3-pip
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp_Studio/Platforms/Python
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
python time_warp_ide.py
```

---

## macOS Quick Install

### Intel & Apple Silicon

```bash
# Verify Python 3.10+ is installed
python3 --version

# Clone repository
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp_Studio/Platforms/Python

# Create virtual environment
python3 -m venv .venv
source .venv/bin/activate

# Install dependencies and run
pip install -r requirements.txt
python3 time_warp_ide.py
```

### Using Homebrew (Optional)

```bash
# Install Python if needed
brew install python@3.10

# Then follow steps above
```

---

## Windows Quick Install

### Windows 10/11 (Recommended)

Open PowerShell or Command Prompt and run:

```powershell
# Verify Python 3.10+ is installed
python --version

# Clone repository
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp_Studio\Platforms\Python

# Create virtual environment
python -m venv .venv
.venv\Scripts\activate

# Install dependencies and run
pip install -r requirements.txt
python time_warp_ide.py
```

### Windows 7+ (Legacy)

Use the same steps above. Python 3.10+ is required.

---

## System Requirements

### Minimum Specifications

- **Operating System**
  - Linux: kernel 3.10+
  - macOS: 10.13 (High Sierra) or newer
  - Windows: Windows 7 or newer

- **Processor**: 1 GHz with SSSE3/SSE4.1 support
  - Most modern CPUs have this
  - VMs may lack these extensions

- **RAM**: 256 MB

- **Disk Space**: 50 MB free space

- **Display**: 800×600 resolution (1024×768 recommended)

### Recommended Specifications

- **Processor**: Modern multi-core CPU (2+ GHz)
- **RAM**: 512 MB or more
- **Disk**: 100 MB+ for documentation and examples
- **Display**: 1024×768 or higher
- **Internet**: For optional features (cloud, collaboration)

### Known Issues & Solutions

#### "Illegal instruction" Error

**Problem**: Your CPU lacks SSSE3 or SSE4 instructions.

**Common Cause**: Running in a virtual machine (QEMU, VirtualBox) that doesn't expose these CPU flags.

**Solution**: 
- Run on physical hardware instead
- Use a modern cloud VM (AWS EC2, Azure, Google Cloud)
- Update your VM/hypervisor settings to expose CPU flags

**Check CPU support**:
```bash
# Linux/macOS
grep -o 'ssse3\|sse4_1' /proc/cpuinfo

# Windows (PowerShell)
Get-WmiObject Win32_Processor | Select-Object Name
```

#### "No module named 'PySide6'"

**Problem**: Python dependencies not installed.

**Solution**:
```bash
# Ensure you're in the virtual environment
source .venv/bin/activate  # Linux/macOS
# or
.venv\Scripts\activate     # Windows

# Reinstall dependencies
pip install -r requirements.txt
```

#### Port Already in Use

**Problem**: IDE tries to use a port that's occupied.

**Solution**: Change the port in Settings → IDE Settings, or restart your system.

---

## Verify Installation

After installing, verify everything works:

```bash
# Navigate to repo root
cd /path/to/Time_Warp_Studio

# Run quick tests
python Tests/run_tests.py --quick
```

Expected output:
```
Running quick smoke tests...
✅ Core interpreter tests passed
✅ Language executor tests passed
✅ UI integration tests passed
=====================================
PASSED: 15 tests in 2.3s
```

---

## First Launch

When you run `python time_warp_ide.py`, you'll see:

1. **Main Window** – Code editor on left, canvas on right
2. **Menu Bar** – File, Edit, Run, Language, View, Help menus
3. **Language Selector** – Currently set to BASIC
4. **Welcome Tab** – Getting started guide

### Write Your First Program

1. **Select Language** – Click the Language menu → BASIC (or PILOT/Logo)
2. **Write Code**:
   ```basic
   PRINT "Hello, World!"
   ```
3. **Run** – Press F5 or click Run → Run Program
4. **See Output** – Results appear in the Output panel

### Try a Logo Program

Switch to Logo language and run:
```logo
FORWARD 100
RIGHT 90
FORWARD 100
RIGHT 90
FORWARD 100
RIGHT 90
FORWARD 100
```

The turtle will draw a square! Switch to Graphics mode (View menu) to see it.

---

## Next Steps

- **Read the User Manual** – `Docs/user/00-user-manual.md`
- **Try Examples** – Check `Examples/basic/`, `Examples/logo/`, etc.
- **Explore Features** – Check out the View menu for debug tools, variable inspector, etc.
- **Learn More** – Visit the documentation at `Docs/INDEX.md`

---

## Getting Help

**Q: How do I use the debug panel?**  
A: View → Debug Panel (or Debug menu) opens the execution debugger.

**Q: Can I change the theme?**  
A: Yes! View → Theme and select from 8 built-in themes (Dracula, Monokai, etc).

**Q: What screen modes are available?**  
A: View → Screen Mode has Text, Graphics, Single, and Combined modes.

**Q: How do I save my program?**  
A: File → Save (Ctrl+S) or File → Save As (Ctrl+Shift+S).

**Q: Where do I report bugs?**  
A: GitHub Issues: <https://github.com/James-HoneyBadger/Time_Warp/issues>

**Q: How do I contribute?**  
A: Read `Docs/developer/00-developer-guide.md` for development setup and guidelines.

---

**Still stuck?** Check the [FAQ](../user/03-faq.md) or open a GitHub Discussion!
