# Installation Guide — Time Warp Studio

This guide covers every way to install or run Time Warp Studio, from a
one-command standalone installer to Docker containers and developer setups.

---

## Table of Contents

1. [Standalone Install (Recommended)](#1-standalone-install-recommended)
2. [System Requirements](#2-system-requirements)
3. [Developer / Portable Install](#3-developer--portable-install)
4. [Docker Install](#4-docker-install)
5. [Troubleshooting](#5-troubleshooting)
6. [Updating & Uninstalling](#6-updating--uninstalling)
7. [Quick Reference](#7-quick-reference)

---

## 1. Standalone Install (Recommended)

`install.sh` in the project root turns Time Warp Studio into a **fully
self-contained application** on Linux. After running it you can launch the
IDE from a terminal **or** from **Applications ▸ Development ▸ Time Warp
Studio** — no virtual environment activation, no `cd`, just run it.

The installer:
- Copies all application files to a dedicated directory
- Creates an isolated Python virtual environment with all dependencies
- Writes a shell launcher (`time-warp-studio`) onto your `PATH`
- Registers a `.desktop` file so the IDE appears in your applications menu
- Re-running the installer **upgrades / overwrites** the existing version

### 1.1 Prerequisites

| Requirement | Minimum | Check |
|---|---|---|
| Linux distro | Any modern distro with a desktop environment | — |
| Python | 3.10+ | `python3 --version` |
| pip | any | `python3 -m pip --version` |
| venv | any | `python3 -m venv --help` |

Install missing prerequisites:

```bash
# Fedora / RHEL / CentOS
sudo dnf install python3 python3-pip

# Ubuntu / Debian
sudo apt update && sudo apt install python3 python3-pip python3-venv
```

### 1.2 System-wide install (all users, recommended)

Installs to `/opt/time-warp-studio` and creates `/usr/local/bin/time-warp-studio`.
Requires sudo / root.

```bash
# Clone (or download a release archive) and enter the directory
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio

# Run the installer as root
sudo ./install.sh
```

When it finishes you will see:

```
════════════════════════════════════════════════════
  Time Warp Studio 10.1.0 — Installed!
════════════════════════════════════════════════════

  Start from terminal:   time-warp-studio
  Start from menu:       Applications ▸ Development ▸ Time Warp Studio
  Installed to:          /opt/time-warp-studio
```

Launch it immediately:

```bash
time-warp-studio
```

Or open your desktop's application launcher and look under **Development**.

### 1.3 Per-user install (current user only, no sudo)

Installs to `~/.local/share/time-warp-studio` and creates
`~/.local/bin/time-warp-studio`.

```bash
cd Time_Warp_Studio
./install.sh --user
```

> **PATH note**: If `~/.local/bin` is not yet on your `PATH` the installer
> will warn you.  Add this line to `~/.bashrc` (or `~/.profile`):
>
> ```bash
> export PATH="$HOME/.local/bin:$PATH"
> ```
>
> Then reload:  `source ~/.bashrc`

### 1.4 Upgrading an existing installation

Re-run the installer with `--upgrade` to wipe and recreate the Python venv
(ensures you get the latest dependency versions).  Without `--upgrade` the
installer still syncs all application files but reuses the existing venv.

```bash
# Upgrade a system-wide install
sudo ./install.sh --upgrade

# Upgrade a per-user install
./install.sh --user --upgrade
```

Without the `--upgrade` flag (incremental sync, faster):

```bash
sudo ./install.sh          # system
./install.sh --user        # per-user
```

### 1.5 What the installer creates

```
System-wide                        Per-user
─────────────────────────────────  ─────────────────────────────────────────
/opt/time-warp-studio/             ~/.local/share/time-warp-studio/
  Platforms/Python/…               (same layout)
  Examples/…
  .venv/                           # isolated Python environment
  install.sh                       # re-runnable for upgrades

/usr/local/bin/time-warp-studio    ~/.local/bin/time-warp-studio
  (shell launcher script)          (shell launcher script)

/usr/share/applications/           ~/.local/share/applications/
  time-warp-studio.desktop         time-warp-studio.desktop

/usr/share/icons/hicolor/          ~/.local/share/icons/hicolor/
  256x256/apps/                    256x256/apps/
  time-warp-studio.png             time-warp-studio.png
```

### 1.6 install.sh full option reference

```
Usage: ./install.sh [OPTIONS]

Options:
  (none)         System-wide install into /opt  (requires sudo)
  --user         Per-user install into ~/.local  (no sudo needed)
  --upgrade      Wipe and recreate the Python venv, reinstall all deps
  --uninstall    Remove Time Warp Studio completely (see §6)
  --help         Show help and exit

Examples:
  sudo ./install.sh                   # fresh system install
  sudo ./install.sh --upgrade         # upgrade system install
  ./install.sh --user                 # fresh per-user install
  ./install.sh --user --upgrade       # upgrade per-user install
  sudo ./install.sh --uninstall       # remove system install
  ./install.sh --user --uninstall     # remove per-user install
```

---

## 2. System Requirements

### Minimum

| Item | Requirement |
|---|---|
| OS | Linux (Ubuntu 20.04+, Fedora 33+, Arch, or any modern distro) |
| Python | 3.10 or newer |
| RAM | 2 GB (4 GB recommended) |
| Disk | 600 MB free |
| CPU | 64-bit with SSE3 / SSE4.1 / SSE4.2 / POPCNT support |
| Display | Any X11 or Wayland desktop |

> **Virtual machine note**: QEMU/KVM using TCG (software emulation) may
> lack the SSE4.2 instruction needed by PySide6.  Enable KVM acceleration or
> pass `+sse4.2` to the QEMU CPU flags.

### Check CPU features

```bash
grep -E 'ssse3|sse4_1|sse4_2|popcnt' /proc/cpuinfo | head -1
```

All four flags must be present.

---

## 3. Developer / Portable Install

Use this if you want to run directly from the source tree (no system install).

```bash
# 1. Clone
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio

# 2. Create a virtual environment
python3 -m venv venv
source venv/bin/activate        # Windows: venv\Scripts\activate

# 3. Install dependencies
pip install --upgrade pip
pip install "PySide6>=6.5.0" "Pillow>=10.0.0"

# 4. Launch
python Platforms/Python/time_warp_ide.py
```

Or use the smart launcher which handles the venv automatically:

```bash
python run.py
```

For contributors, install the full development stack:

```bash
pip install pytest pytest-cov pytest-mock ruff black
python Platforms/Python/test_runner.py --comprehensive
```

---

## 4. Docker Install

Runs the IDE in a container — best compatibility for unusual hardware.

### Prerequisites

- Docker installed and running
- An X11 display (Linux desktop or XQuartz on macOS)

### Build and run

```bash
# Build (takes a few minutes first time)
docker build -f docker/Dockerfile.main -t time-warp-studio .

# Run on Linux (shares your X display)
xhost +local:docker
docker run -it --rm \
  -e DISPLAY="$DISPLAY" \
  -v /tmp/.X11-unix:/tmp/.X11-unix \
  -v "$(pwd)/Examples":/app/Examples \
  time-warp-studio

# Run on macOS (requires XQuartz)
docker run -it --rm \
  -e DISPLAY=host.docker.internal:0 \
  -v "$(pwd)/Examples":/app/Examples \
  time-warp-studio
```

---

## 5. Troubleshooting

### IDE doesn't appear in the Applications menu

```bash
# Restart the desktop database
update-desktop-database ~/.local/share/applications   # per-user install
sudo update-desktop-database /usr/share/applications  # system install

# Or log out and log back in
```

### `time-warp-studio: command not found`

- **System install**: `/usr/local/bin` should already be on `PATH`.
  Check with `echo $PATH`.
- **Per-user install**: Add `~/.local/bin` to your PATH:
  ```bash
  echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
  source ~/.bashrc
  ```

### "Illegal instruction" crash

Your CPU (or VM) is missing SSE4.2.  See §2 for the check command.

### `qt.qpa.plugin: Could not load "xcb"`

Missing X11 runtime libraries:

```bash
# Ubuntu / Debian
sudo apt install libxcb-cursor0 libxkbcommon-x11-0 libxcb-icccm4 \
                 libxcb-keysyms1 libxcb-render-util0

# Fedora
sudo dnf install xcb-util-cursor libxkbcommon-x11 xcb-util-wm \
                 xcb-util-keysyms xcb-util-renderutil
```

### PySide6 fails to install into the venv

```bash
# Force-reinstall with a clean venv
sudo ./install.sh --upgrade        # system install
./install.sh --user --upgrade      # per-user install
```

### Dependencies out of date after a new release

```bash
sudo ./install.sh --upgrade        # system
./install.sh --user --upgrade      # per-user
```

---

## 6. Updating & Uninstalling

### Update to a new version

Pull the latest source, then re-run the installer:

```bash
cd Time_Warp_Studio
git pull

sudo ./install.sh --upgrade        # system install
# or
./install.sh --user --upgrade      # per-user install
```

The `--upgrade` flag forces a complete dependency reinstall.
Without it the sync is faster (application files only; venv kept as-is).

### Uninstall completely

```bash
sudo ./install.sh --uninstall      # removes system install
./install.sh --user --uninstall    # removes per-user install
```

This removes:
- The application directory (`/opt/time-warp-studio` or `~/.local/share/time-warp-studio`)
- The shell launcher
- The `.desktop` menu entry
- The icon

User settings stored in `~/.time_warp/` are **not** removed.
Delete them manually if desired:

```bash
rm -rf ~/.time_warp
```

---

## 7. Quick Reference

### Launching

| Method | Command |
|---|---|
| Terminal (system or per-user install) | `time-warp-studio` |
| Desktop menu | Applications ▸ Development ▸ Time Warp Studio |
| From source tree | `python Platforms/Python/time_warp_ide.py` |
| Smart launcher (handles venv) | `python run.py` |

### Installer commands

| Task | Command |
|---|---|
| System install | `sudo ./install.sh` |
| Per-user install | `./install.sh --user` |
| System upgrade | `sudo ./install.sh --upgrade` |
| Per-user upgrade | `./install.sh --user --upgrade` |
| System uninstall | `sudo ./install.sh --uninstall` |
| Per-user uninstall | `./install.sh --user --uninstall` |

### File locations after install

| Item | System install | Per-user install |
|---|---|---|
| Application | `/opt/time-warp-studio/` | `~/.local/share/time-warp-studio/` |
| Launcher | `/usr/local/bin/time-warp-studio` | `~/.local/bin/time-warp-studio` |
| Desktop entry | `/usr/share/applications/time-warp-studio.desktop` | `~/.local/share/applications/time-warp-studio.desktop` |
| Icon | `/usr/share/icons/hicolor/256x256/apps/time-warp-studio.png` | `~/.local/share/icons/hicolor/256x256/apps/time-warp-studio.png` |
| User config | `~/.time_warp/` | `~/.time_warp/` |

### Test the installation

```bash
# Verify the IDE imports are working
/opt/time-warp-studio/.venv/bin/python -c \
  "from time_warp.core.interpreter import Interpreter; print('✅ OK')"

# Run the full demo test suite (from the source directory)
python tests/test_all_demos.py
```

---

*For more information see [USER_GUIDE.md](USER_GUIDE.md),
[LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md), and the guides in [docs/guides/](guides/).*


---
