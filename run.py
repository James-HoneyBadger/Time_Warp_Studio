#!/usr/bin/env python3
"""
Time Warp Studio Launcher
========================

Intelligent launcher that:
1. Checks for virtual environment (.venv)
2. Creates one if needed
3. Installs/upgrades dependencies
4. Executes Time Warp Studio IDE

Usage:
    python run.py              # Run normally
    python run.py --fresh      # Force fresh venv
    python run.py --skip-setup # Skip dependency check
    python run.py --help       # Show help
"""

import sys
import subprocess
import argparse
import os
from pathlib import Path

# Configuration
PROJECT_ROOT = Path(__file__).parent.absolute()
VENV_DIR = PROJECT_ROOT / ".venv"
PYTHON_EXECUTABLE = sys.executable
IDE_SCRIPT = PROJECT_ROOT / "Platforms" / "Python" / "time_warp_ide.py"
REQUIREMENTS_FILE = PROJECT_ROOT / "Platforms" / "Python" / "requirements.txt"


# ANSI Colors
class Colors:
    RESET = "\033[0m"
    BOLD = "\033[1m"
    DIM = "\033[2m"
    GREEN = "\033[92m"
    BLUE = "\033[94m"
    YELLOW = "\033[93m"
    RED = "\033[91m"


def print_header(text: str) -> None:
    """Print formatted header"""
    print(f"\n{Colors.BOLD}{Colors.BLUE}{'=' * 60}{Colors.RESET}")
    print(f"{Colors.BOLD}{Colors.BLUE}{text}{Colors.RESET}")
    print(f"{Colors.BOLD}{Colors.BLUE}{'=' * 60}{Colors.RESET}\n")


def print_success(text: str) -> None:
    """Print success message"""
    print(f"{Colors.GREEN}✅ {text}{Colors.RESET}")


def print_info(text: str) -> None:
    """Print info message"""
    print(f"{Colors.BLUE}ℹ️  {text}{Colors.RESET}")


def print_warning(text: str) -> None:
    """Print warning message"""
    print(f"{Colors.YELLOW}⚠️  {text}{Colors.RESET}")


def print_error(text: str) -> None:
    """Print error message"""
    print(f"{Colors.RED}❌ {text}{Colors.RESET}")


def check_python_version() -> bool:
    """Check if Python version is 3.10+"""
    if sys.version_info < (3, 10):
        print_error(
            f"Python 3.10+ required (you have {sys.version_info.major}.{sys.version_info.minor})"
        )
        return False
    print_success(
        f"Python {sys.version_info.major}.{sys.version_info.minor}.{sys.version_info.micro}"
    )
    return True


def create_venv() -> bool:
    """Create virtual environment"""
    print_info(f"Creating virtual environment: {VENV_DIR}")
    try:
        subprocess.run(
            [PYTHON_EXECUTABLE, "-m", "venv", str(VENV_DIR)],
            check=True,
            capture_output=True,
            timeout=60,
        )
        print_success("Virtual environment created")
        return True
    except subprocess.CalledProcessError as e:
        print_error(f"Failed to create venv: {e.stderr.decode()}")
        return False
    except subprocess.TimeoutExpired:
        print_error("Virtual environment creation timed out")
        return False


def get_venv_python() -> Path:
    """Get path to Python executable in venv"""
    if sys.platform == "win32":
        return VENV_DIR / "Scripts" / "python.exe"
    return VENV_DIR / "bin" / "python"


def upgrade_pip(venv_python: Path) -> bool:
    """Upgrade pip in virtual environment"""
    print_info("Upgrading pip...")
    try:
        subprocess.run(
            [
                str(venv_python),
                "-m",
                "pip",
                "install",
                "--upgrade",
                "pip",
                "setuptools",
                "wheel",
            ],
            check=True,
            capture_output=True,
            timeout=120,
        )
        print_success("pip upgraded")
        return True
    except subprocess.CalledProcessError:
        print_warning("pip upgrade had issues, continuing anyway...")
        return True
    except subprocess.TimeoutExpired:
        print_warning("pip upgrade timed out, continuing anyway...")
        return True


def install_dependencies(venv_python: Path) -> bool:
    """Install project dependencies"""
    if not REQUIREMENTS_FILE.exists():
        print_warning(f"Requirements file not found: {REQUIREMENTS_FILE}")
        return False

    print_info(f"Installing dependencies from {REQUIREMENTS_FILE.name}...")
    try:
        subprocess.run(
            [str(venv_python), "-m", "pip", "install", "-r", str(REQUIREMENTS_FILE)],
            check=True,
            timeout=300,
        )
        print_success("Dependencies installed")
        return True
    except subprocess.CalledProcessError as e:
        print_error(f"Failed to install dependencies: {e}")
        return False
    except subprocess.TimeoutExpired:
        print_error("Dependency installation timed out")
        return False


def verify_ide_script() -> bool:
    """Verify IDE script exists"""
    if not IDE_SCRIPT.exists():
        print_error(f"IDE script not found: {IDE_SCRIPT}")
        return False
    print_success(f"IDE script found: {IDE_SCRIPT.name}")
    return True


def run_ide(venv_python: Path) -> bool:
    """Execute the IDE"""
    print_header("🚀 Launching Time Warp Studio")

    if not IDE_SCRIPT.exists():
        print_error(f"IDE script not found: {IDE_SCRIPT}")
        return False

    env = os.environ.copy()
    existing_rules = env.get("QT_LOGGING_RULES", "").strip()
    required_rules = {
        "qt.svg.warning": "false",
        "qt.qpa.theme.gnome.warning": "false",
    }

    parsed_rules = {}
    if existing_rules:
        for part in existing_rules.split(";"):
            item = part.strip()
            if not item or "=" not in item:
                continue
            key, value = item.split("=", 1)
            parsed_rules[key.strip()] = value.strip()

    for key, value in required_rules.items():
        parsed_rules.setdefault(key, value)

    env["QT_LOGGING_RULES"] = ";".join(
        f"{key}={value}" for key, value in parsed_rules.items()
    )

    # Prefer the native platform plugin; fall back to xcb if Wayland is
    # unavailable so the launcher doesn't silently get SIGKILL'd by the
    # compositor when there is no active Wayland session.
    if "QT_QPA_PLATFORM" not in env:
        if env.get("WAYLAND_DISPLAY"):
            env["QT_QPA_PLATFORM"] = "wayland;xcb"
        elif env.get("DISPLAY"):
            env["QT_QPA_PLATFORM"] = "xcb"

    try:
        # Run IDE in venv — don't use check=True so we can inspect the
        # returncode and signal ourselves and give a useful message.
        result = subprocess.run(
            [str(venv_python), str(IDE_SCRIPT)],
            cwd=str(PROJECT_ROOT),
            env=env,
        )
        rc = result.returncode
        if rc == 0:
            return True
        # Negative returncode means the process was killed by a signal
        if rc < 0:
            import signal as _signal

            try:
                sig = _signal.Signals(-rc)
                if sig == _signal.SIGKILL:
                    print_error(
                        "IDE was force-killed (SIGKILL). "
                        "This is usually caused by the system OOM killer "
                        "running out of memory, or the Wayland/X compositor "
                        "terminating an unresponsive client. "
                        "Try closing other applications and running again."
                    )
                elif sig == _signal.SIGSEGV:
                    print_error(
                        "IDE crashed with a segmentation fault (SIGSEGV). "
                        "This may be a PySide6/Qt driver issue. "
                        "Try: QT_QPA_PLATFORM=xcb ./run.sh"
                    )
                elif sig == _signal.SIGILL:
                    print_error(
                        "IDE crashed due to an illegal CPU instruction (SIGILL). "
                        "PySide6 6.x requires SSSE3/SSE4.1/SSE4.2/POPCNT. "
                        "Your CPU or VM may not support these instructions."
                    )
                else:
                    print_error(f"IDE terminated by signal {sig.name} ({-rc})")
            except ValueError:
                print_error(f"IDE terminated by unknown signal {-rc}")
            return False
        print_error(f"IDE exited with code {rc}")
        return False
    except KeyboardInterrupt:
        print_info("\nIDE closed by user")
        return True
    except Exception as e:  # noqa: BLE001  # pylint: disable=broad-exception-caught
        print_error(f"Failed to run IDE: {e}")
        return False


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description="Time Warp Studio Launcher",
        epilog="https://github.com/James-HoneyBadger/Time_Warp_Studio",
    )
    parser.add_argument(
        "--fresh", action="store_true", help="Delete and recreate virtual environment"
    )
    parser.add_argument(
        "--skip-setup",
        action="store_true",
        help="Skip dependency installation (assume venv is ready)",
    )
    parser.add_argument(
        "--no-venv",
        action="store_true",
        help="Run with system Python (not recommended)",
    )

    args = parser.parse_args()

    print_header("⏰ Time Warp Studio Launcher")

    # Step 1: Check Python version
    print_info("Checking Python version...")
    if not check_python_version():
        print_error("Please install Python 3.10 or higher")
        sys.exit(1)

    # Step 2: Handle virtual environment
    venv_python = get_venv_python()

    if args.no_venv:
        print_warning("Running with system Python (not recommended)")
        venv_python = Path(PYTHON_EXECUTABLE)
    else:
        # Delete venv if --fresh requested
        if args.fresh and VENV_DIR.exists():
            print_info("Removing existing virtual environment...")
            subprocess.run(["rm", "-rf", str(VENV_DIR)], check=True)
            print_success("Virtual environment removed")

        # Create venv if needed
        if not VENV_DIR.exists():
            print_info("Virtual environment not found")
            if not create_venv():
                sys.exit(1)
        else:
            print_success(f"Virtual environment found: {VENV_DIR.name}/")

    # Step 3: Install dependencies
    if not args.skip_setup:
        print_header("📦 Setting up dependencies")
        if not upgrade_pip(venv_python):
            print_warning("pip upgrade failed, continuing...")

        if not install_dependencies(venv_python):
            print_error("Failed to install dependencies")
            sys.exit(1)
    else:
        print_info("Skipping dependency check (--skip-setup)")

    # Step 4: Verify IDE script exists
    print_header("🔍 Verifying installation")
    if not verify_ide_script():
        sys.exit(1)

    # Step 5: Run IDE
    if not run_ide(venv_python):
        sys.exit(1)

    print_header("👋 Thank you for using Time Warp Studio!")


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print(f"\n{Colors.YELLOW}Launcher interrupted by user{Colors.RESET}")
        sys.exit(0)
    except Exception as e:  # noqa: BLE001  # pylint: disable=broad-exception-caught
        print_error(f"Unexpected error: {e}")
        sys.exit(1)
