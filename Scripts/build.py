#!/usr/bin/env python3
# pylint: disable=too-few-public-methods
"""
Build automation utility for Time Warp IDE.

Provides convenient commands for building all platform variants:
- Python (cross-platform)
- Web (browser-based)
- Windows 2000 (retro edition)
"""

import argparse
import subprocess
import sys
from pathlib import Path
from typing import List, Optional
import shutil


class Colors:
    """ANSI color codes for terminal output."""

    BLUE = "\033[0;34m"
    GREEN = "\033[0;32m"
    RED = "\033[0;31m"
    YELLOW = "\033[1;33m"
    NC = "\033[0m"  # No Color


def info(msg: str) -> None:
    """Print info message."""
    print(f"{Colors.BLUE}ℹ️  {msg}{Colors.NC}")


def success(msg: str) -> None:
    """Print success message."""
    print(f"{Colors.GREEN}✅ {msg}{Colors.NC}")


def error(msg: str) -> None:
    """Print error message."""
    print(f"{Colors.RED}❌ {msg}{Colors.NC}")


def warning(msg: str) -> None:
    """Print warning message."""
    print(f"{Colors.YELLOW}⚠️  {msg}{Colors.NC}")


def run_command(cmd: List[str], cwd: Optional[Path] = None, check: bool = True) -> int:
    """Run a shell command and return exit code."""
    info(f"Running: {' '.join(cmd)}")
    result = subprocess.run(cmd, cwd=cwd, check=False)
    if check and result.returncode != 0:
        error(f"Command failed with exit code {result.returncode}")
    return result.returncode


def build_win2000(_args: argparse.Namespace) -> int:
    """Build Windows 2000 edition."""
    info("Building Windows 2000 edition...")

    root = Path(__file__).parent.parent
    script = root / "scripts" / "build_win2000.sh"

    if not script.exists():
        error("Win2000 build script not found")
        return 1

    return run_command(["bash", str(script)])


def test_python(args: argparse.Namespace) -> int:
    """Run Python tests."""
    info("Running Python tests...")

    root = Path(__file__).parent.parent

    cmd = ["pytest"]
    if args.verbose:
        cmd.append("-v")
    if args.coverage:
        cmd.extend(["--cov", "--cov-report=term-missing"])

    return run_command(cmd, cwd=root)


def format_code(args: argparse.Namespace) -> int:
    """Format Python code with black."""
    info("Formatting Python code...")

    root = Path(__file__).parent.parent

    cmd = ["black", "."]
    if args.check:
        cmd.append("--check")

    return run_command(cmd, cwd=root)


def lint_code(_args: argparse.Namespace) -> int:
    """Lint Python code with flake8."""
    info("Linting Python code...")

    root = Path(__file__).parent.parent

    return run_command(["flake8", "."], cwd=root)


def type_check(_args: argparse.Namespace) -> int:
    """Type check Python code with mypy."""
    info("Type checking Python code...")

    root = Path(__file__).parent.parent

    return run_command(["mypy", "platforms/python"], cwd=root)


def clean(_args: argparse.Namespace) -> int:
    """Clean build artifacts."""
    info("Cleaning build artifacts...")

    root = Path(__file__).parent.parent

    # Python artifacts
    patterns = [
        "**/__pycache__",
        "**/*.pyc",
        "**/*.pyo",
        "**/*.egg-info",
        ".pytest_cache",
        ".coverage",
        "htmlcov",
        "dist",
        ".mypy_cache",
    ]

    for pattern in patterns:
        for path in root.rglob(pattern):
            if path.is_dir():
                info(f"Removing directory: {path}")
                shutil.rmtree(path)
            else:
                info(f"Removing file: {path}")
                path.unlink()

    # Win2000 artifacts
    win2000_dist = root / "dist" / "win2000"
    if win2000_dist.exists():
        info("Cleaning Win2000 build artifacts...")
        run_command(["make", "clean-win2000"], cwd=root)

    success("Clean complete")
    return 0


def main() -> int:
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Time Warp IDE build automation utility"
    )
    subparsers = parser.add_subparsers(dest="command", help="Command to run")

    # Build commands
    build_parser = subparsers.add_parser("build", help="Build platforms")
    build_parser.add_argument(
        "platform", choices=["win2000", "all"], help="Platform to build"
    )

    # Test commands
    test_parser = subparsers.add_parser("test", help="Run tests")
    test_parser.add_argument(
        "-v", "--verbose", action="store_true", help="Verbose output"
    )
    test_parser.add_argument(
        "--coverage", action="store_true", help="Generate coverage report"
    )

    # Code quality commands
    format_parser = subparsers.add_parser("format", help="Format code")
    format_parser.add_argument(
        "--check", action="store_true", help="Check format without modifying"
    )

    subparsers.add_parser("lint", help="Lint code with flake8")
    subparsers.add_parser("typecheck", help="Type check with mypy")

    # Clean command
    subparsers.add_parser("clean", help="Clean build artifacts")

    args = parser.parse_args()

    if not args.command:
        parser.print_help()
        return 1

    # Dispatch to appropriate handler
    handlers = {
        "build": lambda: build_win2000(args),
        "test": lambda: test_python(args),
        "format": lambda: format_code(args),
        "lint": lambda: lint_code(args),
        "typecheck": lambda: type_check(args),
        "clean": lambda: clean(args),
    }

    handler = handlers.get(args.command)
    if handler:
        return handler()

    error(f"Unknown command: {args.command}")
    return 1


if __name__ == "__main__":
    sys.exit(main())
