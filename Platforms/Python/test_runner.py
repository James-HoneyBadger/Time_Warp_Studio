#!/usr/bin/env python3
"""
Test Runner for Time Warp Studio
Orchestrates test execution and reporting globally.
"""

import argparse
import subprocess
import sys
from pathlib import Path


def main():
    parser = argparse.ArgumentParser(description="Time Warp Studio Test Runner")
    parser.add_argument("--basic", action="store_true", help="Run fast smoke tests")
    parser.add_argument(
        "--comprehensive", action="store_true", help="Run full suite with coverage"
    )
    args = parser.parse_args()

    # Ensure we are in the correct directory
    script_dir = Path(__file__).parent.resolve()
    if Path.cwd() != script_dir:
        print(f"ℹ️  Changing working directory to {script_dir}")
        import os

        os.chdir(script_dir)

    # Base pytest command
    cmd = [sys.executable, "-m", "pytest"]

    # Target directories
    # running validation on time_warp package tests and root level test scripts
    targets = ["time_warp/tests", "test_arithmetic.py"]

    if args.comprehensive:
        # Add coverage options
        cmd.extend(
            [
                "--cov=time_warp",
                "--cov-report=html:test_reports/html",
                "--cov-report=term",
                "-v",
            ]
        )

        # Ensure report dir exists
        Path("test_reports").mkdir(exist_ok=True)

        print("📊 Running comprehensive tests with coverage...")
    else:
        # Basic mode
        print("🚀 Running basic smoke tests...")
        # Could limit to specific markers if we had them, e.g. -m "not slow"

    cmd.extend(targets)

    result = subprocess.run(cmd)

    if result.returncode == 0:
        print("\n✅ All tests passed successfully!")
    else:
        print(f"\n❌ Tests failed with exit code {result.returncode}")
        sys.exit(result.returncode)


if __name__ == "__main__":
    main()
