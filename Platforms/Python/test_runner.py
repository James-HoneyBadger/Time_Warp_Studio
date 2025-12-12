#!/usr/bin/env python3
"""
Test runner for Time Warp IDE (Python Port).
"""
import argparse
import os
import subprocess
import sys
import unittest


def run_tests(args):
    """Run tests based on arguments."""
    base_dir = os.path.dirname(os.path.abspath(__file__))
    test_dir = os.path.join(base_dir, "tests")

    # Ensure platforms/python is in PYTHONPATH
    sys.path.insert(0, base_dir)
    # Prepend our platform path to PYTHONPATH so tests import local package
    existing = os.environ.get("PYTHONPATH", "")
    os.environ["PYTHONPATH"] = base_dir + os.pathsep + existing

    if args.comprehensive or args.basic:
        print("Running unit tests...")

        # Run legacy test scripts
        scripts = []
        for f in os.listdir(test_dir):
            if f.startswith("test_") and f.endswith(".py"):
                scripts.append(f)
        failed = False
        for script in scripts:
            print(f"Running {script}...")
            # Use virtual environment if it exists
            python_cmd = sys.executable
            # Check for both venv and .venv directories
            venv_python = os.path.join(base_dir, ".venv", "bin", "python")
            if not os.path.exists(venv_python):
                venv_python = os.path.join(base_dir, "venv", "bin", "python")
            if os.path.exists(venv_python):
                python_cmd = venv_python

            result = subprocess.run(
                [python_cmd, os.path.join(test_dir, script)],
                env=os.environ.copy(),
                check=False,
            )
            if result.returncode != 0:
                print(f"❌ {script} failed")
                failed = True
            else:
                print(f"✅ {script} passed")

        if failed:
            sys.exit(1)

        # Discover and run standard unittest tests (if any)
        loader = unittest.TestLoader()
        suite = loader.discover(test_dir, pattern="test_*.py")
        runner = unittest.TextTestRunner(verbosity=2)
        result = runner.run(suite)

        if not result.wasSuccessful():
            sys.exit(1)

    if args.comprehensive:
        print("\nRunning integration tests (if any)...")
        # Add integration tests here if needed


def main():
    """CLI entry point for the Time Warp test runner.

    Parses command-line arguments and dispatches to run_tests().
    """
    parser = argparse.ArgumentParser(description="Time Warp Test Runner")
    parser.add_argument(
        "--basic",
        action="store_true",
        help=("Run basic unit tests"),
    )
    parser.add_argument(
        "--comprehensive",
        action="store_true",
        help="Run all tests including integration",
    )

    args = parser.parse_args()

    if not (args.basic or args.comprehensive):
        parser.print_help()
        sys.exit(1)

    run_tests(args)


if __name__ == "__main__":
    main()
