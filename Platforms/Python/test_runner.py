#!/usr/bin/env python3
"""
Test runner for Time Warp IDE (Python Port).
"""
import sys
import os
import argparse
import unittest
import subprocess


def run_tests(args):
    """Run tests based on arguments."""
    base_dir = os.path.dirname(os.path.abspath(__file__))
    test_dir = os.path.join(base_dir, "tests")

    # Ensure platforms/python is in PYTHONPATH
    sys.path.insert(0, base_dir)
    os.environ["PYTHONPATH"] = (
        base_dir + os.pathsep + os.environ.get("PYTHONPATH", "")
    )

    if args.comprehensive or args.basic:
        print("Running unit tests...")

        # Run legacy test scripts
        scripts = [
            f for f in os.listdir(test_dir)
            if f.startswith("test_") and f.endswith(".py")
        ]
        failed = False
        for script in scripts:
            print(f"Running {script}...")
            result = subprocess.run(
                [sys.executable, os.path.join(test_dir, script)],
                env=os.environ.copy(),
                check=False
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
    parser = argparse.ArgumentParser(description="Time Warp Test Runner")
    parser.add_argument("--basic", action="store_true",
                        help="Run basic unit tests")
    parser.add_argument("--comprehensive", action="store_true",
                        help="Run all tests including integration")

    args = parser.parse_args()

    if not (args.basic or args.comprehensive):
        parser.print_help()
        sys.exit(1)

    run_tests(args)


if __name__ == "__main__":
    main()
