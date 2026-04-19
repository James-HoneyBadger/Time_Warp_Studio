#!/usr/bin/env python3
"""
Test Runner for Time Warp Studio
Orchestrates test execution and reporting globally.
"""

import argparse
import subprocess
import sys
from pathlib import Path

UNIT_TEST_TARGETS = ["time_warp/tests"]
INTEGRATION_TEST_TARGETS = ["../../tests/test_basic_functionality.py"]
DEMO_TEST_TARGETS = ["../../tests/test_all_demos.py"]


def resolve_test_targets(args) -> list[str]:
    """Resolve the pytest targets for the selected run mode."""
    targets: list[str] = []

    targets.extend(UNIT_TEST_TARGETS)

    if args.comprehensive or args.all or args.integration:
        targets.extend(INTEGRATION_TEST_TARGETS)

    if args.comprehensive or args.all or args.demos:
        targets.extend(DEMO_TEST_TARGETS)

    # Deduplicate while preserving order.
    return list(dict.fromkeys(targets))


def build_pytest_command(args, targets: list[str]) -> list[str]:
    """Build the pytest command for the selected options."""
    cmd = [sys.executable, "-m", "pytest"]

    if args.comprehensive:
        cmd.extend(
            [
                "--cov=time_warp",
                "--cov-report=html:test_reports/html",
                "--cov-report=term",
                "--cov-report=xml:test_reports/coverage.xml",
                "-v",
            ]
        )
    elif args.basic and not (args.integration or args.demos or args.all):
        cmd.extend(["-m", "not slow and not integration and not performance"])

    if args.parallel:
        cmd.extend(["-n", "auto"])

    cmd.extend(targets)
    return cmd


def main():
    parser = argparse.ArgumentParser(description="Time Warp Studio Test Runner")
    parser.add_argument("--basic", action="store_true", help="Run fast smoke tests")
    parser.add_argument(
        "--comprehensive", action="store_true", help="Run full suite with coverage"
    )
    parser.add_argument(
        "--parallel",
        action="store_true",
        help="Run tests in parallel using pytest-xdist",
    )
    parser.add_argument("--unit", action="store_true", help="Run unit tests")
    parser.add_argument(
        "--integration", action="store_true", help="Include root integration tests"
    )
    parser.add_argument(
        "--demos", action="store_true", help="Include demo-program validation tests"
    )
    parser.add_argument(
        "--all", action="store_true", help="Run unit, integration, and demo tests"
    )

    args = parser.parse_args()

    # Ensure we are in the correct directory
    script_dir = Path(__file__).parent.resolve()
    if Path.cwd() != script_dir:
        print(f"ℹ️  Changing working directory to {script_dir}")
        import os

        os.chdir(script_dir)

    targets = resolve_test_targets(args)
    cmd = build_pytest_command(args, targets)

    if args.comprehensive:
        Path("test_reports").mkdir(exist_ok=True)
        print("📊 Running comprehensive tests with coverage...")
    elif args.all:
        print("🧪 Running complete validation suite...")
    elif args.integration or args.demos:
        print("🔎 Running unit tests with extended validation...")
    else:
        print("🚀 Running basic smoke tests...")

    if args.parallel:
        print("⚡ Running tests in parallel mode...")

    print(f"ℹ️  Targets: {', '.join(targets)}")

    result = subprocess.run(cmd)

    if result.returncode == 0:
        print("\n✅ All tests passed successfully!")
    else:
        print(f"\n❌ Tests failed with exit code {result.returncode}")
        sys.exit(result.returncode)


if __name__ == "__main__":
    main()
