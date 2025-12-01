"""Pytest conftest to ensure `time_warp` package is importable.

This adjusts sys.path at test time so that the `Platforms/Python` directory
is included. Some static analyzers and test runners don't include the
project path by default which causes `Unable to import 'time_warp.*'`
errors during linting/testing.
"""

import os
import sys


def pytest_configure():
    """Add the `Platforms/Python` path to sys.path for tests."""
    tests_dir = os.path.dirname(__file__)
    # Add the repo root (two levels up) so imports like
    # `Platforms.Python.time_warp` are importable for tests that use the
    # repository's multi-directory layout.
    repo_root = os.path.abspath(os.path.join(tests_dir, "..", "..", ".."))
    platforms_python = os.path.abspath(os.path.join(tests_dir, ".."))
    if repo_root not in sys.path:
        sys.path.insert(0, repo_root)
    if platforms_python not in sys.path:
        sys.path.insert(0, platforms_python)
