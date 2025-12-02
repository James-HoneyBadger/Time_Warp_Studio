"""
Pytest configuration for Time Warp IDE test suite.

Ensures the time_warp package is importable by adding the Platforms/Python
directory to sys.path before test collection.
"""

import os
import sys


def pytest_configure():
    """Add project paths for import resolution."""
    tests_dir = os.path.dirname(__file__)
    repo_root = os.path.abspath(os.path.join(tests_dir, "..", "..", ".."))
    platforms_python = os.path.abspath(os.path.join(tests_dir, ".."))
    if repo_root not in sys.path:
        sys.path.insert(0, repo_root)
    if platforms_python not in sys.path:
        sys.path.insert(0, platforms_python)
