"""
Pytest configuration for Time Warp IDE test suite.

Ensures the time_warp package is importable by adding the Platforms/Python
directory to sys.path before test collection.
"""

import os
import sys
from PySide6.QtGui import QGuiApplication


def pytest_configure():
    """Add project paths for import resolution."""
    tests_dir = os.path.dirname(__file__)
    repo_root = os.path.abspath(os.path.join(tests_dir, "..", "..", ".."))
    platforms_python = os.path.abspath(os.path.join(tests_dir, ".."))
    if repo_root not in sys.path:
        sys.path.insert(0, repo_root)
    if platforms_python not in sys.path:
        sys.path.insert(0, platforms_python)


def pytest_sessionstart(session):
    """Ensure a QGuiApplication exists before tests that access QFontDatabase run.

    Some modules (themes, UI) query Qt font databases at import time. Creating
    a QGuiApplication at the start of the test session prevents import-time
    errors in headless environments when using a virtual display such as
    Xvfb or Qt's offscreen platform.
    """
    try:
        if QGuiApplication.instance() is None:
            QGuiApplication([])
    except Exception:
        # If Qt is not available or cannot be initialized, allow tests to
        # continue; GUI tests will fail appropriately.
        pass
