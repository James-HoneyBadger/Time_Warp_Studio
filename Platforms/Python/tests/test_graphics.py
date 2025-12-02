#!/usr/bin/env python3
"""
Manual turtle graphics verification script.

Launches the IDE with test programs for visual inspection of canvas rendering,
zoom/pan controls, and coordinate system orientation. Not for CI automation.
"""

import sys

import pytest
from PySide6.QtWidgets import QApplication  # pylint: disable=no-name-in-module

from time_warp.ui import MainWindow  # pylint: disable=import-error,no-name-in-module

pytestmark = pytest.mark.skip(reason="Manual graphics verification (requires display)")

TEST_PROGRAMS = [
    (
        "Square",
        (
            "FORWARD 100\nRIGHT 90\nFORWARD 100\nRIGHT 90\n"
            "FORWARD 100\nRIGHT 90\nFORWARD 100\nRIGHT 90"
        ),
    ),
    (
        "Triangle",
        ("FORWARD 150\nRIGHT 120\nFORWARD 150\nRIGHT 120\n" "FORWARD 150\nRIGHT 120"),
    ),
    ("Star", "REPEAT 5 [\n  FORWARD 200\n  RIGHT 144\n]"),
    ("Spiral", "REPEAT 36 [\n  FORWARD 100\n  RIGHT 10\n]"),
]


def test_graphics():
    """Load IDE with sample Logo programs for visual testing."""
    QApplication(sys.argv)
    window = MainWindow()

    name, program = TEST_PROGRAMS[0]
    editor = window.get_current_editor()
    if editor:
        editor.setPlainText(program)

    print(f"Loaded: {name}")
    print("Run with F5, view Graphics tab. Scroll to zoom, Ctrl+drag to pan.")

    window.show()
    window.resize(1200, 800)


if __name__ == "__main__":
    sys.exit(test_graphics())
