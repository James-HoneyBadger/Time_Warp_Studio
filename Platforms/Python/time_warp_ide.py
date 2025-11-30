#!/usr/bin/env python3
"""Time Warp IDE - Entry point for desktop application."""

import sys
from pathlib import Path

from PySide6.QtCore import Qt
from PySide6.QtWidgets import QApplication

from time_warp.ui import MainWindow


def main():
    """Launch Time Warp IDE."""
    # Enable high DPI scaling
    QApplication.setHighDpiScaleFactorRoundingPolicy(
        Qt.HighDpiScaleFactorRoundingPolicy.PassThrough
    )

    # Create application
    app = QApplication(sys.argv)
    app.setApplicationName("Time Warp IDE")
    app.setApplicationDisplayName("Time Warp IDE")
    app.setOrganizationName("TimeWarp")
    app.setOrganizationDomain("timewarp.edu")

    # Create main window
    window = MainWindow()
    window.show()

    # If a file was passed as argument, open it
    if len(sys.argv) > 1:
        file_path = Path(sys.argv[1])
        if file_path.exists() and file_path.is_file():
            window.load_file(str(file_path))

    # Run application
    sys.exit(app.exec())


if __name__ == "__main__":
    main()
