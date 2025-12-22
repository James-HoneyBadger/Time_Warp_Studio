#!/usr/bin/env python3
"""Time Warp IDE - Entry point for desktop application."""

import logging
import sys
from pathlib import Path

from PySide6.QtCore import Qt
from PySide6.QtWidgets import QApplication

from time_warp.logging_config import setup_logging, get_logger
from time_warp.ui import MainWindow


def main():
    """Launch Time Warp IDE."""
    # Setup logging (INFO level for normal use, can be overridden)
    log_file = Path.home() / ".time_warp" / "logs" / "ide.log"
    setup_logging(log_level=logging.INFO, log_file=log_file)
    
    logger = get_logger(__name__)
    logger.info("Time Warp IDE starting")
    
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
            logger.info(f"Opening file: {file_path}")
            window.load_file(str(file_path))

    # Run application
    try:
        exit_code = app.exec()
        logger.info("Time Warp IDE exiting normally")
        sys.exit(exit_code)
    except Exception as e:
        logger.exception("Unexpected error in main loop")
        raise


if __name__ == "__main__":
    main()
