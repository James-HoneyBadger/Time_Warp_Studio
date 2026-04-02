#!/usr/bin/env python3
"""Time Warp Studio - Entry point for desktop application."""

import os
import sys
from pathlib import Path

from PySide6.QtCore import Qt
from PySide6.QtWidgets import QApplication

from time_warp.logging_config import get_logger, setup_logging
from time_warp.ui import MainWindow


def _configure_qt_logging_rules() -> None:
    """Reduce known non-fatal Qt warning noise in desktop environments."""
    suppress_rules = {
        "qt.svg.warning": "false",
        "qt.qpa.theme.gnome.warning": "false",
    }

    existing = os.environ.get("QT_LOGGING_RULES", "").strip()
    parsed_rules = {}

    if existing:
        for part in existing.split(";"):
            item = part.strip()
            if not item or "=" not in item:
                continue
            key, value = item.split("=", 1)
            parsed_rules[key.strip()] = value.strip()

    for key, value in suppress_rules.items():
        parsed_rules.setdefault(key, value)

    os.environ["QT_LOGGING_RULES"] = ";".join(
        f"{key}={value}" for key, value in parsed_rules.items()
    )


def _check_display_available() -> bool:
    """Verify a graphical display is reachable before Qt tries to open one.

    If neither DISPLAY (X11) nor WAYLAND_DISPLAY is set Qt will call
    qFatal() which ultimately sends an abort/kill signal to the process,
    producing a confusing 'died with SIGKILL' message in the launcher.
    We detect this early and print an actionable error instead.
    """
    if os.environ.get("DISPLAY") or os.environ.get("WAYLAND_DISPLAY"):
        return True
    # Qt also honours QT_QPA_PLATFORM=offscreen for headless use
    if os.environ.get("QT_QPA_PLATFORM", "").startswith("offscreen"):
        return True
    print(
        "❌ No graphical display found.\n"
        "   Set DISPLAY (X11) or WAYLAND_DISPLAY before launching the IDE.\n"
        "   If running headless, use: QT_QPA_PLATFORM=offscreen ./run.sh",
        file=sys.stderr,
    )
    return False


def main():
    """Launch Time Warp Studio."""
    _configure_qt_logging_rules()

    # Fail fast with a clear message rather than letting Qt abort the process
    if not _check_display_available():
        sys.exit(1)

    # Setup logging (INFO level for normal use, can be overridden)
    from time_warp.core.config import LOGS_DIR

    log_file = LOGS_DIR / "ide.log"
    setup_logging(level="INFO", log_file=str(log_file))

    logger = get_logger(__name__)
    logger.info("Time Warp Studio starting")

    # Enable high DPI scaling
    QApplication.setHighDpiScaleFactorRoundingPolicy(
        Qt.HighDpiScaleFactorRoundingPolicy.PassThrough
    )

    # Create application
    app = QApplication(sys.argv)
    app.setApplicationName("Time Warp Studio")
    app.setApplicationDisplayName("Time Warp Studio")
    app.setOrganizationName("TimeWarp")
    app.setOrganizationDomain("timewarp.edu")

    # Create main window
    window = MainWindow()
    window.show()

    # If a file was passed as argument, open it
    if len(sys.argv) > 1:
        file_path = Path(sys.argv[1])
        if file_path.exists() and file_path.is_file():
            logger.info("Opening file: %s", file_path)
            window.load_file(str(file_path))

    # Run application
    try:
        exit_code = app.exec()
        logger.info("Time Warp Studio exiting normally")
        sys.exit(exit_code)
    except (ValueError, TypeError):
        logger.exception("Unexpected error in main loop")
        raise


if __name__ == "__main__":
    main()
