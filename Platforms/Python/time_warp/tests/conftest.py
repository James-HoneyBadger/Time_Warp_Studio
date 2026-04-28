"""
Shared pytest fixtures for Time Warp Studio test suite.
"""
import os
from unittest.mock import patch

import pytest

# ---------------------------------------------------------------------------
# Headless Qt support
# ---------------------------------------------------------------------------
# Set offscreen platform before any Qt import so tests can run in CI without
# a real display.  A real DISPLAY or WAYLAND_DISPLAY takes priority.
_has_display = bool(
    os.environ.get("DISPLAY") or os.environ.get("WAYLAND_DISPLAY")
)
if not _has_display:
    os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")


@pytest.fixture(scope="session")
def qapp():
    """Session-scoped QApplication for Qt widget tests."""
    from PySide6.QtWidgets import QApplication  # type: ignore
    app = QApplication.instance() or QApplication([])
    yield app



@pytest.fixture
def theme_manager():
    """ThemeManager with Qt font enumeration patched out."""
    from time_warp.ui.themes import ThemeManager  # type: ignore  # pylint: disable=import-error  # noqa: PLC0415
    with patch.object(
        ThemeManager,
        "_get_monospace_fonts",
        return_value=["Monospace", "Courier New"],
    ):
        yield ThemeManager()
