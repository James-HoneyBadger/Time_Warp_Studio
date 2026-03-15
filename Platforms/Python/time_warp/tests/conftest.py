"""
Shared pytest fixtures for Time Warp Studio test suite.
"""
from unittest.mock import patch

import pytest


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
