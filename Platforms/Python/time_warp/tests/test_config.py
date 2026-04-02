"""Tests for time_warp.core.config module."""

from pathlib import Path

from time_warp.core.config import (
    APP_DATA_DIR,
    ASSETS_DIR,
    BUNDLES_DIR,
    CONFIG_JSON,
    DATABASES_DIR,
    EXAMPLES_DIR,
    GALLERY_DIR,
    HARDWARE_LOGS_DIR,
    HISTORY_DIR,
    LOGS_DIR,
    PROGRESS_JSON,
    PROJECT_ROOT,
    QSETTINGS_APP,
    QSETTINGS_ORG,
    THEMES_DIR,
)


def test_app_data_dir():
    """Test APP_DATA_DIR is a Path under home."""
    assert isinstance(APP_DATA_DIR, Path)
    assert str(APP_DATA_DIR).endswith(".time_warp")
    assert APP_DATA_DIR.parent == Path.home()


def test_project_root():
    """Test PROJECT_ROOT resolves to repo root."""
    assert isinstance(PROJECT_ROOT, Path)
    assert PROJECT_ROOT.is_dir()
    # Should contain Examples/ and Platforms/
    assert (PROJECT_ROOT / "Examples").is_dir()
    assert (PROJECT_ROOT / "Platforms").is_dir()


def test_examples_dir():
    """Test EXAMPLES_DIR points to Examples."""
    assert isinstance(EXAMPLES_DIR, Path)
    assert EXAMPLES_DIR == PROJECT_ROOT / "Examples"
    assert EXAMPLES_DIR.is_dir()


def test_subdirectories():
    """Test all subdirectories are Paths under APP_DATA_DIR."""
    subdirs = [
        LOGS_DIR,
        DATABASES_DIR,
        GALLERY_DIR,
        HISTORY_DIR,
        BUNDLES_DIR,
        THEMES_DIR,
        ASSETS_DIR,
        HARDWARE_LOGS_DIR,
    ]
    for subdir in subdirs:
        assert isinstance(subdir, Path)
        assert subdir.parent == APP_DATA_DIR


def test_well_known_files():
    """Test config files are Paths under APP_DATA_DIR."""
    files = [CONFIG_JSON, PROGRESS_JSON]
    for file_path in files:
        assert isinstance(file_path, Path)
        assert file_path.parent == APP_DATA_DIR


def test_qsettings_identifiers():
    """Test QSettings constants are strings."""
    assert isinstance(QSETTINGS_ORG, str)
    assert isinstance(QSETTINGS_APP, str)
    assert QSETTINGS_ORG == "TimeWarp"
    assert QSETTINGS_APP == "IDE"
