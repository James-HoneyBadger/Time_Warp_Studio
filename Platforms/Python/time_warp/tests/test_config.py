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


def test_app_data_dir_name():
    """APP_DATA_DIR name is .time_warp."""
    assert APP_DATA_DIR.name == ".time_warp"


def test_logs_dir_under_app_data():
    """LOGS_DIR is a subdirectory of APP_DATA_DIR."""
    assert LOGS_DIR.parent == APP_DATA_DIR
    assert LOGS_DIR.name == "logs"


def test_databases_dir_under_app_data():
    """DATABASES_DIR is a subdirectory of APP_DATA_DIR."""
    assert DATABASES_DIR.parent == APP_DATA_DIR


def test_config_json_is_file_path():
    """CONFIG_JSON has .json suffix."""
    assert CONFIG_JSON.suffix == ".json"


def test_progress_json_is_file_path():
    """PROGRESS_JSON has .json suffix."""
    assert PROGRESS_JSON.suffix == ".json"


def test_subdir_names_are_unique():
    """All configured subdirectory names within APP_DATA_DIR are unique."""
    subdirs = [LOGS_DIR, DATABASES_DIR, GALLERY_DIR, HISTORY_DIR,
               BUNDLES_DIR, THEMES_DIR, ASSETS_DIR, HARDWARE_LOGS_DIR]
    names = [d.name for d in subdirs]
    assert len(names) == len(set(names)), "Duplicate subdirectory names found"


def test_project_root_contains_platforms():
    """PROJECT_ROOT must contain the Platforms directory."""
    assert (PROJECT_ROOT / "Platforms").is_dir()


def test_examples_dir_contains_basic():
    """EXAMPLES_DIR should have a basic/ subdirectory."""
    assert (EXAMPLES_DIR / "basic").is_dir()


def test_themes_dir_name():
    """THEMES_DIR name is 'themes'."""
    assert THEMES_DIR.name == "themes"


def test_gallery_dir_name():
    """GALLERY_DIR name is 'gallery'."""
    assert GALLERY_DIR.name == "gallery"


def test_history_dir_name():
    """HISTORY_DIR name is 'history'."""
    assert HISTORY_DIR.name == "history"


def test_assets_dir_name():
    """ASSETS_DIR name is 'assets'."""
    assert ASSETS_DIR.name == "assets"


def test_bundles_dir_name():
    """BUNDLES_DIR name is 'bundles'."""
    assert BUNDLES_DIR.name == "bundles"


def test_hardware_logs_dir_name():
    """HARDWARE_LOGS_DIR name is 'hardware_logs'."""
    assert HARDWARE_LOGS_DIR.name == "hardware_logs"


def test_config_json_file_name():
    """CONFIG_JSON filename is 'config.json'."""
    assert CONFIG_JSON.name == "config.json"


def test_progress_json_file_name():
    """PROGRESS_JSON filename is 'progress.json'."""
    assert PROGRESS_JSON.name == "progress.json"


def test_project_root_is_absolute():
    """PROJECT_ROOT is an absolute path."""
    assert PROJECT_ROOT.is_absolute()


def test_app_data_dir_is_absolute():
    """APP_DATA_DIR is an absolute path."""
    assert APP_DATA_DIR.is_absolute()


def test_examples_dir_contains_logo():
    """EXAMPLES_DIR should have a logo/ subdirectory."""
    assert (EXAMPLES_DIR / "logo").is_dir()


def test_examples_dir_contains_javascript():
    """EXAMPLES_DIR should have a javascript/ subdirectory."""
    assert (EXAMPLES_DIR / "javascript").is_dir()


def test_examples_dir_contains_erlang():
    """EXAMPLES_DIR should have an erlang/ subdirectory."""
    assert (EXAMPLES_DIR / "erlang").is_dir()


def test_examples_dir_contains_forth():
    """EXAMPLES_DIR should have a forth/ subdirectory."""
    assert (EXAMPLES_DIR / "forth").is_dir()


def test_examples_dir_contains_lua():
    """EXAMPLES_DIR should have a lua/ subdirectory."""
    assert (EXAMPLES_DIR / "lua").is_dir()


def test_examples_dir_contains_prolog():
    """EXAMPLES_DIR should have a prolog/ subdirectory."""
    assert (EXAMPLES_DIR / "prolog").is_dir()


def test_examples_dir_contains_pascal():
    """EXAMPLES_DIR should have a pascal/ subdirectory."""
    assert (EXAMPLES_DIR / "pascal").is_dir()


def test_examples_dir_contains_c():
    """EXAMPLES_DIR should have a c/ subdirectory."""
    assert (EXAMPLES_DIR / "c").is_dir()


def test_examples_dir_contains_hypertalk():
    """EXAMPLES_DIR should have a hypertalk/ subdirectory."""
    assert (EXAMPLES_DIR / "hypertalk").is_dir()


def test_examples_dir_contains_brainfuck():
    """EXAMPLES_DIR should have a brainfuck/ subdirectory."""
    assert (EXAMPLES_DIR / "brainfuck").is_dir()


def test_examples_dir_contains_pilot():
    """EXAMPLES_DIR should have a pilot/ subdirectory."""
    assert (EXAMPLES_DIR / "pilot").is_dir()


def test_examples_dir_contains_lisp():
    """EXAMPLES_DIR should have a lisp/ subdirectory."""
    assert (EXAMPLES_DIR / "lisp").is_dir()


def test_examples_dir_contains_tcl():
    """EXAMPLES_DIR should have a tcl/ subdirectory."""
    assert (EXAMPLES_DIR / "tcl").is_dir()


def test_examples_dir_contains_postscript():
    """EXAMPLES_DIR should have a postscript/ subdirectory."""
    assert (EXAMPLES_DIR / "postscript").is_dir()


def test_examples_dir_contains_cobol():
    """EXAMPLES_DIR should have a cobol/ subdirectory."""
    assert (EXAMPLES_DIR / "cobol").is_dir()


def test_all_path_constants_are_path_instances():
    """All exported path constants are Path instances."""
    paths = [APP_DATA_DIR, PROJECT_ROOT, EXAMPLES_DIR, LOGS_DIR, DATABASES_DIR,
             GALLERY_DIR, HISTORY_DIR, BUNDLES_DIR, THEMES_DIR, ASSETS_DIR,
             HARDWARE_LOGS_DIR, CONFIG_JSON, PROGRESS_JSON]
    for p in paths:
        assert isinstance(p, Path), f"{p!r} is not a Path"


def test_all_subdirs_are_under_app_data():
    """All subdirectory constants are under APP_DATA_DIR."""
    subdirs = [LOGS_DIR, DATABASES_DIR, GALLERY_DIR, HISTORY_DIR,
               BUNDLES_DIR, THEMES_DIR, ASSETS_DIR, HARDWARE_LOGS_DIR]
    for d in subdirs:
        assert str(d).startswith(str(APP_DATA_DIR)), f"{d} not under APP_DATA_DIR"


from time_warp.core.config import QSETTINGS_APP, QSETTINGS_ORG


class TestConfigConstants:
    """Additional config constant tests."""

    def test_qsettings_app_is_string(self):
        assert isinstance(QSETTINGS_APP, str)

    def test_qsettings_org_is_string(self):
        assert isinstance(QSETTINGS_ORG, str)

    def test_qsettings_app_value(self):
        assert QSETTINGS_APP == "IDE"

    def test_qsettings_org_value(self):
        assert QSETTINGS_ORG == "TimeWarp"

    def test_config_json_name(self):
        assert CONFIG_JSON.name == "config.json"

    def test_progress_json_name(self):
        assert PROGRESS_JSON.name == "progress.json"

    def test_logs_dir_name(self):
        assert LOGS_DIR.name == "logs"

    def test_databases_dir_name(self):
        assert DATABASES_DIR.name == "databases"

    def test_gallery_dir_name(self):
        assert GALLERY_DIR.name == "gallery"

    def test_history_dir_name(self):
        assert HISTORY_DIR.name == "history"

    def test_bundles_dir_name(self):
        assert BUNDLES_DIR.name == "bundles"

    def test_themes_dir_name(self):
        assert THEMES_DIR.name == "themes"

    def test_assets_dir_name(self):
        assert ASSETS_DIR.name == "assets"

    def test_hardware_logs_dir_name(self):
        assert HARDWARE_LOGS_DIR.name == "hardware_logs"

    def test_config_json_parent_is_app_data(self):
        assert CONFIG_JSON.parent == APP_DATA_DIR

    def test_progress_json_parent_is_app_data(self):
        assert PROGRESS_JSON.parent == APP_DATA_DIR

    def test_logs_dir_parent_is_app_data(self):
        assert LOGS_DIR.parent == APP_DATA_DIR

    def test_databases_dir_parent_is_app_data(self):
        assert DATABASES_DIR.parent == APP_DATA_DIR

    def test_app_data_is_absolute(self):
        assert APP_DATA_DIR.is_absolute()

    def test_project_root_is_absolute(self):
        assert PROJECT_ROOT.is_absolute()

    def test_examples_dir_is_under_project_root(self):
        assert str(EXAMPLES_DIR).startswith(str(PROJECT_ROOT))


class TestConfigExtended:
    """More config tests."""

    def test_gallery_dir_is_path(self):
        assert isinstance(GALLERY_DIR, Path)

    def test_gallery_dir_name(self):
        assert GALLERY_DIR.name == "gallery"

    def test_bundles_dir_is_path(self):
        assert isinstance(BUNDLES_DIR, Path)

    def test_history_dir_is_path(self):
        assert isinstance(HISTORY_DIR, Path)

    def test_themes_dir_is_path(self):
        assert isinstance(THEMES_DIR, Path)

    def test_examples_dir_is_path(self):
        assert isinstance(EXAMPLES_DIR, Path)

    def test_qsettings_org_is_str(self):
        assert isinstance(QSETTINGS_ORG, str)
        assert len(QSETTINGS_ORG) > 0

    def test_qsettings_app_is_str(self):
        assert isinstance(QSETTINGS_APP, str)
        assert len(QSETTINGS_APP) > 0

    def test_config_json_suffix(self):
        assert CONFIG_JSON.suffix == ".json"

    def test_progress_json_suffix(self):
        assert PROGRESS_JSON.suffix == ".json"

    def test_assets_dir_is_absolute(self):
        assert ASSETS_DIR.is_absolute()

    def test_bundles_dir_under_app_data(self):
        assert str(BUNDLES_DIR).startswith(str(APP_DATA_DIR))

    def test_gallery_dir_under_app_data(self):
        assert str(GALLERY_DIR).startswith(str(APP_DATA_DIR))

    def test_history_dir_under_app_data(self):
        assert str(HISTORY_DIR).startswith(str(APP_DATA_DIR))

    def test_themes_dir_under_app_data(self):
        assert str(THEMES_DIR).startswith(str(APP_DATA_DIR))

    def test_qsettings_org_time_warp(self):
        assert "Time" in QSETTINGS_ORG or "time" in QSETTINGS_ORG or "warp" in QSETTINGS_ORG.lower()


class TestConfigExtended2:
    """Second round of config constant tests."""

    def test_app_data_dir_is_path(self):
        from pathlib import Path
        from time_warp.core.config import APP_DATA_DIR
        assert isinstance(APP_DATA_DIR, Path)

    def test_logs_dir_is_path(self):
        from pathlib import Path
        from time_warp.core.config import LOGS_DIR
        assert isinstance(LOGS_DIR, Path)

    def test_examples_dir_is_path(self):
        from pathlib import Path
        from time_warp.core.config import EXAMPLES_DIR
        assert isinstance(EXAMPLES_DIR, Path)

    def test_config_json_is_path(self):
        from pathlib import Path
        from time_warp.core.config import CONFIG_JSON
        assert isinstance(CONFIG_JSON, Path)

    def test_bundles_dir_is_path(self):
        from pathlib import Path
        from time_warp.core.config import BUNDLES_DIR
        assert isinstance(BUNDLES_DIR, Path)

    def test_assets_dir_is_path(self):
        from pathlib import Path
        from time_warp.core.config import ASSETS_DIR
        assert isinstance(ASSETS_DIR, Path)

    def test_databases_dir_is_path(self):
        from pathlib import Path
        from time_warp.core.config import DATABASES_DIR
        assert isinstance(DATABASES_DIR, Path)

    def test_hardware_logs_dir_is_path(self):
        from pathlib import Path
        from time_warp.core.config import HARDWARE_LOGS_DIR
        assert isinstance(HARDWARE_LOGS_DIR, Path)

    def test_gallery_dir_is_path(self):
        from pathlib import Path
        from time_warp.core.config import GALLERY_DIR
        assert isinstance(GALLERY_DIR, Path)

    def test_history_dir_is_path(self):
        from pathlib import Path
        from time_warp.core.config import HISTORY_DIR
        assert isinstance(HISTORY_DIR, Path)


class TestConfigExtended3:
    """Third round of config constant tests."""

    def test_app_data_dir_exists(self):
        import time_warp.core.config as cfg
        assert hasattr(cfg, "APP_DATA_DIR")

    def test_logs_dir_is_path(self):
        from pathlib import Path
        from time_warp.core.config import LOGS_DIR
        assert isinstance(LOGS_DIR, Path)

    def test_databases_dir_is_path(self):
        from pathlib import Path
        from time_warp.core.config import DATABASES_DIR
        assert isinstance(DATABASES_DIR, Path)

    def test_config_json_is_path(self):
        from pathlib import Path
        from time_warp.core.config import CONFIG_JSON
        assert isinstance(CONFIG_JSON, Path)

    def test_bundles_dir_is_path(self):
        from pathlib import Path
        from time_warp.core.config import BUNDLES_DIR
        assert isinstance(BUNDLES_DIR, Path)

    def test_themes_dir_is_path(self):
        from pathlib import Path
        from time_warp.core.config import THEMES_DIR
        assert isinstance(THEMES_DIR, Path)

    def test_app_data_dir_is_path(self):
        from pathlib import Path
        from time_warp.core.config import APP_DATA_DIR
        assert isinstance(APP_DATA_DIR, Path)

    def test_app_data_dir_has_time_warp(self):
        from time_warp.core.config import APP_DATA_DIR
        assert "time_warp" in str(APP_DATA_DIR)

    def test_config_json_has_json_suffix(self):
        from time_warp.core.config import CONFIG_JSON
        assert str(CONFIG_JSON).endswith(".json")

    def test_logs_dir_parent_is_app_data(self):
        from time_warp.core.config import LOGS_DIR, APP_DATA_DIR
        assert LOGS_DIR.parent == APP_DATA_DIR
