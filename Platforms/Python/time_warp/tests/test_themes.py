"""
Unit tests for ThemeManager and Theme data structures.

Verifies that all 21 themes are defined with the required fields,
that get_theme() resolves correctly, and that apply_theme() does not raise
when called without a live QApplication.
"""

# pylint: disable=import-error

from unittest.mock import MagicMock, patch

import pytest

from time_warp.ui.themes import Theme  # type: ignore[import-not-found]

# theme_manager fixture is provided by conftest.py

EXPECTED_THEME_COUNT = 21

EXPECTED_THEME_NAMES = [
    # Modern Dark (8)
    "Dracula",
    "Monokai",
    "VS Code Dark",
    "GitHub Dark",
    "Nord",
    "One Dark Pro",
    "Solarized Dark",
    "Ocean",
    # Light (4)
    "VS Code Light",
    "GitHub Light",
    "Solarized Light",
    "Spring",
    # High Contrast (2)
    "High Contrast Dark",
    "High Contrast Light",
    # Retro / CRT (7)
    "Amber Monochrome",
    "Green Monochrome",
    "IBM PC CGA",
    "Commodore 64",
    "Apple II",
    "DOS Blue",
    "ZX Spectrum",
]

REQUIRED_THEME_FIELDS = [
    "name",
    "background",
    "foreground",
    "editor_bg",
    "editor_fg",
    "keyword",
    "comment",
    "string",
    "number",
    "canvas_bg",
    "menu_bg",
    "menu_fg",
]


class TestThemeCount:
    def test_total_theme_count(self, theme_manager):
        names = theme_manager.get_theme_names()
        assert len(names) == EXPECTED_THEME_COUNT, (
            f"Expected {EXPECTED_THEME_COUNT} themes, found {len(names)}: {names}"
        )

    def test_all_expected_themes_present(self, theme_manager):
        names = theme_manager.get_theme_names()
        for expected in EXPECTED_THEME_NAMES:
            assert expected in names, f"Missing theme: '{expected}'"


class TestThemeStructure:
    def test_each_theme_is_theme_instance(self, theme_manager):
        for name, theme in theme_manager.themes.items():
            assert isinstance(theme, Theme), f"'{name}' is not a Theme instance"

    def test_required_fields_non_empty(self, theme_manager):
        for theme_name, theme in theme_manager.themes.items():
            for field in REQUIRED_THEME_FIELDS:
                value = getattr(theme, field, None)
                assert value, (
                    f"Theme '{theme_name}' has empty/missing field '{field}'"
                )

    def test_color_fields_start_with_hash(self, theme_manager):
        color_fields = [
            "background",
            "foreground",
            "editor_bg",
            "editor_fg",
            "canvas_bg",
            "keyword",
            "comment",
            "string",
            "number",
        ]
        for theme_name, theme in theme_manager.themes.items():
            for field in color_fields:
                val = getattr(theme, field, None)
                if val is not None:
                    assert val.startswith("#"), (
                        f"Theme '{theme_name}'.{field} = '{val}' is not a hex color"
                    )


class TestGetTheme:
    def test_get_existing_theme(self, theme_manager):
        theme = theme_manager.get_theme("Dracula")
        assert theme.name == "Dracula"

    def test_get_nonexistent_falls_back_to_dracula(self, theme_manager):
        theme = theme_manager.get_theme("NonExistentTheme12345")
        assert theme.name == "Dracula"

    def test_get_theme_names_returns_list(self, theme_manager):
        names = theme_manager.get_theme_names()
        assert isinstance(names, list)
        assert len(names) > 0


class TestApplyThemeNoQt:
    def test_apply_theme_with_mocked_qt(self, theme_manager):
        """apply_theme with mocked Qt classes should not raise."""
        import time_warp.ui.themes as themes_module  # type: ignore[import-not-found]
        with (
            patch.object(themes_module.QApplication, "instance", return_value=None),
            patch.object(themes_module, "QPalette", return_value=MagicMock()),
            patch.object(themes_module, "QColor", return_value=MagicMock()),
        ):
            try:
                theme_manager.apply_theme("Dracula")
            except Exception as exc:  # pylint: disable=broad-except
                pytest.fail(f"apply_theme raised unexpectedly: {exc}")

    def test_apply_all_themes_with_mocked_qt(self, theme_manager):
        """Every theme should survive apply_theme with mocked Qt."""
        import time_warp.ui.themes as themes_module  # type: ignore[import-not-found]
        with (
            patch.object(themes_module.QApplication, "instance", return_value=None),
            patch.object(themes_module, "QPalette", return_value=MagicMock()),
            patch.object(themes_module, "QColor", return_value=MagicMock()),
        ):
            for name in theme_manager.get_theme_names():
                try:
                    theme_manager.apply_theme(name)
                except Exception as exc:  # pylint: disable=broad-except
                    pytest.fail(f"apply_theme('{name}') raised: {exc}")
