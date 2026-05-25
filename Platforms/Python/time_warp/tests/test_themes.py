"""
Unit tests for ThemeManager and Theme data structures.

Verifies that all 28 themes are defined with the required fields,
that get_theme() resolves correctly, and that apply_theme() does not raise
when called without a live QApplication.
"""

# pylint: disable=import-error

from unittest.mock import MagicMock, patch

import pytest

from time_warp.ui.themes import Theme  # type: ignore[import-not-found]

# theme_manager fixture is provided by conftest.py

EXPECTED_THEME_COUNT = 28

EXPECTED_THEME_NAMES = [
    # Modern Dark (10)
    "Dracula",
    "Monokai",
    "VS Code Dark",
    "GitHub Dark",
    "Nord",
    "One Dark Pro",
    "Solarized Dark",
    "Ocean",
    "Catppuccin Mocha",
    "Gruvbox Dark",
    "Tokyo Night",
    # Light (5)
    "Gruvbox Light",
    "Catppuccin Latte",
    "VS Code Light",
    "GitHub Light",
    "Solarized Light",
    "Spring",
    # High Contrast / Accessibility (3)
    "High Contrast Dark",
    "High Contrast Light",
    "Dyslexia Friendly",
    "Accessible Dark Blue",
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
        mock_app = MagicMock()
        with (
            patch.object(themes_module.QApplication, "instance", return_value=mock_app),
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
        mock_app = MagicMock()
        with (
            patch.object(themes_module.QApplication, "instance", return_value=mock_app),
            patch.object(themes_module, "QPalette", return_value=MagicMock()),
            patch.object(themes_module, "QColor", return_value=MagicMock()),
        ):
            for name in theme_manager.get_theme_names():
                try:
                    theme_manager.apply_theme(name)
                except Exception as exc:  # pylint: disable=broad-except
                    pytest.fail(f"apply_theme('{name}') raised: {exc}")


class TestThemeProperties:
    """Tests for Theme object properties."""

    def test_dracula_background(self, theme_manager):
        theme = theme_manager.get_theme("Dracula")
        assert theme.background == "#282a36"

    def test_dracula_foreground(self, theme_manager):
        theme = theme_manager.get_theme("Dracula")
        assert theme.foreground == "#f8f8f2"

    def test_dracula_name(self, theme_manager):
        theme = theme_manager.get_theme("Dracula")
        assert theme.name == "Dracula"

    def test_theme_has_editor_bg(self, theme_manager):
        theme = theme_manager.get_theme("Monokai")
        assert hasattr(theme, "editor_bg")
        assert isinstance(theme.editor_bg, str)

    def test_theme_has_keyword_color(self, theme_manager):
        theme = theme_manager.get_theme("Monokai")
        assert hasattr(theme, "keyword")
        assert isinstance(theme.keyword, str)

    def test_theme_has_comment_color(self, theme_manager):
        theme = theme_manager.get_theme("Nord")
        assert hasattr(theme, "comment")
        assert isinstance(theme.comment, str)

    def test_all_28_themes_present(self, theme_manager):
        assert len(theme_manager.get_theme_names()) == 28

    def test_retro_theme_present(self, theme_manager):
        names = theme_manager.get_theme_names()
        assert "Amber Monochrome" in names or "Green Monochrome" in names

    def test_light_themes_present(self, theme_manager):
        names = theme_manager.get_theme_names()
        light_themes = [n for n in names if "Light" in n]
        assert len(light_themes) >= 2

    def test_font_sizes_count(self, theme_manager):
        sizes = theme_manager.get_font_sizes()
        assert len(sizes) >= 10

    def test_default_font_size(self, theme_manager):
        assert theme_manager.current_font_size in theme_manager.get_font_sizes()

    def test_default_theme_name(self, theme_manager):
        assert theme_manager.current_theme_name in theme_manager.get_theme_names()

    def test_fallback_nonexistent(self, theme_manager):
        theme = theme_manager.get_theme("NonExistent12345")
        assert theme is not None
        assert isinstance(theme.name, str)


class TestThemeColors2:
    """More detailed Theme color tests."""

    def test_monokai_name(self, theme_manager):
        t = theme_manager.get_theme("Monokai")
        assert t.name == "Monokai"

    def test_nord_name(self, theme_manager):
        t = theme_manager.get_theme("Nord")
        assert t.name == "Nord"

    def test_vscode_dark_name(self, theme_manager):
        t = theme_manager.get_theme("VS Code Dark")
        assert t.name == "VS Code Dark"

    def test_vscode_light_name(self, theme_manager):
        t = theme_manager.get_theme("VS Code Light")
        assert t.name == "VS Code Light"

    def test_github_dark_name(self, theme_manager):
        t = theme_manager.get_theme("GitHub Dark")
        assert t.name == "GitHub Dark"

    def test_github_light_name(self, theme_manager):
        t = theme_manager.get_theme("GitHub Light")
        assert t.name == "GitHub Light"

    def test_gruvbox_dark_name(self, theme_manager):
        t = theme_manager.get_theme("Gruvbox Dark")
        assert t.name == "Gruvbox Dark"

    def test_gruvbox_light_name(self, theme_manager):
        t = theme_manager.get_theme("Gruvbox Light")
        assert t.name == "Gruvbox Light"

    def test_catppuccin_mocha_name(self, theme_manager):
        t = theme_manager.get_theme("Catppuccin Mocha")
        assert t.name == "Catppuccin Mocha"

    def test_catppuccin_latte_name(self, theme_manager):
        t = theme_manager.get_theme("Catppuccin Latte")
        assert t.name == "Catppuccin Latte"

    def test_solarized_dark_name(self, theme_manager):
        t = theme_manager.get_theme("Solarized Dark")
        assert t.name == "Solarized Dark"

    def test_solarized_light_name(self, theme_manager):
        t = theme_manager.get_theme("Solarized Light")
        assert t.name == "Solarized Light"

    def test_high_contrast_dark_name(self, theme_manager):
        t = theme_manager.get_theme("High Contrast Dark")
        assert t.name == "High Contrast Dark"

    def test_amber_monochrome_name(self, theme_manager):
        t = theme_manager.get_theme("Amber Monochrome")
        assert t.name == "Amber Monochrome"

    def test_green_monochrome_name(self, theme_manager):
        t = theme_manager.get_theme("Green Monochrome")
        assert t.name == "Green Monochrome"

    def test_commodore64_name(self, theme_manager):
        t = theme_manager.get_theme("Commodore 64")
        assert t.name == "Commodore 64"

    def test_apple_ii_name(self, theme_manager):
        t = theme_manager.get_theme("Apple II")
        assert t.name == "Apple II"

    def test_dos_blue_name(self, theme_manager):
        t = theme_manager.get_theme("DOS Blue")
        assert t.name == "DOS Blue"

    def test_theme_editor_bg_is_hex(self, theme_manager):
        for name in ["Dracula", "Nord", "Monokai"]:
            t = theme_manager.get_theme(name)
            assert t.editor_bg.startswith("#")

    def test_theme_canvas_bg_is_hex(self, theme_manager):
        for name in ["Dracula", "VS Code Dark", "Solarized Dark"]:
            t = theme_manager.get_theme(name)
            assert t.canvas_bg.startswith("#")

    def test_theme_menu_bg_is_hex(self, theme_manager):
        for name in ["Dracula", "Nord"]:
            t = theme_manager.get_theme(name)
            assert t.menu_bg.startswith("#")


class TestThemeAttributes:
    """Test theme attribute correctness."""

    def test_theme_has_foreground(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert t.foreground.startswith("#")

    def test_theme_has_background(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert t.background.startswith("#")

    def test_theme_has_keyword(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert t.keyword.startswith("#")

    def test_theme_has_string(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert t.string.startswith("#")

    def test_theme_has_number(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert t.number.startswith("#")

    def test_theme_has_comment(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert t.comment.startswith("#")

    def test_theme_has_function(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert t.function.startswith("#")

    def test_theme_has_operator(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert t.operator.startswith("#")

    def test_theme_has_variable(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert t.variable.startswith("#")

    def test_theme_has_selection_bg(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert t.selection_bg.startswith("#")

    def test_theme_has_line_number_bg(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert t.line_number_bg.startswith("#")

    def test_theme_has_line_number_fg(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert t.line_number_fg.startswith("#")

    def test_theme_has_error_color(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert t.error_color.startswith("#")

    def test_theme_has_success_color(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert t.success_color.startswith("#")

    def test_theme_has_warning_color(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert t.warning_color.startswith("#")

    def test_theme_has_info_color(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert t.info_color.startswith("#")

    def test_all_themes_have_editor_bg(self, theme_manager):
        for name in theme_manager.themes:
            t = theme_manager.get_theme(name)
            assert t.editor_bg.startswith("#"), f"{name}: editor_bg invalid"

    def test_all_themes_have_foreground(self, theme_manager):
        for name in theme_manager.themes:
            t = theme_manager.get_theme(name)
            assert t.foreground.startswith("#"), f"{name}: foreground invalid"

    def test_total_theme_count_28(self, theme_manager):
        assert len(theme_manager.themes) == 28

    def test_ocean_theme_exists(self, theme_manager):
        t = theme_manager.get_theme("Ocean")
        assert t.name == "Ocean"

    def test_tokyo_night_theme_exists(self, theme_manager):
        t = theme_manager.get_theme("Tokyo Night")
        assert t.name == "Tokyo Night"

    def test_spring_theme_exists(self, theme_manager):
        t = theme_manager.get_theme("Spring")
        assert t.name == "Spring"

    def test_ibm_cga_theme_exists(self, theme_manager):
        t = theme_manager.get_theme("IBM PC CGA")
        assert t.name == "IBM PC CGA"

    def test_zx_spectrum_theme_exists(self, theme_manager):
        t = theme_manager.get_theme("ZX Spectrum")
        assert t.name == "ZX Spectrum"

    def test_high_contrast_light_theme_exists(self, theme_manager):
        t = theme_manager.get_theme("High Contrast Light")
        assert t.name == "High Contrast Light"

    def test_dyslexia_friendly_theme_exists(self, theme_manager):
        t = theme_manager.get_theme("Dyslexia Friendly")
        assert t.name == "Dyslexia Friendly"

    def test_accessible_dark_blue_theme_exists(self, theme_manager):
        t = theme_manager.get_theme("Accessible Dark Blue")
        assert t.name == "Accessible Dark Blue"

    def test_one_dark_pro_theme_exists(self, theme_manager):
        t = theme_manager.get_theme("One Dark Pro")
        assert t.name == "One Dark Pro"


class TestThemeAttributesExtended:
    """More theme attribute tests."""

    def test_dracula_has_background(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert len(t.background) > 0

    def test_monokai_has_foreground(self, theme_manager):
        t = theme_manager.get_theme("Monokai")
        assert len(t.foreground) > 0

    def test_nord_has_keyword(self, theme_manager):
        t = theme_manager.get_theme("Nord")
        assert len(t.keyword) > 0

    def test_solarized_dark_has_editor_bg(self, theme_manager):
        t = theme_manager.get_theme("Solarized Dark")
        assert len(t.editor_bg) > 0

    def test_vs_code_dark_has_comment(self, theme_manager):
        t = theme_manager.get_theme("VS Code Dark")
        assert len(t.comment) > 0

    def test_github_dark_has_string(self, theme_manager):
        t = theme_manager.get_theme("GitHub Dark")
        assert len(t.string) > 0

    def test_amber_has_canvas_bg(self, theme_manager):
        t = theme_manager.get_theme("Amber Monochrome")
        assert len(t.canvas_bg) > 0

    def test_green_has_menu_bg(self, theme_manager):
        t = theme_manager.get_theme("Green Monochrome")
        assert len(t.menu_bg) > 0

    def test_c64_has_number(self, theme_manager):
        t = theme_manager.get_theme("Commodore 64")
        assert len(t.number) > 0

    def test_apple_ii_has_menu_fg(self, theme_manager):
        t = theme_manager.get_theme("Apple II")
        assert len(t.menu_fg) > 0

    def test_dos_blue_name_correct(self, theme_manager):
        t = theme_manager.get_theme("DOS Blue")
        assert t.name == "DOS Blue"

    def test_tokyo_night_background_not_empty(self, theme_manager):
        t = theme_manager.get_theme("Tokyo Night")
        assert t.background != ""

    def test_ocean_editor_fg_not_empty(self, theme_manager):
        t = theme_manager.get_theme("Ocean")
        assert t.editor_fg != ""

    def test_catppuccin_mocha_keyword_not_empty(self, theme_manager):
        t = theme_manager.get_theme("Catppuccin Mocha")
        assert t.keyword != ""

    def test_gruvbox_dark_string_not_empty(self, theme_manager):
        t = theme_manager.get_theme("Gruvbox Dark")
        assert t.string != ""


class TestThemeAttributesExtended2:
    """Second round of theme attribute checks."""

    def test_dracula_background_hash(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert t.background.startswith("#")

    def test_monokai_editor_fg_hash(self, theme_manager):
        t = theme_manager.get_theme("Monokai")
        assert t.editor_fg.startswith("#")

    def test_nord_keyword_not_empty(self, theme_manager):
        t = theme_manager.get_theme("Nord")
        assert len(t.keyword) > 0

    def test_solarized_dark_comment_not_empty(self, theme_manager):
        t = theme_manager.get_theme("Solarized Dark")
        assert len(t.comment) > 0

    def test_github_dark_string_not_empty(self, theme_manager):
        t = theme_manager.get_theme("GitHub Dark")
        assert len(t.string) > 0

    def test_all_themes_have_background(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "background")

    def test_all_themes_have_editor_fg(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "editor_fg")

    def test_all_themes_have_keyword(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "keyword")

    def test_all_themes_have_string(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "string")

    def test_all_themes_have_comment(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "comment")


class TestThemeAttributesExtended3:
    """Third round of theme attribute checks."""

    def test_gruvbox_has_background(self, theme_manager):
        t = theme_manager.get_theme("Gruvbox Dark")
        assert hasattr(t, "background")

    def test_solarized_has_keyword(self, theme_manager):
        t = theme_manager.get_theme("Solarized Light")
        assert hasattr(t, "keyword")

    def test_vs_code_dark_background_hash(self, theme_manager):
        t = theme_manager.get_theme("VS Code Dark")
        assert t.background.startswith("#")

    def test_nord_has_string_attr(self, theme_manager):
        t = theme_manager.get_theme("Nord")
        assert hasattr(t, "string")

    def test_catppuccin_has_comment(self, theme_manager):
        t = theme_manager.get_theme("Catppuccin Mocha")
        assert hasattr(t, "comment")

    def test_github_light_has_editor_fg(self, theme_manager):
        t = theme_manager.get_theme("GitHub Light")
        assert hasattr(t, "editor_fg")

    def test_all_themes_have_keyword_attr(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "keyword")

    def test_all_themes_background_is_string(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert isinstance(t.background, str)

    def test_all_themes_string_attr_is_str(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert isinstance(t.string, str)

    def test_theme_count_gte_10(self, theme_manager):
        names = theme_manager.get_theme_names()
        assert len(names) >= 10


class TestThemeAttributesExtended4:
    """Fourth round of theme attribute checks."""

    def test_all_themes_have_keyword_attr(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "keyword")

    def test_all_themes_have_string_attr(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "string")

    def test_all_themes_have_comment_attr(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "comment")

    def test_theme_names_list_not_empty(self, theme_manager):
        names = theme_manager.get_theme_names()
        assert len(names) > 0

    def test_theme_names_all_strings(self, theme_manager):
        for name in theme_manager.get_theme_names():
            assert isinstance(name, str)

    def test_monokai_background_not_empty(self, theme_manager):
        t = theme_manager.get_theme("Monokai")
        assert len(t.background) > 0

    def test_dracula_background_not_empty(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert len(t.background) > 0

    def test_nord_background_not_empty(self, theme_manager):
        t = theme_manager.get_theme("Nord")
        assert len(t.background) > 0

    def test_theme_manager_not_none(self, theme_manager):
        assert theme_manager is not None

    def test_theme_count_gte_20(self, theme_manager):
        names = theme_manager.get_theme_names()
        assert len(names) >= 20


class TestThemeAttributesExtended5:
    """Fifth round of theme attribute checks."""

    def test_all_themes_have_background(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "background")

    def test_all_themes_have_foreground(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "foreground")

    def test_theme_backgrounds_are_strings(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert isinstance(t.background, str)

    def test_theme_foregrounds_are_strings(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert isinstance(t.foreground, str)

    def test_theme_count_gte_twenty(self, theme_manager):
        assert len(theme_manager.get_theme_names()) >= 20

    def test_dracula_in_themes(self, theme_manager):
        names = [n.lower() for n in theme_manager.get_theme_names()]
        assert any("dracula" in n for n in names)

    def test_monokai_in_themes(self, theme_manager):
        names = [n.lower() for n in theme_manager.get_theme_names()]
        assert any("monokai" in n for n in names)

    def test_nord_in_themes(self, theme_manager):
        names = [n.lower() for n in theme_manager.get_theme_names()]
        assert any("nord" in n for n in names)

    def test_theme_names_unique(self, theme_manager):
        names = theme_manager.get_theme_names()
        assert len(names) == len(set(names))

    def test_all_themes_have_string_attr(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "string")


class TestThemeAttributesExtended6:
    """Sixth round of theme attribute checks."""

    def test_all_themes_have_background(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "background")

    def test_all_themes_have_foreground(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "foreground")

    def test_theme_count_gte_twenty(self, theme_manager):
        assert len(theme_manager.get_theme_names()) >= 20

    def test_theme_names_are_strings(self, theme_manager):
        for name in theme_manager.get_theme_names():
            assert isinstance(name, str)

    def test_theme_names_unique(self, theme_manager):
        names = theme_manager.get_theme_names()
        assert len(names) == len(set(names))

    def test_backgrounds_are_strings(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert isinstance(t.background, str)

    def test_foregrounds_are_strings(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert isinstance(t.foreground, str)

    def test_dracula_in_themes(self, theme_manager):
        names = [n.lower() for n in theme_manager.get_theme_names()]
        assert any("dracula" in n for n in names)

    def test_monokai_in_themes(self, theme_manager):
        names = [n.lower() for n in theme_manager.get_theme_names()]
        assert any("monokai" in n for n in names)

    def test_all_themes_have_keyword(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "keyword")


class TestThemeAttributesExtended7:
    """Seventh round of theme attribute checks."""

    def test_all_themes_have_background(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "background")

    def test_all_themes_have_foreground(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "foreground")

    def test_theme_count_gte_twenty(self, theme_manager):
        assert len(theme_manager.get_theme_names()) >= 20

    def test_theme_names_are_strings(self, theme_manager):
        for name in theme_manager.get_theme_names():
            assert isinstance(name, str)

    def test_theme_names_unique(self, theme_manager):
        names = theme_manager.get_theme_names()
        assert len(names) == len(set(names))

    def test_dracula_in_themes(self, theme_manager):
        names = [n.lower() for n in theme_manager.get_theme_names()]
        assert any("dracula" in n for n in names)

    def test_nord_in_themes(self, theme_manager):
        names = [n.lower() for n in theme_manager.get_theme_names()]
        assert any("nord" in n for n in names)

    def test_all_themes_have_keyword(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "keyword")

    def test_all_themes_background_is_str(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert isinstance(t.background, str)

    def test_get_theme_returns_object(self, theme_manager):
        name = theme_manager.get_theme_names()[0]
        t = theme_manager.get_theme(name)
        assert t is not None


class TestThemeAttributesExtended8:
    """Eighth extended round of theme attribute tests."""

    def test_all_themes_have_foreground(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "foreground")

    def test_all_themes_have_background(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "background")

    def test_theme_count_gte_twenty(self, theme_manager):
        assert len(theme_manager.get_theme_names()) >= 20

    def test_dracula_present(self, theme_manager):
        assert any("dracula" in n.lower() for n in theme_manager.get_theme_names())

    def test_monokai_present(self, theme_manager):
        assert any("monokai" in n.lower() for n in theme_manager.get_theme_names())

    def test_names_are_unique(self, theme_manager):
        names = theme_manager.get_theme_names()
        assert len(names) == len(set(names))

    def test_all_themes_have_comment(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "comment")

    def test_first_theme_not_none(self, theme_manager):
        t = theme_manager.get_theme(theme_manager.get_theme_names()[0])
        assert t is not None

    def test_foreground_is_str(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert isinstance(t.foreground, str)

    def test_names_list_not_empty(self, theme_manager):
        assert len(theme_manager.get_theme_names()) > 0


class TestThemeAttributesExtended9:
    """Ninth extended round of theme attribute tests."""

    def test_theme_manager_exists(self, theme_manager):
        assert theme_manager is not None

    def test_themes_is_list(self, theme_manager):
        themes = theme_manager.get_theme_names()
        assert isinstance(themes, list)

    def test_theme_count_above_10(self, theme_manager):
        assert len(theme_manager.get_theme_names()) > 10

    def test_theme_count_above_20(self, theme_manager):
        assert len(theme_manager.get_theme_names()) > 20

    def test_all_names_nonempty(self, theme_manager):
        for name in theme_manager.get_theme_names():
            assert len(name) > 0

    def test_dracula_in_themes(self, theme_manager):
        assert "Dracula" in theme_manager.get_theme_names()

    def test_theme_has_background(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert hasattr(t, "background")

    def test_theme_has_foreground(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert hasattr(t, "foreground")

    def test_background_is_str(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert isinstance(t.background, str)

    def test_foreground_is_str(self, theme_manager):
        t = theme_manager.get_theme("Dracula")
        assert isinstance(t.foreground, str)


class TestThemeAttributesExtended10:
    def test_manager_exists(self, theme_manager):
        assert theme_manager is not None

    def test_count_above_25(self, theme_manager):
        assert len(theme_manager.get_theme_names()) > 25

    def test_monokai_exists(self, theme_manager):
        assert "Monokai" in theme_manager.get_theme_names()

    def test_nord_exists(self, theme_manager):
        assert "Nord" in theme_manager.get_theme_names()

    def test_monokai_background(self, theme_manager):
        t = theme_manager.get_theme("Monokai")
        assert isinstance(t.background, str)

    def test_monokai_foreground(self, theme_manager):
        t = theme_manager.get_theme("Monokai")
        assert isinstance(t.foreground, str)

    def test_nord_background(self, theme_manager):
        t = theme_manager.get_theme("Nord")
        assert isinstance(t.background, str)

    def test_all_themes_have_bg(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "background")

    def test_all_themes_have_fg(self, theme_manager):
        for name in theme_manager.get_theme_names():
            t = theme_manager.get_theme(name)
            assert hasattr(t, "foreground")

    def test_names_all_str(self, theme_manager):
        for name in theme_manager.get_theme_names():
            assert isinstance(name, str)
