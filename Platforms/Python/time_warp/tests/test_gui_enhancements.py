"""Tests for all 14 GUI enhancements added to Time Warp Studio.

These tests verify:
1. Command Palette  — action collection, search filtering
2. Coach Marks      — should_show, mark_shown, step logic
3. Minimap Widget   — class structure, enable/disable API
4. Canvas Toolbar   — zoom methods, fit-to-screen maths
5. Tab Dot Indicator — tab title updated when modified
6. Breadcrumb label — cursor context extraction
7. Language Picker  — dialog can be constructed
8. Accessibility    — preset applies theme + font changes
9. Run History      — save, load, clear cycle
10. Output Tabs     — routing to errors_log / turtle_log
11. Inline Underlines — error line wave-underline application
12. Theme Preview   — dialog creation
13. Split Editor    — new tab creation with cloned content
14. Minimap Toggle  — toggling all editors

NOTE: Qt objects are only instantiated when the ``qapp`` fixture is active.
"""

from __future__ import annotations

import os
import sys

import pytest

# ---------------------------------------------------------------------------
# Make sure we are running in a headless environment that allows Qt to work.
# ---------------------------------------------------------------------------
os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

# ---------------------------------------------------------------------------
# Skip the entire module if PySide6 is unavailable or cannot initialise.
# Any exception (ImportError, RuntimeError, OSError, etc.) disables the suite.
# ---------------------------------------------------------------------------
try:
    import PySide6.QtCore  # noqa: F401 — import-time check only
    import PySide6.QtWidgets as _qtw  # noqa: F401

    # Verify we can actually create/obtain a QApplication before committing.
    _app_check = _qtw.QApplication.instance() or _qtw.QApplication(sys.argv[:1])
    del _qtw, _app_check
    _PYSIDE6_AVAILABLE = True
except Exception:  # noqa: BLE001 — any failure means Qt is unusable here
    _PYSIDE6_AVAILABLE = False

pytestmark = pytest.mark.skipif(
    not _PYSIDE6_AVAILABLE,
    reason="PySide6 / Qt is not usable in this environment",
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture(scope="module")
def qapp():
    """Return (or create) the QApplication; skip the test if Qt is unusable."""
    try:
        from PySide6.QtWidgets import QApplication

        app = QApplication.instance()
        if app is None:
            app = QApplication(sys.argv[:1])
        yield app
    except Exception as exc:  # noqa: BLE001
        pytest.skip(f"QApplication could not be created: {exc}")


# ---------------------------------------------------------------------------
# 1. CommandPalette
# ---------------------------------------------------------------------------


class TestCommandPalette:
    """Command Palette widget tests."""

    def test_import(self):
        from time_warp.ui.command_palette import CommandPalette

        assert CommandPalette is not None

    def test_construction(self, qapp):
        from time_warp.ui.command_palette import CommandPalette

        palette = CommandPalette()
        assert palette is not None
        assert palette.results_list is not None
        assert palette.search_input is not None
        palette.deleteLater()

    def test_search_filters_actions(self, qapp):
        """Search should narrow the results list."""
        from time_warp.ui.command_palette import CommandPalette
        from PySide6.QtGui import QAction

        palette = CommandPalette()
        # Inject some dummy actions
        palette._actions = [
            ("Open File", "Ctrl+O", QAction("Open File")),
            ("Save File", "Ctrl+S", QAction("Save File")),
            ("Run Program", "Ctrl+R", QAction("Run Program")),
        ]
        # Filter for "file"
        palette._on_search("file")
        assert palette.results_list.count() == 2  # Open File + Save File
        # Filter for something not present
        palette._on_search("zzzzz")
        assert palette.results_list.count() == 0
        # Empty search shows everything
        palette._on_search("")
        assert palette.results_list.count() == 3
        palette.deleteLater()

    def test_keyboard_navigation(self, qapp):
        """Arrow keys should change the selected row."""
        from time_warp.ui.command_palette import CommandPalette
        from PySide6.QtGui import QAction, QKeyEvent
        from PySide6.QtCore import Qt

        palette = CommandPalette()
        palette._actions = [
            ("Action A", "", QAction("A")),
            ("Action B", "", QAction("B")),
        ]
        palette._populate_list(palette._actions)
        assert palette.results_list.currentRow() == 0

        down_key = QKeyEvent(QKeyEvent.Type.KeyPress, Qt.Key_Down, Qt.NoModifier)
        palette.keyPressEvent(down_key)
        assert palette.results_list.currentRow() == 1

        up_key = QKeyEvent(QKeyEvent.Type.KeyPress, Qt.Key_Up, Qt.NoModifier)
        palette.keyPressEvent(up_key)
        assert palette.results_list.currentRow() == 0
        palette.deleteLater()


# ---------------------------------------------------------------------------
# 2. CoachMarkManager
# ---------------------------------------------------------------------------


class TestCoachMarkManager:
    """Coach mark walkthrough tests."""

    def test_import(self):
        from time_warp.ui.coach_marks import CoachMarkManager, CoachMarkOverlay

        assert CoachMarkManager is not None
        assert CoachMarkOverlay is not None

    def test_steps_defined(self):
        from time_warp.ui.coach_marks import CoachMarkManager

        assert len(CoachMarkManager.STEPS) >= 4

    def test_should_show_initially(self):
        """should_show() reads from QSettings; we can just verify it's callable."""
        from time_warp.ui.coach_marks import CoachMarkManager
        from PySide6.QtWidgets import QMainWindow

        # Create a minimal stand-in for main window
        class FakeWin:
            centralWidget = lambda s: None  # noqa: E731
            editor_tabs = None

        mgr = CoachMarkManager(FakeWin())
        result = mgr.should_show()
        assert isinstance(result, bool)

    def test_mark_shown_persists(self):
        """mark_shown() should prevent should_show() returning True on next call."""
        from time_warp.ui.coach_marks import CoachMarkManager
        from PySide6.QtCore import QSettings

        class FakeWin:
            pass

        mgr = CoachMarkManager(FakeWin())
        # Clear any prior state
        s = QSettings("TimeWarp", "IDE")
        s.remove(CoachMarkManager.SETTINGS_KEY)

        assert mgr.should_show() is True
        mgr.mark_shown()
        assert mgr.should_show() is False

        # Cleanup
        s.remove(CoachMarkManager.SETTINGS_KEY)

    def test_navigation_logic(self, qapp):
        """Steps should increment/decrement with next/prev."""
        from time_warp.ui.coach_marks import CoachMarkManager
        from PySide6.QtWidgets import QWidget

        central = QWidget()

        class FakeWin:
            def centralWidget(self):
                return central

        mgr = CoachMarkManager(FakeWin())
        mgr._step_index = 1
        mgr._prev_step()
        assert mgr._step_index == 0
        mgr._next_step()
        assert mgr._step_index == 1
        central.deleteLater()


# ---------------------------------------------------------------------------
# 3. MinimapWidget
# ---------------------------------------------------------------------------


class TestMinimapWidget:
    """Minimap side-panel tests."""

    def test_class_exists(self):
        from time_warp.ui.editor import MinimapWidget

        assert MinimapWidget is not None
        assert MinimapWidget.MINIMAP_WIDTH == 80

    def test_enable_disable(self, qapp):
        from time_warp.ui.editor import CodeEditor

        editor = CodeEditor()
        assert editor.is_minimap_enabled() is False
        assert editor.minimap.isHidden()

        editor.enable_minimap(True)
        assert editor.is_minimap_enabled() is True
        assert not editor.minimap.isHidden()

        editor.enable_minimap(False)
        assert editor.is_minimap_enabled() is False
        assert editor.minimap.isHidden()
        editor.deleteLater()

    def test_minimap_creation(self, qapp):
        from time_warp.ui.editor import CodeEditor, MinimapWidget

        editor = CodeEditor()
        assert isinstance(editor.minimap, MinimapWidget)
        editor.deleteLater()


# ---------------------------------------------------------------------------
# 4. Canvas Toolbar (zoom/pan helpers)
# ---------------------------------------------------------------------------


class TestCanvasToolbar:
    """TurtleCanvas overlay toolbar tests."""

    def test_toolbar_created(self, qapp):
        from time_warp.ui.canvas import TurtleCanvas

        canvas = TurtleCanvas()
        assert hasattr(canvas, "_canvas_toolbar")
        canvas.deleteLater()

    def test_zoom_in(self, qapp):
        from time_warp.ui.canvas import TurtleCanvas

        canvas = TurtleCanvas()
        initial_zoom = canvas.zoom
        canvas._zoom_in()
        assert canvas.zoom > initial_zoom
        canvas.deleteLater()

    def test_zoom_out(self, qapp):
        from time_warp.ui.canvas import TurtleCanvas

        canvas = TurtleCanvas()
        canvas._zoom_in()  # start at 1.25
        canvas._zoom_out()
        assert abs(canvas.zoom - 1.0) < 0.01
        canvas.deleteLater()

    def test_reset_view(self, qapp):
        from time_warp.ui.canvas import TurtleCanvas

        canvas = TurtleCanvas()
        canvas.zoom = 5.0
        canvas.offset_x = 200.0
        canvas.offset_y = 100.0
        canvas._reset_view()
        assert canvas.zoom == 1.0
        assert canvas.offset_x == 0.0
        assert canvas.offset_y == 0.0
        canvas.deleteLater()

    def test_fit_to_screen_no_lines(self, qapp):
        """fit_to_screen with no lines should reset to default."""
        from time_warp.ui.canvas import TurtleCanvas

        canvas = TurtleCanvas()
        canvas.zoom = 3.0
        canvas._fit_to_screen()
        assert canvas.zoom == 1.0
        canvas.deleteLater()

    def test_fit_to_screen_with_lines(self, qapp):
        """fit_to_screen should adjust zoom to fit content."""
        from time_warp.ui.canvas import TurtleCanvas
        from time_warp.graphics.turtle_state import TurtleLine

        canvas = TurtleCanvas()
        canvas.resize(400, 400)
        canvas.lines = [
            TurtleLine(
                start_x=0, start_y=0, end_x=100, end_y=0, color=(255, 0, 0), width=1
            ),
            TurtleLine(
                start_x=0, start_y=0, end_x=0, end_y=100, color=(0, 255, 0), width=1
            ),
        ]
        canvas._fit_to_screen()
        # Zoom should have been calculated (not default 1.0)
        assert canvas.zoom != 1.0 or canvas.offset_x != 0.0 or canvas.offset_y != 0.0
        canvas.deleteLater()

    def test_zoom_capped_at_maximum(self, qapp):
        from time_warp.ui.canvas import TurtleCanvas

        canvas = TurtleCanvas()
        canvas.zoom = 20.0
        canvas._zoom_in()
        assert canvas.zoom <= 24.0
        canvas.deleteLater()

    def test_zoom_capped_at_minimum(self, qapp):
        from time_warp.ui.canvas import TurtleCanvas

        canvas = TurtleCanvas()
        canvas.zoom = 0.06
        canvas._zoom_out()
        assert canvas.zoom >= 0.05
        canvas.deleteLater()


# ---------------------------------------------------------------------------
# 5. Tab Dot Indicator
# ---------------------------------------------------------------------------


class TestTabDotIndicator:
    """Unsaved-changes tab dot indicator tests."""

    def test_dot_added_on_modified(self, qapp):
        """Setting modified=True should add ● to the tab title."""
        from time_warp.ui.main_window import MainWindow
        from PySide6.QtWidgets import QTabWidget, QWidget

        win = MainWindow.__new__(MainWindow)
        # Minimal stub
        win.editor_tabs = QTabWidget()
        win.editor_tabs.addTab(QWidget(), "Untitled")
        win.tab_modified = {0: False}
        win.tab_files = {0: None}
        win.tab_languages = {0: None}

        win._update_tab_title_indicator(0, True)
        title = win.editor_tabs.tabText(0)
        assert "●" in title
        win.editor_tabs.deleteLater()

    def test_dot_removed_on_save(self, qapp):
        """Setting modified=False should remove ● from the tab title."""
        from time_warp.ui.main_window import MainWindow
        from PySide6.QtWidgets import QTabWidget, QWidget

        win = MainWindow.__new__(MainWindow)
        win.editor_tabs = QTabWidget()
        win.editor_tabs.addTab(QWidget(), "Untitled ●")
        win.tab_modified = {0: True}
        win.tab_files = {0: None}
        win.tab_languages = {0: None}

        win._update_tab_title_indicator(0, False)
        title = win.editor_tabs.tabText(0)
        assert "●" not in title
        win.editor_tabs.deleteLater()


# ---------------------------------------------------------------------------
# 6. Breadcrumb — cursor context extraction
# ---------------------------------------------------------------------------


class TestCursorContext:
    """Breadcrumb context extraction tests."""

    def _make_editor_with_content(self, text: str):
        from time_warp.ui.editor import CodeEditor

        editor = CodeEditor()
        editor.setPlainText(text)
        return editor

    def test_def_function_detected(self, qapp):
        from time_warp.ui.main_window import MainWindow

        win = MainWindow.__new__(MainWindow)
        editor = self._make_editor_with_content("def my_function():\n    pass\n")
        # Position cursor inside the function body
        cursor = editor.textCursor()
        cursor.movePosition(cursor.MoveOperation.End)
        editor.setTextCursor(cursor)
        result = win._get_cursor_context(editor, cursor)
        assert result == "my_function"
        editor.deleteLater()

    def test_no_context_returns_empty(self, qapp):
        from time_warp.ui.main_window import MainWindow

        win = MainWindow.__new__(MainWindow)
        editor = self._make_editor_with_content("x = 1\ny = 2\n")
        cursor = editor.textCursor()
        result = win._get_cursor_context(editor, cursor)
        assert result == ""
        editor.deleteLater()

    def test_subroutine_detected(self, qapp):
        from time_warp.ui.main_window import MainWindow

        win = MainWindow.__new__(MainWindow)
        code = "SUB MyRoutine\n  PRINT 1\nEND SUB\n"
        editor = self._make_editor_with_content(code)
        cursor = editor.textCursor()
        cursor.movePosition(cursor.MoveOperation.End)
        editor.setTextCursor(cursor)
        result = win._get_cursor_context(editor, cursor)
        assert result == "MyRoutine"
        editor.deleteLater()


# ---------------------------------------------------------------------------
# 7. Run History — save / load / clear
# ---------------------------------------------------------------------------


class TestRunHistory:
    """Run history save, load, and clear logic tests."""

    def _make_window_stub(self):
        """Create minimal MainWindow-like stub (no real Qt window)."""
        from time_warp.ui.main_window import MainWindow

        win = MainWindow.__new__(MainWindow)
        win._run_history = []
        from PySide6.QtCore import QSettings

        win.settings = QSettings("TimeWarp", "TestIDE")
        win.settings.remove("run_history")
        win._run_history_menu = None  # suppress menu build
        from unittest.mock import MagicMock

        win.statusbar = MagicMock()
        win.language_combo = MagicMock()
        win.editor_tabs = MagicMock()
        win.tab_languages = {}
        win.tab_files = {}
        win.tab_modified = {}
        win.create_new_tab = MagicMock(return_value=0)
        win.set_current_tab_info = MagicMock()
        return win

    def test_save_run_history(self):
        from time_warp.core.interpreter import Language

        win = self._make_window_stub()
        win._save_run_history("PRINT 42", Language.BASIC)
        assert len(win._run_history) == 1
        assert win._run_history[0]["language"] == "BASIC"
        assert "PRINT 42" in win._run_history[0]["code"]
        win.settings.remove("run_history")

    def test_run_history_max_10(self):
        from time_warp.core.interpreter import Language

        win = self._make_window_stub()
        for i in range(15):
            win._save_run_history(f"PRINT {i}", Language.BASIC)
        assert len(win._run_history) <= 10
        win.settings.remove("run_history")

    def test_clear_run_history(self):
        from time_warp.core.interpreter import Language

        win = self._make_window_stub()
        win._save_run_history("PRINT 1", Language.BASIC)
        win._clear_run_history()
        assert len(win._run_history) == 0
        win.settings.remove("run_history")

    def test_load_from_history(self):
        from time_warp.core.interpreter import Language

        win = self._make_window_stub()
        entry = {"code": "PRINT HI", "language": "BASIC", "snippet": "PRINT HI"}
        win._load_run_from_history(entry)
        win.create_new_tab.assert_called_once()
        win.settings.remove("run_history")


# ---------------------------------------------------------------------------
# 8. Output Tab Routing
# ---------------------------------------------------------------------------


class TestOutputTabRouting:
    """Output sub-tab routing tests."""

    def _make_routing_setup(self, qapp):
        """Create a minimal setup to test routing logic."""
        from time_warp.ui.main_window import MainWindow
        from PySide6.QtWidgets import QPlainTextEdit
        from unittest.mock import MagicMock

        win = MainWindow.__new__(MainWindow)
        win.errors_log = QPlainTextEdit()
        win.turtle_log = QPlainTextEdit()
        return win

    def test_error_routes_to_errors_log(self, qapp):
        win = self._make_routing_setup(qapp)
        win._route_output_to_subtabs("❌ Something went wrong", "error")
        assert "❌" in win.errors_log.toPlainText()
        assert win.turtle_log.toPlainText() == ""
        win.errors_log.deleteLater()
        win.turtle_log.deleteLater()

    def test_turtle_routes_to_turtle_log(self, qapp):
        win = self._make_routing_setup(qapp)
        win._route_output_to_subtabs("🐢 FORWARD 50", "normal")
        assert "🐢" in win.turtle_log.toPlainText()
        win.errors_log.deleteLater()
        win.turtle_log.deleteLater()

    def test_normal_output_not_routed(self, qapp):
        win = self._make_routing_setup(qapp)
        win._route_output_to_subtabs("Hello, World!", "normal")
        assert win.errors_log.toPlainText() == ""
        assert win.turtle_log.toPlainText() == ""
        win.errors_log.deleteLater()
        win.turtle_log.deleteLater()

    def test_clear_all_output(self, qapp):
        from unittest.mock import MagicMock

        win = self._make_routing_setup(qapp)
        win.output = MagicMock()
        win.errors_log.setPlainText("some error")
        win.turtle_log.setPlainText("🐢 turtle")
        win._clear_all_output()
        assert win.errors_log.toPlainText() == ""
        assert win.turtle_log.toPlainText() == ""
        win.errors_log.deleteLater()
        win.turtle_log.deleteLater()


# ---------------------------------------------------------------------------
# 9. Inline Error Underlines
# ---------------------------------------------------------------------------


class TestInlineErrorUnderlines:
    """Wave-underline error annotation tests."""

    def test_underline_applied(self, qapp):
        """_underline_error_lines_in_editor should add extra selections."""
        from time_warp.ui.main_window import MainWindow
        from time_warp.ui.editor import CodeEditor

        win = MainWindow.__new__(MainWindow)
        editor = CodeEditor()
        editor.setPlainText("line1\nline2\nline3\n")

        # Must be no extra selections initially
        assert len(editor.extraSelections()) == 0
        win._underline_error_lines_in_editor(editor, {2})
        assert len(editor.extraSelections()) >= 1
        editor.deleteLater()

    def test_empty_line_set_no_error(self, qapp):
        """Empty set should not crash or add selections."""
        from time_warp.ui.main_window import MainWindow
        from time_warp.ui.editor import CodeEditor

        win = MainWindow.__new__(MainWindow)
        editor = CodeEditor()
        editor.setPlainText("line1\nline2\n")
        win._underline_error_lines_in_editor(editor, set())
        assert len(editor.extraSelections()) == 0
        editor.deleteLater()

    def test_none_editor_no_crash(self, qapp):
        """Passing None for editor should not raise."""
        from time_warp.ui.main_window import MainWindow

        win = MainWindow.__new__(MainWindow)
        win._underline_error_lines_in_editor(None, {1})  # should not raise


# ---------------------------------------------------------------------------
# 10. Split Editor Panes
# ---------------------------------------------------------------------------


class TestSplitEditor:
    """Split-editor pane tests."""

    def test_split_creates_new_tab(self, qapp):
        """Splitting should open a new tab with the same code."""
        from time_warp.ui.main_window import MainWindow
        from time_warp.ui.editor import CodeEditor
        from time_warp.core.interpreter import Language
        from unittest.mock import MagicMock, patch
        from PySide6.QtWidgets import QTabWidget

        win = MainWindow.__new__(MainWindow)
        win.tab_languages = {0: Language.BASIC}
        win.editor_tabs = QTabWidget()

        editor = CodeEditor()
        editor.setPlainText("PRINT 42")
        win.editor_tabs.addTab(editor, "Test")
        win.editor_tabs.setCurrentIndex(0)

        created_tabs = []

        def fake_create_new_tab(title, content, language):
            created_tabs.append((title, content, language))
            e2 = CodeEditor()
            e2.setPlainText(content)
            win.editor_tabs.addTab(e2, title)
            return win.editor_tabs.count() - 1

        win.create_new_tab = fake_create_new_tab
        win.set_current_tab_info = MagicMock()
        win.statusbar = MagicMock()

        win._split_editor_right()

        assert len(created_tabs) == 1
        _title, content, lang = created_tabs[0]
        assert content == "PRINT 42"
        assert lang == Language.BASIC
        assert "[split]" in _title
        win.editor_tabs.deleteLater()


# ---------------------------------------------------------------------------
# 11. Minimap Toggle (all editors)
# ---------------------------------------------------------------------------


class TestMinimapToggle:
    """Minimap toggle tests via main_window._toggle_minimap."""

    def test_toggle_on_all_editors(self, qapp):
        """_toggle_minimap should call enable_minimap on every editor tab."""
        from time_warp.ui.main_window import MainWindow
        from time_warp.ui.editor import CodeEditor
        from unittest.mock import MagicMock
        from PySide6.QtWidgets import QTabWidget

        win = MainWindow.__new__(MainWindow)
        win.editor_tabs = QTabWidget()
        win.statusbar = MagicMock()

        editors = [CodeEditor() for _ in range(3)]
        for i, ed in enumerate(editors):
            win.editor_tabs.addTab(ed, f"Tab {i}")

        win._toggle_minimap(True)
        for ed in editors:
            assert ed.is_minimap_enabled() is True

        win._toggle_minimap(False)
        for ed in editors:
            assert ed.is_minimap_enabled() is False

        win.editor_tabs.deleteLater()


# ---------------------------------------------------------------------------
# 12. Accessibility Preset
# ---------------------------------------------------------------------------


class TestAccessibilityPreset:
    """Accessibility preset tests."""

    def test_preset_calls_font_and_theme(self, qapp):
        """_apply_accessibility_preset should call change_font_size and change_theme."""
        from time_warp.ui.main_window import MainWindow
        from unittest.mock import MagicMock, patch

        win = MainWindow.__new__(MainWindow)
        win.statusbar = MagicMock()
        win.crt_enable_action = MagicMock()
        win.crt_enable_action.isChecked.return_value = False

        fake_theme_manager = MagicMock()
        fake_theme_manager.get_theme_names.return_value = [
            "Dracula",
            "Solarized Light",
            "Spring",
        ]
        win.theme_manager = fake_theme_manager

        calls = []
        win.change_theme = lambda name: calls.append(("theme", name))
        win.change_font_size = lambda size: calls.append(("font", size))

        win._apply_accessibility_preset()

        font_calls = [c for c in calls if c[0] == "font"]
        theme_calls = [c for c in calls if c[0] == "theme"]
        assert len(font_calls) == 1 and font_calls[0][1] == 18
        assert len(theme_calls) == 1


# ---------------------------------------------------------------------------
# 13. Theme Preview Dialog — construction only
# ---------------------------------------------------------------------------


class TestThemePreviewDialog:
    """Theme preview dialog creation test."""

    def test_dialog_can_be_constructed(self, qapp):
        """_show_theme_preview_dialog should create a QDialog without crashing."""
        from time_warp.ui.main_window import MainWindow
        from PySide6.QtWidgets import QMainWindow
        from unittest.mock import MagicMock, patch

        # Use a real QMainWindow so QDialog(self) resolves correctly
        class FakeWin(QMainWindow):
            pass

        win = FakeWin()
        fake_tm = MagicMock()
        fake_tm.get_theme_names.return_value = ["Dracula", "Monokai"]
        fake_tm.current_theme_name = "Dracula"
        fake_tm.apply_theme = MagicMock()
        win.theme_manager = fake_tm
        win.change_theme = MagicMock()
        win.statusbar = MagicMock()

        # Patch QDialog.exec to prevent blocking
        with patch("PySide6.QtWidgets.QDialog.exec", return_value=0):
            MainWindow._show_theme_preview_dialog(win)

        fake_tm.get_theme_names.assert_called()
        win.deleteLater()


# ---------------------------------------------------------------------------
# 14. Language Picker Dialog
# ---------------------------------------------------------------------------


class TestLanguagePicker:
    """Language picker dialog tests."""

    def test_dialog_can_be_constructed(self, qapp):
        from time_warp.ui.main_window import MainWindow
        from time_warp.core.interpreter import Language
        from unittest.mock import MagicMock, patch
        from PySide6.QtWidgets import QMainWindow, QTabWidget

        # Use a real QMainWindow so QDialog(self) resolves correctly
        class FakeWin(QMainWindow):
            pass

        win = FakeWin()
        win.editor_tabs = QTabWidget()
        win.tab_languages = {0: Language.BASIC}
        win.language_combo = MagicMock()
        win.language_combo.count.return_value = len(list(Language))
        win.language_combo.itemData = lambda i: list(Language)[i]

        # Patch exec to not block
        with patch("PySide6.QtWidgets.QDialog.exec", return_value=0):
            MainWindow._show_language_picker(win)  # should not raise

        win.editor_tabs.deleteLater()
        win.deleteLater()
