"""
CodeEditor widget unit tests.

Tests line number area sizing, completion insertion, current line
highlighting, and whitespace visibility toggle.
"""

import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[1]))

# pylint: disable=wrong-import-position,import-error,no-name-in-module
from PySide6.QtCore import QSize  # noqa: E402
from PySide6.QtWidgets import QApplication  # noqa: E402

from time_warp.ui.editor import CodeEditor  # noqa: E402


def _ensure_app():
    """Create QApplication if not already running."""
    app = QApplication.instance()
    if app is None:
        app = QApplication(sys.argv)
    return app


def test_line_number_area_size_hint_matches_editor_width():
    """Line number area width matches editor calculation."""
    _ensure_app()
    editor = CodeEditor()
    expected_width = editor.line_number_area_width()
    size_hint = editor.line_number_area.sizeHint()

    assert isinstance(size_hint, QSize)
    assert size_hint.width() == expected_width
    assert size_hint.height() == 0


def test_line_number_area_width_changes_with_more_blocks():
    """Line number area grows wider with more lines."""
    _ensure_app()
    editor = CodeEditor()
    base_width = editor.line_number_area_width()
    editor.setPlainText("\n".join(["line"] * 120))
    new_width = editor.line_number_area_width()

    assert new_width >= base_width + 3


def test_insert_completion_replaces_prefix_correctly():
    """Completion replaces typed prefix, not entire text."""
    _ensure_app()
    editor = CodeEditor()
    editor.setPlainText("te")
    cursor = editor.textCursor()
    cursor.setPosition(len(editor.toPlainText()))
    editor.setTextCursor(cursor)

    editor.completer.setCompletionPrefix("te")
    editor.insert_completion("testcase")

    assert editor.toPlainText() == "testcase"


def test_highlight_current_line_toggle():
    """Current line highlight can be enabled/disabled."""
    _ensure_app()
    editor = CodeEditor()

    assert not editor.is_highlight_current_line_enabled()
    editor.enable_highlight_current_line(True)
    assert editor.is_highlight_current_line_enabled()

    editor.setPlainText("one\n two")
    c = editor.textCursor()
    c.setPosition(0)
    editor.setTextCursor(c)
    c.setPosition(1)
    editor.setTextCursor(c)
    c.setPosition(0)
    editor.setTextCursor(c)

    assert len(editor.extraSelections()) >= 1

    editor.enable_highlight_current_line(False)
    assert not editor.is_highlight_current_line_enabled()
    assert editor.extraSelections() == []


def test_show_whitespace_toggle():
    """Whitespace visibility can be toggled."""
    _ensure_app()
    editor = CodeEditor()

    assert not editor.is_show_whitespace_enabled()
    editor.enable_show_whitespace(True)
    assert editor.is_show_whitespace_enabled()
    assert getattr(editor, "_whitespace_highlighter", None) is not None

    editor.enable_show_whitespace(False)
    assert not editor.is_show_whitespace_enabled()
    assert getattr(editor, "_whitespace_highlighter", None) is None
