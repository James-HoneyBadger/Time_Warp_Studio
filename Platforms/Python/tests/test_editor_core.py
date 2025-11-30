import pathlib
import sys

# Ensure project package directory is importable by pytest
sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[1]))

from PySide6.QtCore import QSize  # noqa: E402
from PySide6.QtWidgets import QApplication  # noqa: E402

# time_warp comes from the repo tree.
# The import may occur after sys.path manipulation in tests.
from time_warp.ui.editor import CodeEditor  # noqa: E402


def _ensure_app():
    """Ensure a QApplication exists for widget tests."""
    app = QApplication.instance()
    if app is None:
        app = QApplication(sys.argv)
    return app


def test_line_number_area_size_hint_matches_editor_width():
    _ensure_app()
    editor = CodeEditor()

    # width should match the value returned by code editor helper
    expected_width = editor.line_number_area_width()
    size_hint = editor.line_number_area.sizeHint()

    assert isinstance(size_hint, QSize)
    assert size_hint.width() == expected_width
    assert size_hint.height() == 0


def test_line_number_area_width_changes_with_more_blocks():
    _ensure_app()
    editor = CodeEditor()

    base_width = editor.line_number_area_width()
    # Insert many lines to increase blockCount and cause width growth
    editor.setPlainText("\n".join(["line"] * 120))
    new_width = editor.line_number_area_width()

    assert new_width >= base_width
    # With many blocks, width will usually grow.
    assert new_width >= base_width + 3


def test_insert_completion_replaces_prefix_correctly():
    _ensure_app()
    editor = CodeEditor()

    # prepare a short prefix in the editor
    editor.setPlainText("te")
    cursor = editor.textCursor()
    # Position the cursor at the end of document explicitly
    cursor.setPosition(len(editor.toPlainText()))
    editor.setTextCursor(cursor)

    # configure completer prefix 'te' and insert completion 'testcase'
    editor.completer.setCompletionPrefix("te")
    editor.insert_completion("testcase")

    assert editor.toPlainText() == "testcase"


def test_highlight_current_line_toggle():
    _ensure_app()
    editor = CodeEditor()

    assert not editor.is_highlight_current_line_enabled()
    editor.enable_highlight_current_line(True)
    assert editor.is_highlight_current_line_enabled()

    # populate text and place cursor to ensure a selection is created
    editor.setPlainText("one\n two")
    c = editor.textCursor()
    c.setPosition(0)
    editor.setTextCursor(c)
    # nudge the cursor to trigger the cursorPositionChanged signal
    c.setPosition(1)
    editor.setTextCursor(c)
    c.setPosition(0)
    editor.setTextCursor(c)

    extras = editor.extraSelections()
    assert len(extras) >= 1

    # disable and expect selections cleared
    editor.enable_highlight_current_line(False)
    assert not editor.is_highlight_current_line_enabled()
    assert editor.extraSelections() == []


def test_show_whitespace_toggle():
    _ensure_app()
    editor = CodeEditor()

    assert not editor.is_show_whitespace_enabled()
    editor.enable_show_whitespace(True)
    assert editor.is_show_whitespace_enabled()
    assert getattr(editor, "_whitespace_highlighter", None) is not None

    editor.enable_show_whitespace(False)
    assert not editor.is_show_whitespace_enabled()
    assert getattr(editor, "_whitespace_highlighter", None) is None
