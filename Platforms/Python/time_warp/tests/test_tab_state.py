"""Tests for the TabState dataclass (ui/tab_state.py)."""

from __future__ import annotations

from ..ui.tab_state import TabState


def test_tab_state_defaults():
    """TabState is created with sensible defaults."""
    ts = TabState()
    assert ts.file is None
    assert ts.modified is False
    assert ts.running is False


def test_tab_state_default_language():
    """Default language is BASIC."""
    from ..core.interpreter import Language
    ts = TabState()
    assert ts.language == Language.BASIC


def test_tab_state_mark_modified():
    ts = TabState()
    ts.mark_modified()
    assert ts.modified is True


def test_tab_state_reset_to_saved():
    ts = TabState(modified=True)
    ts.reset_to_saved()
    assert ts.modified is False


def test_tab_state_with_file():
    ts = TabState(file="/tmp/hello.bas", modified=False)
    assert ts.file == "/tmp/hello.bas"
    ts.mark_modified()
    assert ts.modified is True
    ts.reset_to_saved()
    assert ts.modified is False
    assert ts.file == "/tmp/hello.bas"  # file path preserved


def test_tab_state_running_flag():
    ts = TabState()
    ts.running = True
    assert ts.running is True
    ts.running = False
    assert ts.running is False


def test_tab_state_language_field():
    from ..core.interpreter import Language
    ts = TabState(language=Language.PYTHON)
    assert ts.language == Language.PYTHON


def test_tab_state_independence():
    """Two TabState instances are independent."""
    a = TabState(file="a.bas")
    b = TabState(file="b.bas")
    a.mark_modified()
    assert a.modified is True
    assert b.modified is False
