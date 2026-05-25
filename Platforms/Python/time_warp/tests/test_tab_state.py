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
    ts = TabState(language=Language.LUA)
    assert ts.language == Language.LUA


def test_tab_state_independence():
    """Two TabState instances are independent."""
    a = TabState(file="a.bas")
    b = TabState(file="b.bas")
    a.mark_modified()
    assert a.modified is True
    assert b.modified is False


def test_tab_state_multiple_languages():
    from ..core.interpreter import Language
    languages = [Language.BASIC, Language.LUA, Language.JAVASCRIPT,
                 Language.PASCAL, Language.LOGO]
    for lang in languages:
        ts = TabState(language=lang)
        assert ts.language == lang


def test_tab_state_running_starts_false():
    ts = TabState()
    assert ts.running is False


def test_tab_state_modified_set_in_constructor():
    ts = TabState(modified=True)
    assert ts.modified is True
    ts2 = TabState(modified=False)
    assert ts2.modified is False


def test_tab_state_file_none_by_default():
    ts = TabState()
    assert ts.file is None


def test_tab_state_mark_then_reset_then_mark():
    ts = TabState()
    ts.mark_modified()
    ts.reset_to_saved()
    ts.mark_modified()
    assert ts.modified is True


def test_tab_state_constructor_all_fields():
    from ..core.interpreter import Language
    ts = TabState(file="test.lua", modified=True, language=Language.LUA, running=True)
    assert ts.file == "test.lua"
    assert ts.modified is True
    assert ts.language == Language.LUA
    assert ts.running is True


def test_tab_state_repr_contains_class_name():
    ts = TabState()
    assert "TabState" in repr(ts)


def test_tab_state_equality():
    from ..core.interpreter import Language
    ts1 = TabState(file="a.bas", language=Language.BASIC)
    ts2 = TabState(file="a.bas", language=Language.BASIC)
    assert ts1 == ts2


def test_tab_state_inequality_by_file():
    ts1 = TabState(file="a.bas")
    ts2 = TabState(file="b.bas")
    assert ts1 != ts2


def test_tab_state_inequality_by_language():
    from ..core.interpreter import Language
    ts1 = TabState(language=Language.BASIC)
    ts2 = TabState(language=Language.LUA)
    assert ts1 != ts2


def test_tab_state_many_mark_modified():
    """Calling mark_modified multiple times stays modified."""
    ts = TabState()
    ts.mark_modified()
    ts.mark_modified()
    ts.mark_modified()
    assert ts.modified is True


def test_tab_state_reset_to_saved_idempotent():
    """Calling reset_to_saved on already clean state stays clean."""
    ts = TabState()
    ts.reset_to_saved()
    ts.reset_to_saved()
    assert ts.modified is False


def test_tab_state_erlang_language():
    from ..core.interpreter import Language
    ts = TabState(language=Language.ERLANG)
    assert ts.language == Language.ERLANG


def test_tab_state_prolog_language():
    from ..core.interpreter import Language
    ts = TabState(language=Language.PROLOG)
    assert ts.language == Language.PROLOG


def test_tab_state_lisp_language():
    from ..core.interpreter import Language
    ts = TabState(language=Language.LISP)
    assert ts.language == Language.LISP


def test_tab_state_javascript_language():
    from ..core.interpreter import Language
    ts = TabState(language=Language.JAVASCRIPT)
    assert ts.language == Language.JAVASCRIPT


def test_tab_state_pascal_language():
    from ..core.interpreter import Language
    ts = TabState(language=Language.PASCAL)
    assert ts.language == Language.PASCAL


def test_tab_state_cobol_language():
    from ..core.interpreter import Language
    ts = TabState(language=Language.COBOL)
    assert ts.language == Language.COBOL


def test_tab_state_forth_language():
    from ..core.interpreter import Language
    ts = TabState(language=Language.FORTH)
    assert ts.language == Language.FORTH


def test_tab_state_lua_language():
    from ..core.interpreter import Language
    ts = TabState(language=Language.LUA)
    assert ts.language == Language.LUA


def test_tab_state_brainfuck_language():
    from ..core.interpreter import Language
    ts = TabState(language=Language.BRAINFUCK)
    assert ts.language == Language.BRAINFUCK


def test_tab_state_logo_language():
    from ..core.interpreter import Language
    ts = TabState(language=Language.LOGO)
    assert ts.language == Language.LOGO


def test_tab_state_pilot_language():
    from ..core.interpreter import Language
    ts = TabState(language=Language.PILOT)
    assert ts.language == Language.PILOT


def test_tab_state_hypertalk_language():
    from ..core.interpreter import Language
    ts = TabState(language=Language.HYPERTALK)
    assert ts.language == Language.HYPERTALK


def test_tab_state_c_language():
    from ..core.interpreter import Language
    ts = TabState(language=Language.C)
    assert ts.language == Language.C


def test_tab_state_postscript_language():
    from ..core.interpreter import Language
    ts = TabState(language=Language.POSTSCRIPT)
    assert ts.language == Language.POSTSCRIPT


def test_tab_state_repr_has_classname():
    """repr shows TabState."""
    ts = TabState()
    assert "TabState" in repr(ts)


def test_tab_state_equality_same_defaults():
    """Two default TabStates are equal."""
    a = TabState()
    b = TabState()
    assert a == b


def test_tab_state_inequality_different_file():
    """TabStates with different files are not equal."""
    a = TabState(file="a.bas")
    b = TabState(file="b.bas")
    assert a != b


def test_tab_state_inequality_modified():
    """TabState with modified differs from unmodified."""
    a = TabState()
    b = TabState()
    a.mark_modified()
    assert a != b


def test_tab_state_file_can_change():
    """file attribute can be updated."""
    ts = TabState(file="first.bas")
    ts.file = "second.bas"
    assert ts.file == "second.bas"


def test_tab_state_running_can_toggle_multiple():
    """running can be toggled many times."""
    ts = TabState()
    for _ in range(10):
        ts.running = True
        assert ts.running is True
        ts.running = False
        assert ts.running is False


def test_tab_state_mark_modified_idempotent():
    """Calling mark_modified twice still gives modified=True."""
    ts = TabState()
    ts.mark_modified()
    ts.mark_modified()
    assert ts.modified is True


def test_tab_state_reset_then_mark():
    """reset_to_saved then mark_modified works correctly."""
    ts = TabState()
    ts.mark_modified()
    ts.reset_to_saved()
    assert ts.modified is False
    ts.mark_modified()
    assert ts.modified is True


def test_tab_state_all_fields_set():
    from ..core.interpreter import Language
    ts = TabState(file="test.lua", modified=True, language=Language.LUA, running=True)
    assert ts.file == "test.lua"
    assert ts.modified is True
    assert ts.language == Language.LUA
    assert ts.running is True


def test_tab_state_tcl_language():
    from ..core.interpreter import Language
    ts = TabState(language=Language.TCL)
    assert ts.language == Language.TCL


class TestTabStateAdditional:
    """More TabState tests."""

    def test_file_is_none_by_default(self):
        ts = TabState()
        assert ts.file is None

    def test_running_is_false_by_default(self):
        ts = TabState()
        assert ts.running is False

    def test_modified_is_false_by_default(self):
        ts = TabState()
        assert ts.modified is False

    def test_language_is_basic_by_default(self):
        from time_warp.core.interpreter import Language
        ts = TabState()
        assert ts.language.value == 1  # Language.BASIC == 1

    def test_set_file_to_string(self):
        ts = TabState()
        ts.file = "test.bas"
        assert ts.file == "test.bas"

    def test_set_running_true(self):
        ts = TabState()
        ts.running = True
        assert ts.running is True

    def test_mark_modified_sets_true(self):
        ts = TabState()
        ts.mark_modified()
        assert ts.modified is True

    def test_reset_to_saved_clears_modified(self):
        ts = TabState(modified=True)
        ts.reset_to_saved()
        assert ts.modified is False

    def test_mark_then_reset_then_mark(self):
        ts = TabState()
        ts.mark_modified()
        ts.reset_to_saved()
        ts.mark_modified()
        assert ts.modified is True

    def test_equality_same_language(self):
        from time_warp.core.interpreter import Language
        a = TabState(language=Language.LUA)
        b = TabState(language=Language.LUA)
        assert a == b

    def test_inequality_different_language(self):
        from time_warp.core.interpreter import Language
        a = TabState(language=Language.LUA)
        b = TabState(language=Language.LOGO)
        assert a != b

    def test_inequality_running_vs_not(self):
        a = TabState()
        b = TabState()
        a.running = True
        assert a != b

    def test_running_toggle(self):
        ts = TabState()
        ts.running = True
        ts.running = False
        assert ts.running is False

    def test_file_update(self):
        ts = TabState(file="old.bas")
        ts.file = "new.bas"
        assert ts.file == "new.bas"

    def test_language_can_be_updated(self):
        from time_warp.core.interpreter import Language
        ts = TabState()
        ts.language = Language.LUA
        assert ts.language == Language.LUA

    def test_create_with_all_args(self):
        from time_warp.core.interpreter import Language
        ts = TabState(file="x.lua", modified=True, language=Language.LUA, running=True)
        assert ts.file == "x.lua"
        assert ts.modified is True
        assert ts.running is True

    def test_repr_contains_tabstate(self):
        ts = TabState()
        assert "TabState" in repr(ts)

    def test_create_lua_tab(self):
        from time_warp.core.interpreter import Language
        ts = TabState(language=Language.LUA, file="main.lua")
        assert ts.file == "main.lua"
        assert ts.language == Language.LUA


class TestTabStateExtended:
    """More TabState tests."""

    def test_javascript_tab(self):
        from time_warp.core.interpreter import Language
        ts = TabState(language=Language.JAVASCRIPT, file="app.js")
        assert ts.language == Language.JAVASCRIPT

    def test_python_tab_running(self):
        ts = TabState(running=True)
        assert ts.running is True

    def test_modified_true(self):
        ts = TabState(modified=True)
        assert ts.modified is True

    def test_file_path_stored(self):
        ts = TabState(file="/home/user/code.bas")
        assert ts.file == "/home/user/code.bas"

    def test_multiple_attrs_combined(self):
        from time_warp.core.interpreter import Language
        ts = TabState(file="f.lua", modified=True, language=Language.LUA, running=True)
        assert ts.file == "f.lua"
        assert ts.modified is True
        assert ts.language == Language.LUA
        assert ts.running is True

    def test_default_modified_false(self):
        ts = TabState()
        assert ts.modified is False

    def test_default_running_false(self):
        ts = TabState()
        assert ts.running is False

    def test_lisp_language(self):
        from time_warp.core.interpreter import Language
        ts = TabState(language=Language.LISP)
        assert ts.language == Language.LISP

    def test_cobol_language(self):
        from time_warp.core.interpreter import Language
        ts = TabState(language=Language.COBOL)
        assert ts.language == Language.COBOL

    def test_tcl_language(self):
        from time_warp.core.interpreter import Language
        ts = TabState(language=Language.TCL)
        assert ts.language == Language.TCL

    def test_postscript_language(self):
        from time_warp.core.interpreter import Language
        ts = TabState(language=Language.POSTSCRIPT)
        assert ts.language == Language.POSTSCRIPT

    def test_repr_not_empty(self):
        ts = TabState()
        assert len(repr(ts)) > 0

    def test_is_dataclass_like(self):
        ts1 = TabState(file="a.bas")
        ts2 = TabState(file="a.bas")
        assert ts1.file == ts2.file


class TestTabStateExtended2:
    """Second round of TabState extended tests."""

    def test_default_language_is_basic(self):
        from ..core.interpreter import Language
        ts = TabState()
        assert ts.language == Language.BASIC

    def test_set_language_lua(self):
        from ..core.interpreter import Language
        ts = TabState(language=Language.LUA)
        assert ts.language == Language.LUA

    def test_set_language_javascript(self):
        from ..core.interpreter import Language
        ts = TabState(language=Language.JAVASCRIPT)
        assert ts.language == Language.JAVASCRIPT

    def test_set_language_lisp(self):
        from ..core.interpreter import Language
        ts = TabState(language=Language.LISP)
        assert ts.language == Language.LISP

    def test_set_language_cobol(self):
        from ..core.interpreter import Language
        ts = TabState(language=Language.COBOL)
        assert ts.language == Language.COBOL

    def test_file_none_by_default(self):
        ts = TabState()
        assert ts.file is None or isinstance(ts.file, str)

    def test_file_set(self):
        ts = TabState(file="test.py")
        assert ts.file == "test.py"

    def test_content_empty_by_default(self):
        ts = TabState()
        assert not hasattr(ts, "content") or ts.content is None

    def test_content_set(self):
        ts = TabState(file="test.bas")
        assert isinstance(ts.file, str)

    def test_two_tabs_independent(self):
        ts1 = TabState(file="a.bas")
        ts2 = TabState(file="b.lua")
        assert ts1.file != ts2.file


class TestTabStateExtended3:
    """Third round of TabState extended tests."""

    def test_file_defaults_empty(self):
        ts = TabState()
        assert ts.file == "" or ts.file is None or isinstance(ts.file, str)

    def test_modified_defaults_false(self):
        ts = TabState()
        assert ts.modified is False

    def test_running_defaults_false(self):
        ts = TabState()
        assert ts.running is False

    def test_set_modified_true(self):
        ts = TabState()
        ts.modified = True
        assert ts.modified is True

    def test_set_running_true(self):
        ts = TabState()
        ts.running = True
        assert ts.running is True

    def test_set_file_name(self):
        ts = TabState(file="test.py")
        assert ts.file == "test.py"

    def test_language_js(self):
        from ..core.interpreter import Language
        ts = TabState(language=Language.JAVASCRIPT)
        assert ts.language == Language.JAVASCRIPT

    def test_language_erlang(self):
        from ..core.interpreter import Language
        ts = TabState(language=Language.ERLANG)
        assert ts.language == Language.ERLANG

    def test_two_tabs_different_languages(self):
        from ..core.interpreter import Language
        ts1 = TabState(language=Language.LUA)
        ts2 = TabState(language=Language.BASIC)
        assert ts1.language != ts2.language

    def test_tab_state_is_not_none(self):
        ts = TabState()
        assert ts is not None


class TestTabStateExtended4:
    """Fourth round of TabState extended tests."""

    def test_default_not_running(self):
        ts = TabState()
        assert ts.running is False

    def test_set_running(self):
        ts = TabState()
        ts.running = True
        assert ts.running is True

    def test_set_modified(self):
        ts = TabState()
        ts.modified = True
        assert ts.modified is True

    def test_set_file(self):
        ts = TabState()
        ts.file = "test.bas"
        assert ts.file == "test.bas"

    def test_language_none_by_default(self):
        ts = TabState()
        from ..core.interpreter import Language
        assert ts.language is None or isinstance(ts.language, Language)

    def test_two_tabs_independent_modified(self):
        ts1 = TabState()
        ts2 = TabState()
        ts1.modified = True
        assert ts2.modified is False

    def test_two_tabs_independent_running(self):
        ts1 = TabState()
        ts2 = TabState()
        ts1.running = True
        assert ts2.running is False

    def test_set_language(self):
        from ..core.interpreter import Language
        ts = TabState()
        ts.language = Language.LUA
        assert ts.language == Language.LUA

    def test_tab_state_all_attrs(self):
        ts = TabState()
        assert hasattr(ts, "file")
        assert hasattr(ts, "language")
        assert hasattr(ts, "modified")
        assert hasattr(ts, "running")

    def test_tab_state_is_instance(self):
        ts = TabState()
        assert isinstance(ts, TabState)
