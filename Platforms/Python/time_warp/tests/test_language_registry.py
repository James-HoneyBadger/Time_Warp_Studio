"""Tests for LanguageRegistry in core/interpreter.py."""

from __future__ import annotations

from ..core.interpreter import (
    Language,
    LanguageRegistry,
    _WHOLE_PROGRAM_EXECUTORS,
    language_registry,
)


def test_language_registry_languages():
    """languages() returns all whole-program language keys."""
    reg = LanguageRegistry()
    langs = reg.languages()
    assert len(langs) > 0
    assert Language.LUA in langs
    assert Language.JAVASCRIPT in langs


def test_language_registry_get_executor_whole():
    """get_executor returns a callable for known whole-program languages."""
    reg = LanguageRegistry()
    fn = reg.get_executor(Language.LUA)
    assert fn is not None
    assert callable(fn)


def test_language_registry_get_executor_line_by_line():
    """get_executor returns None for line-by-line languages (e.g. BASIC)."""
    reg = LanguageRegistry()
    fn = reg.get_executor(Language.BASIC)
    assert fn is None


def test_language_registry_is_whole_program():
    reg = LanguageRegistry()
    assert reg.is_whole_program(Language.LUA) is True
    assert reg.is_whole_program(Language.BASIC) is False


def test_language_registry_register():
    """register() adds a new executor and syncs module-level dict."""
    reg = LanguageRegistry()

    def dummy_executor(interp, source, turtle):
        return "ok\n"

    # Use a language not already in the registry; fall back to one that is
    # (BASIC is line-by-line so it should not be in _WHOLE_PROGRAM_EXECUTORS)
    reg.register(Language.BASIC, dummy_executor)
    assert reg.get_executor(Language.BASIC) is dummy_executor
    assert _WHOLE_PROGRAM_EXECUTORS[Language.BASIC] is dummy_executor

    # Cleanup — remove the test entry to avoid side-effects in other tests
    del _WHOLE_PROGRAM_EXECUTORS[Language.BASIC]


def test_module_level_singleton():
    """language_registry module singleton is a LanguageRegistry instance."""
    assert isinstance(language_registry, LanguageRegistry)
    assert Language.LUA in language_registry.languages()


def test_all_whole_program_languages_present():
    """All whole-program languages are registered."""
    reg = LanguageRegistry()
    langs = reg.languages()
    expected = [
        Language.LUA, Language.BRAINFUCK, Language.JAVASCRIPT,
        Language.HYPERTALK, Language.ERLANG, Language.LISP,
        Language.COBOL, Language.TCL, Language.POSTSCRIPT,
    ]
    for lang in expected:
        assert lang in langs, f"{lang} not in registry"


def test_line_by_line_languages_not_in_registry():
    """Line-by-line languages (BASIC, LOGO, etc.) are not whole-program."""
    reg = LanguageRegistry()
    for lang in [Language.BASIC, Language.LOGO, Language.PASCAL]:
        assert not reg.is_whole_program(lang), f"{lang} should not be whole-program"


def test_get_executor_is_callable():
    """get_executor returns callable for each whole-program language."""
    reg = LanguageRegistry()
    for lang in reg.languages():
        fn = reg.get_executor(lang)
        assert callable(fn), f"Executor for {lang} is not callable"


def test_registry_count():
    """LanguageRegistry contains whole-program executors."""
    reg = LanguageRegistry()
    assert len(reg.languages()) >= 13


def test_whole_program_executors_dict_populated():
    """_WHOLE_PROGRAM_EXECUTORS dict is non-empty."""
    assert len(_WHOLE_PROGRAM_EXECUTORS) > 0
    for key, val in _WHOLE_PROGRAM_EXECUTORS.items():
        assert isinstance(key, Language)
        assert callable(val)


def test_language_registry_get_executor_brainfuck():
    """get_executor returns callable for BRAINFUCK."""
    reg = LanguageRegistry()
    fn = reg.get_executor(Language.BRAINFUCK)
    assert fn is not None
    assert callable(fn)


def test_language_registry_get_executor_erlang():
    """get_executor returns callable for ERLANG."""
    reg = LanguageRegistry()
    fn = reg.get_executor(Language.ERLANG)
    assert fn is not None
    assert callable(fn)


def test_language_registry_get_executor_lisp():
    """get_executor returns callable for LISP."""
    reg = LanguageRegistry()
    fn = reg.get_executor(Language.LISP)
    assert fn is not None
    assert callable(fn)


def test_language_registry_get_executor_cobol():
    """get_executor returns callable for COBOL."""
    reg = LanguageRegistry()
    fn = reg.get_executor(Language.COBOL)
    assert fn is not None
    assert callable(fn)


def test_language_registry_get_executor_tcl():
    """get_executor returns callable for TCL."""
    reg = LanguageRegistry()
    fn = reg.get_executor(Language.TCL)
    assert fn is not None
    assert callable(fn)


def test_language_registry_get_executor_postscript():
    """get_executor returns callable for POSTSCRIPT."""
    reg = LanguageRegistry()
    fn = reg.get_executor(Language.POSTSCRIPT)
    assert fn is not None
    assert callable(fn)


def test_language_registry_get_executor_hypertalk():
    """get_executor returns callable for HYPERTALK."""
    reg = LanguageRegistry()
    fn = reg.get_executor(Language.HYPERTALK)
    assert fn is not None
    assert callable(fn)


def test_language_registry_languages_returns_list():
    """languages() returns a list."""
    reg = LanguageRegistry()
    langs = reg.languages()
    assert isinstance(langs, list)


def test_line_by_line_pilot_not_in_registry():
    """PILOT is a line-by-line language and not in whole-program registry."""
    reg = LanguageRegistry()
    assert not reg.is_whole_program(Language.PILOT)


def test_line_by_line_prolog_not_in_registry():
    """PROLOG is a line-by-line language and not in whole-program registry."""
    reg = LanguageRegistry()
    assert not reg.is_whole_program(Language.PROLOG)


def test_line_by_line_forth_not_in_registry():
    """FORTH is a line-by-line language and not in whole-program registry."""
    reg = LanguageRegistry()
    assert not reg.is_whole_program(Language.FORTH)


def test_register_custom_executor_replaces():
    """register() can replace an existing executor."""
    reg = LanguageRegistry()
    original = reg.get_executor(Language.LUA)

    def custom(interp, source, turtle):
        return "custom\n"

    reg.register(Language.LUA, custom)
    assert reg.get_executor(Language.LUA) is custom

    # Restore original
    reg.register(Language.LUA, original)
    assert reg.get_executor(Language.LUA) is original


def test_language_registry_get_executor_javascript():
    """get_executor returns callable for JAVASCRIPT."""
    reg = LanguageRegistry()
    fn = reg.get_executor(Language.JAVASCRIPT)
    assert fn is not None and callable(fn)


def test_language_registry_all_executors_callable():
    """Every executor in the registry is callable."""
    reg = LanguageRegistry()
    for lang in reg.languages():
        fn = reg.get_executor(lang)
        assert callable(fn)


def test_language_registry_is_whole_program_all():
    """is_whole_program returns True for each registered language."""
    reg = LanguageRegistry()
    for lang in reg.languages():
        assert reg.is_whole_program(lang)


def test_language_registry_line_by_line_c():
    """C is a line-by-line language, not whole-program."""
    reg = LanguageRegistry()
    assert not reg.is_whole_program(Language.C)


def test_language_registry_line_by_line_pascal():
    """PASCAL is a line-by-line language, not whole-program."""
    reg = LanguageRegistry()
    assert not reg.is_whole_program(Language.PASCAL)


def test_language_registry_line_by_line_logo():
    """LOGO is a line-by-line language, not whole-program."""
    reg = LanguageRegistry()
    assert not reg.is_whole_program(Language.LOGO)


def test_language_registry_no_duplicates():
    """languages() returns unique entries."""
    reg = LanguageRegistry()
    langs = reg.languages()
    assert len(langs) == len(set(langs))


def test_language_registry_get_executor_none_for_unknown():
    """get_executor returns None for non-whole-program language."""
    reg = LanguageRegistry()
    fn = reg.get_executor(Language.FORTH)
    assert fn is None


def test_language_registry_register_and_verify():
    """register() makes the executor retrievable."""
    reg = LanguageRegistry()
    original = reg.get_executor(Language.BRAINFUCK)
    sentinel = lambda i, s, t: "test\n"
    reg.register(Language.BRAINFUCK, sentinel)
    assert reg.get_executor(Language.BRAINFUCK) is sentinel
    reg.register(Language.BRAINFUCK, original)


def test_whole_program_executors_includes_lisp():
    """_WHOLE_PROGRAM_EXECUTORS includes LISP."""
    assert Language.LISP in _WHOLE_PROGRAM_EXECUTORS


def test_whole_program_executors_includes_cobol():
    """_WHOLE_PROGRAM_EXECUTORS includes COBOL."""
    assert Language.COBOL in _WHOLE_PROGRAM_EXECUTORS


def test_whole_program_executors_includes_tcl():
    """_WHOLE_PROGRAM_EXECUTORS includes TCL."""
    assert Language.TCL in _WHOLE_PROGRAM_EXECUTORS


def test_whole_program_executors_includes_postscript():
    """_WHOLE_PROGRAM_EXECUTORS includes POSTSCRIPT."""
    assert Language.POSTSCRIPT in _WHOLE_PROGRAM_EXECUTORS


class TestLanguageValues:
    """Tests for Language enum values."""

    def test_basic_value(self):
        assert Language.BASIC.value == 1

    def test_pilot_value(self):
        assert Language.PILOT.value == 2

    def test_logo_value(self):
        assert Language.LOGO.value == 3

    def test_c_value(self):
        assert Language.C.value == 4

    def test_prolog_value(self):
        assert Language.PROLOG.value == 5

    def test_pascal_value(self):
        assert Language.PASCAL.value == 6

    def test_forth_value(self):
        assert Language.FORTH.value == 7

    def test_lua_value(self):
        assert Language.LUA.value == 8

    def test_brainfuck_value(self):
        assert Language.BRAINFUCK.value == 9

    def test_javascript_value(self):
        assert Language.JAVASCRIPT.value == 10

    def test_hypertalk_value(self):
        assert Language.HYPERTALK.value == 11

    def test_erlang_value(self):
        assert Language.ERLANG.value == 12

    def test_lisp_value(self):
        assert Language.LISP.value == 13

    def test_cobol_value(self):
        assert Language.COBOL.value == 14

    def test_tcl_value(self):
        assert Language.TCL.value == 15

    def test_postscript_value(self):
        assert Language.POSTSCRIPT.value == 16

    def test_total_languages_20(self):
        assert len(list(Language)) == 20


class TestLanguageExtensions:
    """Tests for Language.from_extension mapping."""

    def test_bas(self):
        assert Language.from_extension(".bas") == Language.BASIC

    def test_b(self):
        assert Language.from_extension(".b") == Language.BASIC

    def test_lua(self):
        assert Language.from_extension(".lua") == Language.LUA

    def test_js(self):
        assert Language.from_extension(".js") == Language.JAVASCRIPT

    def test_erl(self):
        assert Language.from_extension(".erl") == Language.ERLANG

    def test_pl_prolog(self):
        assert Language.from_extension(".pl") == Language.PROLOG

    def test_pro(self):
        assert Language.from_extension(".pro") == Language.PROLOG

    def test_pas(self):
        assert Language.from_extension(".pas") == Language.PASCAL

    def test_c(self):
        assert Language.from_extension(".c") == Language.C

    def test_logo(self):
        assert Language.from_extension(".logo") == Language.LOGO

    def test_f_forth(self):
        assert Language.from_extension(".f") == Language.FORTH

    def test_bf(self):
        assert Language.from_extension(".bf") == Language.BRAINFUCK

    def test_htalk(self):
        assert Language.from_extension(".htalk") == Language.HYPERTALK

    def test_scm(self):
        assert Language.from_extension(".scm") == Language.LISP

    def test_lisp(self):
        assert Language.from_extension(".lisp") == Language.LISP

    def test_cob(self):
        assert Language.from_extension(".cob") == Language.COBOL

    def test_tcl(self):
        assert Language.from_extension(".tcl") == Language.TCL

    def test_ps(self):
        assert Language.from_extension(".ps") == Language.POSTSCRIPT

    def test_pilot(self):
        assert Language.from_extension(".pilot") == Language.PILOT


class TestWholeProgramExecutors:
    """Tests for _WHOLE_PROGRAM_EXECUTORS contents and callable."""

    def test_9_whole_program_langs(self):
        assert len(_WHOLE_PROGRAM_EXECUTORS) >= 13

    def test_basic_not_whole(self):
        assert Language.BASIC not in _WHOLE_PROGRAM_EXECUTORS

    def test_logo_not_whole(self):
        assert Language.LOGO not in _WHOLE_PROGRAM_EXECUTORS

    def test_pascal_not_whole(self):
        assert Language.PASCAL not in _WHOLE_PROGRAM_EXECUTORS

    def test_prolog_not_whole(self):
        assert Language.PROLOG not in _WHOLE_PROGRAM_EXECUTORS

    def test_forth_not_whole(self):
        assert Language.FORTH not in _WHOLE_PROGRAM_EXECUTORS

    def test_c_not_whole(self):
        assert Language.C not in _WHOLE_PROGRAM_EXECUTORS

    def test_pilot_not_whole(self):
        assert Language.PILOT not in _WHOLE_PROGRAM_EXECUTORS

    def test_all_executors_callable(self):
        for lang, fn in _WHOLE_PROGRAM_EXECUTORS.items():
            assert callable(fn), f"{lang} executor is not callable"

    def test_registry_is_whole_program_lua(self):
        reg = LanguageRegistry()
        assert reg.is_whole_program(Language.LUA) is True

    def test_registry_is_whole_program_basic_false(self):
        reg = LanguageRegistry()
        assert reg.is_whole_program(Language.BASIC) is False

    def test_registry_languages_returns_list(self):
        reg = LanguageRegistry()
        langs = reg.languages()
        assert isinstance(langs, list)

    def test_registry_languages_count(self):
        reg = LanguageRegistry()
        langs = reg.languages()
        assert len(langs) >= 13


class TestWholeProgramExecutorsExtended:
    """Extended tests for whole-program executor registry."""

    def test_lua_is_whole(self):
        from ..core.interpreter import _WHOLE_PROGRAM_EXECUTORS, Language
        assert Language.LUA in _WHOLE_PROGRAM_EXECUTORS

    def test_brainfuck_is_whole(self):
        from ..core.interpreter import _WHOLE_PROGRAM_EXECUTORS, Language
        assert Language.BRAINFUCK in _WHOLE_PROGRAM_EXECUTORS

    def test_javascript_is_whole(self):
        from ..core.interpreter import _WHOLE_PROGRAM_EXECUTORS, Language
        assert Language.JAVASCRIPT in _WHOLE_PROGRAM_EXECUTORS

    def test_hypertalk_is_whole(self):
        from ..core.interpreter import _WHOLE_PROGRAM_EXECUTORS, Language
        assert Language.HYPERTALK in _WHOLE_PROGRAM_EXECUTORS

    def test_erlang_is_whole(self):
        from ..core.interpreter import _WHOLE_PROGRAM_EXECUTORS, Language
        assert Language.ERLANG in _WHOLE_PROGRAM_EXECUTORS

    def test_lisp_is_whole(self):
        from ..core.interpreter import _WHOLE_PROGRAM_EXECUTORS, Language
        assert Language.LISP in _WHOLE_PROGRAM_EXECUTORS

    def test_all_whole_are_callable(self):
        from ..core.interpreter import _WHOLE_PROGRAM_EXECUTORS
        for lang, fn in _WHOLE_PROGRAM_EXECUTORS.items():
            assert callable(fn), f"{lang} executor not callable"

    def test_c_not_whole(self):
        from ..core.interpreter import _WHOLE_PROGRAM_EXECUTORS, Language
        assert Language.C not in _WHOLE_PROGRAM_EXECUTORS

    def test_pascal_not_whole(self):
        from ..core.interpreter import _WHOLE_PROGRAM_EXECUTORS, Language
        assert Language.PASCAL not in _WHOLE_PROGRAM_EXECUTORS

    def test_prolog_not_whole(self):
        from ..core.interpreter import _WHOLE_PROGRAM_EXECUTORS, Language
        assert Language.PROLOG not in _WHOLE_PROGRAM_EXECUTORS


class TestWholeProgramExecutorsExtended2:
    """Second round of whole-program executor tests."""

    def test_basic_not_whole(self):
        from ..core.interpreter import _WHOLE_PROGRAM_EXECUTORS, Language
        assert Language.BASIC not in _WHOLE_PROGRAM_EXECUTORS

    def test_logo_not_whole(self):
        from ..core.interpreter import _WHOLE_PROGRAM_EXECUTORS, Language
        assert Language.LOGO not in _WHOLE_PROGRAM_EXECUTORS

    def test_forth_not_whole(self):
        from ..core.interpreter import _WHOLE_PROGRAM_EXECUTORS, Language
        assert Language.FORTH not in _WHOLE_PROGRAM_EXECUTORS

    def test_pilot_not_whole(self):
        from ..core.interpreter import _WHOLE_PROGRAM_EXECUTORS, Language
        assert Language.PILOT not in _WHOLE_PROGRAM_EXECUTORS

    def test_cobol_is_whole(self):
        from ..core.interpreter import _WHOLE_PROGRAM_EXECUTORS, Language
        assert Language.COBOL in _WHOLE_PROGRAM_EXECUTORS

    def test_tcl_is_whole(self):
        from ..core.interpreter import _WHOLE_PROGRAM_EXECUTORS, Language
        assert Language.TCL in _WHOLE_PROGRAM_EXECUTORS

    def test_postscript_is_whole(self):
        from ..core.interpreter import _WHOLE_PROGRAM_EXECUTORS, Language
        assert Language.POSTSCRIPT in _WHOLE_PROGRAM_EXECUTORS

    def test_whole_executors_count_gte_nine(self):
        from ..core.interpreter import _WHOLE_PROGRAM_EXECUTORS
        assert len(_WHOLE_PROGRAM_EXECUTORS) >= 9

    def test_whole_executors_is_dict(self):
        from ..core.interpreter import _WHOLE_PROGRAM_EXECUTORS
        assert isinstance(_WHOLE_PROGRAM_EXECUTORS, dict)

    def test_lisp_is_whole(self):
        from ..core.interpreter import _WHOLE_PROGRAM_EXECUTORS, Language
        assert Language.LISP in _WHOLE_PROGRAM_EXECUTORS
