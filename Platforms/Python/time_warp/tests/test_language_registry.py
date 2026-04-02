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
