"""Targeted command/literal regressions for Ruby executor."""

from __future__ import annotations

from time_warp.core.interpreter import Language
from .conftest_lang import run, has, no_errors

LANG = Language.RUBY


def rb(source: str) -> list[str]:
    return run(source, LANG)


def test_percent_w_word_array_literal():
    out = rb("words = %w[banana apple cherry]\nputs words.inspect")
    assert no_errors(out)
    assert has(out, "banana", "apple", "cherry")


def test_sort_by_symbol_to_proc_shorthand():
    out = rb(
        "words = %w[banana apple fig]\n"
        "puts words.sort_by(&:length).inspect"
    )
    assert no_errors(out)
    assert has(out, '"fig"', '"apple"', '"banana"')


def test_map_symbol_to_proc_shorthand():
    out = rb('puts ["a", "b"].map(&:upcase).inspect')
    assert no_errors(out)
    assert has(out, '"A"', '"B"')
