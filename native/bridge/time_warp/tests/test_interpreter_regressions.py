"""Regression tests for interpreter glitches fixed in the professional overhaul."""

from __future__ import annotations

from time_warp.core.interpreter import Language

from .conftest_lang import no_errors, run


def test_pilot_remark_inside_subroutine_does_not_return():
    out = run("U:*S\nT:after\nE:\n*S\nR: note\nT:in sub\nE:\n", Language.PILOT)
    assert no_errors(out)
    assert out == ["in sub", "after"]


def test_division_by_zero_is_reported_in_every_language():
    cases = {
        Language.BASIC: "10 PRINT 5 / 0\n",
        Language.PILOT: "C:X = 5 / 0\n",
        Language.LOGO: "PRINT 5 / 0\n",
        Language.REXX: "SAY 5 / 0\n",
        Language.SCHEME: "(display (/ 5 0))\n",
    }
    for lang, src in cases.items():
        out = "\n".join(run(src, lang))
        assert "❌" in out and "ivision by zero" in out, (lang, out)


def test_comal_block_structure_is_balanced():
    out = run(
        "10 X := 0\n20 FOR I := 1 TO 3\n30 X := X + I\n40 NEXT I\n"
        '50 IF X > 5 THEN\n60 PRINT "big"\n70 ELSE\n80 PRINT "small"\n90 ENDIF\n',
        Language.COMAL,
    )
    assert out == ["big"]


def test_rexx_select_skips_non_matching_whens():
    out = run(
        "x = 42\nSELECT\n  WHEN x > 100 THEN SAY 'huge'\n"
        "  WHEN x > 10 THEN SAY 'big'\n  OTHERWISE SAY 'small'\nEND\n",
        Language.REXX,
    )
    assert out == ["big"]


def test_scheme_quote_and_string_output_are_distinct():
    assert run('(display "(a b)")\n(newline)\n', Language.SCHEME) == ["(a b)"]
    assert run("(display '(a b))\n(newline)\n", Language.SCHEME) == ["(a b)"]
