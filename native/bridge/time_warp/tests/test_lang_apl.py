"""APL executor tests.

The tests that matter most here are the ones about the two things that make APL
APL: evaluation is right to left with no precedence, and a function's argument is
the whole adjacent strand of values. Almost every wrong APL implementation is
wrong about one of those, so they are asserted directly rather than inferred from
a bigger program.
"""

from __future__ import annotations

from time_warp.core.interpreter import Language

from .conftest_lang import has, no_errors, run

APL = Language.APL


def text(source: str) -> str:
    return "\n".join(run(source, APL))


# -- arrays ------------------------------------------------------------------


def test_iota_counts_from_one() -> None:
    assert text("⍳5").strip() == "1 2 3 4 5"


def test_keyword_spelling_works_like_the_glyph() -> None:
    assert text("IOTA 5").strip() == "1 2 3 4 5"


def test_reshape_makes_a_table() -> None:
    assert text("2 3 ⍴ ⍳6").strip() == "1 2 3\n4 5 6"


def test_reshape_repeats_its_data_cyclically() -> None:
    assert text("3 ⍴ 1 2").strip() == "1 2 1"


def test_reverse_and_rotate() -> None:
    assert text("⌽ 1 2 3").strip() == "3 2 1"
    assert text("1 ⌽ 1 2 3").strip() == "2 3 1"


def test_take_pads_with_zero_and_drop_removes() -> None:
    assert text("3 ↑ 1 2").strip() == "1 2 0"
    assert text("1 ↓ 1 2 3").strip() == "2 3"


def test_transpose_swaps_the_axes() -> None:
    assert text("⍉ 2 3 ⍴ ⍳6").strip() == "1 4\n2 5\n3 6"


def test_catenate_joins_values() -> None:
    assert text("1 2 , 3 4").strip() == "1 2 3 4"


def test_index_of_finds_positions() -> None:
    assert text("1 2 3 ⍳ 2").strip() == "2"


def test_grade_up_returns_positions_not_values() -> None:
    assert text("⍋ 3 1 2").strip() == "2 3 1"


# -- the two defining rules --------------------------------------------------


def test_a_left_argument_is_the_whole_strand() -> None:
    """`1 2 3 + 10` is `11 12 13`, not `1 2 13`."""
    assert text("1 2 3 + 10").strip() == "11 12 13"
    assert text("2 3 + 4").strip() == "6 7"


def test_scalar_functions_are_pervasive() -> None:
    assert text("1 2 3 × 2").strip() == "2 4 6"
    assert text("10 + 1 2 3").strip() == "11 12 13"


def test_mismatched_lengths_are_an_error() -> None:
    output = run("1 2 + 4 5 6", APL)
    assert has(output, "❌", "length error")


def test_there_is_no_precedence() -> None:
    """`2 × 3 + 4` is right to left: 2×7, not (2×3)+4."""
    assert text("2 × 3 + 4").strip() == "14"


def test_parentheses_override_the_order() -> None:
    assert text("(2 × 3) + 4").strip() == "10"


# -- operators ---------------------------------------------------------------


def test_reduce_folds_a_vector() -> None:
    assert text("+/⍳100").strip() == "5050"


def test_scan_returns_the_running_values() -> None:
    assert text("+\\1 2 3 4").strip() == "1 3 6 10"


def test_inner_product() -> None:
    assert text("+/ 1 2 3 × 4 5 6").strip() == "32"


# -- variables and functions -------------------------------------------------


def test_assignment_is_silent_and_the_name_reads_back() -> None:
    output = run("A ← ⍳4", APL)
    assert output == [], output
    assert text("A ← ⍳4\nA").strip() == "1 2 3 4"


def test_a_single_letter_name_must_have_a_value() -> None:
    output = run("FOO", APL)
    assert has(output, "❌", "has no value")


def test_a_monadic_user_function() -> None:
    output = run("∇ R ← DOUBLE X\n  R ← X × 2\n∇", APL)
    assert has(output, "DOUBLE defined")
    assert text("∇ R ← DOUBLE X\n  R ← X × 2\n∇\nDOUBLE 21").strip().endswith("42")


def test_a_dyadic_user_function() -> None:
    source = "∇ R ← X ADD Y\n  R ← X + Y\n∇\n3 ADD 4"
    assert text(source).strip().endswith("7")


def test_a_function_does_not_leak_into_the_caller() -> None:
    """A parameter is local: X inside the function is not the caller's X."""
    source = "X ← 100\n∇ R ← FOOLOCAL Y\n  R ← X\n∇\nFOOLOCAL 1"
    assert text(source).strip().endswith("100")


def test_a_definition_without_a_body_is_reported() -> None:
    output = run("∇ R ← EMPTY X\n∇", APL)
    assert has(output, "❌", "needs a body")


# -- turtle ------------------------------------------------------------------


def test_turtle_movement_is_silent() -> None:
    output = run("FD 100\nRT 90\nPU\nPD\nHOME\nCS", APL)
    assert output == [], output


def test_turtle_position_and_heading_are_readable() -> None:
    assert text("TX").strip() == "0"
    assert text("THETA").strip() == "0"


def test_x_is_an_ordinary_variable() -> None:
    """The turtle's names are two letters so a program can still use X."""
    assert text("X ← 5\nX × 2").strip().endswith("10")


def test_turtle_without_an_argument_is_an_error() -> None:
    output = run("FD", APL)
    assert has(output, "❌")


# -- errors ------------------------------------------------------------------


def test_unmatched_parenthesis() -> None:
    assert has(run("(⍳3", APL), "❌", "unmatched")


def test_missing_operand() -> None:
    assert has(run("1 2 3 +", APL), "❌", "operand is missing")


def test_division_by_zero() -> None:
    assert has(run("1 ÷ 0", APL), "❌", "division by zero")


def test_unknown_glyph_is_refused_by_name() -> None:
    assert has(run("1 ∘ 2", APL), "❌")


def test_a_comment_line_produces_nothing() -> None:
    output = run("⍝ this is a comment", APL)
    assert output == [], output


def test_a_good_line_produces_no_error() -> None:
    assert no_errors(run("+/⍳10", APL))
