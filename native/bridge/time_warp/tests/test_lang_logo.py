"""Comprehensive tests for the Logo executor."""

from __future__ import annotations

from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.LOGO


def logo(source: str) -> list[str]:
    return run(source, LANG)


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------


def test_hello_world():
    out = logo('PRINT "Hello')
    assert no_errors(out)
    assert out == ["Hello"]


def test_print_number():
    out = logo("PRINT 42")
    assert no_errors(out)
    assert has(out, "42")


def test_print_expression():
    # Logo does not evaluate inline arithmetic in PRINT; use MAKE to compute
    out = logo('MAKE "R 7\nPRINT :R')
    assert no_errors(out)
    assert has(out, "7")


# ---------------------------------------------------------------------------
# Variables (MAKE / :var)
# ---------------------------------------------------------------------------


def test_make_variable():
    out = logo('MAKE "X 10\nPRINT :X')
    assert no_errors(out)
    assert has(out, "10")


def test_variable_arithmetic():
    # Arithmetic must be computed via MAKE then printed
    out = logo('MAKE "A 6\nMAKE "B 7\nMAKE "C :A * :B\nPRINT :C')
    assert no_errors(out)
    assert has(out, "42")


def test_string_variable():
    out = logo('MAKE "NAME "World\nPRINT :NAME')
    assert no_errors(out)
    assert out == ["World"]


def test_print_evaluates_arithmetic_and_lists():
    assert logo("PRINT 3 + 4") == ["7"]
    assert logo('MAKE "X 5\nPRINT :X * 2') == ["10"]
    assert logo("PRINT (2 + 3) * 4") == ["20"]
    assert logo("PRINT [hello world]") == ["hello world"]
    assert logo('MAKE "L [a b c]\nPRINT :L') == ["a b c"]
    assert logo("PRINT SUM 2 3") == ["5"]
    assert "Division by zero" in logo("PRINT 5 / 0")[0]


# ---------------------------------------------------------------------------
# Turtle graphics (basic — just check no errors)
# ---------------------------------------------------------------------------


def test_forward():
    out = logo("FORWARD 100")
    assert no_errors(out)


def test_right_turn():
    out = logo("RIGHT 90")
    assert no_errors(out)


def test_penup_pendown():
    out = logo("PENUP\nFORWARD 50\nPENDOWN\nFORWARD 50")
    assert no_errors(out)


def test_home():
    out = logo("FORWARD 50\nHOME")
    assert no_errors(out)


def test_clearscreen():
    out = logo("CLEARSCREEN")
    assert no_errors(out)


def test_repeat():
    out = logo("REPEAT 4 [FORWARD 50 RIGHT 90]")
    assert no_errors(out)


# ---------------------------------------------------------------------------
# Procedures
# ---------------------------------------------------------------------------


def test_to_procedure():
    out = logo(
        "TO SQUARE :S\n"
        "  REPEAT 4 [FORWARD :S RIGHT 90]\n"
        "END\n"
        "SQUARE 50"
    )
    assert no_errors(out)


# ---------------------------------------------------------------------------
# String functions
# ---------------------------------------------------------------------------


def test_first_last():
    # FIRST returns first letter, LAST returns last letter (uppercased)
    out = logo('PRINT FIRST "Hello\nPRINT LAST "Hello')
    assert no_errors(out)
    assert has(out, "H", "O")


def test_if_condition():
    out = logo('MAKE "X 10\nIF :X > 5 [PRINT 1]')
    assert no_errors(out)
    assert has(out, "1")


def test_ifelse_true():
    out = logo('MAKE "N 8\nIFELSE :N > 5 [PRINT 1] [PRINT 0]')
    assert no_errors(out)
    assert has(out, "1")


def test_ifelse_false():
    out = logo('MAKE "N 2\nIFELSE :N > 5 [PRINT 1] [PRINT 0]')
    assert no_errors(out)
    assert has(out, "0")


def test_make_variable_float():
    out = logo('make "x 5\nprint :x')
    assert no_errors(out)
    assert has(out, "5")


def test_word_concat():
    out = logo('print word "hello "world')
    assert no_errors(out)
    assert has(out, "HELLOWORLD")


def test_repeat_accumulate():
    out = logo('make "total 0\nrepeat 3 [make "total :total + 10]\nprint :total')
    assert no_errors(out)
    assert has(out, "30")


def test_sum_builtin():
    out = logo('print sum 2 3')
    assert no_errors(out)
    assert has(out, "5")


def test_product_builtin():
    out = logo('print product 4 5')
    assert no_errors(out)
    assert has(out, "20")


def test_difference_builtin():
    out = logo('print difference 10 3')
    assert no_errors(out)
    assert has(out, "7")


def test_product():
    out = logo('print product 6 7')
    assert no_errors(out)
    assert has(out, "42")


def test_quotient():
    out = logo('print quotient 12 3')
    assert no_errors(out)
    assert has(out, "4")


def test_sum_large():
    out = logo('print sum 100 200')
    assert no_errors(out)
    assert has(out, "300")


# ---------------------------------------------------------------------------
# Example programs
# ---------------------------------------------------------------------------


def test_hello_example():
    import pathlib
    src = (
        pathlib.Path(__file__).parents[4] / "Examples" / "logo" / "hello.logo"
    ).read_text()
    out = logo(src)
    assert no_errors(out)
    assert ok(out)


# ---------------------------------------------------------------------------
# Calls inside blocks
# ---------------------------------------------------------------------------


def _strokes(source: str) -> int:
    """Count the line segments a Logo program draws."""
    from time_warp.core.interpreter import Interpreter
    from time_warp.graphics.turtle_state import TurtleState

    interpreter = Interpreter()
    turtle = TurtleState()
    interpreter.load_program(source, Language.LOGO)
    interpreter.execute(turtle)
    return len(turtle.lines)


# A block body is re-split with its line structure flattened, so a call that
# follows another call used to be swallowed into it and never ran.  Nothing was
# drawn and nothing was reported: mandala.logo lost every one of its petals.
PEN_AND_SQUARE = """TO REDPEN
  MAKE "COLORS ["red "green]
  SETPENCOLOR ITEM 1 :COLORS
END
TO SQUARE :SIDE
  REPEAT 4 [ FD :SIDE RT 90 ]
END
"""


def test_a_block_runs_every_call_in_its_body():
    assert _strokes(PEN_AND_SQUARE + "REPEAT 2 [ REDPEN SQUARE 10 ]") == 8


def test_a_block_runs_every_call_over_several_lines():
    source = PEN_AND_SQUARE + "REPEAT 2 [\n  REDPEN\n  SQUARE 10\n]\n"
    assert _strokes(source) == 8


def test_a_call_after_a_call_in_a_procedure_body_runs():
    source = PEN_AND_SQUARE + """TO DRAW :N
  REPEAT :N [
    REDPEN
    SQUARE 10
    RT 45
  ]
END
DRAW 3
"""
    assert _strokes(source) == 12


def test_a_procedure_still_receives_its_arguments():
    # The split must not fire early and rob the leading call of an argument.
    assert _strokes(PEN_AND_SQUARE + "SQUARE 12") == 4
    assert _strokes(PEN_AND_SQUARE + "REPEAT 3 [ SQUARE 12 ]") == 12


def test_a_call_after_an_argumentless_command_runs():
    # Commands that take nothing cannot be holding the next word as an
    # argument, so the call after them is a statement of its own.  The star
    # showcase at the end of fractal_gallery.logo is written this way.
    assert _strokes(PEN_AND_SQUARE + "PENDOWN SQUARE 10 PENUP") == 4
    assert _strokes(PEN_AND_SQUARE + "PENUP SQUARE 10") == 0
    assert _strokes(PEN_AND_SQUARE + "CS SQUARE 10") == 4
    # HOME walks the turtle back to the origin, drawing on the way.
    assert _strokes(PEN_AND_SQUARE + "CS SQUARE 10 HOME") == 5
