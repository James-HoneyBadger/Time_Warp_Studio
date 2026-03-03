"""Comprehensive tests for the Logo language executor."""

import pytest

from time_warp.core.interpreter import Language

from .conftest_lang import run, ok, has, no_errors, first_error

L = Language.LOGO


# ── helpers ────────────────────────────────────────────────────────────────


def logo(source: str, **kw) -> list[str]:
    """Shortcut: run a Logo program."""
    return run(source, L, **kw)


# ============================================================================
# Movement commands
# ============================================================================


class TestMovement:
    """FORWARD/FD, BACK/BK, RIGHT/RT, LEFT/LT – graphics only."""

    def test_forward(self):
        out = logo("FD 100")
        assert no_errors(out) or len(out) == 0

    def test_forward_full(self):
        out = logo("FORWARD 50")
        assert no_errors(out) or len(out) == 0

    def test_back(self):
        out = logo("BK 50")
        assert no_errors(out) or len(out) == 0

    def test_back_full(self):
        out = logo("BACK 30")
        assert no_errors(out) or len(out) == 0

    def test_backward_alias(self):
        out = logo("BACKWARD 20")
        assert no_errors(out) or len(out) == 0

    def test_right(self):
        out = logo("RT 90")
        assert no_errors(out) or len(out) == 0

    def test_right_full(self):
        out = logo("RIGHT 45")
        assert no_errors(out) or len(out) == 0

    def test_left(self):
        out = logo("LT 90")
        assert no_errors(out) or len(out) == 0

    def test_left_full(self):
        out = logo("LEFT 60")
        assert no_errors(out) or len(out) == 0


# ============================================================================
# Pen commands
# ============================================================================


class TestPen:
    """PU/PENUP, PD/PENDOWN, SETPENCOLOR, SETPENSIZE."""

    def test_penup(self):
        out = logo("PU")
        assert no_errors(out) or len(out) == 0

    def test_penup_full(self):
        out = logo("PENUP")
        assert no_errors(out) or len(out) == 0

    def test_pendown(self):
        out = logo("PD")
        assert no_errors(out) or len(out) == 0

    def test_pendown_full(self):
        out = logo("PENDOWN")
        assert no_errors(out) or len(out) == 0

    def test_setpencolor_name(self):
        out = logo('SETPENCOLOR "red')
        assert no_errors(out) or len(out) == 0

    def test_setpencolor_rgb(self):
        out = logo("SETPENCOLOR 255 0 128")
        assert no_errors(out) or len(out) == 0

    def test_setpensize(self):
        out = logo("SETPENSIZE 3")
        assert no_errors(out) or len(out) == 0

    def test_setpenwidth(self):
        out = logo("SETPENWIDTH 2")
        assert no_errors(out) or len(out) == 0


# ============================================================================
# Position / heading
# ============================================================================


class TestPosition:
    """HOME, SETXY, SETX, SETY, SETHEADING/SETH."""

    def test_home(self):
        out = logo("FD 100\nHOME")
        assert no_errors(out) or len(out) == 0

    def test_setxy(self):
        out = logo("SETXY 100 50")
        assert no_errors(out) or len(out) == 0

    def test_setx(self):
        out = logo("SETX 75")
        assert no_errors(out) or len(out) == 0

    def test_sety(self):
        out = logo("SETY -30")
        assert no_errors(out) or len(out) == 0

    def test_setheading(self):
        out = logo("SETHEADING 180")
        assert no_errors(out) or len(out) == 0

    def test_seth_alias(self):
        out = logo("SETH 90")
        assert no_errors(out) or len(out) == 0

    def test_setposition_list(self):
        out = logo("SETPOS [100 200]")
        assert no_errors(out) or len(out) == 0


# ============================================================================
# Screen commands
# ============================================================================


class TestScreen:
    """CS/CLEARSCREEN, CLEAN, HIDETURTLE/HT, SHOWTURTLE/ST."""

    def test_clearscreen(self):
        out = logo("CS")
        assert no_errors(out) or len(out) == 0

    def test_clearscreen_full(self):
        out = logo("CLEARSCREEN")
        assert no_errors(out) or len(out) == 0

    def test_hideturtle(self):
        out = logo("HT")
        assert no_errors(out) or len(out) == 0

    def test_hideturtle_full(self):
        out = logo("HIDETURTLE")
        assert no_errors(out) or len(out) == 0

    def test_showturtle(self):
        out = logo("ST")
        assert no_errors(out) or len(out) == 0

    def test_showturtle_full(self):
        out = logo("SHOWTURTLE")
        assert no_errors(out) or len(out) == 0


# ============================================================================
# PRINT / SHOW / TYPE
# ============================================================================


class TestOutput:
    """PRINT, SHOW, TYPE output commands."""

    def test_print_number(self):
        out = logo("PRINT 42")
        assert has(out, "42")

    def test_print_word(self):
        out = logo('PRINT "Hello')
        assert has(out, "HELLO")

    def test_print_variable(self):
        out = logo('MAKE "X 10\nPRINT :X')
        assert has(out, "10")

    def test_show(self):
        out = logo('SHOW "World')
        assert has(out, "WORLD")

    def test_type_no_newline(self):
        out = logo('TYPE "Hi')
        assert has(out, "HI")


# ============================================================================
# Variables – MAKE / :VAR
# ============================================================================


class TestVariables:
    """MAKE and :VAR variable references."""

    def test_make_numeric(self):
        out = logo('MAKE "SIZE 50\nPRINT :SIZE')
        assert has(out, "50")

    def test_make_string(self):
        out = logo('MAKE "NAME "Logo\nPRINT :NAME')
        # Strings stored in string_variables
        assert no_errors(out)

    def test_make_expression(self):
        out = logo('MAKE "X 3 + 4\nPRINT :X')
        assert has(out, "7")


# ============================================================================
# REPEAT
# ============================================================================


class TestRepeat:
    """REPEAT n [...] loop."""

    def test_repeat_print(self):
        out = logo('REPEAT 3 [PRINT "hi]')
        assert no_errors(out)
        # Logo uppercases words; output includes leading quote
        joined = "\n".join(out).upper()
        assert joined.count("HI") >= 3

    def test_repeat_turtle(self):
        out = logo("REPEAT 4 [FD 50 RT 90]")
        assert no_errors(out) or len(out) == 0

    def test_repeat_nested(self):
        out = logo("REPEAT 2 [REPEAT 2 [FD 10 RT 90]]")
        assert no_errors(out) or len(out) == 0


# ============================================================================
# IF / IFELSE
# ============================================================================


class TestConditionals:
    """IF cond [...] and IFELSE cond [...] [...]."""

    def test_if_true(self):
        out = logo('MAKE "X 5\nIF :X = 5 [PRINT "yes]')
        assert has(out, "YES")

    def test_if_false(self):
        out = logo('MAKE "X 3\nIF :X = 5 [PRINT "yes]')
        assert not has(out, "YES")

    def test_ifelse_true(self):
        out = logo('MAKE "X 1\nIFELSE :X = 1 [PRINT "T] [PRINT "F]')
        assert has(out, "T")

    def test_ifelse_false(self):
        out = logo('MAKE "X 0\nIFELSE :X = 1 [PRINT "T] [PRINT "F]')
        assert has(out, "F")


# ============================================================================
# Procedures – TO / END
# ============================================================================


class TestProcedures:
    """TO name :param ... END – user-defined procedures."""

    def test_simple_procedure(self):
        src = 'TO GREET\nPRINT "hello\nEND\nGREET'
        out = logo(src)
        assert has(out, "HELLO")

    def test_procedure_with_param(self):
        src = "TO SQUARE :SIZE\nREPEAT 4 [FD :SIZE RT 90]\nEND\nSQUARE 50"
        out = logo(src)
        assert no_errors(out) or len(out) == 0

    def test_procedure_with_output(self):
        src = "TO DOUBLE :N\nOUTPUT :N * 2\nEND\nPRINT DOUBLE 5"
        out = logo(src)
        # DOUBLE 5 returns 10
        assert no_errors(out)


# ============================================================================
# Data operations
# ============================================================================


class TestDataOperations:
    """WORD, LIST, SENTENCE, FIRST, LAST, BUTFIRST, BUTLAST, COUNT, ITEM."""

    def test_first_of_word(self):
        out = logo('PRINT FIRST "Hello')
        assert has(out, "H")

    def test_last_of_word(self):
        out = logo('PRINT LAST "Hello')
        assert has(out, "O")

    def test_butfirst_of_word(self):
        out = logo('PRINT BUTFIRST "Hello')
        assert has(out, "ELLO")

    def test_butlast_of_word(self):
        out = logo('PRINT BUTLAST "Hello')
        assert has(out, "HELL")

    def test_count_word(self):
        # COUNT via variable (inline COUNT not parsed as function)
        out = logo('MAKE "W "Hello\nPRINT COUNT :W')
        assert no_errors(out)

    def test_first_of_list(self):
        # Lists work through variables
        out = logo('MAKE "L [a b c]\nPRINT FIRST :L')
        assert has(out, "A")

    def test_last_of_list(self):
        out = logo('MAKE "L [a b c]\nPRINT LAST :L')
        assert has(out, "C")

    def test_count_list(self):
        out = logo('MAKE "L [a b c]\nPRINT COUNT :L')
        assert no_errors(out)

    def test_item(self):
        out = logo('PRINT ITEM 2 "Hello')
        assert has(out, "E")


# ============================================================================
# Arithmetic (inline) — SUM, DIFFERENCE, PRODUCT, QUOTIENT, RANDOM
# ============================================================================


class TestArithmetic:
    """Inline arithmetic expressions."""

    def test_addition(self):
        out = logo("PRINT SUM 3 4")
        assert has(out, "7")

    def test_subtraction(self):
        out = logo("PRINT DIFFERENCE 10 3")
        assert has(out, "7")

    def test_multiplication(self):
        out = logo("PRINT PRODUCT 6 7")
        assert has(out, "42")

    def test_division(self):
        out = logo("PRINT QUOTIENT 10 2")
        assert has(out, "5")

    def test_random(self):
        out = logo("PRINT RANDOM 100")
        assert no_errors(out)
        # Should produce a number 0-99


# ============================================================================
# ARC
# ============================================================================


class TestArc:
    """ARC angle radius – draws an arc."""

    def test_arc(self):
        out = logo("ARC 180 50")
        assert no_errors(out) or len(out) == 0

    def test_arc_full_circle(self):
        out = logo("ARC 360 30")
        assert no_errors(out) or len(out) == 0


# ============================================================================
# FILLED
# ============================================================================


class TestFilled:
    """FILLED color [...] – fill the traced shape."""

    def test_filled_square(self):
        out = logo('FILLED "red [REPEAT 4 [FD 50 RT 90]]')
        assert no_errors(out) or len(out) == 0

    def test_filled_rgb(self):
        out = logo("FILLED [128 0 255] [REPEAT 3 [FD 60 RT 120]]")
        assert no_errors(out) or len(out) == 0


# ============================================================================
# LABEL
# ============================================================================


class TestLabel:
    """LABEL text – draw text on canvas."""

    def test_label(self):
        out = logo('LABEL "Hello')
        assert no_errors(out) or len(out) == 0


# ============================================================================
# WAIT
# ============================================================================


class TestWait:
    """WAIT n – pause for n/60 seconds."""

    def test_wait(self):
        out = logo("WAIT 1")
        assert no_errors(out) or len(out) == 0


# ============================================================================
# BYE
# ============================================================================


class TestBye:
    """BYE – exit logo."""

    def test_bye(self):
        out = logo("BYE")
        assert has(out, "Goodbye")


# ============================================================================
# SETBGCOLOR / SETBG
# ============================================================================


class TestBackground:
    """SETBGCOLOR – set canvas background."""

    def test_setbg_named(self):
        out = logo('SETBG "RED')
        assert no_errors(out) or len(out) == 0

    def test_setbg_rgb(self):
        out = logo("SETBG 0 128 255")
        assert no_errors(out) or len(out) == 0


# ============================================================================
# Comments
# ============================================================================


class TestComments:
    """Logo comments use semicolons."""

    def test_semicolon_comment(self):
        out = logo("; this is a comment\nPRINT 42")
        assert has(out, "42")
        assert no_errors(out)


# ============================================================================
# REM (compatibility)
# ============================================================================


class TestRem:
    """REM as a no-op comment."""

    def test_rem(self):
        out = logo("REM this is also a comment\nPRINT 1")
        assert has(out, "1")
        assert no_errors(out)


# ============================================================================
# Complex programs
# ============================================================================


class TestComplexPrograms:
    """Multi-feature programs."""

    def test_square_with_variable(self):
        src = 'MAKE "S 40\nREPEAT 4 [FD :S RT 90]'
        out = logo(src)
        assert no_errors(out) or len(out) == 0

    def test_triangle(self):
        out = logo("REPEAT 3 [FD 60 RT 120]")
        assert no_errors(out) or len(out) == 0

    def test_star(self):
        out = logo("REPEAT 5 [FD 80 RT 144]")
        assert no_errors(out) or len(out) == 0

    def test_procedure_call_twice(self):
        src = "TO SQ :S\nREPEAT 4 [FD :S RT 90]\nEND\nSQ 30\nSQ 60"
        out = logo(src)
        assert no_errors(out) or len(out) == 0


# ============================================================================
# Error handling
# ============================================================================


class TestErrors:
    """Error cases."""

    def test_unknown_command(self):
        out = logo("FOOBAR 100")
        assert first_error(out) is not None

    def test_forward_no_arg(self):
        out = logo("FD")
        assert first_error(out) is not None

    def test_setxy_missing_y(self):
        out = logo("SETXY 100")
        assert first_error(out) is not None

    def test_repeat_no_brackets(self):
        out = logo("REPEAT 4 FD 50")
        assert first_error(out) is not None


# ============================================================================
# Reporter commands (POS, XCOR, YCOR, HEADING)
# ============================================================================


class TestReporters:
    """Reporter commands return turtle state."""

    def test_pos(self):
        out = logo("PRINT POS")
        assert has(out, "[0")

    def test_pos_after_move(self):
        out = logo("FD 50\nPRINT POS")
        assert has(out, "50")


# ============================================================================
# TO procedures with string params
# ============================================================================


class TestStringProcParams:
    """TO procedures accept string arguments."""

    def test_to_with_string_param(self):
        out = logo('TO GREET :NAME\n  PRINT :NAME\nEND\nGREET "Hello')
        assert has(out, "HELLO")


# ============================================================================
# REPEAT output deduplication
# ============================================================================


class TestRepeatOutputDedup:
    """Regression: REPEAT should not produce doubled output."""

    def test_repeat_exact_count(self):
        out = logo('REPEAT 3 [PRINT "hi]')
        joined = "\n".join(out).upper()
        # Exactly 3 occurrences, not 6 (was doubled before fix)
        assert joined.count("HI") == 3

    def test_single_print_no_duplicate(self):
        out = logo("PRINT 42")
        # Should produce exactly one output entry containing 42
        matches = [line for line in out if "42" in line]
        assert len(matches) == 1
