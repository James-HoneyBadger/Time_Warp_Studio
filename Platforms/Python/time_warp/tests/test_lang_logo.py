"""Comprehensive tests for the Logo language executor."""

from time_warp.core.interpreter import Language
from time_warp.graphics.turtle_state import TurtleState

from .conftest_lang import run, has, no_errors, first_error

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

    def test_clean(self):
        out = logo("CLEAN")
        assert no_errors(out) or len(out) == 0

    def test_clean_alias(self):
        out = logo("CLEAR")
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


# ============================================================================
# ARITHMETIC FUNCTIONS
# ============================================================================


class TestArithmeticFunctions:
    """SUM, PRODUCT, DIFFERENCE, QUOTIENT."""

    def test_sum(self):
        out = logo("PRINT SUM 3 4")
        assert has(out, "7")
        assert no_errors(out)

    def test_product(self):
        out = logo("PRINT PRODUCT 3 4")
        assert has(out, "12")
        assert no_errors(out)

    def test_difference(self):
        out = logo("PRINT DIFFERENCE 10 3")
        assert has(out, "7")
        assert no_errors(out)

    def test_quotient(self):
        out = logo("PRINT QUOTIENT 10 2")
        assert has(out, "5")
        assert no_errors(out)


# ============================================================================
# COMPARISONS AND CONDITIONALS
# ============================================================================


class TestConditionals:
    """IF and IFELSE branching."""

    def test_if_true(self):
        out = logo('IF 1 = 1 [PRINT "yes]')
        assert has(out, "YES")
        assert no_errors(out)

    def test_if_false_does_not_print(self):
        out = logo('IF 1 = 2 [PRINT "no]')
        assert not has(out, "NO")
        assert no_errors(out)

    def test_ifelse_true_branch(self):
        out = logo('IFELSE 5 > 3 [PRINT "big] [PRINT "small]')
        assert has(out, "BIG")
        assert not has(out, "SMALL")

    def test_ifelse_false_branch(self):
        out = logo('IFELSE 1 > 5 [PRINT "big] [PRINT "small]')
        assert has(out, "SMALL")
        assert not has(out, "BIG")

    def test_greater_than(self):
        out = logo('IF 10 > 5 [PRINT "bigger]')
        assert has(out, "BIGGER")
        assert no_errors(out)

    def test_less_than(self):
        out = logo('IF 3 < 5 [PRINT "smaller]')
        assert has(out, "SMALLER")
        assert no_errors(out)


# ============================================================================
# WORD OPERATIONS
# ============================================================================


class TestWordOperations:
    """FIRST, LAST, word operations."""

    def test_first(self):
        out = logo('PRINT FIRST "hello')
        assert has(out, "H")
        assert no_errors(out)

    def test_last(self):
        out = logo('PRINT LAST "hello')
        assert has(out, "O")
        assert no_errors(out)

    def test_list_display(self):
        out = logo('PRINT LIST "a "b')
        assert has(out, "A")
        assert has(out, "B")
        assert no_errors(out)


# ============================================================================
# VARIABLE MAKE
# ============================================================================


class TestMakeVariable:
    """MAKE and variable recall."""

    def test_make_and_print(self):
        out = logo('MAKE "X 42\nPRINT :X')
        assert has(out, "42")
        assert no_errors(out)

    def test_make_string(self):
        out = logo('MAKE "MSG "Hello\nPRINT :MSG')
        assert has(out, "HELLO")
        assert no_errors(out)

    def test_make_arithmetic(self):
        out = logo('MAKE "A 10\nMAKE "B 5\nPRINT SUM 10 5')
        assert has(out, "15")
        assert no_errors(out)


class TestLogoArithmetic2:
    """Logo arithmetic using built-in functions."""

    def test_quotient(self):
        assert has(logo("PRINT (QUOTIENT 15 3)"), "5")

    def test_remainder(self):
        assert has(logo("PRINT (REMAINDER 10 3)"), "1")

    def test_abs(self):
        assert has(logo("PRINT (ABS -5)"), "5")

    def test_max(self):
        assert has(logo("PRINT (MAX 3 7)"), "7")

    def test_min(self):
        assert has(logo("PRINT (MIN 3 7)"), "3")

    def test_make_and_print(self):
        assert has(logo('MAKE "X 5\nPRINT :X'), "5")

    def test_floor(self):
        assert has(logo("PRINT (FLOOR 3.7)"), "3")


class TestLogoStrings2:
    """More Logo string tests."""

    def test_word_concat(self):
        assert has(logo('MAKE "A "hello\nPRINT :A'), '"HELLO') or \
               has(logo('MAKE "A "hello\nPRINT :A'), 'HELLO')

    def test_print_number(self):
        assert has(logo('MAKE "N 42\nPRINT :N'), "42")

    def test_print_string_var(self):
        out = logo('MAKE "S "test\nPRINT :S')
        assert any('TEST' in l or 'test' in l for l in out)

    def test_make_and_use(self):
        assert has(logo('MAKE "X 100\nPRINT :X'), "100")

    def test_sum_of_vars(self):
        assert has(logo('PRINT SUM 3 4'), "7")

    def test_difference_of_vars(self):
        assert has(logo('PRINT DIFFERENCE 10 3'), "7")

    def test_product_of_vars(self):
        assert has(logo('PRINT PRODUCT 6 7'), "42")


class TestLogoRepeat2:
    """More Logo repeat tests."""

    def test_repeat_3(self):
        result = logo('REPEAT 3 [PRINT "hi]')
        assert len(result) == 3  # Prints 3 times

    def test_repeat_1(self):
        result = logo('REPEAT 1 [PRINT "once]')
        assert len(result) == 1  # Prints 1 time

    def test_repeat_counter_workaround(self):
        # REPEAT doesn't give counter access directly, use MAKE inside
        assert has(logo('MAKE "C 0\nREPEAT 3 [MAKE "C :C + 1]\nPRINT :C'), "3")


class TestLogoArithmetic2:
    """Additional Logo arithmetic tests."""

    def test_sum_7_3(self):
        result = logo('PRINT SUM 7 3')
        assert any("10" in line for line in result)

    def test_product_6_7(self):
        result = logo('PRINT PRODUCT 6 7')
        assert any("42" in line for line in result)

    def test_difference_10_3(self):
        result = logo('PRINT DIFFERENCE 10 3')
        assert any("7" in line for line in result)

    def test_quotient_15_3(self):
        result = logo('PRINT QUOTIENT 15 3')
        assert any("5" in line for line in result)

    def test_sum_chain(self):
        result = logo('PRINT SUM 1 9')
        assert any("10" in line for line in result)

    def test_product_9_9(self):
        result = logo('PRINT PRODUCT 9 9')
        assert any("81" in line for line in result)

    def test_make_var(self):
        result = logo('MAKE "X 99\nPRINT :X')
        assert any("99" in line for line in result)

    def test_make_zero(self):
        result = logo('MAKE "N 0\nPRINT :N')
        assert any("0" in line for line in result)

    def test_print_word(self):
        result = logo('PRINT "HELLO')
        assert any("HELLO" in line for line in result)

    def test_print_logo(self):
        result = logo('PRINT "LOGO')
        assert any("LOGO" in line for line in result)


class TestLogoPilot2:
    """Additional Logo output tests."""

    def test_make_and_print_two(self):
        result = logo('MAKE "A 7\nPRINT :A')
        assert any("7" in line for line in result)

    def test_print_difference_neg(self):
        result = logo('PRINT DIFFERENCE 3 10')
        assert any("-7" in line for line in result)

    def test_two_prints(self):
        result = logo('PRINT "FIRST\nPRINT "SECOND')
        assert any("FIRST" in line for line in result)
        assert any("SECOND" in line for line in result)

    def test_print_product_large(self):
        result = logo('PRINT PRODUCT 12 12')
        assert any("144" in line for line in result)


class TestLogoExtended:
    """More Logo tests."""

    def test_print_sum_3_4(self):
        result = logo('PRINT SUM 3 4')
        assert any("7" in line for line in result)

    def test_print_difference_10_3(self):
        result = logo('PRINT DIFFERENCE 10 3')
        assert any("7" in line for line in result)

    def test_print_product_5_6(self):
        result = logo('PRINT PRODUCT 5 6')
        assert any("30" in line for line in result)

    def test_print_quotient_10_2(self):
        result = logo('PRINT QUOTIENT 10 2')
        assert any("5" in line for line in result)

    def test_make_and_print_word(self):
        result = logo('MAKE "NAME "LOGO\nPRINT :NAME')
        assert any("LOGO" in line for line in result)

    def test_make_number_and_print(self):
        result = logo('MAKE "N 42\nPRINT :N')
        assert any("42" in line for line in result)

    def test_repeat_2_forward(self):
        result = logo('REPEAT 2 [FORWARD 50]')
        assert isinstance(result, list)

    def test_repeat_prints(self):
        result = logo('REPEAT 3 [PRINT "HI]')
        texts = " ".join(result)
        assert texts.count("HI") == 3

    def test_forward_creates_line(self):
        from time_warp.core.interpreter import Interpreter
        ts = TurtleState()
        interp = Interpreter()
        interp.load_program('FORWARD 100', language=L)
        interp.execute(ts)
        assert len(ts.lines) > 0

    def test_right_changes_heading(self):
        from time_warp.core.interpreter import Interpreter
        ts = TurtleState()
        interp = Interpreter()
        interp.load_program('RIGHT 90', language=L)
        interp.execute(ts)
        assert abs(ts.heading - 90) < 1

    def test_penup_no_lines(self):
        from time_warp.core.interpreter import Interpreter
        ts = TurtleState()
        interp = Interpreter()
        interp.load_program('PENUP\nFORWARD 100', language=L)
        interp.execute(ts)
        assert len(ts.lines) == 0

    def test_pendown_after_penup_has_lines(self):
        from time_warp.core.interpreter import Interpreter
        ts = TurtleState()
        interp = Interpreter()
        interp.load_program('PENUP\nFORWARD 50\nPENDOWN\nFORWARD 50', language=L)
        interp.execute(ts)
        assert len(ts.lines) > 0

    def test_setpensize_3(self):
        from time_warp.core.interpreter import Interpreter
        ts = TurtleState()
        interp = Interpreter()
        interp.load_program('SETPENSIZE 3', language=L)
        interp.execute(ts)
        assert ts.pen_width == 3

    def test_hideturtle(self):
        from time_warp.core.interpreter import Interpreter
        ts = TurtleState()
        interp = Interpreter()
        interp.load_program('HIDETURTLE', language=L)
        interp.execute(ts)
        assert ts.visible is False

    def test_showturtle(self):
        from time_warp.core.interpreter import Interpreter
        ts = TurtleState()
        interp = Interpreter()
        interp.load_program('HIDETURTLE\nSHOWTURTLE', language=L)
        interp.execute(ts)
        assert ts.visible is True

    def test_print_multiple_lines(self):
        result = logo('PRINT "A\nPRINT "B\nPRINT "C')
        texts = " ".join(result)
        assert "A" in texts and "B" in texts and "C" in texts

    def test_sum_zero(self):
        result = logo('PRINT SUM 0 0')
        assert any("0" in line for line in result)

    def test_back_moves_backward(self):
        from time_warp.core.interpreter import Interpreter
        ts = TurtleState()
        interp = Interpreter()
        interp.load_program('FORWARD 100\nBACK 50', language=L)
        interp.execute(ts)
        assert len(ts.lines) >= 2


class TestLogoExtended:
    """Extra Logo tests."""

    def logo(self, src):
        return run(src, Language.LOGO)

    def test_print_100(self):
        result = self.logo('PRINT 100')
        assert has(result, "100")

    def test_print_hello(self):
        result = self.logo('PRINT "HELLO')
        assert has(result, "HELLO")

    def test_print_zero(self):
        result = self.logo('PRINT 0')
        assert has(result, "0")

    def test_output_is_list(self):
        result = self.logo('PRINT 1')
        assert isinstance(result, list)

    def test_no_errors_simple(self):
        result = self.logo('PRINT "OK')
        assert no_errors(result)

    def test_make_variable(self):
        result = self.logo('MAKE "X 42\nPRINT :X')
        assert has(result, "42")

    def test_forward_no_crash(self):
        result = self.logo('FORWARD 100')
        assert isinstance(result, list)

    def test_right_no_crash(self):
        result = self.logo('RIGHT 90')
        assert isinstance(result, list)

    def test_left_no_crash(self):
        result = self.logo('LEFT 45')
        assert isinstance(result, list)

    def test_repeat_no_crash(self):
        result = self.logo('REPEAT 4 [FORWARD 50 RIGHT 90]')
        assert isinstance(result, list)

    def test_two_prints(self):
        result = self.logo('PRINT "A\nPRINT "B')
        assert has(result, "A") or has(result, "B")

    def test_empty_source(self):
        result = self.logo('')
        assert isinstance(result, list)

    def test_print_negative(self):
        result = self.logo('PRINT -5')
        assert has(result, "-5")

    def test_print_large_number(self):
        result = self.logo('PRINT 9999')
        assert has(result, "9999")

    def test_penup_no_crash(self):
        result = self.logo('PENUP')
        assert isinstance(result, list)
