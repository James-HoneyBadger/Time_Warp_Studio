"""Additional Logo executor coverage tests - Part 3."""
import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, has, no_errors

L = Language.LOGO


def logo(src, **kw):
    return run(src, L, **kw)


class TestLogoMakeAndVariables:
    """Test MAKE (variable assignment) and related operations."""

    def test_make_integer(self):
        out = logo('MAKE "X 42\nPRINT :X')
        assert has(out, "42")

    def test_make_float(self):
        out = logo('MAKE "PI 3.14\nPRINT :PI')
        assert has(out, "3.14")

    def test_make_expression(self):
        out = logo('MAKE "A 3\nMAKE "B 4\nMAKE "C :A + :B\nPRINT :C')
        assert has(out, "7")

    def test_thing_retrieves_value(self):
        out = logo('MAKE "X 99\nPRINT THING "X')
        assert has(out, "99")

    def test_make_string_word(self):
        out = logo('MAKE "NAME "hello\nPRINT :NAME')
        # Logo uppercases string literals
        assert has(out, "HELLO") or has(out, "hello")


class TestLogoArithmetic:
    """Test arithmetic operations via PRINT."""

    def test_sum_function(self):
        out = logo('PRINT SUM 3 4')
        assert has(out, "7")

    def test_difference_function(self):
        out = logo('PRINT DIFFERENCE 10 3')
        assert has(out, "7")

    def test_product_function(self):
        out = logo('PRINT PRODUCT 3 4')
        assert has(out, "12")

    def test_quotient_function(self):
        out = logo('PRINT QUOTIENT 10 2')
        assert has(out, "5")

    def test_random_produces_output(self):
        out = logo('MAKE "R RANDOM 10\nPRINT :R')
        assert no_errors(out) and len(out) > 0

    def test_print_arithmetic_expr(self):
        out = logo('PRINT SUM 2 3')
        assert has(out, "5")

    def test_print_multiplication(self):
        out = logo('PRINT PRODUCT 6 7')
        assert has(out, "42")


class TestLogoStringFunctions:
    """Test string and list operations."""

    def test_word_concat(self):
        out = logo('PRINT WORD "hello " "world"')
        assert has(out, "HELLO") or has(out, "hello") or has(out, "WORLD") or has(out, "world")

    def test_first_of_string(self):
        out = logo('PRINT FIRST "hello"')
        assert has(out, "H") or has(out, "h")

    def test_last_of_string(self):
        out = logo('PRINT LAST "hello"')
        assert has(out, "O") or has(out, "o")

    def test_butfirst(self):
        out = logo('PRINT BUTFIRST "hello"')
        assert has(out, "ELLO") or has(out, "ello")

    def test_butlast(self):
        out = logo('PRINT BUTLAST "hello"')
        assert has(out, "HELL") or has(out, "hell")

    def test_count_string(self):
        out = logo('COUNT "hello')
        assert has(out, "5")

    def test_item_from_string(self):
        out = logo('PRINT ITEM 1 "hello"')
        assert has(out, "H") or has(out, "h")


class TestLogoListOperations:
    """Test list-related Logo commands."""

    def test_list_creates_list(self):
        out = logo('PRINT LIST "a "b')
        assert (has(out, "A") or has(out, "a")) and (has(out, "B") or has(out, "b"))

    def test_first_of_list(self):
        out = logo('MAKE "L [1 2 3]\nPRINT FIRST :L')
        assert has(out, "1")

    def test_last_of_list(self):
        out = logo('MAKE "L [1 2 3]\nPRINT LAST :L')
        assert has(out, "3")

    @pytest.mark.xfail(reason="COUNT as a nested expression is not yet supported by the Logo executor")
    def test_count_list(self):
        out = logo('MAKE "L [1 2 3]\nMAKE "C COUNT :L\nPRINT :C')
        assert has(out, "3")

    def test_item_from_list(self):
        out = logo('MAKE "L [10 20 30]\nPRINT ITEM 2 :L')
        assert has(out, "20")


class TestLogoIfAndBranches:
    """Test IF and IFELSE."""

    def test_if_true_runs_block(self):
        out = logo('IF 1 = 1 [PRINT "yes"]')
        assert has(out, "YES") or has(out, "yes")

    def test_if_false_skips_block(self):
        out = logo('IF 1 = 2 [PRINT "yes"]')
        assert not has(out, "YES") and not has(out, "yes") and no_errors(out)

    def test_ifelse_true(self):
        out = logo('IFELSE 1 = 1 [PRINT "yes"] [PRINT "no"]')
        assert has(out, "YES") or has(out, "yes")

    def test_ifelse_false(self):
        out = logo('IFELSE 1 = 2 [PRINT "yes"] [PRINT "no"]')
        combined = " ".join(out)
        assert "NO" in combined.upper()

    def test_if_variable_comparison(self):
        out = logo('MAKE "X 5\nIF :X > 3 [PRINT "big"]')
        assert has(out, "BIG") or has(out, "big")

    def test_if_less_than(self):
        out = logo('MAKE "X 2\nIF :X < 3 [PRINT "small"]')
        assert has(out, "SMALL") or has(out, "small")


class TestLogoWhileAndUntil:
    """Test WHILE and UNTIL loops."""

    def test_while_loop_count(self):
        out = logo('MAKE "I 0\nWHILE [:I < 3] [MAKE "I :I + 1]\nPRINT :I')
        assert has(out, "3")

    def test_until_loop(self):
        out = logo('MAKE "I 0\nUNTIL [:I = 3] [MAKE "I :I + 1]\nPRINT :I')
        assert has(out, "3")


class TestLogoRepcount:
    """Test REPCOUNT inside REPEAT."""

    def test_repcount_in_repeat(self):
        out = logo('REPEAT 3 [MAKE "R REPCOUNT\nPRINT :R]')
        assert has(out, "1") and has(out, "2") and has(out, "3")


class TestLogoTurtleGraphics:
    """Test turtle graphic commands."""

    def test_forward_and_back(self):
        out = logo('FORWARD 50\nBACK 50')
        assert no_errors(out)

    def test_right_and_left(self):
        out = logo('RIGHT 90\nLEFT 90')
        assert no_errors(out)

    def test_penup_pendown(self):
        out = logo('PENUP\nFORWARD 10\nPENDOWN\nFORWARD 10')
        assert no_errors(out)

    def test_home_command(self):
        out = logo('FORWARD 100\nHOME')
        assert no_errors(out)

    def test_setx_and_sety(self):
        out = logo('SETX 10\nSETY 20')
        assert no_errors(out)

    def test_setheading(self):
        out = logo('SETHEADING 90')
        assert no_errors(out)

    def test_setpos(self):
        out = logo('SETPOS [10 20]')
        assert no_errors(out)

    def test_xcor_ycor_heading(self):
        out = logo('PRINT XCOR\nPRINT YCOR\nPRINT HEADING')
        assert no_errors(out)

    def test_pensize(self):
        out = logo('PENSIZE 3')
        assert no_errors(out)

    def test_showturtle_hideturtle(self):
        out = logo('HIDETURTLE\nSHOWTURTLE')
        assert no_errors(out)

    def test_arc_command(self):
        out = logo('ARC 90 50')
        assert no_errors(out)

    def test_dot_command(self):
        out = logo('DOT')
        assert no_errors(out)

    def test_stamp_command(self):
        out = logo('STAMP')
        assert no_errors(out)

    def test_label_command(self):
        out = logo('LABEL "Hello')
        assert no_errors(out)


class TestLogoColorSettings:
    """Test color-related commands."""

    def test_setpencolor_red(self):
        out = logo('SETPENCOLOR "red')
        assert no_errors(out)

    def test_setpencolor_number(self):
        out = logo('SETPENCOLOR 2')
        assert no_errors(out)

    def test_setpencolor_rgb(self):
        out = logo('SETPENCOLOR 255 0 0')
        assert no_errors(out)

    def test_setbgcolor_named(self):
        out = logo('SETBGCOLOR "blue')
        assert no_errors(out)

    def test_setpenwidth(self):
        out = logo('SETPENWIDTH 2')
        assert no_errors(out)


class TestLogoProcedures:
    """Test TO/END procedure definitions and calls."""

    def test_define_and_call_simple(self):
        out = logo('TO GREET\nPRINT "hello\nEND\nGREET')
        assert has(out, "HELLO") or has(out, "hello")

    def test_define_with_param(self):
        out = logo('TO SQUARE :SIZE\nREPEAT 4 [FORWARD :SIZE RIGHT 90]\nEND\nSQUARE 50')
        assert no_errors(out)

    def test_stop_in_procedure(self):
        out = logo('TO MYPROC\nSTOP\nPRINT "nope\nEND\nMYPROC')
        combined = " ".join(out)
        assert "NOPE" not in combined.upper()

    def test_output_in_procedure(self):
        out = logo('TO DOUBLE :X\nOUTPUT :X + :X\nEND\nDOUBLE 5')
        assert no_errors(out)


class TestLogoLocalVars:
    """Test LOCAL variables."""

    def test_local_var(self):
        out = logo('TO MYFUNC\nLOCAL "X\nMAKE "X 5\nPRINT :X\nEND\nMYFUNC')
        assert has(out, "5")


class TestLogoScreenMode:
    """Test screen mode commands."""

    @pytest.mark.xfail(reason="TEXTSCREEN not yet implemented in Logo executor")
    def test_text_mode(self):
        out = logo('TEXTSCREEN')
        assert no_errors(out)

    @pytest.mark.xfail(reason="FULLSCREEN not yet implemented in Logo executor")
    def test_graphics_mode(self):
        out = logo('FULLSCREEN')
        assert no_errors(out)

    @pytest.mark.xfail(reason="SPLITSCREEN not yet implemented in Logo executor")
    def test_split_screen(self):
        out = logo('SPLITSCREEN')
        assert no_errors(out)


class TestLogoTypeAndShow:
    """Test TYPE and SHOW commands."""

    def test_type_command(self):
        out = logo('TYPE "hello')
        assert has(out, "HELLO") or has(out, "hello")

    def test_show_command(self):
        out = logo('SHOW [1 2 3]')
        assert no_errors(out)


class TestLogoFilledCommand:
    """Test FILLED shapes."""

    def test_filled_square(self):
        out = logo('FILLED "red [REPEAT 4 [FORWARD 50 RIGHT 90]]')
        assert no_errors(out)

    def test_filled_with_rgb(self):
        out = logo('FILLED [255 0 0] [FORWARD 50 RIGHT 90 FORWARD 50]')
        assert no_errors(out)
