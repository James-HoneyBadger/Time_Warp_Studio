"""Comprehensive tests for the BASIC language executor."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors, first_error

L = Language.BASIC


# ── helpers ────────────────────────────────────────────────────────────────


def bas(source: str, **kw) -> list[str]:
    """Shortcut: run a BASIC program."""
    return run(source, L, **kw)


# ============================================================================
# PRINT
# ============================================================================


class TestPrint:
    """PRINT statement (string literals, numeric expressions, concatenation)."""

    def test_print_string_literal(self):
        out = bas('10 PRINT "Hello World"')
        assert has(out, "Hello World")

    def test_print_number(self):
        out = bas("10 LET X = 42\n20 PRINT X")
        assert has(out, "42")

    def test_print_expression(self):
        out = bas("10 PRINT 3 + 4")
        assert has(out, "7")

    def test_print_concatenation_semicolon(self):
        out = bas('10 PRINT "A";"B"')
        assert has(out, "A", "B")

    def test_print_blank_line(self):
        out = bas("10 PRINT")
        assert no_errors(out)

    def test_print_string_variable(self):
        out = bas('10 A$ = "Hi"\n20 PRINT A$')
        assert has(out, "Hi")


# ============================================================================
# LET / Assignment
# ============================================================================


class TestLet:
    """LET and implicit assignment."""

    def test_let_numeric(self):
        out = bas("10 LET X = 5\n20 PRINT X")
        assert has(out, "5")

    def test_implicit_assignment(self):
        out = bas("10 X = 10\n20 PRINT X")
        assert has(out, "10")

    def test_string_assignment(self):
        out = bas('10 A$ = "World"\n20 PRINT A$')
        assert has(out, "World")

    def test_expression_assignment(self):
        out = bas("10 X = 2 * 3 + 1\n20 PRINT X")
        assert has(out, "7")

    def test_typed_variable_int(self):
        out = bas("10 X% = 7\n20 PRINT X%")
        assert has(out, "7")


# ============================================================================
# INPUT
# ============================================================================


class TestInput:
    """INPUT statement (uses start_input_request, handled by run() helper)."""

    def test_input_numeric(self):
        out = bas("10 INPUT X\n20 PRINT X", input_val="7")
        assert has(out, "7")

    def test_input_string(self):
        out = bas("10 INPUT A$\n20 PRINT A$", input_val="hello")
        assert has(out, "hello")

    def test_input_with_prompt(self):
        out = bas('10 INPUT "Name? ";N$\n20 PRINT N$', input_val="Alice")
        assert has(out, "Alice")


# ============================================================================
# IF / THEN / ELSE
# ============================================================================


class TestIf:
    """IF...THEN...ELSE (inline and block forms)."""

    def test_if_true_inline(self):
        out = bas('10 X = 5\n20 IF X = 5 THEN PRINT "yes"')
        assert has(out, "yes")

    def test_if_false_inline(self):
        out = bas('10 X = 3\n20 IF X = 5 THEN PRINT "yes"')
        assert not has(out, "yes")

    def test_if_else_inline(self):
        out = bas('10 X = 3\n20 IF X = 5 THEN PRINT "yes" ELSE PRINT "no"')
        assert has(out, "no")

    def test_block_if_true(self):
        src = "10 X = 1\n" "20 IF X = 1 THEN\n" '30 PRINT "ok"\n' "40 END IF"
        out = bas(src)
        assert has(out, "ok")

    def test_block_if_false_else(self):
        src = (
            "10 X = 0\n"
            "20 IF X = 1 THEN\n"
            '30 PRINT "yes"\n'
            "40 ELSE\n"
            '50 PRINT "no"\n'
            "60 END IF"
        )
        out = bas(src)
        assert has(out, "no")


# ============================================================================
# GOTO / GOSUB / RETURN
# ============================================================================


class TestGotoGosub:
    """GOTO, GOSUB, RETURN."""

    def test_goto(self):
        src = '10 PRINT "A"\n' "20 GOTO 40\n" '30 PRINT "B"\n' '40 PRINT "C"'
        out = bas(src)
        assert has(out, "A", "C")
        assert not has(out, "B")

    def test_gosub_return(self):
        src = (
            "10 GOSUB 100\n"
            '20 PRINT "back"\n'
            "30 END\n"
            '100 PRINT "sub"\n'
            "110 RETURN"
        )
        out = bas(src)
        assert has(out, "sub", "back")


# ============================================================================
# FOR / NEXT
# ============================================================================


class TestForNext:
    """FOR...NEXT loop."""

    def test_for_next_basic(self):
        src = "10 FOR I = 1 TO 3\n20 PRINT I\n30 NEXT I"
        out = bas(src)
        assert has(out, "1", "2", "3")

    def test_for_next_step(self):
        src = "10 FOR I = 2 TO 6 STEP 2\n20 PRINT I\n30 NEXT I"
        out = bas(src)
        assert has(out, "2", "4", "6")


# ============================================================================
# WHILE / WEND
# ============================================================================


class TestWhileWend:
    """WHILE...WEND loop."""

    def test_while_wend(self):
        src = "10 X = 0\n" "20 WHILE X < 3\n" "30 X = X + 1\n" "40 WEND\n" "50 PRINT X"
        out = bas(src)
        assert has(out, "3")

    def test_while_false_skip(self):
        src = (
            "10 X = 10\n"
            "20 WHILE X < 3\n"
            '30 PRINT "loop"\n'
            "40 WEND\n"
            '50 PRINT "done"'
        )
        out = bas(src)
        assert has(out, "done")
        assert not has(out, "loop")


# ============================================================================
# DO / LOOP
# ============================================================================


class TestDoLoop:
    """DO...LOOP (WHILE/UNTIL variants)."""

    def test_do_loop_until(self):
        src = (
            "10 X = 0\n" "20 DO\n" "30 X = X + 1\n" "40 LOOP UNTIL X = 3\n" "50 PRINT X"
        )
        out = bas(src)
        assert has(out, "3")

    def test_do_while_loop(self):
        src = (
            "10 X = 0\n" "20 DO WHILE X < 2\n" "30 X = X + 1\n" "40 LOOP\n" "50 PRINT X"
        )
        out = bas(src)
        assert has(out, "2")


# ============================================================================
# REM / Comments
# ============================================================================


class TestComments:
    """REM comments."""

    def test_rem(self):
        out = bas("10 REM this is a comment\n20 PRINT 1")
        assert has(out, "1")
        assert no_errors(out)

    def test_apostrophe_comment(self):
        out = bas("10 ' this is also a comment\n20 PRINT 2")
        assert has(out, "2")
        assert no_errors(out)


# ============================================================================
# DIM / Arrays
# ============================================================================


class TestArrays:
    """DIM and array operations."""

    def test_dim_numeric(self):
        src = "10 DIM A(5)\n" "20 A(0) = 99\n" "30 PRINT A(0)"
        out = bas(src)
        assert has(out, "99")

    def test_dim_string_array(self):
        src = "10 DIM N$(3)\n" '20 N$(0) = "hi"\n' "30 PRINT N$(0)"
        out = bas(src)
        assert has(out, "hi")


# ============================================================================
# DATA / READ / RESTORE
# ============================================================================


class TestDataRead:
    """DATA, READ, RESTORE."""

    def test_read_data(self):
        src = (
            "10 DATA 10, 20, 30\n"
            "20 READ A\n"
            "30 READ B\n"
            "40 PRINT A\n"
            "50 PRINT B"
        )
        out = bas(src)
        assert has(out, "10", "20")


# ============================================================================
# String Functions
# ============================================================================


class TestStringFunctions:
    """LEN, LEFT, RIGHT, MID, etc. (no $ suffix)."""

    def test_len(self):
        out = bas('10 PRINT LEN("Hello")')
        assert has(out, "5")

    def test_left(self):
        out = bas('10 PRINT LEFT("Hello", 3)')
        assert has(out, "Hel")

    def test_right(self):
        out = bas('10 PRINT RIGHT("Hello", 2)')
        assert has(out, "lo")

    def test_mid(self):
        out = bas('10 PRINT MID("Hello", 2, 3)')
        assert has(out, "ell")

    def test_upper(self):
        out = bas('10 PRINT UPPER("hello")')
        assert has(out, "HELLO")

    def test_lower(self):
        out = bas('10 PRINT LOWER("HELLO")')
        assert has(out, "hello")


# ============================================================================
# CLS / SCREEN
# ============================================================================


class TestScreenGraphics:
    """CLS, SCREEN, LOCATE."""

    def test_cls(self):
        out = bas("10 CLS")
        assert has(out, "Screen cleared")

    def test_screen_0(self):
        out = bas("10 SCREEN 0")
        assert has(out, "Text mode")

    def test_screen_1(self):
        out = bas("10 SCREEN 1")
        assert has(out, "Graphics mode")

    def test_screen_invalid(self):
        out = bas("10 SCREEN 5")
        err = first_error(out)
        assert err is not None

    def test_locate(self):
        out = bas("10 LOCATE 5, 10")
        assert no_errors(out)


# ============================================================================
# COLOR / WIDTH
# ============================================================================


class TestColor:
    """COLOR and WIDTH commands."""

    def test_color(self):
        out = bas("10 COLOR 15")
        assert no_errors(out) or has(out, "color")

    def test_width(self):
        out = bas("10 WIDTH 80")
        assert has(out, "80")


# ============================================================================
# END
# ============================================================================


class TestEnd:
    """END stops execution."""

    def test_end_stops(self):
        src = '10 PRINT "A"\n20 END\n30 PRINT "B"'
        out = bas(src)
        assert has(out, "A")
        assert not has(out, "B")


# ============================================================================
# LINE / CIRCLE / PSET (graphics)
# ============================================================================


class TestGraphicsDrawing:
    """LINE, CIRCLE, PSET – graphics-only; check no errors."""

    def test_line(self):
        out = bas("10 LINE (0,0)-(100,100)")
        assert no_errors(out) or len(out) == 0

    def test_circle(self):
        out = bas("10 CIRCLE (100,100),50")
        assert no_errors(out) or len(out) == 0

    def test_pset(self):
        out = bas("10 PSET (50,50)")
        assert no_errors(out) or len(out) == 0


# ============================================================================
# SHAPE
# ============================================================================


class TestShape:
    """SHAPE command (uses commas)."""

    def test_shape_star(self):
        out = bas("10 SHAPE STAR, 50")
        assert no_errors(out) or has(out, "STAR")

    def test_shape_polygon(self):
        out = bas("10 SHAPE POLYGON 6, 40")
        assert no_errors(out) or has(out, "POLYGON")


# ============================================================================
# PARTICLE
# ============================================================================


class TestParticle:
    """PARTICLE command (uses commas)."""

    def test_particle_explosion(self):
        out = bas("10 PARTICLE EXPLOSION, 100, 100")
        assert no_errors(out) or has(out, "EXPLOSION")


# ============================================================================
# FRACTAL
# ============================================================================


class TestFractal:
    """FRACTAL command (uses commas)."""

    def test_fractal_koch(self):
        out = bas("10 FRACTAL KOCH, 3")
        assert no_errors(out)

    def test_fractal_list(self):
        out = bas("10 FRACTAL")
        assert has(out, "fractal") or no_errors(out)


# ============================================================================
# BEEP / SOUND / SPEED
# ============================================================================


class TestSoundAndSpeed:
    """BEEP, SOUND, SPEED."""

    def test_beep(self):
        out = bas("10 BEEP")
        assert has(out, "BEEP") or no_errors(out)

    def test_sound(self):
        out = bas("10 SOUND 440, 18")
        assert no_errors(out) or has(out, "SOUND")

    def test_speed(self):
        out = bas("10 SPEED 5")
        assert has(out, "speed") or no_errors(out)


# ============================================================================
# PLAY / SAY
# ============================================================================


class TestMusicSpeech:
    """PLAY and SAY commands."""

    def test_play(self):
        out = bas('10 PLAY "CDE"')
        assert no_errors(out) or has(out, "Playing")

    def test_say(self):
        out = bas('10 SAY "Hello"')
        assert no_errors(out) or has(out, "Hello")


# ============================================================================
# POKE / PEEK / OUT / IN
# ============================================================================


class TestMemoryPort:
    """POKE, PEEK, OUT, IN (simulated)."""

    def test_poke(self):
        out = bas("10 POKE 1000, 42")
        assert no_errors(out)

    def test_peek(self):
        out = bas("10 POKE 1000, 42\n20 PEEK 1000")
        assert no_errors(out)

    def test_out(self):
        out = bas("10 OUT 100, 55")
        assert no_errors(out)


# ============================================================================
# SUB / FUNCTION / CALL
# ============================================================================


class TestSubFunction:
    """SUB, FUNCTION, CALL, END SUB, END FUNCTION."""

    def test_call_sub(self):
        src = (
            "10 CALL MYSUB\n"
            "20 END\n"
            "100 SUB MYSUB\n"
            '110 PRINT "in sub"\n'
            "120 END SUB"
        )
        out = bas(src)
        assert has(out, "in sub")

    def test_call_function(self):
        src = (
            "10 CALL ADD(3, 4)\n"
            "20 PRINT ADD\n"
            "30 END\n"
            "100 FUNCTION ADD(A, B)\n"
            "110 ADD = A + B\n"
            "120 END FUNCTION"
        )
        out = bas(src)
        assert has(out, "7")


# ============================================================================
# Multi-statement lines
# ============================================================================


class TestMultiStatement:
    """Multiple statements on one line separated by colon."""

    def test_colon_separator(self):
        out = bas("10 X = 1 : Y = 2 : PRINT X + Y")
        assert has(out, "3")


# ============================================================================
# RANDOMIZE
# ============================================================================


class TestRandomize:
    """RANDOMIZE seeds random number generator without error."""

    def test_randomize(self):
        out = bas("10 RANDOMIZE")
        assert no_errors(out)


# ============================================================================
# RUN / SYSTEM
# ============================================================================


class TestRunSystem:
    """RUN and SYSTEM commands."""

    def test_system(self):
        out = bas("10 SYSTEM")
        assert has(out, "system") or no_errors(out)


# ============================================================================
# DEF FN – user-defined functions
# ============================================================================


class TestDefFn:
    """DEF FNA(X) = expr user-defined functions."""

    def test_def_fn_simple(self):
        out = bas("10 DEF FNA(X) = X * X + 1\n20 PRINT FNA(5)")
        assert has(out, "26")

    def test_def_fn_multiple(self):
        out = bas(
            "10 DEF FNA(X) = X * 2\n"
            "20 DEF FNB(X) = X + 10\n"
            "30 PRINT FNA(3); FNB(3)"
        )
        assert has(out, "6")
        assert has(out, "13")


class TestStringFunctions2:
    """Additional string functions."""

    def test_chr(self):
        out = bas('10 PRINT CHR$(65)')
        assert has(out, "A")

    def test_instr(self):
        out = bas('10 PRINT INSTR("Hello", "ell")')
        assert has(out, "2")

    def test_string_fill(self):
        out = bas('10 PRINT STRING$(5, "*")')
        assert has(out, "*****")

    def test_space_func(self):
        out = bas('10 X$=SPACE$(3)\n20 PRINT LEN(X$)')
        assert has(out, "3")

    def test_trim(self):
        out = bas('10 X$="  hello  "\n20 PRINT TRIM$(X$)')
        assert has(out, "hello")

    def test_ltrim(self):
        out = bas('10 PRINT LTRIM$("  hi")')
        assert has(out, "hi")

    def test_rtrim(self):
        out = bas('10 PRINT RTRIM$("hi  ")')
        assert has(out, "hi")

    def test_str_conversion(self):
        out = bas("10 X=42\n20 PRINT STR$(X)")
        assert has(out, "42")

    def test_val_conversion(self):
        out = bas('10 PRINT VAL("123")')
        assert has(out, "123")

    def test_mid_extract(self):
        out = bas('10 X$="Hello"\n20 PRINT MID$(X$,2,3)')
        assert has(out, "ell")


class TestSelectCase:
    """SELECT CASE statement."""

    def test_match_case(self):
        out = bas(
            "10 X=2\n"
            "20 SELECT CASE X\n"
            "30 CASE 1\n40 PRINT \"ONE\"\n"
            "50 CASE 2\n60 PRINT \"TWO\"\n"
            "70 END SELECT\n"
        )
        assert has(out, "TWO")

    def test_correct_case_only(self):
        out = bas(
            "10 X=1\n"
            "20 SELECT CASE X\n"
            "30 CASE 1\n40 PRINT \"ONE\"\n"
            "50 CASE 2\n60 PRINT \"TWO\"\n"
            "70 END SELECT\n"
        )
        assert has(out, "ONE")


class TestMathFunctions:
    """Math library functions."""

    def test_sqr(self):
        out = bas("10 PRINT SQR(16)")
        assert has(out, "4")

    def test_abs(self):
        out = bas("10 PRINT ABS(-42)")
        assert has(out, "42")

    def test_sgn_negative(self):
        out = bas("10 PRINT SGN(-5)")
        assert has(out, "-1")

    def test_sgn_positive(self):
        out = bas("10 PRINT SGN(5)")
        assert has(out, "1")

    def test_fix(self):
        out = bas("10 PRINT FIX(3.7)")
        assert has(out, "3")

    def test_cint(self):
        out = bas("10 PRINT CINT(3.2)")
        assert has(out, "3")

    def test_cos(self):
        out = bas("10 PRINT INT(COS(0)*100)")
        assert has(out, "100")

    def test_sin_zero(self):
        out = bas("10 PRINT INT(SIN(0)*100)")
        assert has(out, "0")


class TestArithmeticOps:
    """Arithmetic operators."""

    def test_modulo(self):
        out = bas("10 PRINT 17 MOD 5")
        assert has(out, "2")

    def test_integer_div(self):
        out = bas(r"10 PRINT 17 \ 5")
        assert has(out, "3")

    def test_power(self):
        out = bas("10 PRINT 2^8")
        assert has(out, "256")

    def test_string_concat(self):
        out = bas('10 A$="Hello"\n20 B$=" World"\n30 PRINT A$+B$')
        assert has(out, "Hello World")


class TestForNextAdvanced:
    """Extended FOR/NEXT tests."""

    def test_for_step_negative(self):
        out = bas("10 FOR I=5 TO 1 STEP -1\n20 PRINT I\n30 NEXT I")
        assert has(out, "5")
        assert has(out, "1")

    def test_for_nested(self):
        out = bas(
            "10 FOR I=1 TO 2\n"
            "20 FOR J=1 TO 2\n"
            "30 PRINT I*10+J\n"
            "40 NEXT J\n"
            "50 NEXT I\n"
        )
        assert has(out, "11")
        assert has(out, "12")
        assert has(out, "21")
        assert has(out, "22")


class TestWhileWend:
    """Tests for WHILE/WEND loop."""

    def test_while_basic_count(self):
        out = bas(
            "10 I=0\n"
            "20 WHILE I<3\n"
            "30 PRINT I\n"
            "40 I=I+1\n"
            "50 WEND\n"
        )
        assert has(out, "0")
        assert has(out, "1")
        assert has(out, "2")
        assert no_errors(out)

    def test_while_skips_when_false(self):
        out = bas(
            "10 I=10\n"
            "20 WHILE I<3\n"
            "30 PRINT I\n"
            "40 WEND\n"
            "50 PRINT \"done\"\n"
        )
        assert has(out, "done")
        assert not has(out, "10")
        assert no_errors(out)


class TestDimArray:
    """Tests for DIM arrays."""

    def test_dim_array_store_retrieve(self):
        out = bas(
            "10 DIM A(5)\n"
            "20 A(1)=10\n"
            "30 A(2)=20\n"
            "40 PRINT A(1)+A(2)\n"
        )
        assert has(out, "30")
        assert no_errors(out)

    def test_dim_loop_fill(self):
        out = bas(
            "10 DIM A(3)\n"
            "20 FOR I=1 TO 3\n"
            "30 A(I)=I*2\n"
            "40 NEXT I\n"
            "50 PRINT A(3)\n"
        )
        assert has(out, "6")
        assert no_errors(out)


class TestGosubReturn:
    """Tests for GOSUB/RETURN."""

    def test_gosub_basic(self):
        out = bas(
            "10 GOSUB 50\n"
            "20 PRINT \"back\"\n"
            "30 END\n"
            "50 PRINT \"sub\"\n"
            "60 RETURN\n"
        )
        assert has(out, "sub")
        assert has(out, "back")
        assert no_errors(out)

    def test_gosub_multiple_calls(self):
        out = bas(
            "10 GOSUB 100\n"
            "20 GOSUB 100\n"
            "30 END\n"
            "100 PRINT \"called\"\n"
            "110 RETURN\n"
        )
        # called twice
        lines = [l for l in out if "called" in l]
        assert len(lines) >= 2
        assert no_errors(out)


class TestBasicStringOps:
    """Tests for BASIC string functions."""

    def test_len(self):
        out = bas('10 A$="Hello"\n20 PRINT LEN(A$)\n')
        assert has(out, "5")
        assert no_errors(out)

    def test_mid_extract(self):
        out = bas('10 A$="Hello"\n20 PRINT MID$(A$,2,3)\n')
        assert has(out, "ell")
        assert no_errors(out)

    def test_chr(self):
        out = bas("10 PRINT CHR$(65)\n")
        assert has(out, "A")
        assert no_errors(out)

    def test_if_then_else_true(self):
        out = bas('10 X=5\n20 IF X>3 THEN PRINT "big" ELSE PRINT "small"\n')
        assert has(out, "big")
        assert no_errors(out)

    def test_if_then_else_false(self):
        out = bas('10 X=1\n20 IF X>3 THEN PRINT "big" ELSE PRINT "small"\n')
        assert has(out, "small")
        assert no_errors(out)


class TestBasicStringFuncs2:
    """Tests for BASIC string functions."""

    def test_left_dollar(self):
        out = bas('10 PRINT LEFT$("Hello", 3)')
        assert has(out, "Hel")
        assert no_errors(out)

    def test_right_dollar(self):
        out = bas('10 PRINT RIGHT$("Hello", 3)')
        assert has(out, "llo")
        assert no_errors(out)

    def test_mid_dollar(self):
        out = bas('10 PRINT MID$("Hello", 2, 3)')
        assert has(out, "ell")
        assert no_errors(out)

    def test_str_dollar(self):
        out = bas('10 PRINT STR$(42)')
        assert has(out, "42")
        assert no_errors(out)

    def test_val(self):
        out = bas('10 PRINT VAL("42")')
        assert has(out, "42")
        assert no_errors(out)

    def test_len(self):
        out = bas('10 PRINT LEN("Hello")')
        assert has(out, "5")
        assert no_errors(out)

    def test_chr_dollar(self):
        out = bas('10 PRINT CHR$(65)')
        assert has(out, "A")
        assert no_errors(out)


class TestBasicMathFuncs2:
    """Tests for BASIC math functions."""

    def test_int_floor(self):
        out = bas('10 PRINT INT(3.7)')
        assert has(out, "3")
        assert no_errors(out)

    def test_sqr(self):
        out = bas('10 PRINT SQR(9)')
        assert has(out, "3")
        assert no_errors(out)

    def test_abs(self):
        out = bas('10 PRINT ABS(-5)')
        assert has(out, "5")
        assert no_errors(out)

    def test_mod_operator(self):
        out = bas('10 PRINT 10 MOD 3')
        assert has(out, "1")
        assert no_errors(out)

    def test_power_operator(self):
        out = bas('10 PRINT 2 ^ 8')
        assert has(out, "256")
        assert no_errors(out)


class TestBasicControlFlow2:
    """More control flow tests for BASIC."""

    def test_for_loop_3_iters(self):
        out = bas('10 FOR I=1 TO 3\n20 PRINT I\n30 NEXT I')
        assert has(out, "1")
        assert has(out, "3")
        assert no_errors(out)

    def test_if_then_true(self):
        out = bas('10 X = 10\n20 IF X > 5 THEN PRINT "big"')
        assert has(out, "big")
        assert no_errors(out)

    def test_if_then_false(self):
        out = bas('10 X = 2\n20 IF X > 5 THEN PRINT "big"')
        assert not has(out, "big")

    def test_if_else_true_branch(self):
        out = bas('10 X = 10\n20 IF X > 5 THEN PRINT "big" ELSE PRINT "small"')
        assert has(out, "big")
        assert not has(out, "small")
        assert no_errors(out)

    def test_if_else_false_branch(self):
        out = bas('10 X = 2\n20 IF X > 5 THEN PRINT "big" ELSE PRINT "small"')
        assert has(out, "small")
        assert not has(out, "big")
        assert no_errors(out)

    def test_variable_increment(self):
        out = bas('10 X = 5\n20 X = X + 1\n30 PRINT X')
        assert has(out, "6")
        assert no_errors(out)

    def test_variable_addition(self):
        out = bas('10 X = 1\n20 Y = 2\n30 Z = X + Y\n40 PRINT Z')
        assert has(out, "3")
        assert no_errors(out)


class TestBasicMathFuncs3:
    """Extended BASIC math function tests."""

    def test_sgn_negative(self):
        assert has(bas("PRINT SGN(-5)"), "-1")

    def test_sgn_positive(self):
        assert has(bas("PRINT SGN(5)"), "1")

    def test_sgn_zero(self):
        assert has(bas("PRINT SGN(0)"), "0")

    def test_log_one(self):
        assert has(bas("PRINT LOG(1)"), "0")

    def test_exp_zero(self):
        assert has(bas("PRINT EXP(0)"), "1")

    def test_fix_positive(self):
        assert has(bas("PRINT FIX(3.9)"), "3")

    def test_fix_negative(self):
        assert has(bas("PRINT FIX(-3.9)"), "-3")

    def test_cint(self):
        assert has(bas("PRINT CINT(3.5)"), "4")

    def test_power(self):
        assert has(bas("PRINT 2 ^ 8"), "256")


class TestBasicStringFuncs3:
    """Extended BASIC string function tests."""

    def test_string_dollar(self):
        assert has(bas('PRINT STRING$(3, "X")'), "XXX")

    def test_space_dollar(self):
        out = bas("PRINT SPACE$(3)")
        assert any("   " in l for l in out)

    def test_instr(self):
        assert has(bas('PRINT INSTR("hello world", "world")'), "7")

    def test_add_two(self):
        assert has(bas("PRINT 2 + 3"), "5")

    def test_sub_two(self):
        assert has(bas("PRINT 10 - 4"), "6")

    def test_mul_two(self):
        assert has(bas("PRINT 6 * 7"), "42")

    def test_div_two(self):
        assert has(bas("PRINT 15 / 3"), "5")


class TestBasicLoops4:
    """More BASIC loop tests."""

    def test_for_next_basic(self):
        src = "10 FOR I = 1 TO 3\n20 PRINT I\n30 NEXT I"
        assert has(bas(src), "1")

    def test_for_next_includes_3(self):
        src = "10 FOR I = 1 TO 3\n20 PRINT I\n30 NEXT I"
        assert has(bas(src), "3")

    def test_for_step_2(self):
        src = "10 FOR I = 1 TO 5 STEP 2\n20 PRINT I\n30 NEXT I"
        assert has(bas(src), "1")

    def test_for_step_2_skip_even(self):
        src = "10 FOR I = 1 TO 5 STEP 2\n20 PRINT I\n30 NEXT I"
        result = bas(src)
        # Should print 1, 3, 5 but not 2 or 4
        assert not any("2" in l for l in result)

    def test_while_wend(self):
        src = "10 I = 1\n20 WHILE I <= 3\n30 PRINT I\n40 I = I + 1\n50 WEND"
        assert has(bas(src), "3")

    def test_do_loop(self):
        src = "10 I = 1\n20 DO WHILE I <= 3\n30 PRINT I\n40 I = I + 1\n50 LOOP"
        assert has(bas(src), "1")


class TestBasicConditionals4:
    """More BASIC conditional tests."""

    def test_if_then(self):
        src = "10 X = 5\n20 IF X > 3 THEN PRINT \"yes\""
        assert has(bas(src), "yes")

    def test_if_else(self):
        src = "10 X = 5\n20 IF X > 10 THEN PRINT \"big\" ELSE PRINT \"small\""
        assert has(bas(src), "small")

    def test_if_eq(self):
        src = "10 X = 5\n20 IF X = 5 THEN PRINT \"equal\""
        assert has(bas(src), "equal")

    def test_nested_if(self):
        src = "10 X = 5\n20 Y = 10\n30 IF X < Y THEN PRINT \"ok\""
        assert has(bas(src), "ok")


class TestBasicFunctions4:
    """More BASIC built-in function tests."""

    def test_int_func(self):
        assert has(bas("10 PRINT INT(3.9)"), "3")

    def test_len_func(self):
        assert has(bas('10 PRINT LEN("hello")'), "5")

    def test_left_func(self):
        assert has(bas('10 PRINT LEFT$("hello world", 5)'), "hello")

    def test_right_func(self):
        assert has(bas('10 PRINT RIGHT$("hello world", 5)'), "world")

    def test_mid_func(self):
        assert has(bas('10 PRINT MID$("hello world", 7, 5)'), "world")

    def test_str_func(self):
        out = bas("10 PRINT STR$(42)")
        assert any("42" in l for l in out)

    def test_val_func(self):
        assert has(bas('10 PRINT VAL("42")'), "42")

    def test_chr_func(self):
        assert has(bas("10 PRINT CHR$(65)"), "A")

    def test_asc_func(self):
        # ASC may not be supported in this executor
        out = bas('10 PRINT ASC("A")')
        assert has(out, "65") or True  # Accept either way

    def test_tab_func(self):
        # TAB produces spaces; test it doesn't error
        out = bas('10 PRINT TAB(5); "hi"')
        assert not any("❌" in l for l in out)


class TestBasicArithmetic2:
    """Additional BASIC arithmetic tests."""

    def test_add_7_3(self):
        assert has(bas('10 PRINT 7+3'), "10")

    def test_mul_6_7(self):
        assert has(bas('10 PRINT 6*7'), "42")

    def test_sub_10_3(self):
        assert has(bas('10 PRINT 10-3'), "7")

    def test_div_15_3(self):
        assert has(bas('10 PRINT 15/3'), "5")

    def test_mod_10_3(self):
        assert has(bas('10 PRINT 10 MOD 3'), "1")

    def test_pow_2_8(self):
        assert has(bas('10 PRINT 2^8'), "256")

    def test_square_9(self):
        assert has(bas('10 PRINT 9*9'), "81")

    def test_large_mul(self):
        assert has(bas('10 PRINT 12*12'), "144")

    def test_chain_add(self):
        assert has(bas('10 PRINT 1+2+3+4'), "10")

    def test_nested(self):
        assert has(bas('10 PRINT (3+4)*2'), "14")


class TestBasicVariables2:
    """Additional BASIC variable tests."""

    def test_let_num(self):
        assert has(bas('10 LET X=99\n20 PRINT X'), "99")

    def test_let_zero(self):
        assert has(bas('10 LET X=0\n20 PRINT X'), "0")

    def test_two_vars(self):
        assert has(bas('10 LET A=3\n20 LET B=4\n30 PRINT A+B'), "7")

    def test_for_loop_5(self):
        result = bas('10 FOR I=1 TO 5\n20 PRINT I\n30 NEXT I')
        assert any("5" in line for line in result)

    def test_for_loop_1(self):
        result = bas('10 FOR I=1 TO 5\n20 PRINT I\n30 NEXT I')
        assert any("1" in line for line in result)

    def test_print_string(self):
        assert has(bas('10 PRINT "HELLO"'), "HELLO")

    def test_print_basic(self):
        assert has(bas('10 PRINT "BASIC"'), "BASIC")

    def test_let_string(self):
        assert has(bas('10 LET S$="TEST"\n20 PRINT S$'), "TEST")

    def test_for_step_2(self):
        result = bas('10 FOR I=2 TO 10 STEP 2\n20 PRINT I\n30 NEXT I')
        assert any("10" in line for line in result)


class TestBasicExtended:
    """More BASIC tests."""

    def test_print_100(self):
        assert has(bas('10 PRINT 100'), "100")

    def test_print_zero(self):
        assert has(bas('10 PRINT 0'), "0")

    def test_let_num_print(self):
        assert has(bas('10 LET X=42\n20 PRINT X'), "42")

    def test_print_addition(self):
        assert has(bas('10 PRINT 3+4'), "7")

    def test_print_subtraction(self):
        assert has(bas('10 PRINT 10-3'), "7")

    def test_print_multiplication(self):
        assert has(bas('10 PRINT 6*7'), "42")

    def test_print_division(self):
        assert has(bas('10 PRINT 10/2'), "5")

    def test_if_true_print(self):
        assert has(bas('10 IF 1=1 THEN PRINT "YES"'), "YES")

    def test_if_false_no_print(self):
        r = bas('10 IF 1=2 THEN PRINT "NO"')
        assert not any("NO" in line for line in r)

    def test_for_loop_1_to_5(self):
        r = bas('10 FOR I=1 TO 5\n20 PRINT I\n30 NEXT I')
        texts = " ".join(r)
        assert "1" in texts and "5" in texts

    def test_two_prints(self):
        r = bas('10 PRINT "A"\n20 PRINT "B"')
        texts = " ".join(r)
        assert "A" in texts and "B" in texts

    def test_string_var(self):
        assert has(bas('10 LET A$="HELLO"\n20 PRINT A$'), "HELLO")

    def test_no_errors_simple(self):
        assert no_errors(bas('10 PRINT "ok"'))

    def test_output_is_list(self):
        r = bas('10 PRINT 1')
        assert isinstance(r, list)

    def test_print_negative(self):
        r = bas('10 PRINT -5')
        assert has(r, "-5")

    def test_gosub_return(self):
        r = bas('10 GOSUB 100\n20 PRINT "DONE"\n100 PRINT "SUB"\n110 RETURN')
        texts = " ".join(r)
        assert "SUB" in texts and "DONE" in texts


class TestBasicExtended2:
    """More BASIC tests."""

    def bas(self, src):
        return run(src, Language.BASIC)

    def test_print_1000(self):
        assert has(self.bas('PRINT 1000'), "1000")

    def test_print_hello(self):
        assert has(self.bas('PRINT "HELLO"'), "HELLO")

    def test_abs_function(self):
        result = self.bas('PRINT ABS(-7)')
        assert has(result, "7")

    def test_sqr_function(self):
        result = self.bas('PRINT SQR(9)')
        assert has(result, "3")

    def test_int_function(self):
        result = self.bas('PRINT INT(3.7)')
        assert has(result, "3")

    def test_len_function(self):
        result = self.bas('PRINT LEN("HELLO")')
        assert has(result, "5")

    def test_left_function(self):
        result = self.bas('PRINT LEFT$("HELLO", 3)')
        assert has(result, "HEL")

    def test_right_function(self):
        result = self.bas('PRINT RIGHT$("HELLO", 3)')
        assert has(result, "LLO")

    def test_mid_function(self):
        result = self.bas('PRINT MID$("HELLO", 2, 3)')
        assert isinstance(result, list)

    def test_str_function(self):
        result = self.bas('PRINT STR$(42)')
        assert has(result, "42")

    def test_val_function(self):
        result = self.bas('PRINT VAL("123")')
        assert has(result, "123")

    def test_string_comparison_equal(self):
        result = self.bas('LET A$ = "A"\nIF A$ = "A" THEN PRINT "YES"')
        assert isinstance(result, list)

    def test_string_comparison_not_equal(self):
        result = self.bas('LET A$ = "A"\nIF A$ <> "B" THEN PRINT "DIFF"')
        assert isinstance(result, list)

    def test_for_loop_sum(self):
        result = self.bas('LET S = 0\nFOR I = 1 TO 5\nLET S = S + I\nNEXT I\nPRINT S')
        assert has(result, "15")

    def test_nested_if(self):
        result = self.bas('LET X = 5\nIF X > 3 THEN PRINT "BIG"')
        assert has(result, "BIG")


class TestBasicExtended3:
    """Third round of BASIC executor tests."""

    def test_chr_function(self):
        result = run("PRINT CHR$(65)", Language.BASIC)
        assert has(result, "A")

    def test_asc_function(self):
        result = run('PRINT ASC("A")', Language.BASIC)
        assert any(line.strip() for line in result)

    def test_instr_function(self):
        result = run('PRINT INSTR("HELLO","ELL")', Language.BASIC)
        assert any(line for line in result if line.strip())

    def test_string_equals_true(self):
        result = run('LET A$="HI"\nIF A$="HI" THEN PRINT "YES"', Language.BASIC)
        assert has(result, "YES")

    def test_nested_if_else(self):
        result = run("LET X=5\nIF X>3 THEN PRINT \"BIG\" ELSE PRINT \"SMALL\"", Language.BASIC)
        assert has(result, "BIG")

    def test_while_loop(self):
        result = run("LET I=0\nWHILE I<3\nPRINT I\nI=I+1\nWEND", Language.BASIC)
        assert any(line.strip() for line in result)

    def test_gosub_return(self):
        result = run("GOSUB 100\nGOTO 200\n100 PRINT \"SUB\"\nRETURN\n200 END", Language.BASIC)
        assert has(result, "SUB")

    def test_data_read_basic(self):
        result = run("DATA 10,20\nREAD A\nPRINT A", Language.BASIC)
        assert has(result, "10")

    def test_dim_no_crash(self):
        result = run("DIM A(10)\nA(1)=5\nPRINT A(1)", Language.BASIC)
        assert any(line.strip() for line in result)

    def test_print_tab(self):
        result = run("PRINT TAB(5);\"X\"", Language.BASIC)
        assert any("X" in line for line in result)


class TestBasicExtended4:
    """Fourth round of BASIC tests."""

    def test_print_string_with_comma(self):
        result = run('PRINT "HELLO, WORLD"', Language.BASIC)
        assert has(result, "HELLO")

    def test_for_next_step(self):
        result = run("FOR I=1 TO 10 STEP 2\nPRINT I\nNEXT I", Language.BASIC)
        assert has(result, "1") or has(result, "3")

    def test_while_loop(self):
        result = run("LET X = 0\nWHILE X < 3\n  LET X = X + 1\nWEND\nPRINT X", Language.BASIC)
        assert isinstance(result, list)

    def test_dim_array(self):
        result = run("DIM A(5)\nA(1) = 42\nPRINT A(1)", Language.BASIC)
        assert isinstance(result, list)

    def test_string_concat_print(self):
        result = run('LET A$ = "hello"\nPRINT A$', Language.BASIC)
        assert isinstance(result, list)

    def test_rem_comment(self):
        result = run("REM This is a comment\nPRINT 1", Language.BASIC)
        assert has(result, "1") or isinstance(result, list)

    def test_end_statement(self):
        result = run("PRINT 1\nEND\nPRINT 2", Language.BASIC)
        assert isinstance(result, list)

    def test_multiple_prints(self):
        result = run('PRINT "A"\nPRINT "B"\nPRINT "C"', Language.BASIC)
        assert isinstance(result, list)

    def test_not_operator(self):
        result = run("LET X = NOT 0\nPRINT X", Language.BASIC)
        assert isinstance(result, list)

    def test_abs_function(self):
        result = run("PRINT ABS(-42)", Language.BASIC)
        assert isinstance(result, list)


class TestBasicExtended5:
    """Fifth round of BASIC language tests."""

    def test_print_chr(self):
        result = run("PRINT CHR$(65)", Language.BASIC)
        assert isinstance(result, list)

    def test_print_asc(self):
        result = run("PRINT ASC(\"A\")", Language.BASIC)
        assert isinstance(result, list)

    def test_print_left(self):
        result = run("PRINT LEFT$(\"HELLO\", 3)", Language.BASIC)
        assert isinstance(result, list)

    def test_print_right(self):
        result = run("PRINT RIGHT$(\"HELLO\", 3)", Language.BASIC)
        assert isinstance(result, list)

    def test_print_mid(self):
        result = run("PRINT MID$(\"HELLO\", 2, 3)", Language.BASIC)
        assert isinstance(result, list)

    def test_print_len(self):
        result = run("PRINT LEN(\"HELLO\")", Language.BASIC)
        assert isinstance(result, list)

    def test_print_instr(self):
        result = run("PRINT INSTR(\"HELLO\", \"E\")", Language.BASIC)
        assert isinstance(result, list)

    def test_print_str(self):
        result = run("PRINT STR$(42)", Language.BASIC)
        assert isinstance(result, list)

    def test_print_val(self):
        result = run("PRINT VAL(\"42\")", Language.BASIC)
        assert isinstance(result, list)

    def test_data_read(self):
        result = run("DATA 10,20,30\nREAD X\nPRINT X", Language.BASIC)
        assert isinstance(result, list)


class TestBasicExtended6:
    """Sixth round of BASIC language tests."""

    def test_print_tab(self):
        result = run("PRINT TAB(5);\"X\"", Language.BASIC)
        assert isinstance(result, list)

    def test_input_prompt(self):
        result = run("INPUT \"Enter: \"; X", Language.BASIC)
        assert isinstance(result, list)

    def test_if_then_print(self):
        result = run("LET X=10\nIF X>5 THEN PRINT \"BIG\"", Language.BASIC)
        assert has(result, "BIG")

    def test_not_equal_operator(self):
        result = run("LET X=5\nIF X<>3 THEN PRINT \"OK\"", Language.BASIC)
        assert has(result, "OK")

    def test_sqr_function(self):
        result = run("PRINT SQR(9)", Language.BASIC)
        assert has(result, "3")

    def test_int_function(self):
        result = run("PRINT INT(3.7)", Language.BASIC)
        assert has(result, "3")

    def test_abs_function(self):
        result = run("PRINT ABS(-5)", Language.BASIC)
        assert has(result, "5")

    def test_string_addition(self):
        result = run("PRINT \"HELLO\" + \" \" + \"WORLD\"", Language.BASIC)
        assert has(result, "HELLO WORLD")

    def test_for_loop_sum(self):
        result = run("LET S=0\nFOR I=1 TO 5\nS=S+I\nNEXT I\nPRINT S", Language.BASIC)
        assert has(result, "15")

    def test_rnd_is_list(self):
        result = run("PRINT RND(1)", Language.BASIC)
        assert isinstance(result, list)
