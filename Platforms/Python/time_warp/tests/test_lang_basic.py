"""Comprehensive tests for the BASIC language executor."""

import pytest

from time_warp.core.interpreter import Language

from .conftest_lang import run, ok, has, no_errors, first_error

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
        out = bas('10 INPUT A$\n20 PRINT A$', input_val="hello")
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
        src = (
            "10 X = 1\n"
            "20 IF X = 1 THEN\n"
            '30 PRINT "ok"\n'
            "40 END IF"
        )
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
        src = (
            '10 PRINT "A"\n'
            "20 GOTO 40\n"
            '30 PRINT "B"\n'
            '40 PRINT "C"'
        )
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
        src = (
            "10 X = 0\n"
            "20 WHILE X < 3\n"
            "30 X = X + 1\n"
            "40 WEND\n"
            "50 PRINT X"
        )
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
            "10 X = 0\n"
            "20 DO\n"
            "30 X = X + 1\n"
            "40 LOOP UNTIL X = 3\n"
            "50 PRINT X"
        )
        out = bas(src)
        assert has(out, "3")

    def test_do_while_loop(self):
        src = (
            "10 X = 0\n"
            "20 DO WHILE X < 2\n"
            "30 X = X + 1\n"
            "40 LOOP\n"
            "50 PRINT X"
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
        src = (
            "10 DIM A(5)\n"
            "20 A(0) = 99\n"
            "30 PRINT A(0)"
        )
        out = bas(src)
        assert has(out, "99")

    def test_dim_string_array(self):
        src = (
            '10 DIM N$(3)\n'
            '20 N$(0) = "hi"\n'
            "30 PRINT N$(0)"
        )
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
        out = bas('10 X = 1 : Y = 2 : PRINT X + Y')
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
