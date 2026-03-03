"""Comprehensive tests for the SQR language executor."""

import pytest

from time_warp.core.interpreter import Language

from .conftest_lang import run, ok, has, no_errors, first_error

L = Language.SQR


def sqr(source: str, **kw) -> list[str]:
    """Shortcut: run an SQR program."""
    return run(source, L, **kw)


# ============================================================================
# PRINT / DISPLAY / SHOW
# ============================================================================


class TestOutput:
    def test_print(self):
        out = sqr("begin-program\n  print 'Hello World' ()\nend-program")
        assert has(out, "Hello World")

    def test_display(self):
        out = sqr("begin-program\n  display 'Hello'\nend-program")
        assert has(out, "Hello")

    def test_show(self):
        out = sqr("begin-program\n  show 'Testing'\nend-program")
        assert has(out, "Testing")

    def test_multiple_print(self):
        out = sqr(
            "begin-program\n"
            "  print 'A' ()\n"
            "  print 'B' ()\n"
            "end-program"
        )
        assert has(out, "A") and has(out, "B")


# ============================================================================
# VARIABLES (LET)
# ============================================================================


class TestVariables:
    def test_numeric_var(self):
        out = sqr("begin-program\n  let #x = 42\n  display #x\nend-program")
        assert has(out, "42")

    def test_string_var(self):
        out = sqr("begin-program\n  let $s = 'Hello'\n  display $s\nend-program")
        assert has(out, "Hello")

    def test_expression(self):
        out = sqr("begin-program\n  let #x = 2 + 3\n  display #x\nend-program")
        assert has(out, "5")

    def test_string_expression(self):
        out = sqr(
            "begin-program\n"
            "  let $a = 'Hello'\n"
            "  let $b = 'World'\n"
            "  display $a\n"
            "  display $b\n"
            "end-program"
        )
        assert has(out, "Hello") and has(out, "World")


# ============================================================================
# MOVE
# ============================================================================


class TestMove:
    def test_move_numeric(self):
        out = sqr(
            "begin-program\n"
            "  move 42 to #x\n"
            "  display #x\n"
            "end-program"
        )
        assert has(out, "42")

    def test_move_string(self):
        out = sqr(
            "begin-program\n"
            "  move 'Hello' to $s\n"
            "  display $s\n"
            "end-program"
        )
        assert has(out, "Hello")


# ============================================================================
# ARITHMETIC
# ============================================================================


class TestArithmetic:
    def test_add(self):
        out = sqr("begin-program\n  let #x = 2 + 3\n  display #x\nend-program")
        assert has(out, "5")

    def test_subtract(self):
        out = sqr("begin-program\n  let #x = 10 - 4\n  display #x\nend-program")
        assert has(out, "6")

    def test_multiply(self):
        out = sqr("begin-program\n  let #x = 6 * 7\n  display #x\nend-program")
        assert has(out, "42")

    def test_divide(self):
        out = sqr("begin-program\n  let #x = 20 / 4\n  display #x\nend-program")
        assert has(out, "5")


# ============================================================================
# IF / ELSE / END-IF
# ============================================================================


class TestConditionals:
    def test_if_true(self):
        out = sqr(
            "begin-program\n"
            "  let #x = 5\n"
            "  if #x > 3\n"
            "    display 'yes'\n"
            "  end-if\n"
            "end-program"
        )
        assert has(out, "yes")

    def test_if_else(self):
        out = sqr(
            "begin-program\n"
            "  let #x = 1\n"
            "  if #x > 3\n"
            "    display 'big'\n"
            "  else\n"
            "    display 'small'\n"
            "  end-if\n"
            "end-program"
        )
        assert has(out, "small")


# ============================================================================
# WHILE / END-WHILE
# ============================================================================


class TestWhile:
    def test_while_loop(self):
        out = sqr(
            "begin-program\n"
            "  let #i = 1\n"
            "  while #i <= 3\n"
            "    display #i\n"
            "    let #i = #i + 1\n"
            "  end-while\n"
            "end-program"
        )
        assert has(out, "1") and has(out, "3")


# ============================================================================
# PROCEDURES (DO / BEGIN-PROCEDURE / END-PROCEDURE)
# ============================================================================


class TestProcedures:
    def test_procedure_call(self):
        out = sqr(
            "begin-program\n"
            "  do greet\n"
            "end-program\n"
            "begin-procedure greet\n"
            "  display 'hello'\n"
            "end-procedure"
        )
        assert has(out, "hello")

    def test_procedure_with_var(self):
        out = sqr(
            "begin-program\n"
            "  let #x = 42\n"
            "  do show-val\n"
            "end-program\n"
            "begin-procedure show-val\n"
            "  display #x\n"
            "end-procedure"
        )
        assert has(out, "42")


# ============================================================================
# STRING OPERATIONS
# ============================================================================


class TestStringOps:
    def test_string_concat(self):
        out = sqr(
            "begin-program\n"
            "  let $a = 'Hello'\n"
            "  let $b = 'World'\n"
            "  string $a ' ' $b into $c\n"
            "  display $c\n"
            "end-program"
        )
        assert has(out, "Hello World")


# ============================================================================
# INPUT
# ============================================================================


class TestInput:
    def test_input(self):
        out = sqr(
            "begin-program\n"
            "  input $name 'Enter name'\n"
            "  display $name\n"
            "end-program",
            input_val="Alice"
        )
        assert has(out, "Alice") or no_errors(out)


# ============================================================================
# EVALUATE / WHEN
# ============================================================================


class TestEvaluate:
    def test_evaluate(self):
        out = sqr(
            "begin-program\n"
            "  let #x = 2\n"
            "  evaluate #x\n"
            "    when 1\n"
            "      display 'one'\n"
            "    when 2\n"
            "      display 'two'\n"
            "    when-other\n"
            "      display 'other'\n"
            "  end-evaluate\n"
            "end-program"
        )
        assert has(out, "two")


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_empty_program(self):
        out = sqr("")
        assert no_errors(out) or len(out) == 0
