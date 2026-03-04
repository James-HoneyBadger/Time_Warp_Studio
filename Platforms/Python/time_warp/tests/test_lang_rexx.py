"""Comprehensive tests for the REXX language executor."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors

L = Language.REXX


def rexx(source: str, **kw) -> list[str]:
    """Shortcut: run a REXX program."""
    return run(source, L, **kw)


# ============================================================================
# SAY (output)
# ============================================================================


class TestSay:
    def test_hello_world(self):
        out = rexx('SAY "Hello World"')
        assert has(out, "Hello World")

    def test_say_number(self):
        out = rexx("SAY 42")
        assert has(out, "42")

    def test_say_expression(self):
        out = rexx("SAY 3 + 4")
        assert has(out, "7")

    def test_say_variable(self):
        out = rexx("x = 42\nSAY x")
        assert has(out, "42")

    def test_say_concatenation(self):
        out = rexx('a = "Hello"\nb = "World"\nSAY a b')
        assert has(out, "Hello") and has(out, "World")


# ============================================================================
# VARIABLES / ASSIGNMENT
# ============================================================================


class TestVariables:
    def test_numeric(self):
        out = rexx("x = 42\nSAY x")
        assert has(out, "42")

    def test_string(self):
        out = rexx('x = "Hello"\nSAY x')
        assert has(out, "Hello")

    def test_expression_assignment(self):
        out = rexx("x = 2 + 3\nSAY x")
        assert has(out, "5")


# ============================================================================
# ARITHMETIC
# ============================================================================


class TestArithmetic:
    def test_add(self):
        out = rexx("SAY 2 + 3")
        assert has(out, "5")

    def test_subtract(self):
        out = rexx("SAY 10 - 4")
        assert has(out, "6")

    def test_multiply(self):
        out = rexx("SAY 6 * 7")
        assert has(out, "42")

    def test_divide(self):
        out = rexx("SAY 20 / 4")
        assert has(out, "5")

    def test_integer_divide(self):
        out = rexx("SAY 10 % 3")
        assert no_errors(out)

    def test_power(self):
        out = rexx("SAY 2 ** 10")
        assert has(out, "1024")


# ============================================================================
# IF / ELSE
# ============================================================================


class TestConditional:
    def test_if_true(self):
        out = rexx('x = 5\nIF x > 3 THEN\n  SAY "yes"\nEND')
        assert has(out, "yes")

    def test_if_else(self):
        out = rexx('x = 1\nIF x > 3 THEN\n  SAY "big"\nELSE\n  SAY "small"\nEND')
        assert has(out, "small")

    def test_nested_if(self):
        out = rexx(
            'x = 5\nIF x > 3 THEN\n  IF x > 10 THEN\n    SAY "huge"\n  ELSE\n    SAY "medium"\n  END\nEND'
        )
        assert has(out, "medium")


# ============================================================================
# DO LOOP
# ============================================================================


class TestDoLoop:
    def test_do_count(self):
        out = rexx("DO i = 1 TO 3\n  SAY i\nEND")
        assert has(out, "1") and has(out, "3")

    def test_do_while(self):
        out = rexx("i = 0\nDO WHILE i < 3\n  SAY i\n  i = i + 1\nEND")
        assert has(out, "0") and has(out, "2")

    def test_do_until(self):
        out = rexx("i = 0\nDO UNTIL i >= 3\n  SAY i\n  i = i + 1\nEND")
        assert has(out, "0") and has(out, "2")

    def test_do_forever_leave(self):
        out = rexx(
            "i = 0\nDO FOREVER\n  IF i >= 3 THEN LEAVE\n  SAY i\n  i = i + 1\nEND"
        )
        assert has(out, "0") and has(out, "2") and not has(out, "3")

    def test_iterate(self):
        out = rexx("DO i = 1 TO 5\n  IF i = 3 THEN ITERATE\n  SAY i\nEND")
        assert has(out, "1") and has(out, "4") and not has(out, "3")


# ============================================================================
# STRING BUILTINS
# ============================================================================


class TestStringBuiltins:
    def test_length(self):
        out = rexx('SAY LENGTH("Hello")')
        assert has(out, "5")

    def test_substr(self):
        out = rexx('SAY SUBSTR("Hello", 2, 3)')
        assert has(out, "ell")

    def test_upper(self):
        out = rexx('SAY UPPER("hello")')
        assert has(out, "HELLO")

    def test_pos(self):
        out = rexx('SAY POS("ll", "Hello")')
        assert no_errors(out)

    def test_left(self):
        out = rexx('SAY LEFT("Hello", 3)')
        assert has(out, "Hel")

    def test_right(self):
        out = rexx('SAY RIGHT("Hello", 3)')
        assert has(out, "llo")

    def test_reverse(self):
        out = rexx('SAY REVERSE("Hello")')
        assert has(out, "olleH")

    def test_copies(self):
        out = rexx('SAY COPIES("ab", 3)')
        assert has(out, "ababab")

    def test_strip(self):
        out = rexx('SAY STRIP("  Hi  ")')
        assert has(out, "Hi")

    def test_word(self):
        out = rexx('SAY WORD("Hello World Foo", 2)')
        assert has(out, "World")

    def test_words(self):
        out = rexx('SAY WORDS("Hello World Foo")')
        assert has(out, "3")


# ============================================================================
# NUMERIC BUILTINS
# ============================================================================


class TestNumericBuiltins:
    def test_abs(self):
        out = rexx("SAY ABS(-5)")
        assert has(out, "5")

    def test_max(self):
        out = rexx("SAY MAX(1, 5, 3)")
        assert has(out, "5")

    def test_min(self):
        out = rexx("SAY MIN(1, 5, 3)")
        assert has(out, "1")

    def test_trunc(self):
        out = rexx("SAY TRUNC(3.7)")
        assert has(out, "3")


# ============================================================================
# CALL / PROCEDURE
# ============================================================================


class TestProcedures:
    def test_call_internal(self):
        out = rexx("CALL greet\nEXIT\n" 'greet:\n  SAY "hello"\n  RETURN')
        assert has(out, "hello")


# ============================================================================
# EXIT
# ============================================================================


class TestExit:
    def test_exit(self):
        out = rexx('SAY "before"\nEXIT\nSAY "after"')
        assert has(out, "before") and not has(out, "after")


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_empty_program(self):
        out = rexx("")
        assert no_errors(out) or len(out) == 0
