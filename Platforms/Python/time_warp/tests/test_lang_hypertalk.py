"""Comprehensive tests for the HyperTalk language executor."""


from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors

L = Language.HYPERTALK


def ht(source: str, **kw) -> list[str]:
    """Shortcut: run a HyperTalk program."""
    return run(source, L, **kw)


# ============================================================================
# OUTPUT (answer / say)
# ============================================================================


class TestOutput:
    def test_answer_string(self):
        out = ht('answer "Hello World"')
        assert has(out, "Hello World")

    def test_say_string(self):
        out = ht('say "Hello World"')
        assert has(out, "Hello World")

    def test_answer_number(self):
        out = ht("answer 42")
        assert has(out, "42")

    def test_answer_expression(self):
        out = ht("answer 3 + 4")
        assert has(out, "7")


# ============================================================================
# PUT / VARIABLES
# ============================================================================


class TestVariables:
    def test_put_into(self):
        out = ht('put 42 into x\nanswer x')
        assert has(out, "42")

    def test_put_string(self):
        out = ht('put "Hello" into s\nanswer s')
        assert has(out, "Hello")

    def test_put_after(self):
        out = ht('put "Hello" into s\nput " World" after s\nanswer s')
        assert has(out, "Hello World")

    def test_put_before(self):
        out = ht('put "World" into s\nput "Hello " before s\nanswer s')
        assert has(out, "Hello World")

    def test_get(self):
        out = ht("get 42\nanswer it")
        assert has(out, "42")


# ============================================================================
# ARITHMETIC
# ============================================================================


class TestArithmetic:
    def test_add(self):
        out = ht("answer 2 + 3")
        assert has(out, "5")

    def test_subtract(self):
        out = ht("answer 10 - 4")
        assert has(out, "6")

    def test_multiply(self):
        out = ht("answer 6 * 7")
        assert has(out, "42")

    def test_divide(self):
        out = ht("answer 20 / 4")
        assert has(out, "5")

    def test_mod(self):
        out = ht("answer 10 mod 3")
        assert has(out, "1")

    def test_power(self):
        out = ht("answer 2 ^ 10")
        assert has(out, "1024")


# ============================================================================
# IF / THEN / ELSE
# ============================================================================


class TestConditionals:
    def test_if_true(self):
        out = ht('if 5 > 3 then answer "yes"')
        assert has(out, "yes")

    def test_if_else(self):
        out = ht('if 1 > 3 then\n  answer "yes"\nelse\n  answer "no"\nend if')
        assert has(out, "no")

    def test_if_then_block(self):
        out = ht('if 5 > 3 then\n  answer "A"\n  answer "B"\nend if')
        assert has(out, "A") and has(out, "B")


# ============================================================================
# REPEAT
# ============================================================================


class TestRepeat:
    def test_repeat_times(self):
        out = ht('repeat 3 times\n  answer "X"\nend repeat')
        assert has(out, "X")

    def test_repeat_with(self):
        out = ht("repeat with i = 1 to 3\n  answer i\nend repeat")
        assert has(out, "1") and has(out, "3")

    def test_repeat_while(self):
        out = ht("put 0 into i\nrepeat while i < 3\n  answer i\n  put i + 1 into i\nend repeat")
        assert has(out, "0") and has(out, "2")

    def test_repeat_until(self):
        out = ht("put 0 into i\nrepeat until i >= 3\n  answer i\n  put i + 1 into i\nend repeat")
        assert has(out, "0") and has(out, "2")


# ============================================================================
# HANDLERS (on ... end)
# ============================================================================


class TestHandlers:
    def test_handler(self):
        out = ht(
            'on greet\n  answer "hello"\nend greet\n'
            "greet"
        )
        assert has(out, "hello")

    def test_handler_with_param(self):
        out = ht(
            "on double n\n  answer n * 2\nend double\n"
            "double 5"
        )
        assert has(out, "10")


# ============================================================================
# STRING FUNCTIONS
# ============================================================================


class TestStringFunctions:
    def test_length(self):
        out = ht('answer the length of "Hello"')
        assert has(out, "5")

    def test_char_of(self):
        out = ht('answer char 1 of "Hello"')
        assert has(out, "H")

    def test_word_of(self):
        out = ht('answer word 2 of "Hello World"')
        assert has(out, "World")

    def test_number_of_chars(self):
        out = ht('answer the number of chars in "Hello"')
        assert has(out, "5")

    def test_number_of_words(self):
        out = ht('answer the number of words in "Hello World Foo"')
        assert has(out, "3")


# ============================================================================
# MATH FUNCTIONS
# ============================================================================


class TestMathFunctions:
    def test_sqrt(self):
        out = ht("answer sqrt(16)")
        assert has(out, "4")

    def test_abs(self):
        out = ht("answer abs(-5)")
        assert has(out, "5")

    def test_round(self):
        out = ht("answer round(3.7)")
        assert has(out, "4")

    def test_random(self):
        out = ht("answer random(100)")
        assert no_errors(out)

    def test_sin(self):
        out = ht("answer sin(0)")
        assert has(out, "0")

    def test_cos(self):
        out = ht("answer cos(0)")
        assert has(out, "1")


# ============================================================================
# ASK (input)
# ============================================================================


class TestInput:
    def test_ask(self):
        out = ht('ask "Enter value"\nanswer it', input_val="42")
        assert has(out, "42") or no_errors(out)


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_empty_program(self):
        out = ht("")
        assert no_errors(out) or len(out) == 0


# ============================================================================
# PUT (message box output)
# ============================================================================


class TestPutOutput:
    """put value (no target) emits to message box."""

    def test_put_emits(self):
        out = ht("put 42")
        assert has(out, "42")

    def test_put_in_repeat(self):
        out = ht("repeat with i = 1 to 3\n  put i\nend repeat")
        assert has(out, "1", "2", "3")
