"""Comprehensive tests for the Smalltalk language executor."""


from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors

L = Language.SMALLTALK


def st(source: str, **kw) -> list[str]:
    """Shortcut: run a Smalltalk program."""
    return run(source, L, **kw)


# ============================================================================
# TRANSCRIPT OUTPUT
# ============================================================================


class TestOutput:
    def test_showCr(self):
        out = st("Transcript showCr: 'Hello World'.")
        assert has(out, "Hello World")

    def test_show(self):
        out = st("Transcript show: 'Hello'.")
        assert has(out, "Hello")

    def test_multiple_output(self):
        out = st("Transcript showCr: 'A'.\nTranscript showCr: 'B'.")
        assert has(out, "A") and has(out, "B")

    def test_show_number(self):
        out = st("Transcript showCr: 42.")
        assert has(out, "42")


# ============================================================================
# VARIABLES / ASSIGNMENT
# ============================================================================


class TestVariables:
    def test_assignment(self):
        out = st("| x |\nx := 42.\nTranscript showCr: x.")
        assert has(out, "42")

    def test_string_assignment(self):
        out = st("| s |\ns := 'Hello'.\nTranscript showCr: s.")
        assert has(out, "Hello")

    def test_multiple_vars(self):
        out = st("| a b |\na := 1.\nb := 2.\nTranscript showCr: a + b.")
        assert has(out, "3")


# ============================================================================
# ARITHMETIC
# ============================================================================


class TestArithmetic:
    def test_add(self):
        out = st("Transcript showCr: 2 + 3.")
        assert has(out, "5")

    def test_subtract(self):
        out = st("Transcript showCr: 10 - 4.")
        assert has(out, "6")

    def test_multiply(self):
        out = st("Transcript showCr: 6 * 7.")
        assert has(out, "42")

    def test_divide(self):
        out = st("Transcript showCr: 20 / 4.")
        assert has(out, "5")

    def test_modulo(self):
        out = st("Transcript showCr: 10 \\\\ 3.")
        assert no_errors(out)


# ============================================================================
# CONDITIONALS (ifTrue: / ifFalse:)
# ============================================================================


class TestConditionals:
    def test_ifTrue(self):
        out = st("5 > 3 ifTrue: [Transcript showCr: 'yes'].")
        assert has(out, "yes")

    def test_ifFalse(self):
        out = st("1 > 3 ifFalse: [Transcript showCr: 'no'].")
        assert has(out, "no")

    def test_ifTrue_ifFalse(self):
        out = st("1 > 3 ifTrue: [Transcript showCr: 'yes'] ifFalse: [Transcript showCr: 'no'].")
        assert has(out, "no")


# ============================================================================
# LOOPS
# ============================================================================


class TestLoops:
    def test_timesRepeat(self):
        out = st("3 timesRepeat: [Transcript showCr: 'X'].")
        assert has(out, "X")

    def test_to_do(self):
        out = st("1 to: 3 do: [:i | Transcript showCr: i].")
        assert has(out, "1") and has(out, "3")

    def test_whileTrue(self):
        out = st(
            "| i |\ni := 0.\n"
            "[i < 3] whileTrue: [Transcript showCr: i. i := i + 1]."
        )
        assert has(out, "0") and has(out, "2")

    def test_to_by_do(self):
        out = st("0 to: 10 by: 2 do: [:i | Transcript showCr: i].")
        assert has(out, "0") and has(out, "4")


# ============================================================================
# BLOCKS
# ============================================================================


class TestBlocks:
    def test_block_value(self):
        out = st("| b |\nb := [42].\nTranscript showCr: b value.")
        assert has(out, "42")

    def test_block_with_arg(self):
        out = st("| b |\nb := [:x | x * 2].\nTranscript showCr: (b value: 5).")
        assert has(out, "10")


# ============================================================================
# COLLECTIONS
# ============================================================================


class TestCollections:
    def test_array(self):
        out = st("| a |\na := #(1 2 3).\nTranscript showCr: (a at: 1).")
        assert has(out, "1")

    def test_array_size(self):
        out = st("Transcript showCr: #(1 2 3) size.")
        assert has(out, "3")

    def test_do_each(self):
        out = st("#(10 20 30) do: [:each | Transcript showCr: each].")
        assert has(out, "10") and has(out, "30")

    def test_collect(self):
        out = st("Transcript showCr: (#(1 2 3) collect: [:x | x * 2]).")
        assert no_errors(out)

    def test_select(self):
        out = st("Transcript showCr: (#(1 2 3 4) select: [:x | x > 2]).")
        assert no_errors(out)


# ============================================================================
# STRING MESSAGES
# ============================================================================


class TestStringOps:
    def test_size(self):
        out = st("Transcript showCr: 'Hello' size.")
        assert has(out, "5")

    def test_reversed(self):
        out = st("Transcript showCr: 'Hello' reversed.")
        assert has(out, "olleH")

    def test_asUppercase(self):
        out = st("Transcript showCr: 'hello' asUppercase.")
        assert has(out, "HELLO")

    def test_asLowercase(self):
        out = st("Transcript showCr: 'HELLO' asLowercase.")
        assert has(out, "hello")

    def test_at(self):
        out = st("Transcript showCr: ('Hello' at: 1).")
        assert no_errors(out)


# ============================================================================
# NUMBER MESSAGES
# ============================================================================


class TestNumberOps:
    def test_abs(self):
        out = st("Transcript showCr: -5 abs.")
        assert has(out, "5")

    def test_max(self):
        out = st("Transcript showCr: (3 max: 7).")
        assert has(out, "7")

    def test_min(self):
        out = st("Transcript showCr: (3 min: 7).")
        assert has(out, "3")

    def test_sqrt(self):
        out = st("Transcript showCr: 16 sqrt.")
        assert has(out, "4")

    def test_factorial(self):
        out = st("Transcript showCr: 5 factorial.")
        assert has(out, "120")

    def test_even(self):
        out = st("Transcript showCr: 4 even.")
        assert no_errors(out)

    def test_odd(self):
        out = st("Transcript showCr: 3 odd.")
        assert no_errors(out)


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_empty_program(self):
        out = st("")
        assert no_errors(out) or len(out) == 0
