"""Comprehensive tests for the Forth language executor."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors, first_error

L = Language.FORTH


def fth(source: str, **kw) -> list[str]:
    """Shortcut: run a Forth program."""
    return run(source, L, **kw)


# ============================================================================
# OUTPUT — ." and .
# ============================================================================


class TestOutput:
    def test_dot_quote(self):
        out = fth('." Hello World"')
        assert has(out, "Hello World")

    def test_dot_print_number(self):
        out = fth("42 .")
        assert has(out, "42")

    def test_cr(self):
        out = fth('." Line1" CR ." Line2"')
        assert has(out, "Line1") and has(out, "Line2")

    def test_emit(self):
        out = fth("65 EMIT")  # ASCII 'A'
        assert has(out, "A")

    def test_space(self):
        out = fth("SPACE")
        assert no_errors(out)

    def test_spaces(self):
        out = fth("5 SPACES")
        assert no_errors(out)


# ============================================================================
# STACK OPERATIONS
# ============================================================================


class TestStackOps:
    def test_dup(self):
        out = fth("5 DUP . .")
        assert has(out, "5")

    def test_drop(self):
        out = fth("1 2 DROP .")
        assert has(out, "1")

    def test_swap(self):
        out = fth("1 2 SWAP . .")
        assert has(out, "1") and has(out, "2")

    def test_over(self):
        out = fth("1 2 OVER . . .")
        assert no_errors(out)

    def test_rot(self):
        out = fth("1 2 3 ROT . . .")
        assert no_errors(out)

    def test_nip(self):
        out = fth("1 2 NIP .")
        assert has(out, "2")

    def test_tuck(self):
        out = fth("1 2 TUCK . . .")
        assert no_errors(out)

    def test_2dup(self):
        out = fth("3 4 2DUP . . . .")
        assert no_errors(out)

    def test_2drop(self):
        out = fth("1 2 3 4 2DROP . .")
        assert no_errors(out)

    def test_2swap(self):
        out = fth("1 2 3 4 2SWAP . . . .")
        assert no_errors(out)

    def test_depth(self):
        out = fth("1 2 3 DEPTH .")
        assert has(out, "3")

    def test_dot_s(self):
        out = fth("1 2 3 .S")
        assert has(out, "1") and has(out, "2") and has(out, "3")


# ============================================================================
# ARITHMETIC
# ============================================================================


class TestArithmetic:
    def test_add(self):
        out = fth("3 4 + .")
        assert has(out, "7")

    def test_subtract(self):
        out = fth("10 3 - .")
        assert has(out, "7")

    def test_multiply(self):
        out = fth("6 7 * .")
        assert has(out, "42")

    def test_divide(self):
        out = fth("20 4 / .")
        assert has(out, "5")

    def test_mod(self):
        out = fth("10 3 MOD .")
        assert has(out, "1")

    def test_divmod(self):
        out = fth("10 3 /MOD . .")
        assert no_errors(out)

    def test_negate(self):
        out = fth("5 NEGATE .")
        assert has(out, "-5")

    def test_abs(self):
        out = fth("-5 ABS .")
        assert has(out, "5")

    def test_max(self):
        out = fth("3 7 MAX .")
        assert has(out, "7")

    def test_min(self):
        out = fth("3 7 MIN .")
        assert has(out, "3")

    def test_one_plus(self):
        out = fth("5 1+ .")
        assert has(out, "6")

    def test_one_minus(self):
        out = fth("5 1- .")
        assert has(out, "4")

    def test_star_two(self):
        out = fth("5 2* .")
        assert has(out, "10")


# ============================================================================
# COMPARISON
# ============================================================================


class TestComparison:
    def test_equal(self):
        out = fth("5 5 = .")
        assert has(out, "-1")  # TRUE = -1 in Forth

    def test_not_equal(self):
        out = fth("5 3 <> .")
        assert has(out, "-1")

    def test_less_than(self):
        out = fth("3 5 < .")
        assert has(out, "-1")

    def test_greater_than(self):
        out = fth("5 3 > .")
        assert has(out, "-1")

    def test_zero_equal(self):
        out = fth("0 0= .")
        assert has(out, "-1")

    def test_zero_less(self):
        out = fth("-1 0< .")
        assert has(out, "-1")


# ============================================================================
# LOGIC
# ============================================================================


class TestLogic:
    def test_and(self):
        out = fth("-1 -1 AND .")
        assert has(out, "-1")

    def test_or(self):
        out = fth("-1 0 OR .")
        assert has(out, "-1")

    def test_xor(self):
        out = fth("-1 -1 XOR .")
        assert has(out, "0")

    def test_invert(self):
        out = fth("0 INVERT .")
        assert has(out, "-1")


# ============================================================================
# WORD DEFINITIONS (: ... ;)
# ============================================================================


class TestWordDefinitions:
    def test_define_word(self):
        out = fth(": SQUARE DUP * ;")
        assert has(out, "Defined") or no_errors(out)

    def test_use_defined_word(self):
        out = fth(": SQUARE DUP * ;\n5 SQUARE .")
        assert has(out, "25")

    def test_nested_definitions(self):
        out = fth(": SQUARE DUP * ;\n: CUBE DUP SQUARE * ;\n3 CUBE .")
        assert has(out, "27")


# ============================================================================
# VARIABLES / CONSTANTS / VALUES
# ============================================================================


class TestVariablesAndConstants:
    def test_variable(self):
        out = fth("VARIABLE X\n42 X !\nX @ .")
        assert has(out, "42")

    def test_constant(self):
        out = fth("42 CONSTANT ANSWER\nANSWER .")
        assert has(out, "42")

    def test_value(self):
        out = fth("42 VALUE X\nX .")
        assert has(out, "42")

    def test_value_to(self):
        out = fth("42 VALUE X\n99 TO X\nX .")
        assert has(out, "99")


# ============================================================================
# IF / ELSE / THEN
# ============================================================================


class TestConditional:
    def test_if_true(self):
        out = fth(': TEST 1 IF ." yes" THEN ;\nTEST')
        assert has(out, "yes")

    def test_if_false(self):
        out = fth(': TEST 0 IF ." yes" THEN ;\nTEST')
        assert not has(out, "yes")

    def test_if_else(self):
        out = fth(': TEST 0 IF ." yes" ELSE ." no" THEN ;\nTEST')
        assert has(out, "no")


# ============================================================================
# DO / LOOP
# ============================================================================


class TestDoLoop:
    def test_do_loop(self):
        out = fth(": TEST 5 0 DO I . LOOP ;\nTEST")
        assert has(out, "0") and has(out, "4")

    def test_do_plus_loop(self):
        out = fth(": TEST 10 0 DO I . 2 +LOOP ;\nTEST")
        assert has(out, "0") and has(out, "2") and has(out, "4")

    def test_leave(self):
        out = fth(": TEST 10 0 DO I 5 = IF LEAVE THEN I . LOOP ;\nTEST")
        assert has(out, "0") and not has(out, "6")


# ============================================================================
# BEGIN / UNTIL / WHILE / REPEAT / AGAIN
# ============================================================================


class TestBeginLoops:
    def test_begin_until(self):
        out = fth(": TEST 0 BEGIN DUP . 1+ DUP 3 > UNTIL DROP ;\nTEST")
        assert has(out, "0") and has(out, "3")

    def test_begin_while_repeat(self):
        out = fth(": TEST 0 BEGIN DUP 3 < WHILE DUP . 1+ REPEAT DROP ;\nTEST")
        assert has(out, "0") and has(out, "2")


# ============================================================================
# RETURN STACK
# ============================================================================


class TestReturnStack:
    def test_to_r_from_r(self):
        out = fth("5 >R R> .")
        assert has(out, "5")


# ============================================================================
# BASE
# ============================================================================


class TestBase:
    def test_hex(self):
        out = fth("HEX FF DECIMAL .")
        assert has(out, "255")

    def test_decimal(self):
        out = fth("DECIMAL 42 .")
        assert has(out, "42")


# ============================================================================
# MEMORY
# ============================================================================


class TestMemory:
    def test_here(self):
        out = fth("HERE .")
        assert no_errors(out)

    def test_allot(self):
        out = fth("HERE 10 ALLOT HERE SWAP - .")
        assert has(out, "10")


# ============================================================================
# FLOATING POINT
# ============================================================================


class TestFloatingPoint:
    def test_float_literal(self):
        out = fth("3.14 F.")
        assert has(out, "3.14")

    def test_float_add(self):
        out = fth("1.5 2.5 F+ F.")
        assert has(out, "4")

    def test_float_multiply(self):
        out = fth("2.0 3.0 F* F.")
        assert has(out, "6")

    def test_float_subtract(self):
        out = fth("5.0 2.0 F- F.")
        assert has(out, "3")


# ============================================================================
# COMMENTS
# ============================================================================


class TestComments:
    def test_backslash_comment(self):
        out = fth("\\ This is a comment\n42 .")
        assert has(out, "42")

    def test_paren_comment(self):
        out = fth("( This is a comment ) 42 .")
        assert has(out, "42")


# ============================================================================
# BOOLEAN CONSTANTS
# ============================================================================


class TestBooleans:
    def test_true(self):
        out = fth("TRUE .")
        assert has(out, "-1")

    def test_false(self):
        out = fth("FALSE .")
        assert has(out, "0")


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_unknown_word(self):
        out = fth("XYZZY")
        assert first_error(out) is not None

    def test_stack_underflow(self):
        out = fth("+")
        assert first_error(out) is not None
