"""Comprehensive tests for the APL language executor."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors

L = Language.APL


def apl(source: str, **kw) -> list[str]:
    """Shortcut: run an APL program."""
    return run(source, L, **kw)


# ============================================================================
# OUTPUT (⎕←)
# ============================================================================


class TestOutput:
    def test_display_string(self):
        out = apl("⎕← 'Hello World'")
        assert has(out, "Hello World")

    def test_display_number(self):
        out = apl("⎕← 42")
        assert has(out, "42")

    def test_display_expression(self):
        out = apl("⎕← 2 + 3")
        assert has(out, "5")

    def test_implicit_output(self):
        # Bare expression should produce output
        out = apl("42")
        assert has(out, "42") or no_errors(out)


# ============================================================================
# ASSIGNMENT
# ============================================================================


class TestAssignment:
    def test_assign_number(self):
        out = apl("X ← 42\n⎕← X")
        assert has(out, "42")

    def test_assign_string(self):
        out = apl("S ← 'Hello'\n⎕← S")
        assert has(out, "Hello")

    def test_assign_expression(self):
        out = apl("X ← 2 + 3\n⎕← X")
        assert has(out, "5")


# ============================================================================
# ARITHMETIC
# ============================================================================


class TestArithmetic:
    def test_add(self):
        out = apl("⎕← 2 + 3")
        assert has(out, "5")

    def test_subtract(self):
        out = apl("⎕← 10 - 4")
        assert has(out, "6")

    def test_multiply(self):
        out = apl("⎕← 6 × 7")
        assert has(out, "42")

    def test_divide(self):
        out = apl("⎕← 20 ÷ 4")
        assert has(out, "5")

    def test_power(self):
        out = apl("⎕← 2 * 10")
        assert has(out, "1024")

    def test_ceiling(self):
        out = apl("⎕← ⌈ 3.2")
        assert has(out, "4")

    def test_floor(self):
        out = apl("⎕← ⌊ 3.7")
        assert has(out, "3")

    def test_abs(self):
        out = apl("⎕← | -5")
        assert has(out, "5")

    def test_negate(self):
        out = apl("⎕← - 5")
        assert has(out, "-5")


# ============================================================================
# IOTA (⍳)
# ============================================================================


class TestIota:
    def test_iota(self):
        out = apl("⎕← ⍳ 5")
        assert has(out, "1") and has(out, "5")

    def test_iota_small(self):
        out = apl("⎕← ⍳ 3")
        assert has(out, "1") and has(out, "2") and has(out, "3")


# ============================================================================
# REDUCE (+/)
# ============================================================================


class TestReduce:
    def test_sum_reduce(self):
        out = apl("⎕← +/ 1 2 3 4 5")
        assert has(out, "15")

    def test_product_reduce(self):
        out = apl("⎕← ×/ 1 2 3 4")
        assert has(out, "24")

    def test_max_reduce(self):
        out = apl("⎕← ⌈/ 3 1 4 1 5")
        assert has(out, "5")


# ============================================================================
# REVERSE (⌽)
# ============================================================================


class TestReverse:
    def test_reverse(self):
        out = apl("⎕← ⌽ 1 2 3")
        assert has(out, "3") and has(out, "1")


# ============================================================================
# SHAPE (⍴)
# ============================================================================


class TestShape:
    def test_shape_of(self):
        out = apl("⎕← ⍴ 1 2 3 4 5")
        assert has(out, "5")

    def test_reshape(self):
        out = apl("⎕← 2 3 ⍴ 1 2 3 4 5 6")
        assert no_errors(out)


# ============================================================================
# CONDITIONALS (:If / :Else / :EndIf)
# ============================================================================


class TestConditionals:
    def test_if_true(self):
        out = apl(":If 5 > 3\n  ⎕← 'yes'\n:EndIf")
        assert has(out, "yes")

    def test_if_else(self):
        out = apl(":If 1 > 3\n  ⎕← 'yes'\n:Else\n  ⎕← 'no'\n:EndIf")
        assert has(out, "no")


# ============================================================================
# FOR LOOP (:For / :EndFor)
# ============================================================================


class TestForLoop:
    def test_for_loop(self):
        out = apl(":For i :In 1 2 3\n  ⎕← i\n:EndFor")
        assert has(out, "1") and has(out, "3")


# ============================================================================
# COMPARISON
# ============================================================================


class TestComparison:
    def test_equal(self):
        out = apl("⎕← 5 = 5")
        assert has(out, "1")

    def test_not_equal(self):
        out = apl("⎕← 5 ≠ 3")
        assert has(out, "1")

    def test_less_than(self):
        out = apl("⎕← 3 < 5")
        assert has(out, "1")

    def test_greater_than(self):
        out = apl("⎕← 5 > 3")
        assert has(out, "1")


# ============================================================================
# ARRAY OPERATIONS
# ============================================================================


class TestArrayOps:
    def test_vector(self):
        out = apl("⎕← 1 2 3 4 5")
        assert has(out, "1") and has(out, "5")

    def test_element_wise_add(self):
        out = apl("⎕← 1 2 3 + 10 20 30")
        assert has(out, "11") and has(out, "33")

    def test_element_wise_multiply(self):
        out = apl("⎕← 2 3 4 × 5 6 7")
        assert has(out, "10") and has(out, "28")

    def test_scalar_add(self):
        out = apl("⎕← 10 + 1 2 3")
        assert has(out, "11") and has(out, "13")


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_empty_program(self):
        out = apl("")
        assert no_errors(out) or len(out) == 0
