"""Comprehensive tests for the Haskell language executor."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors

L = Language.HASKELL


def hs(source: str, **kw) -> list[str]:
    """Shortcut: run a Haskell program."""
    return run(source, L, **kw)


# ============================================================================
# putStrLn / putStr / print
# ============================================================================


class TestOutput:
    def test_putStrLn(self):
        out = hs('main = do\n  putStrLn "Hello World"')
        assert has(out, "Hello World")

    def test_putStr(self):
        out = hs('main = do\n  putStr "Hello"')
        assert has(out, "Hello")

    def test_print_number(self):
        out = hs("main = do\n  print 42")
        assert has(out, "42")

    def test_print_expression(self):
        out = hs("main = do\n  print (3 + 4)")
        assert has(out, "7")

    def test_multiple_putStrLn(self):
        out = hs('main = do\n  putStrLn "A"\n  putStrLn "B"')
        assert has(out, "A") and has(out, "B")


# ============================================================================
# LET BINDINGS
# ============================================================================


class TestLetBindings:
    def test_let_int(self):
        out = hs("main = do\n  let x = 42\n  print x")
        assert has(out, "42")

    def test_let_string(self):
        out = hs('main = do\n  let s = "Hello"\n  putStrLn s')
        assert has(out, "Hello")

    def test_let_expression(self):
        out = hs("main = do\n  let x = 2 + 3\n  print x")
        assert has(out, "5")

    def test_let_multiple(self):
        out = hs("main = do\n  let a = 10\n  let b = 20\n  print (a + b)")
        assert has(out, "30")


# ============================================================================
# ARITHMETIC
# ============================================================================


class TestArithmetic:
    def test_add(self):
        out = hs("main = print (2 + 3)")
        assert has(out, "5")

    def test_subtract(self):
        out = hs("main = print (10 - 4)")
        assert has(out, "6")

    def test_multiply(self):
        out = hs("main = print (6 * 7)")
        assert has(out, "42")

    def test_divide(self):
        out = hs("main = print (div 10 3)")
        assert has(out, "3")

    def test_modulo(self):
        out = hs("main = print (mod 10 3)")
        assert has(out, "1")

    def test_power(self):
        out = hs("main = print (2 ^ 10)")
        assert has(out, "1024")


# ============================================================================
# IF-THEN-ELSE
# ============================================================================


class TestConditionals:
    def test_if_true(self):
        out = hs('main = putStrLn (if 5 > 3 then "yes" else "no")')
        assert has(out, "yes")

    def test_if_false(self):
        out = hs('main = putStrLn (if 1 > 3 then "yes" else "no")')
        assert has(out, "no")


# ============================================================================
# FUNCTION DEFINITIONS
# ============================================================================


class TestFunctions:
    def test_simple_function(self):
        out = hs("double x = x * 2\nmain = print (double 5)")
        assert has(out, "10")

    def test_multi_arg_function(self):
        out = hs("add a b = a + b\nmain = print (add 3 4)")
        assert has(out, "7")

    def test_recursive(self):
        out = hs(
            "fact n = if n <= 1 then 1 else n * fact (n - 1)\nmain = print (fact 5)"
        )
        assert has(out, "120")

    def test_where_clause(self):
        out = hs("main = print result\n  where result = 2 + 3")
        assert has(out, "5")


# ============================================================================
# LAMBDA
# ============================================================================


class TestLambda:
    def test_lambda(self):
        out = hs("main = print ((\\x -> x * 2) 5)")
        assert has(out, "10")


# ============================================================================
# LIST OPERATIONS
# ============================================================================


class TestLists:
    def test_list_literal(self):
        out = hs("main = print [1, 2, 3]")
        assert has(out, "1") and has(out, "3")

    def test_head(self):
        out = hs("main = print (head [1, 2, 3])")
        assert has(out, "1")

    def test_tail(self):
        out = hs("main = print (tail [1, 2, 3])")
        assert has(out, "2") and has(out, "3")

    def test_length(self):
        out = hs("main = print (length [1, 2, 3])")
        assert has(out, "3")

    def test_sum(self):
        out = hs("main = print (sum [1, 2, 3, 4])")
        assert has(out, "10")

    def test_product(self):
        out = hs("main = print (product [1, 2, 3, 4])")
        assert has(out, "24")

    def test_reverse(self):
        out = hs("main = print (reverse [1, 2, 3])")
        assert has(out, "3") and has(out, "1")

    def test_map(self):
        out = hs("main = print (map (* 2) [1, 2, 3])")
        assert has(out, "2") and has(out, "6")

    def test_filter(self):
        out = hs("main = print (filter (> 3) [1, 2, 3, 4, 5])")
        assert no_errors(out)

    def test_range(self):
        out = hs("main = print [1..5]")
        assert has(out, "1") and has(out, "5")

    def test_take(self):
        out = hs("main = print (take 3 [1, 2, 3, 4, 5])")
        assert has(out, "1") and has(out, "3")

    def test_drop(self):
        out = hs("main = print (drop 2 [1, 2, 3, 4, 5])")
        assert has(out, "3") and has(out, "5")

    def test_zip(self):
        out = hs("main = print (zip [1, 2] [3, 4])")
        assert no_errors(out)

    def test_concat(self):
        out = hs("main = print ([1, 2] ++ [3, 4])")
        assert has(out, "1") and has(out, "4")

    def test_elem(self):
        out = hs("main = print (elem 2 [1, 2, 3])")
        assert no_errors(out)

    def test_null(self):
        out = hs("main = print (null [])")
        assert no_errors(out)


# ============================================================================
# FOLD
# ============================================================================


class TestFold:
    def test_foldl(self):
        out = hs("main = print (foldl (+) 0 [1, 2, 3])")
        assert has(out, "6")

    def test_foldr(self):
        out = hs("main = print (foldr (+) 0 [1, 2, 3])")
        assert has(out, "6")


# ============================================================================
# STRING OPERATIONS
# ============================================================================


class TestStrings:
    def test_string_concat(self):
        out = hs('main = putStrLn ("Hello" ++ " " ++ "World")')
        assert has(out, "Hello World")

    def test_show(self):
        out = hs("main = putStrLn (show 42)")
        assert has(out, "42")

    def test_words(self):
        out = hs('main = print (words "Hello World")')
        assert has(out, "Hello") and has(out, "World")

    def test_unwords(self):
        out = hs('main = putStrLn (unwords ["Hello", "World"])')
        assert has(out, "Hello World")


# ============================================================================
# COMPARISON
# ============================================================================


class TestComparison:
    def test_equal(self):
        out = hs("main = print (5 == 5)")
        assert no_errors(out)

    def test_not_equal(self):
        out = hs("main = print (5 /= 3)")
        assert no_errors(out)

    def test_less_than(self):
        out = hs("main = print (3 < 5)")
        assert no_errors(out)

    def test_greater_than(self):
        out = hs("main = print (5 > 3)")
        assert no_errors(out)


# ============================================================================
# MATH FUNCTIONS
# ============================================================================


class TestMathFunctions:
    def test_abs(self):
        out = hs("main = print (abs (-5))")
        assert has(out, "5")

    def test_max(self):
        out = hs("main = print (max 3 7)")
        assert has(out, "7")

    def test_min(self):
        out = hs("main = print (min 3 7)")
        assert has(out, "3")

    def test_sqrt(self):
        out = hs("main = print (sqrt 16.0)")
        assert has(out, "4")

    def test_even(self):
        out = hs("main = print (even 4)")
        assert no_errors(out)

    def test_odd(self):
        out = hs("main = print (odd 3)")
        assert no_errors(out)


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_empty_program(self):
        out = hs("")
        assert no_errors(out) or len(out) == 0


# ============================================================================
# List comprehension with inner list literals
# ============================================================================


class TestListCompFix:
    """List comprehension generators with nested commas."""

    def test_list_comp_with_inner_list(self):
        out = hs("main = print [x*2 | x <- [1,2,3]]")
        assert has(out, "2", "4", "6")


# ============================================================================
# Right-section + listcomp with range (regression)
# ============================================================================


class TestRightSection:
    """Regression: (*2) right sections without space."""

    def test_map_star_2(self):
        out = hs("main = print (map (*2) [1,2,3])")
        assert has(out, "2", "4", "6")


class TestListCompRange:
    """Regression: list comp with range generator [x*x | x <- [1..5]]."""

    def test_listcomp_with_dotdot_range(self):
        out = hs("main = print [x*x | x <- [1..5]]")
        assert has(out, "1", "4", "9", "16", "25")
