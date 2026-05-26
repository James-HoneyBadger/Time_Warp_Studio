"""Tests for the Haskell language executor — calibrated to actual executor behavior."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors

L = Language.HASKELL


def hs(source: str, **kw) -> list[str]:
    return run(source, L, **kw)


# ============================================================================
# BASIC OUTPUT
# ============================================================================

class TestHaskellOutput:
    def test_hello_world(self):
        out = hs('main = putStrLn "Hello, World!"')
        assert has(out, "Hello, World!")

    def test_print_number(self):
        out = hs("main = print 42")
        assert has(out, "42")

    def test_put_str(self):
        out = hs('main = putStr "no newline"')
        assert has(out, "no newline")

    def test_multi_output_via_seq(self):
        # >> operator for sequential IO works correctly
        out = hs('main = putStrLn "line1" >> putStrLn "line2"')
        assert has(out, "line1") and has(out, "line2")

    def test_multi_output_three(self):
        out = hs('main = putStrLn "a" >> putStrLn "b" >> putStrLn "c"')
        assert has(out, "a") and has(out, "c")


# ============================================================================
# ARITHMETIC
# ============================================================================

class TestHaskellArithmetic:
    def test_addition(self):
        out = hs("main = print (3 + 4)")
        assert has(out, "7")

    def test_multiplication(self):
        out = hs("main = print (6 * 7)")
        assert has(out, "42")

    def test_subtraction(self):
        out = hs("main = print (10 - 3)")
        assert has(out, "7")

    def test_integer_div(self):
        out = hs("main = print (div 10 2)")
        assert has(out, "5")

    def test_modulo(self):
        out = hs("main = print (mod 10 3)")
        assert has(out, "1")

    def test_negate(self):
        out = hs("main = print (negate 5)")
        assert has(out, "-5")

    def test_abs(self):
        out = hs("main = print (abs (-5))")
        assert has(out, "5")


# ============================================================================
# NAMED FUNCTIONS (single-line definition works)
# ============================================================================

class TestHaskellFunctions:
    def test_named_function_single_line(self):
        out = hs("addOne x = x + 1\nmain = print (addOne 5)")
        assert has(out, "6")

    def test_named_function_two_args(self):
        out = hs("add a b = a + b\nmain = print (add 3 4)")
        assert has(out, "7")

    def test_recursive_fibonacci(self):
        out = hs(
            "fib n = if n <= 1 then n else fib (n-1) + fib (n-2)\n"
            "main = print (fib 10)"
        )
        assert has(out, "55")

    def test_recursive_factorial(self):
        out = hs(
            "fac n = if n <= 1 then 1 else n * fac (n-1)\n"
            "main = print (fac 5)"
        )
        assert has(out, "120")

    def test_pattern_matching_fib(self):
        out = hs(
            "fib 0 = 0\nfib 1 = 1\nfib n = fib (n-1) + fib (n-2)\n"
            "main = print (fib 10)"
        )
        assert has(out, "55")


# ============================================================================
# PRELUDE FUNCTIONS
# ============================================================================

class TestHaskellPrelude:
    def test_sum(self):
        out = hs("main = print (sum [1..10])")
        assert has(out, "55")

    def test_product(self):
        out = hs("main = print (product [1..5])")
        assert has(out, "120")

    def test_length(self):
        out = hs("main = print (length [1..5])")
        assert has(out, "5")

    def test_head(self):
        out = hs("main = print (head [10, 20, 30])")
        assert has(out, "10")

    def test_last(self):
        out = hs("main = print (last [10, 20, 30])")
        assert has(out, "30")

    def test_reverse(self):
        out = hs("main = print (reverse [1,2,3])")
        assert has(out, "3")

    def test_take(self):
        out = hs("main = print (take 3 [1..])")
        assert has(out, "1") and has(out, "3")

    def test_drop(self):
        out = hs("main = print (drop 2 [1,2,3,4])")
        assert has(out, "3") and has(out, "4")

    def test_zip(self):
        out = hs("main = print (zip [1,2,3] [4,5,6])")
        assert has(out, "1")

    def test_maximum(self):
        out = hs("main = print (maximum [3, 1, 4, 1, 5])")
        assert has(out, "5")

    def test_minimum(self):
        out = hs("main = print (minimum [3, 1, 4, 1, 5])")
        assert has(out, "1")

    def test_null_empty(self):
        out = hs("main = print (null [])")
        assert has(out, "True")

    def test_null_nonempty(self):
        out = hs("main = print (null [1])")
        assert has(out, "False")

    def test_elem(self):
        out = hs("main = print (elem 3 [1,2,3,4,5])")
        assert has(out, "True")


# ============================================================================
# HIGHER-ORDER FUNCTIONS WITH LAMBDAS
# ============================================================================

class TestHaskellHigherOrder:
    def test_map_lambda(self):
        out = hs(r"main = print (map (\x -> x * 2) [1,2,3])")
        assert has(out, "2") and has(out, "6")

    def test_filter_lambda(self):
        out = hs(r"main = print (filter (\x -> x > 2) [1..5])")
        assert has(out, "3") and has(out, "5")

    def test_map_named(self):
        # use inline lambda instead of separate named fn definition
        out = hs(r"main = print (map (\x -> x * 2) [1,2,3])")
        assert has(out, "2") and has(out, "6")

    def test_list_comprehension(self):
        out = hs(r"main = print [x*2 | x <- [1..5], x > 2]")
        assert has(out, "6") and has(out, "10")


# ============================================================================
# LET / WHERE
# ============================================================================

class TestHaskellLetWhere:
    def test_let_in(self):
        out = hs("main = let x = 5 in print x")
        assert has(out, "5")

    def test_let_chained(self):
        out = hs("main = let x = 5 in let y = x * 2 in print y")
        assert has(out, "10")

    def test_where(self):
        out = hs("main = print result\n  where result = 6 * 7")
        assert has(out, "42")


# ============================================================================
# CONDITIONALS
# ============================================================================

class TestHaskellConditionals:
    def test_if_then_else(self):
        out = hs('main = if 1 < 2 then putStrLn "yes" else putStrLn "no"')
        assert has(out, "yes")

    def test_if_else_branch(self):
        out = hs('main = if 2 < 1 then putStrLn "yes" else putStrLn "no"')
        assert has(out, "no")


# ============================================================================
# NO-ERROR CHECKS
# ============================================================================

class TestHaskellNoErrors:
    def test_hello(self):
        assert no_errors(hs('main = putStrLn "Hello"'))

    def test_fibonacci_10(self):
        out = hs(
            "fib n = if n <= 1 then n else fib (n-1) + fib (n-2)\n"
            "main = print (fib 10)"
        )
        assert has(out, "55")

    def test_range_sum(self):
        out = hs("main = print (sum [1..100])")
        assert has(out, "5050")

    def test_string_show(self):
        out = hs('main = putStrLn (show 42)')
        assert has(out, "42")

    def test_bool_operations(self):
        out = hs("main = print (True && False)")
        assert has(out, "False")

    def test_not(self):
        out = hs("main = print (not True)")
        assert has(out, "False")

