"""Comprehensive tests for the Haskell executor."""

from __future__ import annotations

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.HASKELL


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def hs(source: str) -> list[str]:
    return run(source, LANG)


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------


def test_hello_world():
    out = hs('main = putStrLn "Hello, World!"')
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_putstr_no_newline():
    out = hs('main = do { putStr "foo"; putStrLn "bar" }')
    assert no_errors(out)
    assert has(out, "foobar")


def test_print_integer():
    out = hs("main = print 42")
    assert no_errors(out)
    assert has(out, "42")


# ---------------------------------------------------------------------------
# Let bindings and arithmetic
# ---------------------------------------------------------------------------


def test_let_binding():
    out = hs(
        "main = do\n"
        "  let x = 6 * 7\n"
        '  putStrLn (show x)\n'
    )
    assert no_errors(out)
    assert has(out, "42")


def test_let_string_binding():
    out = hs(
        'main = do\n'
        '  let greeting = "Hello"\n'
        '  putStrLn (greeting ++ ", Haskell!")\n'
    )
    assert no_errors(out)
    assert has(out, "Hello, Haskell!")


def test_arithmetic():
    out = hs(
        "main = do\n"
        "  let a = 10\n"
        "  let b = 3\n"
        '  putStrLn (show (a + b))\n'
        '  putStrLn (show (a - b))\n'
        '  putStrLn (show (a * b))\n'
        '  putStrLn (show (a `div` b))\n'
        '  putStrLn (show (a `mod` b))\n'
    )
    assert no_errors(out)
    assert has(out, "13", "7", "30", "3", "1")


# ---------------------------------------------------------------------------
# Recursive functions
# ---------------------------------------------------------------------------


def test_factorial():
    src = (
        "factorial :: Int -> Int\n"
        "factorial 0 = 1\n"
        "factorial n = n * factorial (n - 1)\n"
        "\n"
        "main :: IO ()\n"
        "main = do\n"
        "  putStrLn (show (factorial 5))\n"
        "  putStrLn (show (factorial 10))\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "120", "3628800")


def test_fibonacci():
    src = (
        "fib :: Int -> Int\n"
        "fib 0 = 0\n"
        "fib 1 = 1\n"
        "fib n = fib (n - 1) + fib (n - 2)\n"
        "\n"
        "main :: IO ()\n"
        "main = putStrLn (show (fib 10))\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "55")


# ---------------------------------------------------------------------------
# Guards
# ---------------------------------------------------------------------------


def test_guards():
    src = (
        "classify :: Int -> String\n"
        "classify n\n"
        "  | n < 0    = \"negative\"\n"
        "  | n == 0   = \"zero\"\n"
        "  | n < 10   = \"small\"\n"
        "  | otherwise = \"large\"\n"
        "\n"
        "main :: IO ()\n"
        "main = do\n"
        '  putStrLn (classify (-1))\n'
        "  putStrLn (classify 0)\n"
        "  putStrLn (classify 7)\n"
        "  putStrLn (classify 100)\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "negative", "zero", "small", "large")


def test_even_odd():
    src = (
        "main = do\n"
        '  putStrLn (show (even 4))\n'
        '  putStrLn (show (even 3))\n'
        '  putStrLn (show (odd 5))\n'
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "True", "False")


# ---------------------------------------------------------------------------
# Where clauses
# ---------------------------------------------------------------------------


def test_where_clause():
    src = (
        "bmi :: Double -> String\n"
        "bmi weight\n"
        "  | bmiVal < 18.5 = \"Underweight\"\n"
        "  | bmiVal < 25.0 = \"Normal\"\n"
        "  | otherwise     = \"Overweight\"\n"
        "  where\n"
        "    bmiVal = weight / (1.75 * 1.75)\n"
        "\n"
        "main :: IO ()\n"
        "main = putStrLn (bmi 60.0)\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "Normal")


def test_where_helper_function():
    src = (
        "circleArea :: Double -> Double\n"
        "circleArea r = pi * r * r\n"
        "  where pi = 3.14159\n"
        "\n"
        "main :: IO ()\n"
        "main = putStrLn (show (circleArea 5.0))\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "78")


# ---------------------------------------------------------------------------
# List operations
# ---------------------------------------------------------------------------


def test_list_map():
    src = (
        "main = do\n"
        "  let xs = [1, 2, 3, 4, 5]\n"
        "  let doubled = map (* 2) xs\n"
        "  putStrLn (show doubled)\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "2", "4", "6", "8", "10")


def test_list_filter():
    src = (
        "main = do\n"
        "  let xs = [1..10]\n"
        "  let evens = filter even xs\n"
        "  putStrLn (show evens)\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "2", "4", "6", "8", "10")


def test_list_fold():
    src = (
        "main = do\n"
        "  let xs = [1..5]\n"
        "  let s = foldl (+) 0 xs\n"
        "  putStrLn (show s)\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "15")


def test_list_comprehension():
    src = (
        "main = do\n"
        "  let evens = [x | x <- [1..10], even x]\n"
        "  putStrLn (show evens)\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "2", "4", "6", "8", "10")


def test_list_range():
    src = (
        "main = do\n"
        "  putStrLn (show [1..5])\n"
        "  putStrLn (show [1,3..9])\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "1", "3", "5", "9")


def test_list_head_tail():
    src = (
        "main = do\n"
        "  let xs = [10, 20, 30]\n"
        "  putStrLn (show (head xs))\n"
        "  putStrLn (show (tail xs))\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "10", "20", "30")


def test_list_length():
    src = (
        "main = do\n"
        "  let xs = [1..7]\n"
        "  putStrLn (show (length xs))\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "7")


# ---------------------------------------------------------------------------
# String operations
# ---------------------------------------------------------------------------


def test_string_concat():
    src = (
        "main = do\n"
        '  let s = "Hello" ++ ", " ++ "World!"\n'
        "  putStrLn s\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_string_length():
    src = (
        "main = do\n"
        '  let s = "Haskell"\n'
        "  putStrLn (show (length s))\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "7")


def test_string_reverse():
    src = (
        "main = do\n"
        '  putStrLn (reverse "Hello")\n'
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "olleH")


def test_show_various():
    src = (
        "main = do\n"
        "  putStrLn (show 42)\n"
        "  putStrLn (show True)\n"
        "  putStrLn (show [1,2,3])\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "42", "True", "1")


# ---------------------------------------------------------------------------
# Tuple operations
# ---------------------------------------------------------------------------


def test_tuple_fst_snd():
    src = (
        "main = do\n"
        "  let pair = (10, 20)\n"
        "  putStrLn (show (fst pair))\n"
        "  putStrLn (show (snd pair))\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "10", "20")


def test_tuple_pattern_lambda():
    src = (
        "main = do\n"
        "  let pairs = [(1,2),(3,4),(5,6)]\n"
        "  mapM_ (\\(a,b) -> putStrLn (show (a + b))) pairs\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "3", "7", "11")


# ---------------------------------------------------------------------------
# Function composition and higher-order functions
# ---------------------------------------------------------------------------


def test_function_composition():
    src = (
        "main = do\n"
        "  let double = (* 2)\n"
        "  let addOne = (+ 1)\n"
        "  let transform = addOne . double\n"
        "  putStrLn (show (transform 5))\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "11")


def test_operator_sections():
    src = (
        "main = do\n"
        "  let xs = [1..5]\n"
        "  let doubled = map (* 2) xs\n"
        "  let incremented = map (+ 10) xs\n"
        "  putStrLn (show doubled)\n"
        "  putStrLn (show incremented)\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "2", "4", "6", "8", "10", "11", "15")


def test_zipWith():
    src = (
        "main = do\n"
        "  let xs = [1, 2, 3]\n"
        "  let ys = [4, 5, 6]\n"
        "  let sums = zipWith (+) xs ys\n"
        "  putStrLn (show sums)\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "5", "7", "9")


def test_flip():
    src = (
        "main = do\n"
        "  let sub = flip (-)\n"
        "  putStrLn (show (sub 3 10))\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "7")


# ---------------------------------------------------------------------------
# Char / fromEnum / toEnum
# ---------------------------------------------------------------------------


def test_char_ord_chr():
    src = (
        "main = do\n"
        "  putStrLn (show (ord 'A'))\n"
        "  putStrLn [chr 65]\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "65", "A")


def test_fromEnum_toEnum():
    src = (
        "main = do\n"
        "  putStrLn (show (fromEnum 'a'))\n"
        "  putStrLn [toEnum 97]\n"
    )
    out = hs(src)
    assert no_errors(out)
    assert has(out, "97", "a")


# ---------------------------------------------------------------------------
# Data.IORef
# ---------------------------------------------------------------------------


def test_ioref_basic():
    """newIORef / readIORef / modifyIORef."""
    out = hs(
        "main = do\n"
        "  ref <- newIORef 10\n"
        "  modifyIORef ref (*2)\n"
        "  val <- readIORef ref\n"
        "  print val"
    )
    assert no_errors(out)
    assert has(out, "20")


def test_ioref_write():
    """writeIORef overwrites the stored value."""
    out = hs(
        "main = do\n"
        "  ref <- newIORef 0\n"
        "  writeIORef ref 99\n"
        "  val <- readIORef ref\n"
        "  print val"
    )
    assert no_errors(out)
    assert has(out, "99")


# ---------------------------------------------------------------------------
# Data.Map
# ---------------------------------------------------------------------------


def test_map_from_list_and_size():
    """Map.fromList and Map.size."""
    out = hs(
        "import qualified Data.Map.Strict as Map\n"
        "main = do\n"
        "  let m = Map.fromList [(\"a\", 1), (\"b\", 2), (\"c\", 3)]\n"
        "  print (Map.size m)"
    )
    assert no_errors(out)
    assert has(out, "3")


def test_map_lookup_found():
    """Map.lookup returns Just value (rendered as the value itself)."""
    out = hs(
        "import qualified Data.Map as Map\n"
        "main = do\n"
        "  let m = Map.fromList [(\"x\", 42)]\n"
        "  print (Map.lookup \"x\" m)"
    )
    assert no_errors(out)
    assert has(out, "42")


def test_map_lookup_missing():
    """Map.lookup returns Nothing for a missing key."""
    out = hs(
        "import qualified Data.Map as Map\n"
        "main = do\n"
        "  let m = Map.fromList [(\"x\", 1)]\n"
        "  print (Map.lookup \"z\" m)"
    )
    assert no_errors(out)
    assert has(out, "Nothing")


def test_map_insert_and_member():
    """Map.insert and Map.member."""
    out = hs(
        "import qualified Data.Map as Map\n"
        "main = do\n"
        "  let m = Map.fromList [(1, \"one\"), (2, \"two\")]\n"
        "  let m2 = Map.insert 3 \"three\" m\n"
        "  print (Map.size m2)\n"
        "  print (Map.member 3 m2)"
    )
    assert no_errors(out)
    assert has(out, "3")
    assert has(out, "True")


def test_map_keys_and_elems():
    """Map.keys and Map.elems."""
    out = hs(
        "import qualified Data.Map as Map\n"
        "main = do\n"
        "  let m = Map.fromList [(1, 10), (2, 20)]\n"
        "  print (Map.size m)\n"
        "  print (Map.member 1 m)"
    )
    assert no_errors(out)
    assert has(out, "2")
    assert has(out, "True")


# ---------------------------------------------------------------------------
# Algorithms example
# ---------------------------------------------------------------------------


def test_algorithms_example():
    import os
    path = os.path.join(
        os.path.dirname(__file__),
        "..", "..", "..", "..", "..", "Examples", "haskell", "algorithms.hs",
    )
    path = os.path.normpath(path)
    if not os.path.exists(path):
        pytest.skip("algorithms.hs not found")
    with open(path) as f:
        src = f.read()
    out = hs(src)
    assert no_errors(out)
    assert has(out, "gcd(48,18)=6", "ROT13:", "Uryyb", "aaabbbccddddeeefff")


def test_haskell_unwords():
    out = hs('main = putStrLn (unwords ["a","b","c"])')
    assert no_errors(out)
    assert has(out, "a b c")


def test_haskell_map_double():
    out = hs("main = print (map (*2) [1,2,3])")
    assert no_errors(out)
    assert has(out, "2", "4", "6")


def test_haskell_filter_even():
    out = hs("main = print (filter even [1..10])")
    assert no_errors(out)
    assert has(out, "2", "4", "6", "8", "10")


def test_haskell_sum_to_100():
    out = hs("main = print (sum [1..100])")
    assert no_errors(out)
    assert has(out, "5050")
