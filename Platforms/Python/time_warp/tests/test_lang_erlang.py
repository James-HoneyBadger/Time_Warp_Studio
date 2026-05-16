"""Comprehensive tests for the Erlang language executor."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors, first_error

L = Language.ERLANG


def erl(source: str, **kw) -> list[str]:
    """Shortcut: run an Erlang program."""
    return run(source, L, **kw)


# ============================================================================
# MODULE / OUTPUT
# ============================================================================


class TestOutput:
    def test_hello_world(self):
        out = erl(
            """-module(hello).\n-export([start/0]).\nstart() ->\n    io:format("Hello, World!~n")."""
        )
        assert has(out, "Hello, World!")

    def test_io_format_integer(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    io:format("~w~n", [42])."""
        )
        assert has(out, "42")

    def test_io_format_string(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    io:format("~s~n", ["hello"])."""
        )
        assert has(out, "hello")

    def test_no_crash(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    io:format("ok~n")."""
        )
        assert no_errors(out)


# ============================================================================
# ARITHMETIC
# ============================================================================


class TestArithmetic:
    def test_add(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    io:format("~w~n", [2 + 3])."""
        )
        assert has(out, "5")

    def test_subtract(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    io:format("~w~n", [10 - 4])."""
        )
        assert has(out, "6")

    def test_multiply(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    io:format("~w~n", [6 * 7])."""
        )
        assert has(out, "42")

    def test_divide_float(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    io:format("~w~n", [10 / 4])."""
        )
        assert has(out, "2.5")

    def test_div_integer(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    io:format("~w~n", [10 div 3])."""
        )
        assert has(out, "3")

    def test_rem(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    io:format("~w~n", [10 rem 3])."""
        )
        assert has(out, "1")


# ============================================================================
# VARIABLES & PATTERN MATCHING
# ============================================================================


class TestVariables:
    def test_variable_binding(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    X = 42,\n    io:format("~w~n", [X])."""
        )
        assert has(out, "42")

    def test_tuple_match(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    {A, B} = {1, 2},\n    io:format("~w ~w~n", [A, B])."""
        )
        assert has(out, "1")
        assert has(out, "2")

    def test_list_head_tail(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    [H|T] = [1, 2, 3],\n    io:format("~w ~w~n", [H, T])."""
        )
        assert has(out, "1")


# ============================================================================
# FUNCTIONS & RECURSION
# ============================================================================


class TestFunctions:
    def test_function_call(self):
        out = erl(
            """-module(t).\n-export([start/0]).\ndouble(X) -> X * 2.\nstart() ->\n    io:format("~w~n", [double(5)])."""
        )
        assert has(out, "10")

    def test_recursive_factorial(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nfact(0) -> 1;\nfact(N) -> N * fact(N - 1).\nstart() ->\n    io:format("~w~n", [fact(5)])."""
        )
        assert has(out, "120")

    def test_recursive_sum(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nsum([]) -> 0;\nsum([H|T]) -> H + sum(T).\nstart() ->\n    io:format("~w~n", [sum([1,2,3,4,5])])."""
        )
        assert has(out, "15")


# ============================================================================
# LISTS
# ============================================================================


class TestLists:
    def test_length(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    io:format("~w~n", [length([1,2,3])])."""
        )
        assert has(out, "3")

    def test_hd_tl(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    io:format("~w~n", [hd([10,20,30])]),\n    io:format("~w~n", [tl([10,20,30])])."""
        )
        assert has(out, "10")

    def test_list_append(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    io:format("~w~n", [[1,2] ++ [3,4]])."""
        )
        assert has(out, "1")
        assert has(out, "4")

    def test_list_comprehension(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    L = [X * 2 || X <- [1, 2, 3]],\n    io:format("~w~n", [L])."""
        )
        assert has(out, "2")
        assert has(out, "4")
        assert has(out, "6")


# ============================================================================
# CASE EXPRESSION
# ============================================================================


class TestCase:
    def test_case_match(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    X = 2,\n    Result = case X of\n        1 -> one;\n        2 -> two;\n        _ -> other\n    end,\n    io:format("~w~n", [Result])."""
        )
        assert has(out, "two")

    def test_case_wildcard(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    X = 99,\n    Result = case X of\n        1 -> one;\n        _ -> unknown\n    end,\n    io:format("~w~n", [Result])."""
        )
        assert has(out, "unknown")


# ============================================================================
# ATOMS & TUPLES
# ============================================================================


class TestAtomsTuples:
    def test_atom_output(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    io:format("~w~n", [hello])."""
        )
        assert has(out, "hello")

    def test_tuple_output(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    io:format("~w~n", [{ok, 42}])."""
        )
        assert has(out, "ok")
        assert has(out, "42")

    def test_element(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    T = {a, b, c},\n    io:format("~w~n", [element(2, T)])."""
        )
        assert has(out, "b")

    def test_tuple_size(self):
        out = erl(
            """-module(t).\n-export([start/0]).\nstart() ->\n    io:format("~w~n", [tuple_size({1, 2, 3})])."""
        )
        assert has(out, "3")


# ============================================================================
# DEMO PROGRAM
# ============================================================================


class TestDemoProgram:
    """Run the shipped Erlang demo to catch regressions."""

    def test_hello_demo(self):
        import os
        demo = os.path.join(
            os.path.dirname(__file__),
            "..", "..", "..", "..", "..", "Examples", "erlang", "hello.erl",
        )
        demo = os.path.normpath(demo)
        if not os.path.exists(demo):
            return  # Demo not present in this environment; skip gracefully
        with open(demo, encoding="utf-8") as f:
            source = f.read()
        out = erl(source)
        assert no_errors(out), f"Demo produced errors: {out}"
