"""Comprehensive tests for the Erlang executor."""

from __future__ import annotations

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.ERLANG


def erlang(source: str) -> list[str]:
    return run(source, LANG)


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------


def test_hello_world():
    out = erlang(
        "-module(hello).\n"
        "-export([main/0]).\n"
        "main() -> io:format(\"Hello, World!~n\")."
    )
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_format_integer():
    out = erlang(
        "-module(nums).\n"
        "-export([main/0]).\n"
        "main() -> io:format(\"~w~n\", [42])."
    )
    assert no_errors(out)
    assert has(out, "42")


# ---------------------------------------------------------------------------
# Variables and pattern matching
# ---------------------------------------------------------------------------


def test_variable_binding():
    out = erlang(
        "-module(vars).\n"
        "-export([main/0]).\n"
        "main() ->\n"
        "  X = 10,\n"
        "  Y = 3,\n"
        "  io:format(\"~w~n\", [X + Y])."
    )
    assert no_errors(out)
    assert has(out, "13")


def test_pattern_match_tuple():
    out = erlang(
        "-module(tup).\n"
        "-export([main/0]).\n"
        "main() ->\n"
        "  {A, B} = {hello, 42},\n"
        "  io:format(\"~w ~w~n\", [A, B])."
    )
    assert no_errors(out)
    assert has(out, "hello", "42")


# ---------------------------------------------------------------------------
# Functions
# ---------------------------------------------------------------------------


def test_factorial():
    out = erlang(
        "-module(fact).\n"
        "-export([main/0, factorial/1]).\n"
        "factorial(0) -> 1;\n"
        "factorial(N) -> N * factorial(N - 1).\n"
        "main() -> io:format(\"~w~n\", [factorial(5)])."
    )
    assert no_errors(out)
    assert has(out, "120")


# ---------------------------------------------------------------------------
# Lists
# ---------------------------------------------------------------------------


def test_list_operations():
    out = erlang(
        "-module(lists_demo).\n"
        "-export([main/0]).\n"
        "main() ->\n"
        "  L = [1, 2, 3, 4, 5],\n"
        "  io:format(\"~w~n\", [length(L)]),\n"
        "  io:format(\"~w~n\", [hd(L)]),\n"
        "  io:format(\"~w~n\", [lists:sum(L)])."
    )
    assert no_errors(out)
    assert has(out, "5", "1", "15")


def test_list_map():
    out = erlang(
        "-module(lmap).\n"
        "-export([main/0]).\n"
        "main() ->\n"
        "  R = lists:map(fun(X) -> X * 2 end, [1, 2, 3]),\n"
        "  lists:foreach(fun(X) -> io:format(\"~w~n\", [X]) end, R)."
    )
    assert no_errors(out)
    assert has(out, "2", "4", "6")


# ---------------------------------------------------------------------------
# Additional features
# ---------------------------------------------------------------------------


def test_list_comprehension():
    out = erlang(
        "-module(lc).\n"
        "-export([main/0]).\n"
        "main() ->\n"
        "  L = [X*2 || X <- [1,2,3]],\n"
        "  lists:foreach(fun(X) -> io:format(\"~w~n\", [X]) end, L)."
    )
    assert no_errors(out)
    assert has(out, "2", "4", "6")


def test_case_expression():
    out = erlang(
        "-module(cexp).\n"
        "-export([main/0]).\n"
        "main() ->\n"
        "  X = 2,\n"
        "  case X of\n"
        "    1 -> io:format(\"one~n\");\n"
        "    2 -> io:format(\"two~n\");\n"
        "    _ -> io:format(\"other~n\")\n"
        "  end."
    )
    assert no_errors(out)
    assert has(out, "two")


def test_tuple_pattern_match():
    out = erlang(
        "-module(tup).\n"
        "-export([main/0]).\n"
        "main() ->\n"
        "  T = {ok, 42},\n"
        "  {Status, Val} = T,\n"
        "  io:format(\"~w ~w~n\", [Status, Val])."
    )
    assert no_errors(out)
    assert has(out, "ok")
    assert has(out, "42")


def test_string_to_upper():
    out = erlang(
        "-module(su).\n"
        "-export([main/0]).\n"
        "main() ->\n"
        "  S = string:to_upper(\"hello\"),\n"
        "  io:format(\"~s~n\", [S])."
    )
    assert no_errors(out)
    assert has(out, "HELLO")


def test_list_head():
    out = erlang(
        "-module(hd_test).\n"
        "-export([main/0]).\n"
        "main() -> L = [10, 20, 30], io:format(\"~p~n\", [hd(L)])."
    )
    assert no_errors(out)
    assert has(out, "10")


def test_list_sum():
    out = erlang(
        "-module(sum_test).\n"
        "-export([main/0]).\n"
        "main() -> L = [1, 2, 3, 4], io:format(\"~p~n\", [lists:sum(L)])."
    )
    assert no_errors(out)
    assert has(out, "10")


def test_list_sort():
    out = erlang(
        "-module(sort_test).\n"
        "-export([main/0]).\n"
        "main() -> S = lists:sort([3, 1, 4]), io:format(\"~p~n\", [S])."
    )
    assert no_errors(out)
    assert has(out, "1")


def test_list_length():
    out = erlang(
        "-module(len_test).\n"
        "-export([main/0]).\n"
        "main() -> io:format(\"~p~n\", [length([a, b, c, d])])."
    )
    assert no_errors(out)
    assert has(out, "4")


def test_atom_comparison():
    out = erlang(
        "-module(cmp_test).\n"
        "-export([main/0]).\n"
        "main() ->\n"
        "  X = 10,\n"
        "  Y = 20,\n"
        "  io:format(\"~p~n\", [Y > X])."
    )
    assert no_errors(out)
    assert has(out, "true")


def test_multiple_clauses():
    out = erlang(
        "-module(multi_test).\n"
        "-export([main/0]).\n"
        "describe(1) -> one;\n"
        "describe(2) -> two;\n"
        "describe(_) -> other.\n"
        "main() ->\n"
        "  io:format(\"~p~n\", [describe(2)])."
    )
    assert no_errors(out)
    assert has(out, "two")


def test_lists_max():
    out = erlang(
        "-module(max_test).\n"
        "-export([main/0]).\n"
        "main() -> io:format(\"~p~n\", [lists:max([3,1,4,1,5,9])])."
    )
    assert no_errors(out)
    assert has(out, "9")


def test_lists_nth():
    out = erlang(
        "-module(nth_test).\n"
        "-export([main/0]).\n"
        "main() -> io:format(\"~p~n\", [lists:nth(3, [a,b,c,d,e])])."
    )
    assert no_errors(out)
    assert has(out, "c")


def test_lists_foldl():
    out = erlang(
        "-module(fold_test).\n"
        "-export([main/0]).\n"
        "main() ->\n"
        "  L = [1, 2, 3, 4, 5],\n"
        "  io:format(\"~p~n\", [lists:foldl(fun(X, Acc) -> X + Acc end, 0, L)])."
    )
    assert no_errors(out)
    assert has(out, "15")


def test_list_reverse():
    out = erlang(
        "-module(rev_test).\n"
        "-export([main/0]).\n"
        "main() -> io:format(\"~w~n\", [lists:reverse([1, 2, 3])]).\n"
    )
    assert no_errors(out)
    assert has(out, "3", "2", "1")


def test_abs_value():
    out = erlang(
        "-module(abs_test).\n"
        "-export([main/0]).\n"
        "main() -> io:format(\"~w~n\", [abs(-42)]).\n"
    )
    assert no_errors(out)
    assert has(out, "42")


def test_atom_to_list():
    out = erlang(
        "-module(atom_test).\n"
        "-export([main/0]).\n"
        "main() -> X = atom_to_list(hello), io:format(\"~s~n\", [X]).\n"
    )
    assert no_errors(out)
    assert has(out, "hello")


def test_format_integer():
    out = erlang("main() -> io:format(\"~p~n\", [42]).")
    assert no_errors(out)
    assert has(out, "42")


def test_tuple_output():
    out = erlang("main() -> T = {1, 2, 3}, io:format(\"~p~n\", [T]).")
    assert no_errors(out)
    assert has(out, "1", "2", "3")


def test_list_length():
    out = erlang("main() -> io:format(\"~p~n\", [length([1,2,3,4,5])]).")
    assert no_errors(out)
    assert has(out, "5")


def test_hd_list():
    out = erlang("main() -> io:format(\"~p~n\", [hd([1,2,3])]).")
    assert no_errors(out)
    assert has(out, "1")


def test_abs_negative():
    out = erlang("main() -> io:format(\"~p~n\", [abs(-42)]).")
    assert no_errors(out)
    assert has(out, "42")


# ---------------------------------------------------------------------------
# Example programs
# ---------------------------------------------------------------------------


def test_hello_example():
    import pathlib
    src = (
        pathlib.Path(__file__).parents[4] / "Examples" / "erlang" / "hello.erl"
    ).read_text()
    out = erlang(src)
    assert no_errors(out)
    assert ok(out)
