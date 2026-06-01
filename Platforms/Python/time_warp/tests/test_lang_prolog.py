"""Comprehensive tests for the Prolog executor."""

from __future__ import annotations

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.PROLOG


def prolog(source: str) -> list[str]:
    return run(source, LANG)


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------


def test_write_and_nl():
    # Prolog write: use atoms or numbers to avoid quote/comma issues
    out = prolog("?- write(hello), nl.")
    assert no_errors(out)
    assert has(out, "hello")


def test_write_string():
    out = prolog("?- write('Hello World'), nl.")
    assert no_errors(out)
    assert has(out, "Hello World")


def test_multiple_queries():
    out = prolog(
        "?- write('Alpha'), nl.\n"
        "?- write('Beta'), nl."
    )
    assert no_errors(out)
    assert has(out, "Alpha", "Beta")


# ---------------------------------------------------------------------------
# Facts and rules
# ---------------------------------------------------------------------------


def test_fact_query():
    out = prolog(
        "animal(dog).\n"
        "animal(cat).\n"
        "?- animal(dog), write(yes), nl."
    )
    assert no_errors(out)
    assert has(out, "yes")


def test_rule_query():
    out = prolog(
        "parent(alice, bob).\n"
        "parent(bob, charlie).\n"
        "grandparent(X, Z) :- parent(X, Y), parent(Y, Z).\n"
        "?- grandparent(alice, charlie), write(yes), nl."
    )
    assert no_errors(out)
    assert has(out, "yes")


# ---------------------------------------------------------------------------
# Arithmetic
# ---------------------------------------------------------------------------


def test_is_arithmetic():
    out = prolog("?- X is 3 + 4, write(X), nl.")
    assert no_errors(out)
    assert has(out, "7")


def test_modulo():
    out = prolog("?- X is 10 mod 3, write(X), nl.")
    assert no_errors(out)
    assert has(out, "1")


# ---------------------------------------------------------------------------
# Lists
# ---------------------------------------------------------------------------


def test_member():
    out = prolog(
        "?- member(2, [1,2,3]), write(yes), nl."
    )
    assert no_errors(out)
    assert has(out, "yes")


def test_length():
    out = prolog("?- length([a,b,c], L), write(L), nl.")
    assert no_errors(out)
    assert has(out, "3")


# ---------------------------------------------------------------------------
# Additional built-ins
# ---------------------------------------------------------------------------


def test_append():
    out = prolog("?- append([1,2],[3,4],L), write(L).")
    assert no_errors(out)
    assert has(out, "[1,2,3,4]")


def test_findall():
    out = prolog(
        "age(alice, 25).\n"
        "age(bob, 30).\n"
        "age(carol, 22).\n"
        "?- findall(X, age(X, _), L), write(L)."
    )
    assert no_errors(out)
    assert has(out, "alice")
    assert has(out, "bob")
    assert has(out, "carol")


def test_max_list():
    out = prolog("?- max_list([3,1,4,1,5,9,2,6], Max), write(Max).")
    assert no_errors(out)
    assert has(out, "9")


def test_negation_as_failure():
    out = prolog(
        "likes(alice, bob).\n"
        "?- \\+(likes(alice, carol)), write(yes)."
    )
    assert no_errors(out)
    assert has(out, "yes")


def test_between():
    out = prolog(":- between(1, 4, X), write(X), nl, fail ; true.")
    assert no_errors(out)
    assert has(out, "1", "2", "3", "4")


def test_atom_length():
    out = prolog(":- atom_length(hello, L), write(L), nl.")
    assert no_errors(out)
    assert has(out, "5")


def test_succ():
    out = prolog(":- succ(9, X), write(X), nl.")
    assert no_errors(out)
    assert has(out, "10")


def test_list_append():
    out = prolog(":- append([1,2], [3,4], L), write(L), nl.")
    assert no_errors(out)
    assert has(out, "1,2,3,4")


def test_arithmetic_eval():
    out = prolog(":- X is 5 * 5, write(X), nl.")
    assert no_errors(out)
    assert has(out, "25")


def test_list_last():
    out = prolog(":- last([1,2,3,4], X), write(X), nl.")
    assert no_errors(out)
    assert has(out, "4")


def test_multiply():
    out = prolog(":- X is 3 * 4, write(X), nl.")
    assert no_errors(out)
    assert has(out, "12")


def test_list_length():
    out = prolog(":- length([a,b,c,d], N), write(N), nl.")
    assert no_errors(out)
    assert has(out, "4")


def test_successor():
    out = prolog(":- succ(4, X), write(X), nl.")
    assert no_errors(out)
    assert has(out, "5")


def test_write_integer():
    out = prolog("?- write(42), nl.")
    assert no_errors(out)
    assert has(out, "42")


def test_is_arithmetic():
    out = prolog("?- X is 3 + 4, write(X), nl.")
    assert no_errors(out)
    assert has(out, "7")


def test_plus_predicate():
    out = prolog("?- plus(3, 5, X), write(X), nl.")
    assert no_errors(out)
    assert has(out, "8")


def test_max_arithmetic():
    out = prolog("?- X is max(3, 7), write(X), nl.")
    assert no_errors(out)
    assert has(out, "7")


def test_atom_length():
    out = prolog("?- atom_length(hello, N), write(N), nl.")
    assert no_errors(out)
    assert has(out, "5")


# ---------------------------------------------------------------------------
# Example programs
# ---------------------------------------------------------------------------


def test_hello_example():
    import pathlib
    src = (
        pathlib.Path(__file__).parents[4] / "Examples" / "prolog" / "hello.pl"
    ).read_text()
    out = prolog(src)
    assert no_errors(out)
    assert ok(out)
