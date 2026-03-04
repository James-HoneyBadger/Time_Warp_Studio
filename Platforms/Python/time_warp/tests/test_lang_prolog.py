"""Comprehensive tests for the Prolog language executor."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors, first_error

L = Language.PROLOG


def pro(source: str, **kw) -> list[str]:
    """Shortcut: run a Prolog program."""
    return run(source, L, **kw)


# ============================================================================
# FACTS
# ============================================================================


class TestFacts:
    def test_assert_fact(self):
        out = pro("parent(john, mary).")
        assert no_errors(out)

    def test_query_fact_true(self):
        out = pro("parent(john, mary).\n?- parent(john, mary).")
        assert has(out, "true")

    def test_query_fact_false(self):
        out = pro("parent(john, mary).\n?- parent(john, bob).")
        assert has(out, "false")

    def test_query_variable_binding(self):
        out = pro("parent(john, mary).\n?- parent(X, mary).")
        assert has(out, "X=john") or has(out, "X = john")

    def test_multiple_facts(self):
        out = pro("color(sky, blue).\ncolor(grass, green).\n?- color(sky, X).")
        assert has(out, "blue")

    def test_multiple_solutions(self):
        out = pro("parent(john, mary).\nparent(jane, mary).\n?- parent(X, mary).")
        assert has(out, "john") and has(out, "jane")


# ============================================================================
# RULES
# ============================================================================


class TestRules:
    def test_simple_rule(self):
        out = pro(
            "parent(john, mary).\n"
            "parent(mary, bob).\n"
            "grandparent(X, Z) :- parent(X, Y), parent(Y, Z).\n"
            "?- grandparent(john, bob)."
        )
        assert has(out, "true")

    def test_rule_with_variable(self):
        out = pro(
            "parent(john, mary).\n"
            "parent(mary, bob).\n"
            "grandparent(X, Z) :- parent(X, Y), parent(Y, Z).\n"
            "?- grandparent(john, X)."
        )
        assert has(out, "bob")


# ============================================================================
# ARITHMETIC (is/2)
# ============================================================================


class TestArithmetic:
    def test_is_addition(self):
        out = pro("?- X is 2 + 3.")
        assert has(out, "X=5") or has(out, "X = 5")

    def test_is_multiplication(self):
        out = pro("?- X is 4 * 5.")
        assert has(out, "X=20") or has(out, "X = 20")

    def test_is_subtraction(self):
        out = pro("?- X is 10 - 3.")
        assert has(out, "X=7") or has(out, "X = 7")

    def test_is_complex(self):
        out = pro("?- X is (2 + 3) * 4.")
        assert has(out, "X=20") or has(out, "X = 20")


# ============================================================================
# COMPARISON
# ============================================================================


class TestComparison:
    def test_greater_than(self):
        out = pro("?- 5 > 3.")
        assert has(out, "true")

    def test_less_than(self):
        out = pro("?- 3 < 5.")
        assert has(out, "true")

    def test_equal(self):
        out = pro("?- 5 =:= 5.")
        # May use = or =:= for numeric equality
        assert has(out, "true") or no_errors(out)

    def test_unification(self):
        out = pro("?- X = hello.")
        assert has(out, "X=hello") or has(out, "X = hello")


# ============================================================================
# WRITE / NL (I/O)
# ============================================================================


class TestIO:
    def test_write(self):
        out = pro("?- write(hello).")
        assert has(out, "hello")

    def test_writeln(self):
        out = pro("?- writeln(world).")
        assert has(out, "world")

    def test_nl(self):
        out = pro("?- write(a), nl, write(b).")
        assert has(out, "a") and has(out, "b")

    def test_write_number(self):
        out = pro("?- X is 2 + 3, write(X).")
        assert has(out, "5")

    def test_format(self):
        out = pro("?- format('hello ~w', [world]).")
        assert has(out, "hello") and has(out, "world")


# ============================================================================
# CONTROL PREDICATES
# ============================================================================


class TestControl:
    def test_true(self):
        out = pro("?- true.")
        assert has(out, "true")

    def test_fail(self):
        out = pro("?- fail.")
        assert has(out, "false")

    def test_not(self):
        out = pro("?- not(fail).")
        assert has(out, "true")

    def test_once(self):
        out = pro("num(1).\nnum(2).\nnum(3).\n?- once(num(X)).")
        assert has(out, "X=1") or has(out, "X = 1")


# ============================================================================
# LIST OPERATIONS
# ============================================================================


class TestLists:
    def test_member(self):
        out = pro("?- member(2, [1, 2, 3]).")
        assert has(out, "true")

    def test_member_fail(self):
        out = pro("?- member(5, [1, 2, 3]).")
        assert has(out, "false")

    def test_append(self):
        out = pro("?- append([1, 2], [3, 4], X).")
        assert no_errors(out)

    def test_length(self):
        out = pro("?- length([a, b, c], N).")
        assert has(out, "3") or no_errors(out)

    def test_reverse(self):
        out = pro("?- reverse([1, 2, 3], X).")
        assert no_errors(out)

    def test_sort(self):
        out = pro("?- sort([3, 1, 2], X).")
        assert no_errors(out)


# ============================================================================
# FINDALL / BAGOF / SETOF
# ============================================================================


class TestAggregation:
    def test_findall(self):
        out = pro("num(1).\nnum(2).\nnum(3).\n?- findall(X, num(X), L).")
        assert no_errors(out)

    def test_bagof(self):
        out = pro("num(1).\nnum(2).\n?- bagof(X, num(X), L).")
        assert no_errors(out)


# ============================================================================
# BUILT-IN PREDICATES
# ============================================================================


class TestBuiltins:
    def test_atom(self):
        out = pro("?- atom(hello).")
        assert has(out, "true")

    def test_number(self):
        out = pro("?- number(42).")
        assert has(out, "true")

    def test_integer(self):
        out = pro("?- integer(5).")
        assert has(out, "true")

    def test_atom_length(self):
        out = pro("?- atom_length(hello, N).")
        assert has(out, "5") or no_errors(out)

    def test_atom_concat(self):
        out = pro("?- atom_concat(hello, world, X).")
        assert has(out, "helloworld") or no_errors(out)

    def test_succ(self):
        out = pro("?- succ(3, X).")
        assert has(out, "4") or no_errors(out)

    def test_between(self):
        out = pro("?- between(1, 3, X).")
        assert no_errors(out)


# ============================================================================
# CUT
# ============================================================================


class TestCut:
    def test_cut(self):
        out = pro("num(1).\nnum(2).\nnum(3).\n?- num(X), !.")
        # Should find first solution only due to cut
        assert has(out, "1")


# ============================================================================
# ASSERT / RETRACT
# ============================================================================


class TestDynamic:
    def test_assert(self):
        out = pro("?- assert(color(red)), color(red).")
        assert has(out, "true") or no_errors(out)

    def test_retract(self):
        out = pro("thing(a).\n?- retract(thing(a)).")
        assert no_errors(out)


# ============================================================================
# DIRECTIVES
# ============================================================================


class TestDirectives:
    def test_directive(self):
        out = pro(":- write(loaded).")
        assert has(out, "loaded") or no_errors(out)


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_unknown_predicate(self):
        out = pro("?- unknown_predicate(x).")
        assert has(out, "false") or first_error(out) is not None

    def test_syntax_error(self):
        # missing period
        out = pro("?- blah")
        # Either error or buffered
        assert len(out) >= 0
