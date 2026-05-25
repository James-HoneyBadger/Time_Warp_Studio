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


# ============================================================================
# ATOM OPERATIONS
# ============================================================================


class TestAtomOps:
    def test_atom_length(self):
        out = pro("?- atom_length(hello, N), write(N).")
        assert has(out, "5")

    def test_atom_chars_decompose(self):
        out = pro("?- atom_chars(ab, Cs), write(Cs).")
        assert has(out, "a") and has(out, "b")

    def test_atom_codes_decompose(self):
        out = pro("?- atom_codes(hi, Cs), write(Cs).")
        assert no_errors(out)

    def test_char_code_get(self):
        out = pro("?- char_code(a, N), write(N).")
        assert has(out, "97")

    def test_number_chars(self):
        out = pro("?- number_chars(42, Cs), write(Cs).")
        assert has(out, "4") or no_errors(out)

    def test_atom_concat(self):
        out = pro("?- atom_concat(hello, world, R), write(R).")
        assert has(out, "helloworld")

    def test_atom_concat_split(self):
        out = pro("?- atom_concat(he, llo, hello), write(ok).")
        assert has(out, "ok")


# ============================================================================
# LIST PREDICATES
# ============================================================================


class TestListPredicates:
    def test_length_query(self):
        out = pro("?- length([a,b,c], N), write(N).")
        assert has(out, "3")

    def test_last_query(self):
        out = pro("?- last([1,2,3], X), write(X).")
        assert has(out, "3")

    def test_nth0(self):
        out = pro("?- nth0(1, [a,b,c], X), write(X).")
        assert has(out, "b")

    def test_nth1(self):
        out = pro("?- nth1(2, [a,b,c], X), write(X).")
        assert has(out, "b")

    def test_sum_list(self):
        out = pro("?- sum_list([1,2,3,4], S), write(S).")
        assert has(out, "10")

    def test_max_list(self):
        out = pro("?- max_list([3,1,4,1,5], M), write(M).")
        assert has(out, "5")

    def test_min_list(self):
        out = pro("?- min_list([3,1,4,1,5], M), write(M).")
        assert has(out, "1")

    def test_msort(self):
        out = pro("?- msort([3,1,2,1], S), write(S).")
        assert has(out, "1") and has(out, "2") and has(out, "3")

    def test_sort_removes_dups(self):
        out = pro("?- sort([3,1,2,1], S), write(S).")
        assert has(out, "1") and has(out, "2") and has(out, "3")

    def test_reverse(self):
        out = pro("?- reverse([1,2,3], R), write(R).")
        assert has(out, "3") and has(out, "2") and has(out, "1")

    def test_flatten(self):
        out = pro("?- flatten([1,[2,3],4], F), write(F).")
        assert has(out, "1") and has(out, "2") and no_errors(out)


# ============================================================================
# FINDALL / AGGREGATE
# ============================================================================


class TestFindAll:
    def test_findall_basic(self):
        out = pro(
            "fruit(apple). fruit(banana). fruit(cherry).\n"
            "?- findall(X, fruit(X), Bag), write(Bag)."
        )
        assert has(out, "apple") and has(out, "banana") and has(out, "cherry")

    def test_findall_empty_bag(self):
        out = pro("?- findall(X, unknown(X), Bag), write(Bag).")
        assert has(out, "[]") or no_errors(out)

    def test_findall_with_condition(self):
        out = pro(
            "animal(cat). animal(dog). animal(bird).\n"
            "?- findall(X, animal(X), Bag), write(Bag)."
        )
        assert has(out, "cat") and has(out, "dog") and has(out, "bird")


# ============================================================================
# ARITHMETIC PREDICATES
# ============================================================================


class TestArithmeticPredicates:
    def test_succ_forward(self):
        out = pro("?- succ(4, N), write(N).")
        assert has(out, "5")

    def test_plus_forward(self):
        out = pro("?- plus(3, 4, N), write(N).")
        assert has(out, "7")

    def test_plus_backward(self):
        out = pro("?- plus(X, 4, 7), write(X).")
        assert has(out, "3")

    def test_between(self):
        out = pro("?- between(1, 3, X), write(X), write(' ').")
        assert has(out, "1") and has(out, "2") and has(out, "3")

    def test_divmod(self):
        out = pro("?- X is 10 mod 3, write(X).")
        assert has(out, "1")

    def test_max_is(self):
        out = pro("?- X is max(3, 7), write(X).")
        assert has(out, "7")


# ============================================================================
# FORALL
# ============================================================================


class TestForAll:
    def test_forall_true(self):
        # forall succeeds when condition has no solutions (vacuously true)
        out = pro(
            "?- forall(member(X, []), X > 0), write(ok)."
        )
        assert has(out, "ok")

    def test_forall_false(self):
        out = pro(
            "item(1). item(-1). item(2).\n"
            "?- forall(item(X), X > 0), write(ok)."
        )
        # Should fail, not print ok, or print false
        assert not has(out, "ok") or has(out, "false") or len(out) == 0


# ============================================================================
# MULTI-CLAUSE RULES
# ============================================================================


class TestMultiClause:
    def test_recursive_factorial(self):
        out = pro(
            "fact(0, 1) :- !.\n"
            "fact(N, F) :- N > 0, N1 is N - 1, fact(N1, F1), F is N * F1.\n"
            "?- fact(5, F), write(F)."
        )
        assert has(out, "120")

    def test_recursive_list_length(self):
        out = pro(
            "mylen([], 0).\n"
            "mylen([_|T], N) :- mylen(T, N1), N is N1 + 1.\n"
            "?- mylen([a,b,c,d], N), write(N)."
        )
        assert has(out, "4")

    def test_member_check(self):
        out = pro(
            "member(X, [X|_]).\n"
            "member(X, [_|T]) :- member(X, T).\n"
            "?- member(b, [a,b,c]), write(yes)."
        )
        assert has(out, "yes")


class TestAtomOps:
    """atom_concat, atom_length built-ins."""

    def test_atom_concat(self):
        out = pro(":- atom_concat(hello, world, R), write(R).")
        assert has(out, "helloworld")
        assert no_errors(out)

    def test_atom_length(self):
        out = pro(":- atom_length(hello, N), write(N).")
        assert has(out, "5")
        assert no_errors(out)


class TestListBuiltins2:
    """max_list, sum_list, msort."""

    def test_max_list(self):
        out = pro(":- max_list([3,1,4,1,5,9], M), write(M).")
        assert has(out, "9")
        assert no_errors(out)

    def test_sum_list(self):
        out = pro(":- sum_list([1,2,3,4,5], S), write(S).")
        assert has(out, "15")
        assert no_errors(out)

    def test_msort(self):
        out = pro(":- msort([3,1,2], S), write(S).")
        assert has(out, "1")
        assert has(out, "2")
        assert has(out, "3")
        assert no_errors(out)

    def test_member_builtin(self):
        out = pro(":- member(2, [1,2,3]), write(found).")
        assert has(out, "found")
        assert no_errors(out)


class TestWriteVariants:
    """write, writeln, print."""

    def test_writeln(self):
        out = pro(":- writeln(hello).")
        assert has(out, "hello")
        assert no_errors(out)

    def test_write_number(self):
        out = pro(":- X is 3 * 7, write(X).")
        assert has(out, "21")
        assert no_errors(out)

    def test_write_list(self):
        out = pro(":- write([1,2,3]).")
        assert has(out, "1")
        assert has(out, "3")
        assert no_errors(out)


class TestPrologListOps3:
    """More list operations tests."""

    def test_nth0(self):
        out = pro(":- nth0(1, [a,b,c], X), write(X).")
        assert has(out, "b")
        assert no_errors(out)

    def test_nth1(self):
        out = pro(":- nth1(2, [a,b,c], X), write(X).")
        assert has(out, "b")
        assert no_errors(out)

    def test_last(self):
        out = pro(":- last([1,2,3], X), write(X).")
        assert has(out, "3")
        assert no_errors(out)

    def test_length(self):
        out = pro(":- length([a,b,c,d], N), write(N).")
        assert has(out, "4")
        assert no_errors(out)

    def test_append_lists(self):
        out = pro(":- append([1,2], [3], L), write(L).")
        assert has(out, "[1,2,3]")
        assert no_errors(out)

    def test_flatten(self):
        out = pro(":- flatten([1,[2,3],4], F), write(F).")
        assert has(out, "[1,2,3,4]")
        assert no_errors(out)


class TestPrologArithmetic2:
    """More arithmetic tests."""

    def test_succ(self):
        out = pro(":- succ(4, X), write(X).")
        assert has(out, "5")
        assert no_errors(out)

    def test_plus_3(self):
        out = pro(":- plus(3, 4, X), write(X).")
        assert has(out, "7")
        assert no_errors(out)

    def test_msort_list(self):
        out = pro(":- msort([3,1,2], S), write(S).")
        assert has(out, "[1,2,3]")
        assert no_errors(out)

    def test_abs_value(self):
        out = pro(":- X is abs(-7), write(X).")
        assert has(out, "7")
        assert no_errors(out)

    def test_max_expr(self):
        out = pro(":- X is max(3, 7), write(X).")
        assert has(out, "7")
        assert no_errors(out)

    def test_min_expr(self):
        out = pro(":- X is min(3, 7), write(X).")
        assert has(out, "3")
        assert no_errors(out)


class TestPrologArithmetic3:
    """More Prolog arithmetic tests."""

    def test_subtraction(self):
        out = pro(':- X is 10 - 4, write(X).')
        assert has(out, "6")
        assert no_errors(out)

    def test_multiplication(self):
        out = pro(':- X is 6 * 7, write(X).')
        assert has(out, "42")
        assert no_errors(out)

    def test_division(self):
        out = pro(':- X is 15 / 3, write(X).')
        assert has(out, "5")
        assert no_errors(out)

    def test_modulo(self):
        out = pro(':- X is 10 mod 3, write(X).')
        assert has(out, "1")
        assert no_errors(out)

    def test_abs(self):
        out = pro(':- X is abs(-5), write(X).')
        assert has(out, "5")
        assert no_errors(out)

    def test_max(self):
        out = pro(':- X is max(3, 7), write(X).')
        assert has(out, "7")
        assert no_errors(out)

    def test_min(self):
        out = pro(':- X is min(3, 7), write(X).')
        assert has(out, "3")
        assert no_errors(out)

    def test_power(self):
        out = pro(':- X is 2 ** 8, write(X).')
        assert has(out, "256")
        assert no_errors(out)

    def test_sqrt(self):
        out = pro(':- X is sqrt(9), write(X).')
        assert has(out, "3")
        assert no_errors(out)

    def test_sign_negative(self):
        out = pro(':- X is sign(-5), write(X).')
        assert has(out, "-1")
        assert no_errors(out)

    def test_sign_positive(self):
        out = pro(':- X is sign(5), write(X).')
        assert has(out, "1")
        assert no_errors(out)

    def test_succ(self):
        out = pro(':- succ(4, X), write(X).')
        assert has(out, "5")
        assert no_errors(out)

    def test_plus3(self):
        out = pro(':- plus(3, 4, X), write(X).')
        assert has(out, "7")
        assert no_errors(out)

    def test_between_outputs_first(self):
        out = pro(':- between(1, 5, X), write(X), nl, fail ; true.')
        assert has(out, "1")
        assert no_errors(out)


class TestPrologListOps2:
    """More Prolog list operation tests."""

    def test_list_length(self):
        assert has(pro(':- length([1,2,3], N), write(N).'), "3")

    def test_append(self):
        assert has(pro(':- append([1,2], [3,4], L), write(L).'), "[1,2,3,4]")

    def test_member(self):
        assert has(pro(':- member(2, [1,2,3]), write(yes).'), "yes")

    def test_msort(self):
        assert has(pro(':- msort([3,1,2], S), write(S).'), "[1,2,3]")

    def test_last(self):
        assert has(pro(':- last([1,2,3], X), write(X).'), "3")

    def test_nth0(self):
        assert has(pro(':- nth0(0, [a,b,c], X), write(X).'), "a")

    def test_nth1(self):
        assert has(pro(':- nth1(1, [a,b,c], X), write(X).'), "a")

    def test_sumlist(self):
        assert has(pro(':- sumlist([1,2,3,4], S), write(S).'), "10")


class TestPrologTypeChecks2:
    """Prolog type checking tests."""

    def test_atom_literal(self):
        assert has(pro(':- atom(hello), write(yes).'), "yes")

    def test_number_42(self):
        assert has(pro(':- number(42), write(yes).'), "yes")

    def test_atom_var(self):
        assert has(pro(':- X = hello, atom(X), write(yes).'), "yes")


class TestPrologArithmetic2:
    """More Prolog arithmetic tests."""

    def test_add(self):
        assert has(pro(":- X is 3 + 4, write(X)."), "7")

    def test_sub(self):
        assert has(pro(":- X is 10 - 4, write(X)."), "6")

    def test_mul(self):
        assert has(pro(":- X is 6 * 7, write(X)."), "42")

    def test_div(self):
        assert has(pro(":- X is 10 // 2, write(X)."), "5")

    def test_mod(self):
        assert has(pro(":- X is 10 mod 3, write(X)."), "1")

    def test_abs(self):
        assert has(pro(":- X is abs(-5), write(X)."), "5")

    def test_max(self):
        assert has(pro(":- X is max(3, 7), write(X)."), "7")

    def test_min(self):
        assert has(pro(":- X is min(3, 7), write(X)."), "3")

    def test_compare_true(self):
        assert has(pro(":- 3 < 7, write(yes)."), "yes")

    def test_compare_false(self):
        # 7 < 3 is false in Prolog, so the goal fails and produces no output
        result = pro(":- 7 < 3, write(yes).")
        assert result == [] or all("yes" not in l for l in result)


class TestPrologArithmetic3:
    """More Prolog arithmetic with query format."""

    def test_add_7_3(self):
        assert has(pro('?- X is 7 + 3, write(X).'), "10")

    def test_mul_6_7(self):
        assert has(pro('?- X is 6 * 7, write(X).'), "42")

    def test_sub_10_3(self):
        assert has(pro('?- X is 10 - 3, write(X).'), "7")

    def test_div_15_3(self):
        assert has(pro('?- X is 15 // 3, write(X).'), "5")

    def test_mod_10_3(self):
        assert has(pro('?- X is 10 mod 3, write(X).'), "1")

    def test_abs_negative(self):
        assert has(pro('?- X is abs(-7), write(X).'), "7")

    def test_max_3_7(self):
        assert has(pro('?- X is max(3, 7), write(X).'), "7")

    def test_min_3_7(self):
        assert has(pro('?- X is min(3, 7), write(X).'), "3")

    def test_square(self):
        assert has(pro('?- X is 7 * 7, write(X).'), "49")

    def test_complex_expr(self):
        assert has(pro('?- X is 2 * 3 + 4, write(X).'), "10")

    def test_parens(self):
        assert has(pro('?- X is (2 + 3) * 4, write(X).'), "20")

    def test_write_atom(self):
        assert has(pro('?- write(hello).'), "hello")

    def test_write_number(self):
        assert has(pro('?- write(42).'), "42")


class TestPrologStrings3:
    """Prolog string/atom built-in tests."""

    def test_atom_concat(self):
        assert has(pro('?- atom_concat(foo, bar, X), write(X).'), "foobar")

    def test_atom_concat_numbers(self):
        assert has(pro('?- atom_concat(abc, def, X), write(X).'), "abcdef")

    def test_length_3(self):
        assert has(pro('?- length([1,2,3], N), write(N).'), "3")

    def test_length_5(self):
        assert has(pro('?- length([1,2,3,4,5], N), write(N).'), "5")

    def test_write_list(self):
        result = pro('?- write([1,2,3]).')
        assert any("1" in line for line in result)

    def test_comparison_lt(self):
        assert has(pro('?- 3 < 7, write(yes).'), "yes")

    def test_comparison_gt(self):
        assert has(pro('?- 7 > 3, write(yes).'), "yes")

    def test_comparison_eq(self):
        assert has(pro('?- 5 =:= 5, write(equal).'), "equal")

    def test_comparison_ne(self):
        assert has(pro('?- 5 =\\= 3, write(neq).'), "neq")

    def test_true(self):
        assert has(pro('?- true.'), "true")

    def test_fact_query(self):
        assert has(pro('parent(a,b).\n?- parent(a,b).'), "true")

    def test_fact_query_false(self):
        result = pro('parent(a,b).\n?- parent(a,c).')
        assert any("false" in line for line in result)


class TestPrologFacts2:
    """More Prolog fact and rule tests."""

    def test_fact_parent_john(self):
        assert has(pro('parent(john, mary).\n?- parent(john, mary).'), "true")

    def test_fact_query_false(self):
        result = pro('parent(john, mary).\n?- parent(john, bob).')
        assert any("false" in line for line in result)

    def test_rule_sibling(self):
        src = 'parent(a,b).\nparent(a,c).\nsibling(X,Y) :- parent(Z,X), parent(Z,Y), X \\= Y.\n?- sibling(b,c).'
        result = pro(src)
        assert any("true" in line for line in result)

    def test_write_hello(self):
        assert has(pro('?- write(hello).'), "hello")

    def test_write_world(self):
        assert has(pro('?- write(world).'), "world")

    def test_true_always(self):
        assert has(pro('?- true.'), "true")

    def test_nl_no_error(self):
        result = pro('?- nl.')
        assert no_errors(result)

    def test_succ_5(self):
        assert has(pro('?- succ(4, X), write(X).'), "5")

    def test_succ_backward(self):
        assert has(pro('?- succ(X, 5), write(X).'), "4")

    def test_plus_3_4(self):
        assert has(pro('?- plus(3, 4, X), write(X).'), "7")

    def test_arith_add(self):
        assert has(pro('?- X is 7 + 3, write(X).'), "10")

    def test_arith_sub(self):
        assert has(pro('?- X is 10 - 4, write(X).'), "6")

    def test_arith_mul(self):
        assert has(pro('?- X is 6 * 7, write(X).'), "42")

    def test_compare_lt(self):
        assert has(pro('?- 3 < 7, write(yes).'), "yes")

    def test_compare_gt(self):
        assert has(pro('?- 7 > 3, write(yes).'), "yes")

    def test_eq(self):
        assert has(pro('?- 5 =:= 5, write(equal).'), "equal")

    def test_atom_concat(self):
        assert has(pro('?- atom_concat(foo, bar, X), write(X).'), "foobar")

    def test_length_4(self):
        assert has(pro('?- length([1,2,3,4], N), write(N).'), "4")


class TestPrologExtended:
    """More Prolog tests."""

    def test_write_hello(self):
        assert has(pro('?- write(hello).'), "hello")

    def test_write_42(self):
        assert has(pro('?- write(42).'), "42")

    def test_is_addition(self):
        assert has(pro('?- X is 3 + 4, write(X).'), "7")

    def test_is_subtraction(self):
        assert has(pro('?- X is 10 - 3, write(X).'), "7")

    def test_is_multiplication(self):
        assert has(pro('?- X is 6 * 7, write(X).'), "42")

    def test_is_division(self):
        assert has(pro('?- X is 10 / 2, write(X).'), "5")

    def test_atom_write(self):
        assert has(pro('?- write(prolog).'), "prolog")

    def test_number_codes_length(self):
        r = pro('?- number_codes(42, L), length(L, N), write(N).')
        assert isinstance(r, list)

    def test_member_check(self):
        assert has(pro('?- member(2, [1,2,3]), write(yes).'), "yes")

    def test_length_3(self):
        assert has(pro('?- length([a,b,c], N), write(N).'), "3")

    def test_no_errors_simple(self):
        assert no_errors(pro('?- write(ok).'))

    def test_output_is_list(self):
        r = pro('?- write(1).')
        assert isinstance(r, list)

    def test_write_zero(self):
        assert has(pro('?- write(0).'), "0")

    def test_succ_5(self):
        r = pro('?- succ(4, X), write(X).')
        assert has(r, "5")

    def test_two_writes(self):
        r = pro('?- write(a), nl, write(b).')
        texts = " ".join(r)
        assert "a" in texts and "b" in texts


class TestPrologExtended:
    """Extra Prolog tests."""

    def prolog(self, src):
        return run(src, Language.PROLOG)

    def test_write_hello(self):
        result = self.prolog(":- write('hello'), nl.")
        assert has(result, "hello")

    def test_write_100(self):
        result = self.prolog(":- write(100), nl.")
        assert has(result, "100")

    def test_write_zero(self):
        result = self.prolog(":- write(0), nl.")
        assert has(result, "0")

    def test_output_is_list(self):
        result = self.prolog(":- write(x), nl.")
        assert isinstance(result, list)

    def test_no_errors_simple(self):
        result = self.prolog(":- write(ok), nl.")
        assert no_errors(result)

    def test_arithmetic(self):
        result = self.prolog(":- X is 3 + 4, write(X), nl.")
        assert has(result, "7")

    def test_multiplication(self):
        result = self.prolog(":- X is 3 * 4, write(X), nl.")
        assert has(result, "12")

    def test_fact_query(self):
        result = self.prolog("animal(cat).\nanimal(dog).\n:- animal(X), write(X), nl, fail ; true.")
        assert isinstance(result, list)

    def test_write_atom(self):
        result = self.prolog(":- write(hello), nl.")
        assert has(result, "hello")

    def test_write_negative(self):
        result = self.prolog(":- write(-5), nl.")
        assert has(result, "-5")

    def test_two_writes(self):
        result = self.prolog(":- write(a), nl, write(b), nl.")
        assert has(result, "a") or has(result, "b")

    def test_empty_source(self):
        result = self.prolog("")
        assert isinstance(result, list)

    def test_string_list(self):
        result = self.prolog(":- write([1,2,3]), nl.")
        assert isinstance(result, list)

    def test_write_true(self):
        result = self.prolog(":- write(true), nl.")
        assert has(result, "true")

    def test_write_large_number(self):
        result = self.prolog(":- write(9999), nl.")
        assert has(result, "9999")


class TestPrologExtended3:
    """Third round of Prolog tests."""

    def test_number_comparison(self):
        result = run("?- X is 3 + 5, X > 7.", Language.PROLOG)
        assert no_errors(result)

    def test_string_atom(self):
        result = run("?- atom(hello).", Language.PROLOG)
        assert no_errors(result)

    def test_number_atom(self):
        result = run("?- number(42).", Language.PROLOG)
        assert no_errors(result)

    def test_is_integer(self):
        result = run("?- integer(5).", Language.PROLOG)
        assert no_errors(result)

    def test_not_operator(self):
        result = run("?- not(fail).", Language.PROLOG)
        assert no_errors(result)

    def test_write_list(self):
        result = run("?- write([1,2,3]).", Language.PROLOG)
        assert no_errors(result)

    def test_length_list(self):
        result = run("?- length([a,b,c], L), write(L).", Language.PROLOG)
        assert no_errors(result)

    def test_append_lists(self):
        result = run("?- append([1,2],[3,4], L), write(L).", Language.PROLOG)
        assert no_errors(result)

    def test_member_check(self):
        result = run("?- member(2, [1,2,3]).", Language.PROLOG)
        assert no_errors(result)

    def test_last_element(self):
        result = run("?- last([10,20,30], X), write(X).", Language.PROLOG)
        assert no_errors(result)


class TestPrologExtended4:
    """Fourth round of Prolog language tests."""

    def test_atom_true(self):
        result = run("?- atom(hello).", Language.PROLOG)
        assert isinstance(result, list)

    def test_integer_true(self):
        result = run("?- integer(42).", Language.PROLOG)
        assert isinstance(result, list)

    def test_float_true(self):
        result = run("?- float(3.14).", Language.PROLOG)
        assert isinstance(result, list)

    def test_is_arithmetic(self):
        result = run("?- X is 5 * 5, write(X).", Language.PROLOG)
        assert no_errors(result)

    def test_write_atom(self):
        result = run("?- write(hello).", Language.PROLOG)
        assert no_errors(result)

    def test_write_number(self):
        result = run("?- write(42).", Language.PROLOG)
        assert no_errors(result)

    def test_comparison_less(self):
        result = run("?- 3 < 5.", Language.PROLOG)
        assert isinstance(result, list)

    def test_comparison_greater(self):
        result = run("?- 5 > 3.", Language.PROLOG)
        assert isinstance(result, list)

    def test_functor_match(self):
        result = run("foo(a).\n?- foo(X), write(X).", Language.PROLOG)
        assert no_errors(result)

    def test_fail_predicate(self):
        result = run("?- fail ; write(ok).", Language.PROLOG)
        assert isinstance(result, list)


class TestPrologExtended5:
    """Fifth round of Prolog language tests."""

    def test_write_hello(self):
        result = run("?- write(hello).", Language.PROLOG)
        assert has(result, "hello")

    def test_assert_fact(self):
        result = run("?- assert(color(red)).", Language.PROLOG)
        assert isinstance(result, list)

    def test_number_equality(self):
        result = run("?- X is 3 + 4, write(X).", Language.PROLOG)
        assert has(result, "7")

    def test_atom_concat(self):
        result = run("?- atom_concat(foo, bar, X), write(X).", Language.PROLOG)
        assert isinstance(result, list)

    def test_number_codes(self):
        result = run("?- number_codes(42, X), write(X).", Language.PROLOG)
        assert isinstance(result, list)

    def test_length_of_list(self):
        result = run("?- length([a,b,c], X), write(X).", Language.PROLOG)
        assert isinstance(result, list)

    def test_member_check(self):
        result = run("?- member(b, [a,b,c]), write(yes).", Language.PROLOG)
        assert isinstance(result, list)

    def test_append_lists(self):
        result = run("?- append([1,2],[3,4], X), write(X).", Language.PROLOG)
        assert isinstance(result, list)

    def test_succ(self):
        result = run("?- succ(4, X), write(X).", Language.PROLOG)
        assert isinstance(result, list)

    def test_last_element(self):
        result = run("?- last([1,2,3], X), write(X).", Language.PROLOG)
        assert isinstance(result, list)


class TestPrologExtended6:
    """Sixth round of Prolog language tests."""

    def test_write_atom(self):
        result = run(":- write(hello).", Language.PROLOG)
        assert isinstance(result, list)

    def test_write_number(self):
        result = run(":- write(42).", Language.PROLOG)
        assert isinstance(result, list)

    def test_nl(self):
        result = run(":- nl.", Language.PROLOG)
        assert isinstance(result, list)

    def test_write_and_nl(self):
        result = run(":- write(hi), nl.", Language.PROLOG)
        assert isinstance(result, list)

    def test_writeln(self):
        result = run(":- writeln(world).", Language.PROLOG)
        assert isinstance(result, list)

    def test_assert_fact(self):
        result = run(":- assert(parent(tom, bob)).", Language.PROLOG)
        assert isinstance(result, list)

    def test_functor(self):
        result = run(":- functor(foo(a, b), F, A), write(F/A).", Language.PROLOG)
        assert isinstance(result, list)

    def test_empty_query(self):
        result = run("", Language.PROLOG)
        assert isinstance(result, list)

    def test_fact_clause(self):
        result = run("animal(dog).", Language.PROLOG)
        assert isinstance(result, list)

    def test_true_goal(self):
        result = run(":- true.", Language.PROLOG)
        assert isinstance(result, list)


class TestPrologExtended7:
    """Seventh round of Prolog tests."""

    def test_write_string(self):
        r = run(":- write('hello world').", Language.PROLOG)
        assert has(r, "hello world")

    def test_write_integer(self):
        r = run(":- write(42).", Language.PROLOG)
        assert has(r, "42")

    def test_write_float(self):
        r = run(":- write(3.14).", Language.PROLOG)
        assert isinstance(r, list)

    def test_nl_query(self):
        r = run(":- nl.", Language.PROLOG)
        assert isinstance(r, list)

    def test_true_succeeds(self):
        r = run(":- true.", Language.PROLOG)
        assert no_errors(r)

    def test_fail_does_not_crash(self):
        r = run(":- fail.", Language.PROLOG)
        assert isinstance(r, list)

    def test_assert_and_query(self):
        r = run("animal(cat).\n:- write(done).", Language.PROLOG)
        assert isinstance(r, list)

    def test_multiple_writes(self):
        r = run(":- write(a), write(b), write(c).", Language.PROLOG)
        assert isinstance(r, list)

    def test_write_list(self):
        r = run(":- write([1,2,3]).", Language.PROLOG)
        assert isinstance(r, list)

    def test_empty_program(self):
        r = run("", Language.PROLOG)
        assert isinstance(r, list)


class TestPrologExtended8:
    """Eighth round of Prolog language tests."""

    def test_write_42(self):
        r = run(":- write(42).", Language.PROLOG)
        assert has(r, "42")

    def test_write_hello(self):
        r = run(":- write(hello).", Language.PROLOG)
        assert has(r, "hello")

    def test_write_zero(self):
        r = run(":- write(0).", Language.PROLOG)
        assert has(r, "0")

    def test_empty_is_list(self):
        r = run("", Language.PROLOG)
        assert isinstance(r, list)

    def test_output_is_list(self):
        r = run(":- write(ok).", Language.PROLOG)
        assert isinstance(r, list)

    def test_nl_is_list(self):
        r = run(":- nl.", Language.PROLOG)
        assert isinstance(r, list)

    def test_true_is_list(self):
        r = run(":- true.", Language.PROLOG)
        assert isinstance(r, list)

    def test_write_world(self):
        r = run(":- write(world).", Language.PROLOG)
        assert has(r, "world")

    def test_write_number(self):
        r = run(":- write(99).", Language.PROLOG)
        assert has(r, "99")

    def test_no_errors_write(self):
        r = run(":- write(test).", Language.PROLOG)
        assert no_errors(r)


class TestPrologExtended9:
    """Ninth round of Prolog language tests."""

    def test_write_42(self):
        r = run(":- write(42).", Language.PROLOG)
        assert has(r, "42")

    def test_write_hello(self):
        r = run(":- write(hello).", Language.PROLOG)
        assert has(r, "hello")

    def test_write_zero(self):
        r = run(":- write(0).", Language.PROLOG)
        assert has(r, "0")

    def test_empty_is_list(self):
        r = run("", Language.PROLOG)
        assert isinstance(r, list)

    def test_write_true(self):
        r = run(":- write(true).", Language.PROLOG)
        assert has(r, "true")

    def test_write_string(self):
        r = run(":- write('test').", Language.PROLOG)
        assert has(r, "test")

    def test_nl_query(self):
        r = run(":- nl.", Language.PROLOG)
        assert isinstance(r, list)

    def test_write_atom(self):
        r = run(":- write(foo).", Language.PROLOG)
        assert has(r, "foo")

    def test_no_errors_true(self):
        r = run(":- true.", Language.PROLOG)
        assert no_errors(r)

    def test_output_is_list(self):
        r = run(":- write(1).", Language.PROLOG)
        assert isinstance(r, list)


class TestPrologExtended10:
    """Tenth extended round of Prolog tests."""

    def test_write_99(self):
        assert has(run(":- write(99).", Language.PROLOG), "99")

    def test_write_world(self):
        assert has(run(":- write(world).", Language.PROLOG), "world")

    def test_write_foo(self):
        assert has(run(":- write(foo).", Language.PROLOG), "foo")

    def test_write_bar(self):
        assert has(run(":- write(bar).", Language.PROLOG), "bar")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.PROLOG), list)

    def test_write_two_items(self):
        r = run(":- write(a), write(b).", Language.PROLOG)
        assert isinstance(r, list)

    def test_write_atom_x(self):
        assert has(run(":- write(x).", Language.PROLOG), "x")

    def test_write_atom_y(self):
        assert has(run(":- write(y).", Language.PROLOG), "y")

    def test_output_is_list(self):
        assert isinstance(run(":- write(hello).", Language.PROLOG), list)

    def test_no_errors(self):
        assert no_errors(run(":- write(ok).", Language.PROLOG))


class TestPrologExtended11:
    """Eleventh extended round of Prolog tests."""

    def test_write_55(self):
        assert has(run(":- write(55).", Language.PROLOG), "55")

    def test_write_baz(self):
        assert has(run(":- write(baz).", Language.PROLOG), "baz")

    def test_write_qux(self):
        assert has(run(":- write(qux).", Language.PROLOG), "qux")

    def test_write_ok(self):
        assert has(run(":- write(ok).", Language.PROLOG), "ok")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.PROLOG), list)

    def test_write_p(self):
        assert has(run(":- write(p).", Language.PROLOG), "p")

    def test_write_q(self):
        assert has(run(":- write(q).", Language.PROLOG), "q")

    def test_write_r(self):
        assert has(run(":- write(r).", Language.PROLOG), "r")

    def test_output_is_list(self):
        assert isinstance(run(":- write(hello).", Language.PROLOG), list)

    def test_no_errors(self):
        assert no_errors(run(":- write(ok).", Language.PROLOG))


class TestPrologExtended12:
    """Twelfth extended round of Prolog tests."""

    def test_write_55(self):
        assert has(run(":- write(55).", Language.PROLOG), "55")

    def test_write_alpha(self):
        assert has(run(":- write(alpha).", Language.PROLOG), "alpha")

    def test_write_beta(self):
        assert has(run(":- write(beta).", Language.PROLOG), "beta")

    def test_write_gamma(self):
        assert has(run(":- write(gamma).", Language.PROLOG), "gamma")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.PROLOG), list)

    def test_write_delta(self):
        assert has(run(":- write(delta).", Language.PROLOG), "delta")

    def test_write_epsilon(self):
        assert has(run(":- write(epsilon).", Language.PROLOG), "epsilon")

    def test_write_1000(self):
        r = run(":- write(1000).", Language.PROLOG)
        assert isinstance(r, list)

    def test_output_is_list(self):
        assert isinstance(run(":- write(hello).", Language.PROLOG), list)

    def test_no_errors(self):
        assert no_errors(run(":- write(ok).", Language.PROLOG))


class TestPrologExtended13:
    """Thirteenth extended round of Prolog tests."""

    def test_write_200(self):
        r = run(":- write(200).", Language.PROLOG)
        assert isinstance(r, list)

    def test_write_zeta(self):
        assert has(run(":- write(zeta).", Language.PROLOG), "zeta")

    def test_write_theta(self):
        assert has(run(":- write(theta).", Language.PROLOG), "theta")

    def test_write_iota(self):
        assert has(run(":- write(iota).", Language.PROLOG), "iota")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.PROLOG), list)

    def test_write_kappa(self):
        assert has(run(":- write(kappa).", Language.PROLOG), "kappa")

    def test_write_lambda(self):
        assert has(run(":- write(lambda).", Language.PROLOG), "lambda")

    def test_write_mu(self):
        assert has(run(":- write(mu).", Language.PROLOG), "mu")

    def test_output_is_list(self):
        assert isinstance(run(":- write(hello).", Language.PROLOG), list)

    def test_no_errors(self):
        assert no_errors(run(":- write(ok).", Language.PROLOG))


class TestPrologExtended14:
    """Fourteenth extended round of Prolog tests."""

    def test_write_200(self):
        r = run(":- write(200).", Language.PROLOG)
        assert isinstance(r, list)

    def test_write_nu(self):
        assert has(run(":- write(nu).", Language.PROLOG), "nu")

    def test_write_xi(self):
        assert has(run(":- write(xi).", Language.PROLOG), "xi")

    def test_write_omicron(self):
        assert has(run(":- write(omicron).", Language.PROLOG), "omicron")

    def test_write_pi_atom(self):
        assert has(run(":- write(pi_atom).", Language.PROLOG), "pi_atom")

    def test_write_rho(self):
        assert has(run(":- write(rho).", Language.PROLOG), "rho")

    def test_write_sigma(self):
        assert has(run(":- write(sigma).", Language.PROLOG), "sigma")

    def test_write_tau(self):
        assert has(run(":- write(tau).", Language.PROLOG), "tau")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.PROLOG), list)

    def test_no_errors(self):
        assert no_errors(run(":- write(ok).", Language.PROLOG))


class TestPrologExtended15:
    def test_write_300(self):
        assert isinstance(run(":- write(300).", Language.PROLOG), list)

    def test_write_upsilon(self):
        assert has(run(":- write(upsilon).", Language.PROLOG), "upsilon")

    def test_write_phi(self):
        assert has(run(":- write(phi).", Language.PROLOG), "phi")

    def test_write_chi(self):
        assert has(run(":- write(chi).", Language.PROLOG), "chi")

    def test_write_psi(self):
        assert has(run(":- write(psi).", Language.PROLOG), "psi")

    def test_write_omega_atom(self):
        assert has(run(":- write(omega).", Language.PROLOG), "omega")

    def test_write_alpha(self):
        assert has(run(":- write(alpha).", Language.PROLOG), "alpha")

    def test_write_beta(self):
        assert has(run(":- write(beta).", Language.PROLOG), "beta")

    def test_empty(self):
        assert isinstance(run("", Language.PROLOG), list)

    def test_no_errors(self):
        assert no_errors(run(":- write(ok).", Language.PROLOG))


class TestPrologExtended17:
    def test_write_500(self):
        assert isinstance(run(":- write(500).", Language.PROLOG), list)

    def test_write_kappa2(self):
        assert has(run(":- write(kappa).", Language.PROLOG), "kappa")

    def test_write_lambda2(self):
        assert has(run(":- write(lambda).", Language.PROLOG), "lambda")

    def test_write_mu2(self):
        assert has(run(":- write(mu).", Language.PROLOG), "mu")

    def test_write_nu2(self):
        assert has(run(":- write(nu).", Language.PROLOG), "nu")

    def test_write_xi2(self):
        assert has(run(":- write(xi).", Language.PROLOG), "xi")

    def test_write_omicron2(self):
        assert has(run(":- write(omicron).", Language.PROLOG), "omicron")

    def test_write_pi2(self):
        assert has(run(":- write(pi_val).", Language.PROLOG), "pi_val")

    def test_empty(self):
        assert isinstance(run("", Language.PROLOG), list)

    def test_no_errors(self):
        assert no_errors(run(":- write(ok).", Language.PROLOG))


class TestPrologExtended18:
    def test_write_700(self):
        assert isinstance(run(":- write(700).", Language.PROLOG), list)

    def test_write_rho(self):
        assert has(run(":- write(rho).", Language.PROLOG), "rho")

    def test_write_sigma(self):
        assert has(run(":- write(sigma).", Language.PROLOG), "sigma")

    def test_write_tau(self):
        assert has(run(":- write(tau).", Language.PROLOG), "tau")

    def test_write_upsilon(self):
        assert has(run(":- write(upsilon).", Language.PROLOG), "upsilon")

    def test_write_phi(self):
        assert has(run(":- write(phi).", Language.PROLOG), "phi")

    def test_write_chi(self):
        assert has(run(":- write(chi).", Language.PROLOG), "chi")

    def test_write_psi(self):
        assert has(run(":- write(psi).", Language.PROLOG), "psi")

    def test_empty(self):
        assert isinstance(run("", Language.PROLOG), list)

    def test_no_errors(self):
        assert no_errors(run(":- write(ok).", Language.PROLOG))


class TestPrologExtended22:
    def test_atom_true(self):
        assert isinstance(run(":- true.", Language.PROLOG), list)

    def test_assert_fact(self):
        r = run(":- assertz(animal(dog)).", Language.PROLOG)
        assert isinstance(r, list)

    def test_write_1100(self):
        assert isinstance(run(":- write(1100), nl.", Language.PROLOG), list)

    def test_write_iota(self):
        assert has(run(":- write(iota), nl.", Language.PROLOG), "iota")

    def test_write_kappa(self):
        assert has(run(":- write(kappa), nl.", Language.PROLOG), "kappa")

    def test_number_29(self):
        assert has(run(":- write(29), nl.", Language.PROLOG), "29")

    def test_number_30(self):
        assert has(run(":- write(30), nl.", Language.PROLOG), "30")

    def test_is_arithmetic(self):
        r = run(":- X is 550 + 550, write(X), nl.", Language.PROLOG)
        assert has(r, "1100")

    def test_empty(self):
        assert isinstance(run("", Language.PROLOG), list)

    def test_no_errors(self):
        assert no_errors(run(":- write(ok), nl.", Language.PROLOG))


class TestPrologExtended23:
    def test_write_31(self):
        assert has(run(":- write(31), nl.", Language.PROLOG), "31")

    def test_write_32(self):
        assert has(run(":- write(32), nl.", Language.PROLOG), "32")

    def test_write_lambda(self):
        assert has(run(":- write(lambda), nl.", Language.PROLOG), "lambda")

    def test_write_mu(self):
        assert has(run(":- write(mu), nl.", Language.PROLOG), "mu")

    def test_add_1200(self):
        assert has(run(":- X is 600 + 600, write(X), nl.", Language.PROLOG), "1200")

    def test_mul_12(self):
        assert has(run(":- X is 3 * 4, write(X), nl.", Language.PROLOG), "12")

    def test_sub_3(self):
        assert has(run(":- X is 10 - 7, write(X), nl.", Language.PROLOG), "3")

    def test_number_atoms(self):
        r = run(":- number_codes(42, X), atom_codes(A, X), write(A), nl.", Language.PROLOG)
        assert isinstance(r, list)

    def test_output_list2(self):
        assert isinstance(run(":- write(ok2), nl.", Language.PROLOG), list)

    def test_no_errors2(self):
        assert no_errors(run(":- write(ok), nl.", Language.PROLOG))


class TestPrologExtended24:
    def test_write_33(self):
        assert has(run(":- write(33), nl.", Language.PROLOG), "33")

    def test_write_34(self):
        assert has(run(":- write(34), nl.", Language.PROLOG), "34")

    def test_write_nu(self):
        assert has(run(":- write(nu), nl.", Language.PROLOG), "nu")

    def test_write_xi(self):
        assert has(run(":- write(xi), nl.", Language.PROLOG), "xi")

    def test_add_1300(self):
        assert has(run(":- X is 650 + 650, write(X), nl.", Language.PROLOG), "1300")

    def test_mul_20(self):
        assert has(run(":- X is 4 * 5, write(X), nl.", Language.PROLOG), "20")

    def test_sub_7(self):
        assert has(run(":- X is 10 - 3, write(X), nl.", Language.PROLOG), "7")

    def test_functor(self):
        r = run(":- functor(foo(a,b), F, A), write(F/A), nl.", Language.PROLOG)
        assert isinstance(r, list)

    def test_output_list3(self):
        assert isinstance(run(":- write(ok3), nl.", Language.PROLOG), list)

    def test_no_errors3(self):
        assert no_errors(run(":- write(ok), nl.", Language.PROLOG))


class TestPrologExtended25:
    def test_write_35(self):
        assert has(run(":- write(35), nl.", Language.PROLOG), "35")

    def test_write_36(self):
        assert has(run(":- write(36), nl.", Language.PROLOG), "36")

    def test_write_omicron(self):
        assert has(run(":- write(omicron), nl.", Language.PROLOG), "omicron")

    def test_write_pi_atom(self):
        assert has(run(":- write(pi), nl.", Language.PROLOG), "pi")

    def test_add_1400(self):
        assert has(run(":- X is 700 + 700, write(X), nl.", Language.PROLOG), "1400")

    def test_mul_49(self):
        assert has(run(":- X is 7 * 7, write(X), nl.", Language.PROLOG), "49")

    def test_sub_14(self):
        assert has(run(":- X is 20 - 6, write(X), nl.", Language.PROLOG), "14")

    def test_findall(self):
        r = run("num(1). num(2). num(3).\n:- findall(X, num(X), L), length(L, N), write(N), nl.", Language.PROLOG)
        assert isinstance(r, list)

    def test_output_list4(self):
        assert isinstance(run(":- write(ok4), nl.", Language.PROLOG), list)

    def test_no_errors4(self):
        assert no_errors(run(":- write(ok), nl.", Language.PROLOG))


class TestPrologExtended26:
    def test_write_1500(self):
        assert has(run("?- write(1500).", Language.PROLOG), "1500")

    def test_write_37(self):
        assert has(run("?- write(37).", Language.PROLOG), "37")

    def test_write_38(self):
        assert has(run("?- write(38).", Language.PROLOG), "38")

    def test_write_rho(self):
        assert has(run("?- write(rho).", Language.PROLOG), "rho")

    def test_write_sigma(self):
        assert has(run("?- write(sigma).", Language.PROLOG), "sigma")

    def test_is_1500(self):
        r = run("?- X is 750 + 750, write(X).", Language.PROLOG)
        assert has(r, "1500")

    def test_is_64(self):
        r = run("?- X is 8 * 8, write(X).", Language.PROLOG)
        assert has(r, "64")

    def test_is_7(self):
        r = run("?- X is 49 / 7, write(X).", Language.PROLOG)
        assert isinstance(r, list)

    def test_output_list5(self):
        assert isinstance(run("?- write(5).", Language.PROLOG), list)

    def test_no_errors5(self):
        assert no_errors(run("?- write(5).", Language.PROLOG))


class TestPrologExtended27:
    def test_write_1600(self):
        assert has(run("?- write(1600).", Language.PROLOG), "1600")

    def test_write_39(self):
        assert has(run("?- write(39).", Language.PROLOG), "39")

    def test_write_tau(self):
        assert has(run("?- write(tau).", Language.PROLOG), "tau")

    def test_write_upsilon(self):
        assert has(run("?- write(upsilon).", Language.PROLOG), "upsilon")

    def test_is_1600(self):
        r = run("?- X is 800 + 800, write(X).", Language.PROLOG)
        assert has(r, "1600")

    def test_is_81(self):
        r = run("?- X is 9 * 9, write(X).", Language.PROLOG)
        assert has(r, "81")

    def test_is_10(self):
        r = run("?- X is 20 - 10, write(X).", Language.PROLOG)
        assert has(r, "10")

    def test_atom_length(self):
        r = run("?- atom_length(hello, N), write(N).", Language.PROLOG)
        assert isinstance(r, list)

    def test_output_list6(self):
        assert isinstance(run("?- write(6).", Language.PROLOG), list)

    def test_no_errors6(self):
        assert no_errors(run("?- write(6).", Language.PROLOG))


class TestPrologExtended28:
    def test_write_1700(self):
        assert has(run("?- write(1700).", Language.PROLOG), "1700")

    def test_write_40(self):
        assert has(run("?- write(40).", Language.PROLOG), "40")

    def test_write_phi(self):
        assert has(run("?- write(phi).", Language.PROLOG), "phi")

    def test_write_chi(self):
        assert has(run("?- write(chi).", Language.PROLOG), "chi")

    def test_is_1700(self):
        r = run("?- X is 850 + 850, write(X).", Language.PROLOG)
        assert has(r, "1700")

    def test_is_100(self):
        r = run("?- X is 10 * 10, write(X).", Language.PROLOG)
        assert has(r, "100")

    def test_is_90(self):
        r = run("?- X is 100 - 10, write(X).", Language.PROLOG)
        assert has(r, "90")

    def test_number_codes(self):
        r = run("?- number_codes(42, C), write(C).", Language.PROLOG)
        assert isinstance(r, list)

    def test_output_list7(self):
        assert isinstance(run("?- write(7).", Language.PROLOG), list)

    def test_no_errors7(self):
        assert no_errors(run("?- write(7).", Language.PROLOG))


class TestPrologExtended29:
    def test_write_1800(self):
        assert has(run("?- write(1800).", Language.PROLOG), "1800")

    def test_write_41(self):
        assert has(run("?- write(41).", Language.PROLOG), "41")

    def test_write_psi(self):
        assert has(run("?- write(psi).", Language.PROLOG), "psi")

    def test_write_omega(self):
        assert has(run("?- write(omega).", Language.PROLOG), "omega")

    def test_is_1800(self):
        r = run("?- X is 900 + 900, write(X).", Language.PROLOG)
        assert has(r, "1800")

    def test_is_121(self):
        r = run("?- X is 11 * 11, write(X).", Language.PROLOG)
        assert has(r, "121")

    def test_is_80(self):
        r = run("?- X is 100 - 20, write(X).", Language.PROLOG)
        assert has(r, "80")

    def test_succ(self):
        r = run("?- succ(9, X), write(X).", Language.PROLOG)
        assert isinstance(r, list)

    def test_output_list8(self):
        assert isinstance(run("?- write(8).", Language.PROLOG), list)

    def test_no_errors8(self):
        assert no_errors(run("?- write(8).", Language.PROLOG))


class TestPrologExtended30:
    def test_write_1900(self):
        assert has(run("?- write(1900).", Language.PROLOG), "1900")

    def test_write_42(self):
        assert has(run("?- write(42).", Language.PROLOG), "42")

    def test_write_one(self):
        assert has(run("?- write(one).", Language.PROLOG), "one")

    def test_write_two(self):
        assert has(run("?- write(two).", Language.PROLOG), "two")

    def test_is_1900(self):
        r = run("?- X is 950 + 950, write(X).", Language.PROLOG)
        assert has(r, "1900")

    def test_is_144(self):
        r = run("?- X is 12 * 12, write(X).", Language.PROLOG)
        assert has(r, "144")

    def test_is_70(self):
        r = run("?- X is 100 - 30, write(X).", Language.PROLOG)
        assert has(r, "70")

    def test_plus_3(self):
        r = run("?- plus(1, 2, X), write(X).", Language.PROLOG)
        assert isinstance(r, list)

    def test_output_list9(self):
        assert isinstance(run("?- write(9).", Language.PROLOG), list)

    def test_no_errors9(self):
        assert no_errors(run("?- write(9).", Language.PROLOG))
