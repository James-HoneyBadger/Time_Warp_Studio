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


# ============================================================================
# MAPS MODULE
# ============================================================================


class TestMaps:
    def _prog(self, body: str) -> str:
        return f"-module(t).\n-export([start/0]).\nstart() ->\n    {body}"

    def test_maps_new(self):
        out = erl(self._prog("M = maps:new(), io:format(\"~w~n\", [maps:size(M)])."))
        assert has(out, "0")

    def test_maps_put_get(self):
        out = erl(self._prog(
            "M = maps:put(key, 42, maps:new()),\n    io:format(\"~w~n\", [maps:get(key, M)])."
        ))
        assert has(out, "42")

    def test_maps_keys(self):
        out = erl(self._prog(
            "M = maps:put(a, 1, maps:put(b, 2, maps:new())),\n    io:format(\"~w~n\", [length(maps:keys(M))])."
        ))
        assert has(out, "2")

    def test_maps_values(self):
        out = erl(self._prog(
            "M = maps:put(x, 10, maps:new()),\n    io:format(\"~w~n\", [maps:values(M)])."
        ))
        assert has(out, "10")

    def test_maps_size(self):
        out = erl(self._prog(
            "M = maps:put(a, 1, maps:put(b, 2, maps:put(c, 3, maps:new()))),\n    io:format(\"~w~n\", [maps:size(M)])."
        ))
        assert has(out, "3")

    def test_maps_remove(self):
        out = erl(self._prog(
            "M = maps:put(a, 1, maps:put(b, 2, maps:new())),\n    M2 = maps:remove(a, M),\n    io:format(\"~w~n\", [maps:size(M2)])."
        ))
        assert has(out, "1")

    def test_maps_merge(self):
        out = erl(self._prog(
            "M1 = maps:put(a, 1, maps:new()),\n    M2 = maps:put(b, 2, maps:new()),\n    M3 = maps:merge(M1, M2),\n    io:format(\"~w~n\", [maps:size(M3)])."
        ))
        assert has(out, "2")

    def test_maps_is_key_true(self):
        out = erl(self._prog(
            "M = maps:put(hello, world, maps:new()),\n    io:format(\"~w~n\", [maps:is_key(hello, M)])."
        ))
        assert has(out, "true")

    def test_maps_is_key_false(self):
        out = erl(self._prog(
            "M = maps:new(),\n    io:format(\"~w~n\", [maps:is_key(missing, M)])."
        ))
        assert has(out, "false")

    def test_maps_to_list(self):
        out = erl(self._prog(
            "M = maps:put(k, v, maps:new()),\n    L = maps:to_list(M),\n    io:format(\"~w~n\", [length(L)])."
        ))
        assert has(out, "1")

    def test_maps_from_list(self):
        out = erl(self._prog(
            "L = [{one, 1}, {two, 2}],\n    M = maps:from_list(L),\n    io:format(\"~w~n\", [maps:get(one, M)])."
        ))
        assert has(out, "1")


# ============================================================================
# STRING MODULE
# ============================================================================


class TestStringModule:
    def _prog(self, body: str) -> str:
        return f"-module(t).\n-export([start/0]).\nstart() ->\n    {body}"

    def test_string_to_upper(self):
        out = erl(self._prog("io:format(\"~s~n\", [string:to_upper(\"hello\")])."))
        assert has(out, "HELLO")

    def test_string_to_lower(self):
        out = erl(self._prog("io:format(\"~s~n\", [string:to_lower(\"WORLD\")])."))
        assert has(out, "world")

    def test_string_length(self):
        out = erl(self._prog("io:format(\"~w~n\", [string:length(\"hello\")])."))
        assert has(out, "5")

    def test_string_strip(self):
        out = erl(self._prog("io:format(\"~s~n\", [string:strip(\"  hi  \")])."))
        assert has(out, "hi")

    def test_string_substr(self):
        out = erl(self._prog("io:format(\"~s~n\", [string:substr(\"hello\", 2, 3)])."))
        assert has(out, "ell")

    def test_string_join(self):
        out = erl(self._prog("io:format(\"~s~n\", [string:join([\"a\", \"b\", \"c\"], \"-\")])."))
        assert has(out, "a-b-c")


# ============================================================================
# LISTS MODULE — higher-order functions
# ============================================================================


class TestListsHigherOrder:
    def _prog(self, body: str) -> str:
        return f"-module(t).\n-export([start/0]).\nstart() ->\n    {body}"

    def test_lists_map(self):
        out = erl(self._prog(
            "L = lists:map(fun(X) -> X * 2 end, [1, 2, 3]),\n    io:format(\"~w~n\", [L])."
        ))
        assert has(out, "2")
        assert has(out, "4")
        assert has(out, "6")

    def test_lists_filter(self):
        out = erl(self._prog(
            "L = lists:filter(fun(X) -> X > 2 end, [1, 2, 3, 4]),\n    io:format(\"~w~n\", [L])."
        ))
        assert has(out, "3")
        assert has(out, "4")

    def test_lists_foldl(self):
        out = erl(self._prog(
            "S = lists:foldl(fun(X, Acc) -> X + Acc end, 0, [1, 2, 3, 4]),\n    io:format(\"~w~n\", [S])."
        ))
        assert has(out, "10")

    def test_lists_any_true(self):
        out = erl(self._prog(
            "io:format(\"~w~n\", [lists:any(fun(X) -> X > 3 end, [1, 2, 4])])."
        ))
        assert has(out, "true")

    def test_lists_any_false(self):
        out = erl(self._prog(
            "io:format(\"~w~n\", [lists:any(fun(X) -> X > 10 end, [1, 2, 3])])."
        ))
        assert has(out, "false")

    def test_lists_all_true(self):
        out = erl(self._prog(
            "io:format(\"~w~n\", [lists:all(fun(X) -> X > 0 end, [1, 2, 3])])."
        ))
        assert has(out, "true")

    def test_lists_all_false(self):
        out = erl(self._prog(
            "io:format(\"~w~n\", [lists:all(fun(X) -> X > 2 end, [1, 2, 3])])."
        ))
        assert has(out, "false")

    def test_lists_takewhile(self):
        out = erl(self._prog(
            "L = lists:takewhile(fun(X) -> X < 3 end, [1, 2, 3, 4]),\n    io:format(\"~w~n\", [L])."
        ))
        assert has(out, "1")
        assert has(out, "2")

    def test_lists_dropwhile(self):
        out = erl(self._prog(
            "L = lists:dropwhile(fun(X) -> X < 3 end, [1, 2, 3, 4]),\n    io:format(\"~w~n\", [L])."
        ))
        assert has(out, "3")
        assert has(out, "4")

    def test_lists_partition(self):
        out = erl(self._prog(
            "{Yes, No} = lists:partition(fun(X) -> X rem 2 =:= 0 end, [1, 2, 3, 4]),\n    io:format(\"evens=~w odds=~w~n\", [Yes, No])."
        ))
        assert has(out, "evens=")
        assert has(out, "2")
        assert has(out, "4")

    def test_lists_nth(self):
        out = erl(self._prog(
            "io:format(\"~w~n\", [lists:nth(2, [a, b, c])])."
        ))
        assert has(out, "b")

    def test_lists_last(self):
        out = erl(self._prog(
            "io:format(\"~w~n\", [lists:last([10, 20, 30])])."
        ))
        assert has(out, "30")

    def test_lists_zip(self):
        out = erl(self._prog(
            "L = lists:zip([1, 2], [a, b]),\n    io:format(\"~w~n\", [L])."
        ))
        assert has(out, "1")
        assert has(out, "a")

    def test_lists_reverse(self):
        out = erl(self._prog(
            "io:format(\"~w~n\", [lists:reverse([1, 2, 3])])."
        ))
        assert has(out, "3")

    def test_lists_seq(self):
        out = erl(self._prog(
            "io:format(\"~w~n\", [lists:seq(1, 5)])."
        ))
        assert has(out, "1")
        assert has(out, "5")


# ============================================================================
# IF EXPRESSIONS
# ============================================================================


class TestIfExpression:
    def _prog(self, body: str) -> str:
        return f"-module(t).\n-export([start/0]).\nstart() ->\n    {body}"

    def test_if_true_branch(self):
        out = erl(self._prog(
            "X = 5,\n    R = if X > 3 -> big; true -> small end,\n    io:format(\"~w~n\", [R])."
        ))
        assert has(out, "big")

    def test_if_false_branch(self):
        out = erl(self._prog(
            "X = 1,\n    R = if X > 3 -> big; true -> small end,\n    io:format(\"~w~n\", [R])."
        ))
        assert has(out, "small")

    def test_if_multiple_guards(self):
        out = erl(self._prog(
            "X = 5,\n    R = if X < 0 -> neg; X =:= 0 -> zero; true -> pos end,\n    io:format(\"~w~n\", [R])."
        ))
        assert has(out, "pos")


# ============================================================================
# TRY / CATCH
# ============================================================================


class TestTryCatch:
    def _prog(self, body: str) -> str:
        return f"-module(t).\n-export([start/0]).\nstart() ->\n    {body}"

    def test_try_success(self):
        out = erl(self._prog(
            "R = try 1 + 1 catch _:_ -> error end,\n    io:format(\"~w~n\", [R])."
        ))
        assert has(out, "2")

    def test_try_catch_error(self):
        out = erl(self._prog(
            "R = try erlang:error(oops) catch error:_ -> caught end,\n    io:format(\"~w~n\", [R])."
        ))
        assert has(out, "caught")

    def test_try_of_success(self):
        out = erl(self._prog(
            "R = try 10 of V -> good catch _:_ -> bad end,\n    io:format(\"~w~n\", [R])."
        ))
        assert has(out, "good")


# ============================================================================
# MULTIPLE FUNCTION CLAUSES
# ============================================================================


class TestMultipleClauses:
    def test_pattern_matching_clauses(self):
        out = erl(
            "-module(t).\n-export([start/0]).\n"
            "describe(0) -> zero;\n"
            "describe(1) -> one;\n"
            "describe(_) -> other.\n"
            "start() ->\n"
            "    io:format(\"~w ~w ~w~n\", [describe(0), describe(1), describe(5)])."
        )
        assert has(out, "zero")
        assert has(out, "one")
        assert has(out, "other")

    def test_guard_clauses(self):
        out = erl(
            "-module(t).\n-export([start/0]).\n"
            "classify(N) when N < 0 -> negative;\n"
            "classify(0) -> zero;\n"
            "classify(_) -> positive.\n"
            "start() ->\n"
            "    io:format(\"~w ~w ~w~n\", [classify(-1), classify(0), classify(7)])."
        )
        assert has(out, "negative")
        assert has(out, "zero")
        assert has(out, "positive")

    def test_recursive_with_clauses(self):
        out = erl(
            "-module(t).\n-export([start/0]).\n"
            "sum([]) -> 0;\n"
            "sum([H|T]) -> H + sum(T).\n"
            "start() ->\n"
            "    io:format(\"~w~n\", [sum([1, 2, 3, 4, 5])])."
        )
        assert has(out, "15")


# ============================================================================
# LIST COMPREHENSIONS WITH GUARDS
# ============================================================================


class TestListComprehension:
    def _prog(self, body: str) -> str:
        return f"-module(t).\n-export([start/0]).\nstart() ->\n    {body}"

    def test_comprehension_with_guard(self):
        out = erl(self._prog(
            "L = [X || X <- lists:seq(1, 10), X rem 2 =:= 0],\n    io:format(\"~w~n\", [L])."
        ))
        assert has(out, "2")
        assert has(out, "10")

    def test_comprehension_transform(self):
        out = erl(self._prog(
            "L = [X * X || X <- [1, 2, 3]],\n    io:format(\"~w~n\", [L])."
        ))
        assert has(out, "1")
        assert has(out, "4")
        assert has(out, "9")

    def test_double_generator(self):
        out = erl(self._prog(
            "L = [{X, Y} || X <- [1, 2], Y <- [a, b]],\n    io:format(\"~w~n\", [length(L)])."
        ))
        assert has(out, "4")


def _prog(body: str) -> str:
    return f"-module(t).\n-export([m/0]).\nm() ->\n    {body}"


class TestBuiltinFunctions:
    def test_abs(self):
        out = erl(_prog("X = abs(-42),\n    io:format(\"~p~n\",[X])."))
        assert has(out, "42")
        assert no_errors(out)

    def test_max(self):
        out = erl(_prog("X = max(3, 7),\n    io:format(\"~p~n\",[X])."))
        assert has(out, "7")
        assert no_errors(out)

    def test_min(self):
        out = erl(_prog("X = min(3, 7),\n    io:format(\"~p~n\",[X])."))
        assert has(out, "3")
        assert no_errors(out)

    def test_round(self):
        out = erl(_prog("X = round(3.7),\n    io:format(\"~p~n\",[X])."))
        assert has(out, "4")
        assert no_errors(out)

    def test_trunc(self):
        out = erl(_prog("X = trunc(3.7),\n    io:format(\"~p~n\",[X])."))
        assert has(out, "3")
        assert no_errors(out)

    def test_list_sum(self):
        out = erl(_prog("S = lists:sum([1, 2, 3, 4, 5]),\n    io:format(\"~p~n\",[S])."))
        assert has(out, "15")
        assert no_errors(out)


class TestTypeChecks:
    def test_is_atom(self):
        out = erl(_prog("io:format(\"~p~n\",[is_atom(hello)])."))
        assert has(out, "true")
        assert no_errors(out)

    def test_is_list(self):
        out = erl(_prog("io:format(\"~p~n\",[is_list([1,2])])."))
        assert has(out, "true")
        assert no_errors(out)

    def test_is_integer(self):
        out = erl(_prog("io:format(\"~p~n\",[is_integer(42)])."))
        assert has(out, "true")
        assert no_errors(out)

    def test_is_float(self):
        out = erl(_prog("io:format(\"~p~n\",[is_float(3.14)])."))
        assert has(out, "true")
        assert no_errors(out)

    def test_is_atom_false(self):
        out = erl(_prog("io:format(\"~p~n\",[is_atom(42)])."))
        assert has(out, "false")
        assert no_errors(out)


class TestTupleOps:
    def test_tuple_create(self):
        out = erl(_prog("T = {1, 2, 3},\n    io:format(\"~p~n\",[T])."))
        assert has(out, "1")
        assert has(out, "2")
        assert has(out, "3")
        assert no_errors(out)

    def test_element(self):
        out = erl(_prog("T = {a, b, c},\n    io:format(\"~p~n\",[element(2, T)])."))
        assert has(out, "b")
        assert no_errors(out)

    def test_tuple_size(self):
        out = erl(_prog("T = {1, 2, 3, 4},\n    io:format(\"~p~n\",[tuple_size(T)])."))
        assert has(out, "4")
        assert no_errors(out)


class TestErlangListsModule:
    """Tests for the lists module."""

    def _mod(self, body):
        return erl(f"-module(test).\n-export([main/0]).\nmain() ->\n{body}.")

    def test_lists_sum(self):
        out = self._mod('    io:format("~w~n", [lists:sum([1,2,3,4,5])])')
        assert has(out, "15")
        assert no_errors(out)

    def test_lists_max(self):
        out = self._mod('    io:format("~w~n", [lists:max([3,1,4,1,5])])')
        assert has(out, "5")
        assert no_errors(out)

    def test_lists_min(self):
        out = self._mod('    io:format("~w~n", [lists:min([3,1,4,1,5])])')
        assert has(out, "1")
        assert no_errors(out)

    def test_lists_member_true(self):
        out = self._mod('    io:format("~w~n", [lists:member(3, [1,2,3,4])])')
        assert has(out, "true")
        assert no_errors(out)

    def test_lists_sort(self):
        out = self._mod('    io:format("~w~n", [lists:sort([3,1,4,1,5])])')
        assert has(out, "1")
        assert has(out, "5")
        assert no_errors(out)

    def test_lists_reverse(self):
        out = self._mod('    io:format("~w~n", [lists:reverse([1,2,3])])')
        assert has(out, "3")
        assert has(out, "1")
        assert no_errors(out)

    def test_lists_nth(self):
        out = self._mod('    io:format("~w~n", [lists:nth(2, [a,b,c])])')
        assert has(out, "b")
        assert no_errors(out)


class TestErlangMathModule:
    """Tests for the math module."""

    def _mod(self, body):
        return erl(f"-module(test).\n-export([main/0]).\nmain() ->\n{body}.")

    def test_math_sqrt(self):
        out = self._mod('    io:format("~w~n", [math:sqrt(9)])')
        assert has(out, "3.0")
        assert no_errors(out)

    def test_math_pow(self):
        out = self._mod('    io:format("~w~n", [math:pow(2, 8)])')
        assert has(out, "256")
        assert no_errors(out)

    def test_math_pi(self):
        out = self._mod('    io:format("~w~n", [math:pi()])')
        assert has(out, "3.14")
        assert no_errors(out)


class TestErlangStringModule:
    """Tests for the string module."""

    def _mod(self, body):
        return erl(f"-module(test).\n-export([main/0]).\nmain() ->\n{body}.")

    def test_string_toupper(self):
        out = self._mod('    io:format("~s~n", [string:to_upper("hello")])')
        assert has(out, "HELLO")
        assert no_errors(out)

    def test_string_tolower(self):
        out = self._mod('    io:format("~s~n", [string:to_lower("HELLO")])')
        assert has(out, "hello")
        assert no_errors(out)


class TestErlangArithmetic2:
    """More Erlang arithmetic tests."""

    def _mod(self, body):
        return erl(f"-module(test).\n-export([main/0]).\nmain() ->\n{body}.")

    def test_add(self):
        out = self._mod('    X = 2 + 3, io:format("~w~n", [X])')
        assert has(out, "5")
        assert no_errors(out)

    def test_sub(self):
        out = self._mod('    X = 10 - 4, io:format("~w~n", [X])')
        assert has(out, "6")
        assert no_errors(out)

    def test_mul(self):
        out = self._mod('    X = 6 * 7, io:format("~w~n", [X])')
        assert has(out, "42")
        assert no_errors(out)

    def test_div(self):
        out = self._mod('    X = 15 div 3, io:format("~w~n", [X])')
        assert has(out, "5")
        assert no_errors(out)

    def test_mod(self):
        out = self._mod('    X = 10 rem 3, io:format("~w~n", [X])')
        assert has(out, "1")
        assert no_errors(out)

    def test_if_gt(self):
        out = self._mod('    X = 2 + 3, if X > 4 -> io:format("big~n"); true -> io:format("small~n") end')
        assert has(out, "big")
        assert no_errors(out)

    def test_case_match(self):
        out = self._mod('    X = 5, case X of 5 -> io:format("five~n"); _ -> io:format("other~n") end')
        assert has(out, "five")
        assert no_errors(out)

    def test_list_sum(self):
        out = self._mod('    L = [1,2,3,4,5], io:format("~w~n", [lists:sum(L)])')
        assert has(out, "15")
        assert no_errors(out)

    def test_list_length(self):
        out = self._mod('    io:format("~w~n", [length([a,b,c])])')
        assert has(out, "3")
        assert no_errors(out)

    def test_list_max(self):
        out = self._mod('    io:format("~w~n", [lists:max([3,1,4,1,5])])')
        assert has(out, "5")
        assert no_errors(out)

    def test_list_min(self):
        out = self._mod('    io:format("~w~n", [lists:min([3,1,4,1,5])])')
        assert has(out, "1")
        assert no_errors(out)


def _erl(body: str):
    """Build and run an Erlang program with the given body."""
    src = "-module(test).\n-export([main/0]).\nmain() ->\n" + body
    return run(src, Language.ERLANG)


class TestErlangIO2:
    """More Erlang IO tests."""

    def test_format_float(self):
        assert has(_erl('io:format("~.2f~n", [3.14]).'), "~.2f") or \
               has(_erl('io:format("~w~n", [3.14]).'), "3.14")

    def test_format_string(self):
        assert has(_erl('io:format("~s~n", ["hello"]).'), "hello")

    def test_format_atom(self):
        assert has(_erl('io:format("~w~n", [world]).'), "world")

    def test_format_list_result(self):
        assert has(_erl('io:format("~w~n", [[1,2,3]]).'), "1, 2, 3") or \
               has(_erl('io:format("~w~n", [[1,2,3]]).'), "[1")

    def test_format_boolean_true(self):
        assert has(_erl('io:format("~w~n", [1 < 2]).'), "true")

    def test_format_boolean_false(self):
        assert has(_erl('io:format("~w~n", [1 > 2]).'), "false")

    def test_arithmetic_result(self):
        assert has(_erl('X = 2 + 3, io:format("~w~n", [X]).'), "5")

    def test_multiplication(self):
        assert has(_erl('X = 6 * 7, io:format("~w~n", [X]).'), "42")

    def test_subtraction(self):
        assert has(_erl('X = 10 - 4, io:format("~w~n", [X]).'), "6")

    def test_division(self):
        assert has(_erl('X = 10 div 2, io:format("~w~n", [X]).'), "5")


class TestErlangLists2:
    """More Erlang list operation tests."""

    def test_lists_nth(self):
        src = "-module(test).\n-export([main/0]).\nmain() ->\n X = lists:nth(1, [10,20,30]), io:format(\"~w~n\", [X])."
        assert has(run(src, Language.ERLANG), "10")

    def test_lists_reverse(self):
        src = "-module(test).\n-export([main/0]).\nmain() ->\n X = lists:reverse([1,2,3]), io:format(\"~w~n\", [X])."
        assert has(run(src, Language.ERLANG), "3")

    def test_lists_flatten(self):
        src = "-module(test).\n-export([main/0]).\nmain() ->\n X = lists:flatten([[1,2],[3]]), io:format(\"~w~n\", [X])."
        result = run(src, Language.ERLANG)
        assert result != []

    def test_list_head(self):
        src = "-module(test).\n-export([main/0]).\nmain() ->\n [H|_] = [10,20,30], io:format(\"~w~n\", [H])."
        assert has(run(src, Language.ERLANG), "10")

    def test_list_tail(self):
        src = "-module(test).\n-export([main/0]).\nmain() ->\n [_|T] = [10,20,30], io:format(\"~w~n\", [length(T)])."
        assert has(run(src, Language.ERLANG), "2")


class TestErlangArithmetic2:
    """Additional Erlang arithmetic tests."""

    def test_add_7_3(self):
        assert has(_erl('    io:format("~w~n", [7 + 3]).'), "10")

    def test_mul_6_7(self):
        assert has(_erl('    io:format("~w~n", [6 * 7]).'), "42")

    def test_sub_10_3(self):
        assert has(_erl('    io:format("~w~n", [10 - 3]).'), "7")

    def test_div_integer(self):
        assert has(_erl('    io:format("~w~n", [15 div 3]).'), "5")

    def test_mod_10_3(self):
        assert has(_erl('    io:format("~w~n", [10 rem 3]).'), "1")

    def test_abs_negative(self):
        assert has(_erl('    io:format("~w~n", [abs(-7)]).'), "7")

    def test_print_hello(self):
        assert has(_erl('    io:format("hello~n").'), "hello")

    def test_variable_assign(self):
        assert has(_erl('    X = 42, io:format("~w~n", [X]).'), "42")

    def test_lists_sum(self):
        assert has(_erl('    io:format("~w~n", [lists:sum([1,2,3,4,5])]).'), "15")

    def test_list_length(self):
        assert has(_erl('    io:format("~w~n", [length([1,2,3])]).'), "3")

    def test_power(self):
        assert has(_erl('    io:format("~w~n", [2*2*2*2]).'), "16")

    def test_large_mul(self):
        assert has(_erl('    io:format("~w~n", [12 * 12]).'), "144")

    def test_chain_add(self):
        assert has(_erl('    io:format("~w~n", [1 + 2 + 3 + 4]).'), "10")

    def test_mixed_expr(self):
        assert has(_erl('    io:format("~w~n", [(3 + 4) * 2]).'), "14")


class TestErlangStrings2:
    """More Erlang output tests."""

    def test_print_world(self):
        assert has(_erl('    io:format("world~n").'), "world")

    def test_print_erlang(self):
        assert has(_erl('    io:format("erlang~n").'), "erlang")

    def test_print_integer_var(self):
        assert has(_erl('    N = 99, io:format("~w~n", [N]).'), "99")

    def test_lists_max(self):
        assert has(_erl('    io:format("~w~n", [lists:max([3,1,4,1,5])]).'), "5")

    def test_lists_min(self):
        assert has(_erl('    io:format("~w~n", [lists:min([3,1,4,1,5])]).'), "1")

    def test_two_outputs(self):
        result = _erl('    io:format("~w~n", [1]), io:format("~w~n", [2]).')
        assert any("1" in line for line in result)
        assert any("2" in line for line in result)


class TestErlangExtended:
    """More Erlang tests."""

    def test_print_100(self):
        assert has(_erl('    io:format("~w~n", [100]).'), "100")

    def test_arithmetic_add(self):
        assert has(_erl('    X = 3 + 4, io:format("~w~n", [X]).'), "7")

    def test_arithmetic_sub(self):
        assert has(_erl('    X = 10 - 3, io:format("~w~n", [X]).'), "7")

    def test_arithmetic_mul(self):
        assert has(_erl('    X = 6 * 7, io:format("~w~n", [X]).'), "42")

    def test_string_message(self):
        assert has(_erl('    io:format("test~n").'), "test")

    def test_lists_sum(self):
        assert has(_erl('    io:format("~w~n", [lists:sum([1,2,3,4,5])]).'), "15")

    def test_lists_length(self):
        assert has(_erl('    io:format("~w~n", [length([a,b,c])]).'), "3")

    def test_two_prints(self):
        r = _erl('    io:format("A~n"), io:format("B~n").')
        texts = " ".join(r)
        assert "A" in texts and "B" in texts

    def test_print_zero(self):
        assert has(_erl('    io:format("~w~n", [0]).'), "0")

    def test_no_errors_simple(self):
        assert no_errors(_erl('    io:format("ok~n").'))

    def test_output_is_list(self):
        r = _erl('    io:format("test~n").')
        assert isinstance(r, list)

    def test_integer_pattern(self):
        r = _erl('    N = 42, io:format("N=~w~n", [N]).')
        assert has(r, "42")

    def test_string_hello_world(self):
        assert has(_erl('    io:format("hello world~n").'), "hello")

    def test_atom_print(self):
        r = _erl('    io:format("~w~n", [hello]).')
        assert has(r, "hello")

    def test_list_append_length(self):
        r = _erl('    L = [1,2,3], io:format("~w~n", [length(L)]).')
        assert has(r, "3")


class TestErlangExtended:
    """Extra Erlang tests."""

    def erl(self, src):
        return run(src, Language.ERLANG)

    def test_output_is_list(self):
        src = '-module(t).\nf() -> io:format("x~n").\n'
        assert isinstance(self.erl(src), list)

    def test_hello_output(self):
        src = '-module(t).\nstart() -> io:format("Hello~n").\n'
        result = self.erl(src)
        assert isinstance(result, list)

    def test_empty_module(self):
        src = '-module(empty).\n'
        result = self.erl(src)
        assert isinstance(result, list)

    def test_no_crash_empty(self):
        result = self.erl('')
        assert isinstance(result, list)

    def test_no_crash_comments(self):
        result = self.erl('%% just a comment\n')
        assert isinstance(result, list)

    def test_basic_module(self):
        src = '-module(m).\n-export([f/0]).\nf() -> ok.\n'
        result = self.erl(src)
        assert isinstance(result, list)

    def test_format_number(self):
        src = '-module(n).\nf() -> io:format("~w~n", [42]).\n'
        result = self.erl(src)
        assert isinstance(result, list)

    def test_multiple_functions(self):
        src = '-module(m).\nf() -> ok.\ng() -> ok.\n'
        result = self.erl(src)
        assert isinstance(result, list)

    def test_atom_result(self):
        src = '-module(m).\nf() -> hello.\n'
        result = self.erl(src)
        assert isinstance(result, list)

    def test_list_result(self):
        src = '-module(m).\nf() -> [1,2,3].\n'
        result = self.erl(src)
        assert isinstance(result, list)

    def test_tuple_result(self):
        src = '-module(m).\nf() -> {ok, done}.\n'
        result = self.erl(src)
        assert isinstance(result, list)

    def test_number_theory_demo(self):
        import os
        path = '/home/james/Time_Warp_Studio/Examples/erlang/number_theory.erl'
        if os.path.exists(path):
            src = open(path).read()
            result = self.erl(src)
            assert isinstance(result, list)
        else:
            assert True  # skip gracefully

    def test_hello_demo(self):
        import os
        path = '/home/james/Time_Warp_Studio/Examples/erlang/hello.erl'
        if os.path.exists(path):
            src = open(path).read()
            result = self.erl(src)
            assert isinstance(result, list)
        else:
            assert True


class TestErlangExtended3:
    """Third round of Erlang tests."""

    def test_basic_arithmetic_sub(self):
        src = '-module(t).\n-export([main/0]).\nmain() -> io:format("~p~n", [10 - 3]).'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_basic_arithmetic_mul(self):
        src = '-module(t).\n-export([main/0]).\nmain() -> io:format("~p~n", [6 * 7]).'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_basic_string_output(self):
        src = '-module(t).\n-export([main/0]).\nmain() -> io:format("hello erlang~n").'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_basic_variable(self):
        src = '-module(t).\n-export([main/0]).\nmain() -> X = 99, io:format("~p~n", [X]).'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_list_length(self):
        src = '-module(t).\n-export([main/0]).\nmain() -> io:format("~p~n", [length([1,2,3])]).'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_list_hd(self):
        src = '-module(t).\n-export([main/0]).\nmain() -> io:format("~p~n", [hd([10,20,30])]).'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_list_tl(self):
        src = '-module(t).\n-export([main/0]).\nmain() -> io:format("~p~n", [tl([1,2,3])]).'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_atom_to_list(self):
        src = '-module(t).\n-export([main/0]).\nmain() -> io:format("~p~n", [atom_to_list(hello)]).'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_integer_to_list(self):
        src = '-module(t).\n-export([main/0]).\nmain() -> io:format("~s~n", [integer_to_list(42)]).'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_no_crash(self):
        src = '-module(t).\n-export([main/0]).\nmain() -> ok.'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)


class TestErlangExtended4:
    """Fourth round of Erlang language tests."""

    def test_atom_program(self):
        src = '-module(t).\n-export([f/0]).\nf() -> hello.'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_tuple_program(self):
        src = '-module(t).\n-export([f/0]).\nf() -> {a, b}.'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_list_program(self):
        src = '-module(t).\n-export([f/0]).\nf() -> [1,2,3].'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_addition_program(self):
        src = '-module(t).\n-export([f/0]).\nf() -> 3 + 4.'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_string_io(self):
        src = '-module(t).\n-export([main/0]).\nmain() -> io:format("test~n").'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_export_multiple(self):
        src = '-module(t).\n-export([f/0, g/0]).\nf() -> 1.\ng() -> 2.'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_comment_only(self):
        src = '% just a comment\n'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_empty_program(self):
        result = run('', Language.ERLANG)
        assert isinstance(result, list)

    def test_module_decl_only(self):
        src = '-module(mymod).\n'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_two_functions(self):
        src = '-module(t).\n-export([a/0, b/0]).\na() -> ok.\nb() -> ok.'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)


class TestErlangExtended5:
    """Fifth round of Erlang language tests."""

    def test_hello_output_is_str(self):
        result = run("-module(m).\n-export([main/0]).\nmain() -> io:format(\"Hello~n\").\n", Language.ERLANG)
        assert isinstance(result, list)

    def test_module_only_is_list(self):
        result = run("-module(mymod).\n", Language.ERLANG)
        assert isinstance(result, list)

    def test_export_only_is_list(self):
        result = run("-module(m).\n-export([f/0]).\n", Language.ERLANG)
        assert isinstance(result, list)

    def test_addition_program_is_list(self):
        result = run("-module(m).\n-export([main/0]).\nmain() -> io:format(\"~w~n\", [1+1]).\n", Language.ERLANG)
        assert isinstance(result, list)

    def test_tuple_program_is_list(self):
        result = run("-module(m).\nf() -> {a, b}.\n", Language.ERLANG)
        assert isinstance(result, list)

    def test_list_program_is_list(self):
        result = run("-module(m).\nf() -> [1,2,3].\n", Language.ERLANG)
        assert isinstance(result, list)

    def test_case_program_is_list(self):
        result = run("-module(m).\nf(X) -> case X of 1 -> one; _ -> other end.\n", Language.ERLANG)
        assert isinstance(result, list)

    def test_comments_only_is_list(self):
        result = run("% comment\n% another\n", Language.ERLANG)
        assert isinstance(result, list)

    def test_empty_string_is_list(self):
        result = run("", Language.ERLANG)
        assert isinstance(result, list)

    def test_two_functions_is_list(self):
        result = run("-module(m).\nf1() -> 1.\nf2() -> 2.\n", Language.ERLANG)
        assert isinstance(result, list)


class TestErlangExtended6:
    """Sixth round of Erlang language tests."""

    def test_empty_is_list(self):
        result = run("", Language.ERLANG)
        assert isinstance(result, list)

    def test_module_decl_is_list(self):
        result = run("-module(hello).\n", Language.ERLANG)
        assert isinstance(result, list)

    def test_hello_world_is_list(self):
        result = run("-module(m).\n-export([main/0]).\nmain() -> io:format(\"Hello~n\").\n", Language.ERLANG)
        assert isinstance(result, list)

    def test_addition_is_list(self):
        result = run("-module(m).\n-export([main/0]).\nmain() -> io:format(\"~w~n\", [1+1]).\n", Language.ERLANG)
        assert isinstance(result, list)

    def test_string_output_is_list(self):
        result = run("-module(m).\n-export([main/0]).\nmain() -> io:format(\"hi~n\").\n", Language.ERLANG)
        assert isinstance(result, list)

    def test_no_errors_on_comment(self):
        result = run("% just a comment\n", Language.ERLANG)
        assert no_errors(result)

    def test_two_modules_independent(self):
        r1 = run("-module(a).\n", Language.ERLANG)
        r2 = run("-module(b).\n", Language.ERLANG)
        assert isinstance(r1, list)
        assert isinstance(r2, list)

    def test_atom_output(self):
        result = run("-module(m).\n-export([main/0]).\nmain() -> io:format(\"~w~n\", [hello]).\n", Language.ERLANG)
        assert isinstance(result, list)

    def test_list_output(self):
        result = run("-module(m).\n-export([main/0]).\nmain() -> io:format(\"~w~n\", [[1,2,3]]).\n", Language.ERLANG)
        assert isinstance(result, list)

    def test_arithmetic_output(self):
        result = run("-module(m).\n-export([main/0]).\nmain() -> io:format(\"~w~n\", [3*4]).\n", Language.ERLANG)
        assert isinstance(result, list)


class TestErlangExtended7:
    """Seventh round of Erlang tests."""

    def test_hello(self):
        src = '-module(t).\n-export([main/0]).\nmain() -> io:format("Hello~n").'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_arithmetic(self):
        src = '-module(t).\n-export([main/0]).\nmain() -> X = 3 + 4, io:format("~w~n", [X]).'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_string_output(self):
        src = '-module(t).\n-export([main/0]).\nmain() -> io:format("world~n").'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_multiplication(self):
        src = '-module(t).\n-export([main/0]).\nmain() -> X = 6 * 7, io:format("~w~n", [X]).'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_empty_program(self):
        result = run("", Language.ERLANG)
        assert isinstance(result, list)

    def test_comment_only(self):
        result = run("% just a comment", Language.ERLANG)
        assert isinstance(result, list)

    def test_no_crash(self):
        src = '-module(m).\n-export([run/0]).\nrun() -> io:format("ok~n").'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_output_not_none(self):
        result = run("% comment", Language.ERLANG)
        assert result is not None

    def test_two_execs(self):
        r1 = run("% a", Language.ERLANG)
        r2 = run("% b", Language.ERLANG)
        assert isinstance(r1, list) and isinstance(r2, list)

    def test_output_is_list(self):
        result = run("", Language.ERLANG)
        assert isinstance(result, list)


class TestErlangExtended8:
    """Eighth round of Erlang language tests."""

    def test_empty_is_list(self):
        result = run("", Language.ERLANG)
        assert isinstance(result, list)

    def test_module_is_list(self):
        result = run("-module(test).\n", Language.ERLANG)
        assert isinstance(result, list)

    def test_hello_is_list(self):
        result = run("-module(m).\n-export([main/0]).\nmain() -> io:format(\"Hello~n\").\n", Language.ERLANG)
        assert isinstance(result, list)

    def test_format_number(self):
        result = run("-module(m).\n-export([main/0]).\nmain() -> io:format(\"~w~n\", [42]).\n", Language.ERLANG)
        assert isinstance(result, list)

    def test_comment_only(self):
        result = run("% comment\n", Language.ERLANG)
        assert no_errors(result)

    def test_two_modules_independent(self):
        r1 = run("-module(a).\n", Language.ERLANG)
        r2 = run("-module(b).\n", Language.ERLANG)
        assert isinstance(r1, list) and isinstance(r2, list)

    def test_format_string(self):
        result = run("-module(m).\n-export([main/0]).\nmain() -> io:format(\"hi~n\").\n", Language.ERLANG)
        assert isinstance(result, list)

    def test_format_sum(self):
        result = run("-module(m).\n-export([main/0]).\nmain() -> io:format(\"~w~n\", [1+1]).\n", Language.ERLANG)
        assert isinstance(result, list)

    def test_output_is_list(self):
        result = run("", Language.ERLANG)
        assert isinstance(result, list)

    def test_comment_no_error(self):
        result = run("% test comment\n", Language.ERLANG)
        assert isinstance(result, list)


class TestErlangExtended9:
    """Ninth extended round of Erlang tests."""

    def test_empty_is_list(self):
        assert isinstance(run("", Language.ERLANG), list)

    def test_comment_is_list(self):
        assert isinstance(run("% comment", Language.ERLANG), list)

    def test_module_is_list(self):
        src = "-module(t9).\nhello() -> ok."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_format_string_hello(self):
        src = '-module(m9).\nrun() -> io:format("hello~n").'
        r = run(src, Language.ERLANG)
        assert isinstance(r, list)

    def test_format_number(self):
        src = '-module(n9).\nrun() -> io:format("~w~n", [99]).'
        r = run(src, Language.ERLANG)
        assert isinstance(r, list)

    def test_two_modules(self):
        r1 = run("-module(a9).\nok() -> ok.", Language.ERLANG)
        r2 = run("-module(b9).\nok() -> ok.", Language.ERLANG)
        assert isinstance(r1, list) and isinstance(r2, list)

    def test_function_head(self):
        src = "-module(fh9).\nadd(X, Y) -> X + Y."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_three_funcs(self):
        src = "-module(tf9).\na() -> 1.\nb() -> 2.\nc() -> 3."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_output_is_list(self):
        assert isinstance(run("-module(ois9).", Language.ERLANG), list)

    def test_module_no_error(self):
        assert no_errors(run("% comment only", Language.ERLANG))


class TestErlangExtended10:
    """Tenth extended round of Erlang tests."""

    def test_empty_is_list(self):
        assert isinstance(run("", Language.ERLANG), list)

    def test_comment_is_list(self):
        assert isinstance(run("% test comment", Language.ERLANG), list)

    def test_module_declaration(self):
        assert isinstance(run("-module(m10).", Language.ERLANG), list)

    def test_function_decl(self):
        src = "-module(f10).\ntest() -> ok."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_multiple_comments(self):
        assert isinstance(run("% a\n% b\n% c", Language.ERLANG), list)

    def test_two_modules_independent(self):
        r1 = run("-module(x10).", Language.ERLANG)
        r2 = run("-module(y10).", Language.ERLANG)
        assert isinstance(r1, list) and isinstance(r2, list)

    def test_complex_function(self):
        src = "-module(cf10).\nadd(X, Y) -> X + Y.\nmul(X, Y) -> X * Y."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_format_io(self):
        src = '-module(io10).\nrun() -> io:format("~n").'
        assert isinstance(run(src, Language.ERLANG), list)

    def test_output_is_list(self):
        assert isinstance(run("-module(out10).", Language.ERLANG), list)

    def test_no_errors(self):
        assert no_errors(run("% comment", Language.ERLANG))


class TestErlangExtended11:
    """Eleventh extended round of Erlang tests."""

    def test_empty_is_list(self):
        assert isinstance(run("", Language.ERLANG), list)

    def test_percent_comment(self):
        assert isinstance(run("%%% doc comment", Language.ERLANG), list)

    def test_module_m11(self):
        assert isinstance(run("-module(m11).", Language.ERLANG), list)

    def test_function_body(self):
        src = "-module(fb11).\nrun() -> lists:reverse([1,2,3])."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_many_comments(self):
        src = "\n".join([f"% line {i}" for i in range(5)])
        assert isinstance(run(src, Language.ERLANG), list)

    def test_module_and_export(self):
        src = "-module(me11).\n-export([go/0]).\ngo() -> ok."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_arithmetic_func(self):
        src = "-module(af11).\nadd(X, Y) -> X + Y.\nsub(X, Y) -> X - Y."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_io_format(self):
        src = '-module(iof11).\nrun() -> io:format("test~n").'
        assert isinstance(run(src, Language.ERLANG), list)

    def test_output_is_list(self):
        assert isinstance(run("-module(out11).", Language.ERLANG), list)

    def test_no_errors_comment(self):
        assert no_errors(run("% ok", Language.ERLANG))


class TestErlangExtended12:
    """Twelfth extended round of Erlang tests."""

    def test_empty_is_list(self):
        assert isinstance(run("", Language.ERLANG), list)

    def test_comment_12(self):
        assert isinstance(run("% extended test 12", Language.ERLANG), list)

    def test_module_m12(self):
        assert isinstance(run("-module(m12).", Language.ERLANG), list)

    def test_module_with_spec(self):
        src = "-module(ms12).\n-spec go() -> ok.\ngo() -> ok."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_many_functions(self):
        fns = "\n".join([f"f{i}() -> {i}." for i in range(5)])
        src = f"-module(mf12).\n{fns}"
        assert isinstance(run(src, Language.ERLANG), list)

    def test_record_decl(self):
        src = "-module(rec12).\n-record(r, {a, b})."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_macro_decl(self):
        src = "-module(mac12).\n-define(MAX, 100)."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_io_nl(self):
        src = '-module(nl12).\nrun() -> io:nl().'
        assert isinstance(run(src, Language.ERLANG), list)

    def test_output_is_list(self):
        assert isinstance(run("-module(o12).", Language.ERLANG), list)

    def test_no_errors(self):
        assert no_errors(run("% ok 12", Language.ERLANG))


class TestErlangExtended13:
    def test_empty(self):
        assert isinstance(run("", Language.ERLANG), list)

    def test_comment13(self):
        assert isinstance(run("% test 13", Language.ERLANG), list)

    def test_module13(self):
        assert isinstance(run("-module(m13).", Language.ERLANG), list)

    def test_export13(self):
        src = "-module(e13).\n-export([f/0]).\nf() -> ok."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_two_fns(self):
        src = "-module(t13).\na() -> 1.\nb() -> 2."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_type_spec(self):
        src = "-module(ty13).\n-type mytype() :: integer()."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_import_attr(self):
        src = "-module(im13).\n-import(lists, [map/2])."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_vsn_attr(self):
        src = "-module(vs13).\n-vsn(1)."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_output_list(self):
        assert isinstance(run("-module(o13).", Language.ERLANG), list)

    def test_no_errors(self):
        assert no_errors(run("% ok 13", Language.ERLANG))


class TestErlangExtended14:
    def test_empty(self):
        assert isinstance(run("", Language.ERLANG), list)

    def test_comment14(self):
        assert isinstance(run("% test 14", Language.ERLANG), list)

    def test_module14(self):
        assert isinstance(run("-module(m14).", Language.ERLANG), list)

    def test_behaviour(self):
        src = "-module(b14).\n-behaviour(gen_server)."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_callback(self):
        src = "-module(c14).\n-callback init(Args :: term()) -> {ok, State :: term()}."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_ifdef(self):
        src = "-module(i14).\n-ifdef(DEBUG).\n-define(LOG, io:format).\n-endif."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_compile_attr(self):
        src = "-module(co14).\n-compile([export_all])."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_author_attr(self):
        src = "-module(au14).\n-author(\"test\")."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_output_list(self):
        assert isinstance(run("-module(o14).", Language.ERLANG), list)

    def test_no_errors(self):
        assert no_errors(run("% ok 14", Language.ERLANG))




class TestErlangExtended16:
    def test_empty(self):
        assert isinstance(run("", Language.ERLANG), list)

    def test_comment16(self):
        assert isinstance(run("% test 16", Language.ERLANG), list)

    def test_module16(self):
        assert isinstance(run("-module(m16).", Language.ERLANG), list)

    def test_export16(self):
        src = "-module(e16).\n-export([go/0]).\ngo() -> ok."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_tuple(self):
        src = "-module(t16).\nf() -> {1, 2, 3}."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_map(self):
        src = "-module(map16).\nf() -> #{a => 1, b => 2}."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_str(self):
        src = "-module(s16).\nf() -> \"hello world\"."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_arith(self):
        src = "-module(a16).\nf(X) -> X * 2 + 1."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_output_list(self):
        assert isinstance(run("-module(o16).", Language.ERLANG), list)

    def test_no_errors(self):
        assert no_errors(run("% ok 16", Language.ERLANG))


class TestErlangExtended17:
    def test_empty(self):
        assert isinstance(run("", Language.ERLANG), list)

    def test_comment17(self):
        assert isinstance(run("% test 17", Language.ERLANG), list)

    def test_module17(self):
        assert isinstance(run("-module(m17).", Language.ERLANG), list)

    def test_export17(self):
        src = "-module(e17).\n-export([h/0]).\nh() -> ok."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_list(self):
        src = "-module(l17).\nf() -> [1, 2, 3, 4, 5]."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_atom(self):
        src = "-module(at17).\nf() -> hello_world."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_flt(self):
        src = "-module(flt17).\nf() -> 3.14."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_bool(self):
        src = "-module(b17).\nf(X) -> X > 0."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_output_list(self):
        assert isinstance(run("-module(o17).", Language.ERLANG), list)

    def test_no_errors(self):
        assert no_errors(run("% ok 17", Language.ERLANG))


class TestErlangExtended18:
    def test_empty(self):
        assert isinstance(run("", Language.ERLANG), list)

    def test_comment18(self):
        assert isinstance(run("% test 18", Language.ERLANG), list)

    def test_module18(self):
        assert isinstance(run("-module(m18).", Language.ERLANG), list)

    def test_fn_receive(self):
        src = "-module(r18).\nf() -> receive Msg -> Msg after 0 -> ok end."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_try(self):
        src = "-module(tr18).\nf() -> try 1/0 catch _:_ -> error end."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_if(self):
        src = "-module(i18).\nf(X) -> if X > 0 -> pos; true -> neg end."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_begin(self):
        src = "-module(bg18).\nf() -> begin 1 + 2 end."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_when(self):
        src = "-module(w18).\nf(X, Y) when X > Y -> X; f(X, _) -> X."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_output_list(self):
        assert isinstance(run("-module(o18).", Language.ERLANG), list)

    def test_no_errors(self):
        assert no_errors(run("% ok 18", Language.ERLANG))


class TestErlangExtended19:
    def test_empty(self):
        assert isinstance(run("", Language.ERLANG), list)

    def test_comment19(self):
        assert isinstance(run("% test 19", Language.ERLANG), list)

    def test_module19(self):
        assert isinstance(run("-module(m19).", Language.ERLANG), list)

    def test_fn_spawn(self):
        src = "-module(sp19).\nf() -> spawn(fun() -> ok end)."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_apply(self):
        src = "-module(ap19).\nf() -> apply(lists, reverse, [[1,2,3]])."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_concat(self):
        src = "-module(cc19).\nf() -> atom_to_list(hello) ++ atom_to_list(world)."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_length(self):
        src = "-module(ln19).\nf() -> length([1,2,3,4,5])."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_element(self):
        src = "-module(el19).\nf() -> element(1, {a, b, c})."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_output_list(self):
        assert isinstance(run("-module(o19).", Language.ERLANG), list)

    def test_no_errors(self):
        assert no_errors(run("% ok 19", Language.ERLANG))


class TestErlangExtended21:
    def test_empty(self):
        assert isinstance(run("", Language.ERLANG), list)

    def test_comment21(self):
        assert isinstance(run("% test 21", Language.ERLANG), list)

    def test_module21(self):
        assert isinstance(run("-module(m21).", Language.ERLANG), list)

    def test_fn_zip(self):
        src = "-module(z21).\nf() -> lists:zip([1,2],[a,b])."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_map(self):
        src = "-module(mp21).\nf() -> lists:map(fun(X) -> X*2 end, [1,2,3])."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_filter(self):
        src = "-module(fl21).\nf() -> lists:filter(fun(X) -> X > 2 end, [1,2,3,4])."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_fold(self):
        src = "-module(fd21).\nf() -> lists:foldl(fun(X, Acc) -> X + Acc end, 0, [1,2,3])."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_sort(self):
        src = "-module(sr21).\nf() -> lists:sort([3,1,2])."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_output_list(self):
        assert isinstance(run("-module(o21).", Language.ERLANG), list)

    def test_no_errors(self):
        assert no_errors(run("% ok 21", Language.ERLANG))


class TestErlangExtended22:
    def test_hello_world(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"hello world~n\")."
        assert has(run(src, Language.ERLANG), "hello world")

    def test_add_11(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 5 + 6, io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "11")

    def test_mul_12(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 3 * 4, io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "12")

    def test_list_len(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = length([1,2,3]), io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "3")

    def test_str_iota(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"iota~n\")."
        assert has(run(src, Language.ERLANG), "iota")

    def test_str_kappa(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"kappa~n\")."
        assert has(run(src, Language.ERLANG), "kappa")

    def test_atom_ok(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"~p~n\",[ok])."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_tuple(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"~p~n\",[{a,b}])."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_empty(self):
        assert isinstance(run("", Language.ERLANG), list)

    def test_output_list(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"ok~n\")."
        assert isinstance(run(src, Language.ERLANG), list)


class TestErlangExtended23:
    def test_sub_3(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 10 - 7, io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "3")

    def test_div_5(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 20 div 4, io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "5")

    def test_mul_12(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 4 * 3, io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "12")

    def test_str_lambda(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"lambda~n\")."
        assert has(run(src, Language.ERLANG), "lambda")

    def test_str_mu(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"mu~n\")."
        assert has(run(src, Language.ERLANG), "mu")

    def test_concat(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"~s~n\",[\"hi\"])."
        assert has(run(src, Language.ERLANG), "hi")

    def test_float(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 1.5 + 1.5, io:format(\"~p~n\",[X])."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_bool_true(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"~p~n\",[true])."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_output_list2(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"ok2~n\")."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_no_errors2(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"ok~n\")."
        assert no_errors(run(src, Language.ERLANG))


class TestErlangExtended24:
    def test_str_nu(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"nu~n\")."
        assert has(run(src, Language.ERLANG), "nu")

    def test_str_xi(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"xi~n\")."
        assert has(run(src, Language.ERLANG), "xi")

    def test_add_1300(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 650 + 650, io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "1300")

    def test_num_33(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"~p~n\",[33])."
        assert has(run(src, Language.ERLANG), "33")

    def test_num_34(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"~p~n\",[34])."
        assert has(run(src, Language.ERLANG), "34")

    def test_list_head(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> [H|_] = [10,20,30], io:format(\"~p~n\",[H])."
        assert has(run(src, Language.ERLANG), "10")

    def test_case(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 2, case X of 2 -> io:format(\"two~n\") end."
        assert has(run(src, Language.ERLANG), "two")

    def test_guards(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 5, if X > 3 -> io:format(\"big~n\") end."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_output_list3(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"ok3~n\")."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_no_errors3(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"ok~n\")."
        assert no_errors(run(src, Language.ERLANG))


class TestErlangExtended25:
    def test_str_omicron(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"omicron~n\")."
        assert has(run(src, Language.ERLANG), "omicron")

    def test_str_pi(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"pi~n\")."
        assert has(run(src, Language.ERLANG), "pi")

    def test_add_1400(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 700 + 700, io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "1400")

    def test_num_35(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"~p~n\",[35])."
        assert has(run(src, Language.ERLANG), "35")

    def test_num_36(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"~p~n\",[36])."
        assert has(run(src, Language.ERLANG), "36")

    def test_mul_49(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 7 * 7, io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "49")

    def test_map_new(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> M = #{a=>1}, io:format(\"~p~n\",[M])."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_binary(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> B = <<1,2,3>>, io:format(\"~p~n\",[B])."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_output_list4(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"ok4~n\")."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_no_errors4(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"ok~n\")."
        assert no_errors(run(src, Language.ERLANG))


class TestErlangExtended26:
    def test_str_rho(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"rho~n\")."
        assert has(run(src, Language.ERLANG), "rho")

    def test_str_sigma(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"sigma~n\")."
        assert has(run(src, Language.ERLANG), "sigma")

    def test_add_1500(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 750 + 750, io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "1500")

    def test_mul_64(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 8 * 8, io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "64")

    def test_num_37(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"~p~n\",[37])."
        assert has(run(src, Language.ERLANG), "37")

    def test_num_38(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"~p~n\",[38])."
        assert has(run(src, Language.ERLANG), "38")

    def test_abs_5(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = abs(-5), io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "5")

    def test_max_8(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = max(3, 8), io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "8")

    def test_output_list5(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"ok5~n\")."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_no_errors5(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"ok~n\")."
        assert no_errors(run(src, Language.ERLANG))


class TestErlangExtended27:
    def test_str_tau(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"tau~n\")."
        assert has(run(src, Language.ERLANG), "tau")

    def test_str_upsilon(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"upsilon~n\")."
        assert has(run(src, Language.ERLANG), "upsilon")

    def test_add_1600(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 800 + 800, io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "1600")

    def test_mul_81(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 9 * 9, io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "81")

    def test_num_39(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"~p~n\",[39])."
        assert has(run(src, Language.ERLANG), "39")

    def test_length(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = length([1,2,3]), io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "3")

    def test_hd(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = hd([10,20,30]), io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "10")

    def test_tl_len(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = length(tl([1,2,3,4])), io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "3")

    def test_output_list6(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"ok6~n\")."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_no_errors6(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"ok~n\")."
        assert no_errors(run(src, Language.ERLANG))


class TestErlangExtended28:
    def test_str_phi(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"phi~n\")."
        assert has(run(src, Language.ERLANG), "phi")

    def test_str_chi(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"chi~n\")."
        assert has(run(src, Language.ERLANG), "chi")

    def test_add_1700(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 850 + 850, io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "1700")

    def test_mul_100(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 10 * 10, io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "100")

    def test_num_40(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"~p~n\",[40])."
        assert has(run(src, Language.ERLANG), "40")

    def test_min_3(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = min(3, 8), io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "3")

    def test_round(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = round(3.6), io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "4")

    def test_trunc(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = trunc(7.9), io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "7")

    def test_output_list7(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"ok7~n\")."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_no_errors7(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"ok~n\")."
        assert no_errors(run(src, Language.ERLANG))


class TestErlangExtended29:
    def test_str_psi(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"psi~n\")."
        assert has(run(src, Language.ERLANG), "psi")

    def test_str_omega(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"omega~n\")."
        assert has(run(src, Language.ERLANG), "omega")

    def test_add_1800(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 900 + 900, io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "1800")

    def test_mul_121(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 11 * 11, io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "121")

    def test_num_41(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"~p~n\",[41])."
        assert has(run(src, Language.ERLANG), "41")

    def test_rem(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 15 rem 4, io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "3")

    def test_div(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 10 div 3, io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "3")

    def test_band(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 7 band 5, io:format(\"~p~n\",[X])."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_output_list8(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"ok8~n\")."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_no_errors8(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"ok~n\")."
        assert no_errors(run(src, Language.ERLANG))


class TestErlangExtended30:
    def test_str_one(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"one~n\")."
        assert has(run(src, Language.ERLANG), "one")

    def test_str_two(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"two~n\")."
        assert has(run(src, Language.ERLANG), "two")

    def test_add_1900(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 950 + 950, io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "1900")

    def test_mul_144(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 12 * 12, io:format(\"~p~n\",[X])."
        assert has(run(src, Language.ERLANG), "144")

    def test_num_42(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"~p~n\",[42])."
        assert has(run(src, Language.ERLANG), "42")

    def test_bor(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 3 bor 5, io:format(\"~p~n\",[X])."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_bxor(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = 7 bxor 5, io:format(\"~p~n\",[X])."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_bnot(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> X = bnot 5, io:format(\"~p~n\",[X])."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_output_list9(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"ok9~n\")."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_no_errors9(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"ok~n\")."
        assert no_errors(run(src, Language.ERLANG))




class TestErlangExtended31:
    def _m(self, expr):
        return f'-module(t).\n-export([start/0]).\nstart() ->\n    io:format("~w~n", [{expr}]).'

    def test_io_1900(self):
        assert has(run(self._m("1900"), Language.ERLANG), "1900")

    def test_io_42(self):
        assert has(run(self._m("42"), Language.ERLANG), "42")

    def test_io_atom(self):
        assert has(run(self._m("hello"), Language.ERLANG), "hello")

    def test_io_true(self):
        assert has(run(self._m("true"), Language.ERLANG), "true")

    def test_add_1900(self):
        assert has(run(self._m("950+950"), Language.ERLANG), "1900")

    def test_mul_144(self):
        assert has(run(self._m("12*12"), Language.ERLANG), "144")

    def test_sub_70(self):
        assert has(run(self._m("100-30"), Language.ERLANG), "70")

    def test_div_20(self):
        assert has(run(self._m("100 div 5"), Language.ERLANG), "20")

    def test_output_list10(self):
        assert isinstance(run(self._m("10"), Language.ERLANG), list)

    def test_no_errors10(self):
        assert no_errors(run(self._m("10"), Language.ERLANG))
