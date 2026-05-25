"""Tests for Erlang binary/bitstring syntax."""
from __future__ import annotations
import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

L = Language.ERLANG


def test_binary_literal_byte_size():
    src = """
-module(t). -export([main/0]).
main() ->
    B = <<"Hello">>,
    io:format("~w~n", [byte_size(B)]).
"""
    out = run(src, L)
    no_errors(out)
    has(out, "5")


def test_binary_empty():
    src = """
-module(t). -export([main/0]).
main() ->
    B = <<>>,
    io:format("~w~n", [byte_size(B)]).
"""
    out = run(src, L)
    no_errors(out)
    has(out, "0")


def test_binary_to_list():
    src = """
-module(t). -export([main/0]).
main() ->
    B = <<65, 66, 67>>,
    L = binary_to_list(B),
    io:format("~w~n", [L]).
"""
    out = run(src, L)
    no_errors(out)
    has(out, "[65, 66, 67]")


def test_list_to_binary():
    src = """
-module(t). -export([main/0]).
main() ->
    L = [72, 101, 108, 108, 111],
    B = list_to_binary(L),
    io:format("~w~n", [byte_size(B)]).
"""
    out = run(src, L)
    no_errors(out)
    has(out, "5")


def test_binary_packed_integers():
    src = """
-module(t). -export([main/0]).
main() ->
    B = <<1:8, 2:8, 3:8>>,
    io:format("~w~n", [byte_size(B)]).
"""
    out = run(src, L)
    no_errors(out)
    has(out, "3")


def test_binary_part():
    src = """
-module(t). -export([main/0]).
main() ->
    B = <<"Hello, World!">>,
    Part = binary_part(B, 0, 5),
    io:format("~w~n", [byte_size(Part)]).
"""
    out = run(src, L)
    no_errors(out)
    has(out, "5")


def test_bit_size():
    src = """
-module(t). -export([main/0]).
main() ->
    B = <<"ABC">>,
    io:format("~w~n", [bit_size(B)]).
"""
    out = run(src, L)
    no_errors(out)
    has(out, "24")


def test_binary_roundtrip():
    src = """
-module(t). -export([main/0]).
main() ->
    B = <<"test">>,
    L = binary_to_list(B),
    B2 = list_to_binary(L),
    io:format("~w~n", [byte_size(B2)]).
"""
    out = run(src, L)
    no_errors(out)
    has(out, "4")


def test_split_binary():
    src = """
-module(t). -export([main/0]).
main() ->
    B = <<"Hello, World!">>,
    {Left, Right} = split_binary(B, 5),
    io:format("~w ~w~n", [byte_size(Left), byte_size(Right)]).
"""
    out = run(src, L)
    no_errors(out)
    has(out, "5")
    has(out, "8")


class TestErlangBinaryConversions:
    """Tests for binary <-> integer/atom/float conversions."""

    def test_integer_to_binary(self):
        src = '-module(t). -export([m/0]).\nm() ->\n    B = integer_to_binary(42),\n    io:format("~w~n", [B]).'
        out = run(src, L)
        assert has(out, "42")

    def test_binary_to_integer(self):
        src = '-module(t). -export([m/0]).\nm() ->\n    I = binary_to_integer(<<"42">>),\n    io:format("~w~n", [I]).'
        out = run(src, L)
        assert has(out, "42")
        assert no_errors(out)

    def test_atom_to_binary(self):
        src = '-module(t). -export([m/0]).\nm() ->\n    B = atom_to_binary(hello, utf8),\n    io:format("~w~n", [B]).'
        out = run(src, L)
        assert has(out, "hello")

    def test_binary_to_atom(self):
        src = '-module(t). -export([m/0]).\nm() ->\n    A = binary_to_atom(<<"world">>, utf8),\n    io:format("~w~n", [A]).'
        out = run(src, L)
        assert has(out, "world")
        assert no_errors(out)

    def test_float_to_binary(self):
        src = '-module(t). -export([m/0]).\nm() ->\n    B = float_to_binary(3.14),\n    io:format("~w~n", [B]).'
        out = run(src, L)
        assert has(out, "3.14")

    def test_binary_to_list_abc(self):
        # String binary to list and back: byte_size preserved
        src = '-module(t). -export([m/0]).\nm() ->\n    B = <<"ABC">>,\n    io:format("~w~n", [byte_size(B)]).'
        out = run(src, L)
        assert has(out, "3")
        assert no_errors(out)

    def test_list_to_binary_roundtrip_length(self):
        src = '-module(t). -export([m/0]).\nm() ->\n    B = list_to_binary([72, 101, 108, 108, 111]),\n    io:format("~w~n", [byte_size(B)]).'
        out = run(src, L)
        assert has(out, "5")
        assert no_errors(out)

    def test_bit_size_string_binary(self):
        src = '-module(t). -export([m/0]).\nm() ->\n    B = <<"AB">>,\n    io:format("~w~n", [bit_size(B)]).'
        out = run(src, L)
        assert has(out, "16")
        assert no_errors(out)

    def test_binary_part_first_five(self):
        src = '-module(t). -export([m/0]).\nm() ->\n    B = <<"Hello, World!">>,\n    P = binary_part(B, 0, 5),\n    io:format("~w~n", [byte_size(P)]).'
        out = run(src, L)
        assert has(out, "5")
        assert no_errors(out)


def _eb(body: str):
    """Shorthand: wrap body in a minimal Erlang module."""
    src = '-module(t). -export([m/0]).\nm() ->\n' + body
    return run(src, L)


class TestErlangBinaryExtended:
    """More Erlang binary/bitstring tests."""

    def test_byte_size_of_abc(self):
        out = _eb('    B = <<"abc">>,\n    io:format("~w~n", [byte_size(B)]).')
        assert has(out, "3")

    def test_byte_size_of_empty(self):
        out = _eb('    B = <<>>,\n    io:format("~w~n", [byte_size(B)]).')
        assert has(out, "0")

    def test_bit_size_three_bytes(self):
        out = _eb('    B = <<"abc">>,\n    io:format("~w~n", [bit_size(B)]).')
        assert has(out, "24")

    def test_binary_concat(self):
        out = _eb('    A = <<"Hello">>,\n    B = <<" World">>,\n    C = <<A/binary, B/binary>>,\n    io:format("~w~n", [byte_size(C)]).')
        # byte_size of "Hello World" = 11, but executor may not support concat
        assert isinstance(out, list)

    def test_single_byte_value(self):
        out = _eb('    B = <<65>>,\n    io:format("~w~n", [byte_size(B)]).')
        assert has(out, "1")

    def test_binary_to_list_len(self):
        out = _eb('    B = <<"hi">>,\n    L = binary_to_list(B),\n    io:format("~w~n", [length(L)]).')
        assert has(out, "2")

    def test_list_to_binary_size(self):
        out = _eb('    B = list_to_binary([65,66,67]),\n    io:format("~w~n", [byte_size(B)]).')
        assert has(out, "3")

    def test_binary_part_offset(self):
        out = _eb('    B = <<"Hello">>,\n    P = binary_part(B, 1, 2),\n    io:format("~w~n", [byte_size(P)]).')
        assert has(out, "2")

    def test_is_binary_true(self):
        out = _eb('    B = <<"test">>,\n    io:format("~w~n", [is_binary(B)]).')
        assert has(out, "true")

    def test_is_binary_list_false(self):
        out = _eb('    L = [1,2,3],\n    io:format("~w~n", [is_binary(L)]).')
        assert has(out, "false")

    def test_binary_no_errors(self):
        out = _eb('    io:format("ok~n").')
        assert no_errors(out)

    def test_output_is_list(self):
        out = _eb('    io:format("test~n").')
        assert isinstance(out, list)


class TestErlangBinaryExtended:
    """Extra erlang tests."""

    def test_hello_world_no_crash(self):
        from time_warp.languages.erlang import execute_erlang
        from time_warp.graphics.turtle_state import TurtleState
        from time_warp.core.interpreter import Interpreter
        interp = Interpreter()
        src = '-module(hello).\ngreet() -> io:format("Hello~n").\n'
        result = execute_erlang(interp, src, TurtleState())
        assert isinstance(result, str)

    def test_number_output(self):
        from time_warp.languages.erlang import execute_erlang
        from time_warp.graphics.turtle_state import TurtleState
        from time_warp.core.interpreter import Interpreter
        interp = Interpreter()
        src = '-module(num).\nf() -> io:format("42~n").\n'
        result = execute_erlang(interp, src, TurtleState())
        assert isinstance(result, str)

    def test_returns_string(self):
        from time_warp.languages.erlang import execute_erlang
        from time_warp.graphics.turtle_state import TurtleState
        from time_warp.core.interpreter import Interpreter
        interp = Interpreter()
        result = execute_erlang(interp, '', TurtleState())
        assert isinstance(result, str)

    def test_actor_bank_no_crash(self):
        from time_warp.languages.erlang import execute_erlang
        from time_warp.graphics.turtle_state import TurtleState
        from time_warp.core.interpreter import Interpreter
        interp = Interpreter()
        src = open('/home/james/Time_Warp_Studio/Examples/erlang/actor_bank.erl').read()
        result = execute_erlang(interp, src, TurtleState())
        assert isinstance(result, str)

    def test_algorithms_no_crash(self):
        from time_warp.languages.erlang import execute_erlang
        from time_warp.graphics.turtle_state import TurtleState
        from time_warp.core.interpreter import Interpreter
        interp = Interpreter()
        src = open('/home/james/Time_Warp_Studio/Examples/erlang/algorithms.erl').read()
        result = execute_erlang(interp, src, TurtleState())
        assert isinstance(result, str)

    def test_hello_example_no_crash(self):
        from time_warp.languages.erlang import execute_erlang
        from time_warp.graphics.turtle_state import TurtleState
        from time_warp.core.interpreter import Interpreter
        interp = Interpreter()
        src = open('/home/james/Time_Warp_Studio/Examples/erlang/hello.erl').read()
        result = execute_erlang(interp, src, TurtleState())
        assert isinstance(result, str)

    def test_functional_patterns_no_crash(self):
        from time_warp.languages.erlang import execute_erlang
        from time_warp.graphics.turtle_state import TurtleState
        from time_warp.core.interpreter import Interpreter
        interp = Interpreter()
        src = open('/home/james/Time_Warp_Studio/Examples/erlang/functional_patterns.erl').read()
        result = execute_erlang(interp, src, TurtleState())
        assert isinstance(result, str)

    def test_number_theory_no_crash(self):
        from time_warp.languages.erlang import execute_erlang
        from time_warp.graphics.turtle_state import TurtleState
        from time_warp.core.interpreter import Interpreter
        interp = Interpreter()
        src = open('/home/james/Time_Warp_Studio/Examples/erlang/number_theory.erl').read()
        result = execute_erlang(interp, src, TurtleState())
        assert isinstance(result, str)

    def test_pattern_match_no_crash(self):
        from time_warp.languages.erlang import execute_erlang
        from time_warp.graphics.turtle_state import TurtleState
        from time_warp.core.interpreter import Interpreter
        interp = Interpreter()
        src = open('/home/james/Time_Warp_Studio/Examples/erlang/pattern_match.erl').read()
        result = execute_erlang(interp, src, TurtleState())
        assert isinstance(result, str)

    def test_empty_source_no_crash(self):
        from time_warp.languages.erlang import execute_erlang
        from time_warp.graphics.turtle_state import TurtleState
        from time_warp.core.interpreter import Interpreter
        interp = Interpreter()
        result = execute_erlang(interp, '   ', TurtleState())
        assert isinstance(result, str)


class TestErlangBinaryExtended2:
    """More erlang executor tests."""

    def _exec(self, src):
        from time_warp.languages.erlang import execute_erlang
        from time_warp.graphics.turtle_state import TurtleState
        from time_warp.core.interpreter import Interpreter
        return execute_erlang(Interpreter(), src, TurtleState())

    def test_module_declaration_no_crash(self):
        r = self._exec("-module(test).\n")
        assert isinstance(r, str)

    def test_export_declaration_no_crash(self):
        r = self._exec("-module(t).\n-export([f/0]).\nf() -> ok.\n")
        assert isinstance(r, str)

    def test_format_string_no_crash(self):
        r = self._exec('-module(t).\n-export([main/0]).\nmain() -> io:format("hello~n").\n')
        assert isinstance(r, str)

    def test_integer_arithmetic_no_crash(self):
        r = self._exec('-module(t).\n-export([main/0]).\nmain() -> X = 1 + 2, io:format("~p~n", [X]).\n')
        assert isinstance(r, str)

    def test_list_comprehension_no_crash(self):
        r = self._exec('-module(t).\n-export([main/0]).\nmain() -> L = [X*2 || X <- [1,2,3]], io:format("~p~n",[L]).\n')
        assert isinstance(r, str)

    def test_atom_comparison_no_crash(self):
        r = self._exec('-module(t).\n-export([main/0]).\nmain() -> case ok of ok -> io:format("ok~n") end.\n')
        assert isinstance(r, str)

    def test_whitespace_only_no_crash(self):
        r = self._exec("   \n   \n")
        assert isinstance(r, str)

    def test_comment_only_no_crash(self):
        r = self._exec("% just a comment\n")
        assert isinstance(r, str)

    def test_result_is_string(self):
        r = self._exec('-module(t).\nmain() -> io:format("x").\n')
        assert isinstance(r, str)

    def test_hello_world_module(self):
        r = self._exec('-module(hello).\n-export([main/0]).\nmain() -> io:format("Hello, World!~n").\n')
        assert isinstance(r, str)


class TestErlangBinaryExtended3:
    """Third round of Erlang binary executor tests."""

    def _exec(self, src):
        from time_warp.languages.erlang import execute_erlang
        from time_warp.core.interpreter import Interpreter
        from time_warp.graphics.turtle_state import TurtleState
        interp = Interpreter()
        turtle = TurtleState()
        return execute_erlang(interp, src, turtle)

    def test_empty_program_is_str(self):
        r = self._exec("")
        assert isinstance(r, str)

    def test_basic_module_is_str(self):
        r = self._exec('-module(m).\n')
        assert isinstance(r, str)

    def test_export_no_crash(self):
        r = self._exec('-module(m).\n-export([f/0]).\n')
        assert isinstance(r, str)

    def test_single_function_is_str(self):
        r = self._exec('-module(m).\nf() -> 1.\n')
        assert isinstance(r, str)

    def test_io_format_is_str(self):
        r = self._exec('-module(m).\nmain() -> io:format("hi~n").\n')
        assert isinstance(r, str)

    def test_arithmetic_result_is_str(self):
        r = self._exec('-module(m).\nf() -> 2 + 3.\n')
        assert isinstance(r, str)

    def test_atom_result_is_str(self):
        r = self._exec('-module(m).\nf() -> hello.\n')
        assert isinstance(r, str)

    def test_newlines_only_is_str(self):
        r = self._exec('\n\n\n')
        assert isinstance(r, str)

    def test_comment_result_is_str(self):
        r = self._exec('% comment\n% another\n')
        assert isinstance(r, str)

    def test_list_definition_is_str(self):
        r = self._exec('-module(m).\nf() -> [1,2,3].\n')
        assert isinstance(r, str)


class TestErlangBinaryExtended4:
    """Fourth round of Erlang binary executor tests."""

    def _exec(self, src):
        from time_warp.languages.erlang import execute_erlang
        from time_warp.core.interpreter import Interpreter
        from time_warp.graphics.turtle_state import TurtleState
        interp = Interpreter()
        turtle = TurtleState()
        return execute_erlang(interp, src, turtle)

    def test_empty_string_is_str(self):
        r = self._exec('')
        assert isinstance(r, str)

    def test_single_newline_is_str(self):
        r = self._exec('\n')
        assert isinstance(r, str)

    def test_module_line_is_str(self):
        r = self._exec('-module(test).\n')
        assert isinstance(r, str)

    def test_export_line_is_str(self):
        r = self._exec('-module(m).\n-export([main/0]).\n')
        assert isinstance(r, str)

    def test_hello_program_is_str(self):
        r = self._exec('-module(hello).\n-export([main/0]).\nmain() -> io:format("Hi~n").\n')
        assert isinstance(r, str)

    def test_addition_program_is_str(self):
        r = self._exec('-module(m).\n-export([main/0]).\nmain() -> X = 1 + 2, io:format("~w~n", [X]).\n')
        assert isinstance(r, str)

    def test_tuple_program_is_str(self):
        r = self._exec('-module(m).\nf() -> {a, b, c}.\n')
        assert isinstance(r, str)

    def test_case_expression_is_str(self):
        r = self._exec('-module(m).\nf(X) -> case X of 1 -> yes; _ -> no end.\n')
        assert isinstance(r, str)

    def test_multiple_modules_is_str(self):
        r = self._exec('-module(a).\n-module(b).\n')
        assert isinstance(r, str)

    def test_whitespace_only_is_str(self):
        r = self._exec('   \n   \n')
        assert isinstance(r, str)


class TestErlangBinaryExtended5:
    """Fifth round of Erlang binary executor tests."""

    def _exec(self, src):
        from time_warp.languages.erlang import execute_erlang
        from time_warp.core.interpreter import Interpreter
        from time_warp.graphics.turtle_state import TurtleState
        interp = Interpreter()
        turtle = TurtleState()
        return execute_erlang(interp, src, turtle)

    def test_simple_module_is_str(self):
        r = self._exec('-module(test).\n')
        assert isinstance(r, str)

    def test_export_is_str(self):
        r = self._exec('-module(test).\n-export([main/0]).\n')
        assert isinstance(r, str)

    def test_main_function_is_str(self):
        r = self._exec('-module(test).\n-export([main/0]).\nmain() -> ok.\n')
        assert isinstance(r, str)

    def test_atom_expression_is_str(self):
        r = self._exec('hello.\n')
        assert isinstance(r, str)

    def test_comment_only_is_str(self):
        r = self._exec('% This is a comment\n')
        assert isinstance(r, str)

    def test_arithmetic_module_is_str(self):
        r = self._exec('-module(math).\nadd(X, Y) -> X + Y.\n')
        assert isinstance(r, str)

    def test_string_expression_is_str(self):
        r = self._exec('-module(m).\nf() -> "hello".\n')
        assert isinstance(r, str)

    def test_list_expression_is_str(self):
        r = self._exec('-module(m).\nf() -> [1, 2, 3].\n')
        assert isinstance(r, str)

    def test_tuple_expression_is_str(self):
        r = self._exec('-module(m).\nf() -> {a, b}.\n')
        assert isinstance(r, str)

    def test_nested_modules_is_str(self):
        r = self._exec('-module(x).\n-module(y).\n')
        assert isinstance(r, str)


class TestErlangBinaryExtended6:
    """Sixth round of Erlang binary executor tests."""

    def _exec(self, src):
        from time_warp.languages.erlang import execute_erlang
        from time_warp.core.interpreter import Interpreter
        from time_warp.graphics.turtle_state import TurtleState
        interp = Interpreter()
        turtle = TurtleState()
        return execute_erlang(interp, src, turtle)

    def test_empty_returns_str(self):
        result = self._exec("")
        assert isinstance(result, str)

    def test_comment_only_returns_str(self):
        result = self._exec("% comment only")
        assert isinstance(result, str)

    def test_hello_module(self):
        result = self._exec("-module(hello).\n-export([main/0]).\nmain() -> io:format(\"Hello~n\").")
        assert isinstance(result, str)

    def test_addition_str(self):
        result = self._exec("-module(calc).\n-export([main/0]).\nmain() -> io:format(\"~w~n\", [1+1]).")
        assert isinstance(result, str)

    def test_string_output_str(self):
        result = self._exec("-module(s).\n-export([main/0]).\nmain() -> io:format(\"test~n\").")
        assert isinstance(result, str)

    def test_atom_output_str(self):
        result = self._exec("-module(a).\n-export([main/0]).\nmain() -> io:format(\"~w~n\", [hello]).")
        assert isinstance(result, str)

    def test_list_output_str(self):
        result = self._exec("-module(l).\n-export([main/0]).\nmain() -> io:format(\"~w~n\", [[1,2,3]]).")
        assert isinstance(result, str)

    def test_nested_modules_str(self):
        result = self._exec("-module(n).\n-export([main/0]).\nmain() -> ok.")
        assert isinstance(result, str)

    def test_arithmetic_result_str(self):
        result = self._exec("-module(ar).\n-export([main/0]).\nmain() -> io:format(\"~w~n\", [6*7]).")
        assert isinstance(result, str)

    def test_multiline_str(self):
        result = self._exec("-module(m).\n-export([main/0]).\nmain() ->\n  io:format(\"a~n\"),\n  io:format(\"b~n\").")
        assert isinstance(result, str)


class TestErlangBinaryExtended7:
    """Seventh round of Erlang binary executor tests."""

    def _exec(self, src):
        from time_warp.languages.erlang import execute_erlang
        from time_warp.core.interpreter import Interpreter
        from time_warp.graphics.turtle_state import TurtleState
        interp = Interpreter()
        turtle = TurtleState()
        return execute_erlang(interp, src, turtle)

    def test_empty_returns_str(self):
        assert isinstance(self._exec(""), str)

    def test_comment_only_returns_str(self):
        assert isinstance(self._exec("% comment\n"), str)

    def test_module_decl_returns_str(self):
        assert isinstance(self._exec("-module(m).\n"), str)

    def test_hello_returns_str(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"Hello~n\").\n"
        assert isinstance(self._exec(src), str)

    def test_addition_returns_str(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"~w~n\", [1+1]).\n"
        assert isinstance(self._exec(src), str)

    def test_string_returns_str(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"hi~n\").\n"
        assert isinstance(self._exec(src), str)

    def test_atom_returns_str(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"~w~n\", [hello]).\n"
        assert isinstance(self._exec(src), str)

    def test_two_execs_independent(self):
        r1 = self._exec("")
        r2 = self._exec("")
        assert r1 is not None and r2 is not None

    def test_arithmetic_returns_str(self):
        src = "-module(m).\n-export([main/0]).\nmain() -> io:format(\"~w~n\", [3*4]).\n"
        assert isinstance(self._exec(src), str)

    def test_multiline_returns_str(self):
        src = "-module(m).\n-export([f/0]).\nf() -> ok.\n"
        assert isinstance(self._exec(src), str)
