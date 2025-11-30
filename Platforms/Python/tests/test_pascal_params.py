"""Tests for Pascal procedures and functions with parameters/returns.

These tests run in-tree and require the Platforms/Python package on sys.path
during collection so imports are intentionally placed after the sys.path
adjustment.
"""

import os
import sys

# Add in-tree package path so tests import the in-repo package correctly.
sys.path.append(os.path.join(os.getcwd(), "platforms/python"))

# Imports below are intentionally positioned after sys.path setup.
# pylint: disable=wrong-import-position
# Silence import errors from static analyzers for in-tree package imports.
# pylint: disable=import-error
from time_warp.core.interpreter import Interpreter, Language  # noqa: E402
from time_warp.graphics.turtle_state import TurtleState  # noqa: E402


def run(code: str) -> str:
    """Execute Pascal source code and return combined output string."""
    interp = Interpreter()
    interp.load_program(code, Language.PASCAL)
    out = interp.execute(TurtleState())
    return "".join(out)


def test_procedure_params_and_locals():
    """Procedures accept parameters and maintain separate local vars."""
    code = """
    program P;
    var a: integer;
    procedure inc_and_print(x: integer; msg: string);
    begin
      x := x + 1;
      writeln(msg);
    end;
    begin
      a := 1;
      inc_and_print(5, 'ok');
      writeln(a);
    end.
    """
    out = run(code)
    assert out == "ok\n1\n"


def test_function_params_and_return_var():
    """Functions should return values and accept parameters correctly."""
    code = """
    program P;
    function add2(x: integer; y: integer): integer;
    begin
      add2 := x + y;
    end;
    begin
      add2(2,3);
      writeln(add2);
    end.
    """
    out = run(code)
    assert out == "5\n"
