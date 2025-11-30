"""Tests for Pascal variable and parameter handling.

This module is executed in-tree (imports time_warp from the repo). To allow
importing the local package in various editor/CI setups we adjust sys.path so
the package directory is available for imports.
"""

import pathlib
import sys

# Ensure project package directory is importable by pytest
sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[1]))

# These tests import the in-tree package for verification; placement of
# these imports after a sys.path hack is intentional and safe for tests.
# Module intentionally manipulates sys.path so imports sit after that code.
# Disable the wrong-import-position rule for this test module.
# pylint: disable=wrong-import-position
from time_warp.core.interpreter import Interpreter, Language  # noqa: E402
from time_warp.graphics.turtle_state import TurtleState  # noqa: E402


def run(code: str) -> str:
    """Run a short Pascal program through the interpreter and return output.

    The helper wraps Interpreter creation and execution to keep tests concise.
    """
    interp = Interpreter()
    interp.load_program(code, Language.PASCAL)
    out = interp.execute(TurtleState())
    return "".join(out)


def test_procedure_var_param():
    """Verify a procedure receives a var parameter (by reference) correctly.

    This test checks that a procedure modifying a var parameter updates the
    caller's variable.
    """
    code = """
    program P;
    var a: integer;
    procedure inc(var x: integer);
    begin
      x := x + 1;
    end;
    begin
      a := 1;
      inc(a);
      writeln(a);
    end.
    """
    out = run(code)
    assert out == "2\n"
