"""Tests for nested Pascal procedures and variable scoping.

These tests run inside the repository, so the in-tree package path is
added at runtime to enable importing the package during collection.
"""

import os
import sys

# Make the in-repo package importable at test collection time.
sys.path.append(os.path.join(os.getcwd(), "platforms/python"))

# The imports below must stay after the sys.path append; silence the
# warning about import position accordingly.
# pylint: disable=wrong-import-position
# Silence import errors from static analyzers for in-tree package imports.
# pylint: disable=import-error
from time_warp.core.interpreter import Interpreter, Language  # noqa: E402
from time_warp.graphics.turtle_state import TurtleState  # noqa: E402


def run(code: str) -> str:
    """Load Pascal source and execute it, returning combined output."""
    interp = Interpreter()
    interp.load_program(code, Language.PASCAL)
    out = interp.execute(TurtleState())
    return "".join(out)


def test_nested_procedure_modifies_outer_var():
    """Nested procedure should mutate an outer-scope variable."""
    code = """
    program P;
    var a: integer;
    procedure outer;
    begin
      procedure inner;
      begin
        a := a + 1;
      end;
      inner;
    end;
    begin
      a := 1;
      outer;
      writeln(a);
    end.
    """
    out = run(code)
    assert out == "2\n"
