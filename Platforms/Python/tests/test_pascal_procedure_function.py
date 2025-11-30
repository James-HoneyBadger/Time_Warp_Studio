"""Tests for simple Pascal procedures and functions.

These tests run the in-tree interpreter; to keep the module importable in
editor and CI environments we add the project's package path to sys.path
so `time_warp` imports resolve to the local repository code.
"""

import pathlib
import sys

# Ensure project package directory is importable by pytest
sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[1]))

# These imports intentionally come after manipulating sys.path so tests can
# import the local package in-tree. Keep this file readable and stable across
# environments.
# pylint: disable=wrong-import-position
from time_warp.core.interpreter import Interpreter, Language  # noqa: E402
from time_warp.graphics.turtle_state import TurtleState  # noqa: E402


def run(code: str) -> str:
    """Helper to run a short Pascal program and return interpreter output.

    The helper keeps tests concise by handling interpreter setup and
    execution so each test only supplies the Pascal program text.
    """
    interp = Interpreter()
    interp.load_program(code, Language.PASCAL)
    out = interp.execute(TurtleState())
    return "".join(out)


def test_pascal_procedure_call():
    """Verify that a simple procedure can be called multiple times.

    The procedure writes "Hi" and is invoked twice in the program; the
    output should contain two lines with "Hi".
    """
    code = """
    program demo;
    procedure Hello;
    begin
      writeln('Hi');
    end;
    begin
      Hello;
      Hello;
    end.
    """
    assert run(code) == "Hi\nHi\n"


def test_pascal_function_minimal():
    """Verify a minimal Pascal function that returns a value.

    The function F returns an integer and writing F should print the value.
    """
    code = """
    program demo;
    function F: integer;
    begin
      F := 3;
    end;
    begin
      F;  { call function to compute }
      writeln(F);
    end.
    """
    assert run(code) == "3\n"
