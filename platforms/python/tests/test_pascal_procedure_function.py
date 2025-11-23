import sys
import os

sys.path.append(os.path.join(os.getcwd(), "platforms/python"))

from time_warp.core.interpreter import Interpreter, Language
from time_warp.graphics.turtle_state import TurtleState


def run(code: str) -> str:
    interp = Interpreter()
    interp.load_program(code, Language.PASCAL)
    out = interp.execute(TurtleState())
    return "".join(out)


def test_pascal_procedure_call():
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
