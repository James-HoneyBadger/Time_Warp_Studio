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


def test_nested_procedure_modifies_outer_var():
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
