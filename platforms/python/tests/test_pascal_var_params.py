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


def test_procedure_var_param():
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
