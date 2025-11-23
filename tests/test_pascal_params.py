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


def test_procedure_params_and_locals():
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
