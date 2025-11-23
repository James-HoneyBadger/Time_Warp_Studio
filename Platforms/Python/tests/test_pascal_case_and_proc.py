import sys
import os

# Add platform path
sys.path.append(os.path.join(os.getcwd(), "platforms/python"))

from time_warp.core.interpreter import Interpreter, Language
from time_warp.graphics.turtle_state import TurtleState


def run(code: str) -> str:
    interp = Interpreter()
    interp.load_program(code, Language.PASCAL)
    out = interp.execute(TurtleState())
    return "".join(out)


def test_pascal_case_of_basic():
    code = """
    program demo;
    var x: integer;
    begin
      x := 2;
      case x of
        1:
        begin
          writeln('A');
        end;
        2:
        begin
          writeln('B');
        end;
      else:
        begin
          writeln('Z');
        end;
      end
    end.
    """
    assert run(code) == "B\n"


def test_pascal_case_single_line_branch():
    code = """
    program demo;
    var x: integer;
    begin
      x := 1;
      case x of
        1:
          writeln('A');
        2:
          writeln('B');
      else:
          writeln('Z');
      end
    end.
    """
    assert run(code) == "A\n"
