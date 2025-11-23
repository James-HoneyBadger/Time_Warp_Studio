import sys
import os

sys.path.append(os.path.join(os.getcwd(), "platforms/python"))

from time_warp.core.interpreter import Interpreter, Language
from time_warp.graphics.turtle_state import TurtleState


def run(code: str) -> str:
    interp = Interpreter()
    interp.load_program(code, Language.PROLOG)
    out = interp.execute(TurtleState())
    return "".join(out)


def test_prolog_conjunction():
    code = """
    parent(a,b).
    parent(b,c).
    ancestor(X,Z) :- parent(X,Y), parent(Y,Z).
    ?- ancestor(a,c).
    """
    assert run(code).strip() == "✅ true"


def test_prolog_add_builtin():
    code = """
    sum(X,Y,Z) :- add(X,Y,Z).
    ?- sum(2,3,Z).
    """
    out = run(code).strip().splitlines()
    assert any(line.startswith("✅ Z=") for line in out)
    assert any(line.strip() == "✅ Z=5" for line in out)
