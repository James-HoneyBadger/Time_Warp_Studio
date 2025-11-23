import sys
import os

sys.path.append(os.path.join(os.getcwd(), "platforms/python"))

from time_warp.core.interpreter import Interpreter, Language
from time_warp.graphics.turtle_state import TurtleState
import pytest


def run(code: str) -> str:
    interp = Interpreter()
    interp.load_program(code, Language.PROLOG)
    out = interp.execute(TurtleState())
    return "".join(out)


@pytest.mark.skip("Cut semantics still under refinement; backtracking differences TBD")
def test_cut_prunes_alternatives():
    code = """
    % Facts with two alternatives
    p(1).
    p(2).
    q(1).
    q(2).

    % choose_first selects the first p(X), commits with cut, then requires q(X)
    choose_first(X) :- p(X), !, q(X).

    % Query should return only the first solution X=1 and not backtrack to X=2
    ?- choose_first(X).
    """
    out = run(code).strip().splitlines()
    # Expect only one solution with X=1
    assert out == ["✅ X=1"]


@pytest.mark.skip("Cut semantics still under refinement; backtracking differences TBD")
def test_cut_can_fail_without_backtracking():
    code = """
    p(1).
    p(2).
    q(2).

    % First p(1) is chosen, cut commits; q(1) fails -> whole query fails (no backtrack to p(2))
    r(X) :- p(X), !, q(X).

    ?- r(X).
    """
    out = run(code).strip()
    assert out == "❌ false"
