"""Tests around the Prolog CUT (!) operator semantics.

These tests are placed inside the repository's test harness and need the
local package path added so the test runner can import the package.
"""

import os
import sys

import pytest

# Make local package importable (tests live in Platforms/Python)
sys.path.append(os.path.join(os.getcwd(), "Platforms", "Python"))

# Local package imports are intentionally placed after the path tweak above
# pylint: disable=wrong-import-position,import-error
from time_warp.core.interpreter import Interpreter, Language  # noqa: E402
from time_warp.graphics.turtle_state import TurtleState  # noqa: E402


def run(code: str) -> str:
    """Helper to execute a small program text and return captured output."""
    interp = Interpreter()
    interp.load_program(code, Language.PROLOG)
    out = interp.execute(TurtleState())
    return "".join(out)


@pytest.mark.skip("Cut semantics still under refinement; backtracking differences TBD")
def test_cut_prunes_alternatives():
    """Verify cut (!) prevents backtracking to later alternatives."""
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
    """Verify a committed choice can cause the entire query to fail."""
    code = """
    p(1).
    p(2).
    q(2).

    % First p(1) is chosen, cut commits; q(1) fails ->
    % whole query fails (no backtrack to p(2))
    r(X) :- p(X), !, q(X).

    ?- r(X).
    """
    out = run(code).strip()
    assert out == "❌ false"
