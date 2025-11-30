"""Small Prolog enhancement tests.

These tests exercise Prolog builtins and rules. The test helper adjusts
sys.path so the in-tree package can be imported for verification.
"""

import pathlib
import sys

# Ensure project package directory is importable by pytest
sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[1]))

# Imports intentionally come after the sys.path insertion.
# pylint: disable=wrong-import-position
from time_warp.core.interpreter import Interpreter, Language  # noqa: E402
from time_warp.graphics.turtle_state import TurtleState  # noqa: E402


def run(code: str) -> str:
    """Execute the provided Prolog code and return the raw output string."""
    interp = Interpreter()
    interp.load_program(code, Language.PROLOG)
    out = interp.execute(TurtleState())
    return "".join(out)


def test_prolog_conjunction():
    """Verify a simple conjunction and ancestor rule produces true."""
    code = """
    parent(a,b).
    parent(b,c).
    ancestor(X,Z) :- parent(X,Y), parent(Y,Z).
    ?- ancestor(a,c).
    """
    assert run(code).strip() == "✅ true"


def test_prolog_add_builtin():
    """Check the 'add' builtin integrates correctly into user rules."""
    code = """
    sum(X,Y,Z) :- add(X,Y,Z).
    ?- sum(2,3,Z).
    """
    out = run(code).strip().splitlines()
    assert any(line.startswith("✅ Z=") for line in out)
    assert any(line.strip() == "✅ Z=5" for line in out)
