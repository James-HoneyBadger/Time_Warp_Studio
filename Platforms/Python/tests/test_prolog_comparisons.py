"""Unit tests for Prolog comparison predicates.

These tests run the in-tree interpreter; to make imports reliable across
local editor and CI setups we adjust sys.path so the local package can be
imported.
"""

import pathlib
import sys

# Ensure project package directory is importable by pytest
sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[1]))

# These imports intentionally come after the sys.path insertion so tests can
# import the repository package directly.
# pylint: disable=wrong-import-position
from time_warp.core.interpreter import Interpreter, Language  # noqa: E402
from time_warp.graphics.turtle_state import TurtleState  # noqa: E402


def run(code: str) -> str:
    """Run a small Prolog query/program and return trimmed output lines.

    This helper keeps tests concise when calling into the interpreter.
    """
    interp = Interpreter()
    interp.load_program(code, Language.PROLOG)
    out = interp.execute(TurtleState())
    return "".join(out).strip()


def test_eq_with_variable_binding():
    """Check equality with variable binding (X gets bound to 3)."""
    code = """
    ?- eq(X,3).
    """
    out = run(code).splitlines()
    assert out == ["✅ X=3"]


def test_eq_ground_terms_true():
    """Check equality of two identical ground numeric terms returns true."""
    code = """
    ?- eq(4,4).
    """
    out = run(code)
    assert out == "✅ true"


def test_eq_ground_terms_false():
    """Equality of different ground terms should be false."""
    code = """
    ?- eq(4,5).
    """
    out = run(code)
    assert out == "❌ false"


def test_neq_true():
    """Check inequality predicate returns true for differing args."""
    code = """
    ?- neq(4,5).
    """
    out = run(code)
    assert out == "✅ true"


def test_neq_false():
    """Inequality should be false for identical arguments."""
    code = """
    ?- neq(4,4).
    """
    out = run(code)
    assert out == "❌ false"


def test_gt_true():
    """Greater-than predicate returns true for larger first argument."""
    code = """
    ?- gt(5,3).
    """
    out = run(code)
    assert out == "✅ true"


def test_gt_false():
    """Greater-than predicate returns false when first arg is smaller."""
    code = """
    ?- gt(2,3).
    """
    out = run(code)
    assert out == "❌ false"


def test_ge_true_equal():
    """Greater-or-equal is true when arguments are equal."""
    code = """
    ?- ge(3,3).
    """
    out = run(code)
    assert out == "✅ true"


def test_ge_true_greater():
    """Greater-or-equal is true when first arg is greater."""
    code = """
    ?- ge(4,3).
    """
    out = run(code)
    assert out == "✅ true"


def test_ge_false():
    """Greater-or-equal is false when first arg is smaller."""
    code = """
    ?- ge(2,3).
    """
    out = run(code)
    assert out == "❌ false"


def test_le_true_equal():
    """Less-or-equal returns true when arguments are equal."""
    code = """
    ?- le(3,3).
    """
    out = run(code)
    assert out == "✅ true"


def test_le_true_less():
    """Less-or-equal returns true when first arg is less."""
    code = """
    ?- le(2,3).
    """
    out = run(code)
    assert out == "✅ true"


def test_le_false():
    """Less-or-equal returns false when first arg is greater."""
    code = """
    ?- le(4,3).
    """
    out = run(code)
    assert out == "❌ false"
