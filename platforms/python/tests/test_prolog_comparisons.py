import sys
import os

sys.path.append(os.path.join(os.getcwd(), "platforms/python"))

from time_warp.core.interpreter import Interpreter, Language
from time_warp.graphics.turtle_state import TurtleState


def run(code: str) -> str:
    interp = Interpreter()
    interp.load_program(code, Language.PROLOG)
    out = interp.execute(TurtleState())
    return "".join(out).strip()


def test_eq_with_variable_binding():
    code = """
    ?- eq(X,3).
    """
    out = run(code).splitlines()
    assert out == ["✅ X=3"]


def test_eq_ground_terms_true():
    code = """
    ?- eq(4,4).
    """
    out = run(code)
    assert out == "✅ true"


def test_eq_ground_terms_false():
    code = """
    ?- eq(4,5).
    """
    out = run(code)
    assert out == "❌ false"


def test_neq_true():
    code = """
    ?- neq(4,5).
    """
    out = run(code)
    assert out == "✅ true"


def test_neq_false():
    code = """
    ?- neq(4,4).
    """
    out = run(code)
    assert out == "❌ false"


def test_gt_true():
    code = """
    ?- gt(5,3).
    """
    out = run(code)
    assert out == "✅ true"


def test_gt_false():
    code = """
    ?- gt(2,3).
    """
    out = run(code)
    assert out == "❌ false"


def test_ge_true_equal():
    code = """
    ?- ge(3,3).
    """
    out = run(code)
    assert out == "✅ true"


def test_ge_true_greater():
    code = """
    ?- ge(4,3).
    """
    out = run(code)
    assert out == "✅ true"


def test_ge_false():
    code = """
    ?- ge(2,3).
    """
    out = run(code)
    assert out == "❌ false"


def test_le_true_equal():
    code = """
    ?- le(3,3).
    """
    out = run(code)
    assert out == "✅ true"


def test_le_true_less():
    code = """
    ?- le(2,3).
    """
    out = run(code)
    assert out == "✅ true"


def test_le_false():
    code = """
    ?- le(4,3).
    """
    out = run(code)
    assert out == "❌ false"
