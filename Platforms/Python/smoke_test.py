#!/usr/bin/env python3
"""Fast smoke test that runs without pytest.

This script exercises the core interpreter import, language inventory,
and a small BASIC program so CI or local environments without pytest can
still get quick validation.
"""

from __future__ import annotations

import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT))


def test_imports() -> None:
    from time_warp.core.interpreter import Interpreter, Language
    from time_warp.graphics.turtle_state import TurtleState

    assert Interpreter is not None
    assert Language is not None
    assert TurtleState is not None


def test_inventory() -> None:
    import importlib.util

    audit_path = ROOT.parents[1] / "Scripts" / "audit_interpreters.py"
    spec = importlib.util.spec_from_file_location("audit_interpreters", audit_path)
    assert spec is not None and spec.loader is not None
    audit = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = audit
    spec.loader.exec_module(audit)

    inventory = audit.collect_inventory()
    modules = {entry.module for entry in inventory}
    expected = {
        "basic",
        "pilot",
        "logo",
        "c_lang_fixed",
        "prolog",
        "pascal",
        "forth",
        "brainfuck",
        "python_lang",
    }
    assert expected.issubset(modules), f"Missing modules: {expected - modules}"


def test_basic_execution() -> None:
    from time_warp.core.interpreter import Interpreter, Language
    from time_warp.graphics.turtle_state import TurtleState

    interp = Interpreter(Language.BASIC)
    turtle = TurtleState()
    interp.load_program(
        'PRINT "Hello, Time Warp!"\n'
        "FOR I = 1 TO 3\n"
        "  PRINT \"Count: \"; I\n"
        "NEXT I\n",
        Language.BASIC,
    )
    out = interp.execute(turtle)
    text = "\n".join(out)
    assert "Hello, Time Warp!" in text
    assert "Count:  1" in text
    assert "Count:  3" in text


def test_logo_turtle() -> None:
    from time_warp.core.interpreter import Interpreter, Language
    from time_warp.graphics.turtle_state import TurtleState

    interp = Interpreter(Language.LOGO)
    turtle = TurtleState()
    interp.load_program(
        "PENDOWN\n"
        "FORWARD 100\n"
        "RIGHT 90\n"
        "FORWARD 100\n",
        Language.LOGO,
    )
    out = interp.execute(turtle)
    text = "\n".join(out)
    assert "🐢" in text or "turtle" in text.lower() or text == ""
    assert turtle.x != 0 or turtle.y != 0


def test_python_execution() -> None:
    from time_warp.core.interpreter import Interpreter, Language
    from time_warp.graphics.turtle_state import TurtleState

    interp = Interpreter(Language.PYTHON_LANG)
    turtle = TurtleState()
    interp.load_program(
        'print("Hello from Python")\n'
        "for i in range(1, 4):\n"
        '    print(f"Count: {i}")\n',
        Language.PYTHON_LANG,
    )
    out = interp.execute(turtle)
    text = "\n".join(out)
    assert "Hello from Python" in text
    assert "Count: 1" in text
    assert "Count: 3" in text


def test_brainfuck_execution() -> None:
    from time_warp.core.interpreter import Interpreter, Language
    from time_warp.graphics.turtle_state import TurtleState

    interp = Interpreter(Language.BRAINFUCK)
    turtle = TurtleState()
    # Brainfuck program that prints "Hi"
    interp.load_program(
        "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.",
        Language.BRAINFUCK,
    )
    out = interp.execute(turtle)
    text = "\n".join(out)
    assert "Hello World!" in text or "Hi" in text


def main() -> int:
    tests = [
        test_imports,
        test_inventory,
        test_basic_execution,
        test_logo_turtle,
        test_python_execution,
        test_brainfuck_execution,
    ]
    for test in tests:
        try:
            test()
            print(f"PASS  {test.__name__}")
        except Exception as exc:  # noqa: BLE001
            print(f"FAIL  {test.__name__}: {exc}")
            return 1
    print("\nAll smoke tests passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
