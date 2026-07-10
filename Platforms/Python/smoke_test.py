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
        "lua",
        "brainfuck",
        "javascript",
        "hypertalk",
        "erlang",
        "lisp",
        "cobol",
        "tcl",
        "postscript",
        "ruby",
        "python_lang",
        "haskell",
        "asm6502",
        "perl",
        "rexx",
        "smalltalk",
        "apl",
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


def test_lua_execution() -> None:
    from time_warp.core.interpreter import Interpreter, Language
    from time_warp.graphics.turtle_state import TurtleState

    interp = Interpreter(Language.LUA)
    turtle = TurtleState()
    interp.load_program(
        'print("Hello from Lua")\n'
        "for i = 1, 3 do\n"
        "  print(\"Count: \" .. i)\n"
        "end\n",
        Language.LUA,
    )
    out = interp.execute(turtle)
    text = "\n".join(out)
    assert "Hello from Lua" in text
    assert "Count: 1" in text
    assert "Count: 3" in text


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


def test_javascript_execution() -> None:
    from time_warp.core.interpreter import Interpreter, Language
    from time_warp.graphics.turtle_state import TurtleState

    interp = Interpreter(Language.JAVASCRIPT)
    turtle = TurtleState()
    interp.load_program(
        'console.log("Hello from JS");\n'
        "for (var i = 1; i <= 3; i++) {\n"
        '  console.log("Count: " + String(i));\n'
        "}\n",
        Language.JAVASCRIPT,
    )
    out = interp.execute(turtle)
    text = "\n".join(out)
    assert "Hello from JS" in text
    assert "Count: 1" in text
    assert "Count: 3" in text


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


def test_smalltalk_execution() -> None:
    from time_warp.core.interpreter import Interpreter, Language
    from time_warp.graphics.turtle_state import TurtleState

    interp = Interpreter(Language.SMALLTALK)
    turtle = TurtleState()
    interp.load_program(
        "Transcript show: 'Hello from Smalltalk'; cr.\n",
        Language.SMALLTALK,
    )
    out = interp.execute(turtle)
    text = "\n".join(out)
    assert "Hello from Smalltalk" in text
    assert "❌" not in text


def test_postscript_execution() -> None:
    from time_warp.core.interpreter import Interpreter, Language
    from time_warp.graphics.turtle_state import TurtleState

    interp = Interpreter(Language.POSTSCRIPT)
    turtle = TurtleState()
    interp.load_program(
        "/Times-Roman findfont 12 scalefont setfont\n"
        "100 100 moveto\n"
        "(Hello from PostScript) show\n"
        "showpage\n",
        Language.POSTSCRIPT,
    )
    out = interp.execute(turtle)
    text = "\n".join(out)
    assert "Hello from PostScript" in text
    assert "❌" not in text


def main() -> int:
    tests = [
        test_imports,
        test_inventory,
        test_basic_execution,
        test_lua_execution,
        test_logo_turtle,
        test_javascript_execution,
        test_python_execution,
        test_brainfuck_execution,
        test_smalltalk_execution,
        test_postscript_execution,
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
