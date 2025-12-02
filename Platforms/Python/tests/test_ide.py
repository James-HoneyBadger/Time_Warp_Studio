#!/usr/bin/env python3
"""
IDE integration smoke test.

Verifies imports, interpreter execution across all languages, theme system,
and example file discovery. Prints diagnostic output for manual verification.
"""

from pathlib import Path

print("🔍 Testing Time Warp IDE components...")

# 1. UI imports
print("1. Testing imports...")
try:
    # pylint: disable=import-error,no-name-in-module
    import time_warp.ui as _tw_ui  # noqa: F401

    print("   ✅ UI components imported")
except Exception as e:
    raise AssertionError(f"UI import failed: {e}") from e

# 2. Core library
print("2. Testing core library...")
try:
    # pylint: disable=import-error,no-name-in-module
    from time_warp.core.interpreter import Interpreter, Language
    from time_warp.graphics.turtle_state import TurtleState

    print("   ✅ Core library imported")
except Exception as e:
    raise AssertionError(f"Core import failed: {e}") from e

# 3. PySide6
print("3. Testing PySide6...")
try:
    import PySide6  # type: ignore[import]

    print(f"   ✅ PySide6 {PySide6.__version__}")
except Exception as e:
    raise AssertionError(f"PySide6 check failed: {e}") from e

# 4. Language execution
print("4. Testing interpreters...")
try:
    interp = Interpreter()
    turtle = TurtleState()

    interp.load_program("T:Hello from PILOT\nE:", Language.PILOT)
    output = interp.execute(turtle)
    assert any("Hello from PILOT" in line for line in output)

    interp.load_program('10 PRINT "Hello from BASIC"', Language.BASIC)
    output = interp.execute(turtle)
    assert any("Hello from BASIC" in line for line in output)

    turtle = TurtleState()
    interp.load_program("FORWARD 50\nRIGHT 90\nFORWARD 50", Language.LOGO)
    interp.execute(turtle)
    assert len(turtle.lines) >= 1

    print("   ✅ All languages execute correctly")
except Exception as e:
    import traceback

    traceback.print_exc()
    raise AssertionError(f"Execution failed: {e}") from e

# 5. Examples
print("5. Checking examples...")
examples_dir = Path(__file__).parent / "examples"
if examples_dir.exists():
    pilot = list(examples_dir.glob("*.pilot"))
    basic = list(examples_dir.glob("*.bas"))
    logo = list(examples_dir.glob("*.logo"))
    print(f"   ✅ {len(pilot) + len(basic) + len(logo)} example programs")
else:
    print(f"   ⚠️  Examples not found at {examples_dir}")

# 6. Themes
print("6. Testing themes...")
try:
    tm = _tw_ui.ThemeManager()
    themes = tm.get_theme_names()
    assert len(themes) == 14
    print(f"   ✅ {len(themes)} themes available")
except Exception as e:
    raise AssertionError(f"Theme test failed: {e}") from e

print("\n🎉 All checks passed. Launch with: python time_warp_ide.py")
