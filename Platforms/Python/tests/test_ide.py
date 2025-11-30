#!/usr/bin/env python3
"""Test script to verify Time Warp IDE functionality."""

from pathlib import Path

print("🔍 Testing Time Warp IDE components...")
print()

# Test 1: Import all UI components
print("1. Testing imports...")
try:
    import time_warp.ui as _tw_ui  # noqa: F401  # pylint: disable=import-error

    print("   ✅ All UI components imported successfully")
except Exception as e:
    print(f"   ❌ Import failed: {e}")
    raise AssertionError(f"UI import failed: {e}") from e

# Test 2: Import core library
print("2. Testing core library imports...")
try:
    # pylint: disable=import-error
    from time_warp.core.interpreter import (  # pylint: disable=import-error
        Interpreter,
        Language,
    )

    # pylint: disable=import-error
    from time_warp.graphics.turtle_state import (  # pylint: disable=import-error
        TurtleState,
    )

    print("   ✅ Core library imported successfully")
except Exception as e:
    print(f"   ❌ Core import failed: {e}")
    raise AssertionError(f"Core import failed: {e}") from e

# Test 3: Check PySide6
print("3. Testing PySide6...")
try:
    import PySide6  # type: ignore[import]

    # type: ignore[import]
    # from PySide6.QtWidgets import QApplication

    print(f"   ✅ PySide6 version {PySide6.__version__}")
except Exception as e:
    print(f"   ❌ PySide6 not found: {e}")
    raise AssertionError(f"PySide6 check failed: {e}") from e

# Test 4: Test interpreter execution
print("4. Testing interpreter execution...")
try:
    interp = Interpreter()
    turtle = TurtleState()

    # Test PILOT
    interp.load_program("T:Hello from PILOT\nE:", Language.PILOT)
    output = interp.execute(turtle)
    assert any("Hello from PILOT" in line for line in output)

    # Test BASIC
    interp.load_program('10 PRINT "Hello from BASIC"', Language.BASIC)
    output = interp.execute(turtle)
    assert any("Hello from BASIC" in line for line in output)

    # Test Logo
    turtle = TurtleState()  # Reset turtle
    interp.load_program("FORWARD 50\nRIGHT 90\nFORWARD 50", Language.LOGO)
    output = interp.execute(turtle)
    assert len(turtle.lines) >= 1  # At least one line segment drawn

    print("   ✅ Language execution works correctly")
except Exception as e:
    import traceback

    print(f"   ❌ Execution test failed: {e}")
    traceback.print_exc()
    raise AssertionError(f"Execution test failed: {e}") from e

# Test 5: Check example programs
print("5. Checking example programs...")
examples_dir = Path(__file__).parent / "examples"
if examples_dir.exists():
    pilot_files = list(examples_dir.glob("*.pilot"))
    basic_files = list(examples_dir.glob("*.bas"))
    logo_files = list(examples_dir.glob("*.logo"))

    total = len(pilot_files) + len(basic_files) + len(logo_files)
    print(f"   ✅ Found {total} example programs:")
    print(f"      - {len(basic_files)} BASIC (.bas) programs")
    print(f"      - {len(pilot_files)} PILOT (.pilot) programs")
    print(f"      - {len(logo_files)} Logo (.logo) programs")
else:
    print(f"   ⚠️  Examples directory not found at {examples_dir}")

# Test 6: Test ThemeManager
print("6. Testing ThemeManager...")
try:
    tm = _tw_ui.ThemeManager()
    themes = tm.get_theme_names()
    assert len(themes) == 14, f"Expected 14 themes, found {len(themes)}"
    assert "Dracula" in themes
    assert "Monokai" in themes
    print(f"   ✅ ThemeManager working with {len(themes)} themes")
except Exception as e:
    print(f"   ❌ ThemeManager test failed: {e}")
    raise AssertionError(f"ThemeManager test failed: {e}") from e

# Test 7: Check documentation
print("7. Checking documentation...")
docs = [
    "README.md",
    "DESKTOP_QUICKSTART.md",
    "GUI_IMPLEMENTATION_STATUS.md",
    "PROJECT_COMPLETE.md",
]
doc_path = Path(__file__).parent
found_docs = sum(1 for doc in docs if (doc_path / doc).exists())
print(f"   ✅ Found {found_docs}/{len(docs)} documentation files")

print()
print("=" * 60)
print("🎉 All tests passed! Time Warp IDE is ready to use.")
print("=" * 60)
print()
print("To launch the IDE:")
print("  python time_warp_ide.py")
print()
print("Or use the launch script:")
print("  ./launch_ide.sh")
print()
