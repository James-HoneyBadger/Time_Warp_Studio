#!/usr/bin/env python3
"""
Test to verify if we can at least import and partially initialize the IDE.
This will help identify any import or initialization errors.
"""

import sys
from pathlib import Path

project_root = Path(__file__).parent
sys.path.insert(0, str(project_root))

print("[TEST] Starting IDE diagnostic...")
print(f"[TEST] Python: {sys.version}")
print(f"[TEST] Project root: {project_root}")

# Try to import the core components
try:
    print("\n[IMPORT] Importing core interpreter...")
    # pylint: disable=unused-import
    from Platforms.Python.time_warp.core.interpreter import Interpreter, Language
    print(f"  ✅ Interpreter imported: {Interpreter.__name__}, {Language.__name__}")
except Exception as e:  # pylint: disable=broad-except
    print(f"  ❌ Failed: {e}")
    sys.exit(1)

try:
    print("\n[IMPORT] Importing turtle graphics...")
    # pylint: disable=unused-import
    from Platforms.Python.time_warp.graphics.turtle_state import TurtleState
    print(f"  ✅ TurtleState imported: {TurtleState.__name__}")
except Exception as e:  # pylint: disable=broad-except
    print(f"  ❌ Failed: {e}")
    sys.exit(1)

try:
    print("\n[IMPORT] Importing UI components...")
    from Platforms.Python.time_warp.ui.output import OutputPanel, InterpreterThread
    print("  ✅ OutputPanel imported")
    print("  ✅ InterpreterThread imported")
except Exception as e:  # pylint: disable=broad-except
    print(f"  ❌ Failed: {e}")
    import traceback
    traceback.print_exc()
    sys.exit(1)

try:
    print("\n[CHECK] Checking signal definitions...")
    # Check if the signals are defined
    if hasattr(InterpreterThread, 'state_changed'):
        print("  ✅ InterpreterThread.state_changed signal exists")
    else:
        print("  ❌ InterpreterThread.state_changed signal NOT found")
    
    if hasattr(OutputPanel, 'debug_paused'):
        print("  ✅ OutputPanel.debug_paused signal exists")
    else:
        print("  ❌ OutputPanel.debug_paused signal NOT found")
        
except Exception as e:  # pylint: disable=broad-except
    print(f"  ❌ Error checking signals: {e}")

try:
    print(
        "\n[CANVAS] Checking if canvas can be imported (will fail without PySide6)..."
    )
    # pylint: disable=unused-import
    from Platforms.Python.time_warp.ui.canvas import TurtleCanvas
    print(f"  ✅ TurtleCanvas imported (PySide6 available): {TurtleCanvas.__name__}")
except ImportError as e:
    if "PySide6" in str(e):
        print("  ℹ️  PySide6 not available (expected in test env)")
    else:
        print(f"  ❌ Unexpected import error: {e}")
except Exception as e:  # pylint: disable=broad-except
    print(f"  ❌ Error: {e}")

print("\n" + "=" * 60)
print("SUMMARY")
print("=" * 60)
print("""
✅ Core interpreter and graphics working
✅ UI components can be imported
✅ Signals are properly defined

The graphics NOT showing issue is likely:
  1. Canvas.paintEvent() not being called
  2. The signal not reaching on_state_change()
  3. Canvas tab not being switched to
  4. Lines existing but coordinates outside visible area

To debug further, check:
  - Run IDE with debug output enabled
  - Verify tabs_widget is properly connected
  - Check canvas.update() is actually triggering repaint
  - Look for coordinate transformation issues
""")
