#!/usr/bin/env python3
"""
Diagnostic test for graphics rendering pipeline.
Tests the complete flow from code execution to canvas update.
"""

import sys
from pathlib import Path

# Add project to path
project_root = Path(__file__).parent
sys.path.insert(0, str(project_root))

from Platforms.Python.time_warp.core.interpreter import Interpreter, Language
from Platforms.Python.time_warp.graphics.turtle_state import TurtleState
# Note: TurtleCanvas requires PySide6


def test_graphics_pipeline():
    """Test the complete graphics pipeline."""
    
    print("=" * 60)
    print("GRAPHICS PIPELINE DIAGNOSTIC TEST")
    print("=" * 60)
    
    # Step 1: Create turtle
    print("\n[1/4] Creating TurtleState...")
    turtle = TurtleState()
    print(f"  ✅ Turtle created: pen_down={turtle.pen_down}, lines={len(turtle.lines)}")
    
    # Step 2: Setup callback tracking
    print("\n[2/4] Setting up callback tracking...")
    callbacks = []

    def track_callback():
        callbacks.append({
            'time': len(callbacks),
            'lines': len(turtle.lines),
            'pos': (turtle.x, turtle.y),
        })
    
    turtle.on_change = track_callback
    print("  ✅ Callback handler installed")
    
    # Step 3: Execute LOGO program
    print("\n[3/4] Executing LOGO program...")
    logo_code = """FORWARD 100
RIGHT 90
FORWARD 100
LEFT 45
FORWARD 70"""
    
    try:
        interp = Interpreter()
        interp.load_program(logo_code, Language.LOGO)
        interp.execute(turtle)
        
        print("  ✅ Program executed successfully")
        print(f"     - Callbacks triggered: {len(callbacks)}")
        print(f"     - Lines generated: {len(turtle.lines)}")
        print(f"     - Final position: ({turtle.x:.1f}, {turtle.y:.1f})")
        
        if turtle.lines:
            print("     - Line details:")
            for i, line in enumerate(turtle.lines[:5]):
                print(f"       Line {i}: ({line.start_x:.1f}, {line.start_y:.1f}) -> "
                      f"({line.end_x:.1f}, {line.end_y:.1f})")
    except Exception as e:  # pylint: disable=broad-except
        print(f"  ❌ Error: {e}")
        import traceback
        traceback.print_exc()
        return False
    
    # Step 4: Test canvas update (without rendering)
    print("\n[4/4] Testing canvas update...")
    
    try:
        # We can't create a real canvas without PySide6, but we can simulate the update
        print("  ℹ️  Would call canvas.set_turtle_state(turtle)")
        print(f"  ℹ️  Canvas would receive {len(turtle.lines)} lines")
        
        # Simulate what canvas.set_turtle_state does
        canvas_lines_copy = turtle.lines.copy()
        print("  ✅ Simulated canvas.set_turtle_state()")
        print(f"     - Canvas has {len(canvas_lines_copy)} lines")
        
        # Check that lines have valid coordinates
        all_valid = True
        for i, line in enumerate(canvas_lines_copy):
            if not (
                isinstance(line.start_x, (int, float))
                and isinstance(line.start_y, (int, float))
                and isinstance(line.end_x, (int, float))
                and isinstance(line.end_y, (int, float))
            ):
                print(f"     ❌ Line {i} has invalid coordinates")
                all_valid = False
        
        if all_valid:
            print("     ✅ All line coordinates are valid")
        else:
            return False
            
    except Exception as e:  # pylint: disable=broad-except
        print(f"  ❌ Error: {e}")
        import traceback
        traceback.print_exc()
        return False
    
    print("\n" + "=" * 60)
    print("DIAGNOSTIC SUMMARY")
    print("=" * 60)
    print(f"""
Core Pipeline Status:
  ✅ Turtle state creation: WORKING
  ✅ Callback mechanism: WORKING ({len(callbacks)} callbacks)
  ✅ LOGO execution: WORKING ({len(turtle.lines)} lines)
  ✅ Canvas update simulation: WORKING
  
Next Steps (if graphics still not showing):
  1. Check if tab switching is working
  2. Verify canvas paintEvent is being called
  3. Check coordinate transformation in canvas
  4. Verify zoom/pan values aren't hiding lines
  
Coordinate System Check:
  - Lines should be centered around origin (0,0)
  - Y-axis should be flipped in painter (math coords)
  - Canvas should translate to center before drawing
""")
    
    return True


if __name__ == "__main__":
    success = test_graphics_pipeline()
    sys.exit(0 if success else 1)
