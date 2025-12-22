#!/usr/bin/env python3
"""
Test to verify the turtle graphics signal and rendering pipeline.
This mimics what happens in the IDE to diagnose where graphics rendering fails.
"""

import sys
from Platforms.Python.time_warp.graphics.turtle_state import TurtleState
from Platforms.Python.time_warp.core.interpreter import Interpreter, Language

def test_turtle_callback():
    """Test that turtle callbacks work."""
    print("Testing turtle callback mechanism...")
    
    turtle = TurtleState()
    
    # Set up callback
    callback_called = [False]
    def on_change():
        callback_called[0] = True
        print("  ✅ Callback triggered!")
    
    turtle.on_change = on_change
    
    # Execute a forward command
    print("  Executing: turtle.forward(100)")
    turtle.forward(100)
    
    if callback_called[0]:
        print("  ✅ SUCCESS: Callback was called!")
    else:
        print("  ❌ FAILED: Callback was NOT called!")
    
    # Check lines
    print(f"  Lines generated: {len(turtle.lines)}")
    if turtle.lines:
        line = turtle.lines[0]
        print(f"    Line 0: ({line.start_x}, {line.start_y}) -> ({line.end_x}, {line.end_y})")
    
    return callback_called[0] and len(turtle.lines) > 0

def test_interpreter_execution():
    """Test interpreter execution with turtle."""
    print("\nTesting interpreter with turtle...")
    
    turtle = TurtleState()
    interp = Interpreter()
    
    # Set up debug callback to see what's happening
    turtle.on_change = lambda: print("  📡 Turtle state changed!")
    
    # Load LOGO code
    code = "FORWARD 100\nRIGHT 90\nFORWARD 100"
    interp.load_program(code, Language.LOGO)
    
    print(f"  Executing LOGO code:\n{code}")
    output = interp.execute(turtle)
    
    print(f"  Output:\n{output}")
    print(f"  Turtle lines: {len(turtle.lines)}")
    
    for i, line in enumerate(turtle.lines):
        print(f"    Line {i}: ({line.start_x:.1f}, {line.start_y:.1f}) -> ({line.end_x:.1f}, {line.end_y:.1f}), color={line.color}, width={line.width}")
    
    return len(turtle.lines) == 2

def test_qt_signal_flow():
    """Test Qt signal flow (if PySide6 is available)."""
    print("\nTesting Qt signal flow...")
    
    try:
        from PySide6.QtCore import Signal, QObject
        
        class TestEmitter(QObject):
            state_changed = Signal()
        
        turtle = TurtleState()
        emitter = TestEmitter()
        
        signal_received = [False]
        def on_signal():
            signal_received[0] = True
            print("  ✅ Signal received!")
        
        # Connect signal
        emitter.state_changed.connect(on_signal)
        
        # Set turtle callback to emit signal
        turtle.on_change = lambda: emitter.state_changed.emit()
        
        # Trigger turtle change
        print("  Triggering turtle.forward(100)...")
        turtle.forward(100)
        
        if signal_received[0]:
            print("  ✅ SUCCESS: Signal was emitted and received!")
            return True
        else:
            print("  ❌ FAILED: Signal was NOT received!")
            return False
    
    except ImportError as e:
        print(f"  ⚠️ PySide6 not available: {e}")
        return False

if __name__ == "__main__":
    results = {}
    
    results["Turtle Callback"] = test_turtle_callback()
    results["Interpreter Execution"] = test_interpreter_execution()
    results["Qt Signal Flow"] = test_qt_signal_flow()
    
    print("\n" + "="*50)
    print("TEST RESULTS:")
    print("="*50)
    for test_name, passed in results.items():
        status = "✅ PASSED" if passed else "❌ FAILED"
        print(f"{test_name}: {status}")
    
    all_passed = all(results.values())
    print("="*50)
    print(f"Overall: {'✅ ALL TESTS PASSED' if all_passed else '❌ SOME TESTS FAILED'}")
    
    sys.exit(0 if all_passed else 1)
