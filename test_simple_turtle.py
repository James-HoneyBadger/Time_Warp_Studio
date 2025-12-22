#!/usr/bin/env python3
"""Quick test of turtle state only."""

from Platforms.Python.time_warp.graphics.turtle_state import TurtleState

# Test 1: Basic turtle creation
print("Test 1: Creating turtle...")
turtle = TurtleState()
print(f"  Turtle created: x={turtle.x}, y={turtle.y}, pen_down={turtle.pen_down}")

# Test 2: Forward command
print("\nTest 2: Forward 100...")
turtle.forward(100)
print(f"  Turtle position: x={turtle.x}, y={turtle.y}")
print(f"  Lines created: {len(turtle.lines)}")
if turtle.lines:
    line = turtle.lines[0]
    print(f"  Line 0: ({line.start_x}, {line.start_y}) -> ({line.end_x}, {line.end_y})")

# Test 3: Callback
print("\nTest 3: Setting callback...")
callback_count = [0]
def on_change():
    callback_count[0] += 1
    print(f"  Callback #{callback_count[0]} triggered!")

turtle.on_change = on_change

print("  Executing forward(50)...")
turtle.forward(50)

if callback_count[0] > 0:
    print(f"  ✅ Callback works! Called {callback_count[0]} times")
else:
    print("  ❌ Callback NOT called!")

print(f"\nFinal: {len(turtle.lines)} lines, callback_count={callback_count[0]}")
