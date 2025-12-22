# Graphics NOT Showing - Debugging Guide

## Status: Debug Output Added

The core graphics pipeline is **fully functional** (verified by unit tests). The issue is in the Qt/PySide6 rendering layer when running the actual IDE.

## What We Know

✅ **Working:**
- LOGO parser and interpreter
- Turtle graphics command execution  
- Signal callback mechanism
- Line generation (3+ lines confirmed)
- All coordinate math

❌ **Issue:**
- Graphics don't appear in the IDE canvas when running LOGO programs

## Debug Steps for User

When you run the IDE with a LOGO program and graphics don't show, check stderr for these debug messages:

### Expected Output Flow

```
[OUTPUT] run_program called: canvas=True, code_length=XX
[OUTPUT] Set current_canvas = True  
[OUTPUT] Created new turtle
[OUTPUT] _start_thread: language=Language.LOGO, turtle=True
[OUTPUT] Connected state_changed signal
[THREAD] Turtle changed! 1 lines
[THREAD] Turtle changed! 2 lines
[THREAD] Turtle changed! 3 lines
[OUTPUT] _on_state_changed signal received!
[OUTPUT] on_state_change: canvas=True, lines=3
[OUTPUT] Calling set_turtle_state with 3 lines
[CANVAS] set_turtle_state: 3 lines
[CANVAS] Copied lines: self.lines now has 3 lines
[CANVAS] Calling self.update()
[CANVAS] update() called
[CANVAS] paintEvent: 3 lines, zoom=1.0
```

## What To Check If Graphics Don't Appear

### 1. **Program Execution**
If you see messages up to `[THREAD] Turtle changed!` but NOT `[OUTPUT] _on_state_changed signal received!`:
- **Issue**: Signal not reaching the GUI thread
- **Solution**: Check signal-slot connection in Qt

### 2. **Canvas Update**
If you see `[CANVAS] Calling self.update()` but NOT `[CANVAS] paintEvent`:
- **Issue**: Canvas update not triggering paintEvent
- **Solution**: Canvas may need explicit repaint() instead of update(), or may be hidden

### 3. **Canvas Rendering**
If you see `[CANVAS] paintEvent: N lines` but still no graphics:
- **Issue**: Coordinate transformation is wrong OR lines are very small OR zoom is 0
- **Solution**:
  - Check zoom value (should be 1.0)
  - Verify lines have reasonable coordinates (hundreds of pixels, not micro-units)
  - Test manually drawing a simple line

### 4. **Tab Switching**
If you see graphics but they don't appear in the canvas:
- **Issue**: Canvas tab not being switched to
- **Solution**: Check if `[OUTPUT] Switching to Graphics tab (index X)` appears in debug output

## Code Changes Added

### 1. `/Platforms/Python/time_warp/ui/output.py`
- Line 84: Lambda wrapper for turtle callback (ALREADY FIXED)
- Added debug logging to: `run_program()`, `_start_thread()`, `_on_state_changed()`, `on_state_change()`

### 2. `/Platforms/Python/time_warp/ui/canvas.py`
- Lines 135-137: Pen width zoom adjustment (ALREADY FIXED)
- Added debug logging to: `paintEvent()`, `set_turtle_state()`

## How To Run The IDE For Debugging

```bash
# Install PySide6 if not installed
pip install PySide6

# Run IDE with stderr visible to see debug output
python Platforms/Python/time_warp_ide.py 2>&1 | tee debug.log

# Or run with explicit stderr redirection
python Platforms/Python/time_warp_ide.py 2>debug.log
```

Then:
1. Load a LOGO file or create a simple program: `FORWARD 100`
2. Click Run
3. Check debug.log for the output flow above
4. Report which message stops appearing

## Core Logic Verification

All tests pass:
- ✅ `test_simple_turtle.py` - Turtle state and callbacks work
- ✅ `test_graphics_pipeline.py` - Signal pipeline works  
- ✅ `test_graphics_diagnostic.py` - End-to-end core logic works

The problem is isolated to Qt rendering or tab management.

## Next Steps

1. **Run IDE with debug output** (see above)
2. **Report first missing message** in the expected output flow
3. **Check coordinate system** - verify lines aren't at (0,0) or huge values
4. **Try forced repaint** - replace `self.update()` with `self.repaint()` in canvas

## Manual Testing Without IDE

If you want to test the graphics independently:

```python
from Platforms.Python.time_warp.core.interpreter import Interpreter, Language
from Platforms.Python.time_warp.graphics.turtle_state import TurtleState

turtle = TurtleState()
interp = Interpreter()
interp.load_program("FORWARD 100\nRIGHT 90\nFORWARD 100", Language.LOGO)
interp.execute(turtle)

print(f"Lines: {len(turtle.lines)}")
for line in turtle.lines:
    print(f"  ({line.start_x}, {line.start_y}) -> ({line.end_x}, {line.end_y})")
```

This should output:
```
Lines: 2
  (0.0, 0.0) -> (0.0, 100.0)
  (0.0, 100.0) -> (100.0, 100.0)
```

If this works, the problem is definitely in the Qt rendering layer.

---

**Debug Version**: December 22, 2025  
**Status**: Awaiting user feedback with IDE debug output
