# Debugger Guide - Time Warp Studio

Complete guide to using the integrated debugger for debugging programs in Time Warp Studio.

---

## Overview

Time Warp Studio includes a powerful debugger with:

- **Breakpoints** - Stop execution at specific lines
- **Stepping** - Execute one statement at a time
- **Timeline** - Full execution history
- **Variables** - Inspect variable values
- **Stack** - View call stack
- **Rewind** - Navigate backwards through execution

---

## Enabling the Debugger

### Method 1: Debug Mode Toggle

```
Run → Debug Mode [Toggle]
```

Enables debugger for next execution.

### Method 2: Using Breakpoints

Set a breakpoint (Ctrl+B), then run. Debugger automatically activates.

---

## Setting Breakpoints

### Toggle Breakpoint

In editor, click line number to set/remove breakpoint:

```
Ctrl+B          on selected line
```

Or click left margin at desired line.

### Breakpoint Indicators

- Red dot = breakpoint set
- Program stops at breakpoint before executing line

### Example

```basic
' Breakpoint here (line 3)
FOR I = 1 TO 10
    PRINT I          ' < Click line number to set breakpoint
    X = X + 1        ' Program stops here
NEXT I
PRINT "Done"
```

---

## Stepping Through Code

### Step Into (F11)

Execute next statement. If it's a function call, enter the function.

```
Debug → Step Into [F11]
```

Use when:
- Entering procedures/functions
- Debugging function behavior

### Step Over (F10)

Execute next statement. If it's a function call, execute entire function.

```
Debug → Step Over [F10]
```

Use when:
- Function already tested
- Want to move quickly
- Debugging main logic

### Continue (F5)

Resume execution until next breakpoint or program completion.

```
Debug → Continue [F5]
```

Use when:
- Running past current section
- Jumping to next breakpoint

### Pause

While program is running:

```
Debug → Pause
```

Pauses execution mid-run so you can inspect state.

### Stop

```
Debug → Stop
```

Completely stop execution (can't resume).

---

## Inspecting Variables

### Variables Inspector Panel

Located in bottom-right panel tabs.

Shows all currently active variables with:
- Variable name
- Current value
- Data type (number, string, etc.)
- Updates after each step

### Example Inspection

```
VARIABLES
├─ I = 5 (INTEGER)
├─ X = 25 (INTEGER)
├─ NAME$ = "John" (STRING)
└─ GRID(10) = [array of 10 values]
```

### Hovering Over Variables

In editor, hover over variable name to see tooltip:

```
Tooltip: X = 25
```

---

## Execution Timeline

### View Timeline

```
Debug → Timeline
```

Opens timeline viewer showing:
- Step number (0, 1, 2, 3...)
- Line number executed
- Command/statement
- Variable state at that point
- Output generated
- Any errors

### Timeline Example

```
Step  Line  Command              X    Y    Output
────────────────────────────────────────────────
 0    5     X = 10              10   -    
 1    6     Y = 20              10   20   
 2    7     PRINT X + Y         10   20   30
 3    8     FOR I=1 TO 5        10   20   30
 4    9     PRINT I             10   20   30
 5    9     PRINT I             10   20   30, 1
 6    9     PRINT I             10   20   30, 1, 2
 ...
```

### Navigating Timeline

- Click on step to jump to that point
- Use arrow buttons to move forward/backward
- All variables shown for each step

---

## Stack Inspection

### View Call Stack

```
Debug → Stack Trace
```

Shows call stack for procedure/function calls:

```
Stack (top to bottom):
├─ MAIN
│  └─ CALCULATE (line 15)
│     └─ HELPER (line 22)
```

Shows where each procedure was called from.

---

## Common Debugging Workflows

### Finding a Bug

1. **Identify problem** - What's wrong?
2. **Add breakpoint** near suspected location
3. **Run program** - stops at breakpoint (Ctrl+R)
4. **Step through** - use F10/F11 to execute
5. **Watch variables** - are values what expected?
6. **Check timeline** - are steps in right order?
7. **Fix and re-run** - try solution

### Debugging Infinite Loops

```basic
WHILE TRUE              ' Infinite loop!
    PRINT "Help"
WEND
```

To debug:
1. Run with breakpoint on loop line
2. Use F10 to step past loop once
3. Set breakpoint AFTER loop
4. Use Continue (F5) to skip loop
5. Or click Stop to halt

### Tracing Variable Changes

1. Open Variables Inspector
2. Set breakpoint in area of interest
3. Run program (stops at breakpoint)
4. Use Step Over (F10) repeatedly
5. Watch variable values change
6. Use Timeline to see full history

### Array Debugging

```basic
DIM GRID(10, 10)
GRID(5, 5) = 42

' Debug: Is value correct?
```

To inspect array:
1. Set breakpoint after array assignment
2. Run program, stops at breakpoint
3. View Variables Inspector
4. Click GRID to expand array contents
5. See all values stored

---

## Example Debug Session

Create and debug this program:

```basic
DIM NUMBERS(5)
FOR I = 1 TO 5
    NUMBERS(I) = I * I
NEXT I

SUM = 0
FOR I = 1 TO 5
    SUM = SUM + NUMBERS(I)
NEXT I

PRINT "Sum:", SUM
```

### Debug Steps

1. **Set breakpoints**:
   - Line 2 (FOR loop start)
   - Line 8 (Second FOR loop)

2. **Run with Debug Mode**:
   ```
   Run → Debug Mode (toggle on)
   Ctrl+R (Run)
   ```

3. **Stops at breakpoint 1**:
   - Variables Inspector shows: I=1
   - Step Over to populate array

4. **Continue to breakpoint 2**:
   ```
   F5 (Continue)
   ```

5. **Inspect variables**:
   - See NUMBERS array with values
   - See SUM before accumulation

6. **Step through sum loop**:
   ```
   F10 (Step Over)
   ```
   - Watch SUM increase each step
   - Verify calculation is correct

7. **View Timeline**:
   ```
   Debug → Timeline (after program ends)
   ```
   - Scroll through execution history
   - Confirm correct sequence

---

## Debugger Features by Language

All 7 languages support:
- Breakpoints
- Stepping (Step Into/Over)
- Variables inspection
- Timeline recording

### Logo-Specific

Turtle state visible in:
- Variables Inspector (turtle_x, turtle_y, turtle_angle)
- Canvas shows turtle position
- Turtle Inspector panel

### PILOT-Specific

- T-units are single steps
- A-units pause for input
- J-units show conditional jumps

---

## Performance Impact

Debugging has minimal performance impact:

- **Debugger Enabled**: ~10% slower
- **Breakpoint Hit**: Execution pauses
- **Stepping**: Each step takes normal execution time
- **Timeline**: Recorded after execution completes

Large programs may use more memory with timeline recording.

---

## Tips for Effective Debugging

### Tip 1: Use Breakpoints Strategically
- Set before problematic area
- Don't set in fast loops (too many stops)
- Use Continue (F5) to jump between breakpoints

### Tip 2: Inspect Variables Early
- Set breakpoint at variable init
- Watch first few steps carefully
- Catch issues early

### Tip 3: Use Timeline for Complex Logic
- Run entire program once
- Review timeline after
- See all state changes
- Identify divergence from expected

### Tip 4: Step Over vs Into
- Step Over (F10) for known-good functions
- Step Into (F11) for suspected functions
- Speed through with F10, drill down with F11

### Tip 5: Watch Stack Trace
- Helps understand procedure flow
- Confirms recursive calls
- Shows where you are in execution

---

**For more information:**
- [USER_GUIDE.md](USER_GUIDE.md) - General IDE usage
- [LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md) - Language syntax
- [FAQ.md](FAQ.md) - Common questions
