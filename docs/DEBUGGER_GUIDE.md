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

### Method 1: Start Debugging

```text
Debug → Start / Continue Debugging [Ctrl+F9]
```

Runs the current program under the debugger. If no breakpoints are set, execution pauses at the first statement. F7 and F8 do nothing unless a session is already paused.

### Method 2: Using Breakpoints

Set a breakpoint (Ctrl+F8), then start debugging with Ctrl+F9. Execution stops at the breakpoint.

---

## Setting Breakpoints

### Toggle Breakpoint

In editor, click the left margin to set/remove a breakpoint, or:

```text
Ctrl+F8          on the current line
Ctrl+Shift+F8    clear all breakpoints
```

### Breakpoint Indicators

- Red dot = breakpoint set
- Program stops at breakpoint before executing line

### Example

```basic
10 X = 0
20 FOR I = 1 TO 10
30 PRINT I
40 X = X + 1
50 NEXT I
60 PRINT "Done"
```

Click the left margin on line 40 to set a breakpoint there.

---

## Stepping Through Code

### Trace Into (F7) and Step Over (F8)

Both advance one step while the program is paused. A step covers one line or one statement, depending on the **Step** selector in the Debug toolbar. Currently the two commands perform the same action.

```text
Debug → Trace Into [F7]
Debug → Step Over [F8]
```

### Step Out (Shift+F8)

Currently resumes execution until the next breakpoint or the end of the program, the same as Continue. It does not stop when the current procedure returns.

```text
Debug → Step Out [Shift+F8]
```

### Continue (Ctrl+F9)

Resume execution until next breakpoint or program completion.

```text
Debug → Start / Continue Debugging [Ctrl+F9]
```

Use when:

- Running past current section
- Jumping to next breakpoint

### Pause (Ctrl+F7)

While program is running:

```text
Debug → Pause [Ctrl+F7]
```

Pauses execution mid-run so you can inspect state.

### Stop (Ctrl+F2)

```text
Run → Reset Program [Ctrl+F2]
```

Completely stop execution or the debug session (can't resume).

---

## Inspecting Variables

### Variables Panel

The **🔍 Variables** tab in the right-hand panel lists the active variables in a two-column table (Variable, Value), sorted by name. It refreshes when execution pauses. The table is read-only.

Lists and arrays are shown as text in the Value column. The **Watch Expressions** list in the Debug tab accepts expressions you want to track.

---

## Execution Timeline

### View Timeline

The **🐛 Debug** tab contains a **🧭 Timeline** list. Each entry is a recorded frame in the form `Line N - source text`. When statement stepping is active, entries include a statement index, such as `Line N (2/3) - source text`.

### Navigating Timeline

- Select an entry to jump to that point
- Use **Export JSON** to save the timeline
- Use **Export Replay** to save the session as a `.twreplay` file

---

## Stack Inspection

### View Call Stack

The **📚 Call Stack** section of the **🐛 Debug** tab lists active calls, newest first, as `#N: NAME (Line N)`.

```text
#0: HELPER (Line 22)
#1: CALCULATE (Line 15)
```

---

## Common Debugging Workflows

### Finding a Bug

1. **Identify problem** - What's wrong?
2. **Add breakpoint** near suspected location
3. **Start debugging** - stops at breakpoint (Ctrl+F9)
4. **Step through** - use F8/F7 to execute
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

1. Start debugging with a breakpoint on the loop line
2. Use F8 to step past loop once
3. Set breakpoint AFTER loop
4. Use Continue (Ctrl+F9) to skip loop
5. Or press Ctrl+F2 to halt

### Tracing Variable Changes

1. Open Variables Inspector
2. Set breakpoint in area of interest
3. Start debugging (Ctrl+F9; stops at breakpoint)
4. Use Step Over (F8) repeatedly
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
2. Start debugging (Ctrl+F9), stops at breakpoint
3. View Variables Inspector
4. Check the GRID value in the Value column
5. Compare it with the expected value

---

## Example Debug Session

Create and debug this program:

```basic
10 DIM NUMBERS(5)
20 FOR I = 1 TO 5
30 NUMBERS(I) = I * I
40 NEXT I
50 SUM = 0
60 FOR I = 1 TO 5
70 SUM = SUM + NUMBERS(I)
80 NEXT I
90 PRINT "Sum:", SUM
```

### Debug Steps

1. **Set breakpoints**:
   - Line 20 (first FOR loop)
   - Line 60 (second FOR loop)

2. **Start debugging**:

   ```text
   Debug → Start / Continue Debugging (Ctrl+F9)
   ```

3. **Stops at breakpoint 1** before the first loop runs.
   - Press F8 to step one line at a time and watch NUMBERS fill in the Variables tab.

4. **Continue to breakpoint 2**:

   ```text
   Ctrl+F9 (Continue)
   ```

5. **Inspect variables**:
   - NUMBERS and SUM appear in the Variables tab

6. **Step through sum loop**:

   ```text
   F8 (Step Over)
   ```

   - Watch SUM increase each step
   - Verify calculation is correct

7. **View Timeline**:

   ```text
   🐛 Debug tab → 🧭 Timeline
   ```

   - Scroll through execution history
   - Confirm correct sequence

---

## Debugger Features by Language

- **Line-by-line languages** (BASIC, PILOT, Logo, C, Pascal, Prolog, Forth): breakpoints, stepping, the Variables tab, and the Timeline. The Step selector applies to BASIC only; other languages always step by line.
- **Brainfuck and Python**: no step-through. The output panel reports that the program runs in observation mode.

### Logo Turtle State

Turtle position, heading, pen, and color appear in the **🐢 Turtle State** panel of the Debug tab, not in the Variables tab.

---

## Performance Impact

Timeline frames are recorded while the program runs. Long programs produce long timelines and use more memory.

---

## Tips for Effective Debugging

### Tip 1: Use Breakpoints Strategically

- Set before problematic area
- Don't set in fast loops (too many stops)
- Use Continue (Ctrl+F9) to jump between breakpoints

### Tip 2: Inspect Variables Early

- Set breakpoint at variable init
- Watch first few steps carefully
- Catch issues early

### Tip 3: Use Timeline for Complex Logic

- Run entire program once
- Review timeline after
- See all state changes
- Identify divergence from expected

### Tip 4: Skip Ahead With Breakpoints

- Trace Into (F7) and Step Over (F8) currently do the same thing
- Use Continue (Ctrl+F9) with breakpoints to skip to the next point of interest

### Tip 5: Watch Stack Trace

- Helps understand procedure flow
- Confirms recursive calls
- Shows where you are in execution

---

**For more information:**

- [USER_GUIDE.md](USER_GUIDE.md) - General IDE usage
- [LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md) - Language syntax
- [FAQ](FAQ.md) - Common questions
