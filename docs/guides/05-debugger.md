# 05 — Debugger

A guide to using the integrated debugger in Time Warp Studio.

> For full reference, see [DEBUGGER_GUIDE.md](../DEBUGGER_GUIDE.md).

---

## Overview

Time Warp Studio includes a step-through debugger with breakpoints, watch
expressions, a call stack, an execution timeline, and a turtle-state view.

---

## Starting the Debugger

| Method | How |
| -------- | ----- |
| Start debugging | **Debug → Start / Continue Debugging** or **Ctrl+F9** |
| Start paused on line 1 | **F7** (Trace Into) or **F8** (Step Over) |

---

## Setting Breakpoints

- **Ctrl+F8** toggles a breakpoint on the current line.
- Click the line-number gutter to toggle a breakpoint.
- **Ctrl+Shift+F8** clears all breakpoints.
- Breakpoints are saved per file and restored when the file is reopened.

---

## Stepping Through Code

| Action | Shortcut | Description |
| -------- | ---------- | ------------- |
| Trace Into | **F7** | Execute the next statement, entering subroutines |
| Step Over | **F8** | Execute the next statement, skipping subroutine internals |
| Step Out | **Shift+F8** | Run until the current subroutine returns |
| Continue | **Ctrl+F9** | Resume until the next breakpoint or program end |
| Pause | **Ctrl+F7** | Pause a running program |
| Stop / Reset | **Ctrl+F2** | Stop the run or debug session |

---

## Inspecting State

The **Debug** tab in the right-hand panel contains:

- **Watch Expressions**: expressions you add and re-evaluate while stepping
- **Call Stack**: the current call frames
- **Breakpoints**: the breakpoints set in the current file
- **Turtle State**: turtle position, heading, and pen state
- **Output Stream**: program output
- **Δ Variable Changes**: variables that changed on the last step

---

## Execution Timeline

The **Timeline** list in the Debug tab records each executed step. Use
**Export JSON** to save the timeline, or save a `.twreplay` file to replay the
run later.

---

## Tips

1. Set breakpoints **before** starting so execution pauses where you expect.
2. Use the Timeline to follow loop iterations step by step.
3. Use **Ctrl+F9** to continue past a section and **Ctrl+F2** to end the session.
4. Breakpoints are saved per file and persist across IDE restarts.

---

## Further Reading

- [Full Debugger Reference](../DEBUGGER_GUIDE.md)
- [Keyboard Shortcuts](07-shortcuts.md)
- [Troubleshooting](08-troubleshooting.md)
