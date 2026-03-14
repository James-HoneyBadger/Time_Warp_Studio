# 05 — Debugger

A guide to using the integrated debugger in Time Warp Studio.

> For full reference, see [DEBUGGER_GUIDE.md](../DEBUGGER_GUIDE.md).

---

## Overview

Time Warp Studio includes a step-through debugger with breakpoints, variable
inspection, call stack viewing, execution timeline, and rewind capabilities.

---

## Enabling the Debugger

| Method | How |
|--------|-----|
| Menu toggle | **Run → Debug Mode** |
| Breakpoint | Set a breakpoint with **Ctrl+B**, then run |
| Debug Panel | Click **Enable Debug** in the Debug panel |

---

## Setting Breakpoints

- **Ctrl+B** toggles a breakpoint on the current line.
- Click the left margin (line-number gutter) to toggle visually.
- A red dot indicates an active breakpoint.

---

## Stepping Through Code

| Action | Shortcut | Description |
|--------|----------|-------------|
| Step Into | **F11** | Execute the next statement |
| Step Over | **F10** | Execute next, skipping function internals |
| Continue | **F5** | Resume until next breakpoint |
| Stop | **Shift+F5** | Halt execution |

---

## Inspecting State

### Variables Panel

While paused, the **Variables Inspector** panel shows all current variable
names, types, and values. Values update after every step.

### Call Stack

The **Stack Trace** shows the current call depth. Click any frame to jump
to that source location.

---

## Execution Timeline

The debugger records every executed statement. Use the timeline slider in the
Debug panel to scrub forwards and backwards through the history.

- **Rewind** — step backwards to any previous state.
- **Replay** — re-execute from a chosen point.

---

## Tips

1. Set breakpoints **before** running so the debugger activates automatically.
2. Use the timeline to understand loops — scrub through iterations.
3. Watch specific variables by right-clicking them in the Variables panel.
4. Breakpoints persist across runs but not across IDE restarts.

---

## Further Reading

- [Full Debugger Reference](../DEBUGGER_GUIDE.md)
- [Keyboard Shortcuts](07-shortcuts.md)
- [Troubleshooting](08-troubleshooting.md)
