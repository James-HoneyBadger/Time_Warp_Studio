# Keyboard Shortcuts Guide

The authoritative reference is [KEYBOARD_SHORTCUTS.md](../KEYBOARD_SHORTCUTS.md). Press **Shift+F1** or **Ctrl+?** in the IDE to open the Keyboard Shortcuts dialog, which is generated from the live menus. Shortcuts are display-only and cannot be customized in this build.

## File Operations

| Shortcut | Action |
| ---------- | -------- |
| **Ctrl+N** | New file |
| **Ctrl+O** | Open file |
| **Ctrl+S** | Save file |
| **Ctrl+Shift+S** | Save as (new name) |
| **Ctrl+P** | Print code |
| **Ctrl+Shift+H** | Export session as HTML |
| **Ctrl+,** | Preferences |
| **Alt+X** / **Ctrl+Q** | Exit |

## Editing

| Shortcut | Action |
| ---------- | -------- |
| **Ctrl+Z** | Undo |
| **Ctrl+Y** | Redo |
| **Ctrl+X** | Cut |
| **Ctrl+C** | Copy |
| **Ctrl+V** | Paste |
| **Ctrl+A** | Select all |
| **Ctrl+F** | Find |
| **Ctrl+H** | Find and replace |
| **Ctrl+G** | Go to line |
| **Shift+F4** / **Ctrl+Alt+G** | Find in files |
| **Ctrl+Shift+F** | Format code |

## Editor

| Shortcut | Action |
| ---------- | -------- |
| **Tab** | Indent line |
| **Shift+Tab** | Unindent line |
| **Ctrl+/** | Toggle line comment |
| **Ctrl+D** | Duplicate line |
| **Alt+Up** / **Alt+Down** | Move line up / down |
| **Ctrl+Space** | Trigger autocomplete |
| **Ctrl+Shift+[** / **Ctrl+Shift+]** | Fold / unfold block |

## Selection and Navigation

| Shortcut | Action |
| ---------- | -------- |
| **Shift+Home** | Select to start of line |
| **Shift+End** | Select to end of line |
| **Shift+Ctrl+Home** | Select to start of file |
| **Shift+Ctrl+End** | Select to end of file |
| **Ctrl+Home** | Go to start of file |
| **Ctrl+End** | Go to end of file |
| **Home** | Go to start of line |
| **End** | Go to end of line |
| **Ctrl+←** | Move word left |
| **Ctrl+→** | Move word right |
| **Page Up** | Scroll up |
| **Page Down** | Scroll down |

## Program Execution

| Shortcut | Action |
| ---------- | -------- |
| **F9** / **Ctrl+R** | Run program |
| **Shift+F9** / **Ctrl+Shift+R** | Run selection |
| **Ctrl+Alt+R** | Run both split panes (polyglot benchmark) |
| **Ctrl+F2** | Reset program (stops a run or debug session) |
| **Ctrl+F5** | Show errors only |
| **Ctrl+F9** | Start debugging, or continue when paused |
| **Ctrl+F7** | Pause |
| **F7** / **F8** / **Shift+F8** | Trace into / step over / step out |

## IDE Navigation

| Shortcut | Action |
| ---------- | -------- |
| **F10** / **Ctrl+Shift+P** | Command palette |
| **Ctrl+Shift+L** | Language picker |
| **F1** | Context help for the current language |
| **F4** | Document outline |

## View and Display

| Shortcut | Action |
| ---------- | -------- |
| **F5** | Zoom (focus mode) |
| **F6** | Next pane |
| **Ctrl+\** | Split editor right |
| **Ctrl+Shift+\** | Close split editor |
| **Shift+F6** | Telemetry HUD |
| **Shift+F7** | CRT effects |
| **Shift+F10** | Cycle workstation theme |
| **Ctrl+Alt+H** | Toggle high-contrast theme |
| **Ctrl++** / **Ctrl+-** | Increase / decrease editor font size |
| **Alt+1** to **Alt+4** | Workspace decks |

## Language Selection

Use **Ctrl+Shift+L** (Language picker) to change the active language. There are no Alt+letter language shortcuts.

## Help and Documentation

| Shortcut | Action |
| ---------- | -------- |
| **Ctrl+F1** | Keyword inspector |
| **Ctrl+F3** / **Ctrl+Alt+L** | Lessons |
| **Shift+F3** | Browse examples |
| **Shift+F1** / **Ctrl+?** | Keyboard shortcuts dialog |

## Snippets

| Shortcut | Action |
| ---------- | -------- |
| **Ctrl+Shift+I** | Insert snippet |

## Canvas (Turtle Graphics)

| Action | Description |
| -------- | ------------- |
| Scroll wheel | Zoom in / out |
| Click and drag | Pan the canvas |
| Save button | Save canvas as PNG |
| Grid button | Toggle coordinate grid overlay |

## Useful Combinations

### Quick Workflow

1. **Ctrl+N** - Create new file
2. **Type code** - Write your program
3. **Ctrl+S** - Save file
4. **Ctrl+R** - Run program
5. **Modify code** - Edit as needed
6. **Ctrl+R** - Run again

### Debugging Workflow

1. **Ctrl+F8** - Set a breakpoint on the current line
2. **Ctrl+F9** - Start debugging
3. **F8** - Step over; **F7** - trace into; **Shift+F8** - step out
4. **Ctrl+F2** - Stop the session
5. **Ctrl+Z** - Undo changes if needed

### Multi-File Editing

1. **Ctrl+O** - Open a file (**F3** also works)
2. **Ctrl+X/C/V** - Copy code between files
3. **Ctrl+S** - Save current file
4. **Ctrl+Shift+S** - Save with new name
5. **Ctrl+\** - Split the editor to view two files side by side

### Teaching/Presentation

1. **Ctrl++** - Increase editor font size
2. **F9** - Run the example
3. **F5** - Zoom (focus mode) to hide distractions
4. **Ctrl+F2** - Reset before running the next example

## Customizing Shortcuts

Shortcuts cannot be customized in this build. The **Keyboard Shortcuts** dialog (**Help → Keyboard Shortcuts**, **Ctrl+?**) is a read-only, searchable list of the current bindings.

## Platform Notes

- Qt maps **Ctrl** to **Cmd** on macOS, and function keys may require the **Fn** key.
- Desktop environments and window managers can capture some combinations. Check for conflicts if a shortcut does nothing.

## Troubleshooting Shortcuts

**Problem:** Shortcut not working

- **Solution:** Check the menu; each menu item shows its active binding.
- **Solution:** Another application or the desktop may capture the combination.
- **Solution:** Use the **FN** badge on the Function-Key Bar to switch layers manually.

**Problem:** Need a different binding

- **Solution:** Bindings cannot be changed in this build.

## Tips for Efficiency

1. **Use Ctrl+R constantly** - Run after every few changes
2. **Learn Ctrl+Z/Y** - Undo/redo are your safety net
3. **Use Ctrl+F** - Find speeds up navigation
4. **Use the language picker** - Ctrl+Shift+L
5. **Remember Ctrl+Home/End** - Jump to file start/end
6. **Use Ctrl+/** for comments** - Quick documentation
7. **Use Zoom (F5)** - Focus mode hides panels while you type
8. **Use Shift+F1** - Shows the full, live shortcut list

## Cheat Sheet Quick Reference

**Essential 5:**

- Ctrl+S - Save
- Ctrl+R - Run
- Ctrl+Z - Undo
- Ctrl+F - Find
- Ctrl+C/V - Copy/Paste

**Debugging 5:**

- Ctrl+F8 - Toggle breakpoint
- Ctrl+F9 - Start / continue
- F7 / F8 - Trace into / step over
- Shift+F8 - Step out
- Ctrl+F2 - Stop

**Pro Tips:**

- F1 - Context help
- Ctrl+? - See all shortcuts
- Ctrl+Shift+L - Language picker
- Ctrl+G - Go to specific line
- Tab/Shift+Tab - Fast indentation

## Next Steps

- Master [IDE Basics](02-ide-basics.md)
- Learn [Settings and Customization](06-settings.md)
- Review [Getting Started](01-getting-started.md)
- Explore language tutorials

Practice these shortcuts regularly to build muscle memory and work faster!
