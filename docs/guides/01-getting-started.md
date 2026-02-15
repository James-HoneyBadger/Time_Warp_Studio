# Getting Started with Time Warp Studio

Welcome! This guide will get you up and running with Time Warp Studio in minutes.

---

## Installation

### Prerequisites

- **Python 3.10+** (Python 3.11+ recommended)
- **Operating System:** Linux, macOS, or Windows
- **RAM:** 4GB minimum, 8GB recommended
- **CPU:** Modern CPU with SSSE3/SSE4 support (required for PySide6)
  - Most CPUs from 2012+ have this
  - Older VMs/QEMU may lack support

### Step 1: Clone the Repository

```bash
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio
```

### Step 2: Create Virtual Environment

```bash
# Create virtual environment
python3 -m venv .venv

# Activate it
# On Linux/macOS:
source .venv/bin/activate

# On Windows:
.venv\Scripts\activate
```

### Step 3: Install Dependencies

```bash
pip install PySide6 Pillow requests
```

This installs:
- **PySide6** - Qt GUI framework
- **Pillow** - Image processing
- **requests** - HTTP library

### Step 4: Launch IDE

```bash
python Platforms/Python/time_warp_ide.py
```

The IDE window should appear in 2-3 seconds.

---

## First Look at the IDE

When you open Time Warp Studio, you'll see:

```
┌─────────────────────────────────────────┐
│ File Edit View Help                     │
├─────────────────────────────────────────┤
│ [Language ▼] [Run] [Stop] [Clear]      │
├──────────────┬──────────────────────────┤
│              │                          │
│   Editor     │   Turtle Graphics/      │
│              │   Output Panel           │
│              │                          │
├──────────────┴──────────────────────────┤
│ Status: Ready                          │
└─────────────────────────────────────────┘
```

### Main Areas

1. **Menu Bar** - File, Edit, View, Help menus
2. **Toolbar** - Language selector, Run/Stop buttons, Clear button
3. **Editor** (left) - Write your code here
4. **Output Panel** (right) - See results and turtle graphics
5. **Status Bar** - Current status messages

---

## Your First Program

### 1. Select a Language

Click the dropdown next to the toolbar that says "Language" and select **BASIC**.

### 2. Write Code

In the editor, type:

```basic
PRINT "Hello, World!"
```

### 3. Run It

Press **Ctrl+R** or click the **Run** button in the toolbar.

You should see:
```
🚀 Running code...
Hello, World!
✅ Execution complete
```

### Congratulations! 🎉

You've written and executed your first program in Time Warp Studio!

---

## Second Program: Interactive Input

Let's try BASIC with user input:

```basic
INPUT "What is your name? "; NAME$
PRINT "Hello, "; NAME$; "!"
```

When you run this:
1. The IDE will prompt for your name
2. Type your name and press Enter
3. See a personalized greeting

---

## Third Program: Turtle Graphics

Switch to **Logo** language and try:

```logo
FORWARD 100
RIGHT 90
FORWARD 100
RIGHT 90
FORWARD 100
RIGHT 90
FORWARD 100
```

This draws a square! The turtle graphics display appears in the right panel.

---

## Menu Overview

### File Menu
- **New** - Create new program
- **Open** - Load an existing program
- **Save** - Save current program
- **Exit** - Close IDE

### Edit Menu
- **Undo/Redo** - Reverse recent changes
- **Cut/Copy/Paste** - Edit text
- **Find & Replace** - Search code

### View Menu
- **Show/Hide Output** - Toggle output panel
- **Show/Hide Turtle** - Toggle graphics
- **Themes** - Change color scheme
- **Zoom** - Adjust text size

### Help Menu
- **About** - Version information
- **User Manual** - Open this guide
- **Keyboard Shortcuts** - See all shortcuts

---

## Keyboard Shortcuts

Most useful shortcuts:

| Shortcut | Action |
|----------|--------|
| **Ctrl+R** | Run program |
| **Ctrl+S** | Save program |
| **Ctrl+O** | Open program |
| **Ctrl+N** | New program |
| **Ctrl+F** | Find text |
| **Ctrl+H** | Find & replace |
| **Ctrl+E** | Clear output |
| **F1** | Help |

See Help menu for complete list.

---

## What to Try Next

### Learn a Language
Pick a language and explore:
- [BASIC Tutorial](../tutorials/basic.md)
- [Logo Tutorial](../tutorials/logo.md)
- [Python Guide](../tutorials/python.md)

### Run Examples
Browse `Examples/` directory for sample programs in all languages.

### Explore Graphics
Try turtle graphics commands in the [Turtle Graphics Guide](04-turtle-graphics.md).

### Customize
Change themes and settings in [Settings Guide](06-settings.md).

---

## Troubleshooting

### IDE Won't Start

**Error: "Illegal instruction"**
- Your CPU doesn't support required features
- Try running on a newer computer or cloud VM

**Error: "No module named PySide6"**
- Dependencies not installed
- Run: `pip install PySide6 Pillow requests`

**Window appears but is blank**
- Click in the window or wait 5 seconds
- Check your graphics drivers

### Program Won't Run

**SyntaxError message**
- Check the error line number shown in output
- Review syntax for that language

**Program runs but no output**
- Some programs expect input (look for prompt)
- Check if output is redirected to graphics panel

### Performance Issues

- Close other applications
- Reduce graphics complexity
- Try a simpler program first

For more help, see [Troubleshooting Guide](08-troubleshooting.md).

---

## Next Steps

1. ✅ You've installed and run the IDE
2. ✅ You've written your first program
3. 📚 Pick a language to learn
4. 🎨 Experiment with turtle graphics
5. 💾 Save your programs
6. 🚀 Share your creations

---

## Resources

- **Main Documentation** - [README.md](../../README.md)
- **Language Tutorials** - [Tutorials](../tutorials/)
- **Feature Guides** - [Guides](../guides/)
- **API Reference** - [API Docs](../api/)
- **Examples** - `Examples/` directory

---

**Happy Programming! Questions? Check the [FAQ](../reference/faq.md) or ask for help.**
