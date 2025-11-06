# Time Warp IDE - Desktop Quick Start Guide

## 🚀 Installation

### 1. Install Dependencies

```bash
cd Time_Warp_Python
pip install PySide6 pillow
```

**Alternative (development install):**
```bash
pip install -e .
```

### 2. Launch the IDE

```bash
python time_warp_ide.py
```

**Open a specific file:**
```bash
python time_warp_ide.py examples/logo_spiral_walk.logo
python time_warp_ide.py examples/basic_hangman.bas
python time_warp_ide.py examples/pilot_adventure.pilot
```

---

## 🖥️ Interface Overview

### Main Window Components

**1. Menu Bar**
- **File**: New, Open, Save, Save As, Recent Files (last 10), Exit
- **Run**: Run Program (F5), Stop Execution (Shift+F5), Clear Output, Clear Canvas
- **View**: Themes (8 themes), Font Size
- **Help**: Turtle Graphics Reference, About

**2. Code Editor** (Left Panel)
- ✨ Syntax highlighting for TempleCode (BASIC, PILOT, Logo)
- 🔢 Line numbers
- ⚡ Auto-indentation
- 📝 Undo/Redo support
- 🔍 Bracket matching

**3. Output Panel** (Right Panel - Top Tab)
- 📊 Multi-tab interface: **Text** and **Graphics**
- 🎨 Colored output with emoji indicators
- ⚡ Auto-switches to Graphics tab when drawing
- 📜 Scrollable text output

**4. Turtle Graphics Canvas** (Right Panel - Graphics Tab)
- 🐢 Interactive turtle rendering
- 🔍 Zoom: Mouse wheel or +/- buttons
- 🖱️ Pan: Click and drag
- 📐 Coordinate system: (0, 0) at center, Y-axis up
- 🎨 Real-time drawing updates

---

## 🎮 Running Your First Programs

### PILOT Interactive Program

```pilot
T:Welcome to Time Warp!
T:What is your name?
A:NAME
T:Hello *NAME*!
T:What is 5 + 3?
A:ANSWER
M:8
JY:CORRECT
T:Try again! What is 5 + 3?
J:START
L:CORRECT
T:Correct! Well done.
E:
```

**Steps:**
1. Click **File → New** or press **Ctrl+N**
2. Paste the code above
3. Press **F5** or click **Run**
4. Follow the prompts in dialog boxes

### BASIC Graphics Program

```basic
10 CLS
20 FOR I = 1 TO 10
30   LOCATE I, I
40   PRINT "Line "; I
50 NEXT I
60 PRINT "Done!"
```

### Logo Turtle Graphics

```logo
TO SQUARE :SIZE
  REPEAT 4 [
    FORWARD :SIZE
    RIGHT 90
  ]
END

SETCOLOR blue
PENWIDTH 3
SQUARE 100
```

**IDE automatically switches to Graphics tab when turtle draws!**

---

## 🎨 Themes

Switch via **View → Theme**:

Switch via **View → Theme**:

1. **Dracula** - Dark purple (popular!)
2. **Monokai** - Dark with vibrant colors
3. **Solarized Light** - Easy on the eyes
4. **Solarized Dark** - Dark variant
5. **Ocean** - Blue-gray theme
6. **Spring** - Fresh and light
7. **Sunset** - Warm orange tones
8. **Candy** - Purple-pink delight
9. **Forest** - Green nature theme

**Theme preference is saved automatically!**

---

## ⌨️ Keyboard Shortcuts

| Shortcut | Action |
|----------|--------|
| **F5** | Run program |
| **Shift+F5** | Stop execution |
| **Ctrl+N** | New file |
| **Ctrl+O** | Open file |
| **Ctrl+S** | Save file |
| **Ctrl+Shift+S** | Save As |
| **Ctrl+Z** | Undo |
| **Ctrl+Y** | Redo |
| **Ctrl++** | Zoom In (editor) |
| **Ctrl+-** | Zoom Out (editor) |

---

## 📚 Example Programs

The IDE includes **34 example programs**:

**Logo Turtle Graphics** (15 programs)
- `logo_square.logo` - Simple square
- `logo_spiral_walk.logo` - Colorful spiral
- `logo_koch_snowflake.logo` - Fractal
- `logo_flower.logo` - Petal flower with procedures
- ...and more fractals, spirographs, and shapes!

**BASIC Programs** (10 programs)
- `basic_hangman.bas` - Word guessing game
- `basic_graphics.bas` - Graphics demonstration
- `basic_rock_paper_scissors.bas` - Classic game
- ...and more interactive programs!

**PILOT Interactive** (7 programs)
- `pilot_adventure.pilot` - Text adventure
- `pilot_quiz_competition.pilot` - Quiz game
- `pilot_dragon_adventure.pilot` - Story adventure
- ...and more interactive tutorials!

**Access via**: **File → Open** → browse `examples/` directory

---

## 💡 Tips & Tricks

1. **Save Often**: IDE warns about unsaved changes on exit
2. **Recent Files**: Quick access to last 10 files (File → Recent)
3. **Error Hints**: Error messages include typo suggestions
4. **Canvas Zoom**: Use mouse wheel to zoom, click-drag to pan
5. **Auto-Switch**: IDE switches to Graphics tab automatically when drawing
6. **Procedures**: Logo supports user-defined procedures with `TO name :param1 ... END`
7. **Colors**: Use color names (`blue`), hex (`#FF5733`), or RGB (`255,100,50`)

---

## 🔧 Troubleshooting

### IDE Won't Start

```bash
# Check Python version (need 3.8+)
python --version

# Reinstall PySide6
pip install --upgrade PySide6 pillow
```

### Graphics Not Showing

1. Make sure you're using turtle commands (FORWARD, RIGHT, etc.)
2. Click the **Graphics** tab in output panel
3. Check that pen is down (use `PENDOWN` if needed)
4. Try zooming out (canvas might be zoomed in)

### Programs Running Slowly

- Check for infinite loops
- Reduce iteration counts in REPEAT loops
- Logo procedures with high recursion depth can be slow

### Theme Not Saving

Check QSettings configuration location:
- **Linux**: `~/.config/TimeWarp/`
- **macOS**: `~/Library/Preferences/`
- **Windows**: Registry under `HKEY_CURRENT_USER\Software\TimeWarp\`

### Input Dialogs Not Appearing

- INPUT commands show Qt dialog boxes
- Check if dialogs are behind main window
- Try Alt+Tab to find dialog window

---

## 📖 Next Steps

- ✅ **Explore Examples**: Browse all 34 example programs
- 📚 **Read Turtle Reference**: See [docs/TURTLE_GRAPHICS_REFERENCE.md](docs/TURTLE_GRAPHICS_REFERENCE.md)
- 🎨 **Try Different Themes**: Find your favorite!
- 💻 **Write Your Own**: Create programs in all three language styles
- 🤝 **Contribute**: Share your programs with the community!

---

## 🆘 Support

**Need Help?**
- 📖 Full Documentation: [README.md](README.md)
- 🐛 Report Issues: [GitHub Issues](https://github.com/James-HoneyBadger/Time_Warp/issues)
- 📧 Email: james@honey-badger.org
- 🌐 Repository: https://github.com/James-HoneyBadger/Time_Warp

---

<div align="center">

**Time Warp IDE** - *Classic educational programming meets modern desktop experience* 🚀

</div>
