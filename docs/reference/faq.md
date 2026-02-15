# Frequently Asked Questions (FAQ)

Quick answers to common questions about Time Warp Studio.

---

## General Questions

### What is Time Warp Studio?

Time Warp Studio is a desktop educational programming environment built with Python and PySide6 (Qt6) that combines classic and modern programming languages in a single application. It's perfect for learning programming concepts through languages like BASIC, Logo, PILOT, C, Pascal, Prolog, and Forth with integrated turtle graphics.

### Who should use Time Warp Studio?

- Students learning to program
- Teachers in computer science classes
- Anyone interested in classic programming languages
- People wanting to learn graphics programming
- Educators exploring different language paradigms

### Is Time Warp Studio free?

Yes! Time Warp Studio is open source and completely free under the Apache 2.0 license.

### Can I use Time Warp Studio commercially?

Yes. The Apache 2.0 license permits commercial use. See LICENSE file for details.

---

## Installation & Setup

### What are the system requirements?

- **Python 3.10+** (3.11+ recommended)
- **4GB RAM minimum** (8GB+ recommended)
- **Modern CPU** with SSSE3/SSE4 support (most from 2012+)
- **Operating System:** Linux, macOS, or Windows
- **PySide6** (Qt6) - installed automatically with pip

### Why do I get "Illegal instruction" error?

Your CPU lacks required features (SSSE3, SSE4.1, SSE4.2, POPCNT). This commonly happens on:
- Very old computers
- Virtual machines on incompatible hosts
- Some ARM-based systems

**Solution:** Use a newer physical computer or cloud VM.

### How do I install Time Warp Studio?

1. Clone: `git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git`
2. Create venv: `python3 -m venv .venv`
3. Activate: `source .venv/bin/activate`
4. Install: `pip install PySide6 Pillow requests`
5. Run: `python Platforms/Python/time_warp_ide.py`

See [Getting Started Guide](guides/01-getting-started.md) for details.

### Does Time Warp Studio run on Windows?

Yes! All steps work on Windows. Uses `.venv\Scripts\activate` instead of `source`.

### Does Time Warp Studio run on Mac?

Yes! Works on both Intel and Apple Silicon Macs.

---

## Programming Questions

### What languages does Time Warp support?

- **BASIC** - Classic BASIC language
- **Logo** - Turtle graphics and drawing
- **PILOT** - Computer-based instruction
- **C** - C language
- **Pascal** - Structured programming
- **Prolog** - Logic programming
- **Forth** - Stack-based programming

### Can I save my programs?

Yes! Use File → Save or Ctrl+S. Programs are saved with language-appropriate extensions (.bas, .logo, .py, etc.).

### Can I use functions/procedures?

Yes! Each language supports its own function syntax:

**BASIC:**
```basic
GOSUB PrintHello
END

PrintHello:
  PRINT "Hello"
  RETURN
```

**Logo:**
```logo
PROCEDURE SQUARE SIZE
  FOR I = 1 TO 4
    FORWARD SIZE
    RIGHT 90
  NEXT I
END PROCEDURE

SQUARE 100
```

### How do I get user input?

Use INPUT statement:

**BASIC:**
```basic
INPUT "Your name: "; NAME$
PRINT "Hello "; NAME$
```

A dialog box appears for user input.

### Why is my program slow?

Common causes:
- Large loops (millions of iterations)
- Complex graphics
- Deep recursion

**Solutions:** Simplify loops, reduce graphics complexity, check algorithm efficiency.

---

## Graphics Questions

### How do I draw graphics?

Use **Logo** language with turtle graphics:

```logo
FORWARD 100
RIGHT 90
FORWARD 100
```

See [Logo Tutorial](tutorials/logo.md) and [Turtle Graphics Guide](guides/04-turtle-graphics.md).

### Can I use other languages for graphics?

Currently graphics are built into Logo. Python, C, etc. support text output but not graphics in Time Warp.

### How do I change colors?

In Logo, use SETPENCOLOR:

```logo
SETPENCOLOR 1    ' Red
FORWARD 100
SETPENCOLOR 4    ' Blue
FORWARD 100
```

Colors 0-7 are: Black, Red, Green, Yellow, Blue, Magenta, Cyan, White

### Can I save graphics as images?

Currently, graphics are displayed in the IDE canvas but not automatically saved as images. You can take screenshots.

---

## Features & Capabilities

### Can I use external libraries?

Currently, Time Warp uses built-in functionality only. Integration of external libraries is planned for future versions.

### Is there a debugger?

Time Warp provides:
- Error messages with line numbers
- Variable inspection through PRINT
- Step-by-step execution (using PRINT statements)

Full debugging tools are planned.

### Can I import/use other files?

Currently, single-file programs are supported. Multi-file projects are planned.

### Does Time Warp support version control?

Time Warp saves files you can commit to Git. Integrated version control is planned.

---

## Customization & Settings

### How do I change the theme?

Use **View → Themes** in menu bar:
- Dracula, Monokai, Solarized Dark
- Ocean, Spring, Sunset, Candy, Forest

Theme preference is saved automatically.

### How do I zoom in/out?

- **Zoom In:** Ctrl++ 
- **Zoom Out:** Ctrl+-
- **Reset:** Ctrl+0

### Where are settings stored?

Settings saved in `~/.Time_Warp/config.json`:
- Theme preference
- Font size
- Recent files
- Auto-save setting

You can edit this file directly (careful with syntax!).

### Can I change the font?

Currently, font is fixed. Font selection is planned for future versions.

---

## Troubleshooting & Support

### I found a bug. What do I do?

1. Note the exact error and steps to reproduce
2. Check [Troubleshooting Guide](guides/08-troubleshooting.md)
3. Report on [GitHub Issues](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues)
4. Include OS, Python version, and error message

### Where can I get more help?

- **Documentation:** See `docs/` directory
- **Tutorials:** Learn each language in `docs/tutorials/`
- **Examples:** Run sample programs in `Examples/` directory
- **GitHub:** [Issues](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues)
- **Email:** james@honey-badger.org

### How do I report a problem?

Provide:
1. Operating system and version
2. Python version (`python --version`)
3. Exact error message or description
4. Code that reproduces the problem
5. Steps taken before the error

---

## Development & Contributing

### Can I contribute to Time Warp Studio?

Yes! Please read [CODE_OF_CONDUCT.md](../../CODE_OF_CONDUCT.md) first, then:

1. Fork the repository
2. Create feature branch
3. Make your changes
4. Submit pull request

### What areas need help?

- Documentation improvements
- Example programs
- Language support expansion
- Bug fixes
- Performance optimization
- UI/UX improvements

### How do I add a new language?

See [ARCHITECTURE.md](../../ARCHITECTURE.md#adding-a-new-language) for detailed instructions.

### Can I use Time Warp Studio code in my project?

Yes, under Apache 2.0 license terms. See LICENSE file.

---

## Performance & Limitations

### Why is the first run slow?

PySide6 loads Qt libraries on first startup. Subsequent runs are much faster.

### What's the maximum program size?

No hard limit, but very large programs (10,000+ lines) may slow down the editor.

### Can I run multiple programs simultaneously?

Currently, one program at a time. Parallel execution is planned.

### Are there memory limits?

Limited by your computer's RAM. Most programs use minimal memory.

---

## Future Features

### What's planned for next version?

- Full debugger with breakpoints
- Project management system
- Integrated version control
- More language support
- Graphics export to images
- Online code sharing

### How can I suggest features?

File an issue on [GitHub](https://github.com/James-HoneyBadger/Time_Warp_Studio) with "Feature Request" label.

### When will [feature] be added?

Check GitHub Issues for roadmap. No specific timeline available.

---

## Still Have Questions?

- **Read the docs** - Most answers in documentation
- **Check examples** - Examples/ folder has working code
- **Search GitHub issues** - Others may have same question
- **Ask for help** - Email james@honey-badger.org or file GitHub issue

---

**Happy programming!** 🚀
