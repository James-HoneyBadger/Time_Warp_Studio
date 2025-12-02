# Frequently Asked Questions

Common questions and answers about Time Warp IDE.

---

## General Questions

### What is Time Warp IDE?

Time Warp IDE is an educational programming environment that supports six classic programming languages: BASIC, PILOT, Logo, Pascal, Prolog, and C. It's designed to help students learn programming through multiple paradigms in one unified tool.

### Why is it called "Time Warp"?

The name reflects our mission to teach modern programming concepts through classic languages. It's like taking a "time warp" back to when these languages were created, while using modern tools and interfaces.

### Is Time Warp IDE free?

Yes! Time Warp IDE is open source software released under the MIT License. It's completely free to use, modify, and distribute.

### What platforms does it run on?

- **Linux** (Debian, Ubuntu, Arch, Fedora, and more)
- **macOS** (10.13+)
- **Windows** (7+, though 10+ recommended)

### Do I need internet to use it?

No. Once installed, Time Warp IDE runs completely offline. Internet is only needed for downloading updates or accessing online documentation.

---

## Getting Started

### Which language should I start with?

**Complete beginners**: Start with BASIC or PILOT. They use English-like commands and are easy to understand.

**Visual learners**: Try Logo first. Turtle graphics make programming tangible and fun.

**Academic setting**: Pascal is excellent for learning structured programming concepts.

**Curious about AI**: Prolog introduces logic programming in an accessible way.

**Aspiring systems programmers**: C provides low-level programming experience.

### I've never programmed before. Can I use Time Warp IDE?

Absolutely! Time Warp IDE is designed for beginners. Start with the [Student Workbook](../student/00-workbook.md), which assumes no prior programming experience.

### Where do I find example programs?

The `examples/` folder contains sample programs for all six languages:
```
examples/
├── basic/
├── pilot/
├── logo/
├── pascal/
├── prolog/
└── c/
```

You can also find examples in the [Programming Guide](../user/01-programming-guide.md).

---

## Using the IDE

### How do I run a program?

1. Write your code in the editor
2. Press **F5** (or click Run → Run Program)
3. See results in the output panel

### Why isn't my program running?

**Check for**:
- **Syntax errors** - Look for red underlines in the editor
- **Wrong language** - Make sure Language menu matches your code
- **Missing END** - BASIC and Pascal programs need proper endings
- **Unmatched brackets** - Logo commands need proper brackets

### How do I save my work?

- **File → Save** (or Ctrl+S)
- Choose a location and filename
- The file extension is added automatically based on language

### Can I open programs I saved earlier?

Yes! **File → Open** (or Ctrl+O), then navigate to your saved file. Time Warp IDE detects the language automatically.

### The turtle isn't drawing anything!

**Check**:
1. Is **View → Turtle Canvas** enabled?
2. Is your code Logo or using Logo commands?
3. Is the pen down? (`PENDOWN` or `PD`)
4. Are you using valid turtle commands?

### How do I clear the turtle canvas?

Use `CLEARSCREEN` (or `CS`) in Logo, or select **Run → Reset Turtle** from the menu.

### Can I change the colors?

Yes! **Tools → Themes** offers multiple color schemes:
- Light (default bright)
- Dark (easy on eyes)
- Dracula (popular dark theme)
- Solarized (balanced contrast)
- Monokai (vibrant colors)
- Ocean, Forest, Sunset (themed options)

---

## Programming Questions

### What's the difference between PRINT and WRITE?

- **BASIC**: `PRINT` displays output
- **Pascal**: `write` outputs without newline, `writeln` outputs with newline
- **Logo**: `PRINT` displays lists and values

They're similar concepts in different languages.

### Why do I need quotes around text?

Quotes tell the computer "this is text, not a command." Without quotes:
```basic
PRINT Hello     ❌ Computer looks for variable named Hello
PRINT "Hello"   ✅ Computer displays the word Hello
```

### What's a variable?

A variable is like a box that stores information. You can:
- Put values in it
- Read values from it
- Change what's inside

```basic
LET AGE = 12       # Put 12 in box named AGE
PRINT AGE          # Read what's in the box
LET AGE = 13       # Change to 13
```

### What's the difference between = and :=?

- **=** means "is equal to" (comparison)
- **:=** means "assign this value" (assignment)

```pascal
if age = 12 then    { Is age equal to 12? }
age := 13;          { Set age to 13 }
```

BASIC uses `=` for both, Pascal uses `:=` for assignment.

### Why do I get "Syntax Error"?

Common causes:
- **Typo** - Wrong spelling of command
- **Missing quotes** - Text needs quotes
- **Wrong punctuation** - Missing semicolon, bracket, etc.
- **Wrong language** - Using command from different language

Read the error message carefully—it usually points to the problem.

### What does "Type Mismatch" mean?

You're trying to use a value in the wrong way:
```basic
LET NAME$ = 25      ❌ NAME$ is for text, not numbers
LET NAME$ = "Alex"  ✅ Text in text variable
LET AGE = 25        ✅ Number in number variable
```

### How do I add comments to my code?

Each language has different syntax:
```basic
REM This is a BASIC comment
```

```logo
; This is a Logo comment
```

```pascal
{ This is a Pascal comment }
(* or this *)
```

```c
// This is a C comment
/* or this */
```

```prolog
% This is a Prolog comment
```

---

## Turtle Graphics

### What is turtle graphics?

Imagine a turtle with a pen. You give it commands like "move forward 100 steps" or "turn right 90 degrees." As it moves with the pen down, it draws on screen.

### Why doesn't my turtle turn the right amount?

Angles are in **degrees**:
- 90° = right angle (square corner)
- 180° = half turn
- 360° = full circle

For polygons:
- Triangle: 120°
- Square: 90°
- Pentagon: 72°
- Hexagon: 60°

**Formula**: 360 ÷ number of sides

### Can I change the pen color?

Yes! `SETPENCOLOR` followed by a number (1-16):
```logo
SETPENCOLOR 1    ; Red
SETPENCOLOR 2    ; Green
SETPENCOLOR 3    ; Blue
```

### How do I make the pen thicker?

```logo
PENSIZE 5        ; Thicker pen
PENSIZE 1        ; Thin pen
```

### The turtle is off screen!

```logo
HOME             ; Return to center
CLEARSCREEN      ; Clear and reset (CS)
```

---

## Advanced Usage

### Can I use Time Warp IDE for real projects?

Yes! While designed for education, Time Warp IDE is fully functional. Many users create actual programs with it.

### Can I connect to hardware (Arduino, Raspberry Pi)?

Yes! Time Warp IDE includes experimental IoT features for:
- Arduino (via serial)
- Raspberry Pi GPIO
- Various sensors

See [Technical Reference](../developer/01-technical-reference.md) for details.

### Can I add my own language?

If you're a developer, yes! See [Developer Guide](../developer/00-developer-guide.md) for adding language executors.

### Is there a command-line version?

Yes! The Python implementation includes a CLI interpreter:
```bash
python -m time_warp.cli program.bas
```

### Can I use Time Warp IDE with version control (Git)?

Yes! Program files are plain text, perfect for Git. Your `.gitignore` should include:
```
.venv/
__pycache__/
*.pyc
.coverage
```

### How do I report a bug?

1. Check [existing issues](https://github.com/honey-badger-org/Time_Warp/issues)
2. If not found, create a new issue with:
   - What you were doing
   - What you expected
   - What actually happened
   - Your OS and Time Warp IDE version

---

## Educational Use

### Can I use this in my classroom?

Yes! Time Warp IDE is perfect for education. See the [Teacher's Guide](../teacher/00-overview.md) for:
- Lesson plans
- Curriculum alignment
- Assessment tools
- Classroom strategies

### Does it align with educational standards?

Yes! Time Warp IDE supports:
- CSTA K-12 Computer Science Standards
- ISTE Standards for Students
- Common Core Math (through programming)

### Can students work together?

Yes! Time Warp IDE supports:
- Pair programming (two students, one computer)
- Collaborative projects
- Code sharing and review

### How do I assess student work?

The Teacher's Guide includes:
- Project rubrics
- Coding assessments
- Portfolio guidelines
- Skills checklists

### Are there printable materials?

Documentation can be converted to PDF:
```bash
pandoc docs-new/student/00-workbook.md -o workbook.pdf
```

---

## Troubleshooting

### Time Warp IDE won't start

**Linux/macOS**:
- Check CPU requirements (SSSE3, SSE4.1 support)
- Try Python version if binary fails
- Check error logs: `~/.Time_Warp/logs/`

**Windows**:
- Install Visual C++ Redistributables
- Run as Administrator (first time only)
- Check antivirus isn't blocking

### "Illegal instruction" error

Your CPU lacks required extensions. Solutions:
1. Update system (may include microcode updates)
2. Run on physical hardware (not old VM)
3. Use Python version instead of compiled binary

### Program runs slowly

- Large turtle graphics can be slow
- Try reducing complexity
- Use release build (not debug)
- Close other applications

### Can't save files

Check:
- Do you have write permissions?
- Is disk full?
- Is path valid?
- Is file locked by another program?

### Lost my work!

Check:
- **Recent Files** menu (File → Recent)
- **Temp folder**: `~/.Time_Warp/temp/`
- **Autosaves** (if enabled): `~/.Time_Warp/autosave/`

### Theme doesn't change

- Restart Time Warp IDE after changing themes
- Check `~/.Time_Warp/config.json` for settings

---

## Installation Issues

### "Python not found"

**Windows**:
1. Install from [python.org](https://python.org/downloads/)
2. Check "Add Python to PATH" during install
3. Restart terminal/IDE

**Linux**:
```bash
sudo apt install python3 python3-pip  # Debian/Ubuntu
sudo pacman -S python python-pip      # Arch
```

### "Module not found: PySide6"

```bash
pip install PySide6
# or
pip install -e "./platforms/python[dev]"
```

### Permission denied (Linux)

```bash
chmod +x time-warp-ide
```

### macOS Gatekeeper blocks app

Right-click → Open (first time only)

Or:
```bash
xattr -d com.apple.quarantine TimeWarpIDE.app
```

---

## Performance

### How much RAM do I need?

- **Minimum**: 256 MB
- **Recommended**: 512 MB
- **Comfortable**: 1 GB+

### How much disk space?

- **Installation**: ~50 MB
- **With examples**: ~100 MB
- **With documentation**: ~150 MB

### Does it work on old computers?

Yes! Time Warp IDE runs on modest hardware. Requirements:
- 1 GHz CPU (with SSSE3/SSE4.1)
- 256 MB RAM
- 50 MB disk space

Older systems may need Python version instead of binary.

---

## Getting Help

### Where can I ask questions?

- **GitHub Discussions**: [Community Forum](https://github.com/honey-badger-org/Time_Warp/discussions)
- **Issues**: [Bug Reports](https://github.com/honey-badger-org/Time_Warp/issues)
- **Documentation**: You're reading it!

### Is there a mailing list?

Check the [GitHub Discussions](https://github.com/honey-badger-org/Time_Warp/discussions) for community updates.

### Can I get commercial support?

Time Warp IDE is community-supported. For commercial needs, consider:
- Hiring a consultant familiar with the project
- Sponsoring feature development
- Contributing to the project

### How can I contribute?

See [Developer Guide](../developer/00-developer-guide.md) for:
- Code contributions
- Documentation improvements
- Example programs
- Bug reports
- Feature suggestions

---

## Still Have Questions?

- **Search**: [Documentation](../INDEX.md)
- **Examples**: `examples/` folder
- **Community**: [Discussions](https://github.com/honey-badger-org/Time_Warp/discussions)
- **Issues**: [GitHub Issues](https://github.com/honey-badger-org/Time_Warp/issues)

---

*FAQ v5.0.0 - December 2025*
