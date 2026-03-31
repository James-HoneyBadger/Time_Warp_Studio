# Time Warp Studio Documentation

Welcome to the complete Time Warp Studio documentation! This is your central hub for learning to use the IDE, mastering programming languages, and building amazing projects.

> **New to Time Warp Studio?** Start with the [Getting Started Guide](guides/01-getting-started.md)

---

## 🚀 Getting Started

Start here if you're new to Time Warp Studio:

- **[Installation & Setup](guides/01-getting-started.md)** - Install, configure, and launch the IDE
- **[IDE Basics](guides/02-ide-basics.md)** - Learn the interface, menus, and keyboard shortcuts
- **[Installation](INSTALLATION.md)** - Detailed installation instructions for all platforms

---

## 📚 Programming Languages

### Classic Educational

- **[BASIC Tutorial](tutorials/basic.md)** - Classic BASIC with variables, loops, conditionals, and subroutines
- **[PILOT Tutorial](tutorials/pilot.md)** - Interactive teaching language designed for computer-based instruction
- **[Logo Tutorial](tutorials/logo.md)** - Turtle graphics and visual programming for geometric learning

### Systems & Structured Languages

- **[C Reference](tutorials/c.md)** - Systems programming and low-level concepts
- **[Pascal Guide](tutorials/pascal.md)** - Structured programming with strong typing
- **[Fortran Tutorial](tutorials/fortran.md)** - Scientific computing and numerical methods
- **[Assembly Guide](tutorials/assembly.md)** - x86 low-level programming

### Functional & Declarative Languages

- **[Prolog Guide](tutorials/prolog.md)** - Logic programming with facts, rules, and unification
- **[Haskell Tutorial](tutorials/haskell.md)** - Pure functional programming with type system
- **[Scheme Tutorial](tutorials/scheme.md)** - Lisp-family functional programming

### Modern Scripting Languages

- **[Python Tutorial](tutorials/python.md)** - Modern sandboxed Python execution
- **[JavaScript Tutorial](tutorials/javascript.md)** - Web-era scripting
- **[Lua Tutorial](tutorials/lua.md)** - Lightweight embeddable scripting
- **[REXX Tutorial](tutorials/rexx.md)** - IBM mainframe scripting language

### Stack & Concatenative

- **[Forth Guide](tutorials/forth.md)** - Stack-based programming and low-level operations

### Object-Oriented

- **[Smalltalk Tutorial](tutorials/smalltalk.md)** - The original object-oriented language

### Array & Mathematical

- **[APL Tutorial](tutorials/apl.md)** - Array programming with symbolic notation

### Business & Mainframe Languages

- **[COBOL Tutorial](tutorials/cobol.md)** - Business data processing language
- **[JCL Tutorial](tutorials/jcl.md)** - IBM Job Control Language
- **[CICS Tutorial](tutorials/cics.md)** - IBM CICS transaction processing
- **[SQR Tutorial](tutorials/sqr.md)** - Oracle/PeopleSoft report generation

### Data Languages

- **[SQL Tutorial](tutorials/sql.md)** - Relational database queries and manipulation

### Event-Driven

- **[HyperTalk Tutorial](tutorials/hypertalk.md)** - Apple HyperCard scripting language

### Esoteric Languages

- **[Brainfuck Tutorial](tutorials/brainfuck.md)** - Esoteric Turing-complete language

---

## 🎨 Feature Guides

Deep dives into specific IDE features:

- **[Debugger Guide](DEBUGGER_GUIDE.md)** - Step-through debugging with timeline recording and rewind
- **[Turtle Graphics](guides/04-turtle-graphics.md)** - Master Logo turtle graphics, colors, and animations
- **[Settings & Themes](guides/06-settings.md)** - Customize appearance with 28 themes or create your own
- **[Keyboard Shortcuts](guides/07-shortcuts.md)** - Speed up your workflow with essential shortcuts
- **[Lesson System](guides/03-lessons.md)** - Use structured lessons with checkpoints and verification

---

## 📖 Reference & Support

### Quick Reference
- **[Keyboard Shortcuts](guides/07-shortcuts.md)** - All available shortcuts and their actions
- **[FAQ](reference/faq.md)** - 70+ frequently asked questions with answers

### Troubleshooting
- **[Troubleshooting Guide](guides/08-troubleshooting.md)** - Solutions for common issues
- **[Architecture Guide](../ARCHITECTURE.md)** - Project structure and design decisions

---

## 🎯 Learning Paths

### For Complete Beginners

**Recommended Path:**
1. [Getting Started Guide](guides/01-getting-started.md) - Setup and basic navigation
2. [IDE Basics](guides/02-ide-basics.md) - Understand the interface
3. [BASIC Tutorial](tutorials/basic.md) - Learn fundamental programming
4. [Examples Browser](guides/01-getting-started.md#examples) - Run and modify examples

**First Program to Try:**
```basic
PRINT "Hello, World!"
```

### For Learning Graphics

**Recommended Path:**
1. [Logo Tutorial](tutorials/logo.md) - Learn Logo basics
2. [Turtle Graphics Guide](guides/04-turtle-graphics.md) - Advanced turtle techniques
3. Open **File → Examples → logo/** to see 5 examples
4. Try examples like `02_squares.logo`, `05_trees.logo`

**First Turtle Program:**
```logo
REPEAT 4 [FORWARD 100 RIGHT 90]
```

### For Teaching/Classroom Use

**Recommended Resources:**
1. [Classroom Mode Guide](guides/06-settings.md) - Presentation and assignment features
2. [Lesson System Guide](guides/03-lessons.md) - Create structured lessons
3. [Examples Browser](guides/01-getting-started.md#examples) - 93 ready-made programs
4. See [ARCHITECTURE.md](../ARCHITECTURE.md) for extending features

### For Advanced Users

**Topics:**
- Custom themes: [Settings & Themes](guides/06-settings.md)
- Adding languages: [ARCHITECTURE.md](../ARCHITECTURE.md#adding-a-new-language)
- Contributing: [README.md](../README.md#contributing)
- Development: [ARCHITECTURE.md](../ARCHITECTURE.md#development-guide)

---

## 📁 Examples

Browse 93 example programs organized by language:

| Directory | Count | Level | Purpose |
|-----------|-------|-------|---------|
| `basic/` | 5 | Beginner | Learn BASIC fundamentals |
| `logo/` | 5 | Beginner-Intermediate | Turtle graphics and recursion |
| `pilot/` | 3 | Beginner-Intermediate | Interactive instruction |
| `assembly/` | 3 | Advanced | x86 low-level programming |
| `c/` | 5 | Intermediate | Systems programming |
| `cobol/` | 5 | Intermediate | Business data processing |
| `sqr/` | 4 | Intermediate | Report generation |
| `fortran/` | 3 | Intermediate | Scientific computing |
| `haskell/` | 3 | Advanced | Functional programming |
| `javascript/` | 4 | Intermediate | Scripting |
| `pascal/` | 4 | Intermediate | Structured programming |
| `prolog/` | 4 | Advanced | Logic programming |
| `apl/` | 3 | Advanced | Array programming |
| `hypertalk/` | 3 | Intermediate | Event-driven scripting |
| `brainfuck/` | 3 | Advanced | Esoteric computing |
| `cics/` | 4 | Advanced | Mainframe transactions |
| `forth/` | 3 | Advanced | Stack-based programming |
| `jcl/` | 5 | Intermediate | Mainframe job control |
| `lua/` | 4 | Beginner-Intermediate | Scripting |
| `python/` | 5 | Beginner-Intermediate | Modern scripting |
| `rexx/` | 3 | Intermediate | Mainframe scripting |
| `scheme/` | 4 | Intermediate-Advanced | Functional/Lisp |
| `smalltalk/` | 3 | Intermediate | Object-oriented |
| `sql/` | 4 | Intermediate | Database queries |
| `demo/` | 2 | All | Cross-language showcases |

**How to use examples:**
1. Press **Ctrl+E** or **File → Examples**
2. Browse by language and difficulty
3. Click **Load** to open in editor
4. Press **Ctrl+R** to run

---

## 🔧 Common Tasks

### Run a program
1. Open file (Ctrl+O) or create new (Ctrl+N)
2. Type or paste code
3. Press Ctrl+R or click Run

### Change language
- Use Language dropdown (top right of editor)
- Or File → New → [Language]

### Customize theme
- File → Settings → Themes
- 28 built-in themes + custom theme editor

### Use autosave
- Enabled by default
- Auto-saves every 30 seconds to `~/.time_warp/`
- Keep up to 20 versions per file

### Follow a lesson
- File → Lessons
- Select lesson from catalog
- Complete checkpoints with auto-verification

---

## ❓ Need Help?

- **Can't find answer?** Check [FAQ](reference/faq.md)
- **Having issues?** See [Troubleshooting](guides/08-troubleshooting.md)
- **Want to extend?** Read [ARCHITECTURE.md](../ARCHITECTURE.md)
- **Report bug?** Open [GitHub Issue](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues)

---

## 📚 Complete File Index

```
docs/
├── INDEX.md (you are here)
├── INSTALLATION.md
├── DEBUGGER_GUIDE.md
├── LANGUAGE_GUIDE.md
├── USER_GUIDE.md
├── guides/
│   ├── 01-getting-started.md
│   ├── 02-ide-basics.md
│   ├── 03-lessons.md
│   ├── 04-turtle-graphics.md
│   ├── 05-debugger.md
│   ├── 06-settings.md
│   ├── 07-shortcuts.md
│   └── 08-troubleshooting.md
├── tutorials/
│   ├── apl.md, assembly.md, basic.md, brainfuck.md
│   ├── c.md, cics.md, cobol.md, forth.md
│   ├── fortran.md, haskell.md, hypertalk.md
│   ├── javascript.md, jcl.md, logo.md, lua.md
│   ├── pascal.md, pilot.md, prolog.md, python.md
│   ├── rexx.md, scheme.md, smalltalk.md
│   └── sql.md, sqr.md
└── reference/
    └── faq.md
```

---

## 🛠️ Examples and Tutorials Index

Explore example programs and tutorials for all supported languages:

- **[APL Examples](../Examples/apl/)** - Matrix operations, statistics, and more
- **[Assembly Examples](../Examples/assembly/)** - String operations, showcases
- **[BASIC Examples](../Examples/basic/)** - Adventure games, budget trackers
- **[Brainfuck Examples](../Examples/brainfuck/)** - Fibonacci, Hello World
- **[C Examples](../Examples/c/)** - Sorting algorithms, RPN calculator
- **[COBOL Examples](../Examples/cobol/)** - Payroll processing, inventory control
- **[CICS Examples](../Examples/cics/)** - ATM transactions, order entry
- **[Forth Examples](../Examples/forth/)** - Stack-based programming
- **[Fortran Examples](../Examples/fortran/)** - Numerical methods, scientific computing
- **[Haskell Examples](../Examples/haskell/)** - Functional programming
- **[Hypertalk Examples](../Examples/hypertalk/)** - HyperCard scripting
- **[JavaScript Examples](../Examples/javascript/)** - Web scripting basics
- **[JCL Examples](../Examples/jcl/)** - Job control language
- **[Logo Examples](../Examples/logo/)** - Turtle graphics
- **[Lua Examples](../Examples/lua/)** - Lightweight scripting
- **[Pascal Examples](../Examples/pascal/)** - Structured programming
- **[Pilot Examples](../Examples/pilot/)** - Instructional programming
- **[Prolog Examples](../Examples/prolog/)** - Logic programming
- **[Python Examples](../Examples/python/)** - General-purpose scripting
- **[REXX Examples](../Examples/rexx/)** - Scripting for IBM systems
- **[Scheme Examples](../Examples/scheme/)** - Lisp-family functional programming
- **[Smalltalk Examples](../Examples/smalltalk/)** - Object-oriented programming
- **[SQL Examples](../Examples/sql/)** - Database queries
- **[SQR Examples](../Examples/sqr/)** - Reporting language

---

## 📖 Unified Documentation Index

### Core Guides

- **[Getting Started](guides/01-getting-started.md)** - Install and launch the IDE
- **[IDE Basics](guides/02-ide-basics.md)** - Interface and shortcuts
- **[Lessons](guides/03-lessons.md)** - Learn programming step-by-step
- **[Turtle Graphics](guides/04-turtle-graphics.md)** - Visual programming
- **[Debugger Guide](guides/05-debugger.md)** - Debugging tools
- **[Settings](guides/06-settings.md)** - Customize your IDE
- **[Shortcuts](guides/07-shortcuts.md)** - Keyboard shortcuts
- **[Troubleshooting](guides/08-troubleshooting.md)** - Solve common issues

### Tutorials and Examples

- **[Examples Catalog](../Examples/CATALOG.md)** - Explore example programs
- **[Programming Tutorials](#📚-programming-languages)** - Learn supported languages

### Reference

- **[FAQ](reference/faq.md)** - Frequently asked questions
- **[Installation](INSTALLATION.md)** - Detailed setup instructions

---

**Last Updated:** March 2026  
**Version:** 9.0.0  
**License:** MIT

## How to Use This Documentation

1. **Read actively**: Type the examples yourself
2. **Experiment**: Modify code to understand it better
3. **Practice**: Create your own programs
4. **Reference**: Come back to guides when needed
5. **Share**: Help others learn

## Updates and Contributions

This documentation is maintained alongside Time Warp Studio development. If you find:

- **Errors**: Report them so we can fix them
- **Unclear sections**: Let us know what needs clarification
- **Missing topics**: Suggest new documentation
- **Helpful tips**: Share them with the community

## Quick Reference

### Common File Extensions

| Language | Extension | Example |
|----------|-----------|---------|
| BASIC | `.bas` | `program.bas` |
| PILOT | `.pilot` | `lesson.pilot` |
| Logo | `.logo` | `graphics.logo` |
| Python | `.py` | `script.py` |
| C | `.c` | `program.c` |
| Pascal | `.pas` | `program.pas` |
| Prolog | `.pl`, `.pro` | `logic.pl` |

### Keyboard Shortcuts

- **Ctrl+R** - Run program
- **Ctrl+S** - Save file
- **Ctrl+L** - Clear output
- **Ctrl+T** - Open turtle graphics
- **F1** - Show help

### IDE Menu Structure

- **File** - Open, save, new programs
- **Edit** - Cut, copy, paste, undo/redo
- **View** - Toggle panels and windows
- **Language** - Select programming language
- **Run** - Execute program
- **Help** - Access documentation

## Navigation Tips

- Use browser search (Ctrl+F) to find topics
- Click links to navigate between pages
- Use breadcrumbs at the top to understand location
- Go to [README](../README.md) for project overview

## Version Information

- **Time Warp Studio** v9.0.0
- **Last Updated** March 2026

## Need More Help?

- Review the [Getting Started Guide](guides/01-getting-started.md)
- Check the [FAQ](reference/faq.md) for common questions
- Visit the [Troubleshooting Guide](guides/08-troubleshooting.md)
- Read the project [README](../README.md)

---

**Happy learning!** Choose a language tutorial and start exploring the world of programming.
