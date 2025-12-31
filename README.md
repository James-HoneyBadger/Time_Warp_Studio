# Time Warp IDE

**An educational programming environment for learning multiple languages with integrated turtle graphics and game development.**

Time Warp IDE is a retro-inspired, modern IDE designed to teach programming concepts across multiple languages: **BASIC**, **PILOT**, **Logo**, **Pascal**, **Prolog**, **Forth**, and **C**. It features real-time graphics, immediate-mode REPL, comprehensive examples, and an intuitive interface.

## Quick Links

- **[User Guide](docs/user-guide/README.md)** - Getting started, basic usage, features
- **[Programming Tutorials](docs/tutorials/README.md)** - Language-specific tutorials
- **[Technical Reference](docs/technical/README.md)** - Architecture, API, advanced topics
- **[Quick Start](QUICKSTART.md)** - Launch in 60 seconds
- **[Code of Conduct](CODE_OF_CONDUCT.md)** - Community standards and reporting

## What You Can Do

### 🎨 Create Graphics
```logo
REPEAT 4 [FORWARD 100 RIGHT 90]  ; Draw a square
```

### 🔢 Learn Programming Fundamentals
```basic
PRINT "What's your name?"
INPUT NAME$
PRINT "Hello, " + NAME$ + "!"
```

### 🤖 Build Interactive Programs
```pilot
MATCH: ACCEPT X
  C: "STOP" → JUMP END
  C: ELSE → PRINT "Continue"
  J: MATCH
END:
```

### 🎮 Develop Games
```forth
: DRAW-PLAYER PEN-DOWN 10 FORWARD PEN-UP ;
: GAME CLEAR 0 MOVE-X ! DRAW-PLAYER ;
```

## Key Features

| Feature | Status |
|---------|--------|
| **7 Programming Languages** | Full |
| **Turtle Graphics** | Full featured |
| **Code Editor** | Tabs, snippets, themes |
| **Immediate Mode (REPL)** | Yes |
| **Variable Inspector** | Yes |
| **Debugging Tools** | Breakpoints, debug panel |
| **Graphics Modes** | Multiple |
| **Themes** | 23 themes |
| **Educational Examples** | 50+ examples |
| **Performance** | Excellent |

## Getting Started

### Installation & Launch (Python - Recommended)

**Easiest Way - Automated Launch Script:**

```bash
# Clone and enter repository
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio

# Linux/macOS
./launch_ide.sh

# Windows
launch_ide.bat
```

The script automatically:
- Creates a virtual environment
- Installs all dependencies
- Launches the IDE

**Manual Setup (Alternative):**

```bash
cd Platforms/Python
python3 -m venv .venv
source .venv/bin/activate  # Windows: .venv\Scripts\activate
pip install -r requirements.txt
python time_warp_ide.py
```

See [LAUNCHING.md](LAUNCHING.md) for detailed instructions and troubleshooting.

### Quick Examples

**1. Hello World in BASIC**
```basic
PRINT "Hello, World!"
```

**2. Draw a Circle in Logo**
```logo
REPEAT 360 [FORWARD 1 RIGHT 1]
```

**3. Guessing Game in PILOT**
```pilot
ACCEPT X(N)
MATCH: ACCEPT Y(N)
  C: Y = X → PRINT "Correct!"
  C: ELSE → JUMP ACCEPT
JUMP ACCEPT
```

## Project Structure

```
Time_Warp_Studio/
├── Platforms/
│   └── Python/          ← Main implementation (PySide6)
├── Examples/            ← 50+ sample programs
├── docs/                ← Comprehensive documentation
├── Scripts/             ← Build and utility scripts
└── README.md            ← This file
```

See [STRUCTURE.md](STRUCTURE.md) for detailed breakdown.

## Documentation Structure

**For Users:**
- [User Guide](docs/user-guide/README.md) - IDE features, menus, panels
- [Tutorials](docs/tutorials/README.md) - Learn each language
- [Examples](Examples/) - Ready-to-run sample programs

**For Developers:**
- [Technical Reference](docs/technical/README.md) - Architecture, components
- [Building Guide](INSTALL_NATIVE.md) - Native builds
- [API Documentation](docs/technical/api.md) - Interpreter interfaces

## Supported Languages

| Language | Status | Features | Examples |
|----------|--------|----------|----------|
| **BASIC** | ✅ Full | Variables, loops, functions | 11 examples |
| **PILOT** | ✅ Full | Pattern matching, branching | 9 examples |
| **Logo** | ✅ Full | Turtle graphics, recursion | 9 examples |
| **Pascal** | ✅ Experimental | Procedures, arrays | 9 examples |
| **Prolog** | ✅ Experimental | Logic, rules, facts | 5 examples |
| **Forth** | ✅ Experimental | Stack-based, extensible | 5 examples |
| **C** | ✅ Experimental | Functions, arrays | 8 examples |

## Requirements

**Requirements**
- Python 3.8+
- PySide6
- Pillow
- (Optional: pyfirmata, RPi.GPIO for hardware)

## IDE Features

### Editor & Environment
- 📝 **Code Editor** with syntax highlighting and code snippets
- 🎨 **Graphics Canvas** with real-time rendering
- 🔧 **Immediate Mode** (REPL) for quick testing
- 📊 **Variable Inspector** to track program state
- 🎯 **Debug Panel** with breakpoints and step execution
- 🌈 **8 Themes** including Dracula, Monokai, Solarized

### Advanced Features
- 🎵 **Sound Effects** and music playback
- 📱 **Gamepad Support** for interactive programs
- 🔍 **Error Explorer** with detailed messages
- 💾 **File Management** with recent files
- 🎨 **Graphical Output Modes** for different screen styles

### Programming Capabilities
- **7 Languages** in one environment
- **Turtle Graphics** with full color support
- **Recursive Functions** and advanced language features
- **File I/O** operations
- **Hardware Integration** (Raspberry Pi, Arduino)
- **Immediate Execution** with REPL
- **Comprehensive Error Messages** with emoji prefixes

## Learning Path

1. **Start with Logo** - Visual, immediate feedback
2. **Move to BASIC** - Familiar, structured language
3. **Explore PILOT** - Pattern matching, AI concepts
4. **Experiment with others** - Pascal, Prolog, Forth, C

See [Tutorials](docs/tutorials/README.md) for language-specific learning guides.

## Examples

The `Examples/` directory contains 50+ ready-to-run programs demonstrating each language. Open any file in the IDE and click Run:

- **Basic**: Hello world, loops, arrays, games
- **Logo**: Shapes, patterns, fractals, recursion
- **PILOT**: Quiz games, pattern matching, interactive programs
- Plus examples for Pascal, Prolog, Forth, and C

## Contributing

Contributions are welcome! Check the [Technical Reference](docs/technical/README.md) for architecture details and development guidelines.

## License

See [LICENSE](LICENSE) file.

## Version History

**5.1.0** (December 2025)
- Full Python implementation with all features
- 50+ examples for all languages
- 23 themes, full IDE features

## Support & Resources

- 📚 [Full Documentation](docs/)
- 💬 [Examples](Examples/)
- 🔧 [Technical Guide](docs/technical/)
- 🎓 [Tutorials](docs/tutorials/)

---

**Time Warp IDE** - Where retro programming meets modern design.
