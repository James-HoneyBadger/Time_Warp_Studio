# Time Warp IDE

**The Ultimate Educational Programming Environment**

Time Warp IDE is a comprehensive educational platform designed to teach programming through **six classic languages**: BASIC, PILOT, Logo, Pascal, Prolog, and C. Learn multiple programming paradigms in one unified environment.

## 🌟 Features

- **Six Languages**: Master BASIC, PILOT, Logo, Pascal, Prolog, and C in one IDE
- **Turtle Graphics**: Built-in visual programming with Logo-style graphics
- **Educational Focus**: Designed for students and classrooms with comprehensive materials
- **Multi-Platform**: Available for Linux, macOS, and Windows
- **Clear Feedback**: Helpful error messages and immediate results
- **IoT Ready**: Connect to Arduino, Raspberry Pi, and other hardware
- **Cross-Platform**: Same experience on every operating system

## 🎓 For Everyone

**Students**: Start your programming journey with easy-to-learn languages and visual feedback
**Teachers**: Complete curriculum materials, lesson plans, and assessment tools included
**Users**: Professional IDE with syntax highlighting, file management, and themes
**Developers**: Open source, well-documented, and extensible architecture

## 🚀 Quick Start

### Linux

```bash
# Download and run Python version
git clone https://github.com/honey-badger-org/Time_Warp.git
cd Time_Warp
./run.sh
```

### macOS

```bash
# Using Homebrew
brew install honey-badger-org/tap/time-warp-ide
time-warp-ide
```

### Windows

Download `TimeWarpIDE-Setup.exe` from [Releases](https://github.com/honey-badger-org/Time_Warp/releases) and run the installer.

**Full installation guide**: [Quick Start](docs-new/installation/00-quickstart.md)

## 📚 Documentation

**New comprehensive documentation** in `docs-new/`:

### For Students
- **[Student Workbook](docs-new/student/00-workbook.md)** - Learn programming step-by-step

### For Teachers
- **[Teacher's Guide](docs-new/teacher/00-overview.md)** - Lesson plans, curriculum, assessments

### For Users
- **[User Manual](docs-new/user/00-user-manual.md)** - Complete guide to using Time Warp IDE
- **[Programming Guide](docs-new/user/01-programming-guide.md)** - All six languages explained

### For Developers
- **[Developer Guide](docs-new/developer/00-developer-guide.md)** - Architecture and contributing

**Start here**: [Documentation Index](docs-new/INDEX.md)

## 💻 Platforms

Active / historical implementations now consolidated:

| Platform | Location | Status | Description |
|----------|----------|--------|-------------|
| **Python** | `Platforms/Python/` | ✅ Primary | Full-featured PySide6 implementation (BASIC, PILOT, Logo, Pascal, Prolog, C experimental) |
| **Browser** | `Platforms/Browser/` | 🧪 Experimental | HTML/JS interpreter prototype |
| **DOS** | `Platforms/DOS/` | 🧪 Experimental | Text-mode educational port |
| **Windows 2000** | `Platforms/Windows2000/` | 🧪 Historical | Vintage native Win2000 IDE build |

Removed legacy platforms: Rust, Go, Amiga, Haiku, Apple, OS/2.

## 📂 Project Structure

```
Time_Warp/
├── Platforms/         # Platform implementations (Python primary)
│   ├── Python/        # Active PySide6 IDE & interpreters
│   ├── Browser/       # Web prototype
│   ├── DOS/           # Text-mode prototype
│   └── Windows2000/   # Historical native build
├── Docs/              # Comprehensive documentation (users, dev, install)
├── Examples/          # Sample programs (BASIC, Logo, PILOT, etc.)
├── Core_Spec/         # Language specifications
├── Scripts/           # Utility and build scripts
├── Tests/             # Test suite (pytest)
└── Packaging/         # Packaging resources
```

## 🎯 Why Time Warp IDE?

**Educational Excellence**: Every feature designed with learning in mind. Clear error messages, immediate feedback, and visual programming make concepts tangible.

**Historical Perspective**: Understand how programming evolved. Learn why modern languages work the way they do by experiencing their ancestors.

**Multiple Paradigms**:
- **Procedural** (BASIC, C)
- **Educational** (PILOT)
- **Visual** (Logo)
- **Structured** (Pascal)
- **Declarative** (Prolog)

**One Environment**: Switch between languages seamlessly. No need to learn different tools.

## 🎨 What Can You Build?

- **Interactive Games**: Guessing games, quizzes, text adventures
- **Visual Art**: Geometric patterns, spirals, fractals with turtle graphics
- **Educational Tools**: Math practice, vocabulary drills, tutorials
- **Logic Puzzles**: AI concepts with Prolog
- **System Programs**: Low-level concepts with C
- **And more**: Your imagination is the limit!

## 💡 Example: Hello World in All Six Languages

**BASIC**:
```basic
10 PRINT "Hello, World!"
20 END
```

**PILOT**:
```pilot
T:Hello, World!
```

**Logo**:
```logo
PRINT [Hello, World!]
```

**Pascal**:
```pascal
program HelloWorld;
begin
  writeln('Hello, World!');
end.
```

**Prolog**:
```prolog
:- write('Hello, World!'), nl.
```

**C**:
```c
#include <stdio.h>
int main() {
    printf("Hello, World!\n");
    return 0;
}
```

## 🏫 Perfect for Education

Time Warp IDE supports:
- **CSTA K-12 Computer Science Standards**
- **ISTE Standards for Students**
- **Common Core Math (through programming)**
- **21st Century Skills** (problem-solving, creativity)

Includes:
- Ready-to-use lesson plans
- Student workbook with progressive exercises
- Assessment rubrics and tools
- Classroom management strategies
- Differentiation for all skill levels

## 🛠️ Development

### Python Version (Primary)

```bash
cd platforms/python
python3 -m venv .venv
source .venv/bin/activate
pip install -e .[dev]
python time_warp_ide.py
```

### Rust Version (Native)

```bash
cd platforms/rust
cargo run --release
```

### Testing

```bash
# Python
pytest platforms/python/tests

# Rust
cargo test
```

## 🤝 Contributing

We welcome contributions! See [Developer Guide](docs-new/developer/00-developer-guide.md) for:
- Architecture overview
- Code organization
- Testing requirements
- Submission guidelines

**Ways to Contribute**:
- Add new language features
- Improve documentation
- Create example programs
- Report bugs
- Suggest enhancements
- Translate documentation

## 🌐 Community

- **Discussions**: [GitHub Discussions](https://github.com/honey-badger-org/Time_Warp/discussions)
- **Issues**: [Bug Reports & Features](https://github.com/honey-badger-org/Time_Warp/issues)
- **Releases**: [Download Latest](https://github.com/honey-badger-org/Time_Warp/releases)

## 📄 License

MIT License - see [LICENSE](LICENSE) for details.

Open source software created with ❤️ for education.

---

**Get Started**: [Installation Guide](docs-new/installation/00-quickstart.md) | [User Manual](docs-new/user/00-user-manual.md) | [Examples](examples/)

*Time Warp IDE - Making programming education accessible and fun since 2024*
