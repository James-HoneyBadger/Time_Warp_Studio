# Time Warp IDE

**🚀 The Universal Educational Programming Environment for TempleCode**

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](docs/LICENSE)
[![Version](https://img.shields.io/badge/Version-2.1.0-brightgreen.svg)](https://github.com/James-HoneyBadger/Time_Warp/releases/tag/v2.1.0)
[![Status](https://img.shields.io/badge/Status-Production%20Ready-success.svg)](docs/RELEASE_NOTES.md)
[![TempleCode](https://img.shields.io/badge/Language-TempleCode-purple.svg)](docs/TEMPLECODE_SPECIFICATION.md)
[![Platform](https://img.shields.io/badge/Platform-Cross%20Platform-orange.svg)](#🚀-implementations)

> **📚 [Complete Documentation System Available](docs/DOCUMENTATION_INDEX.md)** - Navigate our comprehensive guides for students, educators, developers, and administrators.

---

## 🌟 Welcome to Time Warp IDE

**Time Warp IDE** is the world's first unified educational programming environment that implements **TempleCode** — a revolutionary language that seamlessly blends the best features of BASIC, PILOT, and Logo into a single, coherent programming experience.

Whether you're a student taking your first steps into programming, an educator crafting the perfect curriculum, or a developer exploring retro computing, Time Warp IDE provides the tools, documentation, and community support to make programming accessible, engaging, and fun.

### 🎯 What Makes Time Warp Special?

- **🔄 Unified Language**: Write BASIC, PILOT, and Logo code in the same program
- **🌈 Multiple Implementations**: Choose from Rust, Python, Web, DOS, Windows, or Apple versions
- **🎓 Educational Focus**: Built specifically for learning with comprehensive curriculum materials
- **🐢 Turtle Graphics**: Full Logo-compatible graphics with modern enhancements
- **🕹️ Interactive Features**: Real-time input, pattern matching, and game development support
- **📚 Rich Documentation**: Complete guides for students, teachers, and developers

---

## � What is TempleCode?

TempleCode is a revolutionary programming language that unifies three classic educational languages into one cohesive experience:

### 🔢 BASIC Features
```basic
10 PRINT "Welcome to Time Warp!"
20 LET X = 42
30 FOR I = 1 TO 5
40   PRINT "Count: "; I
50 NEXT I
60 IF X > 40 THEN PRINT "X is large"
```

### 🎭 PILOT Features  
```pilot
T:What is your name?
A:$NAME
T:Hello, $NAME! Nice to meet you.
M:yes,y: *YES
M:no,n: *NO
T:Do you like programming? (yes/no)
A:$ANSWER
Y: T:Great! Let's start coding.
N: T:No worries, we'll take it slow.
```

### 🐢 Logo Features
```logo
TO FLOWER :SIZE
  REPEAT 8 [
    SETCOLOR (RANDOM 256) (RANDOM 256) (RANDOM 256)
    REPEAT 36 [
      FORWARD :SIZE
      RIGHT 10
    ]
    RIGHT 45
  ]
END

FLOWER 50
```

### 🌟 Mixed Programming
```templecode
10 PRINT "Creating a colorful spiral"
20 LET SIZE = 100

T:Starting turtle graphics demo
SETCOLOR red
PENWIDTH 3

30 FOR I = 1 TO 10
40   FORWARD SIZE
50   RIGHT 36
60   LET SIZE = SIZE * 0.9
70 NEXT I

T:Spiral complete! Press any key to continue.
A:$CONTINUE
```

---

## 📦 Choose Your Implementation

### 🦀 **Rust Implementation** (Recommended for Production)
**The flagship native implementation with maximum performance and features**

- **🚀 Performance**: Compiled native executable with egui UI framework
- **✨ Features**: Full TempleCode support, async execution, PNG export, experimental compiler
- **🌍 Platforms**: Linux, macOS, Windows (x86_64, ARM64)
- **🎯 Best For**: Production use, advanced projects, performance-critical applications
- **📖 Documentation**: [Rust Implementation Guide](Time_Warp_Rust/README.md)

```bash
cd Time_Warp_Rust
cargo run --release
```

### 🐍 **Python Implementation** (Educational & Development)
**The most accessible implementation with comprehensive educational features**

- **🎓 Educational Focus**: Extensive test suite, clear code structure, easy to modify
- **🔧 Portability**: Pure Python with PySide6 GUI, runs anywhere Python runs
- **📚 Features**: Full TempleCode support, interactive debugging, code analysis tools
- **🌍 Platforms**: Cross-platform (Python 3.8+)
- **🎯 Best For**: Learning, teaching, curriculum development, experimentation
- **📖 Documentation**: [Python Implementation Guide](Time_Warp_Python/README.md)

```bash
cd Time_Warp_Python
python time_warp_ide.py
```

### 🌐 **Web Implementation** (Universal Access)
**Browser-based IDE accessible from any device with no installation required**

- **🌍 Universal Access**: Run directly in any modern web browser
- **📱 Cross-Device**: Works on desktop, tablet, and mobile devices
- **🔄 Real-Time**: Working turtle graphics, comprehensive debugging tools
- **☁️ Features**: Full TempleCode support, shareable programs, cloud-ready
- **🎯 Best For**: Classroom computers, BYOD environments, quick demonstrations
- **📖 Documentation**: [Web Implementation Guide](Time_Warp_Web/README.md)

```bash
cd Time_Warp_Web
python -m http.server 8080
# Open http://localhost:8080 in your browser
```

### 💾 **DOS Implementation** (Retro Computing)
**Authentic retro computing experience for historical systems and education**

- **🕰️ Historical Accuracy**: Single-file C89 interpreter for authentic DOS experience
- **💾 Minimal Requirements**: Runs on 8086+ with 512KB RAM, no external dependencies
- **🎮 Retro Gaming**: Perfect for DOSBox, vintage hardware, and retro programming
- **📟 Features**: Text-mode interface, full BASIC/PILOT/Logo support
- **🎯 Best For**: Computer history education, retro programming, minimal systems
- **📖 Documentation**: [DOS Implementation Guide](Time_Warp_DOS/README.md)

```bash
cd Time_Warp_DOS
# See README for OpenWatcom/DJGPP build instructions
dosbox -conf dosbox-timewarp.conf
```

### 🪟 **Windows Implementation** (Legacy Support)
**Native Windows implementation for legacy systems and educational institutions**

- **🏢 Enterprise Ready**: Native Windows executable with full OS integration
- **🔧 Legacy Support**: Compatible with Windows 95 through Windows 11
- **📊 Integration**: Windows-specific features, file associations, system integration
- **🎯 Best For**: Windows-only environments, legacy systems, enterprise deployment
- **📖 Documentation**: [Windows Implementation Guide](Time_Warp_Windows/README.md)

### 🍎 **Apple Implementation** (macOS/iOS Development)
**Native Apple ecosystem implementation with modern Swift/SwiftUI**

- **🍎 Native Experience**: Swift/SwiftUI implementation for macOS and iOS
- **🔄 Continuity**: Seamless experience across Mac, iPad, and iPhone
- **🎨 Modern Design**: Follows Apple Human Interface Guidelines
- **☁️ Integration**: iCloud sync, Handoff, and Apple ecosystem features
- **🎯 Best For**: Apple-centric classrooms, iOS development education, modern UX
- **📖 Documentation**: [Apple Implementation Guide](Time_Warp_Apple/README.md)

---

## 🏗️ Project Architecture

```text
🚀 Time Warp IDE Ecosystem
├─ � Core Implementations
│  ├─ 🦀 Rust Implementation (Native Performance)
│  ├─ 🐍 Python Implementation (Educational & Development)  
│  ├─ 🌐 Web Implementation (Browser-Based)
│  ├─ 💾 DOS Implementation (Retro Computing)
│  ├─ 🪟 Windows Implementation (Legacy Support)
│  └─ 🍎 Apple Implementation (macOS/iOS)
│
├─ 📚 Educational Resources
│  ├─ 👨‍🏫 Teacher Guide & Curriculum
│  ├─ 📖 Student Lesson Book  
│  ├─ 🔧 Technical Reference Manual
│  └─ 💡 Example Programs Library
│
├─ 🛠️ Development Ecosystem
│  ├─ 📋 Comprehensive Test Suites
│  ├─ 🔨 Build Tools & Scripts
│  ├─ 📊 Performance Benchmarks
│  └─ 🤝 Contribution Guidelines
│
└─ 🌍 Community & Support
   ├─ � Discussion Forums
   ├─ 🐛 Issue Tracking
   ├─ 📢 Release Notes
   └─ 🏆 Showcase Gallery
```

---

## ✨ Key Features

### Language Features

- ✅ **Unified TempleCode**: Mix BASIC, PILOT, and Logo in one program
- ✅ **Turtle Graphics**: Full Logo-compatible turtle with procedures, colors, and pen control
- ✅ **50+ Commands**: Complete command set verified and tested
- ✅ **Expression Evaluation**: Safe math expressions with operator precedence
- ✅ **Pattern Matching**: PILOT-style text matching with wildcards
- ✅ **User Procedures**: Define reusable procedures with parameters (`TO/END`)
- ✅ **Multi-line Loops**: `REPEAT` blocks with proper nesting
- ✅ **Color Support**: Named colors (red, blue, green, etc.), hex (#FF69B4), and RGB

### IDE Features

- 🎨 **Modern UI**: Clean, responsive interface with syntax highlighting
- 🐢 **Turtle Canvas**: Zoom/pan graphics canvas with coordinate system
- 🎨 **8 Themes**: Dracula, Monokai, Solarized Dark, Ocean, Spring, Sunset, Candy, Forest
- 📁 **File Management**: Open/save with recent files list
- ▶️ **Run Controls**: Execute, stop, clear output/canvas
- 📊 **Real-time Output**: Colored text output with emoji indicators
- 🔍 **Error Help**: Syntax error detection with helpful suggestions

### Educational Features

- 📚 **33+ Example Programs**: Organized by language style and difficulty
- 📖 **Complete Documentation**: Guides for students, teachers, and developers
- 🎓 **Lesson Plans**: 8-week curriculum included (Rust version)
- 💡 **Interactive Learning**: PILOT-style questions and pattern matching
- 🎮 **Game Development**: INKEY$ support for interactive programs

---

## 🚀 Quick Start Guide

### ⚡ Fastest Way to Start

```bash
# Clone the repository
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp

# Option 1: Install to your system (recommended)
./install-user.sh           # User installation (no sudo needed)
timewarp                    # Launch Time Warp IDE

# Option 2: System-wide installation
sudo ./install.sh           # Install for all users
timewarp                    # Launch from anywhere

# Option 3: Run without installing
./run.sh python             # Launch Python implementation directly
./run.sh rust --release     # Launch Rust implementation (optimized)
./run.sh web               # Start web server and open browser
```

**Installation Guides:**
- **Linux**: See [ARCH_INSTALL.md](ARCH_INSTALL.md) or [DEBIAN_INSTALL.md](DEBIAN_INSTALL.md)
- **After Install**: Check [YOUR_INSTALLATION.md](YOUR_INSTALLATION.md) for quick reference

### 🎓 For Educators
1. **Start with Web Implementation** - No installation required, works on any classroom computer
2. **Review Teacher Guide** - [`docs/TEACHER_GUIDE.md`](docs/TEACHER_GUIDE.md) contains complete curriculum
3. **Try Sample Lessons** - [`examples/curriculum/`](examples/curriculum/) has ready-to-use lessons
4. **Setup Student Accounts** - Each implementation supports multiple user profiles

### 👨‍💻 For Developers  
1. **Choose Implementation** - Rust for performance, Python for education, Web for accessibility
2. **Read Architecture Docs** - [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) explains system design
3. **Follow Contribution Guide** - [`docs/CONTRIBUTING.md`](docs/CONTRIBUTING.md) has coding standards
4. **Run Test Suites** - Each implementation has comprehensive automated tests

### 🎒 For Students
1. **Start with Student Guide** - [`docs/STUDENT_GUIDE.md`](docs/STUDENT_GUIDE.md) explains everything
2. **Try Interactive Lessons** - Built-in tutorials walk you through TempleCode
3. **Explore Examples** - [`examples/`](examples/) directory has programs to study and modify
4. **Join Community** - Share your creations and get help from other learners

---

## ✨ Feature Highlights

### 🎨 **Advanced Graphics & Visualization**
- **� Modern Turtle Graphics**: Logo-compatible with 256-color support, pen width control, and shape filling
- **📊 Data Visualization**: Built-in charting and graphing capabilities for STEM education
- **🎮 Game Development**: Sprite support, animation tools, and interactive game creation
- **🖼️ Export Options**: Save graphics as PNG, SVG, or print-ready formats

### 🧠 **Educational Intelligence**
- **💡 Smart Error Messages**: Context-aware suggestions that teach while debugging
- **📈 Progress Tracking**: Built-in analytics to monitor student learning progression
- **🎯 Adaptive Difficulty**: Dynamic content adjustment based on student skill level
- **🏆 Achievement System**: Gamified learning with badges and milestones

### 🔧 **Developer Tools & Features**
- **🔍 Integrated Debugger**: Step-through debugging with variable inspection
- **📝 Syntax Highlighting**: Full IDE experience with code completion and formatting
- **🧪 Unit Testing**: Built-in testing framework for student projects
- **📚 Documentation Generator**: Auto-generate docs from code comments

### 🌍 **Multi-Platform Excellence**
- **☁️ Cloud Integration**: Save and sync projects across devices and platforms
- **🌐 Web Standards**: Progressive Web App support for offline usage
- **📱 Mobile Support**: Touch-friendly interfaces for tablets and smartphones
- **♿ Accessibility**: Full screen reader support and keyboard navigation

---

## 📚 Documentation & Learning Resources

> **�️ [Complete Documentation Index](docs/DOCUMENTATION_INDEX.md)** - Navigate our comprehensive documentation system organized by audience and use case.

### 🎓 **Start Here - New Users & Educators**

| Document | Audience | Purpose | Reading Time |
|----------|----------|---------|--------------|
| **[🎯 User Guide](docs/USER_GUIDE.md)** | End Users | Complete usage manual for all platforms | 30 minutes |
| **[� Installation Guide](docs/INSTALLATION_GUIDE.md)** | IT/Setup | Detailed deployment procedures | 45 minutes |
| **[👨‍🏫 Teacher Guide](docs/TEACHER_GUIDE.md)** | Educators | Complete teaching framework | 60 minutes |

### � **Educational Resources**

| Resource | Audience | Content | Learning Path |
|----------|----------|---------|---------------|
| **[� Student Lesson Book](docs/STUDENT_LESSON_BOOK.md)** | Students | 24 progressive lessons across 5 skill levels | Structured curriculum |
| **[🏗️ Contributing Guide](docs/CONTRIBUTING.md)** | Community | Development and contribution framework | Open source participation |

### � **Technical Documentation**

| Document | Audience | Focus | Technical Depth |
|----------|----------|-------|-----------------|
| **[⚙️ Technical Reference](docs/TECHNICAL_REFERENCE.md)** | Developers | Architecture, APIs, language specification | Deep technical |
| **[🚀 Rust Implementation](Time_Warp_Rust/README.md)** | Performance Focus | Native speed, cross-platform binary | Advanced |
| **[� Python Implementation](Time_Warp_Python/README.md)** | Education Focus | Accessibility, universal compatibility | Intermediate |
| **[🌐 Web Implementation](Time_Warp_Web/README.md)** | Zero Install | Browser-based, mobile-friendly | Beginner |
| **[💾 DOS Implementation](Time_Warp_DOS/README.md)** | Retro Computing | Vintage hardware, computer history | Historical |
| **[🪟 Windows Implementation](Time_Warp_Windows/README.md)** | Enterprise | MSI deployment, Active Directory | IT Professional |
| **[🍎 Apple Implementation](Time_Warp_Apple/README.md)** | Apple Ecosystem | iOS/macOS Universal App, Apple Pencil | Modern Mobile |

### For Developers

- **[Python API Reference](Time_Warp_Python/README.md#api-usage)** - Interpreter API
- **[Rust Developer Reference](Time_Warp_Rust/docs/DEVELOPER_REFERENCE.md)** - Extension guide
- **[Architecture](Time_Warp_Rust/ARCHITECTURE.md)** - System design
- **[Contributing](Time_Warp_Rust/CONTRIBUTING.md)** - How to contribute

---

## 🎨 Example Programs

The `examples/` directory contains 33+ programs demonstrating all language features:

### BASIC Programs (10)
- `basic_guess.bas` - Number guessing game
- `basic_hangman.bas` - Word guessing game
- `basic_rock_paper_scissors.bas` - Interactive game
- `basic_inkey_demo.bas` - Keyboard input demo
- And more...

### PILOT Programs (7)
- `pilot_quiz.pilot` - Simple quiz system
- `pilot_dragon_adventure.pilot` - Text adventure game
- `pilot_calculator.pilot` - Interactive calculator
- And more...

### Logo Programs (15)
- `logo_flower.logo` - Colorful flower pattern
- `logo_koch_snowflake.logo` - Fractal generation
- `logo_spirograph.logo` - Complex geometric patterns
- `logo_starburst_blue.logo` - Starburst with colors
- And more...

### TempleCode (1)
- `demo.tc` - Mixed language demonstration

---

## 🧪 Testing

### Python Tests

```bash
cd Time_Warp_Python

# Run all tests
python test_ide.py

# Run specific test suites
python test_graphics.py
python test_all_turtle_commands.py
python verify_commands.py

# With pytest
pytest tests/ -v
```

### Rust Tests

```bash
cd Time_Warp_Rust

# Run all tests
cargo test

# Run with output
cargo test -- --nocapture

# Run specific test
cargo test test_name
```

---

## 🔧 Project Structure

```text
Time_Warp/
├── Time_Warp_Python/          # Python implementation
│   ├── time_warp/             # Main package
│   │   ├── core/              # Interpreter engine
│   │   ├── languages/         # TempleCode executor
│   │   ├── graphics/          # Turtle graphics
│   │   ├── ui/                # PySide6 UI components
│   │   └── utils/             # Expression evaluator, error hints
│   ├── examples/              # 34 example programs
│   ├── tests/                 # Test suite
│   ├── docs/                  # Documentation
│   ├── time_warp_ide.py       # GUI entry point
│   └── run_time_warp.py       # CLI entry point
│
├── Time_Warp_Rust/            # Rust implementation
│   ├── src/                   # Source code
│   │   ├── interpreter/       # Core interpreter
│   │   ├── languages/         # Language modules
│   │   ├── graphics/          # Turtle & canvas
│   │   ├── ui/                # egui UI
│   │   ├── compiler/          # TempleCode compiler (experimental)
│   │   └── main.rs            # Entry point
│   ├── docs/                  # Comprehensive docs
│   ├── tests/                 # Rust test suite
│   └── Cargo.toml             # Rust dependencies
│
└── examples/                  # Shared examples (33 programs)
```

---

## 🎯 Turtle Graphics Commands

All turtle graphics commands are fully verified and working:

### Movement
- `FORWARD n` / `FD n` - Move forward
- `BACK n` / `BK n` / `BACKWARD n` - Move backward
- `LEFT n` / `LT n` - Turn left (degrees)
- `RIGHT n` / `RT n` - Turn right (degrees)
- `HOME` - Return to center
- `SETXY x y` - Move to position
- `SETHEADING angle` / `SETH angle` - Set heading

### Pen Control
- `PENUP` / `PU` - Lift pen
- `PENDOWN` / `PD` - Lower pen
- `PENWIDTH n` / `SETPENWIDTH n` / `SETPW n` / `SETPENSIZE n` - Set pen width

### Colors
- `SETCOLOR name` - Use color name (red, blue, green, yellow, cyan, magenta, orange, purple, pink, brown, gray, white, black)
- `SETCOLOR #RRGGBB` - Use hex color
- `SETCOLOR r g b` - Use RGB (0-255)
- `SETPENCOLOR r g b` / `SETPC r g b` - Set pen color (RGB)
- `SETBGCOLOR r g b` / `SETBG r g b` - Set background color

### Screen Control
- `CLEARSCREEN` / `CS` / `CLEAR` - Clear all drawings
- `HIDETURTLE` / `HT` - Hide turtle cursor
- `SHOWTURTLE` / `ST` - Show turtle cursor

### Loops & Procedures
- `REPEAT n [ commands ]` - Single-line loop
- Multi-line REPEAT:
  ```logo
  REPEAT count [
    commands
  ]
  ```
- User procedures:
  ```logo
  TO SQUARE :SIZE
    REPEAT 4 [
      FORWARD :SIZE
      RIGHT 90
    ]
  END
  
  SQUARE 100
  ```

**All 50+ commands verified!** See [Turtle Graphics Reference](Time_Warp_Python/docs/TURTLE_GRAPHICS_REFERENCE.md) for complete details.

---

## 🤝 Contributing

Contributions are welcome! Both implementations are actively maintained:

1. **Fork the repository**
2. **Create your feature branch** (`git checkout -b feature/amazing-feature`)
3. **Make your changes** (follow existing code style)
4. **Run tests** (Python: `python test_ide.py`, Rust: `cargo test`)
5. **✨ Make Your Changes** - Follow our coding standards and best practices
6. **🧪 Test Thoroughly** - Ensure all tests pass and add new tests if needed
7. **📝 Document Changes** - Update relevant documentation and add examples
8. **🔄 Submit Pull Request** - Provide clear description and link to issues

See [CONTRIBUTING.md](docs/CONTRIBUTING.md) for detailed guidelines.

### 🏆 **Recognition System**
- **🌟 Contributors Hall of Fame** - Recognition for significant contributions
- **🎖️ Maintainer Status** - Become a core team member with commit access
- **📢 Feature Attribution** - Get credit in release notes and documentation
- **🎁 Swag & Rewards** - Exclusive Time Warp merchandise for top contributors

---

## � License & Legal

**Time Warp IDE** is released under the [MIT License](docs/LICENSE), ensuring maximum freedom for educational and commercial use.

### ✅ **What You Can Do**
- ✅ Use Time Warp in your classroom or educational institution
- ✅ Modify and customize the software for your specific needs
- ✅ Create and distribute derivative works and educational materials
- ✅ Use Time Warp commercially in educational products and services
- ✅ Contribute improvements back to the community (encouraged but not required)

### 📋 **Attribution Requirements**
- Include the original license and copyright notice in distributions
- Acknowledge Time Warp IDE in educational materials and publications
- Link back to this repository when sharing or discussing the project

---

## � Credits & Acknowledgments

### 🏆 **Core Development Team**
- **James Temple** - Project Creator & Lead Developer
- **[Contributors](docs/CONTRIBUTORS.md)** - Amazing community members who make Time Warp better

### 🙏 **Special Thanks**
- **Seymour Papert** - Pioneer of educational programming and Logo language
- **Logo Foundation** - Inspiration and guidance for turtle graphics implementation  
- **BASIC & PILOT Communities** - Historical preservation and documentation efforts
- **Educational Technology Community** - Feedback, testing, and curriculum development
- **Open Source Community** - Tools, libraries, and frameworks that make Time Warp possible

### 🎓 **Educational Partners**
- **MIT Media Lab** - Research collaboration on constructionist learning
- **Stanford Computer Science Education** - Curriculum development and assessment
- **Code.org** - Integration with Hour of Code and CS Education Week
- **National Education Association** - Teacher training and professional development

---

## � Contact Information

**🏢 Project Maintainer**: James Temple  
**📧 Email**: [james@honey-badger.org](mailto:james@honey-badger.org)  
**🌐 Website**: [https://timewarp-ide.org](https://timewarp-ide.org)  
**📱 GitHub**: [@James-HoneyBadger](https://github.com/James-HoneyBadger)  

**🏫 Educational Partnerships**: [education@timewarp-ide.org](mailto:education@timewarp-ide.org)  
**🛠️ Developer Support**: [developers@timewarp-ide.org](mailto:developers@timewarp-ide.org)  
**🚨 Security Issues**: [security@timewarp-ide.org](mailto:security@timewarp-ide.org)  

---

## 🚀 Ready to Start Your Journey?

**Time Warp IDE** is more than just a programming environment — it's a gateway to computational thinking, creative expression, and lifelong learning. Whether you're taking your first steps into programming or teaching the next generation of digital creators, Time Warp provides the tools, community, and support you need to succeed.

### 🎯 **Choose Your Adventure**
- **🎓 Educator?** Start with the [Teacher Guide](docs/TEACHER_GUIDE.md) and explore our [curriculum materials](examples/curriculum/)
- **👩‍💻 Student?** Jump into the [Student Guide](docs/STUDENT_GUIDE.md) and try your first TempleCode program
- **🛠️ Developer?** Check out the [Contributing Guide](docs/CONTRIBUTING.md) and join our development community
- **🏢 Institution?** Contact us about [enterprise deployment](mailto:education@timewarp-ide.org) and professional support

### 🚀 **Get Started Now**
```bash
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp
./run.sh
```

**Welcome to the Time Warp community!** 🌟

---

*Last updated: October 31, 2025 | Version 3.0.0 | [View Change History](docs/CHANGELOG.md)*

