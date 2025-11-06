# Time Warp IDE v2.0.0 - Release Notes

**Release Date**: October 28, 2025  
**Status**: Stable Release  
**License**: MIT

---

## 🎉 Major Release - Feature Complete!

Time Warp IDE 2.0.0 represents the culmination of extensive development effort, delivering a complete, production-ready educational programming environment. Both Python and Rust implementations now have full feature parity and are ready for classroom use.

## 🚀 Quick Start

### Python (Portable & Educational)

```bash
# Install dependencies
pip install PySide6 pillow

# Launch IDE
cd Time_Warp_Python
python time_warp_ide.py

# Or use root launcher
./run.sh python
```

### Rust (Native & High-Performance)

```bash
# Build and run
cd Time_Warp_Rust
cargo run --release

# Or use root launcher
./run.sh rust
```

## ✨ What's New in 2.0.0

### Language Features

- **✅ TempleCode Unified Language** - Seamlessly mix BASIC, PILOT, and Logo commands
- **✅ Logo Procedures** - Define reusable procedures with parameters (`TO name :param ... END`)
- **✅ Multi-line REPEAT** - Nested and top-level multi-line loop blocks
- **✅ 50+ Turtle Commands** - Complete turtle graphics system fully verified
- **✅ Named Colors** - 14 intuitive color names (red, blue, green, yellow, etc.)
- **✅ Color Formats** - Support for hex (#RRGGBB) and RGB (r,g,b) colors
- **✅ Expression Evaluation** - Safe math parser with 15+ functions
- **✅ Error Hints** - Typo suggestions with 100+ common mistakes detected

### Python IDE Features

- **🎨 Modern Qt6 Interface** - Clean, professional PySide6-based GUI
- **🌈 8 Color Themes** - Dracula, Monokai, Solarized, Ocean, Spring, Sunset, Candy, Forest
- **📁 Recent Files** - Track and reopen last 10 files
- **🔍 Find/Replace** - Search and replace functionality
- **📊 Multi-tab Output** - Separate Text and Graphics tabs with auto-switch
- **🎯 Syntax Highlighting** - Color-coded keywords for all three languages
- **📏 Line Numbers** - Dynamic line number gutter
- **🔎 Zoom Controls** - Editor zoom in/out
- **🐢 Canvas Controls** - Pan, zoom turtle graphics
- **⚡ Auto-clear** - Clean output on each run
- **🔒 Security** - Safe execution with iteration/timeout limits

### Rust IDE Features

- **⚡ Native Performance** - Compiled egui interface
- **🔄 Async Execution** - Non-blocking tokio-based execution
- **💾 PNG Export** - Save turtle graphics as images
- **🔨 Experimental Compiler** - TempleCode → C transpiler
- **🐛 Step Debugger** - Line-by-line execution
- **🖥️ Unified Screen** - Combined text/graphics rendering
- **⌨️ INKEY$ Support** - Real-time keyboard detection
- **🎨 Theme System** - Matching 8-theme color system

### Documentation

- **📚 76+ Documentation Files** - Comprehensive guides for all users
- **👥 User Guide** - Unified guide covering both implementations
- **🎓 Educational Content** - Lesson plans, teacher/student guides
- **💡 33 Example Programs** - Categorized by language and difficulty
- **🔧 Developer Docs** - API reference and architecture guides
- **🚀 Quick References** - Complete command references

## 📦 What's Included

### Implementations

- **Python 2.0.0** - Full PySide6 GUI, ~4,000 lines of code
- **Rust 2.0.0** - Native egui interface, comprehensive test suite

### Examples

- **15 Logo Programs** - Graphics, fractals, patterns
- **10 BASIC Programs** - Games, utilities, demos
- **7 PILOT Programs** - Quizzes, adventures, calculators
- **1 TempleCode Demo** - Mixed language showcase

### Documentation

- User guides (3 files)
- Technical references (5 files)
- Educational content (8 files)
- API/developer docs (4 files)
- Project docs (CHANGELOG, CONTRIBUTING, CODE_OF_CONDUCT, SECURITY)

## 🔧 Bug Fixes

- Fixed Logo procedure parameter binding
- Fixed SETCOLOR named color recognition
- Fixed PENWIDTH command aliases
- Fixed multi-line REPEAT at top level
- Fixed expression evaluation in rotation commands
- Fixed BACKWARD and CLEAR command aliases
- Corrected documentation errors and outdated status

## 💡 Improvements

- Cleaner startup (no welcome banner)
- Auto-clear output on run
- Better error messages
- Improved documentation consistency
- Organized folder structure
- Updated version strings

## 📊 Statistics

- **Total Code**: ~10,000 lines (Python + Rust)
- **Test Coverage**: 22 Rust integration tests, 5 Python test scripts
- **Documentation**: 76+ markdown files
- **Examples**: 33 programs across 3 languages
- **Commands**: 50+ turtle graphics commands verified
- **Themes**: 8 color schemes
- **Supported Platforms**: Linux, macOS, Windows

## 🔐 Security

- No use of `eval()` or `exec()` in Python
- 100,000 iteration limit prevents infinite loops
- 10-second execution timeout
- Safe expression evaluation (custom parser)
- Input validation and sanitization
- Memory-safe Rust implementation

## 📋 Upgrade Guide

### From 1.x to 2.0

**No breaking changes!** All 1.x programs run unchanged in 2.0.

**New features available:**
- Logo procedures with parameters
- Multi-line REPEAT blocks
- Named colors
- Expression evaluation in commands

**Update steps:**
1. Download/clone v2.0.0
2. Install dependencies (Python) or rebuild (Rust)
3. Run existing programs - they just work!
4. Explore new features in examples/

## 🎯 Use Cases

### For Students

- Learn programming concepts visually
- Create graphics with turtle commands
- Build interactive quizzes and games
- Experiment with three language styles

### For Teachers

- 8-week curriculum included
- Lesson plans with assessments
- Programming challenges with solutions
- Safe, controlled environment

### For Developers

- Clean API for embedding
- Extensible architecture
- Well-documented codebase
- Active development

## 📸 Screenshots

*(Include screenshots of both Python and Rust IDEs showing:)*
- Main interface with code editor
- Turtle graphics output
- Theme variations
- Example programs running

## 🌟 Highlights

> "Complete, production-ready educational programming environment"

> "50+ turtle commands all verified and working"

> "Feature parity between Python and Rust implementations"

> "76+ documentation files covering every aspect"

> "33 example programs for all skill levels"

## 🤝 Contributing

We welcome contributions! See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

### Ways to Contribute

- 🐛 Report bugs
- ✨ Suggest features
- 📝 Improve documentation
- 🎨 Create example programs
- 🔧 Submit pull requests
- 💬 Help other users

## 📝 License

MIT License - see [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- Inspired by classic educational languages: BASIC, PILOT, and Logo
- Built with modern tools: Rust, Python, egui, PySide6
- Designed for education, learning, and creative expression

## 📧 Contact

**Maintainer**: James Temple  
**Email**: james@honey-badger.org  
**GitHub**: https://github.com/James-HoneyBadger/Time_Warp

## 🔗 Links

- [User Guide](USER_GUIDE.md)
- [Python README](Time_Warp_Python/README.md)
- [Rust README](Time_Warp_Rust/README.md)
- [Quick Reference](Time_Warp_Rust/docs/QUICK_REFERENCE.md)
- [Turtle Graphics Reference](Time_Warp_Python/docs/TURTLE_GRAPHICS_REFERENCE.md)
- [CHANGELOG](CHANGELOG.md)
- [CONTRIBUTING](CONTRIBUTING.md)
- [CODE_OF_CONDUCT](CODE_OF_CONDUCT.md)
- [SECURITY](SECURITY.md)

## 📈 What's Next

Future plans (tentative):
- Additional language features
- More example programs
- Video tutorials
- Online playground
- Plugin system
- Translation/i18n

## 🎉 Thank You!

Thank you to everyone who tested, provided feedback, and supported this project. Time Warp IDE is now ready to help students worldwide learn programming!

---

**Download**: [Release Assets Below](#assets)  
**Install**: See [Quick Start](#-quick-start)  
**Learn**: Read [User Guide](USER_GUIDE.md)  
**Contribute**: See [CONTRIBUTING.md](CONTRIBUTING.md)

**Happy Coding! 🐢🚀**
