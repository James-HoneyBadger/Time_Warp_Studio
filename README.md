# Time Warp IDE v6.0.0

A unified educational programming environment supporting BASIC, PILOT, Logo, Python, C, Pascal, and Prolog with integrated turtle graphics and modern IDE features.

**[Quick Start](#quick-start) • [Documentation](#documentation) • [Examples](#examples) • [Contributing](#contributing)**

---

## Overview

Time Warp IDE brings classic and modern programming languages into a single, accessible environment designed for education and experimentation. Whether you're learning BASIC on a virtual computer, drawing with Logo, or exploring algorithms with Python, Time Warp provides the tools you need.

### Key Features

- 🎨 **Unified Editor** - Syntax highlighting and code editing for all supported languages
- 🐢 **Turtle Graphics** - Full turtle graphics support with interactive canvas
- 📚 **7 Languages** - BASIC, PILOT, Logo, Python, C, Pascal, Prolog
- ⚡ **Fast Execution** - Instant code execution with real-time output
- 🎓 **Educational** - Designed for teaching programming concepts
- 🔧 **Extensible** - Plugin system for custom features
- 💾 **Project Management** - Organize and save your work

---

## Quick Start

### Requirements

- **Python** 3.10 or higher
- **Modern operating system** (Windows, macOS, Linux)
- **4GB RAM** minimum recommended

### Installation

1. **Clone the repository:**
   ```bash
   git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
   cd Time_Warp_Studio
   ```

2. **Create a virtual environment:**
   ```bash
   python3 -m venv .venv_new
   source .venv_new/bin/activate
   ```

3. **Install dependencies:**
   ```bash
   pip install PySide6 Pillow requests
   ```

4. **Run the IDE:**
   ```bash
   python Platforms/Python/time_warp_ide.py
   ```

### First Program

1. Select **BASIC** from the Language dropdown
2. Type:
   ```basic
   PRINT "Hello, World!"
   ```
3. Click **Run** (or press Ctrl+R)

---

## Documentation

### Getting Started
- [Installation & Setup](docs/guides/01-getting-started.md) - Detailed setup instructions
- [IDE Basics](docs/guides/02-ide-basics.md) - Navigate the interface
- [Your First Program](docs/guides/03-first-program.md) - Step-by-step tutorial

### Language Guides
- [BASIC Tutorial](docs/tutorials/basic.md) - Learn BASIC programming
- [PILOT Tutorial](docs/tutorials/pilot.md) - Computer-based instruction language
- [Logo Tutorial](docs/tutorials/logo.md) - Turtle graphics and Logo
- [Python Guide](docs/tutorials/python.md) - Python in Time Warp
- [C Reference](docs/tutorials/c.md) - C programming basics
- [Pascal Guide](docs/tutorials/pascal.md) - Pascal language features
- [Prolog Guide](docs/tutorials/prolog.md) - Logic programming

### Features & Usage
- [Turtle Graphics Guide](docs/guides/04-turtle-graphics.md) - Draw and animate
- [Project Management](docs/guides/05-projects.md) - Organize your code
- [Settings & Themes](docs/guides/06-settings.md) - Customize the IDE
- [Keyboard Shortcuts](docs/guides/07-shortcuts.md) - Speed up your workflow

### Reference
- [API Reference](docs/api/interpreter-api.md) - Interpreter API
- [Built-in Functions](docs/reference/builtins.md) - Available functions by language
- [Troubleshooting](docs/guides/08-troubleshooting.md) - Common issues and solutions
- [FAQ](docs/reference/faq.md) - Frequently asked questions

---

## Examples

Learn from complete example programs in the `Examples/` directory:

```
Examples/
├── basic/          - BASIC language samples
├── pilot/          - PILOT programs
├── logo/           - Turtle graphics examples
├── python/         - Python demonstrations
├── c/              - C language programs
├── pascal/         - Pascal examples
└── prolog/         - Logic programming
```

Run any example by opening it in the IDE and pressing Ctrl+R.

---

## Architecture

### Core Components

**Interpreter** (`Platforms/Python/time_warp/core/interpreter.py`)
- Central command dispatcher
- Language-specific executors
- State management

**Language Executors** (`Platforms/Python/time_warp/core/interpreters/`)
- `basic.py` - BASIC interpreter
- `pilot.py` - PILOT interpreter
- `logo.py` - Logo interpreter with turtle graphics
- Experimental: `python.py`, `c.py`, `pascal.py`, `prolog.py`

**UI Components** (`Platforms/Python/time_warp/ui/`)
- Main window and editor
- Turtle graphics canvas
- Output panel and console

---

## Configuration

User settings are stored in `~/.Time_Warp/config.json`:

```json
{
  "theme": "dracula",
  "font_size": 12,
  "auto_save": true,
  "recent_files": []
}
```

Themes available: Dracula, Monokai, Solarized Dark, Ocean, Spring, Sunset, Candy, Forest

---

## Development

### Project Structure

```
Platforms/Python/
├── time_warp_ide.py          - Main entry point
├── time_warp/
│   ├── core/                 - Interpreter and language executors
│   ├── ui/                   - UI components and main window
│   └── tools/                - Utility modules
└── Examples/                 - Sample programs
```

### Running Tests

```bash
# Set up test environment
cd Platforms/Python
pip install pytest pytest-cov

# Run tests
pytest tests/ -v --cov
```

### Adding a New Language

See [docs/guides/09-extending.md](docs/guides/09-extending.md) for detailed instructions.

---

## Contributing

We welcome contributions! Please read [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) first.

### Getting Started with Development

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Run tests to ensure everything works
5. Commit your changes (`git commit -m 'Add amazing feature'`)
6. Push to your branch (`git push origin feature/amazing-feature`)
7. Open a Pull Request

### Areas for Contribution

- Additional example programs
- Documentation improvements
- Bug fixes and performance improvements
- New language support
- UI/UX enhancements

---

## Troubleshooting

### "Illegal instruction" Error
**Cause:** Your CPU lacks required features (SSSE3, SSE4.1, SSE4.2, POPCNT)  
**Solution:** Run on modern hardware or cloud instance

### IDE won't start
See [Troubleshooting Guide](docs/guides/08-troubleshooting.md)

### Performance Issues
Check [Performance Optimization](docs/guides/08-troubleshooting.md#performance)

---

## License

This project is licensed under the Apache License 2.0 - see [LICENSE](LICENSE) file for details.

---

## Support

- 📖 **Documentation:** See [docs/](docs/) directory
- 🐛 **Issues:** [GitHub Issues](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues)
- 💬 **Discussions:** [GitHub Discussions](https://github.com/James-HoneyBadger/Time_Warp_Studio/discussions)
- 📧 **Email:** james@honey-badger.org

---

## Acknowledgments

Time Warp IDE builds on the legacy of educational programming languages while incorporating modern development practices. Special thanks to the BASIC, Logo, PILOT, and open-source communities for their inspiration.

---

**Made with ❤️ for educators and learners everywhere**
