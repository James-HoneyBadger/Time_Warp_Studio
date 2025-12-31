# Time Warp IDE - Launch Guide

## Quick Start

### Prerequisites
- Python 3.10 or higher
- Modern operating system (Windows, macOS, Linux)

### 1. Setup Virtual Environment
```bash
cd /home/james/Time_Warp_Studio
python3 -m venv .venv_new
source .venv_new/bin/activate  # Linux/macOS
# or
.venv_new\Scripts\activate  # Windows
```

### 2. Install Dependencies
```bash
pip install PySide6 Pillow requests
```

### 3. Launch the IDE
```bash
python Platforms/Python/time_warp_ide.py
```

## What's New (v6.0.0)

- ✅ Reorganized project structure
  - `config/` folder for configuration files
  - `archive/` folder for historical documentation
- ✅ All syntax checks pass
- ✅ IDE successfully initializes without errors

## Project Structure

```
Time_Warp_Studio/
├── Platforms/Python/
│   └── time_warp_ide.py         # Main IDE entry point
├── Examples/                    # Demo programs (BASIC, Logo, Pascal, etc.)
├── docs/                        # Complete documentation
├── config/                      # Configuration files (.flake8, .markdownlint.json)
├── archive/                     # Historical release notes and documentation
└── Scripts/                     # Build and launch utilities
```

## Testing

Run the comprehensive test suite:
```bash
python test_runner.py --comprehensive
```

Quick smoke tests:
```bash
python test_runner.py --basic
```

## Supported Languages

1. **BASIC** - Classic programming language
2. **PILOT** - Computer-based instruction language
3. **Logo** - Turtle graphics and drawing
4. **Python** - Modern Python programming
5. **C** - Systems programming
6. **Pascal** - Structured programming
7. **Prolog** - Logic programming

## Documentation

- 📖 [Full Documentation](docs/INDEX.md)
- 🚀 [Getting Started Guide](docs/guides/01-getting-started.md)
- 🎨 [Turtle Graphics Tutorial](docs/guides/04-turtle-graphics.md)
- ⌨️ [Keyboard Shortcuts](docs/guides/07-shortcuts.md)
- 🔧 [Troubleshooting](docs/guides/08-troubleshooting.md)

## Debugging

If you encounter issues:

1. **Check Python version:**
   ```bash
   python --version
   ```

2. **Verify dependencies:**
   ```bash
   pip list | grep -E "PySide6|Pillow"
   ```

3. **Check for CPU feature requirements:**
   - The IDE requires SSSE3, SSE4.1, SSE4.2, POPCNT CPU features
   - If you see "Illegal instruction" errors, run on modern hardware

4. **View detailed logs:**
   ```bash
   python Platforms/Python/time_warp_ide.py --debug
   ```

5. **See full troubleshooting guide:**
   - [Troubleshooting Guide](docs/guides/08-troubleshooting.md)

## Examples

Browse example programs:
- `Examples/basic/` - BASIC programming examples
- `Examples/logo/` - Logo and turtle graphics
- `Examples/pascal/` - Pascal language examples
- `Examples/prolog/` - Prolog logic programming

## Contributing

See [Contributing Guide](CONTRIBUTING.md) for:
- Code style guidelines
- Testing requirements
- Pull request process

## Support

- 📖 Documentation: [docs/](docs/)
- 🐛 Issues: [GitHub Issues](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues)
- 💬 Discussions: [GitHub Discussions](https://github.com/James-HoneyBadger/Time_Warp_Studio/discussions)

## License

Apache License 2.0 - See [LICENSE](LICENSE)

---

**Next Steps:** Follow the [Getting Started Guide](docs/guides/01-getting-started.md) for your first program!
