# Time Warp IDE

**The Ultimate Educational Programming Environment**

Time Warp IDE is a multi-platform educational tool designed to teach programming through a unified language (**TempleCode**) that combines the best features of **BASIC**, **PILOT**, and **Logo**.

## 🌟 Features

- **Unified Language**: Mix BASIC control structures, PILOT pattern matching, and Logo turtle graphics in a single program.
- **Multi-Platform**: Available in Python, Rust, Go, Web, and more.
- **Educational Focus**: Designed for classrooms with comprehensive guides and lesson plans.

## 🚀 Platforms

The project is organized into platform-specific implementations:

| Platform | Location | Status | Description |
|----------|----------|--------|-------------|
| **Python** | [`platforms/python/`](platforms/python/) | ✅ Complete | The primary educational implementation. Easy to install and modify. |
| **Rust** | [`platforms/rust/`](platforms/rust/) | ✅ Reference | High-performance reference implementation using `egui`. |
| **Web** | [`platforms/web/`](platforms/web/) | ✅ Complete | Browser-based version using HTML5 Canvas. |
| **Go** | [`platforms/go/`](platforms/go/) | ✅ Expanded | CLI and GUI implementation in Go. |
| **Legacy** | [`legacy/`](legacy/) | 🔒 Archived | Older implementations and experiments. |

## 📚 Documentation

- **[Documentation Index](docs/DOCUMENTATION_INDEX.md)**: Start here for all guides.
- **[Installation Guide](docs/installation/INSTALLATION_GUIDE.md)**: How to install on any system.
- **[User Guide](docs/user/USER_GUIDE.md)**: How to use the IDE.
- **[Programming Guide](docs/user/PROGRAMMING_GUIDE.md)**: Learn TempleCode (BASIC, PILOT, Logo).

## 🛠️ Quick Start

### Python Version (Recommended)

```bash
./run.sh
```

Or manually:

```bash
cd platforms/python
pip install -r requirements.txt
python time_warp_ide.py
```

### Rust Version

```bash
cd platforms/rust
cargo run
```

## 📂 Project Structure

- `platforms/`: Source code for all implementations.
- `docs/`: Comprehensive documentation.
- `examples/`: Sample programs in BASIC, PILOT, and Logo.
- `tools/`: Development and build tools.
- `legacy/`: Archived code.

## 🤝 Contributing

See [CONTRIBUTING.md](docs/development/CONTRIBUTING.md) for guidelines.

## 📄 License

MIT License. See [LICENSE](LICENSE) for details.
