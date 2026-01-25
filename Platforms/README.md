# Platforms Overview

This directory contains the Time Warp Studio implementation.

## Active Platform

**Python/** – PySide6 desktop application (primary implementation)
- Native desktop IDE built with Python 3.10+ and PySide6 (Qt6)
- Full support for BASIC, PILOT, Logo languages
- Experimental support for Pascal, Prolog, C
- Integrated turtle graphics canvas
- Cross-platform (Windows, macOS, Linux)

## Architecture

```
Platforms/Python/
├── time_warp_ide.py          # Main entry point
├── time_warp/
│   ├── core/                 # Core interpreter and language executors
│   ├── ui/                   # PySide6 UI components
│   ├── languages/            # Language-specific interpreters
│   └── tests/                # Test suite
└── requirements.txt          # Python dependencies
```

## Documentation

- See [Python/README.md](Python/README.md) for detailed setup
- Main documentation: [../docs/INDEX.md](../docs/INDEX.md)
- Getting started: [../docs/guides/01-getting-started.md](../docs/guides/01-getting-started.md)
