# Time Warp IDE - Developer Guide

**Version:** 3.0.0  
**Last Updated:** November 18, 2025

This guide is for contributors who want to understand, extend, and maintain the Time Warp IDE.

## Overview

Time Warp IDE is a multi-language educational programming environment. It supports BASIC, Logo, PILOT, and TempleCode.

## Repository Structure

The project is organized into platform-specific directories under `platforms/`:

-   `platforms/python/`: The primary Python implementation using PySide6.
-   `platforms/rust/`: Rust implementation (experimental).
-   `platforms/go/`: Go implementation (experimental).
-   `platforms/web/`: Web implementation (experimental).
-   `legacy/`: Archived code.

## Python Implementation (`platforms/python`)

This is the main focus of current development.

### Setup

1.  Navigate to the directory:
    ```bash
    cd platforms/python
    ```
2.  Create a virtual environment:
    ```bash
    python3 -m venv venv
    source venv/bin/activate
    ```
3.  Install dependencies:
    ```bash
    pip install -r requirements.txt
    ```

### Running the IDE

```bash
python time_warp_ide.py
```

### Testing

We use `pytest` for testing.

```bash
python test_runner.py --comprehensive
```

## Architecture

The Python implementation follows a modular architecture:

-   **Core (`time_warp/core/`)**: Contains the `TimeWarpInterpreter` which manages state and dispatches commands.
-   **Languages (`time_warp/languages/`)**: Contains `LanguageExecutor` implementations for each supported language.
-   **UI (`time_warp/ui/`)**: Contains the PySide6 user interface code.

## Contributing

1.  Fork the repository.
2.  Create a feature branch.
3.  Make your changes.
4.  Run tests to ensure no regressions.
5.  Submit a Pull Request.

## Documentation

Documentation is located in the `docs/` directory.
-   `docs/development/`: Developer guides and reference.
-   `docs/user/`: User guides.
-   `docs/installation/`: Installation instructions.
