# Refactoring and Feature Update Summary

## 1. Directory Structure Reorganization
The project has been reorganized to support multiple platforms cleanly:

-   `platforms/python/`: The main Python/PySide6 implementation.
    -   `time_warp/`: Core package.
    -   `time_warp_ide.py`: Entry point.
    -   `tests/`: Unit tests.
-   `platforms/rust/`: Rust implementation.
-   `platforms/go/`: Go implementation.
-   `platforms/web/`: Web implementation.
-   `legacy/`: Archived code (old Python versions).

## 2. Feature Implementation
Missing BASIC commands have been implemented:
-   `DIM`: Array declaration (0-based, initialized to 0.0).
-   `DATA`: Define data values.
-   `READ`: Read values into variables.
-   `RESTORE`: Reset data pointer.

## 3. Bug Fixes
-   Fixed Logo parser to support multi-line commands (e.g., `REPEAT` blocks spanning multiple lines).

## 4. Documentation
-   **README.md**: Completely rewritten to reflect the new structure.
-   **Developer Guide**: Updated to point to the correct locations and explain the architecture.
-   **Developer Reference**: Updated with current API details.

## 5. Scripts
-   `run.sh`: Updated to launch the Python implementation from the new location.
-   `install.sh` / `install-user.sh`: Updated to install from `platforms/python`.
-   `platforms/python/test_runner.py`: New script to run unit tests.

## How to Run
```bash
./run.sh
```

## How to Test
```bash
cd platforms/python
python test_runner.py --basic
```
