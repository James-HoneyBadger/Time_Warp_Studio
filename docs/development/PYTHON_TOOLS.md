# Python Development Tools Guide

This document describes the Python development infrastructure for Time Warp IDE.

## Quick Start

```bash
# Install all development dependencies
pip install -e ".[dev]"

# Or install specific dependency groups
pip install -e ".[gui]"  # GUI only
pip install -e "."       # Core only
```

## Build Automation

The `scripts/build.py` utility provides unified build commands:

```bash
# Build all platforms
./scripts/build.py build all

# Build Rust version (debug)
./scripts/build.py build rust

# Build Rust version (release)
./scripts/build.py build rust --release

# Build Windows 2000 version
./scripts/build.py build win2000

# Run tests
./scripts/build.py test

# Format code
./scripts/build.py format

# Lint code
./scripts/build.py lint

# Type check
./scripts/build.py typecheck

# Clean build artifacts
./scripts/build.py clean
```

## Release Management

The `scripts/release.py` utility automates releases:

```bash
# Generate release notes
./scripts/release.py --notes

# Create git tag
./scripts/release.py --tag

# Create and push tag
./scripts/release.py --tag --push

# Specify version
./scripts/release.py --version 2.1.0 --notes
```

## Code Quality Tools

### Black (Code Formatting)

```bash
# Format all Python code
black .

# Check formatting without changes
black --check .
```

Configuration in `pyproject.toml`:
- Line length: 88 characters
- Target: Python 3.8+

### Flake8 (Linting)

```bash
# Lint all Python code
flake8 .

# Lint specific files
flake8 platforms/python/
```

Configuration in `.flake8`:
- Max line length: 88 (matches black)
- Excludes: legacy code, build artifacts
- Ignores: E203, E266, E501, W503

### MyPy (Type Checking)

```bash
# Type check all code
mypy .

# Type check specific module
mypy platforms/python/
```

Configuration in `pyproject.toml`:
- Python version: 3.8+
- Strict mode disabled (gradual adoption)
- Ignores missing imports for optional dependencies

### isort (Import Sorting)

```bash
# Sort imports
isort .

# Check import order
isort --check-only .
```

Configuration in `pyproject.toml`:
- Profile: black (compatible with black formatting)
- Line length: 88

## Testing

### Pytest

```bash
# Run all tests
pytest

# Run with coverage
pytest --cov=platforms/python --cov-report=html

# Run specific test file
pytest tests/test_core.py

# Run with verbose output
pytest -v
```

Configuration in `pyproject.toml`:
- Test discovery: `tests/` directory
- Coverage reporting: HTML and terminal
- Minimum coverage: 80% (enforced in CI)

### Test Structure

```
tests/
├── test_core.py          # Core interpreter tests
├── test_languages.py     # Language-specific tests
├── test_ui.py            # UI component tests
└── conftest.py           # Pytest fixtures
```

## Dependency Management

### requirements.txt

Root-level dependencies for all environments:

```bash
# Install all dependencies
pip install -r requirements.txt

# Install development dependencies only
pip install pytest pytest-cov black flake8 mypy isort
```

### pyproject.toml

Modern packaging configuration (PEP 517/518):

```toml
[project]
dependencies = [...]        # Core runtime dependencies

[project.optional-dependencies]
gui = [...]                # GUI dependencies (PySide6)
dev = [...]                # Development tools
all = [...]                # Everything
```

Install with optional dependencies:

```bash
pip install -e ".[all]"    # Everything
pip install -e ".[dev]"    # Development tools
pip install -e ".[gui]"    # GUI support
```

## Continuous Integration

GitHub Actions workflows are configured in `.github/workflows/`:

### win2000-build.yml

Automated Windows 2000 cross-compilation:

```yaml
Triggers:
  - Push to main/master
  - Pull requests
  - Manual workflow dispatch
  
Steps:
  1. Install MinGW-w64 toolchain
  2. Build Windows 2000 executable
  3. Run sanity tests
  4. Generate manifest with checksums
  5. Upload artifacts (30-day retention)
  6. Create GitHub release on version tags
```

Artifacts produced:
- `TimeWarpIDE-win2000.zip` (complete package)
- `RELEASE_MANIFEST.txt` (checksums and metadata)

## Project Structure

```
Time_Warp/
├── pyproject.toml           # Modern Python packaging config
├── setup.py                 # Backward compatibility wrapper
├── requirements.txt         # All Python dependencies
├── .flake8                  # Flake8 configuration
├── scripts/
│   ├── build.py            # Build automation CLI
│   └── release.py          # Release automation CLI
├── platforms/
│   └── python/
│       ├── requirements.txt # Platform-specific deps
│       └── setup.py        # Platform-specific setup
└── tests/                   # Test suite
```

## Development Workflow

1. **Clone repository**
   ```bash
   git clone https://github.com/yourusername/Time_Warp.git
   cd Time_Warp
   ```

2. **Set up development environment**
   ```bash
   # Option 1: Virtual environment (recommended)
   python3 -m venv .venv
   source .venv/bin/activate
   pip install -e ".[dev]"
   
   # Option 2: System-wide (with --break-system-packages)
   pip install -e ".[dev]" --break-system-packages
   ```

3. **Make changes**
   - Edit code in `platforms/python/` or other modules
   - Follow coding standards (black, flake8, mypy)

4. **Test changes**
   ```bash
   ./scripts/build.py format   # Format code
   ./scripts/build.py lint     # Check linting
   ./scripts/build.py typecheck # Type check
   ./scripts/build.py test     # Run tests
   ```

5. **Build and verify**
   ```bash
   ./scripts/build.py build all
   ```

6. **Commit and push**
   ```bash
   git add .
   git commit -m "Description of changes"
   git push origin feature-branch
   ```

## Packaging for Distribution

### Python Package (PyPI)

```bash
# Build distribution packages
python -m build

# Upload to PyPI
python -m twine upload dist/*
```

### Platform-Specific Builds

See platform-specific documentation:
- Rust: `platforms/rust/README.md`
- Windows 2000: `platforms/win2000/README.md`
- Web: `platforms/web/README.md`

## Troubleshooting

### ImportError: No module named 'PySide6'

Install GUI dependencies:
```bash
pip install -e ".[gui]"
```

### externally-managed-environment error

Use virtual environment or `--break-system-packages`:
```bash
python3 -m venv .venv
source .venv/bin/activate
pip install -e ".[dev]"
```

### MyPy errors for optional dependencies

Install all dependencies:
```bash
pip install -e ".[all]"
```

Or ignore specific modules in `pyproject.toml`.

### Flake8 line too long

Black should handle this automatically. If needed, add `# noqa: E501` comment.

## Additional Resources

- [Python Packaging Guide](https://packaging.python.org/)
- [PEP 517 - Build Backend Interface](https://peps.python.org/pep-0517/)
- [PEP 518 - Build System Requirements](https://peps.python.org/pep-0518/)
- [Black Documentation](https://black.readthedocs.io/)
- [Flake8 Documentation](https://flake8.pycqa.org/)
- [MyPy Documentation](https://mypy.readthedocs.io/)
- [Pytest Documentation](https://pytest.org/)
