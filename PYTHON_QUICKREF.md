# Python Tooling Quick Reference

## Install Development Dependencies

```bash
# Recommended: Virtual environment
python3 -m venv .venv
source .venv/bin/activate
pip install -e ".[dev]"

# Alternative: System-wide
pip install -r requirements.txt
```

## Build Commands

```bash
# Build everything
./scripts/build.py build all

# Build specific platform
./scripts/build.py build rust         # Debug build
./scripts/build.py build rust --release
./scripts/build.py build win2000

# Clean artifacts
./scripts/build.py clean
```

## Code Quality

```bash
# Format code (auto-fix)
./scripts/build.py format

# Check linting (reports only)
./scripts/build.py lint

# Type checking (reports only)
./scripts/build.py typecheck

# Run all checks
./scripts/build.py format && ./scripts/build.py lint && ./scripts/build.py typecheck
```

## Testing

```bash
# Run all tests
./scripts/build.py test

# Run specific test file
pytest tests/test_specific.py

# Run with coverage report
pytest --cov=platforms/python --cov-report=html
```

## Release Management

```bash
# Generate release notes (no git operations)
./scripts/release.py --notes

# Create git tag (local only)
./scripts/release.py --tag

# Create and push tag to remote
./scripts/release.py --tag --push

# Specify version manually
./scripts/release.py --version 2.1.0 --notes
```

## Common Workflows

### Pre-commit Checklist
```bash
./scripts/build.py format
./scripts/build.py lint
./scripts/build.py typecheck
./scripts/build.py test
```

### Release Preparation
```bash
# 1. Build all platforms
./scripts/build.py build all

# 2. Run full test suite
./scripts/build.py test

# 3. Generate release notes
./scripts/release.py --notes

# 4. Review and edit release notes in dist/

# 5. Create and push tag
./scripts/release.py --tag --push
```

### CI/CD Triggers

**Automatic builds on:**
- Push to `main` or `master`
- Pull request creation
- Tag creation matching `v*.*.*`

**Manual trigger:**
- Go to Actions → Win2000 Build → Run workflow

## File Locations

| File | Purpose |
|------|---------|
| `pyproject.toml` | Modern packaging config |
| `requirements.txt` | All Python dependencies |
| `.flake8` | Linting rules |
| `scripts/build.py` | Build automation CLI |
| `scripts/release.py` | Release automation CLI |
| `.github/workflows/win2000-build.yml` | CI workflow |

## Troubleshooting

**externally-managed-environment error:**
```bash
python3 -m venv .venv
source .venv/bin/activate
pip install -e ".[dev]"
```

**ImportError for PySide6:**
```bash
pip install -e ".[gui]"
```

**Flake8 or MyPy errors:**
```bash
# Auto-fix formatting first
./scripts/build.py format

# Check again
./scripts/build.py lint
./scripts/build.py typecheck
```

## Links

- Full guide: `docs/development/PYTHON_TOOLS.md`
- Implementation summary: `docs/development/PYTHON_INFRASTRUCTURE_SUMMARY.md`
- Main README: `README.md`
