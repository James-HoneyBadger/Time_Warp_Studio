# Golden Test Examples

This directory contains canonical programs for cross-language conformance testing.

## Structure

- `basic/` - BASIC language golden tests
- `pilot/` - PILOT language golden tests
- `logo/` - Logo language golden tests

Each program has a corresponding:
- Input fixture (if needed) in `Examples/golden_fixtures/{language}/`
- Expected output snapshot in `Tests/golden_snapshots/{language}/`

## Programs

### BASIC
- `hello.bas` - Simple print statements
- `arithmetic.bas` - Basic arithmetic operations

### PILOT
- `hello.pilot` - Text output
- `compute.pilot` - Computations with variable substitution

### Logo
- `hello.logo` - Print statements
- `square.logo` - Turtle graphics (square drawing)

## Running Tests

```bash
pytest -q Tests/test_conformance_basic_pilot_logo.py
```

## CI Integration

Golden tests run nightly via `.github/workflows/nightly.yml` and on demand.
