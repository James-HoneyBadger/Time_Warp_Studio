# Cross-Language Conformance & Testing Features

## Overview

This implementation adds three major testing and debugging capabilities to Time Warp IDE:

1. **Golden Test Suite** - Canonical outputs for regression testing
2. **Nightly CI Runner** - Automated headless execution in CI
3. **Playground Mode** - Interactive sandbox for quick repro and debugging

## 1. Golden Test Suite

### Structure
```
Examples/golden/{language}/        # Canonical programs
Examples/golden_fixtures/{language}/  # Input fixtures
Tests/golden_snapshots/{language}/   # Expected outputs
Tests/test_conformance_basic_pilot_logo.py  # Test runner
```

### Usage
```bash
# Run conformance tests
python -m pytest Tests/test_conformance_basic_pilot_logo.py -v

# Add new golden test:
# 1. Create Examples/golden/{lang}/{program}.{ext}
# 2. Add fixture to Examples/golden_fixtures/{lang}/{program}_{ext}.in (if needed)
# 3. Generate snapshot: python Scripts/ci_run_examples.py and copy output
# 4. Place in Tests/golden_snapshots/{lang}/{program}_{ext}.out
```

### Current Coverage
- **BASIC**: hello.bas, arithmetic.bas
- **PILOT**: hello.pilot, compute.pilot
- **Logo**: hello.logo, square.logo

## 2. Nightly CI Runner

### Components
- `Scripts/ci_run_examples.py` - Headless runner script
- `Scripts/interpreter_shim.py` - Interpreter API wrapper
- `.github/workflows/nightly.yml` - GitHub Actions workflow

### Usage
```bash
# Run all examples headlessly
python Scripts/ci_run_examples.py --languages basic pilot logo

# Results in test_reports/{lang}_{program}.out
```

### CI Integration
- Runs nightly at 3 AM UTC
- Manual trigger via workflow_dispatch
- Uploads test_reports/ as artifacts
- Continues on failure (graceful degradation)

## 3. Playground Mode

### Features
- **Snippet execution**: Run isolated commands without full programs
- **REPL mode**: Interactive command-by-command testing
- **State inspection**: View turtle position, heading, variables
- **Quick repro**: Minimal setup for bug reproduction

### Usage
```bash
# Execute inline code
python -m Platforms.Python.time_warp.playground \
  --language basic \
  --code '10 PRINT "Hello"'

# Execute from file
python -m Platforms.Python.time_warp.playground \
  --language pilot \
  --file snippet.pilot

# Interactive REPL
python -m Platforms.Python.time_warp.playground --repl --language logo
# Logo> FD 50
# Logo> RT 90
# Logo> FD 50
# Logo> reset  # Clear state
# Logo> exit
```

### Supported Languages
All Time Warp languages: BASIC, PILOT, Logo, Pascal, Prolog, C

## Implementation Details

### Interpreter Shim
`Scripts/interpreter_shim.py` provides a headless execution wrapper:
- Auto-detects language from file extension
- Graceful degradation if imports fail
- Captures text output as list of strings
- Supports input fixture feeding via `provide_input()`

### Golden Test Normalization
- Trailing whitespace trimmed per line
- Line endings normalized to `\n`
- Emoji prefixes preserved (❌, ✅, ℹ️, 🎨, 🚀)
- Turtle actions logged as text events (future enhancement)

### Known Issues & Future Work
1. **PILOT output duplication**: T: command emits each line twice (under investigation)
2. **Logo turtle logging**: Silent execution; spec calls for `🐢` event logs (not yet implemented)
3. **Determinism**: Randomness/time-based features need seed control for golden tests

## Testing

```bash
# Verify playground
python -m Platforms.Python.time_warp.playground \
  --language basic \
  --code '10 PRINT "Test"'

# Run CI script
python Scripts/ci_run_examples.py --languages basic pilot logo

# Check outputs
ls test_reports/*.out
cat test_reports/basic_hello_world.out
```

## Documentation
- Spec: `Core_Spec/language.md` (Golden Tests section)
- Examples: `Examples/golden/README.md`
- Workflow: `.github/workflows/nightly.yml`
