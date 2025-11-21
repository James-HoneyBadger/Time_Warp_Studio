# Cross-Platform Test Harness

This folder contains a lightweight, Python-based test harness to verify BASIC/Logo/PILOT behavior across available platform implementations.

- Uses the Go CLI in `platforms/go/cmd/timewarp` as the reference executable (runnable on Linux).
- Defines language-agnostic test cases in YAML.
- Produces a concise report (PASS/FAIL/SKIP) with diffs on mismatch.

## Quick run

```bash
python3 tests/run_tests.py
```

## Adding tests

- Edit `tests/cross_platform/specs.yaml`
- Each case defines a `language`, `program` and expected `output` (and optional `points` count for Logo).
