# Running Tests

Requirements: Python 3.10 or newer.

## Set up

From the repository root:

```bash
python3 -m venv .venv
.venv/bin/python -m pip install --upgrade pip
.venv/bin/pip install -r Platforms/Python/requirements.txt -r Platforms/Python/requirements-dev.txt
```

`make dev` installs the same dependencies using the Makefile's Python.

## Run the tests

```bash
# Full pytest suite (sets QT_QPA_PLATFORM=offscreen)
make test

# Same suite without make
QT_QPA_PLATFORM=offscreen .venv/bin/pytest -q Platforms/Python/time_warp/tests

# Quick smoke test (no pytest required)
python3 Platforms/Python/smoke_test.py

# Run every program in Examples/ and check the results
python3 test_all_examples.py

# Lint
.venv/bin/python -m ruff check Platforms/Python
```

Set `QT_QPA_PLATFORM=offscreen` when no display is available. The Qt tests need it on headless machines.
