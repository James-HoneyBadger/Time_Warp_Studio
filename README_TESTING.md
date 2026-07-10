# Running Tests (Docker)

If your local environment is missing `pip` / `venv`, use the included Docker test runner to run the full pytest suite.

Fast smoke test (no pytest or Docker required):

```bash
python Platforms/Python/smoke_test.py
```

Build and run (from project root):

```bash
./docker/run_tests.sh
```

This builds an image that installs the project's core and dev requirements and runs:

```bash
pytest -q Platforms/Python/time_warp/tests
```

If you prefer to run tests locally, ensure your system has `python3-venv` and `pip` installed, then:

```bash
sudo apt update
sudo apt install -y python3-venv python3-pip
python3 -m venv .venv
.venv/bin/python -m pip install --upgrade pip setuptools wheel
.venv/bin/pip install -r Platforms/Python/requirements-dev.txt
.venv/bin/pytest -q Platforms/Python/time_warp/tests
```
