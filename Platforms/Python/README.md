# Time Warp Studio - Python Desktop IDE

The desktop IDE for Time Warp Studio, built with PySide6 (Qt 6). It runs nine programming languages in one editor, with turtle graphics, a step debugger, and themes.

Languages: BASIC, PILOT, Logo, C, Pascal, Prolog, Forth, Brainfuck, Python.

Version: see [VERSION](../../VERSION).

## Requirements

- Python 3.10 or newer
- CPU with SSSE3, SSE4.1, SSE4.2, and POPCNT (required by Qt)
- Core dependencies: PySide6 (Qt 6) and Pillow

## Install and run

From the repository root:

```bash
pip install -r Platforms/Python/requirements.txt
python3 Platforms/Python/time_warp_ide.py
```

Or use the launcher, which checks for a `.venv` and sets up dependencies:

```bash
python3 run.py
```

Optional features (speech, gamepad input, OS theme detection) need extra packages. Each feature runs without its package:

```bash
pip install -r Platforms/Python/requirements-optional.txt
```

Installing the package with `pip install ./Platforms/Python` provides the commands `time-warp-ide`, `time-warp`, and `tw-editor`.

After the package is published to PyPI, install it with:

```bash
python3 -m pip install timewarp-ide
time-warp-ide
```

To install the published package directly from GitHub:

```bash
python3 -m pip install "https://github.com/James-HoneyBadger/Time_Warp_Studio/releases/download/v14.1.2/timewarp_ide-14.1.2-py3-none-any.whl"
time-warp-ide
```

## Tests

See [README_TESTING.md](../../docs/README_TESTING.md).

## Package layout

```text
Platforms/Python/
  time_warp_ide.py      Main entry point
  tw_editor.py          Alternate entry point (tw-editor command)
  smoke_test.py         Quick checks without pytest
  time_warp/
    core/               Interpreter, language registry, config, debugger, project manager, sandbox
    languages/          One executor module per language
    ui/                 Main window, editor, canvas, output, debug panel, themes, mixins
    graphics/           Turtle state
    features/           Lessons, autosave
    utils/              Expression evaluator, error hints, validators
    tests/              pytest suite
```

## Examples

The example programs are in [Examples/](../../Examples/). Start with:

- [Examples/basic/hello.bas](../../Examples/basic/hello.bas)
- [Examples/pilot/hello.pilot](../../Examples/pilot/hello.pilot)
- [Examples/logo/hello.logo](../../Examples/logo/hello.logo)
- [Examples/c/hello.c](../../Examples/c/hello.c)
- [Examples/forth/hello.f](../../Examples/forth/hello.f)

The catalog is [Examples/CATALOG.md](../../Examples/CATALOG.md).

## Documentation

- [README.md](../../README.md): project overview
- [docs/INDEX.md](../../docs/INDEX.md): documentation index
- [docs/USER_GUIDE.md](../../docs/USER_GUIDE.md): using the IDE
- [docs/LANGUAGE_GUIDE.md](../../docs/LANGUAGE_GUIDE.md): language reference
- [FAQ.md](../../docs/FAQ.md): common questions
- [ARCHITECTURE.md](../../docs/ARCHITECTURE.md): design
- [CONTRIBUTING.md](../../CONTRIBUTING.md): how to contribute
