# Technical Reference — Time Warp IDE

This reference documents the internal architecture, public interfaces, and build/packaging details for Time Warp IDE.

Use this document if you are extending the core, writing a new language executor, or integrating a new platform.

---

## Table of Contents

1. Architecture Overview
2. Language Executor API
3. Safe Expression Evaluator
4. Canvas / Graphics API
5. Build, Packaging and Installer
6. CI / GitHub Actions
7. Tests & QA
8. Plugin System & Adding Languages
9. CLI & Headless Modes

---

## 1. Architecture Overview

Time Warp IDE is designed as a collection of stateless language executors that are orchestrated by a small runtime and a cross-platform UI. Implementations exist in multiple platforms (Rust, Python, Windows2000 (C), Browser/JS) but share these concepts:

- Executors are stateless processors: they accept commands and return string output. All UI state (turtle position, canvas) is maintained by the host application.
- Executors return results using standard emoji-prefixed messages: ❌ (errors), ✅ (success), ℹ️ (info), 🐢 (turtle actions), 🚀 (run events) — see the language executor API below.
- A central interpreter routes source code or lines to the appropriate executor.

High level layout:

```
App UI <--> Interpreter/Runner <--> Language Executors
              ^                  ^
              |                  +-- BASIC, PILOT, Logo, Pascal, Prolog, C
              +-- Canvas & IO (turtle, file/load/save)
```

---

## 2. Language Executor API

All language executor implementations follow the same interface: a small set of calls the host runtime uses to execute commands, reset state, and query capabilities.

Common interface (pseudo-Rust/Python):

Rust trait:
```rust
pub trait LanguageExecutor {
    fn execute_command(&mut self, command: &str) -> Result<String, String>;
    fn reset(&mut self);
}
```

Python equivalent:
```python
class LanguageExecutor:
    def execute_command(self, command: str) -> str:
        """Execute a command; return a newline-terminated string.

        String outputs use emoji prefixes to denote type:
        - ❌ errors
        - ✅ success
        - ℹ️ info
        - 🐢 turtle graphics actions (encoded for UI)
        - 🚀 runner events
        """
        raise NotImplementedError

    def reset(self):
        """Reset executor state (memory, variables) to initial defaults."""
        raise NotImplementedError
```

Executor responsibilities:
- Parse input for the language
- Return textual output and control messages
- Do NOT manage UI objects or persist canvas state

If adding a new executor, implement the interface above and register it with the host interpreter.

---

## 3. Safe Expression Evaluator

The project includes a safe expression evaluation module used by executors to evaluate arithmetic expressions without exposing the host to arbitrary code execution.

Location: `platforms/python/time_warp/core/safe_expression_evaluator.py`

Key points:
- Supports a small, audited grammar (numbers, variables, basic operators)
- Uses sandboxing and a whitelist parser to avoid eval()
- Exposes `safe_eval(expression, variables)` returning a numeric result or a controlled error

When writing an executor that evaluates user-supplied arithmetic, call into the safe evaluator rather than using Python's eval/exec.

---

## 4. Canvas / Graphics API

Two important canvases are supported:

- Main GUI canvas (Qt for Python, eframe painter for Rust)
- Retro Windows 2000 canvas and Browser canvas

Windows 2000 platform (C) exposes wrapper calls used by executors. Common functions include:

- Canvas_Forward(distance), Canvas_Back(distance)
- Canvas_Left(angle), Canvas_Right(angle)
- Canvas_SetXY(x, y), Canvas_SetBgColor(r,g,b)
- Canvas_Circle(radius), Canvas_DrawTurtle(), Canvas_Home(), Canvas_PenUp(), Canvas_PenDown()

Browser platform provides a `createTurtle()` factory in `Platforms/Browser/js/graphics.js` and `show()` / `hide()` helpers for canvas lifecycle management.

Executors should emit turtle actions with the `🐢` emoji prefix; the UI picks up these messages and performs UI updates. Do not manipulate UI directly from an executor.

---

## 5. Build, Packaging and Installer

Modern builds use cross-platform CI to produce native installers. The Windows 2000 native installer is built with NSIS.

Installer script: `Platforms/Windows2000/installer/timewarp.nsi`

Key details:
- NSIS script accepts `VERSION` and `OUTDIR` variables via `makensis -DVERSION=... -DOUTDIR=...`.
- Output filename: `TimeWarpIDE-Setup-<VERSION>.exe`—if OUTDIR is provided, the installer is written to `${OUTDIR}/TimeWarpIDE-Setup-<VERSION>.exe`.
- The packaged application binary (inside the installer) is `TimeWarpIDE.exe` (copied into the `dist/` directory before packaging).

Local packaging example:
```bash
# Ensure the application binary is built and placed in Platforms/Windows2000/dist
makensis -DOUTDIR=Platforms/Windows2000/dist -DVERSION=3.0.0 Platforms/Windows2000/installer/timewarp.nsi
```

---

## 6. CI / GitHub Actions

Two workflows manage Windows builds:

- `.github/workflows/build-windows2000.yml` — cross-compile on ubuntu-latest, windows-latest native build and smoke tests.
- `.github/workflows/release-windows2000.yml` — triggered on release/publication, produces and attaches Windows installer to GitHub Release.

Important notes for CI:
- The ubuntu job uses `gcc-mingw-w64-i686` cross toolchain and `makensis` to build NSIS installers.
- For Windows native builds the runner uses MSYS2 (mingw32) + NSIS and runs PowerShell-based smoke tests (silent install/uninstall tests).
- CI passes `-DVERSION=${{ github.event.release.tag_name }}` or a tag-based value so the installer filename matches the release tag.

---

## 7. Tests & QA

Tests are in `Tests/` and include unit tests, integration tests, and platform-specific smoke tests.

Key testing strategies:
- Python: `pytest` + coverage
- Browser: Node.js tests and optional Puppeteer end-to-end tests (require x86_64 runners)
- Windows 2000: PowerShell smoke tests for silent install/uninstall and basic execution

Tips:
- Keep unit tests small and deterministic
- Use fixtures to isolate interpreter state
- When adding new functionality, add both positive and negative tests to ensure robust error handling

---

## 8. Plugin System & Adding Languages

Plugins live under `plugins/`. Each plugin should register via `core/plugin_system.py` in Python or equivalent in other platform implementations.

To add a language:

1. Create a new executor (e.g., `core/interpreters/my_lang.py`) implementing the `LanguageExecutor` interface described above.
2. Register the new executor in the central interpreter (`core/interpreter.py`) so it can be selected from the Language menu.
3. Add syntax highlighting, file type associations, and examples.
4. Add unit tests demonstrating expected behavior and edge-cases.

Example template (Python):
```python
from . import LanguageExecutor

class MyLangExecutor(LanguageExecutor):
    def __init__(self, interpreter):
        self.interpreter = interpreter
        self.variables = {}

    def execute_command(self, command: str) -> str:
        # parse and execute
        return "✅ MyLang executed\n"

    def reset(self):
        self.variables.clear()
```

---

## 9. CLI & Headless Modes

Most platforms include a CLI/console runner for batch testing and CI.

Python example:
```bash
python platforms/python/time_warp_ide.py --headless --run examples/basic/hello_world.bas
```

Use headless mode in CI for unit test harnesses and automated testing of interpreter outputs.

---

If you need more detail about a specific subsystem (e.g., the Rust executor AST, or the Windows2000 canvas internals), open an issue or ask for a dedicated deep-dive; this document is intended as a complete-but-summarized technical reference for most contributor needs.
