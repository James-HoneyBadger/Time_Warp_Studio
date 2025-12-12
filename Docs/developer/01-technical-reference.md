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

Time Warp IDE centers on a Python (PySide6) application that orchestrates stateless language executors and renders UI (editor, output, turtle canvas). The Python implementation is the only actively maintained version; Browser and Windows2000 folders remain for historical/experimental reference.

- Executors are stateless processors: they accept commands and return string output. All UI state (turtle position, canvas) is maintained by the host application.
- Executors return results using standard emoji-prefixed messages: ❌ (errors), ✅ (success), ℹ️ (info), 🐢 (turtle actions), 🚀 (run events).
- A central interpreter routes source code or lines to the appropriate executor.

High level layout:

```
PySide6 UI <--> TimeWarpInterpreter <--> Language Executors
                 ^                   ^
                 |                   +-- BASIC, PILOT, Logo (+ experimental Pascal, Prolog, C)
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

Location: `Platforms/Python/time_warp/core/safe_expression_evaluator.py`

Key points:
- Supports a small, audited grammar (numbers, variables, basic operators)
- Uses sandboxing and a whitelist parser to avoid eval()
- Exposes `safe_eval(expression, variables)` returning a numeric result or a controlled error

When writing an executor that evaluates user-supplied arithmetic, call into the safe evaluator rather than using Python's eval/exec.

---

## 4. Canvas / Graphics API

Canvas support:

- Main GUI canvas (Qt for Python) is the authoritative renderer.
- Historical canvases in Windows2000 and Browser are retained for reference; they are not actively maintained.

Executors emit turtle actions with the `🐢` emoji prefix; the UI consumes these messages and performs updates. Executors must not manipulate UI directly.

---

## 5. Build, Packaging and Installer

Builds focus on the Python application. Windows2000 NSIS scripts exist for archival purposes and are not part of the active release pipeline.

---

## 6. CI / GitHub Actions

CI currently validates the Python implementation via pytest and coverage. Historical Windows2000 workflows may be present but are not actively used.

---

## 7. Tests & QA

Tests are in `Tests/` and cover unit, integration, and GUI workflows for the Python application.

Key testing strategies:
- Python: `pytest` + coverage

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
