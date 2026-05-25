#!/usr/bin/env python3
"""Sandbox worker script — run inside an isolated subprocess.

Reads a JSON payload from stdin:
  {"language": "BRAINFUCK", "source": "...", "variables": {...}}

Writes a JSON result to stdout:
  {"output": "...", "variables": {...}}

This script is designed to be a minimal, self-contained runner.  It uses
only the project's own executor modules — no UI, no Qt.
"""

from __future__ import annotations

import json
import sys
from pathlib import Path

# Make sure the Platforms/Python directory is on sys.path so imports work
# when the script is executed directly as a subprocess (not via -m).
_here = Path(__file__).resolve()
_platforms_python = _here.parents[3]  # Platforms/Python/
if str(_platforms_python) not in sys.path:
    sys.path.insert(0, str(_platforms_python))


class _StubTurtle:
    """Minimal TurtleState stub — drops all graphics commands silently."""
    pos_x: float = 0.0
    pos_y: float = 0.0
    angle: float = 90.0
    pen_down: bool = True
    pen_color: str = "#ffffff"
    pen_width: int = 1
    visible: bool = True
    commands: list = []
    fill_color: str = "#ffffff"
    is_filling: bool = False

    def __getattr__(self, name: str):
        # Return a no-op callable for any unknown attribute
        return lambda *a, **kw: None


class _StubInterpreter:
    """Minimal Interpreter stub with just enough API for executors."""

    def __init__(self, language_name: str, variables: dict) -> None:
        from time_warp.core.interpreter import Language
        try:
            self.language = Language[language_name]
        except KeyError:
            self.language = Language.BASIC

        self.variables: dict = dict(variables)
        self._output: list[str] = []
        self.debug_mode: bool = False
        self.debug_timeline = None
        self.input_values: list[str] = []
        self._input_idx: int = 0
        self.MAX_EXECUTION_TIME: float = 10.0

    # ------------------------------------------------------------------
    # API used by executors
    # ------------------------------------------------------------------

    def log_output(self, text: str) -> None:
        self._output.append(text)

    def request_input(self, prompt: str = "") -> str:
        if self._input_idx < len(self.input_values):
            val = self.input_values[self._input_idx]
            self._input_idx += 1
            return val
        return ""

    def get_variables(self) -> dict:
        return dict(self.variables)

    def set_variable(self, name: str, value) -> None:
        self.variables[name] = value

    def get_variable(self, name: str, default=None):
        return self.variables.get(name, default)

    # Stub out other commonly accessed attributes
    def __getattr__(self, name: str):
        return None


def main() -> None:
    raw = sys.stdin.read()
    try:
        payload = json.loads(raw)
    except json.JSONDecodeError as exc:
        sys.stdout.write(json.dumps({"output": f"❌ Worker: bad JSON input: {exc}\n", "variables": {}}))
        sys.exit(1)

    language_name: str = payload.get("language", "BASIC")
    source: str = payload.get("source", "")
    variables: dict = payload.get("variables", {})

    interpreter = _StubInterpreter(language_name, variables)
    turtle = _StubTurtle()

    try:
        from time_warp.core.interpreter import _WHOLE_PROGRAM_EXECUTORS, Language
        lang = Language[language_name]
        executor = _WHOLE_PROGRAM_EXECUTORS.get(lang)
        if executor is None:
            result_text = f"❌ No sandbox executor for {language_name}\n"
        else:
            result_text = executor(interpreter, source, turtle)  # type: ignore[arg-type]
    except KeyError:
        result_text = f"❌ Unknown language: {language_name}\n"
    except MemoryError:
        result_text = "❌ Out of memory\n"
    except RecursionError:
        result_text = "❌ Maximum recursion depth exceeded\n"
    except Exception as exc:  # noqa: BLE001
        result_text = f"❌ Sandbox worker error: {exc}\n"

    # Merge log_output lines with the direct return value
    prefix = "".join(interpreter._output)
    full_output = prefix + (result_text or "")

    sys.stdout.write(
        json.dumps({"output": full_output, "variables": interpreter.variables})
    )
    sys.exit(0)


if __name__ == "__main__":
    main()
