"""
Interpreter shim to execute BASIC/PILOT/Logo programs headlessly.
Attempts to import the Python interpreter from Platforms/Python and run code,
returning captured textual output. If the interpreter is unavailable, it falls
back gracefully.
"""

from __future__ import annotations
import pathlib
from typing import Optional

# Best-effort import of the project's interpreter
try:
    # Expected structure based on project docs
    from Platforms.Python.time_warp.core.interpreter import Interpreter, Language
    from Platforms.Python.time_warp.graphics.turtle_state import TurtleState
except ImportError:
    Interpreter = None  # type: ignore
    TurtleState = None  # type: ignore
    Language = None  # type: ignore


EXT_TO_LANGUAGE = {
    ".bas": "basic",
    ".pilot": "pilot",
    ".logo": "logo",
}


def detect_language(
    path: pathlib.Path,
    override: Optional[str] = None,
) -> Optional[str]:
    """Return language name based on file extension or override.

    If `override` is provided, it is returned directly; otherwise we
    map the path suffix using `EXT_TO_LANGUAGE`.
    """
    if override:
        return override
    return EXT_TO_LANGUAGE.get(path.suffix.lower())


def run_program(
    language: str,
    program_path: pathlib.Path,
    inputs: Optional[str],
) -> str:
    """
    Run a program headlessly via the Time Warp interpreter, returning text output.
    If interpreter import fails, returns an empty string.
    Note: language parameter kept for API consistency; interpreter auto-detects.
    """
    _ = language  # Interpreter auto-detects language from source
    if Interpreter is None or TurtleState is None or Language is None:
        # Interpreter not importable in this environment; return empty output.
        return ""

    interpreter = Interpreter()
    turtle = TurtleState()
    source = program_path.read_text(encoding="utf-8")

    # Detect language from file extension
    detected_language = Language.from_extension(program_path.suffix)

    # Load the program with language hint
    try:
        interpreter.load_program(source, language=detected_language)
    except RuntimeError:
        return ""
    except AttributeError:
        return ""

    # Provide inputs up-front when supported
    if inputs:
        try:
            # Feed all input lines
            for line in inputs.splitlines():
                interpreter.provide_input(line)
        except RuntimeError:
            pass
        except AttributeError:
            pass

    # Execute program and capture output
    try:
        output_lines = interpreter.execute(turtle)
        output = "\n".join(output_lines)
    except RuntimeError:
        output = ""

    return output
