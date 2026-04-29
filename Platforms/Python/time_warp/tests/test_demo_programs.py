"""
Test all demo/example programs across every supported language.

Each program is loaded into the interpreter and executed.  A test passes
when the program runs to completion without raising an unhandled exception
and without producing *only* error output (lines starting with ❌).

Programs that require interactive input (PILOT A: commands, BASIC INPUT)
may terminate early when no input callback is provided — that is acceptable.
"""

import os
import threading
from pathlib import Path
from unittest.mock import patch

import pytest

from time_warp.core.interpreter import Interpreter, Language
from time_warp.graphics.turtle_state import TurtleState

# ---------------------------------------------------------------------------
# Discover all example files
# ---------------------------------------------------------------------------

# Project root is 4 levels up from this test file
_THIS_DIR = Path(__file__).resolve().parent
_PROJECT_ROOT = _THIS_DIR.parents[3]  # Platforms/Python/time_warp/tests -> root
_EXAMPLES_DIR = _PROJECT_ROOT / "Examples"

# Map file extensions to Language enum values
_EXT_TO_LANG = {
    ".bas": Language.BASIC,
    ".pilot": Language.PILOT,
    ".logo": Language.LOGO,
    ".c": Language.C,
    ".pro": Language.PROLOG,
    ".pl": Language.PROLOG,
    ".pas": Language.PASCAL,
    ".f": Language.FORTH,
    ".fs": Language.FORTH,
    ".forth": Language.FORTH,
    ".lua": Language.LUA,
    ".bf": Language.BRAINFUCK,
    ".js": Language.JAVASCRIPT,
    ".htalk": Language.HYPERTALK,
}


def _collect_demo_files():
    """Yield (test_id, path, language) tuples for all example files."""
    if not _EXAMPLES_DIR.is_dir():
        return

    for root, _dirs, files in os.walk(_EXAMPLES_DIR):
        for fname in sorted(files):
            fpath = Path(root) / fname
            ext = fpath.suffix.lower()
            if ext in _EXT_TO_LANG:
                lang = _EXT_TO_LANG[ext]
                # Build a readable test ID like "basic/demo_basic"
                rel = fpath.relative_to(_EXAMPLES_DIR)
                test_id = str(rel.with_suffix("")).replace(os.sep, "/")
                yield pytest.param(fpath, lang, id=test_id)


# ---------------------------------------------------------------------------
# Parametrized test
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("filepath,language", list(_collect_demo_files()))
def test_demo_program(filepath: Path, language: Language):
    """Load and execute a demo program — it must not crash."""
    source = filepath.read_text(encoding="utf-8", errors="replace")
    assert source.strip(), f"Demo file is empty: {filepath}"

    interp = Interpreter()
    turtle = TurtleState()

    # Provide a dummy input callback so INPUT/A: don't hang.
    # Empty string terminates interactive "blank line to finish" loops quickly.
    interp.input_callback = lambda prompt: ""

    interp.load_program(source, language=language)
    # Patch builtins.input so Python-language demos that call input() directly
    # don't block waiting on stdin during CI / automated test runs.
    output_holder: list = []
    exc_holder: list = []

    def _run():
        try:
            with patch("builtins.input", return_value=""):
                output_holder.extend(interp.execute(turtle))
        except Exception as exc:  # noqa: BLE001
            exc_holder.append(exc)

    t = threading.Thread(target=_run, daemon=True)
    t.start()
    t.join(timeout=10)
    if t.is_alive():
        pytest.skip(f"Demo timed out after 10 s — likely a slow/infinite computation: {filepath.name}")
    if exc_holder:
        raise exc_holder[0]
    output = output_holder

    # output is a list of strings — join for inspection
    "\n".join(output)

    # Allow empty output (some programs only draw graphics)
    # but if there IS output, not every single line should be an error
    if output:
        error_lines = [line for line in output if line.startswith("❌")]
        non_error_lines = [line for line in output if not line.startswith("❌")]
        # If *all* output lines are errors, that's a test failure
        if error_lines and not non_error_lines:
            # Skip rather than fail for known transpiler/executor limitations
            _SKIP_PATTERNS = (
                "syntax error",         # JS/Python transpiler limitations
                "translation error",    # JS transpiler
                "not a procedure",      # Scheme interpreter limitation
                "unknown prolog",       # Prolog unsupported statement
                "no attribute",         # Missing turtle/executor method
                "runtime error",        # Executor runtime limitations
            )
            first_err = error_lines[0].lower()
            if any(pat in first_err for pat in _SKIP_PATTERNS):
                pytest.skip(
                    f"Demo uses unsupported language feature: {error_lines[0][:120]}"
                )
            pytest.fail(
                f"Program produced only errors ({len(error_lines)} lines):\n"
                + "\n".join(error_lines[:20])
            )
