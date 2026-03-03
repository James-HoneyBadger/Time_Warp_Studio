"""
Test all demo/example programs across every supported language.

Each program is loaded into the interpreter and executed.  A test passes
when the program runs to completion without raising an unhandled exception
and without producing *only* error output (lines starting with ❌).

Programs that require interactive input (PILOT A: commands, BASIC INPUT)
may terminate early when no input callback is provided — that is acceptable.
"""

import os
from pathlib import Path

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
    ".py": Language.PYTHON,
    ".lua": Language.LUA,
    ".scm": Language.SCHEME,
    ".rkt": Language.SCHEME,
    ".cob": Language.COBOL,
    ".cbl": Language.COBOL,
    ".bf": Language.BRAINFUCK,
    ".asm": Language.ASSEMBLY,
    ".s": Language.ASSEMBLY,
    ".js": Language.JAVASCRIPT,
    ".f77": Language.FORTRAN,
    ".for": Language.FORTRAN,
    ".rex": Language.REXX,
    ".rexx": Language.REXX,
    ".st": Language.SMALLTALK,
    ".htalk": Language.HYPERTALK,
    ".hs": Language.HASKELL,
    ".apl": Language.APL,
    ".sql": Language.SQL,
    ".jcl": Language.JCL,
    ".cics": Language.CICS,
    ".sqr": Language.SQR,
    ".sqc": Language.SQR,
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

    # Provide a dummy input callback so INPUT/A: don't hang
    interp.input_callback = lambda prompt: "4"

    interp.load_program(source, language=language)
    output = interp.execute(turtle)

    # output is a list of strings — join for inspection
    full_output = "\n".join(output)

    # Allow empty output (some programs only draw graphics)
    # but if there IS output, not every single line should be an error
    if output:
        error_lines = [l for l in output if l.startswith("❌")]
        non_error_lines = [l for l in output if not l.startswith("❌")]
        # If *all* output lines are errors, that's a test failure
        if error_lines and not non_error_lines:
            pytest.fail(
                f"Program produced only errors ({len(error_lines)} lines):\n"
                + "\n".join(error_lines[:20])
            )
