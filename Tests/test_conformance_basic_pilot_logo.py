"""Golden conformance tests for BASIC, PILOT, and Logo outputs.

Loads example programs, runs them headlessly, and compares output to
snapshots in `Tests/golden_snapshots/`.
"""

import pathlib
import sys
import pytest

# NOTE: This is a skeleton for cross-language golden tests.
# It expects golden examples and snapshots to exist at the paths below.

ROOT = pathlib.Path(__file__).resolve().parents[1]
EXAMPLES_DIR = ROOT / "Examples" / "golden"
FIXTURES_DIR = ROOT / "Examples" / "golden_fixtures"
SNAPSHOTS_DIR = ROOT / "Tests" / "golden_snapshots"

# Add ROOT to sys.path for interpreter_shim import
sys.path.insert(0, str(ROOT))

LANGUAGES = ["basic", "pilot", "logo"]


def load_file(path: pathlib.Path) -> str:
    """Read and return file contents as UTF-8 string."""
    with open(path, "r", encoding="utf-8") as f:
        return f.read()


def normalize_output(text: str) -> str:
    """Normalize output by trimming per-line trailing spaces and EOLs."""
    # Trim trailing whitespace per line and normalize EOLs
    lines = [line.rstrip() for line in text.splitlines()]
    result = "\n".join(lines)
    return result + ("\n" if text.endswith("\n") else "")


@pytest.mark.parametrize("language", LANGUAGES)
def test_golden_programs(language):
    """Run golden examples for a language and compare outputs to snapshots."""
    lang_dir = EXAMPLES_DIR / language
    assert lang_dir.exists(), f"Missing golden examples for {language}: {lang_dir}"
    ext = "bas" if language == "basic" else ("pilot" if language == "pilot" else "logo")
    programs = sorted(p.name for p in lang_dir.glob("*." + ext))
    assert programs, f"No golden programs found for {language}"

    for program in programs:
        program_path = lang_dir / program
        fixture_path = FIXTURES_DIR / language / (program.replace(".", "_") + ".in")
        snapshot_path = SNAPSHOTS_DIR / language / (program.replace(".", "_") + ".out")

        # Load inputs if available
        inputs = None
        if fixture_path.exists():
            inputs = load_file(fixture_path)

        # Execute program via interpreter API
        # Placeholder: replace with interpreter invocation when available in this repo
        actual_output = execute_headless(language, program_path, inputs)
        normalized_actual = normalize_output(actual_output)

        # Compare with snapshot
        assert (
            snapshot_path.exists()
        ), f"Missing snapshot for {language}/{program}: {snapshot_path}"
        expected_output = load_file(snapshot_path)
        normalized_expected = normalize_output(expected_output)

        assert normalized_actual == normalized_expected, (
            f"Output mismatch for {language}/{program}\n\n"
            f"=== Expected ===\n{normalized_expected}\n\n"
            f"=== Actual ===\n{normalized_actual}\n"
        )


# pylint: disable=wrong-import-position,import-error
from Scripts.interpreter_shim import run_program


def execute_headless(
    language: str,
    program_path: pathlib.Path,
    inputs: str | None,
) -> str:
    """
    Execute a program headlessly and return its text output.
    """
    return run_program(language, program_path, inputs)
