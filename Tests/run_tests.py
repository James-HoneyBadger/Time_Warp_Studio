#!/usr/bin/env python3
"""Cross-platform test harness (initial).

Currently uses the Go interpreter as an executable oracle.
Future: plug in other platform runners (via config) and compare outputs.
"""
from __future__ import annotations

import difflib
import os
import shutil
import subprocess
import sys


ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
GO_CLI = os.path.join(ROOT, "platforms", "go", "cmd", "timewarp")
SPECS = os.path.join(ROOT, "tests", "cross_platform", "specs.yaml")


class Colors:  # pylint: disable=too-few-public-methods
    G = "\x1b[32m"
    R = "\x1b[31m"
    Y = "\x1b[33m"
    B = "\x1b[34m"
    X = "\x1b[0m"


def ensure_go_built() -> bool:
    if shutil.which("go") is None:
        print(f"{Colors.Y}SKIP{Colors.X} go toolchain missing; skipping build")
        return False
    exe = GO_CLI
    # Build if main binary absent
    if not os.path.exists(exe):
        print("Building Go CLI...")
        try:
            subprocess.check_call(
                ["go", "build", "./..."],
                cwd=os.path.join(ROOT, "platforms", "go"),
            )
        except subprocess.CalledProcessError as e:
            print(f"{Colors.R}FAIL{Colors.X} go build failed: {e}")
            return False
    return True


def load_specs():
    try:
        import importlib  # pylint: disable=import-outside-toplevel

        yaml = importlib.import_module("yaml")  # type: ignore
    except ModuleNotFoundError:
        msg = (
            f"{Colors.Y}SKIP{Colors.X} PyYAML not installed. "
            f"Install with: pip install pyyaml"
        )
        print(msg)
        return []
    with open(SPECS, "r", encoding="utf-8") as f:
        return yaml.safe_load(f)


def run_go_program(
    _language: str, program: str
) -> tuple[str, list[tuple[float, float]]]:
    """Execute a multi-line program in a simplistic way per language.

    For now we feed the entire program line-by-line to the Go CLI which
    treats each line independently. TODO: replace with batch execution
    once Go executor adds multi-line parsing per language.
    """
    with subprocess.Popen(
        ["go", "run", "./cmd/timewarp"],
        cwd=os.path.join(ROOT, "platforms", "go"),
        stdout=subprocess.PIPE,
        stdin=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    ) as proc:
        out_chunks: list[str] = []
        # Feed lines via communicate to avoid flush errors if build fails early
        stdout, stderr = proc.communicate(input=program)
        if stderr:
            out_chunks.append(stderr)
        out_chunks.append(stdout)
    raw = "".join(out_chunks)
    return normalize_output(raw), []


def normalize_output(raw: str) -> str:
    """Strip prompts/banners and emoji prefixes for comparison.

    Keeps semantic output only so expected specs are clean across platforms.
    """
    lines = []
    for line in raw.splitlines():
        t = line.strip()
        if not t:
            continue
        if t.startswith("ℹ️") or t.startswith(">"):
            # Drop banner / prompt
            continue
        # Remove common emoji prefixes completely for graphics commands
        if t and t[0] in {"🚀", "🔄", "🔙", "📝", "🎨", "🐢", "✏️", "⭕", "📏", "📍"}:
            # Skip emoji-only lines (turtle graphics movements)
            continue
        # Remove success prefix from remaining lines
        if t.startswith("✅ "):
            t = t[2:].strip()
            t = t.lstrip()
        lines.append(t)
    if not lines:
        return ""
    return "\n".join(lines) + "\n"


def compare_text(expected: str, actual: str) -> tuple[bool, str]:
    # Preserve expected newlines; don't strip trailing newline from YAML block
    exp = expected.replace("\r", "")
    if expected and not expected.endswith("\n"):
        exp += "\n"
    act = actual.replace("\r", "")
    # Handle truly empty output
    if not exp.strip() and not act.strip():
        return True, ""
    # Normalize trailing newlines
    act = act if act.endswith("\n") else act + "\n"
    if exp == act:
        return True, ""
    diff = difflib.unified_diff(
        exp.splitlines(keepends=True),
        act.splitlines(keepends=True),
        fromfile="expected",
        tofile="actual",
    )
    return False, "".join(diff)


def main() -> int:
    # pylint: disable=too-many-locals,too-many-branches
    if not os.path.exists(SPECS):
        print("Spec file missing; abort")
        return 2
    specs = load_specs()
    built = ensure_go_built()
    total = 0
    passed = 0
    failed = 0
    skipped = 0
    for case in specs:
        total += 1
        cid = case.get("id", "(no-id)")
        lang = case.get("language")
        prog = case.get("program", "")
        expect_out = case.get("expect", {}).get("output", "")
        if not built:
            print(f"{Colors.Y}SKIP{Colors.X} {cid} (go build unavailable)")
            skipped += 1
            continue
        if lang == "BASIC":
            actual_out = run_basic_batch(prog)
            _pts: list[tuple[float, float]] = []
        elif lang == "PILOT":
            actual_out = run_pilot_batch(prog)
            _pts = []
        else:
            actual_out, _pts = run_go_program(lang, prog)
        ok, diff = compare_text(expect_out, actual_out)
        if ok:
            print(f"{Colors.G}PASS{Colors.X} {cid}")
            passed += 1
        else:
            print(f"{Colors.R}FAIL{Colors.X} {cid}")
            if diff:
                for line_ in diff.splitlines():
                    if line_.startswith("+") and not line_.startswith("+++"):
                        print(f"{Colors.G}{line_}{Colors.X}")
                    elif line_.startswith("-") and not line_.startswith("---"):
                        print(f"{Colors.R}{line_}{Colors.X}")
                    else:
                        print(line_)
            failed += 1
    summary = (
        f"\nSummary: {passed} passed, {failed} failed, {skipped} "
        f"skipped, {total} total"
    )
    print(summary)
    return 0 if failed == 0 else 1


def run_basic_batch(program: str) -> str:
    """Execute a multi-line BASIC program using Go CLI batch mode."""
    if shutil.which("go") is None:
        return ""
    try:
        with subprocess.Popen(
            ["go", "run", "./cmd/timewarp", "--batch", "BASIC"],
            cwd=os.path.join(ROOT, "platforms", "go"),
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        ) as proc:
            assert proc.stdin is not None
            # Use communicate(input=...) to avoid flush errors on closed stdin
            stdout, stderr = proc.communicate(input=program)
    except (OSError, FileNotFoundError) as e:
        return f"❌ spawn failed: {e}\n"
    raw = stderr + stdout
    return normalize_output(raw)


def run_pilot_batch(program: str) -> str:
    """Execute a multi-line PILOT program using Go CLI batch mode."""
    if shutil.which("go") is None:
        return ""
    try:
        with subprocess.Popen(
            ["go", "run", "./cmd/timewarp", "--batch", "PILOT"],
            cwd=os.path.join(ROOT, "platforms", "go"),
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        ) as proc:
            assert proc.stdin is not None
            stdout, stderr = proc.communicate(input=program)
    except (OSError, FileNotFoundError) as e:
        return f"❌ spawn failed: {e}\n"
    raw = stderr + stdout
    return normalize_output(raw)


if __name__ == "__main__":
    sys.exit(main())
