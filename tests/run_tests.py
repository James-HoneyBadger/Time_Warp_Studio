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
GO_CLI = os.path.join(ROOT, "Time_Warp_Go", "cmd", "timewarp")
SPECS = os.path.join(ROOT, "tests", "cross_platform", "specs.yaml")


class Colors:
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
                cwd=os.path.join(ROOT, "Time_Warp_Go"),
            )
        except subprocess.CalledProcessError as e:
            print(f"{Colors.R}FAIL{Colors.X} go build failed: {e}")
            return False
    return True


def load_specs():
    try:
        import yaml  # type: ignore
    except Exception:
        msg = (
            f"{Colors.Y}SKIP{Colors.X} PyYAML not installed. "
            f"Install with: pip install pyyaml"
        )
        print(msg)
        return []
    with open(SPECS, "r", encoding="utf-8") as f:
        return yaml.safe_load(f)


def run_go_program(
    language: str, program: str
) -> tuple[str, list[tuple[float, float]]]:
    """Execute a multi-line program in a simplistic way per language.

    For now we feed the entire program line-by-line to the Go CLI which
    treats each line independently. TODO: replace with batch execution
    once Go executor adds multi-line parsing per language.
    """
    proc = subprocess.Popen(
        ["go", "run", "./cmd/timewarp"],
        cwd=os.path.join(ROOT, "Time_Warp_Go"),
        stdout=subprocess.PIPE,
        stdin=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    out_chunks: list[str] = []
    for line in program.strip().split("\n"):
        assert proc.stdin is not None
        proc.stdin.write(line + "\n")
    if proc.stdin:
        proc.stdin.close()
    stdout, stderr = proc.communicate()
    if stderr:
        out_chunks.append(stderr)
    out_chunks.append(stdout)
    return "".join(out_chunks), []


def compare_text(expected: str, actual: str) -> tuple[bool, str]:
    exp = expected.replace("\r", "").strip()
    if expected and not expected.endswith("\n"):
        exp += "\n"
    act = actual.replace("\r", "")
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


if __name__ == "__main__":
    sys.exit(main())
