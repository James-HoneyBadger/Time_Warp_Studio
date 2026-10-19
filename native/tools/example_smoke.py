#!/usr/bin/env python3
"""Runs every example program through the language bridge and reports the result.

The C++ suite (`python_bridge_tests`) covers the protocol and the drawing for one
program per language. This covers the *corpus*: all 49 examples, in all ten
languages, checking that each one runs and that the ones that draw actually draw.

That distinction matters because the two failures look alike from a unit test and
differently in use. A broken protocol fails a unit test loudly. A language whose
turtle commands never reach the canvas runs "successfully" and produces an empty
picture -- which is precisely the report this tool was written in response to.

Usage:

    python3 native/tools/example_smoke.py [--verbose]

Exit code is 0 when every example runs, 1 otherwise. Drawing counts are printed
per language so a regression that silently stops one language drawing is visible
as a number that fell, not as a green run.
"""

from __future__ import annotations

import argparse
import collections
import json
import pathlib
import subprocess
import sys

# Language key -> the extension its examples use. The keys are the ones the C++
# side sends, which are the settings keys from `native/src/ui/language.cpp`.
LANGUAGES = {
    "basic": "bas",
    "pilot": "pilot",
    "logo": "logo",
    "comal": "comal",
    "rexx": "rexx",
    "scheme": "scm",
    "focal": "foc",
    "chip8": "ch8",
    "apl": "apl",
    "smalltalk": "st",
}

# Languages that are expected to draw nothing *as a language*: PILOT is a
# dialogue language whose examples ask questions and print answers.
TEXT_ONLY = {"pilot"}

# The examples that must produce drawing, listed rather than inferred.
#
# Most examples are text programs -- a budget tracker, a statistics drill, a
# quiz -- and asserting that they draw would be asserting something false. But
# "the program ran and the picture is empty" is the exact failure this file was
# written for, so the examples that *do* draw are named here and a stroke count of
# zero for any of them is a failure.
#
# The list is a deliberate assertion, in the same spirit as the differential
# harnesses' documented divergences: when a new drawing example is added, it is
# added here, and the number next to it is what the run has to beat.
DRAWING_EXAMPLES = {
    "basic/math_explorer.bas",
    "basic/space_simulation.bas",
    "logo/community_campus.logo",
    "logo/fractal_forest.logo",
    "logo/fractal_gallery.logo",
    "logo/galaxy.logo",
    "logo/geometric_artistry.logo",
    "logo/hello.logo",
    "logo/mandala.logo",
    "logo/spirograph.logo",
    "logo/turtle_3d.logo",
    "comal/turtle_art.comal",
    "rexx/turtle_spiral.rexx",
    "scheme/turtle_polygons.scm",
    "focal/square.foc",
    "chip8/font_and_frames.ch8",
    "chip8/hires.ch8",
    "chip8/paint.ch8",
    "apl/arrays_and_turtle.apl",
    "smalltalk/shapes.st",
}


def repository_root() -> pathlib.Path:
    """The checkout this script lives in, so paths do not depend on the cwd."""
    return pathlib.Path(__file__).resolve().parents[2]


def run_one(
    interpreter: str,
    worker: pathlib.Path,
    language: str,
    source: str,
    name: str,
    timeout: float,
) -> dict:
    """Runs one program and returns what came back."""
    request = json.dumps(
        {"t": "run", "language": language, "source": source, "name": name}
    )
    try:
        completed = subprocess.run(
            [interpreter, str(worker)],
            input=request + "\n",
            capture_output=True,
            text=True,
            timeout=timeout,
            check=False,
        )
    except subprocess.TimeoutExpired:
        return {
            "ok": False,
            "strokes": 0,
            "error": f"timed out after {timeout:.0f}s",
            "output": "",
        }

    strokes = 0
    output = []
    done = None
    errors = []
    for line in completed.stdout.splitlines():
        try:
            message = json.loads(line)
        except ValueError:
            # Not protocol: something else wrote to stdout. Worth reporting rather
            # than skipping, because it means a message was corrupted.
            errors.append(f"non-protocol output: {line[:80]}")
            continue
        kind = message.get("t")
        if kind == "canvas":
            for command in message.get("cmds", []):
                drawn = command.get("k")
                if drawn == "clear":
                    # A CLEARSCREEN discards everything drawn so far. Counting every
                    # frame instead made this total depend on how many intermediate
                    # frames happened to stream before each clear, so it wavered by
                    # hundreds between identical runs and could not be compared with
                    # anything. What matters is the ink that is there at the end.
                    strokes = 0
                elif drawn in ("line", "poly", "rect", "point", "ellipse", "text"):
                    # Every drawing primitive counts, not just the line ones.
                    # The corpus used only lines and polylines when this metric
                    # was written, so the narrower test was invisible; a
                    # pixel-graphics language draws filled rectangles, and its
                    # ink was then not seen at all. Widening it changed no
                    # existing total -- 5639, 7840, 29, 67, 43 all stand -- so
                    # it is the same measurement, made complete.
                    strokes += 1
        elif kind == "out":
            output.append(message.get("text", ""))
        elif kind == "err":
            errors.append(message.get("text", ""))
        elif kind == "done":
            done = message

    if done is None:
        detail = completed.stderr.strip().splitlines()
        reason = detail[-1] if detail else "no diagnostics"
        return {
            "ok": False,
            "strokes": strokes,
            "error": f"the worker produced no result ({reason})",
            "output": "".join(output),
        }
    if not done.get("ok", False) or errors:
        return {
            "ok": False,
            "strokes": strokes,
            "error": done.get("error") or errors[0],
            "output": "".join(output),
        }
    return {"ok": True, "strokes": strokes, "error": "", "output": "".join(output)}


def main(argv: list[str] | None = None) -> int:
    """Runs every example and returns 0 only when all of them behaved."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--verbose", action="store_true", help="print every program, not just failures"
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=90.0,
        help="seconds allowed per program (the interpreter has its own 10s budget)",
    )
    options = parser.parse_args(argv)

    root = repository_root()
    worker = root / "native" / "bridge" / "worker.py"
    if not worker.is_file():
        print(f"the language worker was not found at {worker}", file=sys.stderr)
        return 1

    # The repository's virtual environment first, because that is where this
    # project's dependencies live; `python3` otherwise. The two names are kept
    # apart so the type is a `str` at every use, not a Path that sometimes turns
    # into one.
    interpreter_path = root / ".venv/bin/python3"
    interpreter = str(interpreter_path) if interpreter_path.is_file() else "python3"

    counts: dict[str, dict[str, int]] = collections.defaultdict(
        lambda: {"passed": 0, "failed": 0, "strokes": 0}
    )
    failures: list[str] = []
    ran = 0

    for language, extension in LANGUAGES.items():
        for path in sorted((root / "Examples").glob(f"**/*.{extension}")):
            ran += 1
            result = run_one(
                interpreter,
                worker,
                language,
                path.read_text(encoding="utf-8"),
                path.name,
                options.timeout,
            )
            counts[language]["strokes"] += result["strokes"]

            # A program that did not run at all is the first thing to report, and
            # it is a different failure from one that ran and produced no drawing.
            if not result["ok"]:
                counts[language]["failed"] += 1
                failures.append(f"{language}/{path.name}: {result['error']}")
                continue

            # Drawing is expected only where it is real: see DRAWING_EXAMPLES.
            identifier = f"{language}/{path.name}"
            must_draw = identifier in DRAWING_EXAMPLES
            if must_draw and result["strokes"] == 0:
                counts[language]["failed"] += 1
                failures.append(
                    f"{identifier}: ran but drew nothing, and it is expected to draw"
                )
                continue
            if not must_draw and language in TEXT_ONLY and result["strokes"] != 0:
                counts[language]["failed"] += 1
                failures.append(
                    f"{identifier}: expected no drawing, got {result['strokes']} strokes"
                )
                continue

            counts[language]["passed"] += 1
            if options.verbose:
                print(f"  ok  {identifier}  ({result['strokes']} strokes)")

    print(f"{'language':10s} {'passed':>6s} {'failed':>6s} {'strokes':>8s}")
    for language in LANGUAGES:
        entry = counts[language]
        print(
            f"{language:10s} {entry['passed']:6d} {entry['failed']:6d} {entry['strokes']:8d}"
        )

    if failures:
        print(f"\n{len(failures)} failure(s):", file=sys.stderr)
        for failure in failures:
            print(f"  {failure}", file=sys.stderr)
        return 1

    print(f"\nall {ran} example programs ran")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
