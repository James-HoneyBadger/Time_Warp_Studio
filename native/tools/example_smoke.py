#!/usr/bin/env python3
"""Runs every example program through the language bridge and reports the result.

The C++ suite (`python_bridge_tests`) covers the protocol and the drawing for one
program per language. This covers the *corpus*: all 60 examples, in all ten
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
import os
import shutil
import pathlib
import subprocess
import sys

from harness import locate_check, repository_root

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
    "forth": "fs",
}

# Languages that are expected to draw nothing *as a language*: PILOT is a
# dialogue language whose examples ask questions and print answers.
TEXT_ONLY = {"pilot"}

# Examples the reference engine cannot run, because they use what the native
# ports added and the reference never had: the numbered `SCREEN` modes, sprite
# `GET`/`PUT`, `POINT`. They are not skipped silently -- `lang_tests` runs each
# of them through the C++ engine and asserts it draws, which is the gate this
# file uses too, just on the other side of the port.
NATIVE_ONLY = {"basic/star_patrol.bas"}

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
    # The ten showcases added with the 16.1.0 example collection: each one
    # draws as well as prints, so a silent run is a broken run for all of them.
    "apl/showcase.apl",
    "basic/showcase.bas",
    "chip8/showcase.ch8",
    "comal/showcase.comal",
    "focal/showcase.foc",
    "logo/showcase.logo",
    "pilot/showcase.pilot",
    "rexx/showcase.rexx",
    "scheme/showcase.scm",
    "smalltalk/showcase.st",
    "forth/showcase.fs",
    "forth/turtle.fs",
}


# What counts as ink. A clear empties the recording, so the total is what is on
# the canvas at the end rather than a count of every frame that streamed past --
# the difference used to make an identical drawing report hundreds either way.
DRAWN_KINDS = ("line", "poly", "rect", "point", "ellipse", "text")


# What counts as ink. A clear empties the recording, so the total is what is on
# the canvas at the end rather than a count of every frame that streamed past --
# the difference used to make an identical drawing report hundreds either way.
DRAWN_KINDS = ("line", "poly", "rect", "point", "ellipse", "text")


def locate_check(root: pathlib.Path) -> pathlib.Path | None:
    """The built runner: beside this checkout, or wherever the caller put it."""
    override = os.environ.get("TIME_WARP_CHECK")
    if override:
        candidate = pathlib.Path(override)
        return candidate if candidate.is_file() else None
    for candidate in (
        root / "native" / "build" / "time_warp_check",
        root / "native" / "build" / "time_warp_check.exe",
        root / "native" / "build-debug" / "time_warp_check",
    ):
        if candidate.is_file():
            return candidate
    found = shutil.which("time_warp_check")
    return pathlib.Path(found) if found else None


def run_one(
    check: pathlib.Path,
    language: str,
    source: str,
    timeout: float,
) -> dict:
    """Runs one program and returns what came back.

    The runner is `time_warp_check`, which runs the program through the same
    native engines the IDE runs. It replaced a spawn of `native/bridge/worker.py`
    for the reason the engine was deleted: every language has an executor, so
    asking a second interpreter what a program did was asking the most expensive
    possible source for an answer this build already had.

    The result still says how much ink there is, because "it ran and drew
    nothing" is the failure this file exists to catch.
    """
    try:
        completed = subprocess.run(
            [str(check), "--language", language, "--json", "-"],
            input=source,
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
            kinds = message.get("kinds", {})
            strokes = sum(kinds.get(name, 0) for name in DRAWN_KINDS)
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
            "error": f"the run produced no result ({reason})",
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
    check = locate_check(root)
    if check is None:
        print(
            "time_warp_check was not found; build it with"
            " `cmake --build native/build --target time_warp_check`",
            file=sys.stderr,
        )
        return 1

    counts: dict[str, dict[str, int]] = collections.defaultdict(
        lambda: {"passed": 0, "failed": 0, "strokes": 0}
    )
    failures: list[str] = []
    ran = 0

    for language, extension in LANGUAGES.items():
        for path in sorted((root / "Examples").glob(f"**/*.{extension}")):
            identifier = f"{language}/{path.name}"
            if identifier in NATIVE_ONLY:
                # Gated by `lang_tests` instead: these use commands the reference
                # engine never implemented, so running them here would measure
                # the reference's gaps rather than the example.
                continue
            ran += 1
            result = run_one(
                check,
                language,
                path.read_text(encoding="utf-8"),
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
