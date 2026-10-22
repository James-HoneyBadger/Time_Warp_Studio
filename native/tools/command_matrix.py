#!/usr/bin/env python3
"""Examine every language command, one at a time, through the real bridge.

The corpus gate (`example_smoke.py`) answers "do the example programs run".
This answers the much finer question: *does each command do something?*

The rule it enforces is the one that matters for this project. A command must
produce **output, or drawing, or a reported error**. Silence is the failure mode
users report as "nothing happens", and it is invisible to every other gate here:
a program that quietly does nothing still exits zero. (The mandala bug was
exactly this -- 2,232 strokes of geometry dropped without a word.)

So each row declares a command, the smallest source that invokes it, and what is
expected:

    any     something happens: output, drawing, or an error -- but not silence
    prints  output appears (optionally naming the text that must be in it)
    draws    at least one drawing command reaches the canvas
    error   the engine *reports* the problem rather than ignoring it
    optional
            the command needs a backend that is an optional dependency. Output,
            drawing, or the documented "not available" refusal all count; any
            other error (or silence) is still a failure.

Rows marked ``error`` are the adversarial half: a command given nothing, or the
wrong thing, must say so.

Coverage is reported against the extracted inventory, so a command that is
implemented and neither tested nor explicitly waived shows up as a gap rather
than passing unnoticed.

Usage:
    python3 native/tools/command_matrix.py                 # everything
    python3 native/tools/command_matrix.py --language logo # one language
    python3 native/tools/command_matrix.py --verbose       # list every row
"""

from __future__ import annotations

import argparse
import collections
import json
import os
import pathlib
import re
import subprocess
import sys
import threading

BRIDGE = pathlib.Path(__file__).resolve().parents[1] / "bridge"
sys.path.insert(0, str(BRIDGE))

from time_warp.tests.command_table import ROWS, SCRATCH, WAIVERS  # noqa: E402

from harness import locate_check, repository_root  # noqa: E402

#: A command that hangs must fail the run rather than wedge it.
ROW_TIMEOUT_SECONDS = 20.0


#: A command that hangs must fail the run rather than wedge it.
ROW_TIMEOUT_SECONDS = 20.0


def repository_root() -> pathlib.Path:
    return pathlib.Path(__file__).resolve().parents[2]


class Worker:
    """One `time_warp_check` process per row.

    This used to keep a long-lived `native/bridge/worker.py` alive and drive it
    over the documented protocol. The engine is gone, so each row is a program and
    each program is a process: the report arrives as the same `out`/`canvas`/`done`
    messages, from the engines the IDE actually runs.

    The name is kept because a row's result is still "what the worker said", and
    renaming the seam would rewrite twenty call sites to no purpose.
    """

    def __init__(self, check: pathlib.Path, timeout: float = 180.0) -> None:
        self._check = check
        self._timeout = timeout

    def run(self, language: str, source: str, name: str) -> dict:
        """Runs one source and returns what came back."""
        try:
            completed = subprocess.run(
                [str(self._check), "--language", language, "--json", "-"],
                input=source,
                capture_output=True,
                text=True,
                timeout=self._timeout,
                check=False,
            )
        except subprocess.TimeoutExpired:
            return {
                "output": "",
                "drawings": 0,
                "errors": [f"{name} timed out after {self._timeout:.0f}s"],
                "prompts": 0,
                "done": {"ok": False, "error": "timed out"},
                "malformed": [],
            }

        output: list[str] = []
        drawings = 0
        errors: list[str] = []
        malformed: list[str] = []
        done: dict | None = None
        for line in completed.stdout.splitlines():
            if not line.strip():
                continue
            try:
                message = json.loads(line)
            except json.JSONDecodeError:
                # The protocol is JSON lines: anything else is a defect worth
                # surfacing rather than skipping over.
                malformed.append(line[:200])
                continue
            kind = message.get("t")
            if kind == "out":
                output.append(str(message.get("text", "")))
            elif kind == "canvas":
                # Ink plus the turtle operations that change the drawing without
                # adding to it: a row is "the command did something", and `RIGHT
                # 90` is a command that did something.
                drawings += sum(int(count) for count in message.get("kinds", {}).values())
                drawings += int(message.get("actions", 0))
            elif kind == "err":
                errors.append(str(message.get("text", "")))
            elif kind == "done":
                done = message

        if done is None:
            detail = completed.stderr.strip().splitlines()
            errors.append(detail[-1] if detail else "the run produced no result")

        return {
            "output": "".join(output) + "\n" if output else "",
            "drawings": drawings,
            "errors": errors,
            "prompts": 0,
            "done": done,
            "malformed": malformed,
        }

    def close(self) -> None:
        """Nothing to close: a run is a process that has already exited."""

def close(self) -> None:
        self._process.terminate()
        try:
            self._process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            self._process.kill()


#: The project's error convention: interpreter errors are returned as output
#: lines prefixed with this, not as protocol 'err' messages. A harness that only
#: watched 'err' would call every reported error a silent failure.
ERROR_MARKER = "\u274c"

#: What an optional backend says when it is not installed. A row expecting
#: `optional` accepts this refusal as an acknowledgement: the command ran and
#: reported the truth, rather than pretending a speaker moved when there is none.
OPTIONAL_REFUSAL = re.compile(
    r"speech synthesis (?:is )?not available|not installed|no .{0,20}backend available",
    re.IGNORECASE,
)


def _normalise(literal: str) -> str:
    """Compares command spellings that differ only in whitespace or a colon.

    The dispatchers accept ``PRINT  #`` as well as ``PRINT #``, and a command can
    follow the statement separator, so those are the same command.
    """
    return " ".join(literal.strip().lstrip(":").split()).upper()


def judge(row, result: dict) -> tuple[bool, str]:
    """Decides whether a row behaved. Returns (ok, why-not)."""
    if result["malformed"]:
        return False, "the worker emitted a line that is not JSON"

    done = result["done"]
    if done is None:
        return False, "no 'done' message: the run did not finish"

    reported = (
        bool(result["errors"])
        or bool(done.get("error"))
        or ERROR_MARKER in result["output"]
    )
    silent = not result["output"].strip() and result["drawings"] == 0 and not reported
    expect = row.expect

    if expect == "error":
        # An error must be *reported*: the point of the row is that the engine
        # says something rather than ignoring bad input.
        if not reported:
            return False, "expected a reported error, got silence"
        return True, ""

    if expect == "optional":
        if reported:
            detail = next(
                (
                    line
                    for line in (
                        [*result["errors"]]
                        + [
                            text
                            for text in result["output"].splitlines()
                            if ERROR_MARKER in text
                        ]
                    )
                    if line.strip()
                ),
                "",
            )
            if OPTIONAL_REFUSAL.search(detail):
                return True, ""
            return (
                False,
                f"optional backend failed unexpectedly: {detail.strip()[:120]}",
            )
        if silent:
            return (
                False,
                "the command produced nothing: no output, no drawing, no error",
            )
        return True, ""

    if reported:
        # A row that expects the command to work cannot pass while the engine
        # reported an error alongside it: JOYINIT's row used to pass on the
        # strength of an error message that happened to contain the right words.
        detail = (result["errors"] or [str(done.get("error", ""))])[0]
        if not detail:
            detail = next(
                (
                    line
                    for line in result["output"].splitlines()
                    if ERROR_MARKER in line
                ),
                "",
            )
        return (
            False,
            f"reported an error while expected to work: {detail.strip()[:120]}",
        )

    if done.get("ok") is False:
        detail = (result["errors"] or [str(done.get("error", ""))])[0]
        return False, f"failed: {detail[:120]}"

    if silent:
        return False, "the command produced nothing: no output, no drawing, no error"

    if expect == "draws" and result["drawings"] == 0:
        return False, "expected drawing, got none"

    if expect.startswith("prints"):
        wanted = expect.partition(":")[2]
        if wanted and wanted not in result["output"]:
            return (
                False,
                f"expected output containing {wanted!r}, got {result['output'][:80]!r}",
            )
        if not result["output"].strip():
            return False, "expected output, got none"

    return True, ""


def _language_key(name: str) -> str:
    """A language name with case and punctuation removed, for comparisons."""
    return re.sub(r"[^a-z0-9]", "", name.lower())


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--language", help="only this language (case-insensitive)")
    parser.add_argument(
        "--verbose", action="store_true", help="print every row, not just failures"
    )
    parser.add_argument(
        "--explain",
        action="store_true",
        help="print what each row actually did, for authoring new rows from evidence",
    )
    parser.add_argument("--json", help="write the full results to this path")
    options = parser.parse_args(argv)

    # Compared with punctuation removed, because a row labels its language the way
    # a person writes it ("CHIP-8") while the inventory keys it by module name
    # ("chip8"). `--language chip8` should find both.
    wanted = _language_key(options.language) if options.language else ""
    rows = [r for r in ROWS if not wanted or _language_key(r.language) == wanted]
    if not rows:
        print(f"no rows for {options.language!r}", file=sys.stderr)
        return 1

    root = repository_root()
    worker = locate_check(root)
    if worker is None:
        print(
            "time_warp_check was not found; build it with"
            " `cmake --build native/build --target time_warp_check`",
            file=sys.stderr,
        )
        return 1
    by_language: dict[str, dict[str, int]] = collections.defaultdict(
        lambda: {"passed": 0, "failed": 0}
    )
    failures: list[tuple] = []
    results: list[dict] = []

    bridge = Worker(worker)
    try:
        for row in rows:
            outcome = bridge.run(row.language, row.source, f"matrix:{row.command}")
            ok, why = judge(row, outcome)
            bucket = by_language[row.language]
            bucket["passed" if ok else "failed"] += 1
            results.append(
                {
                    "language": row.language,
                    "command": row.command,
                    "source": row.source,
                    "expect": row.expect,
                    "ok": ok,
                    "why": why,
                    "prompts": outcome["prompts"],
                }
            )
            if ok:
                if options.verbose:
                    print(f"  ok   {row.language:6s} {row.command:16s} {row.expect}")
            else:
                failures.append((row, why, outcome))
                print(f"  FAIL {row.language:6s} {row.command:16s} {why}")
            if options.explain:
                # Observed, not assumed: this is how a new row's expectation gets
                # written down instead of guessed.
                seen = outcome["output"].replace("\n", "\u23ce").strip()[:70]
                notes = [f"out={seen!r}"] if seen else []
                if outcome["drawings"]:
                    notes.append(f"draw={outcome['drawings']}")
                if outcome["errors"]:
                    notes.append(f"err={outcome['errors'][0][:40]!r}")
                if outcome["prompts"]:
                    notes.append(f"prompts={outcome['prompts']}")
                print(
                    f"       {row.command:18s} expect={row.expect:14s} "
                    f"{'  '.join(notes) or 'silent'}"
                )
    finally:
        bridge.close()

    # The file-channel rows write one scratch file. The engine's file service
    # resolves a relative name against the working directory, so running this from
    # the repository root would otherwise leave litter behind. The rows exist to
    # prove the round trip works, so the harness tidies up after itself.
    (pathlib.Path.cwd() / SCRATCH).unlink(missing_ok=True)

    print()
    print(f"{'language':10s} {'passed':>7s} {'failed':>7s}")
    for language in sorted(by_language):
        bucket = by_language[language]
        print(f"{language:10s} {bucket['passed']:7d} {bucket['failed']:7d}")

    print(f"rows: {len(rows)}   waived: {len(WAIVERS)}")

    # The completeness check: against the commands the executors actually
    # dispatch on, report what is neither exercised nor waived. The inventory is
    # a candidate list (it cannot tell a command from a punctuation mark), so a
    # gap here is a prompt to look, not proof of a defect.
    if not options.language:
        import command_inventory

        inventory = command_inventory.collect()
        for language, entry in sorted(inventory.items()):
            implemented = set(entry["dispatch_literals"]) | set(entry["table_commands"])
            effective: set[str] = set()
            for row in ROWS:
                # The label is prose ("IF / ELSE / END IF"), so the command it is
                # filed under is its first word; `also` catches the rest.
                head = row.command.split()
                if head:
                    effective.add(_normalise(head[0]))
                effective.add(_normalise(row.command))
                effective.update(_normalise(extra) for extra in row.also)
            effective.update(_normalise(w.command) for w in WAIVERS)
            missing = {
                name for name in implemented if _normalise(name) not in effective
            }
            print(
                f"  {language}: {len(missing)} candidate(s) neither tested nor waived"
            )
            if options.verbose:
                for name in sorted(missing):
                    print(f"      {name}")

    if options.json:
        pathlib.Path(options.json).write_text(json.dumps(results, indent=1))

    if failures:
        print()
        for row, why, outcome in failures[:20]:
            print(f"{row.language} / {row.command}: {why}")
            print(f"    source: {row.source!r}")
            print(f"    output: {outcome['output'][:120]!r}")
            if outcome["prompts"]:
                print(f"    input prompts: {outcome['prompts']}")
        if len(failures) > 20:
            print(f"... and {len(failures) - 20} more")
        return 1

    print()
    print(f"all {len(rows)} command rows behaved")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
