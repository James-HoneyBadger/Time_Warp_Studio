#!/usr/bin/env python3
"""Run every command-table row through a persistent REPL session.

`command_matrix.py` proves that each command works when its row is a whole
program. The REPL is a different execution path: the submitter replaces the
program text on every entry, the interpreter keeps its bindings, and the
Smalltalk/CHIP-8 whole-program languages run each entry from scratch. A command
that works in a program but not in the bar is still a bug a user sees.

This tool drives the native API's `repl.*` methods, one fresh session per row, so
one row cannot hide another through leftover state. It judges rows with the same
expectations as `command_matrix.py` (`any`, `prints:TEXT`, `draws`, `error`,
`optional`).

Usage:
    python3 native/tools/repl_matrix.py
    python3 native/tools/repl_matrix.py --language basic --verbose
    python3 native/tools/repl_matrix.py --json /tmp/repl.json
"""

from __future__ import annotations

import argparse
import collections
import json
import pathlib
import re
import select
import subprocess
import sys
import time

ROOT = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))

from command_table import (  # noqa: E402  # pylint: disable=wrong-import-position,import-error
    ROWS,
    SCRATCH,
)

ERROR_MARKER = "\u274c"
#: A refusal from an optional backend is an acknowledgement, not a silent
#: failure. Keep this in step with `command_matrix.py`.
OPTIONAL_REFUSAL = re.compile(
    r"speech synthesis (?:is )?not available|not installed|no .{0,20}backend available",
    re.IGNORECASE,
)
ROW_TIMEOUT_SECONDS = 30.0


class ApiError(RuntimeError):
    """The API process failed or stopped speaking its protocol."""


class Api:
    """One `time_warp_api` process and one request at a time."""

    def __init__(self, binary: pathlib.Path) -> None:
        self.binary = binary
        self.process: subprocess.Popen[str] | None = None
        self.next_id = 0
        self.start()

    def start(self) -> None:
        """Start a fresh API process."""
        # The process lives until stop(); Popen's context manager cannot span the
        # requests this class makes.
        self.process = subprocess.Popen(  # pylint: disable=consider-using-with
            [str(self.binary)],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
            bufsize=1,
        )

    def stop(self) -> None:
        """Stop the API process, escalating to kill if it will not exit."""
        if self.process is None:
            return
        try:
            if self.process.poll() is None:
                self.process.terminate()
                self.process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            self.process.kill()
            self.process.wait(timeout=5)

    def call(
        self,
        method: str,
        params: dict | None = None,
        timeout: float = ROW_TIMEOUT_SECONDS,
    ) -> dict:
        """Send one JSON-RPC request and return its result or error object."""
        if self.process is None or self.process.poll() is not None:
            raise ApiError("the API process exited")
        self.next_id += 1
        request = {"jsonrpc": "2.0", "id": self.next_id, "method": method}
        if params is not None:
            request["params"] = params
        assert self.process.stdin is not None and self.process.stdout is not None
        self.process.stdin.write(json.dumps(request) + "\n")
        self.process.stdin.flush()

        ready, _, _ = select.select([self.process.stdout], [], [], timeout)
        if not ready:
            raise ApiError(f"{method}: no response within {timeout:g}s")
        line = self.process.stdout.readline()
        if not line:
            raise ApiError(f"{method}: the API closed stdout")
        response = json.loads(line)
        if "error" in response:
            return {"__error__": response["error"]}
        return response.get("result", {})

    def restart(self) -> None:
        """Replace a wedged process so the remaining rows can run."""
        self.stop()
        self.start()


def language_key(name: str) -> str:
    """A language name with punctuation and case removed."""
    return "".join(character for character in name.lower() if character.isalnum())


def judge(row, result: dict) -> tuple[bool, str]:
    """Decide whether one REPL result satisfies the row's expectation."""
    # Each expectation is a separate verdict; keeping them as early returns makes
    # the failure message the line that failed.
    # pylint: disable=too-many-return-statements
    if "__error__" in result:
        error = result["__error__"]
        return False, f"protocol error {error.get('code')}: {error.get('message', '')}"
    output = str(result.get("output", ""))
    error = str(result.get("error", ""))
    drawing = result.get("drawing") or {}
    commands = int(drawing.get("commands", 0) or 0)
    actions = int(drawing.get("actions", 0) or 0)
    reported = bool(error.strip()) or ERROR_MARKER in output
    silent = not output.strip() and commands == 0 and actions == 0 and not reported
    expect = row.expect

    if expect == "error":
        if not reported:
            return False, "expected a reported error, got silence"
        return True, ""

    # "any" means any observable result, including a reported error: optional
    # backends such as speech synthesis are allowed to say they are unavailable.
    if expect == "any":
        if silent:
            return (
                False,
                "the command produced nothing: no output, no drawing, no error",
            )
        return True, ""

    if expect == "optional":
        if silent:
            return (
                False,
                "the command produced nothing: no output, no drawing, no error",
            )
        if reported:
            detail = error.strip() or next(
                (line for line in output.splitlines() if ERROR_MARKER in line), ""
            )
            if OPTIONAL_REFUSAL.search(detail):
                return True, ""
            return False, f"optional backend failed unexpectedly: {detail[:120]}"
        return True, ""

    if reported:
        detail = error.strip() or next(
            (line for line in output.splitlines() if ERROR_MARKER in line), ""
        )
        return False, f"reported an error while expected to work: {detail[:120]}"

    if silent:
        return False, "the command produced nothing: no output, no drawing, no error"

    if expect == "draws" and commands == 0 and actions == 0:
        return False, "expected drawing, got none"

    if expect.startswith("prints"):
        wanted = expect.partition(":")[2]
        if wanted and wanted not in output:
            return False, f"expected output containing {wanted!r}, got {output[:100]!r}"
        if not output.strip():
            return False, "expected output, got none"

    return True, ""


def main(argv: list[str] | None = None) -> int:
    """Run the rows and return a process exit status."""
    # A CLI's linear setup/teardown is clearer than pushing state into helpers.
    # pylint: disable=too-many-locals,too-many-branches,too-many-statements
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--language", help="only this language (case-insensitive)")
    parser.add_argument("--verbose", action="store_true", help="print every row")
    parser.add_argument("--json", help="write the full results to this path")
    options = parser.parse_args(argv)

    binary = ROOT / "native" / "build" / "time_warp_api"
    if not binary.is_file():
        print(
            f"repl_matrix: {binary} was not built; run make -C native", file=sys.stderr
        )
        return 1
    if not binary.stat().st_mode & 0o111:
        print(f"repl_matrix: {binary} is not executable", file=sys.stderr)
        return 1

    wanted = language_key(options.language) if options.language else ""
    rows = [row for row in ROWS if not wanted or language_key(row.language) == wanted]
    if not rows:
        print(f"no rows for {options.language!r}", file=sys.stderr)
        return 1

    api = Api(binary)
    results: list[dict] = []
    failures: list[tuple] = []
    buckets: dict[str, dict[str, int]] = collections.defaultdict(
        lambda: {"passed": 0, "failed": 0}
    )
    started = time.monotonic()
    try:
        for index, row in enumerate(rows, 1):
            try:
                created = api.call(
                    "repl.create",
                    {"language": row.language.lower().replace("-", "")},
                )
                session = created.get("session")
                if not session:
                    raise ApiError(f"repl.create returned no session: {created!r}")
                try:
                    outcome = api.call(
                        "repl.eval",
                        {"session": session, "source": row.source},
                    )
                finally:
                    try:
                        api.call("repl.close", {"session": session}, timeout=5)
                    except (ApiError, json.JSONDecodeError):
                        pass
            except (ApiError, json.JSONDecodeError, OSError) as error:
                outcome = {"__error__": {"code": -1, "message": str(error)}}
                # A wedged request cannot be cancelled in-process; start a clean
                # server so the remaining rows still get a chance.
                api.restart()

            ok, why = judge(row, outcome)
            bucket = buckets[row.language]
            bucket["passed" if ok else "failed"] += 1
            results.append(
                {
                    "language": row.language,
                    "command": row.command,
                    "source": row.source,
                    "expect": row.expect,
                    "ok": ok,
                    "why": why,
                    "output": outcome.get("output", ""),
                    "drawing": (outcome.get("drawing") or {}).get("commands", 0),
                    "actions": (outcome.get("drawing") or {}).get("actions", 0),
                }
            )
            if ok:
                if options.verbose:
                    print(f"  ok   {row.language:10s} {row.command:28s} {row.expect}")
            else:
                failures.append((row, why, outcome))
                print(f"  FAIL {row.language:10s} {row.command:28s} {why}")
            if index % 50 == 0:
                print(f"  ... {index}/{len(rows)} rows", file=sys.stderr)
    finally:
        api.stop()
        (pathlib.Path.cwd() / SCRATCH).unlink(missing_ok=True)

    elapsed = time.monotonic() - started
    print()
    print(f"{'language':12s} {'passed':>7s} {'failed':>7s}")
    for language in sorted(buckets):
        bucket = buckets[language]
        print(f"{language:12s} {bucket['passed']:7d} {bucket['failed']:7d}")
    print(f"rows: {len(rows)}   elapsed: {elapsed:.1f}s")

    if options.json:
        pathlib.Path(options.json).write_text(
            json.dumps(results, indent=1), encoding="utf-8"
        )

    if failures:
        print()
        for row, why, outcome in failures[:30]:
            print(f"{row.language} / {row.command}: {why}")
            print(f"    source: {row.source!r}")
            print(f"    output: {str(outcome.get('output', ''))[:160]!r}")
        if len(failures) > 30:
            print(f"... and {len(failures) - 30} more")
        return 1

    print()
    print(f"all {len(rows)} rows behaved through the REPL")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
