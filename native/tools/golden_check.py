#!/usr/bin/env python3
"""The ports against the frozen reference corpus.

`native/tests/golden/lang_corpus.json.gz` holds the reference engine's answers to
470 programs: what each printed, every drawing call in order, and the variables it
left behind. This compares the native ports with those answers, and imports no
Python to do it -- the corpus carries the program text as well as the answer, which
is what makes the gate survive the engine it was recorded from.

    python3 native/tools/golden_check.py
    python3 native/tools/golden_check.py --language basic --verbose

#### Why this is the gate now

Until 17.2 there were three differential harnesses: expression, Scheme and
language. Two of them imported an interpreter from `native/bridge/` and ran the
same program twice to compare. That is a stronger check than comparing with a
recording -- it notices a bug in both engines at once -- and it stopped being
possible at 18.0 for the reason the port happened at all: the engine is gone.

What is left is honest about being weaker. The corpus was recorded from the
reference and is frozen; a change that makes the ports agree with a *wrong*
recording cannot be detected here. `--record` is therefore gone too: there is
nothing left to record from. If a language's behaviour is deliberately changed, the
corpus entry is edited by hand, with the change in the same commit -- which is the
same discipline the rest of the suite already asks for.

Scheme is here too, in its own corpus (`scheme_corpus.json.gz`, 192 programs,
recorded from the reference before it was deleted), because the Scheme differential
harness imported the engine as well. Its corpus keeps the *reference's* answer, not
the port's, so the 27 programs where the two were documented to differ are listed
with the reference's text and a `divergence` note: the port's answer for those is
printed rather than compared.
"""

from __future__ import annotations

import argparse
import gzip
import hashlib
import json
import re
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
CLI = REPO_ROOT / "native" / "build" / "lang_cli"
GOLDEN_PATH = REPO_ROOT / "native" / "tests" / "golden" / "lang_corpus.json.gz"
SCHEME_PATH = REPO_ROOT / "native" / "tests" / "golden" / "scheme_corpus.json.gz"
SCHEME_CLI = REPO_ROOT / "native" / "build" / "scheme_cli"

#: Programs whose answer depends on the machine rather than on the engine.
#: `STICK`/`STRIG`/`JOYINIT` read `/dev/input/js*`: a plugged-in joystick is not a
#: language difference. The corpus does not contain these rows -- they were skipped
#: when they were recorded -- and the check skips them again so an entry added by
#: hand cannot be compared against a machine's answer.
HARDWARE_MARKERS = {
    "JOYINIT": "reads a gamepad",
    "STICK(": "reads a gamepad",
    "STRIG(": "reads a gamepad",
}

_NUMBER = re.compile(r"[+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?")


def normalise_variables(
    values: dict[str, tuple[str, str] | list[str]],
) -> dict[str, tuple[str, str]]:
    """One vocabulary for both sides.

    Python called 7.0 a `float` and showed it as `7.0`; the panel calls it a
    `number` and shows `7`, because the learner wrote `7`. The corpus was recorded
    from the first and the port answers with the second, so the comparison is on
    the value and on the word a person reads.
    """
    result: dict[str, tuple[str, str]] = {}
    for name, pair in values.items():
        kind, value = pair
        if kind in {"float", "number", "int", "boolean"}:
            result[name] = ("number", format(float(value), ".12g"))
        elif kind in {"str", "text"}:
            result[name] = ("text", value)
        else:
            result[name] = (kind, value)
    return result


def normalise(text: str) -> str:
    """One spelling for a number, so 50 and 50.0 compare equal."""

    def replace(match: re.Match[str]) -> str:
        try:
            return format(float(match.group(0)), ".12g")
        except ValueError:  # pragma: no cover - the regex only matches numbers
            return match.group(0)

    return _NUMBER.sub(replace, text)


def source_digest(source: str) -> str:
    """A fingerprint of the program an expectation was recorded from."""
    return hashlib.sha256(source.encode("utf-8")).hexdigest()


def read_corpus(path: Path) -> list[dict]:
    if path.suffix == ".gz":
        with gzip.open(path, "rt", encoding="utf-8") as handle:
            return json.load(handle)
    return json.loads(path.read_text(encoding="utf-8"))


def run_native(key: str, source: str) -> dict:
    """The port's answer, parsed from `lang_cli`'s three record kinds."""
    completed = subprocess.run(
        [str(CLI), key],
        input=source,
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    if completed.returncode != 0:
        raise RuntimeError(
            f"lang_cli exited {completed.returncode}: {completed.stderr.strip()}"
        )
    output: list[str] = []
    drawing: list[str] = []
    variables: dict[str, tuple[str, str]] = {}
    for line in completed.stdout.splitlines():
        parts = line.split("\t")
        if parts[0] == "OUT":
            output.append(normalise(parts[1] if len(parts) > 1 else ""))
        elif parts[0] == "DRAW":
            drawing.append(normalise(parts[1] if len(parts) > 1 else ""))
        elif parts[0] == "VAR" and len(parts) >= 4:
            variables[parts[1]] = (parts[2], parts[3])
    return {
        "output": output,
        "drawing": drawing,
        "variables": normalise_variables(variables),
    }


def compare(name: str, want: dict, got: dict) -> tuple[bool, str]:
    problems: list[str] = []
    if got["output"] != want["output"]:
        problems.append(
            f"output:\n  recorded: {want['output']}\n  native:   {got['output']}"
        )
    if got["drawing"] != want["drawing"]:
        want_drawing, got_drawing = want["drawing"], got["drawing"]
        first = next(
            (
                index
                for index, (a, b) in enumerate(zip(got_drawing, want_drawing))
                if a != b
            ),
            min(len(got_drawing), len(want_drawing)),
        )
        problems.append(
            "drawing differs at "
            f"{first}: recorded {want_drawing[first : first + 3]} "
            f"native {got_drawing[first : first + 3]}"
        )
    if got["variables"] != want["variables"]:
        problems.append(
            f"variables:\n  recorded: {want['variables']}\n  native:   {got['variables']}"
        )
    return not problems, f"{name}\n  " + "\n  ".join(problems)


def run_scheme(cli: Path, program: str) -> str:
    """The port's answer for a Scheme program: its whole output text."""
    completed = subprocess.run(
        [str(cli)],
        input=program,
        capture_output=True,
        text=True,
        timeout=120,
        check=False,
    )
    return completed.stdout


def check_scheme(path: Path, cli: Path) -> tuple[int, list[str], int]:
    """Compares `scheme_cli` with the recorded answers.

    Returns (agreed, failures, diverged). A program the reference and the port were
    documented to disagree on is counted as diverged and not compared: its recorded
    text is the reference's, and the whole point of the entry is that the port says
    something else for a reason written down beside it.
    """
    agree = 0
    diverged = 0
    failures: list[str] = []
    for entry in read_corpus(path):
        program = entry["program"]
        if entry.get("sha256") != hashlib.sha256(program.encode()).hexdigest():
            failures.append("scheme: a recorded program does not match its own digest")
            continue
        actual = run_scheme(cli, program)
        if entry.get("divergence"):
            diverged += 1
            if actual == entry["output"]:
                failures.append(
                    f"scheme: {program.strip()[:60]!r}\n  documented as a divergence "
                    f"({entry['divergence']}) and it now agrees; remove the note"
                )
            continue
        if actual != entry["output"]:
            failures.append(
                f"scheme: {program.strip()[:60]!r}\n"
                f"  recorded: {entry['output'].strip()!r}\n"
                f"  port:     {actual.strip()!r}"
            )
            continue
        agree += 1
    return agree, failures, diverged


def scheme_half(corpus: Path | None = None) -> int:
    """The Scheme verdict, printed and returned."""
    path = corpus or SCHEME_PATH
    if not path.exists():
        print(f"no Scheme corpus at {path}", file=sys.stderr)
        return 2
    if not SCHEME_CLI.is_file():
        print(
            f"{SCHEME_CLI} was not built; run `cmake --build native/build "
            f"--target scheme_cli`",
            file=sys.stderr,
        )
        return 2
    agree, failures, diverged = check_scheme(path, SCHEME_CLI)
    for failure in failures:
        print(f"DIFFER {failure}")
    total = agree + len(failures) + diverged
    print(
        f"scheme: {agree} agreed, {len(failures)} differed, {diverged} documented "
        f"divergence(s) ({total} programs)"
    )
    return 0 if not failures else 1


def hardware_reason(source: str) -> str | None:
    upper = source.upper()
    for marker, reason in HARDWARE_MARKERS.items():
        if marker in upper:
            return reason
    return None


def ported_languages() -> list[str]:
    """Which languages have a native executor, asked of the native side.

    `executor_for` is the only place that knows. Keeping a second list here meant a
    new port had to be registered twice, and forgetting the second copy looked like
    a language with no corpus.
    """
    return subprocess.run(
        [str(CLI), "--ported"], capture_output=True, text=True, check=True
    ).stdout.split()


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--verbose", action="store_true")
    parser.add_argument(
        "--language",
        action="append",
        help="only these languages (default: every one with a native executor)",
    )
    parser.add_argument(
        "--corpus",
        default=str(GOLDEN_PATH),
        help="the recorded answers to compare with",
    )
    parser.add_argument(
        "--scheme",
        action="store_true",
        help="compare the Scheme machine against its own recorded corpus",
    )
    parser.add_argument(
        "--all",
        action="store_true",
        help="the language corpus and the Scheme corpus, which is what CI runs",
    )
    args = parser.parse_args(argv)

    path = Path(args.corpus)
    if not path.is_absolute():
        path = REPO_ROOT / path
    if not path.exists():
        print(f"no corpus at {path}", file=sys.stderr)
        return 2
    if not CLI.is_file():
        print(
            f"{CLI} was not built; run `cmake --build native/build --target lang_cli`",
            file=sys.stderr,
        )
        return 2

    status = 0
    if args.scheme or args.all:
        scheme_status = scheme_half()
        if scheme_status != 0:
            return scheme_status
        if args.scheme and not args.all:
            return 0

    ported = ported_languages()
    chosen = args.language or ported
    missing = [key for key in chosen if key not in ported]
    if missing:
        print(
            f"no native executor for: {', '.join(missing)} "
            f"(ported: {', '.join(ported)})",
            file=sys.stderr,
        )
        return 2

    agree = 0
    diverged = 0
    failures: list[str] = []
    skipped: list[str] = []
    for entry in read_corpus(path):
        key = entry["language"]
        if key not in chosen:
            continue
        name = entry["name"]
        source = entry["source"]
        reason = hardware_reason(source)
        if reason is not None:
            skipped.append(f"{key}: {name} ({reason})")
            continue
        if entry.get("source_sha256") != source_digest(source):
            failures.append(
                f"{key}: {name}\n  the recorded source does not match its own "
                f"digest; the entry was edited without re-recording"
            )
            continue
        # The recorded side is compared as it stands: it was normalised once when
        # it was recorded, and `normalise` is not idempotent -- a hex colour
        # `#E1000E5` reads as the number 1000E5 and becomes `#E100000000` on a
        # second pass. Normalising both sides would have compared two different
        # manglings of the same colour and called it a difference.
        want = {
            "output": list(entry["output"]),
            "drawing": list(entry["drawing"]),
            "variables": normalise_variables(entry["variables"]),
        }
        actual = run_native(key, source)
        # An entry can be *documented* as one the port answers differently, the way
        # the Scheme corpus does. The recorded side stays the reference's; the note
        # says what changed and why. A note that has gone stale -- the port agreeing
        # again -- is a failure, so a divergence cannot quietly become a lie.
        if entry.get("divergence"):
            diverged += 1
            ok, _ = compare(name, want, actual)
            if ok:
                failures.append(
                    f"{key}: {name}\n  documented as a divergence "
                    f"({entry['divergence']}) and it now agrees; remove the note"
                )
            continue
        ok, detail = compare(name, want, actual)
        if ok:
            agree += 1
            if args.verbose:
                print(f"agree  {key}: {name}")
        else:
            failures.append(f"{key}: {detail}")

    for entry in skipped:
        print(f"skip   {entry}")
    for failure in failures:
        print(f"DIFFER {failure}")
    print(
        f"\n{agree} agreed, {len(failures)} differed, {len(skipped)} skipped, "
        f"{diverged} documented divergence(s) ({', '.join(chosen)})"
    )
    return 0 if not failures else 1


if __name__ == "__main__":
    raise SystemExit(main())
