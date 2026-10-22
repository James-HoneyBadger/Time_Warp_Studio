#!/usr/bin/env python3
"""Checks what the documentation *claims*, not just what it links to.

`check_doc_links.py` proves every relative link and `#anchor` resolves. That is a
weaker property than it looks: a sentence can say "`locateWorker()` searches a fixed
order" and link to nothing, and the gate is silent. Three claims of that shape were
found by hand after 17.0 deleted the bridge — `docs/ARCHITECTURE.md` documented a
function that no longer existed, `native/README.md` said a harness protocol "still
lives in `native/bridge/worker.py`", and `docs/ROADMAP.md` owed an allow-list that
had been in `ports.cpp` for several releases.

There are three rules, each aimed at a specific way prose goes stale:

  1. A repository path named in backticks must exist. `native/src/ui/main_window.cpp`
     is a claim; if the file moved, the claim is false.
  2. A retired name must be discussed in the past tense. `PythonWorker`,
     `locateWorker`, `worker.py` and the rest are gone; a file may say they were
     deleted, and may not say they do something. A region between
     `<!-- history -->` and `<!-- /history -->` is exempt, because a record of what
     was deleted has to be allowed to name it.
  3. A `make <target>` in a command must be a target that exists, in the Makefile
     that would be consulted from where the command says it runs.

What it does not check: whether a sentence is *true*. A rule about truth is a rule
about nothing; these three are properties of the text a machine can settle, and
settling them is what stops the class of defect above.
"""

from __future__ import annotations

import argparse
import pathlib
import re
import subprocess
import sys

REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]

#: Names that no longer exist in this repository. A file may say they were
#: deleted; it may not use them in the present tense. Adding to this list is the
#: whole maintenance cost of the rule.
RETIRED = (
    "PythonWorker",
    "locateWorker",
    "timewarp_bridge",
    "python_bridge_tests",
    "worker.py",
    "TIME_WARP_WORKER",
    "TIME_WARP_PYTHON",
)

#: Verbs that make a mention a record rather than a claim. Deliberately small and
#: matched on the same line: "The worker classes (...) are deleted" passes,
#: "`python_bridge_tests` runs a program through the real worker" does not.
PAST_TENSE = re.compile(
    r"\b(was|were|is|are)?\s*(deleted|removed|gone|retired|dropped|replaced|went)\b"
    r"|\bused to\b|\bno longer\b|\bonce\b|\bhad been\b|\bno part of\b",
    re.IGNORECASE,
)

#: A region a document marks as history. `CHANGELOG.md` and the porting plan are
#: wholly historical, and say so at the top.
HISTORY_OPEN = "<!-- history -->"
HISTORY_CLOSE = "<!-- /history -->"

#: A line that names a path on purpose as an example -- "create
#: `native/bridge/.../my_lang.py`" in a procedure -- says so, rather than the gate
#: learning a list of placeholder words and missing the eleventh.
IGNORE = "<!-- doc-claims: ignore -->"

#: A file that is a record from top to bottom. `CHANGELOG.md` cannot describe a
#: removal without naming what was removed, so the alternative to this marker is a
#: gate that reports the changelog forever.
WHOLE_FILE = "<!-- history: all -->"

BACKTICK = re.compile(r"`([^`\n]+)`")
#: `file.cpp:120-140` and `test_module.py::test_name` are the two ways this
#: project cites a place inside a file.
LINE_SUFFIX = re.compile(r"(:\d+(-\d+)?|::\S+)$")
FENCE = re.compile(r"^\s*```")
MAKE_COMMAND = re.compile(r"^\s*make(?:\s+-C\s+(\S+))?\s+([A-Za-z0-9_.-]+)")


def tracked_files() -> list[pathlib.Path]:
    """Every tracked Markdown file, which is what a reviewer sees.

    `git ls-files` keeps the build trees, the virtual environment and
    `node_modules` out of the run without a second ignore file -- the same reason
    `lint-md` uses it.
    """
    completed = subprocess.run(
        ["git", "ls-files", "*.md"],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
        check=True,
    )
    return [REPO_ROOT / line for line in completed.stdout.splitlines() if line.strip()]


def make_targets(path: pathlib.Path) -> set[str]:
    """The targets a Makefile defines: its rules and its `.PHONY` list."""
    if not path.is_file():
        return set()
    targets: set[str] = set()
    variables: dict[str, list[str]] = {}
    lines = path.read_text(encoding="utf-8").splitlines()
    # `.PHONY: $(FORWARDED)` is how the top-level Makefile lists the targets it
    # forwards to `native/`, so a rule that ignored variables would call
    # `make test` a target that does not exist.
    for line in lines:
        match = re.match(r"^([A-Za-z_][A-Za-z0-9_]*)\s*[:?]?=\s*(.*)$", line)
        if match:
            variables[match.group(1)] = match.group(2).split()
    for line in lines:
        if line.startswith(".PHONY:"):
            for word in line.split(":", 1)[1].split():
                if word == "\\" or not word:
                    continue
                if word.startswith("$(") and word.endswith(")"):
                    targets.update(variables.get(word[2:-1], []))
                else:
                    targets.add(word)
            continue
        if line.startswith((" ", "\t", "#")) or not line.strip():
            continue
        if ":" in line and not line.startswith("."):
            name = line.split(":", 1)[0].strip()
            if name and "=" not in name and " " not in name:
                targets.add(name)
    return targets


def top_level_directories() -> set[str]:
    return {entry.name for entry in REPO_ROOT.iterdir() if entry.is_dir()}


def path_claim(token: str, top_level: set[str]) -> str | None:
    """The path a backticked token claims, or None when it is not a path claim.

    Conservative on purpose: a gate that fires on `--language basic` or on
    `turtle_state.py` (a package-relative name that has to be read in context) is a
    gate somebody switches off.
    """
    candidate = token.strip()
    if "/" not in candidate or " " in candidate:
        return None
    if any(character in candidate for character in "<>$*{}|"):
        return None
    if candidate.startswith(("http", "www", "file:", "~")):
        return None
    candidate = candidate.split("#", 1)[0]
    candidate = LINE_SUFFIX.sub("", candidate)
    first = candidate.split("/", 1)[0]
    if first not in top_level:
        return None
    return candidate.rstrip("/")


def check_text(
    text: str,
    top_level: set[str],
    root_targets: set[str],
    native_targets: set[str],
) -> list[tuple[int, str]]:
    """Returns (line number, message) for every false claim in one document."""
    problems: list[tuple[int, str]] = []
    in_history = WHOLE_FILE in text[:1000]
    in_fence = False

    for number, line in enumerate(text.splitlines(), start=1):
        if HISTORY_OPEN in line:
            in_history = True
            continue
        if HISTORY_CLOSE in line:
            in_history = False
            continue
        if FENCE.match(line):
            in_fence = not in_fence
            continue
        if IGNORE in line:
            continue
        if in_history:
            # A record has to be able to name what it records, including a path
            # that has since moved.
            continue

        for token in BACKTICK.findall(line):
            claimed = path_claim(token, top_level)
            if claimed is None:
                continue
            if not (REPO_ROOT / claimed).exists():
                problems.append((number, f"`{claimed}` does not exist"))

        if not in_history:
            for name in RETIRED:
                if name not in line:
                    continue
                if PAST_TENSE.search(line):
                    continue
                problems.append(
                    (number, f"`{name}` was deleted; this line says it is here")
                )

        if in_fence:
            match = MAKE_COMMAND.match(line)
            if match:
                directory, target = match.group(1), match.group(2)
                scoped = native_targets if directory and directory.rstrip("/").endswith("native") else root_targets
                # `make VAR=value something` and `make $@` are not target claims.
                if target and not target.startswith(("$", "-")) and target not in scoped:
                    problems.append(
                        (
                            number,
                            f"`make {target}` is not a target of "
                            + ("native/Makefile" if directory else "the root Makefile"),
                        )
                    )

    return problems


def check_file(
    path: pathlib.Path,
    top_level: set[str],
    root_targets: set[str],
    native_targets: set[str],
) -> list[tuple[int, str]]:
    return check_text(
        path.read_text(encoding="utf-8"), top_level, root_targets, native_targets
    )


def self_test() -> int:
    """Proves the three rules fire, and that both escapes suppress them.

    A gate that cannot be shown to fail is a gate nobody should trust: the whole
    reason this one exists is that a checker which only ever passes looks exactly
    like a repository with nothing wrong in it.
    """
    top = {"native", "docs"}
    targets = {"test"}
    cases = [
        ("a moved path", "see `native/src/ui/moved.cpp`", 1),
        ("a deleted name", "`PythonWorker` does it", 1),
        ("a name in the past tense", "`PythonWorker` was deleted", 0),
        ("a history region", "<!-- history -->\n`PythonWorker` does it\n<!-- /history -->", 0),
        ("a whole-file record", "<!-- history: all -->\n`PythonWorker` does it", 0),
        ("an example path", "`native/my_lang.py`<!-- doc-claims: ignore -->", 0),
        ("a make target that exists", "```\nmake test\n```", 0),
        ("a make target that does not", "```\nmake frobnicate\n```", 1),
        ("a non-path in backticks", "`--language basic`", 0),
    ]
    failures = 0
    for name, text, expected in cases:
        found = len(check_text(text, top, targets, targets))
        if found != expected:
            failures += 1
            print(f"  self-test: {name}: expected {expected}, got {found}")
    if failures:
        print(f"  {failures} self-test failure(s)", file=sys.stderr)
        return 1
    print(f"self-test: {len(cases)} cases")
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--quiet", action="store_true", help="report only the failures"
    )
    parser.add_argument(
        "--self-test", action="store_true", help="check the checker and exit"
    )
    options = parser.parse_args(argv)

    if options.self_test:
        return self_test()

    top_level = top_level_directories()
    root_targets = make_targets(REPO_ROOT / "Makefile")
    native_targets = make_targets(REPO_ROOT / "native" / "Makefile")

    files = tracked_files()
    failures = 0
    for path in files:
        for number, message in check_file(path, top_level, root_targets, native_targets):
            failures += 1
            print(f"{path.relative_to(REPO_ROOT)}:{number}: {message}")

    if failures:
        print(
            f"\n{failures} false claim(s). A path that moved, a name that was "
            f"deleted, or a target that does not exist: fix the sentence, or wrap "
            f"the record in {HISTORY_OPEN} ... {HISTORY_CLOSE}.",
            file=sys.stderr,
        )
        return 1
    if not options.quiet:
        print(f"doc claims: {len(files)} files, no false claims")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
