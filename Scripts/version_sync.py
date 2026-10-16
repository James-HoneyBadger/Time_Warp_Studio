#!/usr/bin/env python3
"""Check, and optionally apply, the hand-written copies of the product version.

`VERSION` is the source of truth. The Qt application reads it at configure time, so
the binary, the window title and the About box cannot drift apart; what this script
looks after are the copies a person writes by hand -- the documentation and the
bridge package metadata.

Without `--write` nothing is modified and any disagreement makes the exit status 1,
which is what CI uses to refuse a change where the numbers no longer agree.
"""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from datetime import date
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]
VERSION_FILE = REPO_ROOT / "VERSION"


@dataclass(frozen=True)
class Copy:
    """A hand-written copy of the version, which this script can rewrite."""

    path: Path
    pattern: str
    replacement: str
    description: str


@dataclass(frozen=True)
class Requirement:
    """Something a release must have that no script should write for you.

    The changelog is deliberately not rewritable: its newest release heading is
    usually a version that has already shipped, so renaming it would rewrite
    history. A missing section is reported instead.
    """

    path: Path
    pattern: str
    description: str


def _read_version() -> str:
    version = VERSION_FILE.read_text(encoding="utf-8").strip()
    if not re.fullmatch(r"\d+\.\d+\.\d+", version):
        raise ValueError(f"Invalid VERSION value: {version!r}")
    return version


def _copies(version: str) -> list[Copy]:
    return [
        Copy(
            path=REPO_ROOT / "native" / "bridge" / "time_warp" / "__init__.py",
            pattern=r'(?m)^__version__ = ".*"$',
            replacement=f'__version__ = "{version}"',
            description="bridge package __version__",
        ),
        Copy(
            path=REPO_ROOT / "native" / "bridge" / "time_warp" / "__init__.py",
            pattern=r"Aligned with Time Warp Studio v[0-9.]+ release",
            replacement=f"Aligned with Time Warp Studio v{version} release",
            description="bridge package docstring version",
        ),
        # The README's only version marker is the badge. It used to end with a
        # "- **Version:**" line as well, and tracked that too, but the line was
        # dropped when the page was shortened -- which is what the "no match"
        # error below is for: a copy target that outlives its text stops the
        # release rather than passing unnoticed.
        Copy(
            path=REPO_ROOT / "README.md",
            pattern=r"Version-[0-9.]+-orange",
            replacement=f"Version-{version}-orange",
            description="README version badge",
        ),
        Copy(
            path=REPO_ROOT / "docs" / "ARCHITECTURE.md",
            pattern=r"(?m)^## Time Warp Studio v[0-9.]+ ",
            replacement=f"## Time Warp Studio v{version} ",
            description="architecture guide title",
        ),
    ]


def _requirements(version: str) -> list[Requirement]:
    return [
        Requirement(
            path=REPO_ROOT / "CHANGELOG.md",
            pattern=rf"(?m)^## \[{re.escape(version)}\] - ",
            description=f"a changelog section headed '## [{version}] - <date>'",
        ),
    ]


def _updated_text(copy: Copy, content: str) -> str:
    """The copy's new text, or the same text when it already agrees."""
    updated, count = re.subn(copy.pattern, copy.replacement, content, count=1)
    if count != 1:
        raise ValueError(
            f"no match for {copy.description} in "
            f"{copy.path.relative_to(REPO_ROOT)}: the file has changed shape"
        )
    return updated


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--write",
        action="store_true",
        help="write the copies, instead of only reporting that they are stale",
    )
    parser.add_argument(
        "--release-date",
        default=date.today().isoformat(),
        help="unused; kept so existing release scripts keep working",
    )
    args = parser.parse_args()

    version = _read_version()
    copies = _copies(version)

    # Read and match everything before writing anything. Writing as each target
    # was reached left a half-applied bump behind when a target went missing --
    # which is what happened when the AppStream and snapcraft files were deleted
    # and this script still named them.
    updates: list[tuple[Copy, str]] = []
    try:
        for copy in copies:
            content = copy.path.read_text(encoding="utf-8")
            if _updated_text(copy, content) != content:
                updates.append((copy, content))
    except (OSError, ValueError) as exc:
        print(f"❌ {exc}", file=sys.stderr)
        return 2

    if args.write:
        for copy, _content in updates:
            copy.path.write_text(_updated_text(copy, _content), encoding="utf-8")

    if updates:
        # The emoji reports whether a person has something left to do. After a
        # successful --write there is nothing outstanding, so it reads as a
        # success; without the flag the same state is a failure, and does.
        if args.write:
            print(f"✅ Brought {len(updates)} copy/copies up to VERSION ({version}).")
        else:
            print(f"❌ Stale: {len(updates)} copy/copies disagree with VERSION ({version}).")
        for copy, _content in updates:
            print(f"   - {copy.description} ({copy.path.relative_to(REPO_ROOT)})")
    else:
        print(f"✅ All {len(copies)} hand-written copies match {version}.")

    missing = 0
    for requirement in _requirements(version):
        try:
            content = requirement.path.read_text(encoding="utf-8")
        except OSError as exc:
            print(f"❌ {exc}", file=sys.stderr)
            missing += 1
            continue
        if not re.search(requirement.pattern, content):
            print(f"❌ Release paperwork missing: {requirement.description}", file=sys.stderr)
            missing += 1

    if missing:
        return 1
    if updates and not args.write:
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())