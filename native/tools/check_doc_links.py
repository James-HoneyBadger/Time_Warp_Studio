#!/usr/bin/env python3
# SPDX-License-Identifier: MIT
"""Check that every relative link in the documentation resolves.

The documentation is a set of books that link to each other, and a dead link is
how a reader discovers that the map is wrong. This is the cheap half of honesty:
every relative path must exist, and every `#anchor` must name a heading in the
target file, using the same slug GitHub builds.

Scope: the files given on the command line, or `docs/**/*.md`, `README.md`
and `Examples/CATALOG.md`.
External links (`http:`, `https:`, `mailto:`) are not fetched -- a network check
belongs in CI's own step, not in a script a contributor runs offline. Links inside
fenced code blocks and inline code are ignored, because a listing is not a link.

Exit status is 1 when anything is broken, 0 otherwise, so it can gate a workflow.
"""

from __future__ import annotations

import re
import sys
import unicodedata
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]

LINK = re.compile(r"!?\[[^\]]*\]\(([^)\s]+)(?:\s+\"[^\"]*\")?\)")
HEADING = re.compile(r"^(#{1,6})\s+(.*?)\s*#*\s*$")
FENCE = re.compile(r"^\s*(```|~~~)")
INLINE_CODE = re.compile(r"`[^`]*`")


def slug(text: str) -> str:
    """GitHub's heading anchor: lower case, punctuation dropped, spaces to '-'."""
    text = text.replace("`", "")
    text = re.sub(r"<[^>]+>", "", text)
    text = text.strip().lower()
    kept = []
    for character in text:
        if character.isalnum() or character in "-_ ":
            kept.append(character)
        elif unicodedata.category(character).startswith("P"):
            continue
    return "".join(kept).replace(" ", "-")


def anchors(path: Path) -> set[str]:
    """Every anchor a Markdown file offers, duplicates numbered as GitHub does."""
    seen: dict[str, int] = {}
    result: set[str] = set()
    in_fence = False
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except (OSError, UnicodeDecodeError):
        return result
    for line in lines:
        if FENCE.match(line):
            in_fence = not in_fence
            continue
        if in_fence:
            continue
        match = HEADING.match(line)
        if match is None:
            continue
        base = slug(match.group(2))
        count = seen.get(base, 0)
        seen[base] = count + 1
        result.add(base if count == 0 else f"{base}-{count}")
    return result


def links_in(path: Path) -> list[tuple[int, str]]:
    """Every link target in a file, with its line number. Fences are skipped."""
    found: list[tuple[int, str]] = []
    in_fence = False
    for number, line in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        if FENCE.match(line):
            in_fence = not in_fence
            continue
        if in_fence:
            continue
        for match in LINK.finditer(INLINE_CODE.sub("", line)):
            found.append((number, match.group(1)))
    return found


def documents() -> list[Path]:
    if len(sys.argv) > 1:
        return [Path(argument) for argument in sys.argv[1:]]
    files = sorted((ROOT / "docs").rglob("*.md"))
    return [ROOT / "README.md", ROOT / "Examples" / "CATALOG.md", *files]


def main() -> int:
    failures: list[str] = []
    anchor_cache: dict[Path, set[str]] = {}
    checked = 0

    for document in documents():
        if not document.is_file():
            failures.append(f"{document}: is not a file")
            continue
        for number, target in links_in(document):
            if re.match(r"^[a-zA-Z][a-zA-Z0-9+.-]*:", target):
                continue
            checked += 1
            path_text, _, fragment = target.partition("#")
            if not path_text:
                destination = document
            else:
                destination = (document.parent / path_text).resolve()
                if not destination.exists():
                    failures.append(f"{document}:{number}: missing {path_text}")
                    continue
            if not fragment or destination.is_dir():
                continue
            if destination.suffix.lower() != ".md":
                continue
            if destination not in anchor_cache:
                anchor_cache[destination] = anchors(destination)
            if fragment not in anchor_cache[destination]:
                failures.append(
                    f"{document}:{number}: no #{fragment} in {destination.name}"
                )

    for failure in failures:
        print(f"❌ {failure}")
    if failures:
        print(f"doc links: {len(failures)} broken of {checked} checked")
        return 1
    print(f"doc links: {checked} checked, all resolve")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
