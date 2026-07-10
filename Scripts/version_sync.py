#!/usr/bin/env python3
"""Synchronize Time Warp Studio version metadata from the top-level VERSION file."""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]
VERSION_FILE = REPO_ROOT / "VERSION"


@dataclass(frozen=True)
class ReplacementTarget:
    path: Path
    pattern: str
    replacement: str
    description: str


def _read_version() -> str:
    version = VERSION_FILE.read_text(encoding="utf-8").strip()
    if not re.fullmatch(r"\d+\.\d+\.\d+", version):
        raise ValueError(f"Invalid VERSION value: {version!r}")
    return version


def _targets(version: str, release_date: str) -> list[ReplacementTarget]:
    return [
        ReplacementTarget(
            path=REPO_ROOT / "Platforms/Python/pyproject.toml",
            pattern=r'(?m)^version = ".*"$',
            replacement=f'version = "{version}"',
            description="pyproject version",
        ),
        ReplacementTarget(
            path=REPO_ROOT / "Platforms/Python/time_warp/__init__.py",
            pattern=r"Aligned with Time Warp Studio v[0-9.]+ release",
            replacement=f"Aligned with Time Warp Studio v{version} release",
            description="package docstring version",
        ),
        ReplacementTarget(
            path=REPO_ROOT / "Platforms/Python/time_warp/__init__.py",
            pattern=r'(?m)^__version__ = ".*"$',
            replacement=f'__version__ = "{version}"',
            description="package __version__",
        ),
        ReplacementTarget(
            path=REPO_ROOT / "packaging/linux/snapcraft.yaml",
            pattern=r"(?m)^version: '.*'$",
            replacement=f"version: '{version}'",
            description="snap version",
        ),
        ReplacementTarget(
            path=(
                REPO_ROOT / "packaging/linux/org.time-warp-studio.IDE.metainfo.xml"
            ),
            pattern=r'<release version="[0-9.]+" date="[0-9-]+">',
            replacement=f'<release version="{version}" date="{release_date}">',
            description="metainfo release tag",
        ),
        ReplacementTarget(
            path=(
                REPO_ROOT / "packaging/linux/org.time-warp-studio.IDE.metainfo.xml"
            ),
            pattern=r"v[0-9.]+ —",
            replacement=f"v{version} —",
            description="metainfo release note version",
        ),
    ]


def _apply_target(target: ReplacementTarget, write: bool) -> bool:
    content = target.path.read_text(encoding="utf-8")
    updated, count = re.subn(target.pattern, target.replacement, content, count=1)
    if count != 1:
        raise ValueError(f"Could not update {target.description} in {target.path}")

    changed = updated != content
    if changed and write:
        target.path.write_text(updated, encoding="utf-8")
    return changed


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--write",
        action="store_true",
        help="Write synchronized versions back to disk.",
    )
    parser.add_argument(
        "--release-date",
        default="2026-06-01",
        help="Release date to use for AppStream metadata.",
    )
    args = parser.parse_args()

    version = _read_version()
    changed_descriptions: list[str] = []

    try:
        for target in _targets(version, args.release_date):
            if _apply_target(target, write=args.write):
                changed_descriptions.append(target.description)
    except (OSError, ValueError) as exc:
        print(f"❌ {exc}", file=sys.stderr)
        return 1

    mode = "Synchronized" if args.write else "Checked"
    if changed_descriptions:
        print(f"✅ {mode} {len(changed_descriptions)} version targets for {version}.")
        for description in changed_descriptions:
            print(f" - {description}")
    else:
        print(f"✅ All version targets already match {version}.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())