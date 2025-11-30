#!/usr/bin/env python3
"""
Release automation utility for Time Warp IDE.

Handles versioning, changelog generation, and release artifact preparation.
"""

import argparse
import hashlib
import subprocess
import sys
from datetime import datetime
from pathlib import Path
from typing import Dict, List


def get_version() -> str:
    """Get current version from pyproject.toml."""
    pyproject = Path(__file__).parent.parent / "pyproject.toml"

    with open(pyproject, encoding="utf-8") as f:
        for line in f:
            if line.startswith("version = "):
                return line.split('"')[1]

    return "0.0.0"


def generate_checksums(artifacts: List[Path]) -> Dict[str, str]:
    """Generate SHA256 checksums for artifacts."""
    checksums = {}

    for artifact in artifacts:
        if not artifact.exists():
            continue

        sha256_hash = hashlib.sha256()
        with open(artifact, "rb") as file_handle:
            for byte_block in iter(lambda fh=file_handle: fh.read(4096), b""):
                sha256_hash.update(byte_block)

        checksums[artifact.name] = sha256_hash.hexdigest()

    return checksums


def create_release_notes(version: str, artifacts: List[Path]) -> str:
    """Create release notes with checksums."""
    notes = f"""# Time Warp IDE v{version}

Released: {datetime.utcnow().strftime('%Y-%m-%d')}

## Platform Builds

"""

    checksums = generate_checksums(artifacts)

    for artifact in artifacts:
        if artifact.exists():
            size = artifact.stat().st_size
            checksum = checksums.get(artifact.name, "N/A")
            notes += f"### {artifact.name}\n"
            notes += f"- Size: {size:,} bytes\n"
            notes += f"- SHA256: `{checksum}`\n\n"

    notes += """
## Installation

### Windows 2000 Edition
1. Download `TimeWarpIDE-win2000.zip`
2. Extract to any folder
3. Run `TimeWarpIDE.exe`

### Python Version
```bash
pip install time-warp-ide
time-warp
```

## Checksums

Verify downloads:
```bash
sha256sum -c checksums.txt
```

"""

    return notes


def create_checksums_file(artifacts: List[Path], output: Path) -> None:
    """Create checksums.txt file."""
    checksums = generate_checksums(artifacts)

    with open(output, "w", encoding="utf-8") as f:
        for name, checksum in sorted(checksums.items()):
            f.write(f"{checksum}  {name}\n")


def tag_release(version: str, push: bool = False) -> int:
    """Create git tag for release."""
    tag = f"v{version}"

    # Check if tag exists
    result = subprocess.run(
        ["git", "tag", "-l", tag],
        capture_output=True,
        text=True,
        check=False,
    )

    if result.stdout.strip():
        print(f"Tag {tag} already exists")
        return 0

    # Create tag
    result = subprocess.run(
        ["git", "tag", "-a", tag, "-m", f"Release {version}"],
        check=False,
    )

    if result.returncode != 0:
        return result.returncode

    if push:
        print(f"Pushing tag {tag} to remote...")
        return subprocess.run(
            ["git", "push", "origin", tag],
            check=False,
        ).returncode

    return 0


def main() -> int:
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Time Warp IDE release automation",
    )

    parser.add_argument(
        "--version", help="Version to release (default: from pyproject.toml)"
    )
    parser.add_argument(
        "--tag",
        action="store_true",
        help="Create git tag for release",
    )
    parser.add_argument(
        "--push",
        action="store_true",
        help="Push tag to remote (implies --tag)",
    )
    parser.add_argument(
        "--notes",
        action="store_true",
        help="Generate release notes",
    )

    args = parser.parse_args()

    version = args.version or get_version()
    root = Path(__file__).parent.parent
    dist_dir = root / "dist"

    print(f"Preparing release v{version}...")

    # Find artifacts
    artifacts = []

    win2000_zip = dist_dir / "win2000" / "TimeWarpIDE-win2000.zip"
    if win2000_zip.exists():
        artifacts.append(win2000_zip)

    # Generate checksums
    if artifacts:
        checksums_file = dist_dir / "checksums.txt"
        create_checksums_file(artifacts, checksums_file)
        print(f"Created {checksums_file}")

    # Generate release notes
    if args.notes or args.tag:
        notes = create_release_notes(version, artifacts)
        notes_file = dist_dir / f"RELEASE_NOTES_v{version}.md"

        with open(notes_file, "w", encoding="utf-8") as f:
            f.write(notes)

        print(f"Created {notes_file}")

    # Create git tag
    if args.tag or args.push:
        return tag_release(version, push=args.push)

    return 0


if __name__ == "__main__":
    sys.exit(main())
