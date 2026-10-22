"""Where the harnesses find the checkout and the runner they drive.

Two scripts, one question: "which `time_warp_check` should I spawn, and where is
the repository it should be pointed at?" Answering it twice is how the second copy
drifts -- one of them looks in `native/build`, the other in `native/build-debug`,
and a machine that only has one of those reports a missing tool as a failing
program.

The runner is a build artifact, so every path here is a *lookup* rather than a
constant: `TIME_WARP_CHECK` wins if it is set (a CI job with a staged install, or a
debug build), then the two build trees the Makefile produces, then `PATH`.
"""

from __future__ import annotations

import os
import pathlib
import shutil


def repository_root() -> pathlib.Path:
    """The checkout this script lives in, so paths do not depend on the cwd."""
    return pathlib.Path(__file__).resolve().parents[2]


def locate_check(root: pathlib.Path | None = None) -> pathlib.Path | None:
    """The built `time_warp_check`, or None when it has not been built."""
    override = os.environ.get("TIME_WARP_CHECK")
    if override:
        candidate = pathlib.Path(override)
        return candidate if candidate.is_file() else None
    base = root if root is not None else repository_root()
    for candidate in (
        base / "native" / "build" / "time_warp_check",
        base / "native" / "build" / "time_warp_check.exe",
        base / "native" / "build-debug" / "time_warp_check",
        base / "native" / "build-debug" / "time_warp_check.exe",
    ):
        if candidate.is_file():
            return candidate
    found = shutil.which("time_warp_check")
    return pathlib.Path(found) if found else None
