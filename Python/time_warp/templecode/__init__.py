"""Shim package for Time Warp IDE.

This module re-exports the legacy TempleCode interpreter symbols so
existing imports like `from templecode import TempleInterpreter` keep
working from within the Time Warp Python app.
"""

from __future__ import annotations

import os
import sys

# Add repository root to sys.path so the implicit-namespace package
# `legacy` is importable at runtime (PEP 420).
_REPO_ROOT = os.path.abspath(
    os.path.join(os.path.dirname(__file__), os.pardir, os.pardir, os.pardir)
)
if _REPO_ROOT not in sys.path:
    sys.path.insert(0, _REPO_ROOT)

try:
    # Import from the archived legacy implementation in this repository.
    from legacy.templecode import (
        TempleInterpreter,
        IOBase,
        TurtleAPI,
    )  # type: ignore
except Exception as exc:  # pragma: no cover - import guard
    raise ImportError(
        "Time Warp: couldn't import legacy.templecode. "
        "Ensure you are running from the repository root or that the repo "
        "root is on sys.path."
    ) from exc

__all__ = ["TempleInterpreter", "IOBase", "TurtleAPI"]
