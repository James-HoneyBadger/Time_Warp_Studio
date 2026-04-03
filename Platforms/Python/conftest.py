"""Root conftest for Time Warp Studio test suite.

Explicitly excludes integration tests that require external services
(FastAPI, SQLAlchemy, websockets) not available in standard CI.
"""

from __future__ import annotations

import os

# ---------------------------------------------------------------------------
# Ensure headless Qt environment for all tests
# ---------------------------------------------------------------------------
os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")
os.environ.setdefault("LIBGL_ALWAYS_SOFTWARE", "1")

# ---------------------------------------------------------------------------
# Prevent collection of test files requiring unavailable dependencies.
# Add paths here for any tests that need optional packages not in [dev].
# ---------------------------------------------------------------------------
collect_ignore: list[str] = [
    "time_warp/tests/backend",
]
