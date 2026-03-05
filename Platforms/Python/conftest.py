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
# Prevent collection of integration tests that require services unavailable
# in standard CI.  This is a belt-and-suspenders backup to the --ignore
# entries in pyproject.toml addopts.
# ---------------------------------------------------------------------------
collect_ignore = [
    "time_warp/tests/test_api_integration.py",
    "time_warp/tests/test_websocket_integration.py",
    "time_warp/tests/test_multiplayer_integration.py",
]
