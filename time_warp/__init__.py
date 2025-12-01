"""Stub package to help static analyzers import the real `time_warp` package
which resides in `Platforms/Python/time_warp` in this repository layout.

This shim will, at import time, attempt to load the actual package
implementation directly from Platforms/Python and then replace itself in
`sys.modules` with that module so the real code is used.

This is intentionally minimal to avoid interfering with runtime installs
where a normal packaging workflow would handle imports.
"""

from __future__ import annotations

import importlib.util
import os
import sys

_ROOT = os.path.dirname(__file__)
_PLATFORM_PY_DIR = os.path.join(_ROOT, "Platforms", "Python")
_REAL_INIT = os.path.join(_PLATFORM_PY_DIR, "time_warp", "__init__.py")

if os.path.isfile(_REAL_INIT):
    spec = importlib.util.spec_from_file_location("time_warp", _REAL_INIT)
    if spec is not None and spec.loader is not None:
        module = importlib.util.module_from_spec(spec)
        # Replace the current stub module with the loaded implementation
        sys.modules["time_warp"] = module
        spec.loader.exec_module(module)
    else:
        # Fallback: leave stub as-is; static analysis may still try to
        # import the package, so avoid raising here.
        pass
else:
    # No match on this repo layout — nothing to do.
    pass
