"""Compatibility shim to expose the `Scripts/` directory as the
`scripts` package for tests and tooling that expect lowercase imports.

This simply adds the repository `Scripts/` directory to the module search
path for the `scripts` package. This avoids renaming or duplicating files
and keeps the existing structure intact.
"""

import os
import pkgutil

_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
_SCRIPTS_DIR = os.path.join(_ROOT, "Scripts")
if os.path.isdir(_SCRIPTS_DIR):
    # Prepend the actual Scripts dir to the package __path__ so imports
    # like `scripts.generate_placeholders` resolve to the canonical
    # `Scripts/generate_placeholders.py` module path.
    __path__.insert(0, _SCRIPTS_DIR)

__all__ = [name for _, name, _ in pkgutil.iter_modules([_SCRIPTS_DIR])]  # type: ignore
