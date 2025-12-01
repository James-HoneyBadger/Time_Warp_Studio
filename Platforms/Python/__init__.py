"""Python package shim to expose `Platforms.Python` as a package.

This file is minimal and only exists to help static analyzers and test
runners find modules within the multi-directory layout of Time Warp.
"""

# Pylint: The package name intentionally uses a capitalized component
# because 'Platforms' is a top-level directory representing platform
# choices; we allow the name here for clarity in the repo layout.
# pylint: disable=invalid-name
__all__ = []
