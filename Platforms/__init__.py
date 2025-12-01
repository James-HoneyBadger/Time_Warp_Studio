"""Platforms package shim to make `Platforms.Python` a valid Python package.

This file is intentionally minimal to support static analysis and testing
tools that rely on package-style imports for the multi-folder repository
layout used here.

Note: The top-level folder name is intentionally capitalized for
historical and cross-platform packaging reasons. Pylint may warn about
the module name not following the snake_case convention; that warning
is intentionally suppressed here to avoid requiring a folder rename.
"""

# Silence invalid-name for this file when linters aren't using the
# repository-wide pylintrc (keeps editor diagnostics quiet in IDEs).
# pylint: disable=invalid-name

__all__ = []
