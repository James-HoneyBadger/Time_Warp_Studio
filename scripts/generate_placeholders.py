#!/usr/bin/env python3
"""Wrapper to call top-level Scripts/generate_placeholders.py script.

This wrapper ensures that the script can be invoked via both the
`Scripts/` and `scripts/` paths (case-insensitive convenience).
"""
import os
import runpy

SCRIPT_PATH = os.path.join(
    os.path.dirname(__file__), "..", "Scripts", "generate_placeholders.py"
)

if __name__ == "__main__":
    runpy.run_path(SCRIPT_PATH, run_name="__main__")
