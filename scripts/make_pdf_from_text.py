#!/usr/bin/env python3
"""Wrapper to call `Scripts/make_pdf_from_text.py`."""
import os
import runpy

SCRIPT_PATH = os.path.join(
    os.path.dirname(__file__), "..", "Scripts", "make_pdf_from_text.py"
)

if __name__ == "__main__":
    runpy.run_path(SCRIPT_PATH, run_name="__main__")
