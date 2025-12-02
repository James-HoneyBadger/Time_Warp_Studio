#!/usr/bin/env python3
# pylint: disable=duplicate-code
"""Create a simple multi-line PDF from a plain text file (no external deps).
Usage: python3 scripts/make_pdf_from_text.py <input.txt> <output.pdf>
"""
from __future__ import annotations

from pathlib import Path
import sys

# Add parent to path for scripts.pdf_utils import
if __name__ == "__main__":
    sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

# pylint: disable=wrong-import-position
from Scripts.pdf_utils import write_pdf


def main() -> None:
    """Convert a text file to a simple PDF using `Scripts.pdf_utils`."""
    if len(sys.argv) != 3:
        print(
            "Usage: python3 scripts/make_pdf_from_text.py\n"
            "  <input.txt> <output.pdf>"
        )
        sys.exit(2)
    input_path = Path(sys.argv[1])
    out_path = Path(sys.argv[2])
    if not input_path.exists():
        print(f"Input file not found: {input_path}")
        sys.exit(2)
    lines = input_path.read_text(encoding="utf-8").splitlines()
    write_pdf(lines, out_path)
    print(f"Wrote PDF: {out_path}")


if __name__ == "__main__":
    main()
