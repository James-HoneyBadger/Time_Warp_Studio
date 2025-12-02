#!/usr/bin/env python3
"""
CI headless runner: executes example programs and collects outputs.
This script is a skeleton and should be wired to the interpreter APIs.
"""
from __future__ import annotations
import argparse
import pathlib
import sys

ROOT = pathlib.Path(__file__).resolve().parents[1]
EXAMPLES = ROOT / "Examples"
REPORTS = ROOT / "test_reports"

# Add ROOT to sys.path for interpreter_shim import
sys.path.insert(0, str(ROOT))

LANG_EXT = {
    "basic": ".bas",
    "pilot": ".pilot",
    "logo": ".logo",
}


def find_examples(languages: list[str]) -> list[pathlib.Path]:
    paths: list[pathlib.Path] = []
    for lang in languages:
        lang_dir = EXAMPLES / lang
        if not lang_dir.exists():
            continue
        ext = LANG_EXT.get(lang)
        paths.extend(sorted(lang_dir.glob(f"*{ext}")))
    return paths


from Scripts.interpreter_shim import run_program


def run_example(language: str, path: pathlib.Path, inputs: str | None) -> str:
    """
    Execute a program headlessly and return its text output.
    """
    return run_program(language, path, inputs)


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description="Headless example runner")
    parser.add_argument(
        "--languages",
        nargs="+",
        default=["basic", "pilot", "logo"],
        help="Languages to run",
    )
    args = parser.parse_args(argv)

    examples = find_examples(args.languages)
    print(f"Found {len(examples)} examples")

    REPORTS.mkdir(exist_ok=True)
    for ex in examples:
        lang = ex.parent.name
        fixture = (
            EXAMPLES / "golden_fixtures" / lang / (ex.name.replace(".", "_") + ".in")
        )
        inputs = None
        if fixture.exists():
            inputs = fixture.read_text(encoding="utf-8")
        output = run_example(lang, ex, inputs)
        out_path = REPORTS / f"{lang}_{ex.stem}.out"
        out_path.write_text(output, encoding="utf-8")
        print(f"Wrote {out_path}")

    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
