#!/usr/bin/env python3
"""Run selected example programs headlessly using the interpreter.

This script executes interpreted examples (BASIC, PILOT, LOGO, Pascal, Prolog,
and C) using the in-tree Python executors (where available) and feeds inputs
from optional fixture files.

Usage:
    ci_run_examples.py [examples/.../file1.bas ...]

If no files provided, it runs a default set useful for CI (non-interactive or
with fixtures). Fixtures are searched under Examples/fixtures with the same
relative path and a .in extension, e.g. Examples/basic/showcase.in
"""

import sys
from pathlib import Path
from typing import List

ROOT = Path(__file__).resolve().parents[3]
EXAMPLES = ROOT / "Examples"
SCRIPTS = Path(__file__).resolve().parent

sys.path.insert(0, str(ROOT / "Platforms" / "Python"))

from time_warp.core.interpreter import Interpreter, Language  # noqa: E402
from time_warp.graphics.turtle_state import TurtleState  # noqa: E402


def load_fixture_for(example_path: Path) -> List[str]:
    rel = example_path.relative_to(EXAMPLES)
    fixture = ROOT / "Examples" / "fixtures" / rel.with_suffix(".in")
    if fixture.exists():
        text = fixture.read_text(encoding="utf-8")
        lines = [line.rstrip("\n") for line in text.splitlines()]
        return lines
    return []


def run_example(path: Path, inputs: List[str]):
    print(f"\n--- Running: {path} ---")
    # For some languages we prefer to compile and run native executables
    # (C examples are intended as compile-and-run programs rather than
    #  interpreted C-lines). Detect .c and compile with gcc if available.
    if path.suffix.lower() == ".c":
        exe = Path("/tmp") / (path.stem + "-ci-bin")
        try:
            import subprocess

            subprocess.run(
                ["gcc", str(path), "-o", str(exe)], check=True, capture_output=True
            )
            # Run compiled binary with fixture input (if provided)
            inp = "\n".join(inputs) if inputs else None
            run = subprocess.run(
                [str(exe)], input=inp, text=True, capture_output=True, check=True
            )
            if run.stdout:
                for line in run.stdout.splitlines():
                    print(line)
            if run.stderr:
                print(run.stderr)
            return
        except Exception as exc:  # fallback to interpreter
            print(f"Compiler/run failed — falling back to interpreter: {exc}")
    else:
        code = path.read_text(encoding="utf-8")
        interp = Interpreter()
        turtle = TurtleState()
        interp.reset()
        # Set language based on file extension so the correct executor is used
        interp.set_language(Language.from_extension(path.suffix))
        interp.load_program(code, language=interp.language)

    input_iter = iter(inputs)

    all_output = []
    # For compiled C path we early-return above; this block runs for interpreted languages
    while True:
        outputs = interp.execute(turtle)
        for o in outputs:
            print(o)
            all_output.append(o)

        if interp.pending_input:
            try:
                v = next(input_iter)
                print(f"[fixture] providing input: {repr(v)}")
                interp.provide_input(v)
                # resume
                continue
            except StopIteration:
                print(
                    "No more fixture input available — providing empty string to"
                    " continue."
                )
                interp.provide_input("")
                continue

        # Not pending input; execution ended or waiting — break loop
        break

    print(f"--- Finished: {path} (lines drawn: {len(turtle.lines)}) ---\n")
    return all_output


def main(argv: List[str]):
    if len(argv) > 1:
        files = [Path(p) for p in argv[1:]]
        # normalize to the repo Examples/ area when a relative path is given
        files = [f if f.is_absolute() else ROOT / f for f in files]
    else:
        # default CI set
        files = [
            EXAMPLES / "basic" / "hello_world.bas",
            EXAMPLES / "basic" / "showcase.bas",
            EXAMPLES / "logo" / "showcase.logo",
            EXAMPLES / "pilot" / "showcase.pilot",
            EXAMPLES / "pascal" / "showcase.pas",
            EXAMPLES / "prolog" / "showcase.pl",
            EXAMPLES / "c" / "showcase.c",
        ]

    for f in files:
        if not f.exists():
            print(f"Example not found, skipping: {f}")
            continue
        inputs = load_fixture_for(f)
        try:
            run_example(f, inputs)
        except KeyboardInterrupt:
            print("Execution interrupted by user")
            raise
        except Exception as exc:
            print(f"Error running {f}: {exc}")


if __name__ == "__main__":
    main(sys.argv)
