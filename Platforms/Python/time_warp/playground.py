#!/usr/bin/env python3
"""
Playground Mode: Sandboxed snippet runner for isolated interpreter testing.
Execute single commands or small code blocks to quickly reproduce and debug issues.

Usage:
    python -m time_warp.playground --language basic --code '10 PRINT "Hello"'
    python -m time_warp.playground --language pilot --file snippet.pilot
    python -m time_warp.playground --repl
"""
from __future__ import annotations
import argparse
import sys
from pathlib import Path

try:
    from .core.interpreter import Interpreter, Language
    from .graphics.turtle_state import TurtleState
except ImportError:
    # Allow running as script from repo root
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))
    from Platforms.Python.time_warp.core.interpreter import (
        Interpreter,
        Language,
    )
    from Platforms.Python.time_warp.graphics.turtle_state import TurtleState


def run_snippet(
    language: Language,
    code: str,
    inputs: str | None = None,
) -> tuple[list[str], TurtleState]:
    """
    Execute a code snippet in an isolated interpreter sandbox.
    Returns (output_lines, turtle_state).
    """
    interpreter = Interpreter()
    turtle = TurtleState()

    # Load program
    interpreter.load_program(code, language=language)

    # Provide inputs if available
    if inputs:
        for line in inputs.splitlines():
            interpreter.provide_input(line)

    # Execute and capture output
    output_lines = interpreter.execute(turtle)

    return (output_lines, turtle)


def repl_mode(language: Language):
    """Interactive REPL for quick command testing."""
    print(f"Playground REPL ({language.friendly_name()})")
    print("Enter commands (Ctrl+D or 'exit' to quit, 'reset' to clear state)")
    print()

    interpreter = Interpreter()
    turtle = TurtleState()
    interpreter.language = language

    while True:
        try:
            line = input(f"{language.friendly_name()}> ").strip()
        except (EOFError, KeyboardInterrupt):
            print("\nExiting...")
            break

        if not line:
            continue
        if line.lower() in ("exit", "quit"):
            break
        if line.lower() == "reset":
            interpreter = Interpreter()
            turtle = TurtleState()
            interpreter.language = language
            print("State reset.")
            continue

        # Execute line
        interpreter.load_program(line, language=language)
        try:
            output = interpreter.execute(turtle)
            for out_line in output:
                print(out_line)
        except RuntimeError as e:
            print(f"Error: {e}")


def main(argv: list[str] | None = None) -> int:
    """Parse args and run Playground mode: REPL or single snippet."""
    parser = argparse.ArgumentParser(
        description="Playground: isolated command execution for debugging"
    )
    parser.add_argument(
        "--language",
        "-l",
        choices=["basic", "pilot", "logo", "pascal", "prolog", "c"],
        default="basic",
        help="Programming language",
    )
    parser.add_argument(
        "--code",
        "-c",
        help="Code to execute inline",
    )
    parser.add_argument(
        "--file",
        "-f",
        type=Path,
        help="Code file to execute",
    )
    parser.add_argument(
        "--inputs",
        "-i",
        help="Newline-separated inputs to provide",
    )
    parser.add_argument(
        "--repl",
        "-r",
        action="store_true",
        help="Interactive REPL mode",
    )

    args = parser.parse_args(argv)

    # Map language string to enum
    lang_map = {
        "basic": Language.BASIC,
        "pilot": Language.PILOT,
        "logo": Language.LOGO,
        "pascal": Language.PASCAL,
        "prolog": Language.PROLOG,
        "c": Language.C,
    }
    language = lang_map[args.language]

    # REPL mode
    if args.repl:
        repl_mode(language)
        return 0

    # Snippet execution
    if args.code:
        code = args.code
    elif args.file:
        code = args.file.read_text(encoding="utf-8")
    else:
        print("Error: Provide --code or --file (or use --repl)")
        return 1

    output_lines, turtle = run_snippet(language, code, args.inputs)

    print("=== Output ===")
    for line in output_lines:
        print(line)

    print("\n=== Turtle State ===")
    print(f"Position: ({turtle.x:.1f}, {turtle.y:.1f})")
    print(f"Heading: {turtle.heading:.1f}°")
    print(f"Pen down: {turtle.pen_down}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
