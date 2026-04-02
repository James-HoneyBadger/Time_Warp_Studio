"""Shared helpers for language-comprehensive tests."""

from __future__ import annotations

from time_warp.core.interpreter import Interpreter, Language
from time_warp.graphics.turtle_state import TurtleState


def run(source: str, language: Language, *, input_val: str = "4") -> list[str]:
    """Execute *source* in the given language and return the output lines.

    For languages that use start_input_request() (BASIC, PILOT, etc.)
    the helper automatically provides *input_val* on each pending-input
    pause and resumes until the program finishes.
    """
    interp = Interpreter()
    turtle = TurtleState()
    interp.input_callback = lambda _prompt: input_val
    interp.load_program(source, language=language)
    all_output: list[str] = []

    # Run (possibly multiple times if there are input pauses)
    for _ in range(20):  # safety limit on input rounds
        out = interp.execute(turtle)
        all_output.extend(out)
        if interp.pending_input:
            interp.provide_input(input_val)
            # After providing input the interpreter can be resumed
            continue
        break

    return all_output


def ok(output: list[str]) -> bool:
    """True when there is at least some non-error output."""
    non_err = [line for line in output if not line.startswith("❌")]
    return len(non_err) > 0


def has(output: list[str], *fragments: str) -> bool:
    """True when every *fragment* appears somewhere in the joined output."""
    blob = "\n".join(output)
    return all(f in blob for f in fragments)


def no_errors(output: list[str]) -> bool:
    """True when NO line in *output* starts with the error emoji."""
    return not any(line.startswith("❌") for line in output)


def first_error(output: list[str]) -> str | None:
    """Return the first error line or None."""
    for line in output:
        if line.startswith("❌"):
            return line
    return None
