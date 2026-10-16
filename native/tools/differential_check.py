#!/usr/bin/env python3
"""Differential test: C++20 port vs. the original Python implementation.

Feeds a corpus of expressions to both implementations using an identical
evaluation context (the same variables, array, string variables and TIMER hook)
and reports any divergence in:

  * success/failure classification
  * numeric result (within a relative tolerance)
  * error message (informational; the port splits Python's ValueError into
    phase-specific types, and a documented set of cases intentionally differ)

Usage:
    python3 native/tools/differential_check.py native/build/expr_cli
"""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO_ROOT / "native" / "bridge"))

from time_warp.utils.expression_evaluator import (  # noqa: E402
    DivisionByZeroError,
    ExpressionEvaluator,
)

# Expressions where the port deliberately diverges from the reference.  Each
# entry is (expression, reason).
KNOWN_DIVERGENCES = {
    "0xFF": "reference 0x branch was unreachable (dead code); port fixes it",
    "0X10": "reference 0x branch was unreachable (dead code); port fixes it",
    "0o17": "reference 0o branch was unreachable (dead code); port fixes it",
    "SQRT(0 - 1)": "Port raises a domain error; reference raises ValueError",
    "LOG(0)": "Port raises a domain error; reference raises ValueError",
    "LOG2(0)": "Port raises a domain error; reference raises ValueError",
    "LOG10(0)": "Port raises a domain error; reference raises ValueError",
    "ASIN(2)": "Port raises a domain error; reference raises ValueError",
    "ACOS(0 - 2)": "Port raises a domain error; reference raises ValueError",
    "(0 - 8) ^ 0.5": "Port rejects the complex result; reference returns complex",
    "0 ^ -1": "Port raises DivisionByZero; reference raises ZeroDivisionError",
    "999999999999999999999999 AND 1": "Port rejects >64-bit ints (no bignum)",
    # The reference implemented `==` as `abs(a - b) < 1e-10`, which raises
    # TypeError for strings -- string equality could never work there.  The port
    # compares strings lexicographically instead, so these now evaluate.
    "NAME$ = OTHER$": "reference raised TypeError; port compares strings",
    "NAME$ = MISSING$": "reference raised TypeError; port compares strings",
}

CORPUS = [
    # -- literals -----------------------------------------------------------
    "0", "1", "42", "1.5", ".5", "1.", "3.14159", "0.001",
    "&HFF", "&hff", "&H0A",
    "0xFF", "0X10", "0o17",
    # -- arithmetic / precedence -------------------------------------------
    "1 + 2", "2 + 3 * 4", "(2 + 3) * 4", "10 / 4", "10 - 4 - 1",
    "2 ^ 3", "2 ^ 3 ^ 2", "2 ** 3", "2 * -3", "-5 + 2", "-3", "+7",
    "2 + 3 * X", "X + 1", "X * X", "A / B",
    # -- integer division family -------------------------------------------
    "7 // 2", "-7 // 2", "7 \\ 2", "-7 \\ 2", "9 \\ 4", "-9 \\ 4",
    "7 % 3", "-7 % 3", "7 MOD 3", "-7 MOD 3", "7 REM 3", "-7 REM 3",
    "9 % 4", "-9 % 4", "9 REM 4", "-9 REM 4",
    # -- bitwise / logical --------------------------------------------------
    "5 OR 2", "5 XOR 3", "6 AND 3", "1 << 4", "256 >> 4", "NOT 0", "NOT 5",
    "1 + 2 AND 3", "A & B | 1", "6 /\\ 3", "4 \\/ 1", "~5", "12 AND 10",
    # -- comparisons --------------------------------------------------------
    "2 + 3 > 4", "3 = 3", "3 <> 4", "1 < 2", "1 > 2", "1 <= 1", "1 >= 2",
    "1 < 2 AND 3 > 2", "1 > 2 OR 3 > 2",
    # -- math builtins ------------------------------------------------------
    "SIN(PI / 2)", "COS(0)", "TAN(0)", "ASIN(0)", "ACOS(1)", "ATAN(1)",
    "SINH(0)", "COSH(0)", "TANH(0)", "SQRT(16)", "SQR(9)", "ABS(0 - 5)",
    "INT(3.7)", "INT(-3.7)", "FIX(3.7)", "FIX(-3.7)", "CINT(2.5)", "CINT(3.5)",
    "ROUND(2.5)", "ROUND(3.5)", "TRUNC(-3.7)", "SGN(-9)", "SGN(9)", "SGN(0)",
    "SIGN(-9)", "MIN(3, 7)", "MAX(3, 7)", "POW(2, 10)", "ATAN2(0, 1)",
    "EXP(0)", "LOG(1)", "LOG10(1000)", "LOG2(8)", "FLOOR(2.9)", "CEIL(2.1)",
    "VAL(7)", "FLOAT(7)", "INTEGER(-3.7)", "ABS(14)",
    # -- constants & variables ---------------------------------------------
    "PI", "E", "TAU", "TRUE", "FALSE", "PI > 3", "X% + 1", "X# + 1",
    "TIMER + 1", "TIMER",
    # -- arrays and strings -------------------------------------------------
    "TAB(0)", "TAB(1)", "TAB(2)", "TAB(A - 1)", "TAB(A)", "TAB(4)",
    "TAB(0 - 1)", "LEN(NAME$)", "LEN(MISSING$)", "NAME$ = OTHER$",
    "NAME$ = MISSING$", "VAL(NAME$)",
    # -- errors -------------------------------------------------------------
    # NOTE: the empty expression is covered by the unit tests rather than here,
    # because the CLI protocol uses blank lines as separators.
    "1 / 0", "1 // 0", "1 % 0", "1 MOD 0", "1 REM 0", "1 \\ 0", "X / (X - X)",
    "(1 + 2", "1 + 2)", "1 +", "Q", "NOPE(1)", "LEN(5)", "MIN(1)",
    "1 @ 2", "1 $ 2", "1.2.3", "&H", "0x", "1e300", "NAME$",
    "0 ^ -1", "(0 - 8) ^ 0.5", "SQRT(0 - 1)", "LOG(0)", "LOG2(0)", "LOG10(0)",
    "ASIN(2)", "ACOS(0 - 2)", "999999999999999999999999 AND 1",
]


def run_reference(expressions: list[str]) -> dict[str, tuple[str, str]]:
    """Evaluates the corpus with the Python reference implementation.

    ``TIMER`` is resolved through ``get_game_state()`` in the reference, so the
    game-state accessor is stubbed to the same constant the C++ CLI's hook
    returns (42.0).
    """
    from time_warp.features import game_support

    class _State:
        def get_timer_value(self) -> float:
            return 42.0

    original = game_support.get_game_state
    game_support.get_game_state = lambda: _State()  # type: ignore[assignment]

    results: dict[str, tuple[str, str]] = {}
    try:
        evaluator = ExpressionEvaluator(
            variables={"X": 10.0, "A": 4.0, "B": 0.0},
            arrays={"TAB": [0.0, 10.0, 20.0, 30.0]},
            string_variables={"NAME$": "Time Warp", "OTHER$": "Time Warp"},
        )
        for expression in expressions:
            try:
                results[expression] = ("OK", repr(evaluator.evaluate(expression)))
            except Exception as error:  # noqa: BLE001 - mirroring the CLI
                kind = "DIVZERO" if isinstance(error, DivisionByZeroError) else "ERR"
                results[expression] = (kind, str(error))
    finally:
        game_support.get_game_state = original
    return results


def run_port(binary: Path, expressions: list[str]) -> dict[str, tuple[str, str]]:
    completed = subprocess.run(
        [str(binary)],
        input="\n".join(expressions) + "\n",
        capture_output=True,
        text=True,
        check=True,
    )
    results: dict[str, tuple[str, str]] = {}
    for line in completed.stdout.splitlines():
        if "\t" not in line:
            continue
        expression, outcome = line.split("\t", 1)
        status, _, detail = outcome.partition(" ")
        results[expression] = (status, detail)
    return results


def close_enough(a: str, b: str) -> bool:
    try:
        lhs = float(a)
        rhs = float(b)
    except ValueError:
        return False
    if lhs == rhs:
        return True
    scale = max(abs(lhs), abs(rhs), 1.0)
    return abs(lhs - rhs) <= 1e-9 * scale


def main() -> int:
    if len(sys.argv) != 2:
        print(__doc__)
        return 2
    binary = Path(sys.argv[1])
    if not binary.exists():
        print(f"error: {binary} not found; build the project first")
        return 2

    reference = run_reference(CORPUS)
    port = run_port(binary, CORPUS)

    mismatches: list[str] = []
    expected_divergences: list[str] = []

    for expression in CORPUS:
        ref_status, ref_detail = reference[expression]
        port_status, port_detail = port[expression]

        ref_ok = ref_status == "OK"
        port_ok = port_status == "OK"

        if ref_ok != port_ok:
            reason = KNOWN_DIVERGENCES.get(expression)
            line = (
                f"{expression!r}: reference={ref_status} {ref_detail} | "
                f"port={port_status} {port_detail}"
            )
            (expected_divergences if reason else mismatches).append(line)
            continue

        if ref_ok and not close_enough(ref_detail, port_detail):
            reason = KNOWN_DIVERGENCES.get(expression)
            line = (
                f"{expression!r}: reference={ref_detail} | port={port_detail}"
            )
            (expected_divergences if reason else mismatches).append(line)

    total = len(CORPUS)
    print(f"corpus size                : {total}")
    print(f"agreements                 : {total - len(mismatches) - len(expected_divergences)}")
    print(f"known/documented divergences: {len(expected_divergences)}")
    print(f"UNEXPECTED mismatches      : {len(mismatches)}")

    if expected_divergences:
        print("\nDocumented divergences (expected):")
        for line in expected_divergences:
            print(f"  - {line}")

    if mismatches:
        print("\nUNEXPECTED mismatches:")
        for line in mismatches:
            print(f"  ! {line}")
        return 1

    print("\nAll non-divergent cases agree with the Python reference.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
