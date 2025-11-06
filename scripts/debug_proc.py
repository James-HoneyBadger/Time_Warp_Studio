#!/usr/bin/env python3
from templecode import TempleInterpreter


def main() -> int:
    code = """PRINT "Line 1"
PROC test n
  PRINT "In proc"
ENDPROC
PRINT "Line after ENDPROC"
CALL test 5
PRINT "Line after CALL"
"""

    interp = TempleInterpreter()
    interp.load_program(code)

    print(f"Loaded {len(interp.lines)} lines")
    for i, line in enumerate(interp.lines):
        print(f"{i}: {line}")

    print(f"\nProcs: {interp.procs}")

    print("\nRunning:")
    interp.run_program()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
