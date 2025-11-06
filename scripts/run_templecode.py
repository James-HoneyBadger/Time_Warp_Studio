#!/usr/bin/env python3
import sys
from templecode import TempleInterpreter, ConsoleIO, NullTurtle


def main() -> int:
    if len(sys.argv) < 2:
        print("Usage: python scripts/run_templecode.py <file.tc>")
        return 1
    path = sys.argv[1]
    try:
        with open(path, "r", encoding="utf-8") as f:
            code = f.read()
    except OSError as e:
        print(f"Error reading {path}: {e}")
        return 2

    interp = TempleInterpreter(io=ConsoleIO(), turtle=NullTurtle())
    interp.run(code)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
