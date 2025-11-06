import os
import sys

# Ensure project root is on sys.path for local imports before importing package
root = os.path.dirname(os.path.dirname(__file__))
if root not in sys.path:
    sys.path.insert(0, root)

from templecode import TempleInterpreter, IOBase, NullTurtle


class CaptureIO(IOBase):
    def __init__(self) -> None:
        self._buf: list[str] = []

    def write(self, text: str) -> None:
        self._buf.append(text)

    def read(self, prompt: str = "") -> str:  # no input during tests
        return ""

    def get(self) -> str:
        return "".join(self._buf)


def run_code(code: str) -> str:
    io = CaptureIO()
    interp = TempleInterpreter(io=io, turtle=NullTurtle())
    interp.run(code)
    return io.get()


def test_print_hello() -> None:
    out = run_code('PRINT "Hello"\n')
    assert out == "Hello\n"


def test_factorial_recursive() -> None:
    code = (
        "PROC factorial n\n"
        "  IF n <= 1 THEN RETURN 1\n"
        "  LET prev = 0\n"
        "  CALL factorial n - 1 INTO prev\n"
        "  RETURN n * prev\n"
        "ENDPROC\n"
        "CALL factorial 5 INTO fact\n"
        'PRINT "5! = " + str(fact)\n'
    )
    out = run_code(code)
    # Only assert the final line to keep the test focused
    assert out.endswith("5! = 120\n")


def test_procedure_call_flow() -> None:
    # Mirror the simple proc flow
    code = (
        'PRINT "Line 1"\n'
        "PROC test n\n"
        '  PRINT "In proc"\n'
        "ENDPROC\n"
        'PRINT "Line after ENDPROC"\n'
        "CALL test 5\n"
        'PRINT "Line after CALL"\n'
    )
    out = run_code(code)
    lines = [ln for ln in out.splitlines() if ln.strip()]
    assert lines[0] == "Line 1"
    assert any("In proc" in ln for ln in lines)  # proc prints when called
    assert lines[-1] == "Line after CALL"
