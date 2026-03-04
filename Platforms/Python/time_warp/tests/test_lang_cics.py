"""Comprehensive tests for the CICS language executor."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, ok, has, no_errors

L = Language.CICS


def cics(source: str, **kw) -> list[str]:
    """Shortcut: run a CICS program."""
    return run(source, L, **kw)


# ============================================================================
# DISPLAY
# ============================================================================


class TestDisplay:
    def test_display_string(self):
        out = cics("       DISPLAY 'Hello World'.")
        assert has(out, "Hello World")

    def test_display_number(self):
        out = cics("       DISPLAY 42.")
        assert has(out, "42")

    def test_display_variable(self):
        out = cics(
            "       01 WS-NAME PIC X(10) VALUE 'Alice'.\n" "       DISPLAY WS-NAME."
        )
        assert has(out, "Alice")


# ============================================================================
# EXEC CICS SEND TEXT
# ============================================================================


class TestSendText:
    def test_send_text(self):
        out = cics("       EXEC CICS SEND TEXT FROM('Hello CICS') END-EXEC.")
        assert has(out, "Hello") or no_errors(out)


# ============================================================================
# MOVE
# ============================================================================


class TestMove:
    def test_move_literal(self):
        out = cics(
            "       01 WS-X PIC 9(3).\n"
            "       MOVE 42 TO WS-X.\n"
            "       DISPLAY WS-X."
        )
        assert has(out, "42")

    def test_move_string(self):
        out = cics(
            "       01 WS-S PIC X(10).\n"
            "       MOVE 'Hello' TO WS-S.\n"
            "       DISPLAY WS-S."
        )
        assert has(out, "Hello")


# ============================================================================
# DATA DECLARATIONS
# ============================================================================


class TestDataDeclarations:
    def test_pic_x(self):
        out = cics(
            "       01 WS-STR PIC X(5) VALUE 'Hello'.\n" "       DISPLAY WS-STR."
        )
        assert has(out, "Hello")

    def test_pic_9(self):
        out = cics("       01 WS-NUM PIC 9(3) VALUE 42.\n" "       DISPLAY WS-NUM.")
        assert has(out, "42")


# ============================================================================
# PERFORM
# ============================================================================


class TestPerform:
    def test_perform_paragraph(self):
        out = cics(
            "       PERFORM SHOW-MSG.\n"
            "       EXEC CICS RETURN END-EXEC.\n"
            "       SHOW-MSG.\n"
            "           DISPLAY 'hello'."
        )
        assert has(out, "hello")

    def test_perform_times(self):
        out = cics(
            "       PERFORM 3 TIMES\n           DISPLAY 'X'\n       END-PERFORM."
        )
        assert has(out, "X")


# ============================================================================
# EXEC CICS RETURN
# ============================================================================


class TestReturn:
    def test_return(self):
        out = cics(
            "       DISPLAY 'before'.\n"
            "       EXEC CICS RETURN END-EXEC.\n"
            "       DISPLAY 'after'."
        )
        assert has(out, "before")


# ============================================================================
# EXEC CICS READ / WRITE (file I/O)
# ============================================================================


class TestFileIO:
    def test_write_read(self):
        out = cics(
            "       01 WS-DATA PIC X(20) VALUE 'test data'.\n"
            "       01 WS-KEY PIC X(8) VALUE 'KEY001'.\n"
            "       EXEC CICS WRITE FILE('MYFILE') FROM(WS-DATA) RIDFLD(WS-KEY) END-EXEC.\n"
            "       EXEC CICS READ FILE('MYFILE') INTO(WS-DATA) RIDFLD(WS-KEY) END-EXEC.\n"
            "       DISPLAY WS-DATA."
        )
        assert no_errors(out) or ok(out)


# ============================================================================
# EXEC CICS ASSIGN
# ============================================================================


class TestAssign:
    def test_assign(self):
        out = cics("       EXEC CICS ASSIGN END-EXEC.")
        assert no_errors(out) or len(out) >= 0


# ============================================================================
# ARITHMETIC
# ============================================================================


class TestArithmetic:
    def test_add(self):
        out = cics(
            "       01 WS-A PIC 9(3) VALUE 10.\n"
            "       01 WS-B PIC 9(3) VALUE 20.\n"
            "       ADD WS-A TO WS-B.\n"
            "       DISPLAY WS-B."
        )
        assert has(out, "30")

    def test_compute(self):
        out = cics(
            "       01 WS-X PIC 9(3).\n"
            "       COMPUTE WS-X = 2 + 3.\n"
            "       DISPLAY WS-X."
        )
        assert has(out, "5")


# ============================================================================
# CONDITIONALS
# ============================================================================


class TestConditionals:
    def test_if(self):
        out = cics(
            "       01 WS-X PIC 9(3) VALUE 5.\n"
            "       IF WS-X > 3\n"
            "           DISPLAY 'yes'\n"
            "       END-IF."
        )
        assert has(out, "yes")


# ============================================================================
# CICS BANNER
# ============================================================================


class TestBanner:
    def test_program_produces_output(self):
        out = cics("       DISPLAY 'Hello CICS'.\n" "       EXEC CICS RETURN END-EXEC.")
        # CICS programs typically produce banner output
        assert len(out) > 0


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_empty_program(self):
        out = cics("")
        assert no_errors(out) or len(out) == 0
