"""Comprehensive tests for the FORTRAN language executor."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors

L = Language.FORTRAN


def fort(source: str, **kw) -> list[str]:
    """Shortcut: run a FORTRAN program."""
    return run(source, L, **kw)


# ============================================================================
# OUTPUT (WRITE / PRINT)
# ============================================================================


class TestOutput:
    def test_write_string(self):
        out = fort(
            "      PROGRAM HELLO\n      WRITE(*,*) 'Hello World'\n      STOP\n      END"
        )
        assert has(out, "Hello World")

    def test_print_string(self):
        out = fort(
            "      PROGRAM HELLO\n      PRINT *, 'Test Output'\n      STOP\n      END"
        )
        assert has(out, "Test Output")

    def test_write_number(self):
        out = fort("      PROGRAM N\n      WRITE(*,*) 42\n      STOP\n      END")
        assert has(out, "42")

    def test_write_expression(self):
        out = fort("      PROGRAM E\n      WRITE(*,*) 3 + 4\n      STOP\n      END")
        assert has(out, "7")


# ============================================================================
# VARIABLES / DECLARATIONS
# ============================================================================


class TestVariables:
    def test_integer(self):
        out = fort(
            "      PROGRAM V\n"
            "      INTEGER X\n"
            "      X = 42\n"
            "      WRITE(*,*) X\n"
            "      STOP\n"
            "      END"
        )
        assert has(out, "42")

    def test_real(self):
        out = fort(
            "      PROGRAM V\n"
            "      REAL R\n"
            "      R = 3.14\n"
            "      WRITE(*,*) R\n"
            "      STOP\n"
            "      END"
        )
        assert has(out, "3.14")

    def test_character(self):
        out = fort(
            "      PROGRAM V\n"
            "      CHARACTER*10 S\n"
            "      S = 'Hello'\n"
            "      WRITE(*,*) S\n"
            "      STOP\n"
            "      END"
        )
        assert has(out, "Hello")

    def test_parameter(self):
        out = fort(
            "      PROGRAM V\n"
            "      INTEGER N\n"
            "      PARAMETER (N = 42)\n"
            "      WRITE(*,*) N\n"
            "      STOP\n"
            "      END"
        )
        assert has(out, "42")


# ============================================================================
# ARITHMETIC
# ============================================================================


class TestArithmetic:
    def test_addition(self):
        out = fort("      PROGRAM A\n      WRITE(*,*) 2 + 3\n      STOP\n      END")
        assert has(out, "5")

    def test_subtraction(self):
        out = fort("      PROGRAM A\n      WRITE(*,*) 10 - 4\n      STOP\n      END")
        assert has(out, "6")

    def test_multiplication(self):
        out = fort("      PROGRAM A\n      WRITE(*,*) 6 * 7\n      STOP\n      END")
        assert has(out, "42")

    def test_division(self):
        out = fort("      PROGRAM A\n      WRITE(*,*) 20 / 4\n      STOP\n      END")
        assert has(out, "5")

    def test_power(self):
        out = fort("      PROGRAM A\n      WRITE(*,*) 2 ** 10\n      STOP\n      END")
        assert has(out, "1024")


# ============================================================================
# IF / THEN / ELSE
# ============================================================================


class TestConditional:
    def test_if_true(self):
        out = fort(
            "      PROGRAM I\n"
            "      INTEGER X\n"
            "      X = 5\n"
            "      IF (X .GT. 3) THEN\n"
            "        WRITE(*,*) 'yes'\n"
            "      ENDIF\n"
            "      STOP\n"
            "      END"
        )
        assert has(out, "yes")

    def test_if_else(self):
        out = fort(
            "      PROGRAM I\n"
            "      INTEGER X\n"
            "      X = 1\n"
            "      IF (X .GT. 3) THEN\n"
            "        WRITE(*,*) 'big'\n"
            "      ELSE\n"
            "        WRITE(*,*) 'small'\n"
            "      ENDIF\n"
            "      STOP\n"
            "      END"
        )
        assert has(out, "small")


# ============================================================================
# DO LOOP
# ============================================================================


class TestDoLoop:
    def test_do_loop(self):
        out = fort(
            "      PROGRAM D\n"
            "      INTEGER I\n"
            "      DO 10 I = 1, 3\n"
            "        WRITE(*,*) I\n"
            "   10 CONTINUE\n"
            "      STOP\n"
            "      END"
        )
        assert has(out, "1") and has(out, "3")


# ============================================================================
# GOTO
# ============================================================================


class TestGoto:
    def test_goto(self):
        out = fort(
            "      PROGRAM G\n"
            "      GOTO 20\n"
            "      WRITE(*,*) 'skip'\n"
            "   20 WRITE(*,*) 'here'\n"
            "      STOP\n"
            "      END"
        )
        assert has(out, "here") and not has(out, "skip")


# ============================================================================
# SUBROUTINE / CALL
# ============================================================================


class TestSubroutine:
    def test_subroutine_call(self):
        out = fort(
            "      PROGRAM S\n"
            "      CALL GREET\n"
            "      STOP\n"
            "      END\n"
            "      SUBROUTINE GREET\n"
            "      WRITE(*,*) 'hello'\n"
            "      RETURN\n"
            "      END"
        )
        assert has(out, "hello")


# ============================================================================
# STOP / END
# ============================================================================


class TestStopEnd:
    def test_stop(self):
        out = fort(
            "      PROGRAM S\n"
            "      WRITE(*,*) 'before'\n"
            "      STOP\n"
            "      WRITE(*,*) 'after'\n"
            "      END"
        )
        assert has(out, "before")


# ============================================================================
# COMMENTS
# ============================================================================


class TestComments:
    def test_c_comment(self):
        out = fort(
            "C     THIS IS A COMMENT\n"
            "      PROGRAM T\n"
            "      WRITE(*,*) 'OK'\n"
            "      STOP\n"
            "      END"
        )
        assert has(out, "OK")


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_empty_program(self):
        out = fort("")
        assert no_errors(out) or len(out) == 0


# ============================================================================
# Free-form Fortran (regression)
# ============================================================================


class TestFreeForm:
    """Regression: Free-form Fortran with :: declarations and END DO."""

    def test_free_form_do_loop(self):
        src = "PROGRAM FREETEST\nINTEGER :: I\nDO I = 1, 3\n  PRINT *, I\nEND DO\nEND PROGRAM"
        out = fort(src)
        assert no_errors(out)
        assert has(out, "1")
        assert has(out, "3")

    def test_free_form_real_decl(self):
        src = "PROGRAM REALTEST\nREAL :: X = 3.14\nPRINT *, X\nEND PROGRAM"
        out = fort(src)
        assert no_errors(out)
        assert has(out, "3.14")
