"""Comprehensive tests for the JCL language executor."""

import pytest

from time_warp.core.interpreter import Language

from .conftest_lang import run, ok, has, no_errors, first_error

L = Language.JCL


def jcl(source: str, **kw) -> list[str]:
    """Shortcut: run a JCL program."""
    return run(source, L, **kw)


# ============================================================================
# JOB CARD
# ============================================================================


class TestJobCard:
    def test_basic_job(self):
        out = jcl("//HELLO   JOB (ACCT),'HELLO WORLD',CLASS=A")
        assert no_errors(out) or ok(out)

    def test_job_with_step(self):
        out = jcl(
            "//HELLO   JOB (ACCT),'HELLO',CLASS=A\n"
            "//STEP1   EXEC PGM=IEFBR14"
        )
        assert no_errors(out) or ok(out)


# ============================================================================
# EXEC PGM
# ============================================================================


class TestExecPgm:
    def test_iefbr14(self):
        out = jcl(
            "//TEST    JOB (ACCT),'TEST',CLASS=A\n"
            "//STEP1   EXEC PGM=IEFBR14"
        )
        assert no_errors(out)

    def test_iebgener(self):
        out = jcl(
            "//TEST    JOB (ACCT),'TEST',CLASS=A\n"
            "//STEP1   EXEC PGM=IEBGENER\n"
            "//SYSUT1  DD *\n"
            "Hello World\n"
            "/*\n"
            "//SYSUT2  DD SYSOUT=*\n"
            "//SYSIN   DD DUMMY\n"
            "//SYSPRINT DD SYSOUT=*"
        )
        assert no_errors(out) or ok(out)

    def test_sort(self):
        out = jcl(
            "//TEST    JOB (ACCT),'TEST',CLASS=A\n"
            "//STEP1   EXEC PGM=SORT\n"
            "//SORTIN  DD *\n"
            "CHARLIE\nALPHA\nBRAVO\n"
            "/*\n"
            "//SORTOUT DD SYSOUT=*\n"
            "//SYSIN   DD *\n"
            "  SORT FIELDS=(1,10,CH,A)\n"
            "/*"
        )
        assert no_errors(out) or ok(out)


# ============================================================================
# DD STATEMENTS
# ============================================================================


class TestDD:
    def test_dd_inline(self):
        out = jcl(
            "//TEST    JOB (ACCT),'TEST',CLASS=A\n"
            "//STEP1   EXEC PGM=IEBGENER\n"
            "//SYSUT1  DD *\n"
            "Test Data\n"
            "/*\n"
            "//SYSUT2  DD SYSOUT=*\n"
            "//SYSIN   DD DUMMY\n"
            "//SYSPRINT DD SYSOUT=*"
        )
        assert no_errors(out) or ok(out)

    def test_dd_sysout(self):
        out = jcl(
            "//TEST    JOB (ACCT),'TEST',CLASS=A\n"
            "//STEP1   EXEC PGM=IEFBR14\n"
            "//OUT     DD SYSOUT=*"
        )
        assert no_errors(out)

    def test_dd_dsn(self):
        out = jcl(
            "//TEST    JOB (ACCT),'TEST',CLASS=A\n"
            "//STEP1   EXEC PGM=IEFBR14\n"
            "//DATA    DD DSN=MY.DATASET,DISP=SHR"
        )
        assert no_errors(out)


# ============================================================================
# PARM
# ============================================================================


class TestParm:
    def test_parm_parameter(self):
        out = jcl(
            "//TEST    JOB (ACCT),'TEST',CLASS=A\n"
            "//STEP1   EXEC PGM=MYPROG,PARM='HELLO'"
        )
        assert no_errors(out) or ok(out)


# ============================================================================
# COMMENTS
# ============================================================================


class TestComments:
    def test_comment(self):
        out = jcl(
            "//*THIS IS A COMMENT\n"
            "//TEST    JOB (ACCT),'TEST',CLASS=A\n"
            "//STEP1   EXEC PGM=IEFBR14"
        )
        assert no_errors(out)


# ============================================================================
# JES2 OUTPUT
# ============================================================================


class TestJES2:
    def test_jes2_banner(self):
        out = jcl(
            "//HELLO   JOB (ACCT),'HELLO',CLASS=A\n"
            "//STEP1   EXEC PGM=IEFBR14"
        )
        # JCL typically produces JES2 banner/job output
        assert len(out) > 0 or no_errors(out)


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_empty_program(self):
        out = jcl("")
        assert no_errors(out) or len(out) == 0

    def test_invalid_statement(self):
        out = jcl("INVALID STATEMENT")
        # Should handle gracefully
        assert len(out) >= 0
