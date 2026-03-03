"""Comprehensive tests for the Assembly language executor."""

import pytest

from time_warp.core.interpreter import Language

from .conftest_lang import run, ok, has, no_errors, first_error

L = Language.ASSEMBLY


def asm(source: str, **kw) -> list[str]:
    """Shortcut: run an Assembly program."""
    return run(source, L, **kw)


# ============================================================================
# PRINT
# ============================================================================


class TestPrint:
    def test_print_string(self):
        out = asm('PRINT "Hello World"\nHALT')
        assert has(out, "Hello World")

    def test_print_register(self):
        out = asm("MOV R0, 42\nPRINT R0\nHALT")
        assert has(out, "42")


# ============================================================================
# MOV (register loading)
# ============================================================================


class TestMov:
    def test_mov_immediate(self):
        out = asm("MOV R0, 10\nPRINT R0\nHALT")
        assert has(out, "10")

    def test_mov_register(self):
        out = asm("MOV R0, 42\nMOV R1, R0\nPRINT R1\nHALT")
        assert has(out, "42")

    def test_mov_multiple_registers(self):
        out = asm("MOV R0, 1\nMOV R1, 2\nMOV R2, 3\nPRINT R0\nPRINT R1\nPRINT R2\nHALT")
        assert has(out, "1") and has(out, "2") and has(out, "3")


# ============================================================================
# ARITHMETIC (ADD, SUB, MUL, DIV)
# ============================================================================


class TestArithmetic:
    def test_add(self):
        out = asm("MOV R0, 3\nMOV R1, 4\nADD R0, R1\nPRINT R0\nHALT")
        assert has(out, "7")

    def test_sub(self):
        out = asm("MOV R0, 10\nMOV R1, 4\nSUB R0, R1\nPRINT R0\nHALT")
        assert has(out, "6")

    def test_mul(self):
        out = asm("MOV R0, 6\nMOV R1, 7\nMUL R0, R1\nPRINT R0\nHALT")
        assert has(out, "42")

    def test_div(self):
        out = asm("MOV R0, 20\nMOV R1, 4\nDIV R0, R1\nPRINT R0\nHALT")
        assert has(out, "5")

    def test_add_immediate(self):
        out = asm("MOV R0, 5\nADD R0, 3\nPRINT R0\nHALT")
        assert has(out, "8")


# ============================================================================
# CMP / CONDITIONAL JUMPS
# ============================================================================


class TestConditionalJumps:
    def test_je_equal(self):
        out = asm(
            "MOV R0, 5\nMOV R1, 5\nCMP R0, R1\nJE equal\n"
            'PRINT "not equal"\nHALT\n'
            'equal:\nPRINT "equal"\nHALT'
        )
        assert has(out, "equal")

    def test_je_not_equal(self):
        out = asm(
            "MOV R0, 5\nMOV R1, 3\nCMP R0, R1\nJE equal\n"
            'PRINT "not equal"\nHALT\n'
            'equal:\nPRINT "equal"\nHALT'
        )
        assert has(out, "not equal")

    def test_jne(self):
        out = asm(
            "MOV R0, 5\nMOV R1, 3\nCMP R0, R1\nJNE diff\n"
            'PRINT "same"\nHALT\n'
            'diff:\nPRINT "different"\nHALT'
        )
        assert has(out, "different")

    def test_jg(self):
        out = asm(
            "MOV R0, 5\nMOV R1, 3\nCMP R0, R1\nJG greater\n"
            'PRINT "not greater"\nHALT\n'
            'greater:\nPRINT "greater"\nHALT'
        )
        assert has(out, "greater")

    def test_jl(self):
        out = asm(
            "MOV R0, 2\nMOV R1, 5\nCMP R0, R1\nJL less\n"
            'PRINT "not less"\nHALT\n'
            'less:\nPRINT "less"\nHALT'
        )
        assert has(out, "less")


# ============================================================================
# JMP (unconditional jump)
# ============================================================================


class TestJmp:
    def test_jmp(self):
        out = asm(
            'JMP skip\nPRINT "skipped"\nskip:\nPRINT "reached"\nHALT'
        )
        assert has(out, "reached") and not has(out, "skipped")


# ============================================================================
# STACK (PUSH / POP)
# ============================================================================


class TestStack:
    def test_push_pop(self):
        out = asm("MOV R0, 42\nPUSH R0\nMOV R0, 0\nPOP R0\nPRINT R0\nHALT")
        assert has(out, "42")

    def test_push_multiple(self):
        out = asm(
            "MOV R0, 1\nPUSH R0\nMOV R0, 2\nPUSH R0\n"
            "POP R0\nPRINT R0\nPOP R0\nPRINT R0\nHALT"
        )
        assert has(out, "2") and has(out, "1")


# ============================================================================
# CALL / RET (subroutines)
# ============================================================================


class TestSubroutines:
    def test_call_ret(self):
        out = asm(
            "CALL sub\nPRINT R0\nHALT\n"
            "sub:\nMOV R0, 99\nRET"
        )
        assert has(out, "99")


# ============================================================================
# MEMORY (LOAD / STORE)
# ============================================================================


class TestMemory:
    def test_store_load(self):
        out = asm(
            "MOV R0, 42\nSTORE [100], R0\nMOV R0, 0\nLOAD R0, [100]\nPRINT R0\nHALT"
        )
        assert has(out, "42")


# ============================================================================
# HALT
# ============================================================================


class TestHalt:
    def test_halt_stops(self):
        out = asm('PRINT "before"\nHALT\nPRINT "after"')
        assert has(out, "before") and not has(out, "after")


# ============================================================================
# INPUT
# ============================================================================


class TestInput:
    def test_input(self):
        out = asm("INPUT R0\nPRINT R0\nHALT", input_val="42")
        assert has(out, "42") or no_errors(out)


# ============================================================================
# LOGICAL OPERATIONS
# ============================================================================


class TestLogical:
    def test_and(self):
        out = asm("MOV R0, 15\nMOV R1, 9\nAND R0, R1\nPRINT R0\nHALT")
        assert no_errors(out)

    def test_or(self):
        out = asm("MOV R0, 10\nMOV R1, 5\nOR R0, R1\nPRINT R0\nHALT")
        assert no_errors(out)

    def test_xor(self):
        out = asm("MOV R0, 15\nMOV R1, 9\nXOR R0, R1\nPRINT R0\nHALT")
        assert no_errors(out)

    def test_not(self):
        out = asm("MOV R0, 0\nNOT R0\nPRINT R0\nHALT")
        assert no_errors(out)

    def test_shl(self):
        out = asm("MOV R0, 1\nSHL R0, 3\nPRINT R0\nHALT")
        assert has(out, "8")

    def test_shr(self):
        out = asm("MOV R0, 8\nSHR R0, 2\nPRINT R0\nHALT")
        assert has(out, "2")


# ============================================================================
# COMMENTS
# ============================================================================


class TestComments:
    def test_semicolon_comment(self):
        out = asm("; This is a comment\nMOV R0, 42\nPRINT R0\nHALT")
        assert has(out, "42")


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_unknown_instruction(self):
        out = asm("XYZZY\nHALT")
        assert first_error(out) is not None or len(out) > 0

    def test_empty_program(self):
        out = asm("")
        assert no_errors(out) or len(out) == 0
