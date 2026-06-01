"""Comprehensive tests for the 6502 Assembly executor."""

from __future__ import annotations

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.ASM6502


def asm(source: str) -> list[str]:
    return run(source, LANG)


# ---------------------------------------------------------------------------
# Basic programs — check no errors and some output
# ---------------------------------------------------------------------------


def test_hello_world():
    out = asm(
        "; Hello World\n"
        "        .ORG $0200\n"
        "START:\n"
        '        LDX #<MSG\n'
        "        LDY #>MSG\n"
        "        JSR PRINTSTR\n"
        "        BRK\n"
        'MSG:    .ASCIIZ "Hello, World!"\n'
    )
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_print_char():
    out = asm(
        "        .ORG $0200\n"
        "START:\n"
        "        LDA #'A'\n"
        "        JSR CHROUT\n"
        "        BRK\n"
    )
    assert no_errors(out)
    assert has(out, "A")


def test_print_number():
    out = asm(
        "        .ORG $0200\n"
        "START:\n"
        "        LDA #42\n"
        "        JSR PRINTDEC\n"
        "        BRK\n"
    )
    assert no_errors(out)
    assert has(out, "42")


def test_load_store():
    out = asm(
        "        .ORG $0200\n"
        "START:\n"
        "        LDA #$41\n"     # 65 = 'A'
        "        STA $00\n"
        "        LDA $00\n"
        "        JSR CHROUT\n"
        "        BRK\n"
    )
    assert no_errors(out)
    assert has(out, "A")


def test_increment():
    out = asm(
        "        .ORG $0200\n"
        "START:\n"
        "        LDA #41\n"      # 41 + 1 = 42
        "        CLC\n"
        "        ADC #1\n"
        "        JSR PRINTDEC\n"
        "        BRK\n"
    )
    assert no_errors(out)
    assert has(out, "42")


def test_loop_with_dex():
    out = asm(
        "        .ORG $0200\n"
        "START:\n"
        "        LDX #3\n"
        "LOOP:\n"
        "        TXA\n"
        "        JSR PRINTDEC\n"
        "        JSR NEWLINE\n"
        "        DEX\n"
        "        BNE LOOP\n"
        "        BRK\n"
    )
    assert no_errors(out)
    assert has(out, "3", "2", "1")


def test_conditional_branch():
    out = asm(
        "        .ORG $0200\n"
        "START:\n"
        "        LDA #5\n"
        "        CMP #5\n"
        "        BEQ EQUAL\n"
        "        LDA #'N'\n"
        "        JSR CHROUT\n"
        "        BRK\n"
        "EQUAL:\n"
        "        LDA #'Y'\n"
        "        JSR CHROUT\n"
        "        BRK\n"
    )
    assert no_errors(out)
    assert has(out, "Y")


def test_subroutine():
    out = asm(
        "        .ORG $0200\n"
        "MAIN:\n"
        "        JSR GREET\n"
        "        BRK\n"
        "GREET:\n"
        '        LDX #<MSG\n'
        "        LDY #>MSG\n"
        "        JSR PRINTSTR\n"
        "        RTS\n"
        'MSG:    .ASCIIZ "Hi!"\n'
    )
    assert no_errors(out)
    assert has(out, "Hi!")


# ---------------------------------------------------------------------------
# Additional arithmetic
# ---------------------------------------------------------------------------


def test_adc_arithmetic():
    out = asm(
        "        .ORG $0200\n"
        "START:\n"
        "        LDA #10\n"
        "        CLC\n"
        "        ADC #5\n"
        "        JSR PRINTDEC\n"
        "        BRK\n"
    )
    assert no_errors(out)
    assert has(out, "15")


def test_sbc_arithmetic():
    out = asm(
        "        .ORG $0200\n"
        "START:\n"
        "        LDA #20\n"
        "        SEC\n"
        "        SBC #8\n"
        "        JSR PRINTDEC\n"
        "        BRK\n"
    )
    assert no_errors(out)
    assert has(out, "12")


def test_y_register():
    out = asm(
        "        .ORG $0200\n"
        "START:\n"
        "        LDY #7\n"
        "        TYA\n"
        "        JSR PRINTDEC\n"
        "        BRK\n"
    )
    assert no_errors(out)
    assert has(out, "7")


def test_inx_register():
    out = asm(
        "        .ORG $0200\n"
        "START:\n"
        "        LDX #9\n"
        "        INX\n"
        "        TXA\n"
        "        JSR PRINTDEC\n"
        "        BRK\n"
    )
    assert no_errors(out)
    assert has(out, "10")


def test_dex_register():
    out = asm(
        "        .ORG $0200\n"
        "START:\n"
        "        LDX #5\n"
        "        DEX\n"
        "        TXA\n"
        "        JSR PRINTDEC\n"
        "        BRK\n"
    )
    assert no_errors(out)
    assert has(out, "4")


def test_branch_equal():
    out = asm(
        "        .ORG $0200\n"
        "START:\n"
        "        LDA #5\n"
        "        CMP #5\n"
        "        BEQ EQUAL\n"
        "        LDA #0\n"
        "        JSR PRINTDEC\n"
        "        JMP DONE\n"
        "EQUAL:\n"
        "        LDA #99\n"
        "        JSR PRINTDEC\n"
        "DONE:\n"
        "        BRK\n"
    )
    assert no_errors(out)
    assert has(out, "99")


def test_lda_immediate():
    out = asm(
        "START:\n"
        "        LDA #42\n"
        "        JSR PRINTDEC\n"
        "        BRK\n"
    )
    assert no_errors(out)
    assert has(out, "42")


def test_adc_add():
    out = asm(
        "START:\n"
        "        LDA #10\n"
        "        CLC\n"
        "        ADC #5\n"
        "        JSR PRINTDEC\n"
        "        BRK\n"
    )
    assert no_errors(out)
    assert has(out, "15")


def test_dey_register():
    out = asm(
        "START:\n"
        "        LDY #7\n"
        "        DEY\n"
        "        TYA\n"
        "        JSR PRINTDEC\n"
        "        BRK\n"
    )
    assert no_errors(out)
    assert has(out, "6")


def test_lda_100():
    out = asm(
        "START:\n"
        "        LDA #100\n"
        "        JSR PRINTDEC\n"
        "        BRK\n"
    )
    assert no_errors(out)
    assert has(out, "100")


def test_ldx_sta_lda():
    out = asm(
        "START:\n"
        "        LDX #7\n"
        "        STX $10\n"
        "        LDA $10\n"
        "        JSR PRINTDEC\n"
        "        BRK\n"
    )
    assert no_errors(out)
    assert has(out, "7")


def test_beq_branch():
    out = asm(
        "START:\n"
        "        LDA #0\n"
        "        CMP #0\n"
        "        BEQ EQUAL\n"
        "        BRK\n"
        "EQUAL:\n"
        "        LDA #99\n"
        "        JSR PRINTDEC\n"
        "        BRK\n"
    )
    assert no_errors(out)
    assert has(out, "99")


# ---------------------------------------------------------------------------
# Example programs
# ---------------------------------------------------------------------------


def test_hello_example():
    import pathlib
    src = (
        pathlib.Path(__file__).parents[4] / "Examples" / "asm6502" / "hello.asm"
    ).read_text()
    out = asm(src)
    assert no_errors(out)
    assert ok(out)
