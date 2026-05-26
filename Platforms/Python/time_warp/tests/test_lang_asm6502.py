"""Tests for the 6502 Assembly language executor."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors

L = Language.ASM6502


def asm(source: str, **kw) -> list[str]:
    return run(source, L, **kw)


# ============================================================================
# BASIC OUTPUT (JSR $FFD2 = CHROUT)
# ============================================================================

class TestPrintChar:
    def test_print_A(self):
        # LDA #65 = 'A', JSR CHROUT
        out = asm("LDA #65\nJSR $FFD2\nBRK")
        assert has(out, "A")

    def test_print_newline(self):
        out = asm("JSR $FFCC\nBRK")
        assert no_errors(out)

    def test_print_decimal(self):
        # PRINTDEC = $FFB8
        out = asm("LDA #42\nJSR $FFB8\nBRK")
        assert has(out, "42")

    def test_print_hex(self):
        # PRINTHEX = $FFB0
        out = asm("LDA #$1A\nJSR $FFB0\nBRK")
        assert has(out, "1A") or has(out, "1a")

    def test_hello_world_via_chrout(self):
        lines = ["LDA #72", "JSR $FFD2",  # H
                 "LDA #101", "JSR $FFD2",  # e
                 "LDA #108", "JSR $FFD2",  # l
                 "LDA #108", "JSR $FFD2",  # l
                 "LDA #111", "JSR $FFD2",  # o
                 "BRK"]
        out = asm("\n".join(lines))
        assert has(out, "Hello") or has(out, "H")


class TestPrintString:
    def test_printstr_basic(self):
        prog = """
        .ORG $0200
        LDX #<msg
        LDY #>msg
        JSR $FFA0
        BRK
msg:    .TEXT "Hello"
        .BYTE 0
"""
        out = asm(prog)
        assert no_errors(out)

    def test_print_signed_dec(self):
        # PRINTSDEC = $FF90
        out = asm("LDA #255\nJSR $FF90\nBRK")  # 255 as signed = -1
        assert no_errors(out)


# ============================================================================
# REGISTERS AND ARITHMETIC
# ============================================================================

class TestRegisters:
    def test_lda_immediate(self):
        out = asm("LDA #10\nJSR $FFB8\nBRK")
        assert has(out, "10")

    def test_ldx_immediate(self):
        out = asm("LDX #5\nTXA\nJSR $FFB8\nBRK")
        assert has(out, "5")

    def test_ldy_immediate(self):
        out = asm("LDY #7\nTYA\nJSR $FFB8\nBRK")
        assert has(out, "7")

    def test_tax_txa(self):
        out = asm("LDA #99\nTAX\nTXA\nJSR $FFB8\nBRK")
        assert has(out, "99")

    def test_tay_tya(self):
        out = asm("LDA #88\nTAY\nTYA\nJSR $FFB8\nBRK")
        assert has(out, "88")

    def test_clc_adc(self):
        # 3 + 4 = 7
        out = asm("CLC\nLDA #3\nADC #4\nJSR $FFB8\nBRK")
        assert has(out, "7")

    def test_sec_sbc(self):
        # 10 - 3 = 7
        out = asm("SEC\nLDA #10\nSBC #3\nJSR $FFB8\nBRK")
        assert has(out, "7")

    def test_and(self):
        # 0xFF AND 0x0F = 0x0F = 15
        out = asm("LDA #$FF\nAND #$0F\nJSR $FFB8\nBRK")
        assert has(out, "15")

    def test_ora(self):
        # 0x0F OR 0xF0 = 0xFF = 255
        out = asm("LDA #$0F\nORA #$F0\nJSR $FFB8\nBRK")
        assert has(out, "255")

    def test_eor(self):
        # 0xFF XOR 0xFF = 0x00 = 0
        out = asm("LDA #$FF\nEOR #$FF\nJSR $FFB8\nBRK")
        assert has(out, "0")

    def test_inx(self):
        out = asm("LDX #5\nINX\nTXA\nJSR $FFB8\nBRK")
        assert has(out, "6")

    def test_dex(self):
        out = asm("LDX #5\nDEX\nTXA\nJSR $FFB8\nBRK")
        assert has(out, "4")

    def test_iny(self):
        out = asm("LDY #5\nINY\nTYA\nJSR $FFB8\nBRK")
        assert has(out, "6")

    def test_dey(self):
        out = asm("LDY #5\nDEY\nTYA\nJSR $FFB8\nBRK")
        assert has(out, "4")


# ============================================================================
# BRANCHES
# ============================================================================

class TestBranches:
    def test_bne_loop(self):
        # Count from 0 to 3 using BNE
        prog = """
        LDX #0
loop:   TXA
        JSR $FFB8
        INX
        CPX #3
        BNE loop
        BRK
"""
        out = asm(prog)
        assert has(out, "0") and has(out, "2")

    def test_beq(self):
        prog = """
        LDA #5
        CMP #5
        BEQ equal
        LDA #1
        JSR $FFB8
        BRK
equal:  LDA #99
        JSR $FFB8
        BRK
"""
        out = asm(prog)
        assert has(out, "99")

    def test_bcc(self):
        # LDA #3, CMP #5 → carry clear (3 < 5)
        prog = """
        CLC
        LDA #3
        CMP #5
        BCC small
        LDA #0
        JSR $FFB8
        BRK
small:  LDA #1
        JSR $FFB8
        BRK
"""
        out = asm(prog)
        assert has(out, "1")

    def test_bcs(self):
        prog = """
        LDA #10
        CMP #5
        BCS big
        LDA #0
        JSR $FFB8
        BRK
big:    LDA #1
        JSR $FFB8
        BRK
"""
        out = asm(prog)
        assert has(out, "1")

    def test_bmi_bpl(self):
        # LDA with negative bit set → BMI branches
        prog = """
        LDA #$80
        BMI negative
        LDA #0
        JSR $FFB8
        BRK
negative: LDA #1
        JSR $FFB8
        BRK
"""
        out = asm(prog)
        assert has(out, "1")


# ============================================================================
# MEMORY ACCESS
# ============================================================================

class TestMemory:
    def test_sta_lda_zeropage(self):
        prog = """
        LDA #42
        STA $10
        LDA $10
        JSR $FFB8
        BRK
"""
        out = asm(prog)
        assert has(out, "42")

    def test_sta_lda_absolute(self):
        prog = """
        LDA #77
        STA $0300
        LDA $0300
        JSR $FFB8
        BRK
"""
        out = asm(prog)
        assert has(out, "77")

    def test_indexed_x(self):
        prog = """
        LDX #1
        LDA #55
        STA $10,X
        LDA $10,X
        JSR $FFB8
        BRK
"""
        out = asm(prog)
        assert has(out, "55")

    def test_stack_push_pull(self):
        prog = """
        LDA #33
        PHA
        LDA #0
        PLA
        JSR $FFB8
        BRK
"""
        out = asm(prog)
        assert has(out, "33")


# ============================================================================
# JSR / RTS (subroutines)
# ============================================================================

class TestSubroutines:
    def test_jsr_rts(self):
        prog = """
        JSR print_hi
        BRK
print_hi:
        LDA #72
        JSR $FFD2
        LDA #105
        JSR $FFD2
        RTS
"""
        out = asm(prog)
        assert has(out, "Hi")

    def test_nested_subroutine(self):
        prog = """
        JSR outer
        BRK
outer:  JSR inner
        RTS
inner:  LDA #88
        JSR $FFB8
        RTS
"""
        out = asm(prog)
        assert has(out, "88")


# ============================================================================
# DIRECTIVES
# ============================================================================

class TestDirectives:
    def test_org_directive(self):
        prog = """
        .ORG $0200
        LDA #5
        JSR $FFB8
        BRK
"""
        out = asm(prog)
        assert has(out, "5")

    def test_byte_directive(self):
        prog = """
        .ORG $0200
        JMP start
data:   .BYTE $41, $42, $43   ; A, B, C
start:
        LDA data
        JSR $FFD2
        BRK
"""
        out = asm(prog)
        assert no_errors(out)

    def test_equ_directive(self):
        prog = """
CHROUT  EQU $FFD2
        LDA #65
        JSR CHROUT
        BRK
"""
        out = asm(prog)
        assert has(out, "A")

    def test_labels(self):
        prog = """
        LDA #0
        JMP skip
        LDA #1      ; should be skipped
skip:
        JSR $FFB8
        BRK
"""
        out = asm(prog)
        assert has(out, "0")


# ============================================================================
# SHIFT / ROTATE
# ============================================================================

class TestShift:
    def test_lsr(self):
        # 8 >> 1 = 4
        out = asm("LDA #8\nLSR A\nJSR $FFB8\nBRK")
        assert has(out, "4")

    def test_asl(self):
        # 3 << 1 = 6
        out = asm("LDA #3\nASL A\nJSR $FFB8\nBRK")
        assert has(out, "6")

    def test_rol_ror(self):
        out = asm("CLC\nLDA #1\nROL A\nJSR $FFB8\nBRK")
        assert has(out, "2")


# ============================================================================
# COMPARE
# ============================================================================

class TestCompare:
    def test_cmp_equal(self):
        prog = """
        LDA #5
        CMP #5
        BEQ done
        LDA #0
        JSR $FFB8
        BRK
done:   LDA #1
        JSR $FFB8
        BRK
"""
        out = asm(prog)
        assert has(out, "1")

    def test_cpx(self):
        prog = """
        LDX #3
        CPX #3
        BEQ done
        LDA #0
        JSR $FFB8
        BRK
done:   LDA #1
        JSR $FFB8
        BRK
"""
        out = asm(prog)
        assert has(out, "1")

    def test_cpy(self):
        prog = """
        LDY #7
        CPY #7
        BEQ done
        LDA #0
        JSR $FFB8
        BRK
done:   LDA #1
        JSR $FFB8
        BRK
"""
        out = asm(prog)
        assert has(out, "1")


# ============================================================================
# NO-ERROR CHECKS
# ============================================================================

class TestNoErrors:
    def test_nop(self):
        assert no_errors(asm("NOP\nBRK"))

    def test_brk_alone(self):
        assert no_errors(asm("BRK"))

    def test_multiline_program(self):
        prog = """
        LDA #1
        CLC
        ADC #2
        STA $10
        LDA $10
        JSR $FFB8
        BRK
"""
        out = asm(prog)
        assert has(out, "3")

    def test_countdown(self):
        prog = """
        LDA #5
loop:   JSR $FFB8
        SEC
        SBC #1
        BNE loop
        BRK
"""
        out = asm(prog)
        assert has(out, "5") and has(out, "1")
