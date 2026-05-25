; Registers and Arithmetic in 6502 Assembly
; Demonstrates all arithmetic operations, flags, and register transfers

        .ORG $0200

; Zero-page scratch
A_VAL   = $10
B_VAL   = $11
RESULT  = $12

START:
        ; --- Basic arithmetic ---
        LDX #<ARITH_HDR
        LDY #>ARITH_HDR
        JSR PRINTSTR
        JSR NEWLINE

        ; Addition: 40 + 27 = 67
        LDA #40
        STA A_VAL
        LDA #27
        STA B_VAL
        CLC
        LDA A_VAL
        ADC B_VAL
        STA RESULT
        LDX #<ADD_MSG
        LDY #>ADD_MSG
        JSR PRINTSTR
        LDA RESULT
        JSR PRINTDEC
        JSR NEWLINE

        ; Subtraction: 100 - 37 = 63
        LDA #100
        SEC
        SBC #37
        STA RESULT
        LDX #<SUB_MSG
        LDY #>SUB_MSG
        JSR PRINTSTR
        LDA RESULT
        JSR PRINTDEC
        JSR NEWLINE

        ; Multiply by 4 via left shifts: 11 * 4 = 44
        LDA #11
        ASL A           ; A = A * 2 (22)
        ASL A           ; A = A * 2 (44)
        STA RESULT
        LDX #<MUL4_MSG
        LDY #>MUL4_MSG
        JSR PRINTSTR
        LDA RESULT
        JSR PRINTDEC
        JSR NEWLINE

        ; Divide by 8 via right shifts: 96 / 8 = 12
        LDA #96
        LSR A           ; /2
        LSR A           ; /4
        LSR A           ; /8
        STA RESULT
        LDX #<DIV8_MSG
        LDY #>DIV8_MSG
        JSR PRINTSTR
        LDA RESULT
        JSR PRINTDEC
        JSR NEWLINE

        ; --- Bitwise operations ---
        LDX #<BITWISE_HDR
        LDY #>BITWISE_HDR
        JSR PRINTSTR
        JSR NEWLINE

        ; AND: $FF & $0F = $0F (mask lower nibble)
        LDA #$FF
        AND #$0F
        LDX #<AND_MSG
        LDY #>AND_MSG
        JSR PRINTSTR
        JSR PRINTHEX
        JSR NEWLINE

        ; OR: $A0 | $0F = $AF
        LDA #$A0
        ORA #$0F
        LDX #<ORA_MSG
        LDY #>ORA_MSG
        JSR PRINTSTR
        JSR PRINTHEX
        JSR NEWLINE

        ; EOR: $FF ^ $AA = $55
        LDA #$FF
        EOR #$AA
        LDX #<EOR_MSG
        LDY #>EOR_MSG
        JSR PRINTSTR
        JSR PRINTHEX
        JSR NEWLINE

        ; --- Register transfers ---
        LDX #<XFER_HDR
        LDY #>XFER_HDR
        JSR PRINTSTR
        JSR NEWLINE

        LDA #42
        TAX             ; X = A
        TAY             ; Y = A
        LDA #0          ; Clear A
        TXA             ; A = X (42)
        LDX #<TXA_MSG
        LDY #>TXA_MSG
        JSR PRINTSTR
        JSR PRINTDEC
        JSR NEWLINE

        BRK

; String data
ARITH_HDR:  .ASCIIZ "=== Arithmetic ==="
ADD_MSG:    .ASCIIZ "40 + 27 = "
SUB_MSG:    .ASCIIZ "100 - 37 = "
MUL4_MSG:   .ASCIIZ "11 * 4 (via ASL) = "
DIV8_MSG:   .ASCIIZ "96 / 8 (via LSR) = "
BITWISE_HDR:.ASCIIZ "=== Bitwise ==="
AND_MSG:    .ASCIIZ "$FF AND $0F = $"
ORA_MSG:    .ASCIIZ "$A0 ORA $0F = $"
EOR_MSG:    .ASCIIZ "$FF EOR $AA = $"
XFER_HDR:   .ASCIIZ "=== Transfers ==="
TXA_MSG:    .ASCIIZ "TXA (value should be 42): "
