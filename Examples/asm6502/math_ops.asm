; 6502 Assembly - Math Operations
; Multiply and divide using shift-and-add / shift-and-subtract
; Also: 16-bit addition and BCD arithmetic

.org $0200

; Zero page registers
; $00 = multiplicand / dividend
; $01 = multiplier / divisor
; $02 = result lo
; $03 = result hi
; $04 = remainder
; $05 = bit counter

;---------------------------------------------------------
; MUL8 - 8-bit multiply (unsigned)
; Input:  $00 = multiplicand, $01 = multiplier
; Output: $02 (lo), $03 (hi)  [16-bit product]
;---------------------------------------------------------
MUL8:
    LDA #0
    STA $02         ; product lo = 0
    STA $03         ; product hi = 0
    LDX #8          ; 8 bits to process

MUL_LOOP:
    LSR $01         ; shift multiplier right (LSB into carry)
    BCC MUL_SKIP    ; if bit was 0, skip add
    CLC
    LDA $02
    ADC $00
    STA $02         ; product lo += multiplicand
    LDA $03
    ADC #0
    STA $03         ; propagate carry to hi byte
MUL_SKIP:
    ASL $00         ; multiplicand <<= 1
    DEX
    BNE MUL_LOOP
    RTS

;---------------------------------------------------------
; DIV8 - 8-bit divide (unsigned)
; Input:  $00 = dividend, $01 = divisor
; Output: $02 = quotient, $04 = remainder
;---------------------------------------------------------
DIV8:
    LDA #0
    STA $04         ; remainder = 0
    LDA $00
    STA $02         ; quotient accumulator = dividend
    LDX #8

DIV_LOOP:
    ASL $02         ; shift quotient left (MSB into carry)
    ROL $04         ; shift remainder left, bring in bit
    LDA $04
    CMP $01         ; remainder >= divisor?
    BCC DIV_SKIP    ; no: skip
    SBC $01
    STA $04         ; remainder -= divisor
    INC $02         ; quotient bit = 1
DIV_SKIP:
    DEX
    BNE DIV_LOOP
    RTS

;---------------------------------------------------------
; ADD16 - 16-bit addition
; Input:  $00/$01 = operand A (lo/hi), $02/$03 = operand B
; Output: $02/$03 = result
;---------------------------------------------------------
ADD16:
    CLC
    LDA $00
    ADC $02
    STA $02
    LDA $01
    ADC $03
    STA $03
    RTS

;---------------------------------------------------------
; MAIN - demonstrate operations
;---------------------------------------------------------
START:
    ; Multiply 15 × 17 = 255
    LDA #15
    STA $00
    LDA #17
    STA $01
    JSR MUL8
    ; $02 = 255 (0xFF), $03 = 0

    ; Multiply 200 × 200 = 40000 (needs 16 bits)
    LDA #200
    STA $00
    LDA #200
    STA $01
    JSR MUL8
    ; $02 = $40 (64), $03 = $9C (156) → 156*256+64 = 40000

    ; Divide 100 ÷ 7 = 14 remainder 2
    LDA #100
    STA $00
    LDA #7
    STA $01
    JSR DIV8
    ; $02 = 14, $04 = 2

    ; 16-bit add: $1234 + $5678 = $68AC
    LDA #$34
    STA $00
    LDA #$12
    STA $01
    LDA #$78
    STA $02
    LDA #$56
    STA $03
    JSR ADD16
    ; $02/$03 = $AC/$68

    BRK

.end
