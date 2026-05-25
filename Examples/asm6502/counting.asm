; Counting and Loops in 6502 Assembly
; Demonstrates INX, BNE, CPX, and basic loop patterns

        .ORG $0200

; --- Count from 1 to 10, printing each number ---
START:
        LDX #<COUNT_MSG
        LDY #>COUNT_MSG
        JSR PRINTSTR
        JSR NEWLINE

        LDA #1                  ; Start at 1
LOOP1:
        JSR PRINTDEC            ; Print A as decimal
        JSR NEWLINE
        CLC
        ADC #1                  ; A = A + 1
        CMP #11                 ; Compare with 11 (stop after 10)
        BNE LOOP1               ; Loop if not equal

; --- Count DOWN from 10 to 1 ---
        LDX #<DOWN_MSG
        LDY #>DOWN_MSG
        JSR PRINTSTR
        JSR NEWLINE

        LDA #10                 ; Start at 10
LOOP2:
        JSR PRINTDEC
        JSR NEWLINE
        SEC
        SBC #1                  ; A = A - 1
        BNE LOOP2               ; Loop while A != 0

; --- Even numbers 0-20 using X register ---
        LDX #<EVEN_MSG
        LDY #>EVEN_MSG
        JSR PRINTSTR
        JSR NEWLINE

        LDX #0                  ; X = 0
LOOP3:
        TXA                     ; A = X (copy X to A for printing)
        JSR PRINTDEC
        LDA #' '
        JSR CHROUT
        INX
        INX                     ; X += 2 (skip odd numbers)
        CPX #22                 ; Stop at 22 (prints 0..20)
        BNE LOOP3
        JSR NEWLINE

        BRK

; Strings
COUNT_MSG:  .ASCIIZ "Count 1 to 10:"
DOWN_MSG:   .ASCIIZ "Count down 10 to 1:"
EVEN_MSG:   .ASCIIZ "Even numbers 0-20:"
