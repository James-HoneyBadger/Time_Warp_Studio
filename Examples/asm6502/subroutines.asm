; Subroutines and Stack in 6502 Assembly
; Demonstrates JSR, RTS, PHA, PLA, and nested subroutine calls

        .ORG $0200

; Zero page
COUNTER = $10
RESULT  = $11

START:
        LDX #<HDR
        LDY #>HDR
        JSR PRINTSTR
        JSR NEWLINE

        ; Call GREET subroutine
        JSR GREET

        ; Call PRINT_STARS subroutine with count in A
        LDA #10
        JSR PRINT_STARS
        JSR NEWLINE

        ; Compute sum 1+2+...+5 using ACCUMULATE
        LDA #0
        STA RESULT
        LDX #1          ; Start from 1
        STX COUNTER
SUM_LOOP:
        LDA COUNTER
        CLC
        ADC RESULT
        STA RESULT
        INC COUNTER
        LDA COUNTER
        CMP #6
        BNE SUM_LOOP

        LDX #<SUM_MSG
        LDY #>SUM_MSG
        JSR PRINTSTR
        LDA RESULT
        JSR PRINTDEC
        JSR NEWLINE

        ; Demonstrate stack: push 3 values, pop in reverse
        LDX #<STACK_MSG
        LDY #>STACK_MSG
        JSR PRINTSTR
        JSR NEWLINE

        LDA #10
        PHA             ; Push 10
        LDA #20
        PHA             ; Push 20
        LDA #30
        PHA             ; Push 30

        ; Now pop in LIFO order: should be 30, 20, 10
        PLA
        JSR PRINTDEC
        JSR NEWLINE
        PLA
        JSR PRINTDEC
        JSR NEWLINE
        PLA
        JSR PRINTDEC
        JSR NEWLINE

        ; Nested calls: OUTER -> INNER -> DEEPEST
        JSR OUTER

        BRK

; --- Subroutines ---

GREET:
        LDX #<GREET_MSG
        LDY #>GREET_MSG
        JSR PRINTSTR
        JSR NEWLINE
        RTS

; Print A asterisks
PRINT_STARS:
        TAX             ; X = count
STAR_LOOP:
        CPX #0
        BEQ STAR_DONE
        LDA #'*'
        JSR CHROUT
        DEX
        JMP STAR_LOOP
STAR_DONE:
        RTS

; Nested calls
OUTER:
        LDX #<OUTER_MSG
        LDY #>OUTER_MSG
        JSR PRINTSTR
        JSR NEWLINE
        JSR INNER
        RTS

INNER:
        LDX #<INNER_MSG
        LDY #>INNER_MSG
        JSR PRINTSTR
        JSR NEWLINE
        JSR DEEPEST
        RTS

DEEPEST:
        LDX #<DEEP_MSG
        LDY #>DEEP_MSG
        JSR PRINTSTR
        JSR NEWLINE
        RTS

; Data
HDR:        .ASCIIZ "=== Subroutines ==="
GREET_MSG:  .ASCIIZ "Hello from GREET subroutine!"
SUM_MSG:    .ASCIIZ "Sum 1+2+3+4+5 = "
STACK_MSG:  .ASCIIZ "Stack (LIFO order, expect 30,20,10):"
OUTER_MSG:  .ASCIIZ "Outer called"
INNER_MSG:  .ASCIIZ "  Inner called"
DEEP_MSG:   .ASCIIZ "    Deepest called"
