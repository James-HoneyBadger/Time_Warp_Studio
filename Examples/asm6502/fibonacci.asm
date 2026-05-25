; Fibonacci Sequence in 6502 Assembly
; Computes first 12 Fibonacci numbers using zero-page memory
; Demonstrates memory, multi-byte arithmetic, and subroutines

        .ORG $0200

; Zero-page variables
PREV    = $10           ; Previous Fibonacci number (8-bit)
CURR    = $11           ; Current Fibonacci number  (8-bit)
TEMP    = $12           ; Temporary storage
COUNT   = $13           ; Loop counter

START:
        LDX #<TITLE
        LDY #>TITLE
        JSR PRINTSTR
        JSR NEWLINE

        ; Initialise: fib(0)=0, fib(1)=1
        LDA #0
        STA PREV
        STA COUNT

        LDA #1
        STA CURR

        ; Print fib(0) = 0
        LDA PREV
        JSR PRINTDEC
        JSR NEWLINE

FIBLOOP:
        ; Print current value
        LDA CURR
        JSR PRINTDEC
        JSR NEWLINE

        ; Compute next = PREV + CURR
        LDA PREV
        CLC
        ADC CURR
        STA TEMP        ; TEMP = PREV + CURR

        ; Shift: PREV = CURR, CURR = TEMP
        LDA CURR
        STA PREV
        LDA TEMP
        STA CURR

        ; Increment counter (we've printed 1 seed + COUNT loops)
        INC COUNT
        LDA COUNT
        CMP #11         ; Print 12 numbers total (fib(0)..fib(11))
        BNE FIBLOOP

        LDA #'-'
        JSR CHROUT
        JSR CHROUT
        JSR CHROUT
        JSR CHROUT
        JSR CHROUT
        JSR CHROUT
        JSR NEWLINE

        LDX #<FOOTER
        LDY #>FOOTER
        JSR PRINTSTR
        JSR NEWLINE

        BRK

TITLE:  .ASCIIZ "Fibonacci Sequence:"
FOOTER: .ASCIIZ "End of sequence."
