; Hello World in 6502 Assembly
; Demonstrates character output and basic I/O

        .ORG $0200              ; Assemble code starting at $0200

START:
        ; Print the string "Hello, World!" using PRINTSTR
        LDX #<MSG               ; Low byte of message address
        LDY #>MSG               ; High byte of message address
        JSR PRINTSTR            ; Print zero-terminated string

        ; Print a newline
        JSR NEWLINE

        ; Print individual characters to spell "6502"
        LDA #'6'
        JSR CHROUT
        LDA #'5'
        JSR CHROUT
        LDA #'0'
        JSR CHROUT
        LDA #'2'
        JSR CHROUT
        JSR NEWLINE

        BRK                     ; End program

; Data
MSG:    .ASCIIZ "Hello, World!"  ; Zero-terminated string
