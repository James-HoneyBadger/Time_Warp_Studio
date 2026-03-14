; String Operations — Assembly (8-bit educational)
; Demonstrates: data movement, comparisons, loops, subroutines
;
; ══════════════════════════════════════
;   📝 String Processing Toolkit
; ══════════════════════════════════════

; --- Data Section ---
section .data
    title:   DB "String Operations Demo", 0
    str1:    DB "Hello, World!", 0
    str2:    DB "HELLO, WORLD!", 0
    str3:    DB "Assembly", 0
    buffer:  DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    newline: DB 10, 0

section .text

; --- Entry Point ---
_start:
    ; Print title
    MOV A, title
    CALL print_string
    CALL print_nl

    ; --- String Length ---
    MOV A, str1
    CALL strlen
    ; B now has length
    CALL print_string   ; "Hello, World!"
    CALL print_nl

    ; --- String Copy ---
    MOV A, str3          ; source = "Assembly"
    MOV B, buffer        ; dest = buffer
    CALL strcpy
    MOV A, buffer
    CALL print_string    ; Should print "Assembly"
    CALL print_nl

    ; --- String Reverse ---
    MOV A, str3
    CALL str_reverse     ; Reverse "Assembly" in buffer
    MOV A, buffer
    CALL print_string    ; Should print "ylbmessA"
    CALL print_nl

    ; --- Character Count ---
    MOV A, str1          ; Count 'l' in "Hello, World!"
    MOV B, 108           ; ASCII 'l'
    CALL char_count
    ; Result in C register
    ADD C, 48            ; Convert to ASCII digit
    OUT C                ; Print count
    CALL print_nl

    ; --- To Uppercase ---
    MOV A, str1
    MOV B, buffer
    CALL to_upper
    MOV A, buffer
    CALL print_string    ; "HELLO, WORLD!"
    CALL print_nl

    ; --- String Compare ---
    MOV A, buffer        ; "HELLO, WORLD!" (from to_upper)
    MOV B, str2          ; "HELLO, WORLD!"
    CALL strcmp
    ; Zero flag set if equal
    CMP A, 0
    JE strings_equal
    MOV A, 78            ; 'N'
    OUT A
    JMP done_compare
strings_equal:
    MOV A, 89            ; 'Y' — strings match
    OUT A
done_compare:
    CALL print_nl

    ; Done
    HLT

; === Subroutines ===

; Print null-terminated string at address A
print_string:
    PUSH A
.loop:
    MOV B, [A]
    CMP B, 0
    JE .done
    OUT B
    INC A
    JMP .loop
.done:
    POP A
    RET

; Print newline
print_nl:
    MOV A, 10
    OUT A
    RET

; String length: A = address, result in B
strlen:
    PUSH A
    MOV B, 0
.loop:
    MOV C, [A]
    CMP C, 0
    JE .done
    INC B
    INC A
    JMP .loop
.done:
    POP A
    RET

; String copy: A = src, B = dest
strcpy:
    PUSH A
    PUSH B
.loop:
    MOV C, [A]
    MOV [B], C
    CMP C, 0
    JE .done
    INC A
    INC B
    JMP .loop
.done:
    POP B
    POP A
    RET

; Convert to uppercase: A = src, B = dest
to_upper:
    PUSH A
    PUSH B
.loop:
    MOV C, [A]
    CMP C, 0
    JE .done
    CMP C, 97       ; 'a'
    JL .store
    CMP C, 122      ; 'z'
    JG .store
    SUB C, 32       ; Convert to uppercase
.store:
    MOV [B], C
    INC A
    INC B
    JMP .loop
.done:
    MOV C, 0
    MOV [B], C
    POP B
    POP A
    RET

; Count char B in string A, result in C
char_count:
    PUSH A
    MOV C, 0
.loop:
    MOV D, [A]
    CMP D, 0
    JE .done
    CMP D, B
    JNE .skip
    INC C
.skip:
    INC A
    JMP .loop
.done:
    POP A
    RET

; Reverse string A into buffer
str_reverse:
    PUSH A
    ; First get length
    CALL strlen        ; length in B
    ; B = length, copy reversed
    MOV C, 0           ; dest index
    ADD A, B
    DEC A              ; Point to last char
.loop:
    CMP B, 0
    JE .done
    MOV D, [A]
    MOV [buffer + C], D
    DEC A
    INC C
    DEC B
    JMP .loop
.done:
    MOV D, 0
    MOV [buffer + C], D
    POP A
    RET

; String compare: A = str1, B = str2, result 0=equal
strcmp:
    PUSH B
.loop:
    MOV C, [A]
    MOV D, [B]
    CMP C, D
    JNE .not_equal
    CMP C, 0
    JE .equal
    INC A
    INC B
    JMP .loop
.equal:
    MOV A, 0
    POP B
    RET
.not_equal:
    MOV A, 1
    POP B
    RET
