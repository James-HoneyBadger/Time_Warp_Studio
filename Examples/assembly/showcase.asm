; ═══════════════════════════════════════════════════════════════
;  x86-64 ASSEMBLY LANGUAGE SHOWCASE (NASM syntax, Linux/64-bit)
;  Demonstrates: syscalls, arithmetic, loops, string ops,
;                stack frames, register usage, bit operations.
; ═══════════════════════════════════════════════════════════════

section .data
    ; ── String constants ──────────────────────────────────────
    hdr         db "═══════════════════════════════════════", 10
                db "  x86-64 ASSEMBLY LANGUAGE SHOWCASE",    10
                db "═══════════════════════════════════════", 10, 0
    hdr_len     equ $ - hdr

    lbl_arith   db 10, "  1. ARITHMETIC OPERATIONS", 10, 0
    lbl_bitops  db 10, "  2. BITWISE OPERATIONS", 10, 0
    lbl_loop    db 10, "  3. LOOP & ACCUMULATION", 10, 0
    lbl_fib     db 10, "  4. FIBONACCI SEQUENCE", 10, 0
    lbl_fact    db 10, "  5. FACTORIAL", 10, 0
    lbl_str     db 10, "  6. STRING OPERATIONS", 10, 0

    eq_sign     db " = ", 0
    nl          db 10, 0
    indent      db "     ", 0
    hello_str   db "Hello, Assembly!", 0
    hello_len   equ $ - hello_str - 1

    ; Format strings for numeric output
    dec_prefix  db "  Value (decimal): ", 0
    hex_prefix  db "  Value (hex):     0x", 0
    bin_prefix  db "  Value (binary):  ", 0

section .bss
    ; Output buffer for number conversion
    num_buf     resb 32
    hex_buf     resb 20
    bin_buf     resb 72

section .text
    global _start

; ── SYSCALL HELPERS ─────────────────────────────────────────────
%macro sys_write 2          ; sys_write str, len
    mov  rax, 1             ; sys_write
    mov  rdi, 1             ; stdout
    mov  rsi, %1
    mov  rdx, %2
    syscall
%endmacro

%macro print_str 1          ; print null-terminated string
    push rbp
    mov  rbp, rsp
    mov  rsi, %1
    call strlen_fn
    sys_write %1, rax
    pop  rbp
%endmacro

%macro print_nl 0
    sys_write nl, 1
%endmacro

; ── ENTRY POINT ─────────────────────────────────────────────────
_start:
    ; Print header
    sys_write hdr, hdr_len

    ; ── SECTION 1: Arithmetic ───────────────────────────────────
    print_str lbl_arith

    ; Addition: rax = 42 + 58 = 100
    mov  rax, 42
    mov  rbx, 58
    add  rax, rbx           ; rax = 100
    print_str indent
    print_str dec_prefix
    call print_decimal
    print_nl

    ; Multiplication: 7 * 13 = 91
    mov  rax, 7
    mov  rbx, 13
    imul rax, rbx           ; rax = 91
    print_str indent
    mov  rcx, rax
    mov  rax, 7
    print_str dec_prefix
    call print_decimal
    print_nl

    ; Division: 100 / 7 = 14 remainder 2
    mov  rax, 100
    cqo                     ; sign-extend rax into rdx:rax
    mov  rbx, 7
    idiv rbx                ; rax=quotient, rdx=remainder
    ; rax now = 14
    print_str indent
    print_str dec_prefix
    call print_decimal
    print_nl

    ; ── SECTION 2: Bitwise Operations ───────────────────────────
    print_str lbl_bitops

    mov  rax, 0b10110101    ; 181
    print_str indent
    print_str dec_prefix
    call print_decimal
    print_nl
    print_str indent
    print_str hex_prefix
    call print_hex
    print_nl
    print_str indent
    print_str bin_prefix
    call print_binary
    print_nl

    ; AND, OR, XOR, NOT
    mov  rax, 0b10110101    ; 181
    mov  rbx, 0b11001100    ; 204
    and  rax, rbx           ; 0b10000100 = 132
    print_str indent
    print_str dec_prefix
    call print_decimal
    print_nl

    ; Shift operations
    mov  rax, 1
    shl  rax, 8             ; 1 << 8 = 256
    print_str indent
    print_str dec_prefix
    call print_decimal
    print_nl

    ; ── SECTION 3: Loop & Summation ─────────────────────────────
    print_str lbl_loop

    ; Sum 1..100 using loop
    xor  rax, rax           ; accumulator = 0
    mov  rcx, 100           ; counter = 100
.sum_loop:
    add  rax, rcx
    dec  rcx
    jnz  .sum_loop          ; loop while rcx != 0
    ; rax = 5050
    print_str indent
    print_str dec_prefix
    call print_decimal
    print_nl

    ; ── SECTION 4: Fibonacci ────────────────────────────────────
    print_str lbl_fib
    print_str indent

    xor  rax, rax           ; fib(0) = 0
    mov  rbx, 1             ; fib(1) = 1
    mov  rcx, 10            ; compute 10 terms

.fib_loop:
    ; Print current fib number
    push rax
    push rbx
    push rcx
    call print_decimal
    print_str indent       ; reuse as space between numbers
    pop  rcx
    pop  rbx
    pop  rax

    mov  rdx, rax
    add  rdx, rbx           ; next = a + b
    mov  rax, rbx           ; a = b
    mov  rbx, rdx           ; b = next
    dec  rcx
    jnz  .fib_loop
    print_nl

    ; ── SECTION 5: Factorial (iterative) ────────────────────────
    print_str lbl_fact
    print_str indent

    mov  rbx, 10            ; compute 10!
    mov  rax, 1             ; result = 1
    mov  rcx, rbx           ; loop counter
.fact_loop:
    imul rax, rcx
    dec  rcx
    jnz  .fact_loop
    ; rax = 3628800
    print_str dec_prefix
    call print_decimal
    print_nl

    ; ── SECTION 6: String Length ────────────────────────────────
    print_str lbl_str
    print_str "    String: '"
    sys_write hello_str, hello_len
    print_str "'"
    print_nl
    print_str "    Length: "
    mov  rsi, hello_str
    call strlen_fn
    call print_decimal
    print_nl

    ; ── EXIT ──────────────────────────────────────────────────
    mov  rax, 60            ; sys_exit
    xor  rdi, rdi           ; exit code 0
    syscall

; ────────────────────────────────────────────────────────────────
; print_decimal: print rax as decimal to stdout
; Clobbers: rax, rbx, rcx, rdx, rdi, rsi
; ────────────────────────────────────────────────────────────────
print_decimal:
    push rbp
    mov  rbp, rsp
    sub  rsp, 32

    lea  rdi, [num_buf + 31]
    mov  byte [rdi], 0       ; null terminator
    dec  rdi

    test rax, rax
    jnz  .pd_convert
    mov  byte [rdi], '0'
    lea  rsi, [rdi]
    mov  rdx, 1
    jmp  .pd_print

.pd_convert:
    mov  rbx, 10
.pd_loop:
    test rax, rax
    jz   .pd_done
    xor  rdx, rdx
    div  rbx
    add  dl, '0'
    mov  [rdi], dl
    dec  rdi
    jmp  .pd_loop
.pd_done:
    inc  rdi
    mov  rsi, rdi
    lea  rcx, [num_buf + 31]
    sub  rcx, rsi
    mov  rdx, rcx

.pd_print:
    mov  rax, 1
    mov  rdi_saved, rdi
    mov  rdi, 1
    syscall

    add  rsp, 32
    pop  rbp
    ret

; Simplified: real code would handle all register saves properly.
; This demo focuses on concept illustration.

; ────────────────────────────────────────────────────────────────
; strlen_fn: compute strlen of [rsi], return in rax
; ────────────────────────────────────────────────────────────────
strlen_fn:
    push rcx
    xor  rax, rax
.sl_loop:
    cmp  byte [rsi + rax], 0
    je   .sl_done
    inc  rax
    jmp  .sl_loop
.sl_done:
    pop  rcx
    ret

; ────────────────────────────────────────────────────────────────
; print_hex: print rax as hex
; ────────────────────────────────────────────────────────────────
print_hex:
    push rbx
    push rcx
    push rdx
    lea  rdi, [hex_buf]
    mov  rcx, 16
    mov  rbx, rax
.hex_loop:
    rol  rbx, 4
    mov  rdx, rbx
    and  rdx, 0Fh
    cmp  rdx, 9
    jle  .hex_digit
    add  rdx, 'A' - 10
    jmp  .hex_store
.hex_digit:
    add  rdx, '0'
.hex_store:
    mov  [rdi], dl
    inc  rdi
    dec  rcx
    jnz  .hex_loop
    mov  byte [rdi], 10
    sys_write hex_buf, 17
    pop  rdx
    pop  rcx
    pop  rbx
    ret

; ────────────────────────────────────────────────────────────────
; print_binary: print rax as 64-bit binary
; ────────────────────────────────────────────────────────────────
print_binary:
    push rbx
    push rcx
    lea  rdi, [bin_buf]
    mov  rcx, 8             ; print 8 bits (low byte)
    mov  rbx, rax
    shl  rbx, 56
.bin_loop:
    xor  rax, rax
    shl  rbx, 1
    adc  al, '0'
    mov  [rdi], al
    inc  rdi
    dec  rcx
    jnz  .bin_loop
    mov  byte [rdi], 10
    sys_write bin_buf, 9
    pop  rcx
    pop  rbx
    ret
