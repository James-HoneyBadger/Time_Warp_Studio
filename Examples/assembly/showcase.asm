; ═══════════════════════════════════════════════════════════════
;  x86-64 ASSEMBLY LANGUAGE SHOWCASE (Virtual CPU simulator)
;  Demonstrates: registers, arithmetic, logic, loops, branches.
; ═══════════════════════════════════════════════════════════════

section .data
    banner   db "═══════════════════════════════════════", 10
             db "  x86-64 ASSEMBLY LANGUAGE SHOWCASE",    10
             db "═══════════════════════════════════════", 10, 0
    s_arith  db 10, "  1. ARITHMETIC OPERATIONS", 10, 0
    s_bit    db 10, "  2. BITWISE OPERATIONS", 10, 0
    s_loop   db 10, "  3. LOOP & ACCUMULATION", 10, 0
    s_fib    db 10, "  4. FIBONACCI SEQUENCE", 10, 0
    s_fact   db 10, "  5. FACTORIAL", 10, 0
    s_eq     db " = ", 0
    s_nl     db 10, 0
    s_42p58  db "  42 + 58 = ", 0
    s_7x13   db "  7 x 13  = ", 0
    s_100d7  db "  100 / 7 = ", 0
    s_sum    db "  Sum 1..100 = ", 0
    s_fib0   db "  fib(10)  = ", 0
    s_10fac  db "  10! = ", 0
    s_and    db "  0xB5 AND 0xCC = ", 0
    s_shl    db "  1 SHL 8 = ", 0

section .text
    global _start

_start:
    ; ── Banner ──────────────────────────────────────────────────
    PRINTS banner

    ; ── SECTION 1: Arithmetic ───────────────────────────────────
    PRINTS s_arith

    ; Addition: 42 + 58 = 100
    PRINTS s_42p58
    mov  rax, 42
    mov  rbx, 58
    add  rax, rbx
    PRINTR rax
    PRINTS s_nl

    ; Multiplication: 7 * 13 = 91
    PRINTS s_7x13
    mov  rax, 7
    mov  rbx, 13
    imul rax, rbx
    PRINTR rax
    PRINTS s_nl

    ; Division: 100 / 7 = 14
    PRINTS s_100d7
    mov  rax, 100
    mov  rbx, 7
    div  rbx
    PRINTR rax
    PRINTS s_nl

    ; ── SECTION 2: Bitwise Operations ───────────────────────────
    PRINTS s_bit

    PRINTS s_and
    mov  rax, 0b10110101
    mov  rbx, 0b11001100
    and  rax, rbx
    PRINTR rax
    PRINTS s_nl

    PRINTS s_shl
    mov  rax, 1
    shl  rax, 8
    PRINTR rax
    PRINTS s_nl

    ; ── SECTION 3: Loop & Summation ─────────────────────────────
    PRINTS s_loop

    ; Sum 1..100 = 5050
    PRINTS s_sum
    xor  rax, rax
    mov  rcx, 100
.sum_loop:
    add  rax, rcx
    dec  rcx
    jnz  .sum_loop
    PRINTR rax
    PRINTS s_nl

    ; ── SECTION 4: Fibonacci ────────────────────────────────────
    PRINTS s_fib

    PRINTS s_fib0
    xor  rax, rax
    mov  rbx, 1
    mov  rcx, 9
.fib_loop:
    mov  rdx, rax
    add  rdx, rbx
    mov  rax, rbx
    mov  rbx, rdx
    dec  rcx
    jnz  .fib_loop
    PRINTR rax
    PRINTS s_nl

    ; ── SECTION 5: Factorial ────────────────────────────────────
    PRINTS s_fact

    PRINTS s_10fac
    mov  rax, 1
    mov  rcx, 10
.fact_loop:
    imul rax, rcx
    dec  rcx
    jnz  .fact_loop
    PRINTR rax
    PRINTS s_nl

    ; ── Exit ────────────────────────────────────────────────────
    mov  rax, 60
    xor  rdi, rdi
    syscall
