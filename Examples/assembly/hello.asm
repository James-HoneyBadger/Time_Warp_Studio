; HELLO WORLD — Assembly Language Demo
; Welcome to Assembly programming!
; Time Warp Studio — x86-style Toy Assembly Demo

section .data
    msg db "HELLO WORLD", 0
    welcome db "Welcome to Assembly Language", 0

section .text

_start:
    ; Print header
    PRINT "HELLO WORLD"
    PRINT "Welcome to Assembly"
    ; Demonstrate basic arithmetic operations
    mov eax, 10
    mov ebx, 5
    add eax, ebx        ; eax = 15
    PRINT "ARITHMETIC DEMO"
    PRINT eax

    mov eax, 20
    sub eax, 8          ; eax = 12
    PRINT eax

    mov eax, 4
    mul eax, 3          ; eax = 12
    PRINT eax

    ; ===== CONDITIONALS =====
    ; Branch based on comparison
    PRINT "CONDITIONALS DEMO"
    mov eax, 42
    mov ebx, 42
    cmp eax, ebx
    je  equal_label
    jmp not_equal
equal_label:
    PRINT "Values are equal"
    jmp done_cond
not_equal:
    PRINT "Values are not equal"
done_cond:

    mov eax, 100
    mov ebx, 50
    cmp eax, ebx
    jg  greater_label
    jmp not_greater
greater_label:
    PRINT "First value is greater"
    jmp done_cmp
not_greater:
    PRINT "Second value is not less"
done_cmp:

    ; ===== SUBROUTINES =====
    PRINT "SUBROUTINES DEMO"
    call greet_sub
    jmp end_program

greet_sub:
    PRINT "Hello from subroutine"
    ret

end_program:
    PRINT "DONE — Assembly demo complete"
    halt
