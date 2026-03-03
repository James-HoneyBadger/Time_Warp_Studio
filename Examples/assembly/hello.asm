; =============================================
;  Assembly Comprehensive Demo - Time Warp Studio
; =============================================

; --- Hello World ---
PRINT "===== HELLO WORLD ====="
PRINT "Welcome to Assembly!"
PRINT ""

; --- Registers and MOV ---
PRINT "===== REGISTERS ====="
MOV R0, 42
PRINT R0
MOV R1, 100
PRINT R1
PRINT ""

; --- Arithmetic ---
PRINT "===== ARITHMETIC ====="
MOV R0, 10
MOV R1, 3
ADD R0, R1
PRINT R0        ; 13

MOV R0, 10
MOV R1, 3
SUB R0, R1
PRINT R0        ; 7

MOV R0, 6
MOV R1, 7
MUL R0, R1
PRINT R0        ; 42

MOV R0, 20
MOV R1, 4
DIV R0, R1
PRINT R0        ; 5
PRINT ""

; --- Bit Operations ---
PRINT "===== BIT OPS ====="
MOV R0, 1
SHL R0, 3
PRINT R0        ; 8 (1 << 3)

MOV R0, 16
SHR R0, 2
PRINT R0        ; 4 (16 >> 2)

MOV R0, 12
MOV R1, 10
AND R0, R1
PRINT R0        ; 8 (1100 & 1010)

MOV R0, 12
MOV R1, 10
OR R0, R1
PRINT R0        ; 14 (1100 | 1010)

MOV R0, 12
MOV R1, 10
XOR R0, R1
PRINT R0        ; 6 (1100 ^ 1010)
PRINT ""

; --- Conditional Jumps ---
PRINT "===== CONDITIONALS ====="
MOV R0, 5
MOV R1, 5
CMP R0, R1
JE equal
PRINT "not equal"
JMP done_eq
equal:
PRINT "equal"
done_eq:

MOV R0, 10
MOV R1, 3
CMP R0, R1
JG greater
PRINT "not greater"
JMP done_gt
greater:
PRINT "greater"
done_gt:
PRINT ""

; --- Unconditional Jump ---
PRINT "===== JUMPS ====="
JMP skip
PRINT "this is skipped"
skip:
PRINT "reached after jump"
PRINT ""

; --- Stack Operations ---
PRINT "===== STACK ====="
MOV R0, 42
PUSH R0
MOV R0, 99
PUSH R0
POP R1
PRINT R1        ; 99
POP R1
PRINT R1        ; 42
PRINT ""

; --- Subroutines ---
PRINT "===== SUBROUTINES ====="
CALL my_sub
PRINT "back from subroutine"
JMP after_sub
my_sub:
PRINT "inside subroutine"
MOV R0, 99
RET
after_sub:
PRINT R0        ; 99
PRINT ""

; --- Memory ---
PRINT "===== MEMORY ====="
MOV R0, 777
STORE [100], R0
MOV R0, 0
LOAD R0, [100]
PRINT R0        ; 777
PRINT ""

PRINT "===== DONE ====="
HALT
