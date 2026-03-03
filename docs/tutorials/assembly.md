# Assembly Language Tutorial

Time Warp Studio supports x86-64 Linux assembly (AT&T and Intel syntax). Assembly gives you direct control over the CPU — no abstractions.

## Hello World (Linux syscall)

```asm
; hello.asm — x86-64 Linux, Intel syntax (NASM)
section .data
    msg db "Hello from Assembly!", 10   ; 10 = newline
    len equ $ - msg

section .text
    global _start

_start:
    ; sys_write(1, msg, len)
    mov  rax, 1          ; syscall: write
    mov  rdi, 1          ; fd: stdout
    mov  rsi, msg        ; buffer
    mov  rdx, len        ; count
    syscall

    ; sys_exit(0)
    mov  rax, 60         ; syscall: exit
    xor  rdi, rdi        ; exit code 0
    syscall
```

## Registers (x86-64)

| Register | Size | Purpose |
|----------|------|---------|
| `rax` | 64-bit | Accumulator / syscall number |
| `rbx` | 64-bit | Base / callee-saved |
| `rcx` | 64-bit | Counter |
| `rdx` | 64-bit | Data |
| `rsi` | 64-bit | Source index / arg 2 |
| `rdi` | 64-bit | Dest index / arg 1 |
| `rsp` | 64-bit | Stack pointer |
| `rbp` | 64-bit | Base pointer |
| `r8–r15` | 64-bit | General purpose |
| `eax` | 32-bit | Lower 32 bits of rax |

## Basic Instructions

```asm
; Data movement
mov  rax, 42        ; rax = 42
mov  rbx, rax       ; rbx = rax
xor  rcx, rcx       ; rcx = 0 (fast zero)

; Arithmetic
add  rax, 10        ; rax += 10
sub  rbx, 5         ; rbx -= 5
imul rax, 3         ; rax *= 3
idiv rbx            ; rax /= rbx (rdx:rax / rbx)
inc  rcx            ; rcx++
dec  rdx            ; rdx--
neg  rax            ; rax = -rax

; Logic
and  rax, 0xFF      ; bitwise AND
or   rax, 0x01      ; bitwise OR
xor  rax, rax       ; rax = 0
not  rax            ; bitwise NOT
shl  rax, 2         ; left shift by 2 (multiply by 4)
shr  rax, 1         ; right shift by 1 (divide by 2)
```

## Labels and Jumps

```asm
; Conditional control flow
    mov  rax, 10
    mov  rbx, 5
    cmp  rax, rbx       ; set flags: rax - rbx
    jg   greater        ; jump if rax > rbx
    jl   less
    je   equal

greater:
    ; rax > rbx
    jmp  done
less:
    ; rax < rbx
    jmp  done
equal:
    ; rax == rbx
done:
```

## Loops

```asm
; Count from 1 to 10 using rcx
section .text
    global _start

_start:
    mov  rcx, 1         ; i = 1

loop_start:
    cmp  rcx, 10
    jg   loop_end       ; if i > 10, exit

    ; --- body: print digit character ---
    push rcx
    ; compute ASCII digit
    mov  rax, rcx
    add  rax, 48        ; '0' = 48
    ; ... write syscall here ...
    pop  rcx
    inc  rcx            ; i++
    jmp  loop_start

loop_end:
    mov  rax, 60
    xor  rdi, rdi
    syscall
```

## The Stack

```asm
; Function call / local storage
sub  rsp, 16        ; allocate 16 bytes on stack
mov  qword [rsp], 42   ; store value

push rax            ; push onto stack (rsp -= 8, [rsp] = rax)
pop  rbx            ; pop  from stack (rbx = [rsp], rsp += 8)

; Typical function prologue/epilogue
push rbp
mov  rbp, rsp
sub  rsp, 32        ; local variables

; ... function body ...

mov  rsp, rbp
pop  rbp
ret
```

## Linux Syscall Numbers (x86-64)

| Number | Name | Args |
|--------|------|------|
| 0 | `read` | fd, buf, count |
| 1 | `write` | fd, buf, count |
| 60 | `exit` | code |

## Further Reading

- [Examples/assembly/](../Examples/assembly/) — 10 Assembly example programs
- [Language Guide: Assembly](LANGUAGE_GUIDE.md#assembly)
