; 6502 Assembly - Sorting Algorithm
; Bubble sort of an array in zero page memory
; Simulated execution with register trace

; Memory layout
; $00 = array base (8 bytes)
; $08 = array length
; $09 = outer loop counter (i)
; $0A = inner loop counter (j)
; $0B = temp swap register
; $0C = swap flag (any swaps made this pass)

; Initialize array with unsorted values
; [64, 25, 12, 90, 3, 77, 44, 18]

.org $0200          ; program starts at $0200

START:
    ; Store array values in zero page
    LDA #64
    STA $00
    LDA #25
    STA $01
    LDA #12
    STA $02
    LDA #90
    STA $03
    LDA #3
    STA $04
    LDA #77
    STA $05
    LDA #44
    STA $06
    LDA #18
    STA $07
    LDA #8
    STA $08         ; array length = 8

    ; Print "Bubble Sort Demo"
    ; (in simulator: output via $FFFE)

OUTER:
    LDA #0
    STA $09         ; i = 0
    LDA $08
    SEC
    SBC #1
    STA $0D         ; limit = n-1

OUTER_LOOP:
    LDA $09
    CMP $0D
    BCS DONE        ; if i >= limit, done

    ; Inner loop: compare adjacent pairs
    LDA #0
    STA $0A         ; j = 0

INNER_LOOP:
    LDA $0D
    LDA $09
    TAX             ; X = i
    LDA $0D
    SEC
    SBC $09         ; limit - i
    STA $0E         ; inner limit

    LDA $0A
    CMP $0E
    BCS NEXT_OUTER  ; if j >= inner_limit, next outer pass

    ; Load arr[j] and arr[j+1]
    LDX $0A
    LDA $00,X       ; A = arr[j]
    INX
    CMP $00,X       ; compare arr[j] with arr[j+1]
    BCC NO_SWAP     ; if arr[j] <= arr[j+1], no swap

    ; Swap arr[j] and arr[j+1]
    STA $0B         ; temp = arr[j]
    LDA $00,X       ; A = arr[j+1]
    DEX
    STA $00,X       ; arr[j] = arr[j+1]
    INX
    LDA $0B
    STA $00,X       ; arr[j+1] = temp

NO_SWAP:
    INC $0A         ; j++
    JMP INNER_LOOP

NEXT_OUTER:
    INC $09         ; i++
    JMP OUTER_LOOP

DONE:
    BRK             ; halt / breakpoint

.end
