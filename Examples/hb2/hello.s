; hello.s -- the 1982 tube, on a 1992 machine.
;
; MODE 0 is the map the HB-1 has, byte for byte: text cells at $8000, attributes at
; $9000, the cursor at $A000. This program writes the cells directly, which is what a
; 1982 program did, and prints through the operating system's own call, which is what
; a 1992 one does. Both end up in the same memory, and the screen dump is the same
; screen.
;
;   ./native/build/time_warp_gates hb Examples/hb2/hello.s --ticks 2 --screen

        ORG $1000

        MOV R0, #0
        INT 5                   ; MODE 0: the 1982 text screen
        INT 10                  ; and a clear one

; A banner, written a cell at a time through the 1982 address.
        MOV R6, #0              ; R6 = the character index
        LEA R7, banner          ; R7 = where the text is
banner_loop:
        LOADB R0, [R7]
        CMP R0, #0
        JZ banner_done
        MOV R1, #$8000          ; the text page, as it was in 1982
        ADD R1, R6
        STOREB [R1], R0
        MOV R2, #$9000          ; and the attribute page
        ADD R2, R6
        MOV R3, #$1E            ; bright ink on black
        STOREB [R2], R3
        ADD R7, #1
        ADD R6, #1
        JMP banner_loop

; The operating system's print call, which writes at the cursor and scrolls.
banner_done:
        MOV R0, #10
        INT 1
        LEA R0, message
        INT 8                   ; print a zero-terminated string

        MOV R0, #0
        INT 0

banner:  DB "HB-2 Model 92", 0
message: DB "A 1992 machine, in the map a 1982 program knows.", 10, 0
