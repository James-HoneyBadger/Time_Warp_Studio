; tape.s -- the cassette: record a file, rewind the head, play it back.
;
; A disk is a tree you can see and a cassette is a position. This one records five
; bytes, rewinds the head, plays them back into a buffer and prints them, so the
; screen is the proof that the round trip happened -- and the same four services,
; 20 to 23, are what the prompt's CSAVE, CLOAD and REWIND call.
;
;   ./native/build/time_warp_gates hb Examples/hb2/tape.s --ticks 2

        MOV R0, #0
        INT 5                   ; MODE 0: the 1982 text map, where printing is cells
        MOV R0, #7
        INT 7                   ; clear it

        LEA R0, name            ; the name on the cassette's label
        LEA R1, message         ; what goes on it
        MOV R2, #5              ; five bytes
        INT 20                  ; record; R0 comes back 1 when it was written

        INT 22                  ; rewind: the head goes back to the start
        LEA R0, name            ; the name again: recording left its result in R0
        LEA R1, buffer
        INT 21                  ; play it back; R0 comes back as the length

        LEA R5, buffer          ; and print what came back
loop:   LOADB R1, [R5]
        CMP R1, #0
        JZ done
        MOV R0, R1
        INT 1
        INC R5
        JMP loop

done:   MOV R0, #10             ; a newline, so the line is a line
        INT 1
        MOV R0, #0
        INT 0                   ; exit

name:   DB "GREETING", 0
message: DB "HELLO"
buffer: DS 8                    ; the zeros after the five bytes terminate the text
