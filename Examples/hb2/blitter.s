; blitter.s -- the picture the processor never moves.
;
; 320 x 200 with one byte per pixel, and every shape drawn by the blitter: a filled
; frame, a fan of lines, and an XOR bar that undoes itself. The processor issues the
; stores and leaves the pixels to the chip, which is what a 1992 machine spent its
; money on.
;
;   ./native/build/time_warp_gates hb Examples/hb2/blitter.s --ticks 4 --screen

        ORG $1000

        MOV R0, #1              ; MODE 1: 320 x 200, a byte a pixel
        INT 5

        MOV R0, #0
        INT 7                   ; clear to black

; The frame, in cyan.
        MOV R0, #0              ; x
        MOV R1, #10             ; y
        MOV R2, #300            ; w
        MOV R3, #2              ; h
        MOV R4, #11
        INT 3
        MOV R0, #0
        MOV R1, #188
        MOV R2, #300
        MOV R3, #2
        MOV R4, #11
        INT 3
        MOV R0, #0
        MOV R1, #10
        MOV R2, #2
        MOV R3, #180
        MOV R4, #11
        INT 3
        MOV R0, #298
        MOV R1, #10
        MOV R2, #2
        MOV R3, #180
        MOV R4, #11
        INT 3

; A fan: twelve lines from the middle of the frame to its top edge.
        MOV R6, #0              ; R6 = the step along the top edge
fan:    MOV R0, #160            ; from the centre...
        MOV R1, #100
        MOV R2, R6              ; ...to a point on the top edge
        MOV R3, #20
        MOV R4, #13             ; magenta
        INT 4                   ; line
        ADD R6, #25
        CMP R6, #300
        JLT fan

; A bar, then the same bar XORed: the second pass erases the first exactly, which is
; how a cursor was drawn on a machine with no spare memory.
        MOV R0, #120
        MOV R1, #90
        MOV R2, #80
        MOV R3, #20
        MOV R4, #7
        INT 3
        MOV R0, #120
        MOV R1, #90
        MOV R2, #80
        MOV R3, #20
        MOV R4, #7
        INT 3

; And a bar that stays, in bright yellow, so the screen has ink to show for it.
        MOV R0, #120
        MOV R1, #90
        MOV R2, #80
        MOV R3, #20
        MOV R4, #14
        INT 3

        MOV R0, #0
        INT 0
