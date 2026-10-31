; sprite.s -- hardware sprites, and the collision register.
;
; One 16 x 16 diamond, copied into the chip's pattern bank with the service that does
; exactly that, then two sprites placed on the screen. A game asks the collision
; register whether they met; reading it clears it, so the question is asked once.
;
;   ./native/build/time_warp_gates hb Examples/hb2/sprite.s --ticks 4 --screen

        ORG $1000

        MOV R0, #1              ; MODE 1: 320 x 200
        INT 5
        MOV R0, #0
        INT 7                   ; clear to black

        MOV R0, #0              ; pattern 0
        LEA R1, diamond         ; ...from this mask in RAM
        INT 13

        MOV R0, #0              ; sprite 0
        MOV R1, #40             ; x
        MOV R2, #100            ; y
        MOV R3, #0              ; pattern 0
        MOV R4, #12             ; cyan
        INT 9

        MOV R0, #1              ; sprite 1, further along the same line
        MOV R1, #120
        MOV R2, #100
        MOV R3, #0
        MOV R4, #13             ; magenta
        INT 9

; A ground line, so the picture has a floor.
        MOV R0, #0
        MOV R1, #140
        MOV R2, #319
        MOV R3, #140
        MOV R4, #10
        INT 4

        MOV R0, #0
        INT 0

; The mask: sixteen rows of sixteen bits, two bytes a row. A diamond is what a
; sixteen-pixel sprite can be, and it is symmetrical so a screen dump reads it.
diamond:
        DB $01, $80
        DB $03, $C0
        DB $07, $E0
        DB $0F, $F0
        DB $1F, $F8
        DB $1F, $F8
        DB $1F, $F8
        DB $1F, $F8
        DB $1F, $F8
        DB $1F, $F8
        DB $1F, $F8
        DB $0F, $F0
        DB $07, $E0
        DB $03, $C0
        DB $01, $80
        DB $00, $00
