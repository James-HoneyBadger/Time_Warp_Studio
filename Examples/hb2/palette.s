; palette.s -- the machine's first colour.
;
; Switches to 640 x 480 x 16 and paints sixteen bars, one per palette entry, then
; rewrites the DAC so the same picture is a different picture. Nothing here is a
; skin: every bar is a store, and the palette is memory the program owns.
;
;   ./native/build/time_warp_gates hb Examples/hb2/palette.s --ticks 4 --screen
;
; The last INT is the program saying it is done; the machine records the code.

        ORG $1000

        MOV R0, #2              ; MODE 2: 640 x 480, four bits per pixel
        INT 5

        MOV R7, #0              ; R7 = the bar's column
bars:   MOV R0, R7
        MOV R1, #0
        MOV R2, #40             ; 16 bars of 40 columns is 640
        MOV R3, #480
        MOV R4, R7              ; colour = the bar's number
        INT 3                   ; fill
        ADD R7, #40
        CMP R7, #640
        JLT bars

; A frame line in the bright entry, so the picture has an edge.
        MOV R0, #0
        MOV R1, #470
        MOV R2, #639
        MOV R3, #470
        MOV R4, #15
        INT 4                   ; line

; Re-programme the DAC: entry 1 becomes blue, entry 2 green, entry 3 red.
        MOV R0, #1
        MOV R1, #0
        MOV R2, #0
        MOV R3, #7
        INT 6
        MOV R0, #2
        MOV R1, #0
        MOV R2, #7
        MOV R3, #0
        INT 6
        MOV R0, #3
        MOV R1, #7
        MOV R2, #0
        MOV R3, #0
        INT 6

        MOV R0, #0
        INT 0                   ; exit 0
