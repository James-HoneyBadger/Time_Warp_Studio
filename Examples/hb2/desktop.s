; desktop.s -- HB-OS, without the scheduler: what the machine's screen is for.
;
; A wallpaper, two overlapping windows with title bars and shadows, and a task bar
; along the bottom. The windows overlap, which is the whole point of a framebuffer:
; the second one is composed over the first, and the first is still there behind it.
;
; The round-robin scheduler that would put a process in each window is a later
; proposal; this is the picture it will have to paint.
;
;   ./native/build/time_warp_gates hb Examples/hb2/desktop.s --ticks 4 --screen

        ORG $1000

        MOV R0, #1              ; MODE 1: 320 x 200
        INT 5

; Wallpaper: a dark teal, so the windows have something to sit on.
        MOV R0, #0
        MOV R1, #0
        MOV R2, #320
        MOV R3, #200
        MOV R4, #3
        INT 3

; Window one: shadow, body, title bar, and a line of "text" as a band.
        MOV R0, #22
        MOV R1, #22
        MOV R2, #150
        MOV R3, #100
        MOV R4, #0
        INT 3                   ; shadow
        MOV R0, #18
        MOV R1, #18
        MOV R2, #150
        MOV R3, #100
        MOV R4, #7
        INT 3                   ; body
        MOV R0, #18
        MOV R1, #18
        MOV R2, #150
        MOV R3, #12
        MOV R4, #9
        INT 3                   ; title bar

; Window two, overlapping the first: this is what a framebuffer buys.
        MOV R0, #152
        MOV R1, #78
        MOV R2, #150
        MOV R3, #90
        MOV R4, #0
        INT 3
        MOV R0, #148
        MOV R1, #74
        MOV R2, #150
        MOV R3, #90
        MOV R4, #7
        INT 3
        MOV R0, #148
        MOV R1, #74
        MOV R2, #150
        MOV R3, #12
        MOV R4, #13
        INT 3                   ; the focused window's title bar

; A pointer, drawn as two lines: a 1992 machine's arrow, and proof that a pixel is
; just a byte.
        MOV R0, #100
        MOV R1, #120
        MOV R2, #100
        MOV R3, #136
        MOV R4, #15
        INT 4
        MOV R0, #100
        MOV R1, #136
        MOV R2, #106
        MOV R3, #130
        MOV R4, #15
        INT 4
        MOV R0, #100
        MOV R1, #120
        MOV R2, #106
        MOV R3, #126
        MOV R4, #15
        INT 4

; The task bar, and one button in it.
        MOV R0, #0
        MOV R1, #186
        MOV R2, #320
        MOV R3, #14
        MOV R4, #8
        INT 3
        MOV R0, #4
        MOV R1, #188
        MOV R2, #40
        MOV R3, #10
        MOV R4, #7
        INT 3

        MOV R0, #0
        INT 0
