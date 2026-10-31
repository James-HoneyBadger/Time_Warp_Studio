; chord.s -- four channels, and a waveform the program writes itself.
;
; The 1982 machine had one latch and five noises. This one has four channels of
; sampled audio, each with a waveform, a period, a volume and an envelope, and the
; waveform bank is memory: this program rewrites slot 4 so the note it plays is the
; instrument it made rather than the one the machine shipped.
;
; It draws a meter while it plays, one bar a channel, because a machine that only
; makes a noise and shows nothing is the kind of demo this project treats as broken.
;
;   ./native/build/time_warp_gates hb Examples/hb2/chord.s --ticks 4 --screen

        ORG $1000

        MOV R0, #1
        INT 5                   ; MODE 1
        MOV R0, #0
        INT 7                   ; clear

; Rewrite waveform slot 4: a staircase, thirty-two samples, through the audio page.
        MOV R6, #0              ; R6 = index into the slot
stairs: MOV R0, #4              ; slot 4
        MOV R1, R6              ; ...this sample
        MOV R2, R6
        MUL R2, #8              ; a staircase in eighths
        INT 15                  ; write it into the bank
        ADD R6, #1
        CMP R6, #32
        JLT stairs

; A chord: four channels, four waveforms, four periods.
        MOV R0, #0
        MOV R1, #0              ; square
        MOV R2, #4
        MOV R3, #192
        INT 14
        MOV R0, #1
        MOV R1, #1              ; sine
        MOV R2, #6
        MOV R3, #160
        INT 14
        MOV R0, #2
        MOV R1, #2              ; saw
        MOV R2, #8
        MOV R3, #128
        INT 14
        MOV R0, #3
        MOV R1, #4              ; the staircase this program wrote
        MOV R2, #16
        MOV R3, #224
        INT 14

; The meter: one bar a channel, taller for a louder one.
        MOV R0, #20
        MOV R1, #180
        MOV R2, #40
        MOV R3, #20
        MOV R4, #12
        INT 3
        MOV R0, #80
        MOV R1, #160
        MOV R2, #40
        MOV R3, #40
        MOV R4, #10
        INT 3
        MOV R0, #140
        MOV R1, #140
        MOV R2, #40
        MOV R3, #60
        MOV R4, #13
        INT 3
        MOV R0, #200
        MOV R1, #110
        MOV R2, #40
        MOV R3, #90
        MOV R4, #14
        INT 3

; A frame around the meter.
        MOV R0, #18
        MOV R1, #108
        MOV R2, #224
        MOV R3, #108
        MOV R4, #15
        INT 4
        MOV R0, #18
        MOV R1, #202
        MOV R2, #224
        MOV R3, #202
        MOV R4, #15
        INT 4

        MOV R0, #0
        INT 0
