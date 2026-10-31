; task_a.s -- one of two processes on the HB-OS desktop.
;
; Runs for ever, printing a line and then wasting a moment, so the scheduler is seen
; to switch away from it. Its output appears in its own window, not on the console:
; a process prints into its window, and the desktop composes the windows.
;
;   ./native/build/time_warp_gates hb --tasks Examples/hb2/task_a.s Examples/hb2/task_b.s --ticks 6 --screen

again:  LEA R0, line            ; print the process's name
        INT 8
        MOV R2, #1200           ; a delay: the scheduler takes the processor here
wait:   DEC R2
        JNZ wait
        MOV R3, #$3000          ; a counter in RAM, so progress is a fact and not a
        LOAD R4, [R3]           ; log line
        INC R4
        STORE [R3], R4
        JMP again

line:   DB "TASK A: RUNNING", 10, 0
