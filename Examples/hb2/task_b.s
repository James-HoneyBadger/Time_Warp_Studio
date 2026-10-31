; task_b.s -- the other process.
;
; The same shape as task_a, with its own window and its own counter, so two programs
; making progress at once is something a test can assert rather than something a
; picture merely suggests.
;
;   ./native/build/time_warp_gates hb --tasks Examples/hb2/task_a.s Examples/hb2/task_b.s --ticks 6 --screen

again:  LEA R0, line
        INT 8
        MOV R2, #900            ; a different delay, so the two take turns unevenly
wait:   DEC R2
        JNZ wait
        MOV R3, #$3002
        LOAD R4, [R3]
        INC R4
        STORE [R3], R4
        JMP again

line:   DB "TASK B: RUNNING", 10, 0
