       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIBONACCI.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-A     PIC 9(12) VALUE 0.
           01 WS-B     PIC 9(12) VALUE 1.
           01 WS-TEMP  PIC 9(12) VALUE 0.
           01 WS-COUNT PIC 9(4)  VALUE 0.
           01 WS-LIMIT PIC 9(4)  VALUE 12.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Fibonacci sequence:".
           DISPLAY WS-A.
           DISPLAY WS-B.
           PERFORM FIB-LOOP VARYING WS-COUNT FROM 1 BY 1
               UNTIL WS-COUNT > WS-LIMIT.
           STOP RUN.

       FIB-LOOP.
           COMPUTE WS-TEMP = WS-A + WS-B.
           MOVE WS-B TO WS-A.
           MOVE WS-TEMP TO WS-B.
           DISPLAY WS-TEMP.
