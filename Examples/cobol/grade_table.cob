       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRADE-TABLE.
       AUTHOR. Time Warp Studio.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-STUDENT-COUNT  PIC 99 VALUE 6.
       01  WS-INDEX          PIC 99 VALUE 1.
       01  WS-SUM            PIC 9(5) VALUE 0.
       01  WS-AVERAGE        PIC 9(3)V99 VALUE 0.
       01  WS-MIN            PIC 9(3) VALUE 999.
       01  WS-MAX            PIC 9(3) VALUE 0.
       01  WS-GRADE-LABEL    PIC X(2) VALUE SPACES.
       01  WS-SCORES.
           05 WS-SCORE OCCURS 6 TIMES PIC 9(3) VALUE 0.
       01  WS-NAMES.
           05 WS-NAME  OCCURS 6 TIMES PIC X(12) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY 'Student Grade Table'.
           DISPLAY '==================='.

           PERFORM LOAD-DATA.
           PERFORM CALC-STATS.
           PERFORM PRINT-REPORT.

           STOP RUN.

       LOAD-DATA.
           MOVE 'Alice'      TO WS-NAME(1).
           MOVE 'Bob'        TO WS-NAME(2).
           MOVE 'Carol'      TO WS-NAME(3).
           MOVE 'David'      TO WS-NAME(4).
           MOVE 'Eve'        TO WS-NAME(5).
           MOVE 'Frank'      TO WS-NAME(6).

           MOVE 92 TO WS-SCORE(1).
           MOVE 78 TO WS-SCORE(2).
           MOVE 85 TO WS-SCORE(3).
           MOVE 61 TO WS-SCORE(4).
           MOVE 97 TO WS-SCORE(5).
           MOVE 74 TO WS-SCORE(6).

       CALC-STATS.
           MOVE 0   TO WS-SUM.
           MOVE 999 TO WS-MIN.
           MOVE 0   TO WS-MAX.

           PERFORM SUM-LOOP VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > WS-STUDENT-COUNT.

           COMPUTE WS-AVERAGE = WS-SUM / WS-STUDENT-COUNT.

       SUM-LOOP.
           ADD WS-SCORE(WS-INDEX) TO WS-SUM.
           IF WS-SCORE(WS-INDEX) < WS-MIN
               MOVE WS-SCORE(WS-INDEX) TO WS-MIN.
           IF WS-SCORE(WS-INDEX) > WS-MAX
               MOVE WS-SCORE(WS-INDEX) TO WS-MAX.

       PRINT-REPORT.
           DISPLAY 'Name         Score  Grade'.
           DISPLAY '------------------------'.

           PERFORM PRINT-ROW VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > WS-STUDENT-COUNT.

           DISPLAY '------------------------'.
           DISPLAY 'Average: ' WS-AVERAGE.
           DISPLAY 'Minimum: ' WS-MIN.
           DISPLAY 'Maximum: ' WS-MAX.

       PRINT-ROW.
           EVALUATE TRUE
               WHEN WS-SCORE(WS-INDEX) >= 90
                   MOVE 'A ' TO WS-GRADE-LABEL
               WHEN WS-SCORE(WS-INDEX) >= 80
                   MOVE 'B ' TO WS-GRADE-LABEL
               WHEN WS-SCORE(WS-INDEX) >= 70
                   MOVE 'C ' TO WS-GRADE-LABEL
               WHEN WS-SCORE(WS-INDEX) >= 60
                   MOVE 'D ' TO WS-GRADE-LABEL
               WHEN OTHER
                   MOVE 'F ' TO WS-GRADE-LABEL
           END-EVALUATE.
           DISPLAY WS-NAME(WS-INDEX) '  ' WS-SCORE(WS-INDEX)
                   '     ' WS-GRADE-LABEL.
