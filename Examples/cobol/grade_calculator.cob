       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRADE-CALC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-SCORE  PIC 9(3) VALUE 85.
           01 WS-GRADE  PIC X(2) VALUE "  ".

       PROCEDURE DIVISION.
       MAIN-PARA.
           EVALUATE TRUE
               WHEN WS-SCORE >= 90
                   MOVE "A" TO WS-GRADE
               WHEN WS-SCORE >= 80
                   MOVE "B" TO WS-GRADE
               WHEN WS-SCORE >= 70
                   MOVE "C" TO WS-GRADE
               WHEN WS-SCORE >= 60
                   MOVE "D" TO WS-GRADE
               WHEN OTHER
                   MOVE "F" TO WS-GRADE
           END-EVALUATE.
           DISPLAY "Score: " WS-SCORE.
           DISPLAY "Grade: " WS-GRADE.
           STOP RUN.
