       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULTIPLICATION-TABLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-I     PIC 9(2) VALUE 1.
           01 WS-J     PIC 9(2) VALUE 1.
           01 WS-PROD  PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Multiplication Table (1-5):".
           PERFORM OUTER-LOOP VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 5.
           STOP RUN.

       OUTER-LOOP.
           PERFORM INNER-LOOP VARYING WS-J FROM 1 BY 1
               UNTIL WS-J > 5.

       INNER-LOOP.
           COMPUTE WS-PROD = WS-I * WS-J.
           DISPLAY WS-I " x " WS-J " = " WS-PROD.
