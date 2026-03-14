       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORT-GENERATOR.
       AUTHOR. TIME-WARP-STUDIO.

      * ══════════════════════════════════════════
      *   📊 Sales Report Generator
      *   Demonstrates: tables, PERFORM, COMPUTE,
      *   formatted output, STRING operations
      * ══════════════════════════════════════════

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-TITLE      PIC X(40)
           VALUE "QUARTERLY SALES REPORT".
       01 WS-DATE        PIC X(10)  VALUE "2025-03-15".

       01 WS-SALES-TABLE.
           05 WS-REGION OCCURS 4 TIMES.
               10 WS-REGION-NAME PIC X(12).
               10 WS-Q1         PIC 9(6)V99.
               10 WS-Q2         PIC 9(6)V99.
               10 WS-Q3         PIC 9(6)V99.
               10 WS-Q4         PIC 9(6)V99.

       01 WS-IDX         PIC 9.
       01 WS-TOTAL       PIC 9(7)V99.
       01 WS-GRAND-TOTAL PIC 9(8)V99 VALUE 0.
       01 WS-AVG         PIC 9(6)V99.
       01 WS-BEST-REGION PIC X(12).
       01 WS-BEST-SALES  PIC 9(8)V99 VALUE 0.

       01 WS-EDIT-TOTAL  PIC $$$,$$$,$$9.99.
       01 WS-EDIT-AVG    PIC $$$,$$9.99.
       01 WS-EDIT-GRAND  PIC $$,$$$,$$9.99.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INIT-DATA
           PERFORM PRINT-HEADER
           PERFORM PRINT-DETAIL-LINES
           PERFORM PRINT-SUMMARY
           STOP RUN.

       INIT-DATA.
           MOVE "North"       TO WS-REGION-NAME(1)
           MOVE 125000.50     TO WS-Q1(1)
           MOVE 138500.75     TO WS-Q2(1)
           MOVE 142300.00     TO WS-Q3(1)
           MOVE 165800.25     TO WS-Q4(1)

           MOVE "South"       TO WS-REGION-NAME(2)
           MOVE 98750.00      TO WS-Q1(2)
           MOVE 102400.50     TO WS-Q2(2)
           MOVE 115600.75     TO WS-Q3(2)
           MOVE 128900.00     TO WS-Q4(2)

           MOVE "East"        TO WS-REGION-NAME(3)
           MOVE 156200.00     TO WS-Q1(3)
           MOVE 162800.50     TO WS-Q2(3)
           MOVE 171500.25     TO WS-Q3(3)
           MOVE 185400.00     TO WS-Q4(3)

           MOVE "West"        TO WS-REGION-NAME(4)
           MOVE 87500.00      TO WS-Q1(4)
           MOVE 94200.75      TO WS-Q2(4)
           MOVE 101800.50     TO WS-Q3(4)
           MOVE 112300.25     TO WS-Q4(4).

       PRINT-HEADER.
           DISPLAY "╔══════════════════════════════════════"
                   "════════════════════╗"
           DISPLAY "║         📊 " WS-TITLE
                   "          ║"
           DISPLAY "║         Date: " WS-DATE
                   "                          ║"
           DISPLAY "╠══════════════════════════════════════"
                   "════════════════════╣"
           DISPLAY "║ Region       Q1          Q2"
                   "          Q3          Q4  ║"
           DISPLAY "╠══════════════════════════════════════"
                   "════════════════════╣".

       PRINT-DETAIL-LINES.
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > 4
               COMPUTE WS-TOTAL = WS-Q1(WS-IDX)
                   + WS-Q2(WS-IDX)
                   + WS-Q3(WS-IDX)
                   + WS-Q4(WS-IDX)

               ADD WS-TOTAL TO WS-GRAND-TOTAL

               IF WS-TOTAL > WS-BEST-SALES
                   MOVE WS-TOTAL TO WS-BEST-SALES
                   MOVE WS-REGION-NAME(WS-IDX)
                       TO WS-BEST-REGION
               END-IF

               MOVE WS-TOTAL TO WS-EDIT-TOTAL
               COMPUTE WS-AVG = WS-TOTAL / 4
               MOVE WS-AVG TO WS-EDIT-AVG

               DISPLAY "║ " WS-REGION-NAME(WS-IDX)
                   "  Total: " WS-EDIT-TOTAL
                   "  Avg: " WS-EDIT-AVG "  ║"
           END-PERFORM.

       PRINT-SUMMARY.
           DISPLAY "╠══════════════════════════════════════"
                   "════════════════════╣"
           MOVE WS-GRAND-TOTAL TO WS-EDIT-GRAND
           DISPLAY "║ Grand Total: " WS-EDIT-GRAND
                   "                          ║"
           DISPLAY "║ Best Region: " WS-BEST-REGION
                   "                              ║"
           MOVE WS-BEST-SALES TO WS-EDIT-GRAND
           DISPLAY "║ Best Sales:  " WS-EDIT-GRAND
                   "                          ║"
           DISPLAY "╚══════════════════════════════════════"
                   "════════════════════╝"
           DISPLAY " "
           DISPLAY "✅ Report generation complete.".
