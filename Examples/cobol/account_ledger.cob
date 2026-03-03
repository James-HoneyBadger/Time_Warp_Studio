      *================================================================
      * PROGRAM:    GLPOST01
      * DESCRIPTION: General Ledger Posting & Trial Balance
      *              - Post journal entries to GL
      *              - Compute account running balances
      *              - Print formatted trial balance report
      *================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GLPOST01.
       AUTHOR. ACME-SYSTEMS.
       DATE-WRITTEN. 2024-01-15.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZOS.
       OBJECT-COMPUTER. IBM-ZOS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORT-FILE ASSIGN TO RPTOUT
                              ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  REPORT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS.
       01  REPORT-LINE             PIC X(133).

       WORKING-STORAGE SECTION.

       01  WS-COUNTERS.
           05  WS-JRNL-COUNT       PIC S9(7) COMP VALUE ZEROS.
           05  WS-LINE-COUNT       PIC S9(5) COMP VALUE ZEROS.
           05  WS-PAGE-COUNT       PIC S9(5) COMP VALUE ZEROS.
           05  WS-POSTED-COUNT     PIC S9(7) COMP VALUE ZEROS.
           05  WS-ERROR-COUNT      PIC S9(7) COMP VALUE ZEROS.

       01  WS-TOTALS.
           05  WS-TOTAL-DEBITS     PIC S9(13)V99 COMP-3 VALUE ZEROS.
           05  WS-TOTAL-CREDITS    PIC S9(13)V99 COMP-3 VALUE ZEROS.
           05  WS-TOTAL-ASSETS     PIC S9(13)V99 COMP-3 VALUE ZEROS.
           05  WS-TOTAL-LIAB       PIC S9(13)V99 COMP-3 VALUE ZEROS.
           05  WS-TOTAL-EQUITY     PIC S9(13)V99 COMP-3 VALUE ZEROS.
           05  WS-TOTAL-REVENUE    PIC S9(13)V99 COMP-3 VALUE ZEROS.
           05  WS-TOTAL-EXPENSE    PIC S9(13)V99 COMP-3 VALUE ZEROS.

       01  WS-WORK.
           05  WS-PERIOD-ID        PIC X(6).
           05  WS-PERIOD-DATE-FROM PIC X(8).
           05  WS-PERIOD-DATE-TO   PIC X(8).
           05  WS-CURRENT-DATE     PIC X(8).
           05  WS-DIFF             PIC S9(13)V99 COMP-3.
           05  WS-ENDING-BAL       PIC S9(13)V99 COMP-3.

      *--- SQLCA ---
       01  SQLCA.
           05  SQLCAID              PIC X(8).
           05  SQLCABC              PIC S9(9) COMP.
           05  SQLCODE              PIC S9(9) COMP.
           05  SQLERRM.
               10  SQLERRML         PIC S9(4) COMP.
               10  SQLERRMC         PIC X(70).

      *--- HOST VARIABLES ---
       01  HV-GL.
           05  HV-PERIOD-ID         PIC X(6).
           05  HV-JOURNAL-ID        PIC S9(9) COMP.
           05  HV-ACCT-NUMBER       PIC X(6).
           05  HV-ACCT-DESC         PIC X(40).
           05  HV-ACCT-TYPE         PIC X(1).
           05  HV-NORMAL-BAL        PIC X(1).
           05  HV-DR-TOTAL          PIC S9(13)V99 COMP-3.
           05  HV-CR-TOTAL          PIC S9(13)V99 COMP-3.
           05  HV-BEG-BAL          PIC S9(13)V99 COMP-3.
           05  HV-END-BAL          PIC S9(13)V99 COMP-3.
           05  HV-JOURNAL-DATE      PIC X(8).
           05  HV-LINE-COUNT        PIC S9(9) COMP.
           05  HV-JRNL-STATUS       PIC X(1).

      *--- HEADER LINES ---
       01  HDR1.
           05  FILLER  PIC X(30) VALUE SPACES.
           05  FILLER  PIC X(40)
               VALUE 'ACME CORPORATION — GENERAL LEDGER'.
           05  FILLER  PIC X(28) VALUE SPACES.
           05  FILLER  PIC X(5) VALUE 'PAGE:'.
           05  H1-PAGE PIC ZZZZZ.
           05  FILLER  PIC X(25) VALUE SPACES.

       01  HDR2.
           05  FILLER  PIC X(30) VALUE SPACES.
           05  FILLER  PIC X(40)
               VALUE '           TRIAL BALANCE'.
           05  FILLER  PIC X(63) VALUE SPACES.

       01  HDR3.
           05  FILLER  PIC X(8)  VALUE 'PERIOD: '.
           05  H3-PER  PIC X(6).
           05  FILLER  PIC X(3)  VALUE SPACES.
           05  FILLER  PIC X(6)  VALUE 'DATE: '.
           05  H3-DATE PIC X(8).
           05  FILLER  PIC X(102) VALUE SPACES.

       01  COL1.
           05  FILLER  PIC X(6)  VALUE 'ACCT'.
           05  FILLER  PIC X(2)  VALUE SPACES.
           05  FILLER  PIC X(40) VALUE 'DESCRIPTION'.
           05  FILLER  PIC X(1)  VALUE 'T'.
           05  FILLER  PIC X(2)  VALUE SPACES.
           05  FILLER  PIC X(15) VALUE 'BEG BALANCE'.
           05  FILLER  PIC X(15) VALUE 'PERIOD DEBITS'.
           05  FILLER  PIC X(15) VALUE 'PERIOD CREDITS'.
           05  FILLER  PIC X(15) VALUE 'END BALANCE'.
           05  FILLER  PIC X(22) VALUE SPACES.

      *--- DETAIL LINE ---
       01  DET-LINE.
           05  DL-ACCT             PIC X(6).
           05  FILLER              PIC X(2) VALUE SPACES.
           05  DL-DESC             PIC X(40).
           05  DL-TYPE             PIC X(1).
           05  FILLER              PIC X(2) VALUE SPACES.
           05  DL-BEG-BAL         PIC S(ZZZ,ZZZ,ZZZ.99-).
           05  FILLER              PIC X(2) VALUE SPACES.
           05  DL-DEBITS          PIC ZZZ,ZZZ,ZZZ.99.
           05  FILLER              PIC X(2) VALUE SPACES.
           05  DL-CREDITS         PIC ZZZ,ZZZ,ZZZ.99.
           05  FILLER              PIC X(2) VALUE SPACES.
           05  DL-END-BAL         PIC S(ZZZ,ZZZ,ZZZ.99-).

      *--- SECTION TOTAL LINE ---
       01  SEC-TOT-LINE.
           05  FILLER              PIC X(48) VALUE SPACES.
           05  STL-LABEL           PIC X(15).
           05  FILLER              PIC X(2) VALUE SPACES.
           05  STL-TOTAL           PIC S(ZZZ,ZZZ,ZZZ,ZZ9.99-).
           05  FILLER              PIC X(36) VALUE SPACES.

      *--- GRAND TOTAL LINE ---
       01  GRAND-LINE.
           05  FILLER              PIC X(30) VALUE SPACES.
           05  GTL-LABEL           PIC X(20).
           05  FILLER              PIC X(2) VALUE SPACES.
           05  GTL-DR              PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER              PIC X(4) VALUE SPACES.
           05  GTL-CR              PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER              PIC X(34) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-POST-OPEN-JOURNALS
           PERFORM 3000-PRINT-TRIAL-BALANCE
           PERFORM 4000-PRINT-TOTALS
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           OPEN OUTPUT REPORT-FILE

           EXEC SQL
               SELECT PERIOD_ID, DATE_FROM, DATE_TO
               INTO :HV-PERIOD-ID, :WS-PERIOD-DATE-FROM,
                    :WS-PERIOD-DATE-TO
               FROM FISCAL_PERIOD
               WHERE STATUS = 'OPEN'
               FETCH FIRST 1 ROW ONLY
           END-EXEC

           MOVE HV-PERIOD-ID TO WS-PERIOD-ID
           MOVE WS-PERIOD-ID TO H3-PER

           EXEC SQL
               SELECT CHAR(CURRENT DATE, ISO)
               INTO :WS-CURRENT-DATE
               FROM SYSIBM.SYSDUMMY1
           END-EXEC

           MOVE WS-CURRENT-DATE TO H3-DATE
           PERFORM 9100-PRINT-HEADERS.

      *--- 2000 POST OPEN JOURNALS ---
       2000-POST-OPEN-JOURNALS.
           EXEC SQL
               DECLARE JRNL-CUR CURSOR FOR
               SELECT JOURNAL_ID
               FROM   GL_JOURNAL
               WHERE  PERIOD_ID = :HV-PERIOD-ID
               AND    STATUS = 'OPEN'
               ORDER BY JOURNAL_ID
           END-EXEC

           EXEC SQL  OPEN JRNL-CUR  END-EXEC

           PERFORM UNTIL SQLCODE = 100
               EXEC SQL
                   FETCH JRNL-CUR INTO :HV-JOURNAL-ID
               END-EXEC

               IF SQLCODE = 0
                   PERFORM 2100-VALIDATE-AND-POST
               END-IF
           END-PERFORM

           EXEC SQL  CLOSE JRNL-CUR  END-EXEC.

       2100-VALIDATE-AND-POST.
      *    Verify journal balances (total debits = total credits)
           EXEC SQL
               SELECT SUM(CASE DR_CR_IND WHEN 'D' THEN AMOUNT ELSE 0 END),
                      SUM(CASE DR_CR_IND WHEN 'C' THEN AMOUNT ELSE 0 END),
                      COUNT(*)
               INTO :HV-DR-TOTAL, :HV-CR-TOTAL, :HV-LINE-COUNT
               FROM GL_JOURNAL_LINE
               WHERE JOURNAL_ID = :HV-JOURNAL-ID
           END-EXEC

           COMPUTE WS-DIFF = HV-DR-TOTAL - HV-CR-TOTAL

           IF WS-DIFF NOT = ZEROS
               ADD 1 TO WS-ERROR-COUNT
               MOVE 'UNBALANCED' TO REPORT-LINE
               WRITE REPORT-LINE
           ELSE
      *        Mark journal as POSTED
               EXEC SQL
                   UPDATE GL_JOURNAL
                   SET    STATUS = 'POSTED',
                          POSTED_DATE = CURRENT DATE,
                          POSTED_BY   = 'GLPOST01'
                   WHERE  JOURNAL_ID = :HV-JOURNAL-ID
               END-EXEC

               EXEC SQL  COMMIT  END-EXEC
               ADD 1 TO WS-POSTED-COUNT
               ADD HV-DR-TOTAL TO WS-TOTAL-DEBITS
               ADD HV-CR-TOTAL TO WS-TOTAL-CREDITS
           END-IF.

      *--- 3000 PRINT TRIAL BALANCE ---
       3000-PRINT-TRIAL-BALANCE.
           EXEC SQL
               DECLARE TB-CUR CURSOR FOR
               SELECT COA.ACCT_NUMBER,
                      COA.ACCT_DESC,
                      COA.ACCT_TYPE,
                      COA.NORMAL_BAL,
                      COALESCE(BP.BEG_BALANCE, 0),
                      COALESCE(SUM(CASE JL.DR_CR_IND
                                   WHEN 'D' THEN JL.AMOUNT
                                   ELSE 0 END), 0),
                      COALESCE(SUM(CASE JL.DR_CR_IND
                                   WHEN 'C' THEN JL.AMOUNT
                                   ELSE 0 END), 0)
               FROM   CHART_OF_ACCOUNTS COA
               LEFT JOIN GL_JOURNAL_LINE JL
                      ON  JL.ACCT_NUMBER = COA.ACCT_NUMBER
               LEFT JOIN GL_JOURNAL GJ
                      ON  GJ.JOURNAL_ID = JL.JOURNAL_ID
                      AND GJ.PERIOD_ID  = :HV-PERIOD-ID
                      AND GJ.STATUS     = 'POSTED'
               LEFT JOIN (
                   SELECT ACCT_NUMBER,
                          ENDING_BALANCE AS BEG_BALANCE
                   FROM   GL_PERIOD_BALANCE
                   WHERE  PERIOD_SEQ = (
                       SELECT PERIOD_SEQ - 1
                       FROM   FISCAL_PERIOD
                       WHERE  PERIOD_ID = :HV-PERIOD-ID)
               ) BP ON BP.ACCT_NUMBER = COA.ACCT_NUMBER
               WHERE COA.ACTIVE_FLAG = 'Y'
               GROUP BY COA.ACCT_NUMBER, COA.ACCT_DESC,
                        COA.ACCT_TYPE,   COA.NORMAL_BAL,
                        BP.BEG_BALANCE
               ORDER BY COA.ACCT_NUMBER
           END-EXEC

           EXEC SQL  OPEN TB-CUR  END-EXEC

           PERFORM UNTIL SQLCODE = 100
               EXEC SQL
                   FETCH TB-CUR
                   INTO :HV-ACCT-NUMBER, :HV-ACCT-DESC,
                        :HV-ACCT-TYPE,   :HV-NORMAL-BAL,
                        :HV-BEG-BAL, :HV-DR-TOTAL, :HV-CR-TOTAL
               END-EXEC

               IF SQLCODE = 0
                   PERFORM 3100-COMPUTE-ENDING-BAL
                   PERFORM 3200-WRITE-DETAIL
                   PERFORM 3300-ACCUMULATE-TOTALS
               END-IF
           END-PERFORM

           EXEC SQL  CLOSE TB-CUR  END-EXEC.

       3100-COMPUTE-ENDING-BAL.
           EVALUATE HV-NORMAL-BAL
               WHEN 'D'
                   COMPUTE WS-ENDING-BAL =
                       HV-BEG-BAL + HV-DR-TOTAL - HV-CR-TOTAL
               WHEN 'C'
                   COMPUTE WS-ENDING-BAL =
                       HV-BEG-BAL - HV-DR-TOTAL + HV-CR-TOTAL
           END-EVALUATE.

       3200-WRITE-DETAIL.
           IF WS-LINE-COUNT > 55
               PERFORM 9100-PRINT-HEADERS
           END-IF
           MOVE HV-ACCT-NUMBER  TO DL-ACCT
           MOVE HV-ACCT-DESC    TO DL-DESC
           MOVE HV-ACCT-TYPE    TO DL-TYPE
           MOVE HV-BEG-BAL      TO DL-BEG-BAL
           MOVE HV-DR-TOTAL     TO DL-DEBITS
           MOVE HV-CR-TOTAL     TO DL-CREDITS
           MOVE WS-ENDING-BAL   TO DL-END-BAL
           MOVE DET-LINE        TO REPORT-LINE
           WRITE REPORT-LINE
           ADD 1 TO WS-LINE-COUNT.

       3300-ACCUMULATE-TOTALS.
           EVALUATE HV-ACCT-TYPE
               WHEN 'A'  ADD WS-ENDING-BAL TO WS-TOTAL-ASSETS
               WHEN 'L'  ADD WS-ENDING-BAL TO WS-TOTAL-LIAB
               WHEN 'E'  ADD WS-ENDING-BAL TO WS-TOTAL-EQUITY
               WHEN 'R'  ADD WS-ENDING-BAL TO WS-TOTAL-REVENUE
               WHEN 'X'  ADD WS-ENDING-BAL TO WS-TOTAL-EXPENSE
           END-EVALUATE.

      *--- 4000 PRINT TOTALS ---
       4000-PRINT-TOTALS.
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE FROM SPACES

           MOVE 'TOTAL ASSETS:   '  TO STL-LABEL
           MOVE WS-TOTAL-ASSETS     TO STL-TOTAL
           WRITE REPORT-LINE FROM SEC-TOT-LINE

           MOVE 'TOTAL LIABILITIES:' TO STL-LABEL
           MOVE WS-TOTAL-LIAB       TO STL-TOTAL
           WRITE REPORT-LINE FROM SEC-TOT-LINE

           MOVE 'TOTAL EQUITY:   '   TO STL-LABEL
           MOVE WS-TOTAL-EQUITY      TO STL-TOTAL
           WRITE REPORT-LINE FROM SEC-TOT-LINE

           MOVE 'TOTAL REVENUE:  '   TO STL-LABEL
           MOVE WS-TOTAL-REVENUE     TO STL-TOTAL
           WRITE REPORT-LINE FROM SEC-TOT-LINE

           MOVE 'TOTAL EXPENSE:  '   TO STL-LABEL
           MOVE WS-TOTAL-EXPENSE     TO STL-TOTAL
           WRITE REPORT-LINE FROM SEC-TOT-LINE

           WRITE REPORT-LINE FROM SPACES

           MOVE 'TOTAL DEBITS vs CREDITS:' TO GTL-LABEL
           MOVE WS-TOTAL-DEBITS  TO GTL-DR
           MOVE WS-TOTAL-CREDITS TO GTL-CR
           WRITE REPORT-LINE FROM GRAND-LINE

      *    Net income = Revenue - Expense
           COMPUTE WS-DIFF = WS-TOTAL-REVENUE - WS-TOTAL-EXPENSE
           MOVE 'NET INCOME (LOSS):  '  TO STL-LABEL
           MOVE WS-DIFF                 TO STL-TOTAL
           WRITE REPORT-LINE FROM SEC-TOT-LINE.

       9000-TERMINATE.
           CLOSE REPORT-FILE.

       9100-PRINT-HEADERS.
           ADD 1 TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT TO H1-PAGE
           WRITE REPORT-LINE FROM HDR1 AFTER PAGE
           WRITE REPORT-LINE FROM HDR2
           WRITE REPORT-LINE FROM HDR3
           WRITE REPORT-LINE FROM SPACES
           WRITE REPORT-LINE FROM COL1
           WRITE REPORT-LINE FROM SPACES
           MOVE 7 TO WS-LINE-COUNT.
