      *================================================================
      * PROGRAM:    INVCTR01
      * DESCRIPTION: Inventory Control System
      *              - Receive goods into warehouse
      *              - Issue items to sales orders
      *              - Transfer stock between warehouses
      *              - Generate reorder alerts
      *================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVCTR01.
       AUTHOR. ACME-SYSTEMS.
       DATE-WRITTEN. 2024-01-15.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZOS.
       OBJECT-COMPUTER. IBM-ZOS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANS-FILE  ASSIGN TO TRANSIN
                              ORGANIZATION IS SEQUENTIAL
                              ACCESS MODE IS SEQUENTIAL.
           SELECT REPORT-FILE ASSIGN TO RPTOUT
                              ORGANIZATION IS SEQUENTIAL.
           SELECT ERROR-FILE  ASSIGN TO ERROUT
                              ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  TRANS-RECORD.
           05  TR-TRANS-TYPE       PIC X(4).
                   88  TR-RECEIPT         VALUE 'RECV'.
                   88  TR-ISSUE           VALUE 'ISSU'.
                   88  TR-TRANSFER        VALUE 'TRFR'.
                   88  TR-ADJUSTMENT      VALUE 'ADJT'.
           05  TR-ITEM-NUMBER      PIC X(10).
           05  TR-FROM-WAREHOUSE   PIC X(4).
           05  TR-TO-WAREHOUSE     PIC X(4).
           05  TR-QUANTITY         PIC S9(7)V99 COMP-3.
           05  TR-UNIT-COST        PIC S9(7)V9999 COMP-3.
           05  TR-ORDER-NUMBER     PIC X(10).
           05  TR-REFERENCE        PIC X(20).
           05  TR-TRANS-DATE       PIC X(8).
           05  FILLER              PIC X(131).

       FD  REPORT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS.
       01  REPORT-LINE             PIC X(133).

       FD  ERROR-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 200 CHARACTERS.
       01  ERROR-LINE              PIC X(200).

       WORKING-STORAGE SECTION.

      *--- Program Switches ---
       01  WS-SWITCHES.
           05  WS-EOF-SWITCH        PIC X(1)  VALUE 'N'.
               88  WS-EOF                     VALUE 'Y'.
           05  WS-FIRST-PAGE        PIC X(1)  VALUE 'Y'.

      *--- Transaction Counts ---
       01  WS-COUNTERS.
           05  WS-TRANS-READ        PIC S9(7) COMP VALUE ZEROS.
           05  WS-RECV-COUNT        PIC S9(7) COMP VALUE ZEROS.
           05  WS-ISSU-COUNT        PIC S9(7) COMP VALUE ZEROS.
           05  WS-TRFR-COUNT        PIC S9(7) COMP VALUE ZEROS.
           05  WS-ADJT-COUNT        PIC S9(7) COMP VALUE ZEROS.
           05  WS-ERROR-COUNT       PIC S9(7) COMP VALUE ZEROS.
           05  WS-REORDER-COUNT     PIC S9(7) COMP VALUE ZEROS.
           05  WS-PAGE-COUNT        PIC S9(5) COMP VALUE ZEROS.
           05  WS-LINE-COUNT        PIC S9(5) COMP VALUE ZEROS.

      *--- Working Fields ---
       01  WS-WORK-AREA.
           05  WS-CURRENT-DATE      PIC X(8).
           05  WS-CURRENT-TIME      PIC X(6).
           05  WS-SQLCODE-SAVE      PIC S9(9) COMP VALUE ZEROS.
           05  WS-NEW-BALANCE       PIC S9(9)V99 COMP-3 VALUE ZEROS.
           05  WS-NEW-BALANCE-EDIT  PIC ZZZ,ZZ9.99-.
           05  WS-TRANS-VALUE       PIC S9(9)V9999 COMP-3 VALUE ZEROS.
           05  WS-GL-AMOUNT         PIC S9(9)V99 COMP-3 VALUE ZEROS.

      *--- Item and Balance Work Fields ---
       01  WS-ITEM-DATA.
           05  WS-ITEM-DESC         PIC X(40).
           05  WS-STD-COST          PIC S9(7)V9999 COMP-3.
           05  WS-REORDER-POINT     PIC S9(7)V99 COMP-3.
           05  WS-REORDER-QTY       PIC S9(7)V99 COMP-3.
           05  WS-ON-HAND           PIC S9(9)V99 COMP-3.
           05  WS-ON-ORDER          PIC S9(9)V99 COMP-3.
           05  WS-ALLOCATED         PIC S9(9)V99 COMP-3.
           05  WS-AVAILABLE         PIC S9(9)V99 COMP-3.

      *--- Report Header Lines ---
       01  HDR-LINE1.
           05  FILLER               PIC X(20) VALUE SPACES.
           05  FILLER               PIC X(40)
               VALUE 'ACME CORPORATION — INVENTORY TRANSACTION'.
           05  FILLER               PIC X(20) VALUE SPACES.
           05  FILLER               PIC X(7) VALUE 'PAGE:'.
           05  HDR-PAGE-NO          PIC ZZZZZ.
           05  FILLER               PIC X(41) VALUE SPACES.

       01  HDR-LINE2.
           05  FILLER               PIC X(20) VALUE SPACES.
           05  FILLER               PIC X(50)
               VALUE '           ACTIVITY REGISTER'.
           05  FILLER               PIC X(63) VALUE SPACES.

       01  HDR-LINE3.
           05  FILLER               PIC X(5) VALUE 'DATE:'.
           05  HDR-DATE             PIC X(8).
           05  FILLER               PIC X(3) VALUE SPACES.
           05  FILLER               PIC X(5) VALUE 'TIME:'.
           05  HDR-TIME             PIC X(6).
           05  FILLER               PIC X(106) VALUE SPACES.

       01  COL-LINE.
           05  FILLER               PIC X(4)  VALUE 'TYPE'.
           05  FILLER               PIC X(2)  VALUE SPACES.
           05  FILLER               PIC X(10) VALUE 'ITEM'.
           05  FILLER               PIC X(2)  VALUE SPACES.
           05  FILLER               PIC X(40) VALUE 'DESCRIPTION'.
           05  FILLER               PIC X(5)  VALUE 'WH   '.
           05  FILLER               PIC X(12) VALUE '   QUANTITY'.
           05  FILLER               PIC X(12) VALUE '  UNIT COST '.
           05  FILLER               PIC X(13) VALUE ' TRANS VALUE '.
           05  FILLER               PIC X(14) VALUE '   NEW BALANCE'.
           05  FILLER               PIC X(18) VALUE SPACES.

      *--- Detail Report Line ---
       01  DETAIL-LINE.
           05  DL-TRANS-TYPE        PIC X(4).
           05  FILLER               PIC X(2) VALUE SPACES.
           05  DL-ITEM-NUM          PIC X(10).
           05  FILLER               PIC X(2) VALUE SPACES.
           05  DL-ITEM-DESC         PIC X(40).
           05  DL-WAREHOUSE         PIC X(5).
           05  DL-QUANTITY          PIC ZZZ,ZZ9.99-.
           05  FILLER               PIC X(2) VALUE SPACES.
           05  DL-UNIT-COST         PIC ZZZ,ZZ9.9999.
           05  FILLER               PIC X(2) VALUE SPACES.
           05  DL-TRANS-VALUE       PIC ZZZ,ZZZ,ZZ9.99-.
           05  FILLER               PIC X(2) VALUE SPACES.
           05  DL-NEW-BALANCE       PIC ZZZ,ZZZ,ZZ9.99-.

      *--- Reorder Alert Line ---
       01  REORDER-LINE.
           05  FILLER               PIC X(5) VALUE '*** '.
           05  FILLER               PIC X(8) VALUE 'REORDER:'.
           05  RL-ITEM-NUM          PIC X(10).
           05  FILLER               PIC X(2) VALUE SPACES.
           05  RL-ITEM-DESC         PIC X(40).
           05  FILLER               PIC X(3) VALUE 'OH='.
           05  RL-ON-HAND           PIC ZZZ,ZZ9.99-.
           05  FILLER               PIC X(3) VALUE 'OP='.
           05  RL-REORDER-PT        PIC ZZZ,ZZZ.
           05  FILLER               PIC X(4) VALUE 'SQ: '.
           05  RL-SUGGEST-QTY       PIC ZZZ,ZZZ.
           05  FILLER               PIC X(52) VALUE SPACES.

      *--- Summary Line ---
       01  SUMMARY-LINE.
           05  FILLER               PIC X(10) VALUE SPACES.
           05  SL-LABEL             PIC X(30).
           05  SL-COUNT             PIC ZZZ,ZZ9.
           05  FILLER               PIC X(87) VALUE SPACES.

      *--- EXEC SQL INCLUDE SQLCA ---
       01  SQLCA.
           05  SQLCAID              PIC X(8)  VALUE 'SQLCA'.
           05  SQLCABC              PIC S9(9) COMP VALUE 136.
           05  SQLCODE              PIC S9(9) COMP.
           05  SQLERRM.
               10  SQLERRML         PIC S9(4) COMP.
               10  SQLERRMC         PIC X(70).
           05  SQLERRP              PIC X(8).
           05  SQLERRD              OCCURS 6 TIMES PIC S9(9) COMP.
           05  SQLWARN.
               10  SQLWARN0         PIC X(1).
               10  SQLWARN1         PIC X(1).
           05  SQLSTATE             PIC X(5).

      *--- Host Variables ---
       01  HV-ITEM.
           05  HV-ITEM-NUMBER       PIC X(10).
           05  HV-WAREHOUSE-ID      PIC X(4).
           05  HV-QUANTITY          PIC S9(9)V99 COMP-3.
           05  HV-UNIT-COST         PIC S9(9)V9999 COMP-3.
           05  HV-TRANS-TYPE        PIC X(4).
           05  HV-REFERENCE         PIC X(20).
           05  HV-FROM-WH           PIC X(4).
           05  HV-TO-WH             PIC X(4).
           05  HV-ORDER-NUMBER      PIC X(10).
           05  HV-BALANCE           PIC S9(9)V99 COMP-3.
           05  HV-PERIOD-DATE       PIC X(8).
           05  HV-ITEM-DESC-OUT     PIC X(40).
           05  HV-STD-COST-OUT      PIC S9(9)V9999 COMP-3.
           05  HV-REORDER-PT-OUT    PIC S9(9)V99 COMP-3.
           05  HV-REORDER-QTY-OUT   PIC S9(9)V99 COMP-3.
           05  HV-ON-HAND-OUT       PIC S9(9)V99 COMP-3.
           05  HV-GL-DR-ACCT        PIC X(6).
           05  HV-GL-CR-ACCT        PIC X(6).
           05  HV-GL-AMT            PIC S9(9)V99 COMP-3.
           05  HV-GL-DESC           PIC X(50).

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 9100-GET-DATE-TIME
           PERFORM 2000-PROCESS-TRANSACTIONS
               UNTIL WS-EOF
           PERFORM 3000-PRINT-SUMMARY
           PERFORM 9000-TERMINATE
           STOP RUN.

      *--- 1000 INITIALIZE ---
       1000-INITIALIZE.
           OPEN INPUT  TRANS-FILE
                OUTPUT REPORT-FILE
                       ERROR-FILE

           PERFORM 9200-WRITE-HEADERS

           READ TRANS-FILE
               AT END MOVE 'Y' TO WS-EOF-SWITCH
           END-READ.

      *--- 2000 PROCESS TRANSACTIONS ---
       2000-PROCESS-TRANSACTIONS.
           ADD 1 TO WS-TRANS-READ

           EVALUATE TRUE
               WHEN TR-RECEIPT
                   PERFORM 2100-PROCESS-RECEIPT
               WHEN TR-ISSUE
                   PERFORM 2200-PROCESS-ISSUE
               WHEN TR-TRANSFER
                   PERFORM 2300-PROCESS-TRANSFER
               WHEN TR-ADJUSTMENT
                   PERFORM 2400-PROCESS-ADJUSTMENT
               WHEN OTHER
                   PERFORM 2900-INVALID-TRANS
           END-EVALUATE

           READ TRANS-FILE
               AT END MOVE 'Y' TO WS-EOF-SWITCH
           END-READ.

      *--- 2100 PROCESS RECEIPT ---
       2100-PROCESS-RECEIPT.
           ADD 1 TO WS-RECV-COUNT

           MOVE TR-ITEM-NUMBER  TO HV-ITEM-NUMBER
           MOVE TR-TO-WAREHOUSE TO HV-WAREHOUSE-ID

      *    Fetch current balance and item details
           EXEC SQL
               SELECT I.ITEM_DESC, I.STD_COST,
                      I.REORDER_POINT, I.REORDER_QTY,
                      COALESCE(B.QTY_ON_HAND, 0)
               INTO   :HV-ITEM-DESC-OUT, :HV-STD-COST-OUT,
                      :HV-REORDER-PT-OUT, :HV-REORDER-QTY-OUT,
                      :HV-ON-HAND-OUT
               FROM   ITEM_MASTER I
               LEFT JOIN INVENTORY_BALANCE B
                      ON  B.ITEM_NUMBER  = I.ITEM_NUMBER
                      AND B.WAREHOUSE_ID = :HV-WAREHOUSE-ID
               WHERE  I.ITEM_NUMBER = :HV-ITEM-NUMBER
           END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 2950-SQL-ERROR
               GO TO 2199-RECEIPT-EXIT
           END-IF

      *    Calculate new balance
           COMPUTE WS-NEW-BALANCE =
               HV-ON-HAND-OUT + TR-QUANTITY
           COMPUTE WS-TRANS-VALUE =
               TR-QUANTITY * TR-UNIT-COST

      *    Update or insert balance record
           EXEC SQL
               UPDATE INVENTORY_BALANCE
               SET    QTY_ON_HAND     = QTY_ON_HAND + :TR-QUANTITY,
                      LAST_TRANS_DATE = CURRENT DATE
               WHERE  ITEM_NUMBER  = :HV-ITEM-NUMBER
               AND    WAREHOUSE_ID = :HV-WAREHOUSE-ID
           END-EXEC

           IF SQLCODE = 100
               EXEC SQL
                   INSERT INTO INVENTORY_BALANCE
                   (ITEM_NUMBER, WAREHOUSE_ID,
                    QTY_ON_HAND, QTY_ON_ORDER, QTY_ALLOCATED,
                    LAST_TRANS_DATE)
                   VALUES
                   (:HV-ITEM-NUMBER, :HV-WAREHOUSE-ID,
                    :TR-QUANTITY, 0, 0, CURRENT DATE)
               END-EXEC
           END-IF

      *    Record transaction history
           MOVE 'RECV'              TO HV-TRANS-TYPE
           MOVE TR-REFERENCE        TO HV-REFERENCE
           MOVE TR-QUANTITY         TO HV-QUANTITY
           MOVE TR-UNIT-COST        TO HV-UNIT-COST
           PERFORM 2800-INSERT-TRANS-HISTORY

      *    Post GL: DR 1400 Inventory / CR 2100 Accounts Payable
           MOVE '1400'              TO HV-GL-DR-ACCT
           MOVE '2100'              TO HV-GL-CR-ACCT
           COMPUTE HV-GL-AMT = TR-QUANTITY * TR-UNIT-COST
           MOVE 'Receipt: '         TO HV-GL-DESC
           STRING 'Receipt: ' DELIMITED SIZE
                  TR-ITEM-NUMBER    DELIMITED SPACE
                  INTO HV-GL-DESC
           PERFORM 2810-POST-GL

           EXEC SQL  COMMIT  END-EXEC

      *    Write report detail
           PERFORM 9300-WRITE-DETAIL-LINE

      *    Check reorder needs
           IF WS-NEW-BALANCE < HV-REORDER-PT-OUT
               PERFORM 3100-PRINT-REORDER-ALERT
               ADD 1 TO WS-REORDER-COUNT
           END-IF

       2199-RECEIPT-EXIT.
           EXIT.

      *--- 2200 PROCESS ISSUE ---
       2200-PROCESS-ISSUE.
           ADD 1 TO WS-ISSU-COUNT
           MOVE TR-ITEM-NUMBER    TO HV-ITEM-NUMBER
           MOVE TR-FROM-WAREHOUSE TO HV-WAREHOUSE-ID

           EXEC SQL
               SELECT I.ITEM_DESC, I.STD_COST,
                      I.REORDER_POINT, I.REORDER_QTY,
                      B.QTY_ON_HAND
               INTO   :HV-ITEM-DESC-OUT, :HV-STD-COST-OUT,
                      :HV-REORDER-PT-OUT, :HV-REORDER-QTY-OUT,
                      :HV-ON-HAND-OUT
               FROM   ITEM_MASTER I
               JOIN INVENTORY_BALANCE B
                    ON  B.ITEM_NUMBER  = I.ITEM_NUMBER
                    AND B.WAREHOUSE_ID = :HV-WAREHOUSE-ID
               WHERE  I.ITEM_NUMBER = :HV-ITEM-NUMBER
           END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 2950-SQL-ERROR
               GO TO 2299-ISSUE-EXIT
           END-IF

           IF HV-ON-HAND-OUT < TR-QUANTITY
               MOVE 'ISSU-SHORTFALL'  TO ERROR-LINE
               WRITE ERROR-LINE
               ADD 1 TO WS-ERROR-COUNT
               GO TO 2299-ISSUE-EXIT
           END-IF

           COMPUTE WS-NEW-BALANCE =
               HV-ON-HAND-OUT - TR-QUANTITY

           EXEC SQL
               UPDATE INVENTORY_BALANCE
               SET    QTY_ON_HAND     = QTY_ON_HAND - :TR-QUANTITY,
                      QTY_ALLOCATED   = QTY_ALLOCATED - :TR-QUANTITY,
                      LAST_TRANS_DATE = CURRENT DATE
               WHERE  ITEM_NUMBER  = :HV-ITEM-NUMBER
               AND    WAREHOUSE_ID = :HV-WAREHOUSE-ID
           END-EXEC

           MOVE 'ISSU'            TO HV-TRANS-TYPE
           MOVE TR-ORDER-NUMBER   TO HV-REFERENCE
           MOVE TR-QUANTITY       TO HV-QUANTITY
           MOVE HV-STD-COST-OUT   TO HV-UNIT-COST
           PERFORM 2800-INSERT-TRANS-HISTORY

      *    DR 5000 COGS / CR 1400 Inventory
           MOVE '5000'            TO HV-GL-DR-ACCT
           MOVE '1400'            TO HV-GL-CR-ACCT
           COMPUTE HV-GL-AMT = TR-QUANTITY * HV-STD-COST-OUT
           STRING 'Issue to order: ' DELIMITED SIZE
                  TR-ORDER-NUMBER    DELIMITED SPACE
                  INTO HV-GL-DESC
           PERFORM 2810-POST-GL

           EXEC SQL  COMMIT  END-EXEC
           PERFORM 9300-WRITE-DETAIL-LINE

           IF WS-NEW-BALANCE < HV-REORDER-PT-OUT
               PERFORM 3100-PRINT-REORDER-ALERT
               ADD 1 TO WS-REORDER-COUNT
           END-IF

       2299-ISSUE-EXIT.
           EXIT.

      *--- 2300 PROCESS TRANSFER ---
       2300-PROCESS-TRANSFER.
           ADD 1 TO WS-TRFR-COUNT
           MOVE TR-ITEM-NUMBER    TO HV-ITEM-NUMBER
           MOVE TR-FROM-WAREHOUSE TO HV-WAREHOUSE-ID

           EXEC SQL
               UPDATE INVENTORY_BALANCE
               SET    QTY_ON_HAND     = QTY_ON_HAND - :TR-QUANTITY,
                      LAST_TRANS_DATE = CURRENT DATE
               WHERE  ITEM_NUMBER  = :HV-ITEM-NUMBER
               AND    WAREHOUSE_ID = :HV-WAREHOUSE-ID
           END-EXEC

           MOVE TR-TO-WAREHOUSE   TO HV-WAREHOUSE-ID
           EXEC SQL
               UPDATE INVENTORY_BALANCE
               SET    QTY_ON_HAND     = QTY_ON_HAND + :TR-QUANTITY,
                      LAST_TRANS_DATE = CURRENT DATE
               WHERE  ITEM_NUMBER  = :HV-ITEM-NUMBER
               AND    WAREHOUSE_ID = :HV-WAREHOUSE-ID
           END-EXEC

           IF SQLCODE = 100
               EXEC SQL
                   INSERT INTO INVENTORY_BALANCE
                   (ITEM_NUMBER, WAREHOUSE_ID,
                    QTY_ON_HAND, QTY_ON_ORDER, QTY_ALLOCATED,
                    LAST_TRANS_DATE)
                   VALUES (:HV-ITEM-NUMBER, :HV-WAREHOUSE-ID,
                           :TR-QUANTITY, 0, 0, CURRENT DATE)
               END-EXEC
           END-IF

           EXEC SQL  COMMIT  END-EXEC
           PERFORM 9300-WRITE-DETAIL-LINE.

      *--- 2400 PROCESS ADJUSTMENT ---
       2400-PROCESS-ADJUSTMENT.
           ADD 1 TO WS-ADJT-COUNT
           MOVE TR-ITEM-NUMBER    TO HV-ITEM-NUMBER
           MOVE TR-TO-WAREHOUSE   TO HV-WAREHOUSE-ID
           MOVE TR-QUANTITY       TO HV-QUANTITY

           EXEC SQL
               UPDATE INVENTORY_BALANCE
               SET    QTY_ON_HAND     = QTY_ON_HAND + :HV-QUANTITY,
                      LAST_TRANS_DATE = CURRENT DATE
               WHERE  ITEM_NUMBER  = :HV-ITEM-NUMBER
               AND    WAREHOUSE_ID = :HV-WAREHOUSE-ID
           END-EXEC

           EXEC SQL  COMMIT  END-EXEC
           PERFORM 9300-WRITE-DETAIL-LINE.

      *--- 2800 INSERT TRANSACTION HISTORY ---
       2800-INSERT-TRANS-HISTORY.
           EXEC SQL
               INSERT INTO INVENTORY_TRANSACTION
               (ITEM_NUMBER, WAREHOUSE_ID, TRANS_TYPE,
                TRANS_DATE, QUANTITY, UNIT_COST,
                REFERENCE_NO, CREATED_BY)
               VALUES
               (:HV-ITEM-NUMBER, :HV-WAREHOUSE-ID, :HV-TRANS-TYPE,
                CURRENT DATE,    :HV-QUANTITY, :HV-UNIT-COST,
                :HV-REFERENCE, 'INVCTR01')
           END-EXEC.

      *--- 2810 POST GL ---
       2810-POST-GL.
           EXEC SQL
               INSERT INTO GL_JOURNAL_LINE
               (JOURNAL_ID, LINE_SEQ, ACCT_NUMBER,
                DR_CR_IND, AMOUNT, DESCRIPTION)
               SELECT GJ.JOURNAL_ID, 
                      COUNT(*) + 1,
                      :HV-GL-DR-ACCT,
                      'D', :HV-GL-AMT, :HV-GL-DESC
               FROM   GL_JOURNAL GJ
               WHERE  GJ.STATUS = 'OPEN'
               AND    GJ.JOURNAL_TYPE = 'AUTO'
               FETCH FIRST 1 ROW ONLY
           END-EXEC.

      *--- 2900 INVALID TRANSACTION TYPE ---
       2900-INVALID-TRANS.
           ADD 1 TO WS-ERROR-COUNT
           MOVE TR-TRANS-TYPE TO ERROR-LINE
           WRITE ERROR-LINE.

      *--- 2950 SQL ERROR ---
       2950-SQL-ERROR.
           ADD 1 TO WS-ERROR-COUNT
           MOVE SQLERRMC TO ERROR-LINE
           WRITE ERROR-LINE
           EXEC SQL  ROLLBACK  END-EXEC.

      *--- 3000 PRINT SUMMARY ---
       3000-PRINT-SUMMARY.
           MOVE '===== TRANSACTION SUMMARY =====' TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'Transactions Read:  '  TO SL-LABEL
           MOVE WS-TRANS-READ          TO SL-COUNT
           MOVE SUMMARY-LINE           TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'Receipts Processed: '  TO SL-LABEL
           MOVE WS-RECV-COUNT          TO SL-COUNT
           MOVE SUMMARY-LINE           TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'Issues Processed:   '  TO SL-LABEL
           MOVE WS-ISSU-COUNT          TO SL-COUNT
           MOVE SUMMARY-LINE           TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'Transfers:          '  TO SL-LABEL
           MOVE WS-TRFR-COUNT          TO SL-COUNT
           MOVE SUMMARY-LINE           TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'Adjustments:        '  TO SL-LABEL
           MOVE WS-ADJT-COUNT          TO SL-COUNT
           MOVE SUMMARY-LINE           TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'Reorder Alerts:     '  TO SL-LABEL
           MOVE WS-REORDER-COUNT       TO SL-COUNT
           MOVE SUMMARY-LINE           TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE 'Errors:             '  TO SL-LABEL
           MOVE WS-ERROR-COUNT         TO SL-COUNT
           MOVE SUMMARY-LINE           TO REPORT-LINE
           WRITE REPORT-LINE.

      *--- 3100 PRINT REORDER ALERT ---
       3100-PRINT-REORDER-ALERT.
           MOVE TR-ITEM-NUMBER      TO RL-ITEM-NUM
           MOVE HV-ITEM-DESC-OUT    TO RL-ITEM-DESC
           MOVE WS-NEW-BALANCE      TO RL-ON-HAND
           MOVE HV-REORDER-PT-OUT   TO RL-REORDER-PT
           MOVE HV-REORDER-QTY-OUT  TO RL-SUGGEST-QTY
           MOVE REORDER-LINE        TO REPORT-LINE
           WRITE REPORT-LINE.

      *--- 9000 TERMINATE ---
       9000-TERMINATE.
           CLOSE TRANS-FILE
                 REPORT-FILE
                 ERROR-FILE.

      *--- 9100 GET DATE/TIME ---
       9100-GET-DATE-TIME.
           EXEC SQL
               SELECT CHAR(CURRENT DATE, ISO),
                      CHAR(CURRENT TIME, ISO)
               INTO :WS-CURRENT-DATE, :WS-CURRENT-TIME
               FROM SYSIBM.SYSDUMMY1
           END-EXEC
           MOVE WS-CURRENT-DATE TO HDR-DATE
           MOVE WS-CURRENT-TIME TO HDR-TIME.

      *--- 9200 WRITE HEADERS ---
       9200-WRITE-HEADERS.
           ADD 1 TO WS-PAGE-COUNT
           MOVE WS-PAGE-COUNT TO HDR-PAGE-NO
           WRITE REPORT-LINE FROM HDR-LINE1 AFTER PAGE
           WRITE REPORT-LINE FROM HDR-LINE2
           WRITE REPORT-LINE FROM HDR-LINE3
           WRITE REPORT-LINE FROM SPACES
           WRITE REPORT-LINE FROM COL-LINE
           WRITE REPORT-LINE FROM SPACES
           MOVE 7 TO WS-LINE-COUNT.

      *--- 9300 WRITE DETAIL LINE ---
       9300-WRITE-DETAIL-LINE.
           IF WS-LINE-COUNT > 55
               PERFORM 9200-WRITE-HEADERS
           END-IF
           MOVE TR-TRANS-TYPE       TO DL-TRANS-TYPE
           MOVE TR-ITEM-NUMBER      TO DL-ITEM-NUM
           MOVE HV-ITEM-DESC-OUT    TO DL-ITEM-DESC
           MOVE TR-TO-WAREHOUSE     TO DL-WAREHOUSE
           MOVE TR-QUANTITY         TO DL-QUANTITY
           MOVE TR-UNIT-COST        TO DL-UNIT-COST
           MOVE WS-TRANS-VALUE      TO DL-TRANS-VALUE
           MOVE WS-NEW-BALANCE      TO WS-NEW-BALANCE-EDIT
           MOVE WS-NEW-BALANCE-EDIT TO DL-NEW-BALANCE
           MOVE DETAIL-LINE         TO REPORT-LINE
           WRITE REPORT-LINE
           ADD 1 TO WS-LINE-COUNT.
