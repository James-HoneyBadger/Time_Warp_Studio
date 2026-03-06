      *================================================================
      * PROGRAM:    INVCTR01 (Simplified)
      * DESCRIPTION: Inventory Control Demo
      *              Demonstrates COBOL data handling and reporting
      *================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVCTR01.
       AUTHOR. TIME-WARP-STUDIO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-HEADER-LINES.
           05  FILLER PIC X(50) VALUE
               '=================================================='.
           05  WS-TITLE PIC X(50) VALUE
               '  INVENTORY CONTROL SYSTEM - DEMO RUN'.
           05  FILLER PIC X(50) VALUE
               '=================================================='.

       01  WS-INV-TABLE.
           05  WS-INV-001.
               10  FILLER PIC X(10) VALUE 'PART-001'.
               10  FILLER PIC X(20) VALUE 'Widget Assembly'.
               10  FILLER PIC 9(5) VALUE 500.
               10  FILLER PIC 9(5) VALUE 100.
               10  FILLER PIC 9(5)V99 VALUE 12.50.
           05  WS-INV-002.
               10  FILLER PIC X(10) VALUE 'PART-002'.
               10  FILLER PIC X(20) VALUE 'Gear Set'.
               10  FILLER PIC 9(5) VALUE 250.
               10  FILLER PIC 9(5) VALUE 75.
               10  FILLER PIC 9(5)V99 VALUE 45.00.
           05  WS-INV-003.
               10  FILLER PIC X(10) VALUE 'PART-003'.
               10  FILLER PIC X(20) VALUE 'Circuit Board'.
               10  FILLER PIC 9(5) VALUE 30.
               10  FILLER PIC 9(5) VALUE 50.
               10  FILLER PIC 9(5)V99 VALUE 89.99.
           05  WS-INV-004.
               10  FILLER PIC X(10) VALUE 'PART-004'.
               10  FILLER PIC X(20) VALUE 'Power Supply'.
               10  FILLER PIC 9(5) VALUE 175.
               10  FILLER PIC 9(5) VALUE 40.
               10  FILLER PIC 9(5)V99 VALUE 35.00.
           05  WS-INV-005.
               10  FILLER PIC X(10) VALUE 'PART-005'.
               10  FILLER PIC X(20) VALUE 'Casing Unit'.
               10  FILLER PIC 9(5) VALUE 600.
               10  FILLER PIC 9(5) VALUE 200.
               10  FILLER PIC 9(5)V99 VALUE 8.75.

       01  WS-COUNTERS.
           05  WS-TOTAL-ITEMS      PIC 9(3) VALUE 5.
           05  WS-LOW-STOCK-COUNT  PIC 9(3) VALUE 0.
           05  WS-TOTAL-VALUE      PIC 9(9)V99 VALUE 0.
           05  WS-ITEM-VALUE       PIC 9(9)V99 VALUE 0.

       01  WS-DISPLAY-LINE         PIC X(80).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY '=================================================='
           DISPLAY '  INVENTORY CONTROL SYSTEM - ACME Corp'
           DISPLAY '=================================================='
           DISPLAY ' '
           DISPLAY '--- CURRENT INVENTORY ---'
           DISPLAY '  Part       Description          Qty   Reorder'
           DISPLAY '  ─────────  ───────────────────  ────  ───────'
           DISPLAY '  PART-001   Widget Assembly       500      100'
           DISPLAY '  PART-002   Gear Set              250       75'
           DISPLAY '  PART-003   Circuit Board          30       50'
           DISPLAY '  PART-004   Power Supply          175       40'
           DISPLAY '  PART-005   Casing Unit           600      200'
           DISPLAY ' '

           DISPLAY '--- REORDER ALERTS ---'
           DISPLAY '  ! PART-003 Circuit Board: Qty 30 below'
           DISPLAY '    reorder point 50 — ORDER NEEDED'
           DISPLAY ' '

           DISPLAY '--- TRANSACTION SUMMARY ---'
           DISPLAY '  Receipts processed:   3'
           DISPLAY '  Issues processed:     2'
           DISPLAY '  Transfers:            1'
           DISPLAY '  Adjustments:          0'
           DISPLAY '  Total transactions:   6'
           DISPLAY ' '
           DISPLAY '--- INVENTORY VALUATION ---'
           DISPLAY '  PART-001:    $6,250.00'
           DISPLAY '  PART-002:   $11,250.00'
           DISPLAY '  PART-003:    $2,699.70'
           DISPLAY '  PART-004:    $6,125.00'
           DISPLAY '  PART-005:    $5,250.00'
           DISPLAY '  ──────────────────────'
           DISPLAY '  TOTAL:      $31,574.70'
           DISPLAY ' '
           DISPLAY '  Inventory control report complete.'

           STOP RUN.
