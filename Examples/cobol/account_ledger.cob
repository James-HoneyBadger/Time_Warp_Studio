      *================================================================
      * PROGRAM:    GLPOST01 (Simplified)
      * DESCRIPTION: General Ledger Posting & Trial Balance Demo
      *              Demonstrates COBOL report formatting
      *================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GLPOST01.
       AUTHOR. TIME-WARP-STUDIO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-COUNTERS.
           05  WS-TOTAL-DEBITS     PIC 9(9)V99 VALUE 0.
           05  WS-TOTAL-CREDITS    PIC 9(9)V99 VALUE 0.
           05  WS-NET-INCOME       PIC S9(9)V99 VALUE 0.

       01  WS-DISPLAY         PIC X(80).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY '=================================================='
           DISPLAY '  GENERAL LEDGER POSTING & TRIAL BALANCE'
           DISPLAY '  ACME CORPORATION — Period 2024-01'
           DISPLAY '=================================================='
           DISPLAY ' '

           DISPLAY '--- JOURNAL ENTRIES POSTED ---'
           DISPLAY '  JE-001  Cash              DR    $50,000.00'
           DISPLAY '          Capital Stock      CR    $50,000.00'
           DISPLAY '          (Owner investment)'
           DISPLAY ' '
           DISPLAY '  JE-002  Office Equipment  DR    $12,500.00'
           DISPLAY '          Cash               CR    $12,500.00'
           DISPLAY '          (Equipment purchase)'
           DISPLAY ' '
           DISPLAY '  JE-003  Rent Expense      DR     $3,200.00'
           DISPLAY '          Cash               CR     $3,200.00'
           DISPLAY '          (Monthly rent)'
           DISPLAY ' '
           DISPLAY '  JE-004  Cash              DR    $28,750.00'
           DISPLAY '          Service Revenue    CR    $28,750.00'
           DISPLAY '          (Consulting income)'
           DISPLAY ' '
           DISPLAY '  JE-005  Salary Expense    DR     $8,400.00'
           DISPLAY '          Cash               CR     $8,400.00'
           DISPLAY '          (Employee payroll)'
           DISPLAY ' '

           DISPLAY '--- TRIAL BALANCE ---'
           DISPLAY '  Account              Debit        Credit'
           DISPLAY '  ──────────────────  ───────────  ───────────'
           DISPLAY '  Cash                $54,650.00'
           DISPLAY '  Office Equipment    $12,500.00'
           DISPLAY '  Rent Expense         $3,200.00'
           DISPLAY '  Salary Expense       $8,400.00'
           DISPLAY '  Capital Stock                     $50,000.00'
           DISPLAY '  Service Revenue                   $28,750.00'
           DISPLAY '  ──────────────────  ───────────  ───────────'
           DISPLAY '  TOTALS              $78,750.00   $78,750.00'
           DISPLAY ' '
           DISPLAY '  Trial Balance: IN BALANCE'
           DISPLAY ' '
           DISPLAY '--- INCOME SUMMARY ---'
           DISPLAY '  Revenue:          $28,750.00'
           DISPLAY '  Expenses:         $11,600.00'
           DISPLAY '  Net Income:       $17,150.00'
           DISPLAY ' '
           DISPLAY '  GL posting complete.'

           STOP RUN.
