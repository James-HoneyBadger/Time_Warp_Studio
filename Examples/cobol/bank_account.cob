       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK-ACCOUNT.
       AUTHOR. Time Warp Studio.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ACCOUNT-NUM    PIC 9(6) VALUE 100001.
       01  WS-BALANCE        PIC 9(8)V99 VALUE 1000.00.
       01  WS-DEPOSIT        PIC 9(6)V99 VALUE 0.
       01  WS-WITHDRAW       PIC 9(6)V99 VALUE 0.
       01  WS-INTEREST-RATE  PIC V9(4) VALUE 0.0250.
       01  WS-INTEREST       PIC 9(6)V99 VALUE 0.
       01  WS-NAME           PIC X(20) VALUE 'John Smith'.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY 'Simple Bank Account Simulation'.
           DISPLAY '================================'.
           DISPLAY 'Account: ' WS-ACCOUNT-NUM.
           DISPLAY 'Owner  : ' WS-NAME.
           DISPLAY 'Balance: $' WS-BALANCE.

           MOVE 500.00 TO WS-DEPOSIT.
           ADD WS-DEPOSIT TO WS-BALANCE.
           DISPLAY 'After deposit of $' WS-DEPOSIT ': $' WS-BALANCE.

           MOVE 250.00 TO WS-WITHDRAW.
           SUBTRACT WS-WITHDRAW FROM WS-BALANCE.
           DISPLAY 'After withdrawal of $' WS-WITHDRAW ': $' WS-BALANCE.

           COMPUTE WS-INTEREST = WS-BALANCE * WS-INTEREST-RATE.
           ADD WS-INTEREST TO WS-BALANCE.
           DISPLAY 'Annual interest (2.5%): $' WS-INTEREST.
           DISPLAY 'Final balance: $' WS-BALANCE.

           STOP RUN.
