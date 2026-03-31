       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKACCT.
       AUTHOR. TIME WARP STUDIO.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ACCOUNT-NUMBER     PIC 9(6).
       01  WS-ACCOUNT-NAME       PIC X(30).
       01  WS-BALANCE            PIC S9(7)V99.
       01  WS-TRANSACTION-TYPE   PIC X(1).
       01  WS-AMOUNT             PIC S9(7)V99.
       01  WS-SQLCODE            PIC S9(4) COMP.
       01  WS-MESSAGE            PIC X(80).
       EXEC SQL INCLUDE SQLCA END-EXEC.
       PROCEDURE DIVISION.
       DISPLAY "*** BANK ACCOUNT MANAGER ***".
       DISPLAY "Enter Account Number: ".
       ACCEPT WS-ACCOUNT-NUMBER.
       EXEC SQL
            SELECT ACCOUNT_NAME, BALANCE
              INTO :WS-ACCOUNT-NAME, :WS-BALANCE
              FROM ACCOUNTS
             WHERE ACCOUNT_NUMBER = :WS-ACCOUNT-NUMBER
       END-EXEC.
       IF SQLCODE NOT = 0
           DISPLAY "Account not found."
           STOP RUN
       END-IF.
       DISPLAY "Account Name: " WS-ACCOUNT-NAME.
       DISPLAY "Current Balance: " WS-BALANCE.
       DISPLAY "Transaction Type (D=Deposit, W=Withdraw): ".
       ACCEPT WS-TRANSACTION-TYPE.
       DISPLAY "Amount: ".
       ACCEPT WS-AMOUNT.
       IF WS-TRANSACTION-TYPE = 'D'
           ADD WS-AMOUNT TO WS-BALANCE
       ELSE
           SUBTRACT WS-AMOUNT FROM WS-BALANCE
       END-IF.
       EXEC SQL
            UPDATE ACCOUNTS
               SET BALANCE = :WS-BALANCE
             WHERE ACCOUNT_NUMBER = :WS-ACCOUNT-NUMBER
       END-EXEC.
       IF SQLCODE = 0
           DISPLAY "Transaction successful. New Balance: " WS-BALANCE
       ELSE
           DISPLAY "Transaction failed."
       END-IF.
       STOP RUN.
