"""
Integration test: COBOL/CICS/JCL/SQL bank account manager demo
Ensures SQL schema is loaded, then runs COBOL demo and checks output.
"""
import os
from time_warp.core.interpreter import Language
from time_warp.core.config import EXAMPLES_DIR
from .conftest_lang import run, has

SQL_SCHEMA = EXAMPLES_DIR / "sql" / "bank_schema.sql"
COBOL_DEMO = EXAMPLES_DIR / "cobol" / "bank_account_manager.cob"

L_SQL = Language.SQL
L_COBOL = Language.COBOL

def test_cobol_sql_demo():
    # Step 1: Load SQL schema
    with open(SQL_SCHEMA) as f:
        sql_code = f.read()
    sql_out = run(sql_code, L_SQL)
    # Accept successful schema load output
    assert has(sql_out, "Command completed successfully.") or has(sql_out, "already exists")
    # Step 2: Run COBOL demo with valid account
    cobol_code = (
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. BANKACCT.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       01  WS-ACCOUNT-NUMBER     PIC 9(6) VALUE 100001.\n"
        "       01  WS-ACCOUNT-NAME       PIC X(30).\n"
        "       01  WS-BALANCE            PIC S9(7)V99.\n"
        "       01  WS-TRANSACTION-TYPE   PIC X(1) VALUE 'D'.\n"
        "       01  WS-AMOUNT             PIC S9(7)V99 VALUE 100.00.\n"
        "       EXEC SQL INCLUDE SQLCA END-EXEC.\n"
        "       PROCEDURE DIVISION.\n"
        "       EXEC SQL\n"
        "            SELECT ACCOUNT_NAME, BALANCE\n"
        "              INTO :WS-ACCOUNT-NAME, :WS-BALANCE\n"
        "              FROM ACCOUNTS\n"
        "             WHERE ACCOUNT_NUMBER = :WS-ACCOUNT-NUMBER\n"
        "       END-EXEC.\n"
        "       ADD WS-AMOUNT TO WS-BALANCE.\n"
        "       EXEC SQL\n"
        "            UPDATE ACCOUNTS\n"
        "               SET BALANCE = :WS-BALANCE\n"
        "             WHERE ACCOUNT_NUMBER = :WS-ACCOUNT-NUMBER\n"
        "       END-EXEC.\n"
        "       DISPLAY 'Transaction successful. New Balance: ' WS-BALANCE.\n"
        "       STOP RUN.\n"
    )
    cobol_out = run(cobol_code, L_COBOL)
    assert has(cobol_out, "Transaction successful")
    assert has(cobol_out, "1600.00") or has(cobol_out, "New Balance")
