10  REM ================================================
20  REM  BUDGET TRACKER - Personal Finance Manager
30  REM  A genuinely useful personal finance tool.
40  REM  Track income, expenses, and view reports.
50  REM ================================================
60  REM -- Initialize arrays for budget data --
70  DIM CAT$(20)
80  DIM AMOUNT(20)
90  DIM TRANSTYPE$(20)
100 TRANS_COUNT = 0
110 TOTAL_INCOME = 0
120 TOTAL_EXPENSES = 0
130 REM
140 PRINT "╔══════════════════════════════════╗"
150 PRINT "║      BASIC BUDGET TRACKER        ║"
160 PRINT "║   Personal Finance Manager       ║"
170 PRINT "╚══════════════════════════════════╝"
180 PRINT ""
190 REM -- Load sample data --
200 GOSUB 8000
210 REM -- Main Menu Loop --
220 DONE = 0
230 WHILE DONE = 0
240   PRINT ""
250   PRINT "═══════════════════════════"
260   PRINT " MAIN MENU"
270   PRINT "═══════════════════════════"
280   PRINT " 1. Add Income"
290   PRINT " 2. Add Expense"
300   PRINT " 3. View All Transactions"
310   PRINT " 4. View Summary Report"
320   PRINT " 5. View by Category"
330   PRINT " 6. Clear All Data"
340   PRINT " 7. Exit"
350   PRINT "═══════════════════════════"
360   PRINT "Choice: ";
370   INPUT CHOICE
380   IF CHOICE = 1 THEN GOSUB 1000
390   IF CHOICE = 2 THEN GOSUB 2000
400   IF CHOICE = 3 THEN GOSUB 3000
410   IF CHOICE = 4 THEN GOSUB 4000
420   IF CHOICE = 5 THEN GOSUB 5000
430   IF CHOICE = 6 THEN GOSUB 6000
440   IF CHOICE = 7 THEN DONE = 1
450 WEND
460 PRINT ""
470 PRINT "Thank you for using Budget Tracker!"
480 END

1000 REM =============================
1010 REM  ADD INCOME
1020 REM =============================
1030 PRINT ""
1040 PRINT "--- ADD INCOME ---"
1050 PRINT "Category (e.g. Salary, Freelance): ";
1060 INPUT CAT$
1070 PRINT "Amount: $";
1080 INPUT AMT
1090 IF AMT <= 0 THEN PRINT "Amount must be positive." : RETURN
1100 TRANS_COUNT = TRANS_COUNT + 1
1110 CAT$(TRANS_COUNT) = CAT$
1120 AMOUNT(TRANS_COUNT) = AMT
1130 TRANSTYPE$(TRANS_COUNT) = "INCOME"
1140 TOTAL_INCOME = TOTAL_INCOME + AMT
1150 PRINT "✓ Added income: "; CAT$; " $"; AMT
1160 RETURN

2000 REM =============================
2010 REM  ADD EXPENSE
2020 REM =============================
2030 PRINT ""
2040 PRINT "--- ADD EXPENSE ---"
2050 PRINT "Category (e.g. Rent, Food, Gas): ";
2060 INPUT CAT$
2070 PRINT "Amount: $";
2080 INPUT AMT
2090 IF AMT <= 0 THEN PRINT "Amount must be positive." : RETURN
2100 TRANS_COUNT = TRANS_COUNT + 1
2110 CAT$(TRANS_COUNT) = CAT$
2120 AMOUNT(TRANS_COUNT) = AMT
2130 TRANSTYPE$(TRANS_COUNT) = "EXPENSE"
2140 TOTAL_EXPENSES = TOTAL_EXPENSES + AMT
2150 PRINT "✓ Added expense: "; CAT$; " $"; AMT
2160 RETURN

3000 REM =============================
3010 REM  VIEW ALL TRANSACTIONS
3020 REM =============================
3030 PRINT ""
3040 PRINT "═══════════════════════════════════════"
3050 PRINT "  # TYPE       CATEGORY        AMOUNT"
3060 PRINT "───────────────────────────────────────"
3070 IF TRANS_COUNT = 0 THEN PRINT "  No transactions recorded." : RETURN
3080 FOR I = 1 TO TRANS_COUNT
3090   PRINT "  "; I; "  "; TRANSTYPE$(I); "   "; CAT$(I); "    $"; AMOUNT(I)
3100 NEXT I
3110 PRINT "═══════════════════════════════════════"
3120 RETURN

4000 REM =============================
4010 REM  SUMMARY REPORT
4020 REM =============================
4030 PRINT ""
4040 PRINT "╔═══════════════════════════════════╗"
4050 PRINT "║        FINANCIAL SUMMARY          ║"
4060 PRINT "╚═══════════════════════════════════╝"
4070 PRINT ""
4080 PRINT "  Total Income:    $"; TOTAL_INCOME
4090 PRINT "  Total Expenses:  $"; TOTAL_EXPENSES
4100 BALANCE = TOTAL_INCOME - TOTAL_EXPENSES
4110 PRINT "  ─────────────────────────────"
4120 PRINT "  Net Balance:     $"; BALANCE
4130 PRINT ""
4140 IF BALANCE > 0 THEN PRINT "  ✓ You are AHEAD by $"; BALANCE
4150 IF BALANCE < 0 THEN PRINT "  ✗ You are SHORT by $"; ABS(BALANCE)
4160 IF BALANCE = 0 THEN PRINT "  = You are exactly EVEN"
4170 PRINT ""
4180 REM Calculate savings rate
4190 IF TOTAL_INCOME > 0 THEN SAVINGS_RATE = (BALANCE / TOTAL_INCOME) * 100
4200 IF TOTAL_INCOME > 0 THEN PRINT "  Savings Rate:"; INT(SAVINGS_RATE); "%"
4210 PRINT ""
4220 PRINT "  Total Transactions:"; TRANS_COUNT
4230 RETURN

5000 REM =============================
5010 REM  VIEW BY CATEGORY
5020 REM =============================
5030 PRINT ""
5040 PRINT "Enter category to search: ";
5050 INPUT SEARCH$
5060 PRINT ""
5070 PRINT "Transactions for: "; SEARCH$
5080 PRINT "─────────────────────────────"
5090 FOUND = 0
5100 CAT_TOTAL = 0
5110 FOR I = 1 TO TRANS_COUNT
5120   IF CAT$(I) = SEARCH$ THEN FOUND = 1 : PRINT TRANSTYPE$(I); " $"; AMOUNT(I) : CAT_TOTAL = CAT_TOTAL + AMOUNT(I)
5130 NEXT I
5140 IF FOUND = 0 THEN PRINT "No transactions found for that category."
5150 IF FOUND = 1 THEN PRINT "Category Total: $"; CAT_TOTAL
5160 RETURN

6000 REM =============================
6010 REM  CLEAR ALL DATA
6020 REM =============================
6030 PRINT "Are you sure? (Y/N): ";
6040 INPUT CONFIRM$
6050 IF CONFIRM$ <> "Y" THEN PRINT "Cancelled." : RETURN
6060 TRANS_COUNT = 0
6070 TOTAL_INCOME = 0
6080 TOTAL_EXPENSES = 0
6090 PRINT "All data cleared."
6100 RETURN

8000 REM =============================
8010 REM  LOAD SAMPLE DATA
8020 REM =============================
8030 PRINT "Loading sample data..."
8040 REM Sample income
8050 TRANS_COUNT = 1 : CAT$(1) = "Salary" : AMOUNT(1) = 3500 : TRANSTYPE$(1) = "INCOME" : TOTAL_INCOME = 3500
8060 TRANS_COUNT = 2 : CAT$(2) = "Freelance" : AMOUNT(2) = 450 : TRANSTYPE$(2) = "INCOME" : TOTAL_INCOME = 3950
8070 REM Sample expenses
8080 TRANS_COUNT = 3 : CAT$(3) = "Rent" : AMOUNT(3) = 1200 : TRANSTYPE$(3) = "EXPENSE" : TOTAL_EXPENSES = 1200
8090 TRANS_COUNT = 4 : CAT$(4) = "Food" : AMOUNT(4) = 380 : TRANSTYPE$(4) = "EXPENSE" : TOTAL_EXPENSES = 1580
8100 TRANS_COUNT = 5 : CAT$(5) = "Gas" : AMOUNT(5) = 95 : TRANSTYPE$(5) = "EXPENSE" : TOTAL_EXPENSES = 1675
8110 TRANS_COUNT = 6 : CAT$(6) = "Utilities" : AMOUNT(6) = 120 : TRANSTYPE$(6) = "EXPENSE" : TOTAL_EXPENSES = 1795
8120 TRANS_COUNT = 7 : CAT$(7) = "Food" : AMOUNT(7) = 65 : TRANSTYPE$(7) = "EXPENSE" : TOTAL_EXPENSES = 1860
8130 PRINT "7 sample transactions loaded."
8140 RETURN
