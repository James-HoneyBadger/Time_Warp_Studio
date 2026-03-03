      *================================================================
      * ACME CORPORATION — PAYROLL PROCESSING SYSTEM
      * Program: PAYPRO01
      * Author:  ACME Data Center
      * Date:    2025-12-26
      * Purpose: Read EMPLOYEE and TIMECARD tables, compute gross
      *          pay, all deductions, and net pay. Write PAYCHECK
      *          records and GL journal entries.
      *
      * Input:   WS-PERIOD-ID (pay period from JCL PARM or SYSIN)
      * Output:  PAYCHECK inserts, GL journal, PRNTFILE report
      *================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYPRO01.
       AUTHOR. ACME-DATACENTER.
       DATE-WRITTEN. 2025-12-26.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZOS.
       OBJECT-COMPUTER. IBM-ZOS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRNTFILE ASSIGN TO UT-S-PRNTFILE
                           ORGANIZATION IS SEQUENTIAL
                           ACCESS MODE IS SEQUENTIAL.
           SELECT ERRFILE  ASSIGN TO UT-S-ERRFILE
                           ORGANIZATION IS SEQUENTIAL
                           ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PRNTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  PRINT-RECORD           PIC X(133).

       FD  ERRFILE
           RECORDING MODE IS F
           RECORD CONTAINS 200 CHARACTERS.
       01  ERR-RECORD             PIC X(200).

       WORKING-STORAGE SECTION.
      *--- Program Control
       01  WS-PROGRAM-NAME        PIC X(8)   VALUE 'PAYPRO01'.
       01  WS-PERIOD-ID           PIC X(8)   VALUE '2025122BI'.
       01  WS-RUN-DATE            PIC X(10)  VALUE SPACES.
       01  WS-PROCESS-STATUS      PIC X(1)   VALUE 'G'.
           88  PROCESS-GOOD                  VALUE 'G'.
           88  PROCESS-ERROR                 VALUE 'E'.

      *--- Counters & Accumulators
       01  WS-COUNTERS.
           05  WS-EMPS-READ       PIC 9(6)   VALUE ZEROS.
           05  WS-EMPS-PAID       PIC 9(6)   VALUE ZEROS.
           05  WS-EMPS-SKIPPED    PIC 9(6)   VALUE ZEROS.
           05  WS-TOTAL-GROSS     PIC 9(12)V99  VALUE ZEROS.
           05  WS-TOTAL-FED-TAX   PIC 9(10)V99  VALUE ZEROS.
           05  WS-TOTAL-STATE-TAX PIC 9(10)V99  VALUE ZEROS.
           05  WS-TOTAL-FICA      PIC 9(10)V99  VALUE ZEROS.
           05  WS-TOTAL-MEDICARE  PIC 9(10)V99  VALUE ZEROS.
           05  WS-TOTAL-BENEFITS  PIC 9(10)V99  VALUE ZEROS.
           05  WS-TOTAL-NET       PIC 9(12)V99  VALUE ZEROS.

      *--- Employee Work Area (mirrors EMPLOYEE table)
       01  WS-EMPLOYEE.
           05  WS-EMP-ID          PIC X(8).
           05  WS-EMP-LAST        PIC X(30).
           05  WS-EMP-FIRST       PIC X(20).
           05  WS-DEPT-ID         PIC X(4).
           05  WS-JOB-CODE        PIC X(6).
           05  WS-PAY-TYPE        PIC X(1).
               88  SALARIED                  VALUE 'S'.
               88  HOURLY                    VALUE 'H'.
           05  WS-PAY-RATE        PIC 9(8)V99.
           05  WS-STD-HOURS       PIC 99V99  VALUE 40.00.
           05  WS-FED-EXEMPT      PIC 9.
           05  WS-STATE-EXEMPT    PIC 9.
           05  WS-MARITAL-STATUS  PIC X.

      *--- Timecard Totals (aggregated from TIMECARD)
       01  WS-TIMECARD-TOTALS.
           05  WS-REG-HOURS       PIC 99V99  VALUE ZEROS.
           05  WS-OT-HOURS        PIC 99V99  VALUE ZEROS.
           05  WS-SICK-HOURS      PIC 99V99  VALUE ZEROS.
           05  WS-VAC-HOURS       PIC 99V99  VALUE ZEROS.

      *--- Pay Calculation Work Fields
       01  WS-PAY-CALC.
           05  WS-PERIOD-RATE     PIC 9(8)V99.
           05  WS-HOURLY-RATE     PIC 9(6)V99.
           05  WS-OT-RATE         PIC 9(6)V99.
           05  WS-REG-EARNINGS    PIC 9(8)V99.
           05  WS-OT-EARNINGS     PIC 9(8)V99.
           05  WS-GROSS-PAY       PIC 9(8)V99.
           05  WS-FEDERAL-TAX     PIC 9(6)V99.
           05  WS-STATE-TAX       PIC 9(6)V99.
           05  WS-SOC-SEC-TAX     PIC 9(6)V99.
           05  WS-MEDICARE-TAX    PIC 9(6)V99.
           05  WS-HEALTH-DED      PIC 9(6)V99.
           05  WS-DENTAL-DED      PIC 9(6)V99.
           05  WS-RETIRE-DED      PIC 9(6)V99.
           05  WS-TOTAL-DEDS      PIC 9(8)V99.
           05  WS-NET-PAY         PIC 9(8)V99.

      *--- Tax Rate Constants
       01  WS-TAX-RATES.
           05  WS-SOC-SEC-RATE    PIC V9999  VALUE .0620.
           05  WS-MEDICARE-RATE   PIC V9999  VALUE .0145.
           05  WS-SOC-SEC-WAGE-BASE PIC 9(7)V99 VALUE 168600.00.

      *--- Standard Deduction Rates (benefit elections)
       01  WS-BENEFIT-RATES.
           05  WS-HLTH-BI-WEEKLY  PIC 999V99 VALUE 125.00.
           05  WS-DENT-BI-WEEKLY  PIC 99V99  VALUE  18.50.
           05  WS-RETIRE-PCT      PIC V999   VALUE .040.

      *--- Check Number Seed
       01  WS-CHECK-SEED          PIC 9(8)   VALUE 10050001.
       01  WS-CHECK-NO            PIC X(12).

      *--- GL Journal Fields
       01  WS-JOURNAL-ID          PIC X(12).
       01  WS-GL-LINE-NO          PIC 99     VALUE 0.

      *--- Federal Tax Table (bi-weekly, simplified withholding)
       01  WS-FED-TAX-TABLE.
           05  FILLER             PIC 9(6)V99 VALUE 0.
           05  FILLER             PIC V999    VALUE .100.
           05  FILLER             PIC 9(6)V99 VALUE 1038.46.
           05  FILLER             PIC V999    VALUE .120.
           05  FILLER             PIC 9(6)V99 VALUE 3346.15.
           05  FILLER             PIC V999    VALUE .220.
           05  FILLER             PIC 9(6)V99 VALUE 6423.08.
           05  FILLER             PIC V999    VALUE .240.
           05  FILLER             PIC 9(6)V99 VALUE 12003.85.
           05  FILLER             PIC V999    VALUE .320.
           05  FILLER             PIC 9(6)V99 VALUE 15000.00.
           05  FILLER             PIC V999    VALUE .350.

       01  WS-TAX-TABLE-ENTRY REDEFINES WS-FED-TAX-TABLE
                              OCCURS 6 TIMES.
           05  WS-TTX-BRACKET     PIC 9(6)V99.
           05  WS-TTX-RATE        PIC V999.

      *--- SQL Communication Area
           EXEC SQL INCLUDE SQLCA END-EXEC.

      *--- Embedded SQL Host Variables
           EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  HV-PERIOD-ID           PIC X(8).
       01  HV-EMP-ID              PIC X(8).
       01  HV-LAST-NAME           PIC X(30).
       01  HV-FIRST-NAME          PIC X(20).
       01  HV-DEPT-ID             PIC X(4).
       01  HV-JOB-CODE            PIC X(6).
       01  HV-PAY-TYPE            PIC X(1).
       01  HV-PAY-RATE            PIC 9(8)V99 USAGE COMP-3.
       01  HV-STD-HOURS           PIC 99V99   USAGE COMP-3.
       01  HV-FED-EXEMPT          PIC 9       USAGE COMP.
       01  HV-STATE-EXEMPT        PIC 9       USAGE COMP.
       01  HV-MARITAL-STATUS      PIC X(1).
       01  HV-TOT-REG-HRS         PIC 99V99   USAGE COMP-3.
       01  HV-TOT-OT-HRS          PIC 99V99   USAGE COMP-3.
       01  HV-CHECK-ID            PIC X(12).
       01  HV-CHECK-DATE          PIC X(10).
       01  HV-GROSS-PAY           PIC 9(8)V99 USAGE COMP-3.
       01  HV-FED-TAX             PIC 9(6)V99 USAGE COMP-3.
       01  HV-STATE-TAX           PIC 9(6)V99 USAGE COMP-3.
       01  HV-SOC-SEC             PIC 9(6)V99 USAGE COMP-3.
       01  HV-MEDICARE            PIC 9(6)V99 USAGE COMP-3.
       01  HV-HEALTH-INS          PIC 9(6)V99 USAGE COMP-3.
       01  HV-DENTAL-INS          PIC 9(6)V99 USAGE COMP-3.
       01  HV-RETIRE-401K         PIC 9(6)V99 USAGE COMP-3.
       01  HV-NET-PAY             PIC 9(8)V99 USAGE COMP-3.
           EXEC SQL END DECLARE SECTION END-EXEC.

      *--- Print Line Templates
       01  PL-HEADER1.
           05  FILLER PIC X(133) VALUE
           '     ACME CORPORATION — PAYROLL REGISTER          PAGE __'.
       01  PL-HEADER2.
           05  FILLER PIC X(133) VALUE
           '  EMP ID   EMPLOYEE NAME                  DEPT  GROSS PAY'
           '    FED TAX  STATE TAX    FICA    NET PAY'.
       01  PL-DETAIL.
           05  FILLER         PIC X(2)   VALUE SPACES.
           05  PL-D-EMPID     PIC X(8).
           05  FILLER         PIC X(2)   VALUE SPACES.
           05  PL-D-NAME      PIC X(30).
           05  FILLER         PIC X(2)   VALUE SPACES.
           05  PL-D-DEPT      PIC X(4).
           05  FILLER         PIC X(2)   VALUE SPACES.
           05  PL-D-GROSS     PIC ZZZ,ZZ9.99.
           05  FILLER         PIC X(2)   VALUE SPACES.
           05  PL-D-FED       PIC ZZ,ZZ9.99.
           05  FILLER         PIC X(2)   VALUE SPACES.
           05  PL-D-STATE     PIC ZZ,ZZ9.99.
           05  FILLER         PIC X(2)   VALUE SPACES.
           05  PL-D-FICA      PIC ZZ,ZZ9.99.
           05  FILLER         PIC X(2)   VALUE SPACES.
           05  PL-D-NET       PIC ZZZ,ZZ9.99.

       01  PL-TOTALS.
           05  FILLER         PIC X(44) VALUE 'TOTALS:'.
           05  PT-GROSS       PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER         PIC X(2)  VALUE SPACES.
           05  PT-FED         PIC ZZZ,ZZ9.99.
           05  FILLER         PIC X(2)  VALUE SPACES.
           05  PT-STATE       PIC ZZZ,ZZ9.99.
           05  FILLER         PIC X(2)  VALUE SPACES.
           05  PT-FICA        PIC ZZZ,ZZ9.99.
           05  FILLER         PIC X(2)  VALUE SPACES.
           05  PT-NET         PIC ZZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
      *================================================================
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-EMPLOYEES
           PERFORM 3000-POST-GL-JOURNAL
           PERFORM 9000-FINALIZE
           STOP RUN.

      *================================================================
       1000-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE(1:10) TO WS-RUN-DATE

           MOVE WS-PERIOD-ID  TO HV-PERIOD-ID

      *    Open cursor for all active employees
           EXEC SQL
               DECLARE EMP-CURSOR CURSOR FOR
               SELECT E.EMP_ID, E.LAST_NAME, E.FIRST_NAME,
                      E.DEPT_ID, E.JOB_CODE, E.PAY_TYPE,
                      E.PAY_RATE, E.STANDARD_HOURS,
                      E.FEDERAL_EXEMPT, E.STATE_EXEMPT,
                      E.MARITAL_STATUS
               FROM   EMPLOYEE E
               WHERE  E.ACTIVE_FLAG = 'Y'
               ORDER BY E.DEPT_ID, E.LAST_NAME
           END-EXEC

           EXEC SQL OPEN EMP-CURSOR END-EXEC
           IF SQLCODE NOT = 0
               MOVE 'E' TO WS-PROCESS-STATUS
               DISPLAY 'PAYPRO01 ERROR: Cannot open EMP-CURSOR'
                   ' SQLCODE=' SQLCODE
               STOP RUN
           END-IF

           OPEN OUTPUT PRNTFILE
           WRITE PRINT-RECORD FROM PL-HEADER1
           WRITE PRINT-RECORD FROM PL-HEADER2.

      *================================================================
       2000-PROCESS-EMPLOYEES.
           PERFORM UNTIL SQLCODE = 100
               EXEC SQL
                   FETCH EMP-CURSOR
                   INTO :HV-EMP-ID, :HV-LAST-NAME, :HV-FIRST-NAME,
                        :HV-DEPT-ID, :HV-JOB-CODE, :HV-PAY-TYPE,
                        :HV-PAY-RATE, :HV-STD-HOURS,
                        :HV-FED-EXEMPT, :HV-STATE-EXEMPT,
                        :HV-MARITAL-STATUS
               END-EXEC

               IF SQLCODE = 0
                   MOVE HV-EMP-ID         TO WS-EMP-ID
                   MOVE HV-LAST-NAME      TO WS-EMP-LAST
                   MOVE HV-FIRST-NAME     TO WS-EMP-FIRST
                   MOVE HV-DEPT-ID        TO WS-DEPT-ID
                   MOVE HV-PAY-TYPE       TO WS-PAY-TYPE
                   MOVE HV-PAY-RATE       TO WS-PAY-RATE
                   MOVE HV-STD-HOURS      TO WS-STD-HOURS
                   MOVE HV-FED-EXEMPT     TO WS-FED-EXEMPT
                   MOVE HV-STATE-EXEMPT   TO WS-STATE-EXEMPT
                   MOVE HV-MARITAL-STATUS TO WS-MARITAL-STATUS

                   ADD 1 TO WS-EMPS-READ

                   PERFORM 2100-GET-TIMECARD-TOTALS
                   PERFORM 2200-CALCULATE-GROSS
                   PERFORM 2300-CALCULATE-TAXES
                   PERFORM 2400-CALCULATE-DEDUCTIONS
                   PERFORM 2500-COMPUTE-NET
                   PERFORM 2600-WRITE-PAYCHECK
                   PERFORM 2700-PRINT-PAY-LINE
               END-IF
           END-PERFORM

           EXEC SQL CLOSE EMP-CURSOR END-EXEC.

      *================================================================
       2100-GET-TIMECARD-TOTALS.
      *    For hourly employees, sum actual hours from TIMECARD.
      *    For salaried, use standard hours.
           IF SALARIED
               MOVE WS-STD-HOURS TO WS-REG-HOURS
               MOVE ZEROS        TO WS-OT-HOURS
           ELSE
               EXEC SQL
                   SELECT COALESCE(SUM(T.REG_HOURS), 0),
                          COALESCE(SUM(T.OT_HOURS),  0)
                   INTO   :HV-TOT-REG-HRS, :HV-TOT-OT-HRS
                   FROM   TIMECARD T
                   WHERE  T.EMP_ID = :HV-EMP-ID
                   AND    T.PERIOD_ID = :HV-PERIOD-ID
               END-EXEC
               MOVE HV-TOT-REG-HRS TO WS-REG-HOURS
               MOVE HV-TOT-OT-HRS  TO WS-OT-HOURS
           END-IF.

      *================================================================
       2200-CALCULATE-GROSS.
           IF SALARIED
      *        Bi-weekly salary = annual / 26
               DIVIDE 26 INTO WS-PAY-RATE
                   GIVING WS-PERIOD-RATE ROUNDED
               MOVE WS-PERIOD-RATE TO WS-REG-EARNINGS
               MOVE ZEROS          TO WS-OT-EARNINGS
           ELSE
      *        Hourly: reg + OT at 1.5x
               MOVE WS-PAY-RATE TO WS-HOURLY-RATE
               MULTIPLY WS-HOURLY-RATE BY 1.5
                   GIVING WS-OT-RATE ROUNDED
               MULTIPLY WS-HOURLY-RATE BY WS-REG-HOURS
                   GIVING WS-REG-EARNINGS ROUNDED
               MULTIPLY WS-OT-RATE BY WS-OT-HOURS
                   GIVING WS-OT-EARNINGS ROUNDED
           END-IF

           ADD WS-REG-EARNINGS WS-OT-EARNINGS
               GIVING WS-GROSS-PAY.

      *================================================================
       2300-CALCULATE-TAXES.
      *    Federal withholding — simplified percentage method
           EVALUATE TRUE
               WHEN WS-GROSS-PAY < 519.23
                   COMPUTE WS-FEDERAL-TAX ROUNDED =
                       WS-GROSS-PAY * .10
               WHEN WS-GROSS-PAY < 1673.08
                   COMPUTE WS-FEDERAL-TAX ROUNDED =
                       (WS-GROSS-PAY - 519.23) * .12 + 51.92
               WHEN WS-GROSS-PAY < 3211.54
                   COMPUTE WS-FEDERAL-TAX ROUNDED =
                       (WS-GROSS-PAY - 1673.08) * .22 + 190.45
               WHEN OTHER
                   COMPUTE WS-FEDERAL-TAX ROUNDED =
                       (WS-GROSS-PAY - 3211.54) * .24 + 528.87
           END-EVALUATE

      *    Adjust for exemptions (each exemption = ~$175.96 bi-weekly)
           IF WS-FED-EXEMPT > 0
               COMPUTE WS-FEDERAL-TAX ROUNDED =
                   WS-FEDERAL-TAX - (WS-FED-EXEMPT * 175.96)
               IF WS-FEDERAL-TAX < ZEROS
                   MOVE ZEROS TO WS-FEDERAL-TAX
               END-IF
           END-IF

      *    State tax (Illinois flat rate 4.95%)
           COMPUTE WS-STATE-TAX ROUNDED = WS-GROSS-PAY * .0495
           IF WS-STATE-EXEMPT > 0
               COMPUTE WS-STATE-TAX ROUNDED =
                   WS-STATE-TAX - (WS-STATE-EXEMPT * 75.00)
               IF WS-STATE-TAX < ZEROS
                   MOVE ZEROS TO WS-STATE-TAX
               END-IF
           END-IF

      *    Social Security and Medicare (FICA)
           COMPUTE WS-SOC-SEC-TAX ROUNDED =
               WS-GROSS-PAY * WS-SOC-SEC-RATE
           COMPUTE WS-MEDICARE-TAX ROUNDED =
               WS-GROSS-PAY * WS-MEDICARE-RATE.

      *================================================================
       2400-CALCULATE-DEDUCTIONS.
           MOVE WS-HLTH-BI-WEEKLY    TO WS-HEALTH-DED
           MOVE WS-DENT-BI-WEEKLY    TO WS-DENTAL-DED
           COMPUTE WS-RETIRE-DED ROUNDED =
               WS-GROSS-PAY * WS-RETIRE-PCT.

      *================================================================
       2500-COMPUTE-NET.
           ADD WS-FEDERAL-TAX WS-STATE-TAX WS-SOC-SEC-TAX
               WS-MEDICARE-TAX WS-HEALTH-DED WS-DENTAL-DED
               WS-RETIRE-DED
               GIVING WS-TOTAL-DEDS

           SUBTRACT WS-TOTAL-DEDS FROM WS-GROSS-PAY
               GIVING WS-NET-PAY

      *    Accumulate totals
           ADD WS-GROSS-PAY    TO WS-TOTAL-GROSS
           ADD WS-FEDERAL-TAX  TO WS-TOTAL-FED-TAX
           ADD WS-STATE-TAX    TO WS-TOTAL-STATE-TAX
           ADD WS-SOC-SEC-TAX  TO WS-TOTAL-FICA
           ADD WS-MEDICARE-TAX TO WS-TOTAL-FICA
           ADD WS-HEALTH-DED   TO WS-TOTAL-BENEFITS
           ADD WS-DENTAL-DED   TO WS-TOTAL-BENEFITS
           ADD WS-NET-PAY      TO WS-TOTAL-NET
           ADD 1               TO WS-EMPS-PAID.

      *================================================================
       2600-WRITE-PAYCHECK.
      *    Build check number: YYYYMMDD + sequence
           ADD 1 TO WS-CHECK-SEED
           STRING WS-PERIOD-ID(1:8) WS-CHECK-SEED(5:4)
               DELIMITED SIZE INTO WS-CHECK-NO

           MOVE WS-CHECK-NO      TO HV-CHECK-ID
           MOVE WS-RUN-DATE      TO HV-CHECK-DATE
           MOVE WS-GROSS-PAY     TO HV-GROSS-PAY
           MOVE WS-FEDERAL-TAX   TO HV-FED-TAX
           MOVE WS-STATE-TAX     TO HV-STATE-TAX
           MOVE WS-SOC-SEC-TAX   TO HV-SOC-SEC
           MOVE WS-MEDICARE-TAX  TO HV-MEDICARE
           MOVE WS-HEALTH-DED    TO HV-HEALTH-INS
           MOVE WS-DENTAL-DED    TO HV-DENTAL-INS
           MOVE WS-RETIRE-DED    TO HV-RETIRE-401K
           MOVE WS-NET-PAY       TO HV-NET-PAY

           EXEC SQL
               INSERT INTO PAYCHECK (
                   CHECK_ID, EMP_ID, PERIOD_ID, CHECK_DATE,
                   GROSS_PAY, FED_TAX, STATE_TAX, LOCAL_TAX,
                   SOC_SEC, MEDICARE, HEALTH_INS, DENTAL_INS,
                   RETIREMENT_401K, OTHER_DED, NET_PAY,
                   VOID_FLAG, POSTED_FLAG
               ) VALUES (
                   :HV-CHECK-ID, :HV-EMP-ID, :HV-PERIOD-ID,
                   :HV-CHECK-DATE,
                   :HV-GROSS-PAY, :HV-FED-TAX, :HV-STATE-TAX, 0,
                   :HV-SOC-SEC, :HV-MEDICARE, :HV-HEALTH-INS,
                   :HV-DENTAL-INS, :HV-RETIRE-401K, 0, :HV-NET-PAY,
                   'N', 'N'
               )
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'PAYCHECK INSERT ERROR, EMP=' WS-EMP-ID
                   ' SQLCODE=' SQLCODE
           END-IF.

      *================================================================
       2700-PRINT-PAY-LINE.
           MOVE WS-EMP-ID             TO PL-D-EMPID
           STRING WS-EMP-LAST ', ' WS-EMP-FIRST(1:10)
               DELIMITED SIZE INTO PL-D-NAME
           MOVE WS-DEPT-ID            TO PL-D-DEPT
           MOVE WS-GROSS-PAY          TO PL-D-GROSS
           MOVE WS-FEDERAL-TAX        TO PL-D-FED
           MOVE WS-STATE-TAX          TO PL-D-STATE
           MOVE WS-SOC-SEC-TAX        TO PL-D-FICA
           MOVE WS-NET-PAY            TO PL-D-NET
           WRITE PRINT-RECORD FROM PL-DETAIL.

      *================================================================
       3000-POST-GL-JOURNAL.
      *    Create a GL journal: Dr Salary Expense, Cr all liability accounts
           MOVE '202512PAYP01' TO WS-JOURNAL-ID
           EXEC SQL
               INSERT INTO GL_JOURNAL (
                   JOURNAL_ID, JOURNAL_DATE, FISCAL_YEAR,
                   FISCAL_PERIOD, DESCRIPTION, JOURNAL_TYPE,
                   SOURCE_DOC, POSTED_FLAG, CREATED_BY
               ) VALUES (
                   :WS-JOURNAL-ID, CURRENT DATE,
                   2025, 12,
                   'BI-WEEKLY PAYROLL 2025-12-26',
                   'PAYP', :HV-PERIOD-ID, 'N', 'PAYPRO01'
               )
           END-EXEC

      *    Line 1: Dr Salaries & Wages (6100)
           EXEC SQL
               INSERT INTO GL_JOURNAL_LINE VALUES (
                   :WS-JOURNAL-ID, 1, '6100', 'D001',
                   'GROSS PAYROLL',
                   :WS-TOTAL-GROSS, 0.00, :HV-PERIOD-ID
               )
           END-EXEC

      *    Line 2: Cr Cash-Payroll (1020) for net
           EXEC SQL
               INSERT INTO GL_JOURNAL_LINE VALUES (
                   :WS-JOURNAL-ID, 2, '1020', NULL,
                   'NET PAYROLL DISBURSEMENT',
                   0.00, :WS-TOTAL-NET, :HV-PERIOD-ID
               )
           END-EXEC

      *    Line 3: Cr Federal Tax Withheld (2300)
           EXEC SQL
               INSERT INTO GL_JOURNAL_LINE VALUES (
                   :WS-JOURNAL-ID, 3, '2300', NULL,
                   'FEDERAL TAX LIABILITY',
                   0.00, :WS-TOTAL-FED-TAX, :HV-PERIOD-ID
               )
           END-EXEC

      *    Line 4: Cr State Tax Withheld (2310)
           EXEC SQL
               INSERT INTO GL_JOURNAL_LINE VALUES (
                   :WS-JOURNAL-ID, 4, '2310', NULL,
                   'STATE TAX LIABILITY',
                   0.00, :WS-TOTAL-STATE-TAX, :HV-PERIOD-ID
               )
           END-EXEC

      *    Line 5: Cr FICA Payable (2320)
           EXEC SQL
               INSERT INTO GL_JOURNAL_LINE VALUES (
                   :WS-JOURNAL-ID, 5, '2320', NULL,
                   'FICA/MEDICARE LIABILITY',
                   0.00, :WS-TOTAL-FICA, :HV-PERIOD-ID
               )
           END-EXEC.

      *================================================================
       9000-FINALIZE.
           MOVE WS-TOTAL-GROSS    TO PT-GROSS
           MOVE WS-TOTAL-FED-TAX  TO PT-FED
           MOVE WS-TOTAL-STATE-TAX TO PT-STATE
           ADD WS-TOTAL-FICA WS-TOTAL-FICA GIVING WS-COUNTERS
           MOVE WS-TOTAL-FICA     TO PT-FICA
           MOVE WS-TOTAL-NET      TO PT-NET
           WRITE PRINT-RECORD FROM PL-TOTALS

           EXEC SQL COMMIT END-EXEC

           CLOSE PRNTFILE

           DISPLAY 'PAYPRO01 COMPLETE'
           DISPLAY '  EMPLOYEES READ:  ' WS-EMPS-READ
           DISPLAY '  EMPLOYEES PAID:  ' WS-EMPS-PAID
           DISPLAY '  TOTAL GROSS:    $' WS-TOTAL-GROSS
           DISPLAY '  TOTAL NET PAY:  $' WS-TOTAL-NET
           DISPLAY '  RETURN CODE: 0'.
