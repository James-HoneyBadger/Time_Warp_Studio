-- ================================================================
--  ACME CORPORATION ERP — BUSINESS INTELLIGENCE QUERIES
--  Self-contained demo: creates tables, inserts sample data,
--  then runs analytics queries.  SQLite-compatible.
-- ================================================================

-- ──────────────────────────────────────────────────────────────
--  SCHEMA SETUP
-- ──────────────────────────────────────────────────────────────

CREATE TABLE IF NOT EXISTS DEPARTMENT (
    DEPT_ID   INTEGER PRIMARY KEY,
    DEPT_NAME TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS EMPLOYEE (
    EMP_ID      INTEGER PRIMARY KEY,
    FIRST_NAME  TEXT,
    LAST_NAME   TEXT,
    DEPT_ID     INTEGER,
    JOB_TITLE   TEXT,
    HIRE_DATE   TEXT
);

CREATE TABLE IF NOT EXISTS PAYCHECK (
    CHECK_ID    INTEGER PRIMARY KEY,
    EMP_ID      INTEGER,
    PERIOD_DATE TEXT,
    GROSS_PAY   REAL,
    NET_PAY     REAL,
    FEDERAL_TAX REAL,
    STATE_TAX   REAL,
    VOID_FLAG   TEXT DEFAULT 'N'
);

CREATE TABLE IF NOT EXISTS SALES_ORDER (
    ORDER_ID    INTEGER PRIMARY KEY,
    EMP_ID      INTEGER,
    CUST_NAME   TEXT,
    ORDER_DATE  TEXT,
    AMOUNT      REAL,
    STATUS      TEXT
);

CREATE TABLE IF NOT EXISTS CHART_OF_ACCOUNTS (
    ACCT_ID     INTEGER PRIMARY KEY,
    ACCT_NAME   TEXT,
    ACCT_TYPE   TEXT,
    BALANCE     REAL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS INVENTORY (
    ITEM_ID     INTEGER PRIMARY KEY,
    ITEM_NAME   TEXT,
    QTY_ON_HAND INTEGER,
    UNIT_COST   REAL,
    REORDER_PT  INTEGER
);

-- ──────────────────────────────────────────────────────────────
--  SAMPLE DATA
-- ──────────────────────────────────────────────────────────────

DELETE FROM DEPARTMENT;
INSERT INTO DEPARTMENT VALUES (1,'Engineering'),(2,'Sales'),(3,'Finance'),(4,'HR');

DELETE FROM EMPLOYEE;
INSERT INTO EMPLOYEE VALUES
  (101,'Alice','Smith',1,'Engineer','2020-03-15'),
  (102,'Bob','Jones',2,'Sales Rep','2019-07-01'),
  (103,'Carol','Davis',3,'Accountant','2021-01-10'),
  (104,'Dan','Wilson',1,'Senior Engineer','2018-05-20'),
  (105,'Eve','Brown',4,'HR Manager','2017-11-30');

DELETE FROM PAYCHECK;
INSERT INTO PAYCHECK VALUES
  (1,101,'2024-01-15',5000,3800,750,250,'N'),
  (2,102,'2024-01-15',4200,3200,630,210,'N'),
  (3,103,'2024-01-15',4500,3400,675,225,'N'),
  (4,104,'2024-01-15',6500,4900,975,325,'N'),
  (5,105,'2024-01-15',5200,3950,780,260,'N'),
  (6,101,'2024-02-15',5000,3800,750,250,'N'),
  (7,102,'2024-02-15',4800,3650,720,240,'N'),
  (8,104,'2024-02-15',6500,4900,975,325,'N');

DELETE FROM SALES_ORDER;
INSERT INTO SALES_ORDER VALUES
  (1001,102,'Acme LLC','2024-01-05',12500,'CLOSED'),
  (1002,102,'Beta Corp','2024-01-18',8750,'CLOSED'),
  (1003,102,'Gamma Inc','2024-02-03',21000,'CLOSED'),
  (1004,102,'Delta Ltd','2024-02-20',5500,'OPEN'),
  (1005,101,'Acme LLC','2024-03-01',3200,'OPEN');

DELETE FROM CHART_OF_ACCOUNTS;
INSERT INTO CHART_OF_ACCOUNTS VALUES
  (1000,'Cash','ASSET',45000),
  (1100,'Accounts Receivable','ASSET',82000),
  (2000,'Accounts Payable','LIABILITY',31000),
  (3000,'Retained Earnings','EQUITY',215000),
  (4000,'Revenue','REVENUE',380000),
  (5000,'Salaries Expense','EXPENSE',120000),
  (5100,'Rent Expense','EXPENSE',18000);

DELETE FROM INVENTORY;
INSERT INTO INVENTORY VALUES
  (1,'Widget A',150,12.50,50),
  (2,'Widget B',35,28.00,40),
  (3,'Gadget X',200,5.75,100),
  (4,'Gadget Y',12,95.00,25),
  (5,'Component Z',500,1.20,200);

-- ──────────────────────────────────────────────────────────────
--  SECTION 1: PAYROLL ANALYTICS
-- ──────────────────────────────────────────────────────────────

-- 1.1 Payroll summary by department
SELECT
    D.DEPT_NAME                      AS department,
    COUNT(DISTINCT E.EMP_ID)         AS headcount,
    SUM(P.GROSS_PAY)                 AS gross_payroll,
    SUM(P.NET_PAY)                   AS net_payroll,
    SUM(P.FEDERAL_TAX + P.STATE_TAX) AS total_taxes,
    AVG(P.GROSS_PAY)                 AS avg_gross_pay
FROM PAYCHECK P
JOIN EMPLOYEE   E ON E.EMP_ID  = P.EMP_ID
JOIN DEPARTMENT D ON D.DEPT_ID = E.DEPT_ID
WHERE P.VOID_FLAG = 'N'
GROUP BY D.DEPT_ID, D.DEPT_NAME
ORDER BY gross_payroll DESC;

-- 1.2 Year-to-date payroll by employee
SELECT
    E.EMP_ID,
    E.LAST_NAME || ', ' || E.FIRST_NAME AS employee_name,
    E.JOB_TITLE,
    D.DEPT_NAME,
    SUM(P.GROSS_PAY)   AS ytd_gross,
    SUM(P.NET_PAY)     AS ytd_net,
    SUM(P.FEDERAL_TAX) AS ytd_fed_tax,
    COUNT(P.CHECK_ID)  AS pay_periods
FROM PAYCHECK P
JOIN EMPLOYEE   E ON E.EMP_ID  = P.EMP_ID
JOIN DEPARTMENT D ON D.DEPT_ID = E.DEPT_ID
WHERE P.VOID_FLAG = 'N'
GROUP BY E.EMP_ID, E.LAST_NAME, E.FIRST_NAME, E.JOB_TITLE, D.DEPT_NAME
ORDER BY E.LAST_NAME, E.FIRST_NAME;

-- ──────────────────────────────────────────────────────────────
--  SECTION 2: SALES ANALYTICS
-- ──────────────────────────────────────────────────────────────

-- 2.1 Sales by customer
SELECT
    CUST_NAME,
    COUNT(ORDER_ID)      AS order_count,
    SUM(AMOUNT)          AS total_revenue,
    AVG(AMOUNT)          AS avg_order_value,
    MAX(ORDER_DATE)      AS last_order_date
FROM SALES_ORDER
WHERE STATUS = 'CLOSED'
GROUP BY CUST_NAME
ORDER BY total_revenue DESC;

-- 2.2 Sales rep performance
SELECT
    E.FIRST_NAME || ' ' || E.LAST_NAME AS sales_rep,
    COUNT(S.ORDER_ID)    AS orders_closed,
    SUM(S.AMOUNT)        AS total_sales
FROM SALES_ORDER S
JOIN EMPLOYEE E ON E.EMP_ID = S.EMP_ID
WHERE S.STATUS = 'CLOSED'
GROUP BY E.EMP_ID, E.FIRST_NAME, E.LAST_NAME
ORDER BY total_sales DESC;

-- ──────────────────────────────────────────────────────────────
--  SECTION 3: GENERAL LEDGER
-- ──────────────────────────────────────────────────────────────

-- 3.1 Trial balance
SELECT
    ACCT_ID,
    ACCT_NAME,
    ACCT_TYPE,
    BALANCE
FROM CHART_OF_ACCOUNTS
ORDER BY ACCT_ID;

-- 3.2 Balance sheet totals
SELECT
    ACCT_TYPE,
    SUM(BALANCE) AS total_balance
FROM CHART_OF_ACCOUNTS
GROUP BY ACCT_TYPE
ORDER BY ACCT_TYPE;

-- ──────────────────────────────────────────────────────────────
--  SECTION 4: INVENTORY ANALYTICS
-- ──────────────────────────────────────────────────────────────

-- 4.1 Inventory valuation
SELECT
    ITEM_ID,
    ITEM_NAME,
    QTY_ON_HAND,
    UNIT_COST,
    ROUND(QTY_ON_HAND * UNIT_COST, 2) AS total_value,
    CASE WHEN QTY_ON_HAND <= REORDER_PT THEN 'REORDER NOW' ELSE 'OK' END AS status
FROM INVENTORY
ORDER BY total_value DESC;

-- 4.2 Items below reorder point
SELECT
    ITEM_NAME,
    QTY_ON_HAND,
    REORDER_PT,
    REORDER_PT - QTY_ON_HAND AS shortage
FROM INVENTORY
WHERE QTY_ON_HAND <= REORDER_PT
ORDER BY shortage DESC;
