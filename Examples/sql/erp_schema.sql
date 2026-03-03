-- ═══════════════════════════════════════════════════════════════════════
--  ACME CORPORATION ERP SYSTEM — Database Schema
--  ┌─────────────────────────────────────────────────────────────────┐
--  │  Modules: HR/Payroll, Inventory, Accounts Receivable,           │
--  │           General Ledger, Purchase Orders, Sales Orders         │
--  │  Tables: 18 core tables, referential integrity throughout       │
--  │  Compatible with: IBM DB2, Oracle, PostgreSQL, SQL Server       │
--  └─────────────────────────────────────────────────────────────────┘
-- ═══════════════════════════════════════════════════════════════════════

-- ─── MODULE 1: ORGANIZATIONAL STRUCTURE ─────────────────────────────────

CREATE TABLE COMPANY (
    COMPANY_ID      CHAR(4)        NOT NULL,
    COMPANY_NAME    VARCHAR(60)    NOT NULL,
    ADDRESS_LINE1   VARCHAR(50),
    ADDRESS_LINE2   VARCHAR(50),
    CITY            VARCHAR(30),
    STATE_CODE      CHAR(2),
    ZIP_CODE        CHAR(10),
    PHONE           CHAR(15),
    TAX_ID          CHAR(12),
    FISCAL_YEAR_END CHAR(5),        -- MM/DD
    CONSTRAINT PK_COMPANY PRIMARY KEY (COMPANY_ID)
);

CREATE TABLE DEPARTMENT (
    DEPT_ID         CHAR(4)        NOT NULL,
    COMPANY_ID      CHAR(4)        NOT NULL,
    DEPT_NAME       VARCHAR(40)    NOT NULL,
    DEPT_CODE       CHAR(6)        NOT NULL,
    MANAGER_EMP_ID  CHAR(8),
    COST_CENTER     CHAR(6),
    GL_ACCOUNT      CHAR(10),
    ACTIVE_FLAG     CHAR(1)        DEFAULT 'Y',
    CONSTRAINT PK_DEPT PRIMARY KEY (DEPT_ID),
    CONSTRAINT FK_DEPT_CO FOREIGN KEY (COMPANY_ID) REFERENCES COMPANY(COMPANY_ID)
);

-- ─── MODULE 2: HUMAN RESOURCES ───────────────────────────────────────────

CREATE TABLE EMPLOYEE (
    EMP_ID          CHAR(8)        NOT NULL,
    COMPANY_ID      CHAR(4)        NOT NULL,
    DEPT_ID         CHAR(4)        NOT NULL,
    LAST_NAME       VARCHAR(30)    NOT NULL,
    FIRST_NAME      VARCHAR(20)    NOT NULL,
    MIDDLE_INITIAL  CHAR(1),
    SSN             CHAR(11)       NOT NULL,   -- masked: XXX-XX-XXXX
    DATE_OF_BIRTH   DATE           NOT NULL,
    HIRE_DATE       DATE           NOT NULL,
    TERM_DATE       DATE,
    JOB_CODE        CHAR(6)        NOT NULL,
    JOB_TITLE       VARCHAR(40),
    PAY_TYPE        CHAR(1)        NOT NULL,   -- S=Salary, H=Hourly
    PAY_RATE        DECIMAL(10,2)  NOT NULL,   -- annual/hourly
    STANDARD_HOURS  DECIMAL(5,2)   DEFAULT 40.00,
    FEDERAL_EXEMPT  SMALLINT       DEFAULT 1,
    STATE_EXEMPT    SMALLINT       DEFAULT 1,
    MARITAL_STATUS  CHAR(1)        DEFAULT 'S', -- S=Single, M=Married
    ACTIVE_FLAG     CHAR(1)        DEFAULT 'Y',
    CONSTRAINT PK_EMP PRIMARY KEY (EMP_ID),
    CONSTRAINT FK_EMP_DEPT FOREIGN KEY (DEPT_ID) REFERENCES DEPARTMENT(DEPT_ID)
);

CREATE TABLE JOB_CODE (
    JOB_CODE        CHAR(6)        NOT NULL,
    JOB_TITLE       VARCHAR(40)    NOT NULL,
    PAY_GRADE       CHAR(2)        NOT NULL,
    MIN_RATE        DECIMAL(10,2)  NOT NULL,
    MID_RATE        DECIMAL(10,2)  NOT NULL,
    MAX_RATE        DECIMAL(10,2)  NOT NULL,
    EXEMPT_FLAG     CHAR(1)        DEFAULT 'Y', -- FLSA exempt
    CONSTRAINT PK_JOB PRIMARY KEY (JOB_CODE)
);

-- ─── MODULE 3: PAYROLL ───────────────────────────────────────────────────

CREATE TABLE PAY_PERIOD (
    PERIOD_ID       CHAR(8)        NOT NULL,   -- YYYYMMWW
    COMPANY_ID      CHAR(4)        NOT NULL,
    PERIOD_BEGIN    DATE           NOT NULL,
    PERIOD_END      DATE           NOT NULL,
    CHECK_DATE      DATE           NOT NULL,
    PERIOD_TYPE     CHAR(1)        NOT NULL,   -- W=Weekly, B=BiWeekly, S=SemiMonthly, M=Monthly
    STATUS          CHAR(1)        DEFAULT 'O', -- O=Open, C=Closed, P=Posted
    CONSTRAINT PK_PERIOD PRIMARY KEY (PERIOD_ID, COMPANY_ID)
);

CREATE TABLE TIMECARD (
    TIMECARD_ID     INTEGER        NOT NULL GENERATED ALWAYS AS IDENTITY,
    EMP_ID          CHAR(8)        NOT NULL,
    PERIOD_ID       CHAR(8)        NOT NULL,
    WORK_DATE       DATE           NOT NULL,
    REG_HOURS       DECIMAL(5,2)   DEFAULT 0.00,
    OT_HOURS        DECIMAL(5,2)   DEFAULT 0.00,
    SICK_HOURS      DECIMAL(5,2)   DEFAULT 0.00,
    VAC_HOURS       DECIMAL(5,2)   DEFAULT 0.00,
    HOLIDAY_HOURS   DECIMAL(5,2)   DEFAULT 0.00,
    DEPT_ID         CHAR(4),
    JOB_CODE        CHAR(6),
    CONSTRAINT PK_TC PRIMARY KEY (TIMECARD_ID),
    CONSTRAINT FK_TC_EMP FOREIGN KEY (EMP_ID) REFERENCES EMPLOYEE(EMP_ID)
);

CREATE TABLE PAYCHECK (
    CHECK_ID        CHAR(12)       NOT NULL,
    EMP_ID          CHAR(8)        NOT NULL,
    PERIOD_ID       CHAR(8)        NOT NULL,
    CHECK_DATE      DATE           NOT NULL,
    GROSS_PAY       DECIMAL(10,2)  NOT NULL,
    FED_TAX         DECIMAL(8,2)   DEFAULT 0.00,
    STATE_TAX       DECIMAL(8,2)   DEFAULT 0.00,
    LOCAL_TAX       DECIMAL(8,2)   DEFAULT 0.00,
    SOC_SEC         DECIMAL(8,2)   DEFAULT 0.00,
    MEDICARE        DECIMAL(8,2)   DEFAULT 0.00,
    HEALTH_INS      DECIMAL(8,2)   DEFAULT 0.00,
    DENTAL_INS      DECIMAL(8,2)   DEFAULT 0.00,
    RETIREMENT_401K DECIMAL(8,2)   DEFAULT 0.00,
    OTHER_DED       DECIMAL(8,2)   DEFAULT 0.00,
    NET_PAY         DECIMAL(10,2)  NOT NULL,
    VOID_FLAG       CHAR(1)        DEFAULT 'N',
    POSTED_FLAG     CHAR(1)        DEFAULT 'N',
    CONSTRAINT PK_CHK PRIMARY KEY (CHECK_ID),
    CONSTRAINT FK_CHK_EMP FOREIGN KEY (EMP_ID) REFERENCES EMPLOYEE(EMP_ID)
);

-- ─── MODULE 4: INVENTORY ──────────────────────────────────────────────────

CREATE TABLE WAREHOUSE (
    WAREHOUSE_ID    CHAR(4)        NOT NULL,
    WAREHOUSE_NAME  VARCHAR(40)    NOT NULL,
    ADDRESS         VARCHAR(80),
    MANAGER_ID      CHAR(8),
    ACTIVE_FLAG     CHAR(1)        DEFAULT 'Y',
    CONSTRAINT PK_WH PRIMARY KEY (WAREHOUSE_ID)
);

CREATE TABLE ITEM_MASTER (
    ITEM_NO         CHAR(10)       NOT NULL,
    ITEM_DESC       VARCHAR(60)    NOT NULL,
    ITEM_TYPE       CHAR(2)        NOT NULL,   -- FG=Finished, RM=Raw Material, WIP, SV=Service
    UNIT_OF_MEASURE CHAR(4)        NOT NULL,
    UNIT_COST       DECIMAL(10,4)  NOT NULL,
    UNIT_PRICE      DECIMAL(10,4)  NOT NULL,
    REORDER_POINT   INTEGER        DEFAULT 0,
    REORDER_QTY     INTEGER        DEFAULT 0,
    LEAD_TIME_DAYS  INTEGER        DEFAULT 30,
    VENDOR_ID       CHAR(8),
    GL_ASSET_ACCT   CHAR(10),
    GL_COGS_ACCT    CHAR(10),
    ACTIVE_FLAG     CHAR(1)        DEFAULT 'Y',
    CONSTRAINT PK_ITEM PRIMARY KEY (ITEM_NO)
);

CREATE TABLE INVENTORY_BALANCE (
    ITEM_NO         CHAR(10)       NOT NULL,
    WAREHOUSE_ID    CHAR(4)        NOT NULL,
    QTY_ON_HAND     INTEGER        DEFAULT 0,
    QTY_COMMITTED   INTEGER        DEFAULT 0,  -- reserved for open orders
    QTY_ON_ORDER    INTEGER        DEFAULT 0,  -- outstanding POs
    LAST_COUNT_DATE DATE,
    LAST_RECV_DATE  DATE,
    CONSTRAINT PK_INV PRIMARY KEY (ITEM_NO, WAREHOUSE_ID),
    CONSTRAINT FK_INV_ITEM FOREIGN KEY (ITEM_NO) REFERENCES ITEM_MASTER(ITEM_NO)
);

CREATE TABLE INVENTORY_TRANSACTION (
    TRANS_ID        INTEGER        NOT NULL GENERATED ALWAYS AS IDENTITY,
    ITEM_NO         CHAR(10)       NOT NULL,
    WAREHOUSE_ID    CHAR(4)        NOT NULL,
    TRANS_DATE      DATE           NOT NULL,
    TRANS_TYPE      CHAR(4)        NOT NULL, -- RECV, ISSU, ADJP, ADJM, XFER, RTRN
    QUANTITY        INTEGER        NOT NULL,
    UNIT_COST       DECIMAL(10,4),
    REFERENCE_NO    CHAR(12),
    OPERATOR_ID     CHAR(8),
    CONSTRAINT PK_ITRANS PRIMARY KEY (TRANS_ID)
);

-- ─── MODULE 5: CUSTOMER & ACCOUNTS RECEIVABLE ────────────────────────────

CREATE TABLE CUSTOMER (
    CUSTOMER_ID     CHAR(8)        NOT NULL,
    COMPANY_NAME    VARCHAR(60)    NOT NULL,
    CONTACT_NAME    VARCHAR(40),
    ADDRESS_LINE1   VARCHAR(50),
    ADDRESS_LINE2   VARCHAR(50),
    CITY            VARCHAR(30),
    STATE_CODE      CHAR(2),
    ZIP_CODE        CHAR(10),
    COUNTRY         CHAR(3)        DEFAULT 'USA',
    PHONE           CHAR(15),
    EMAIL           VARCHAR(60),
    CREDIT_LIMIT    DECIMAL(12,2)  DEFAULT 10000.00,
    PAYMENT_TERMS   CHAR(6)        DEFAULT 'NET30',
    SALES_REP_ID    CHAR(8),
    AR_ACCOUNT      CHAR(10),
    TAX_EXEMPT      CHAR(1)        DEFAULT 'N',
    ACTIVE_FLAG     CHAR(1)        DEFAULT 'Y',
    CONSTRAINT PK_CUST PRIMARY KEY (CUSTOMER_ID)
);

CREATE TABLE SALES_ORDER (
    ORDER_NO        CHAR(10)       NOT NULL,
    CUSTOMER_ID     CHAR(8)        NOT NULL,
    ORDER_DATE      DATE           NOT NULL,
    SHIP_DATE       DATE,
    REQUIRED_DATE   DATE,
    SHIP_VIA        VARCHAR(20),
    PO_NUMBER       CHAR(20),
    STATUS          CHAR(1)        DEFAULT 'O', -- O=Open, P=Partial, C=Complete, X=Cancelled
    SUBTOTAL        DECIMAL(12,2)  DEFAULT 0.00,
    TAX_AMOUNT      DECIMAL(10,2)  DEFAULT 0.00,
    FREIGHT         DECIMAL(8,2)   DEFAULT 0.00,
    ORDER_TOTAL     DECIMAL(12,2)  DEFAULT 0.00,
    SALES_REP_ID    CHAR(8),
    WAREHOUSE_ID    CHAR(4),
    CONSTRAINT PK_SO PRIMARY KEY (ORDER_NO),
    CONSTRAINT FK_SO_CUST FOREIGN KEY (CUSTOMER_ID) REFERENCES CUSTOMER(CUSTOMER_ID)
);

CREATE TABLE SALES_ORDER_LINE (
    ORDER_NO        CHAR(10)       NOT NULL,
    LINE_NO         SMALLINT       NOT NULL,
    ITEM_NO         CHAR(10)       NOT NULL,
    ITEM_DESC       VARCHAR(60),
    QTY_ORDERED     INTEGER        NOT NULL,
    QTY_SHIPPED     INTEGER        DEFAULT 0,
    UNIT_PRICE      DECIMAL(10,4)  NOT NULL,
    DISCOUNT_PCT    DECIMAL(5,2)   DEFAULT 0.00,
    EXTENDED_PRICE  DECIMAL(12,2),
    STATUS          CHAR(1)        DEFAULT 'O',
    CONSTRAINT PK_SOL PRIMARY KEY (ORDER_NO, LINE_NO),
    CONSTRAINT FK_SOL_SO FOREIGN KEY (ORDER_NO) REFERENCES SALES_ORDER(ORDER_NO),
    CONSTRAINT FK_SOL_ITEM FOREIGN KEY (ITEM_NO) REFERENCES ITEM_MASTER(ITEM_NO)
);

-- ─── MODULE 6: GENERAL LEDGER ─────────────────────────────────────────────

CREATE TABLE CHART_OF_ACCOUNTS (
    GL_ACCOUNT      CHAR(10)       NOT NULL,
    ACCOUNT_NAME    VARCHAR(50)    NOT NULL,
    ACCT_TYPE       CHAR(2)        NOT NULL, -- AS=Asset, LI=Liability, EQ=Equity, RE=Revenue, EX=Expense
    NORMAL_BALANCE  CHAR(1)        NOT NULL, -- D=Debit, C=Credit
    PARENT_ACCOUNT  CHAR(10),
    IS_HEADER       CHAR(1)        DEFAULT 'N',
    IS_POSTING      CHAR(1)        DEFAULT 'Y',
    ACTIVE_FLAG     CHAR(1)        DEFAULT 'Y',
    CONSTRAINT PK_COA PRIMARY KEY (GL_ACCOUNT)
);

CREATE TABLE FISCAL_PERIOD (
    FISCAL_YEAR     SMALLINT       NOT NULL,
    FISCAL_PERIOD   SMALLINT       NOT NULL,  -- 1-12 (+ 13 for adjustments)
    PERIOD_NAME     CHAR(10)       NOT NULL,
    BEGIN_DATE      DATE           NOT NULL,
    END_DATE        DATE           NOT NULL,
    STATUS          CHAR(1)        DEFAULT 'O', -- O=Open, C=Closed
    CONSTRAINT PK_FP PRIMARY KEY (FISCAL_YEAR, FISCAL_PERIOD)
);

CREATE TABLE GL_JOURNAL (
    JOURNAL_ID      CHAR(12)       NOT NULL,
    JOURNAL_DATE    DATE           NOT NULL,
    FISCAL_YEAR     SMALLINT       NOT NULL,
    FISCAL_PERIOD   SMALLINT       NOT NULL,
    DESCRIPTION     VARCHAR(80)    NOT NULL,
    JOURNAL_TYPE    CHAR(4)        NOT NULL, -- PAYP=Payroll, CASH, INVC, MISC, RECR=Recurring
    SOURCE_DOC      CHAR(15),
    POSTED_FLAG     CHAR(1)        DEFAULT 'N',
    POSTED_DATE     DATE,
    CREATED_BY      CHAR(8),
    CONSTRAINT PK_JNL PRIMARY KEY (JOURNAL_ID)
);

CREATE TABLE GL_JOURNAL_LINE (
    JOURNAL_ID      CHAR(12)       NOT NULL,
    LINE_NO         SMALLINT       NOT NULL,
    GL_ACCOUNT      CHAR(10)       NOT NULL,
    DEPT_ID         CHAR(4),
    DESCRIPTION     VARCHAR(50),
    DEBIT_AMOUNT    DECIMAL(14,2)  DEFAULT 0.00,
    CREDIT_AMOUNT   DECIMAL(14,2)  DEFAULT 0.00,
    REFERENCE       CHAR(20),
    CONSTRAINT PK_JNL_LINE PRIMARY KEY (JOURNAL_ID, LINE_NO),
    CONSTRAINT FK_JNL_LINE FOREIGN KEY (JOURNAL_ID) REFERENCES GL_JOURNAL(JOURNAL_ID),
    CONSTRAINT FK_JNL_COA  FOREIGN KEY (GL_ACCOUNT) REFERENCES CHART_OF_ACCOUNTS(GL_ACCOUNT)
);

-- ─── SEED DATA ───────────────────────────────────────────────────────────

INSERT INTO COMPANY VALUES (
    'ACME', 'Acme Corporation', '100 Enterprise Drive', 'Suite 500',
    'Chicago', 'IL', '60601', '312-555-0100', '36-1234567', '12/31'
);

INSERT INTO CHART_OF_ACCOUNTS VALUES
('1000', 'CURRENT ASSETS',              'AS', 'D', NULL,   'Y', 'N', 'Y'),
('1010', 'Cash - Operating',            'AS', 'D', '1000', 'N', 'Y', 'Y'),
('1020', 'Cash - Payroll',              'AS', 'D', '1000', 'N', 'Y', 'Y'),
('1100', 'Accounts Receivable',         'AS', 'D', '1000', 'N', 'Y', 'Y'),
('1200', 'Inventory',                   'AS', 'D', '1000', 'N', 'Y', 'Y'),
('2000', 'CURRENT LIABILITIES',         'LI', 'C', NULL,   'Y', 'N', 'Y'),
('2100', 'Accounts Payable',            'LI', 'C', '2000', 'N', 'Y', 'Y'),
('2200', 'Accrued Payroll',             'LI', 'C', '2000', 'N', 'Y', 'Y'),
('2300', 'Federal Tax Withheld',        'LI', 'C', '2000', 'N', 'Y', 'Y'),
('2310', 'State Tax Withheld',          'LI', 'C', '2000', 'N', 'Y', 'Y'),
('2320', 'FICA Payable',                'LI', 'C', '2000', 'N', 'Y', 'Y'),
('3000', 'EQUITY',                      'EQ', 'C', NULL,   'Y', 'N', 'Y'),
('3100', 'Common Stock',                'EQ', 'C', '3000', 'N', 'Y', 'Y'),
('3900', 'Retained Earnings',           'EQ', 'C', '3000', 'N', 'Y', 'Y'),
('4000', 'REVENUE',                     'RE', 'C', NULL,   'Y', 'N', 'Y'),
('4100', 'Product Sales Revenue',       'RE', 'C', '4000', 'N', 'Y', 'Y'),
('4200', 'Service Revenue',             'RE', 'C', '4000', 'N', 'Y', 'Y'),
('5000', 'COST OF GOODS SOLD',          'EX', 'D', NULL,   'Y', 'N', 'Y'),
('5100', 'COGS - Products',             'EX', 'D', '5000', 'N', 'Y', 'Y'),
('6000', 'OPERATING EXPENSES',          'EX', 'D', NULL,   'Y', 'N', 'Y'),
('6100', 'Salaries & Wages',            'EX', 'D', '6000', 'N', 'Y', 'Y'),
('6110', 'Employer Payroll Taxes',      'EX', 'D', '6000', 'N', 'Y', 'Y'),
('6120', 'Employee Benefits',           'EX', 'D', '6000', 'N', 'Y', 'Y'),
('6200', 'Office Expenses',             'EX', 'D', '6000', 'N', 'Y', 'Y'),
('6300', 'Marketing Expenses',          'EX', 'D', '6000', 'N', 'Y', 'Y');

INSERT INTO DEPARTMENT VALUES
('D001', 'ACME', 'Operations',         'OPS001', NULL, 'CC001', '6100', 'Y'),
('D002', 'ACME', 'Finance',            'FIN001', NULL, 'CC002', '6100', 'Y'),
('D003', 'ACME', 'Human Resources',    'HR0001', NULL, 'CC003', '6100', 'Y'),
('D004', 'ACME', 'Information Tech',   'IT0001', NULL, 'CC004', '6100', 'Y'),
('D005', 'ACME', 'Sales & Marketing',  'SAL001', NULL, 'CC005', '6300', 'Y'),
('D006', 'ACME', 'Warehouse',          'WHS001', NULL, 'CC006', '6100', 'Y');

INSERT INTO JOB_CODE VALUES
('MGR001', 'Operations Manager',      'M1', 75000, 90000, 120000, 'Y'),
('ENG001', 'Software Engineer',       'E3', 70000, 85000, 110000, 'Y'),
('ENG002', 'Senior Software Engineer','E4', 90000,110000, 140000, 'Y'),
('ACT001', 'Accountant',              'P2', 55000, 65000,  80000, 'Y'),
('SAL001', 'Sales Representative',    'S2', 45000, 55000,  75000, 'Y'),
('WRH001', 'Warehouse Associate',     'H1', 18.50, 22.00,  28.00, 'N'),
('WRH002', 'Warehouse Supervisor',    'H2', 25.00, 30.00,  38.00, 'N');

INSERT INTO EMPLOYEE VALUES
('E0001001', 'ACME', 'D001', 'Smith',    'Robert',  'J', '555-12-3456', '1975-03-15', '2010-01-15', NULL, 'MGR001', 'Operations Manager',  'S', 95000.00, 40, 1, 0, 'M', 'Y'),
('E0001002', 'ACME', 'D002', 'Johnson',  'Patricia','A', '666-23-4567', '1980-07-22', '2012-06-01', NULL, 'ACT001', 'Accountant',           'S', 65000.00, 40, 1, 0, 'S', 'Y'),
('E0001003', 'ACME', 'D004', 'Williams', 'James',   'R', '777-34-5678', '1988-11-30', '2015-03-20', NULL, 'ENG001', 'Software Engineer',    'S', 82000.00, 40, 1, 1, 'S', 'Y'),
('E0001004', 'ACME', 'D004', 'Brown',    'Linda',   'K', '888-45-6789', '1985-04-10', '2013-09-15', NULL, 'ENG002', 'Sr. Software Engineer', 'S',108000.00, 40, 2, 1, 'M', 'Y'),
('E0001005', 'ACME', 'D005', 'Davis',    'Michael', 'T', '999-56-7890', '1990-09-05', '2018-01-08', NULL, 'SAL001', 'Sales Representative',  'S', 52000.00, 40, 1, 0, 'M', 'Y'),
('E0001006', 'ACME', 'D006', 'Miller',   'Susan',   'E', '111-67-8901', '1992-12-18', '2019-07-22', NULL, 'WRH001', 'Warehouse Associate',   'H',    22.50, 40, 0, 0, 'S', 'Y'),
('E0001007', 'ACME', 'D006', 'Wilson',   'David',   'L', '222-78-9012', '1983-06-25', '2011-04-11', NULL, 'WRH002', 'Warehouse Supervisor',  'H',    31.00, 40, 1, 0, 'M', 'Y');

INSERT INTO WAREHOUSE VALUES
('WH01', 'Chicago Main Warehouse',  '100 Industrial Ave, Chicago, IL 60608', 'E0001007', 'Y'),
('WH02', 'Detroit Distribution Ctr','500 Motor Lane, Detroit, MI 48201',     NULL, 'Y');

INSERT INTO ITEM_MASTER VALUES
('ITM-001001', 'Industrial Widget A',     'FG', 'EACH', 12.5000, 24.9900,  50, 100, 14, NULL, '1200', '5100', 'Y'),
('ITM-001002', 'Industrial Widget B',     'FG', 'EACH', 18.7500, 37.9900,  30,  75, 21, NULL, '1200', '5100', 'Y'),
('ITM-001003', 'Steel Rod 1" x 12"',     'RM', 'EACH',  2.1500,  0.0000, 200, 500, 30, NULL, '1200', '5100', 'Y'),
('ITM-001004', 'Assembly Kit Pro',        'FG', 'KIT',  45.0000, 89.9900,  20,  50, 10, NULL, '1200', '5100', 'Y'),
('ITM-001005', 'Maintenance Service',     'SV', 'HOUR', 65.0000, 95.0000,   0,   0,  0, NULL, NULL,   NULL,   'Y');

INSERT INTO INVENTORY_BALANCE VALUES
('ITM-001001', 'WH01', 450, 25,  0, '2025-12-01', '2025-11-28'),
('ITM-001002', 'WH01', 210, 15, 75, '2025-12-01', '2025-11-15'),
('ITM-001003', 'WH01', 880,  0,500, '2025-12-01', '2025-12-03'),
('ITM-001004', 'WH01',  65, 10,  0, '2025-11-15', '2025-10-20'),
('ITM-001001', 'WH02', 120,  5,  0, '2025-12-01', '2025-11-20');

INSERT INTO CUSTOMER VALUES
('CUST0001', 'Acme Industrial Supply',    'Tom Baker',    '200 Commerce St', NULL, 'Chicago',      'IL', '60602', 'USA', '312-555-0200', 'tom@acme-supply.com',  50000, 'NET30', 'E0001005', '1100', 'N', 'Y'),
('CUST0002', 'Midwest Manufacturing LLC', 'Sara Lee',     '75 Factory Rd',   NULL, 'Milwaukee',    'WI', '53201', 'USA', '414-555-0300', 'sara@mmfg.com',        25000, 'NET15', 'E0001005', '1100', 'N', 'Y'),
('CUST0003', 'Great Lakes Distributors',  'Chris Hunt',   '500 Lake Shore',  NULL, 'Cleveland',    'OH', '44101', 'USA', '216-555-0400', 'chris@gld.com',       100000, 'NET45', 'E0001005', '1100', 'N', 'Y'),
('CUST0004', 'Capital City Parts Inc',    'Ana Torres',   '1000 Capital Way',NULL, 'Indianapolis', 'IN', '46201', 'USA', '317-555-0500', 'ana@ccparts.com',      15000, 'NET30', 'E0001005', '1100', 'Y', 'Y');

INSERT INTO SALES_ORDER VALUES
('SO-2025-001', 'CUST0001', '2025-11-15', '2025-11-18', '2025-11-20', 'UPS Ground',  'PO-88421', 'C', 1247.25, 99.78,  25.00, 1372.03, 'E0001005', 'WH01'),
('SO-2025-002', 'CUST0003', '2025-11-22', '2025-11-25', '2025-11-30', 'FedEx 2-Day', 'PO-99234', 'C', 5396.00, 431.68, 45.00, 5872.68, 'E0001005', 'WH01'),
('SO-2025-003', 'CUST0002', '2025-12-01', NULL,          '2025-12-10', 'Will Call',   'PO-77654', 'O',  899.70,  71.98,  0.00,  971.68, 'E0001005', 'WH01'),
('SO-2025-004', 'CUST0004', '2025-12-03', NULL,          '2025-12-15', 'UPS Ground',  'PO-55123', 'O',  269.96,  21.60, 15.00,  306.56, 'E0001005', 'WH01');

INSERT INTO SALES_ORDER_LINE VALUES
('SO-2025-001', 1, 'ITM-001001', 'Industrial Widget A', 50, 50, 24.9900, 0.00, 1249.50, 'C'),
('SO-2025-002', 1, 'ITM-001001', 'Industrial Widget A',100,100, 24.9900, 8.00, 2299.08, 'C'),
('SO-2025-002', 2, 'ITM-001004', 'Assembly Kit Pro',    35, 35, 89.9900, 5.00, 2987.17, 'C'),
('SO-2025-003', 1, 'ITM-001002', 'Industrial Widget B', 24,  0, 37.9900, 2.00,  893.08, 'O'),
('SO-2025-004', 1, 'ITM-001001', 'Industrial Widget A', 10,  0, 24.9900,10.00,  224.91, 'O'),
('SO-2025-004', 2, 'ITM-001002', 'Industrial Widget B',  2,  0, 37.9900, 0.00,   75.98, 'O');

INSERT INTO PAY_PERIOD VALUES
('2025122BI', 'ACME', '2025-12-08', '2025-12-21', '2025-12-26', 'B', 'O');

-- ─── VIEWS ───────────────────────────────────────────────────────────────

CREATE VIEW VW_EMPLOYEE_SUMMARY AS
SELECT
    E.EMP_ID,
    E.LAST_NAME || ', ' || E.FIRST_NAME AS FULL_NAME,
    D.DEPT_NAME,
    J.JOB_TITLE,
    E.PAY_TYPE,
    CASE E.PAY_TYPE
        WHEN 'S' THEN E.PAY_RATE
        ELSE E.PAY_RATE * E.STANDARD_HOURS * 52
    END AS ANNUAL_EQUIVALENT,
    E.HIRE_DATE,
    E.ACTIVE_FLAG
FROM EMPLOYEE E
JOIN DEPARTMENT D ON E.DEPT_ID = D.DEPT_ID
JOIN JOB_CODE J   ON E.JOB_CODE = J.JOB_CODE;

CREATE VIEW VW_OPEN_ORDERS AS
SELECT
    O.ORDER_NO,
    C.COMPANY_NAME AS CUSTOMER_NAME,
    O.ORDER_DATE,
    O.REQUIRED_DATE,
    O.ORDER_TOTAL,
    O.STATUS,
    COUNT(L.LINE_NO) AS LINE_COUNT,
    SUM(L.QTY_ORDERED) AS TOTAL_UNITS
FROM SALES_ORDER O
JOIN CUSTOMER C ON O.CUSTOMER_ID = C.CUSTOMER_ID
JOIN SALES_ORDER_LINE L ON O.ORDER_NO = L.ORDER_NO
WHERE O.STATUS IN ('O', 'P')
GROUP BY O.ORDER_NO, C.COMPANY_NAME, O.ORDER_DATE, O.REQUIRED_DATE, O.ORDER_TOTAL, O.STATUS;

CREATE VIEW VW_INVENTORY_STATUS AS
SELECT
    I.ITEM_NO,
    M.ITEM_DESC,
    W.WAREHOUSE_NAME,
    I.QTY_ON_HAND,
    I.QTY_COMMITTED,
    I.QTY_ON_ORDER,
    I.QTY_ON_HAND - I.QTY_COMMITTED AS QTY_AVAILABLE,
    M.REORDER_POINT,
    CASE WHEN (I.QTY_ON_HAND - I.QTY_COMMITTED) <= M.REORDER_POINT
         THEN 'REORDER NOW' ELSE 'OK' END AS STOCK_STATUS,
    M.UNIT_COST,
    I.QTY_ON_HAND * M.UNIT_COST AS INVENTORY_VALUE
FROM INVENTORY_BALANCE I
JOIN ITEM_MASTER M ON I.ITEM_NO = M.ITEM_NO
JOIN WAREHOUSE W   ON I.WAREHOUSE_ID = W.WAREHOUSE_ID;

-- ─── USEFUL QUERIES ──────────────────────────────────────────────────────

-- Payroll summary by department
-- SELECT D.DEPT_NAME,
--        COUNT(E.EMP_ID) AS HEADCOUNT,
--        SUM(CASE WHEN E.PAY_TYPE='S' THEN E.PAY_RATE ELSE E.PAY_RATE*40*52 END) AS TOTAL_ANNUALIZED
-- FROM EMPLOYEE E JOIN DEPARTMENT D ON E.DEPT_ID = D.DEPT_ID
-- WHERE E.ACTIVE_FLAG = 'Y' GROUP BY D.DEPT_NAME ORDER BY TOTAL_ANNUALIZED DESC;

-- Items below reorder point
-- SELECT * FROM VW_INVENTORY_STATUS WHERE STOCK_STATUS = 'REORDER NOW';

-- Open orders aged > 5 days
-- SELECT * FROM VW_OPEN_ORDERS WHERE CURRENT DATE - REQUIRED_DATE > 5;
