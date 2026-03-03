-- ================================================================
--  ACME CORPORATION ERP — BUSINESS INTELLIGENCE QUERIES
--  SQL analytics, views, and report queries for management.
--  Compatible with IBM DB2 / PostgreSQL / most ANSI-SQL engines.
-- ================================================================

-- ──────────────────────────────────────────────────────────────
--  SECTION 1: PAYROLL ANALYTICS
-- ──────────────────────────────────────────────────────────────

-- 1.1 Payroll summary by department
SELECT
    D.DEPT_NAME                                AS department,
    COUNT(DISTINCT E.EMP_ID)                   AS headcount,
    SUM(P.GROSS_PAY)                           AS gross_payroll,
    SUM(P.NET_PAY)                             AS net_payroll,
    SUM(P.FEDERAL_TAX + P.STATE_TAX)          AS total_taxes,
    SUM(P.FICA_EE + P.MEDICARE_EE)            AS total_fica,
    SUM(P.BENEFITS_DED)                        AS total_benefits,
    AVG(P.GROSS_PAY)                           AS avg_gross_pay,
    MIN(P.GROSS_PAY)                           AS min_gross_pay,
    MAX(P.GROSS_PAY)                           AS max_gross_pay
FROM
    PAYCHECK    P
    JOIN EMPLOYEE   E  ON E.EMP_ID    = P.EMP_ID
    JOIN DEPARTMENT D  ON D.DEPT_ID   = E.DEPT_ID
    JOIN PAY_PERIOD PP ON PP.PERIOD_ID = P.PERIOD_ID
WHERE
    PP.STATUS = 'CLOSED'
    AND PP.DATE_FROM >= CURRENT DATE - 365 DAYS
    AND P.VOID_FLAG = 'N'
GROUP BY
    D.DEPT_ID, D.DEPT_NAME
ORDER BY
    SUM(P.GROSS_PAY) DESC;

-- 1.2 Year-to-date payroll by employee (current year)
SELECT
    E.EMP_ID,
    E.LAST_NAME  || ', ' || E.FIRST_NAME       AS employee_name,
    J.JOB_TITLE,
    D.DEPT_NAME,
    SUM(P.GROSS_PAY)                           AS ytd_gross,
    SUM(P.NET_PAY)                             AS ytd_net,
    SUM(P.FEDERAL_TAX)                         AS ytd_fed_tax,
    SUM(P.STATE_TAX)                           AS ytd_state_tax,
    SUM(P.FICA_EE + P.MEDICARE_EE)            AS ytd_fica,
    COUNT(P.CHECK_NUMBER)                      AS pay_periods
FROM
    PAYCHECK    P
    JOIN EMPLOYEE   E  ON E.EMP_ID    = P.EMP_ID
    JOIN JOB_CODE   J  ON J.JOB_CODE  = E.JOB_CODE
    JOIN DEPARTMENT D  ON D.DEPT_ID   = E.DEPT_ID
    JOIN PAY_PERIOD PP ON PP.PERIOD_ID = P.PERIOD_ID
WHERE
    YEAR(PP.DATE_FROM) = YEAR(CURRENT DATE)
    AND P.VOID_FLAG = 'N'
GROUP BY
    E.EMP_ID, E.LAST_NAME, E.FIRST_NAME,
    J.JOB_TITLE, D.DEPT_NAME
ORDER BY
    E.LAST_NAME, E.FIRST_NAME;

-- ──────────────────────────────────────────────────────────────
--  SECTION 2: SALES & CUSTOMER ANALYTICS
-- ──────────────────────────────────────────────────────────────

-- 2.1 Top customers by revenue (last 12 months)
SELECT
    C.CUST_ID,
    C.CUST_NAME,
    C.CUST_TYPE,
    COUNT(DISTINCT SO.ORDER_ID)                AS order_count,
    SUM(SOL.QTY_ORDERED * SOL.UNIT_PRICE)     AS total_revenue,
    AVG(SOL.QTY_ORDERED * SOL.UNIT_PRICE)     AS avg_order_value,
    MAX(SO.ORDER_DATE)                         AS last_order_date,
    DECIMAL(SUM(SOL.QTY_ORDERED * SOL.UNIT_PRICE) /
        SUM(SUM(SOL.QTY_ORDERED * SOL.UNIT_PRICE))
        OVER () * 100, 5, 2)                   AS pct_of_total
FROM
    CUSTOMER       C
    JOIN SALES_ORDER     SO  ON SO.CUST_ID  = C.CUST_ID
    JOIN SALES_ORDER_LINE SOL ON SOL.ORDER_ID = SO.ORDER_ID
WHERE
    SO.ORDER_DATE >= CURRENT DATE - 365 DAYS
    AND SO.STATUS IN ('SHIPPED','INVOICED','PAID')
GROUP BY
    C.CUST_ID, C.CUST_NAME, C.CUST_TYPE
ORDER BY
    total_revenue DESC
FETCH FIRST 20 ROWS ONLY;

-- 2.2 Sales by item (product performance)
SELECT
    I.ITEM_NUMBER,
    I.ITEM_DESC,
    I.CATEGORY,
    COUNT(DISTINCT SO.ORDER_ID)                AS orders_containing,
    SUM(SOL.QTY_ORDERED)                       AS total_qty_sold,
    SUM(SOL.QTY_ORDERED * SOL.UNIT_PRICE)     AS total_revenue,
    SUM(SOL.QTY_ORDERED * I.STD_COST)         AS total_cost,
    SUM(SOL.QTY_ORDERED * (SOL.UNIT_PRICE - I.STD_COST)) AS gross_profit,
    DECIMAL(SUM(SOL.QTY_ORDERED * (SOL.UNIT_PRICE - I.STD_COST)) /
        NULLIF(SUM(SOL.QTY_ORDERED * SOL.UNIT_PRICE),0) * 100, 5, 2)
                                               AS margin_pct
FROM
    ITEM_MASTER    I
    JOIN SALES_ORDER_LINE SOL ON SOL.ITEM_NUMBER = I.ITEM_NUMBER
    JOIN SALES_ORDER      SO  ON SO.ORDER_ID     = SOL.ORDER_ID
WHERE
    SO.STATUS IN ('SHIPPED','INVOICED','PAID')
GROUP BY
    I.ITEM_NUMBER, I.ITEM_DESC, I.CATEGORY
ORDER BY
    total_revenue DESC;

-- 2.3 Monthly sales trend (last 6 months)
SELECT
    YEAR(SO.ORDER_DATE)  AS order_year,
    MONTH(SO.ORDER_DATE) AS order_month,
    COUNT(DISTINCT SO.ORDER_ID)               AS orders,
    COUNT(DISTINCT SO.CUST_ID)                AS unique_customers,
    SUM(SOL.QTY_ORDERED)                      AS total_units,
    SUM(SOL.QTY_ORDERED * SOL.UNIT_PRICE)    AS total_revenue
FROM
    SALES_ORDER      SO
    JOIN SALES_ORDER_LINE SOL ON SOL.ORDER_ID = SO.ORDER_ID
WHERE
    SO.ORDER_DATE >= CURRENT DATE - 180 DAYS
    AND SO.STATUS NOT IN ('CANCELLED')
GROUP BY
    YEAR(SO.ORDER_DATE), MONTH(SO.ORDER_DATE)
ORDER BY
    order_year, order_month;

-- ──────────────────────────────────────────────────────────────
--  SECTION 3: INVENTORY ANALYTICS
-- ──────────────────────────────────────────────────────────────

-- 3.1 Inventory turnover by item
WITH sales_cogs AS (
    SELECT
        SOL.ITEM_NUMBER,
        SUM(SOL.QTY_ORDERED * I.STD_COST)  AS annual_cogs
    FROM SALES_ORDER_LINE SOL
    JOIN SALES_ORDER      SO ON SO.ORDER_ID = SOL.ORDER_ID
    JOIN ITEM_MASTER      I  ON I.ITEM_NUMBER = SOL.ITEM_NUMBER
    WHERE SO.ORDER_DATE >= CURRENT DATE - 365 DAYS
      AND SO.STATUS NOT IN ('CANCELLED')
    GROUP BY SOL.ITEM_NUMBER
),
avg_inv AS (
    SELECT
        ITEM_NUMBER,
        AVG(QTY_ON_HAND * 1.0) AS avg_on_hand
    FROM INVENTORY_BALANCE
    GROUP BY ITEM_NUMBER
)
SELECT
    I.ITEM_NUMBER,
    I.ITEM_DESC,
    I.CATEGORY,
    COALESCE(AI.avg_on_hand, 0)                   AS avg_inventory_units,
    COALESCE(SC.annual_cogs, 0)                   AS annual_cogs,
    I.STD_COST * COALESCE(AI.avg_on_hand, 0)      AS avg_inventory_value,
    CASE
        WHEN I.STD_COST * COALESCE(AI.avg_on_hand, 0) = 0 THEN NULL
        ELSE DECIMAL(COALESCE(SC.annual_cogs,0) /
            (I.STD_COST * COALESCE(AI.avg_on_hand,0)), 7, 2)
    END AS inventory_turns,
    CASE
        WHEN COALESCE(SC.annual_cogs,0) = 0 THEN NULL
        ELSE DECIMAL(365 * I.STD_COST * COALESCE(AI.avg_on_hand,0)
            / COALESCE(SC.annual_cogs,0), 7, 1)
    END AS days_on_hand
FROM
    ITEM_MASTER I
    LEFT JOIN avg_inv  AI ON AI.ITEM_NUMBER = I.ITEM_NUMBER
    LEFT JOIN sales_cogs SC ON SC.ITEM_NUMBER = I.ITEM_NUMBER
ORDER BY
    inventory_turns DESC NULLS LAST;

-- 3.2 Items below reorder point
SELECT
    I.ITEM_NUMBER,
    I.ITEM_DESC,
    I.CATEGORY,
    W.WAREHOUSE_NAME,
    IB.QTY_ON_HAND,
    IB.QTY_ON_ORDER,
    IB.QTY_ALLOCATED,
    (IB.QTY_ON_HAND - IB.QTY_ALLOCATED)   AS available_qty,
    I.REORDER_POINT,
    I.REORDER_QTY                           AS suggested_order_qty,
    I.STD_COST * I.REORDER_QTY             AS suggested_order_cost
FROM
    INVENTORY_BALANCE IB
    JOIN ITEM_MASTER  I  ON I.ITEM_NUMBER  = IB.ITEM_NUMBER
    JOIN WAREHOUSE    W  ON W.WAREHOUSE_ID = IB.WAREHOUSE_ID
WHERE
    (IB.QTY_ON_HAND - IB.QTY_ALLOCATED) < I.REORDER_POINT
ORDER BY
    (IB.QTY_ON_HAND - IB.QTY_ALLOCATED - I.REORDER_POINT),
    I.ITEM_NUMBER;

-- ──────────────────────────────────────────────────────────────
--  SECTION 4: ACCOUNTS RECEIVABLE ANALYTICS
-- ──────────────────────────────────────────────────────────────

-- 4.1 AR Aging Summary
SELECT
    C.CUST_ID,
    C.CUST_NAME,
    SUM(
      CASE WHEN DAYS(CURRENT DATE) - DAYS(SO.ORDER_DATE) <= 30
           THEN SOL.QTY_ORDERED * SOL.UNIT_PRICE ELSE 0 END
    )                                             AS current_0_30,
    SUM(
      CASE WHEN DAYS(CURRENT DATE) - DAYS(SO.ORDER_DATE) BETWEEN 31 AND 60
           THEN SOL.QTY_ORDERED * SOL.UNIT_PRICE ELSE 0 END
    )                                             AS days_31_60,
    SUM(
      CASE WHEN DAYS(CURRENT DATE) - DAYS(SO.ORDER_DATE) BETWEEN 61 AND 90
           THEN SOL.QTY_ORDERED * SOL.UNIT_PRICE ELSE 0 END
    )                                             AS days_61_90,
    SUM(
      CASE WHEN DAYS(CURRENT DATE) - DAYS(SO.ORDER_DATE) > 90
           THEN SOL.QTY_ORDERED * SOL.UNIT_PRICE ELSE 0 END
    )                                             AS over_90,
    SUM(SOL.QTY_ORDERED * SOL.UNIT_PRICE)        AS total_ar
FROM
    CUSTOMER       C
    JOIN SALES_ORDER      SO  ON SO.CUST_ID  = C.CUST_ID
    JOIN SALES_ORDER_LINE SOL ON SOL.ORDER_ID = SO.ORDER_ID
WHERE
    SO.STATUS IN ('INVOICED')   -- outstanding invoices only
GROUP BY
    C.CUST_ID, C.CUST_NAME
HAVING
    SUM(SOL.QTY_ORDERED * SOL.UNIT_PRICE) > 0
ORDER BY
    total_ar DESC;

-- ──────────────────────────────────────────────────────────────
--  SECTION 5: PROFIT & LOSS STATEMENT QUERY
-- ──────────────────────────────────────────────────────────────

-- 5.1 P&L for current period
WITH account_balances AS (
    SELECT
        COA.ACCT_NUMBER,
        COA.ACCT_DESC,
        COA.ACCT_TYPE,
        COA.ACCT_SUBTYPE,
        COA.NORMAL_BAL,
        COALESCE(SUM(CASE WHEN JL.DR_CR_IND = 'D' THEN JL.AMOUNT ELSE 0 END),0)
            - COALESCE(SUM(CASE WHEN JL.DR_CR_IND = 'C' THEN JL.AMOUNT ELSE 0 END),0)
            AS period_net_activity
    FROM CHART_OF_ACCOUNTS COA
    LEFT JOIN GL_JOURNAL_LINE JL ON JL.ACCT_NUMBER = COA.ACCT_NUMBER
    LEFT JOIN GL_JOURNAL      GJ ON GJ.JOURNAL_ID  = JL.JOURNAL_ID
    LEFT JOIN FISCAL_PERIOD   FP ON FP.PERIOD_ID   = GJ.PERIOD_ID
        AND FP.PERIOD_ID = (SELECT PERIOD_ID FROM FISCAL_PERIOD
                            WHERE STATUS = 'CLOSED'
                            ORDER BY PERIOD_SEQ DESC FETCH FIRST 1 ROW ONLY)
    WHERE COA.ACCT_TYPE IN ('R', 'X')
    GROUP BY COA.ACCT_NUMBER, COA.ACCT_DESC, COA.ACCT_TYPE,
             COA.ACCT_SUBTYPE,  COA.NORMAL_BAL
)
SELECT
    ACCT_TYPE,
    ACCT_SUBTYPE,
    ACCT_NUMBER,
    ACCT_DESC,
    CASE
        WHEN ACCT_TYPE = 'R' AND NORMAL_BAL = 'C'
            THEN  period_net_activity * -1   -- revenues show as positive
        WHEN ACCT_TYPE = 'X' AND NORMAL_BAL = 'D'
            THEN  period_net_activity        -- expenses show as positive
        ELSE period_net_activity
    END AS amount
FROM account_balances
ORDER BY ACCT_TYPE DESC, ACCT_SUBTYPE, ACCT_NUMBER;

-- 5.2 P&L summary roll-up
WITH pl AS (
    SELECT
        COA.ACCT_TYPE,
        COALESCE(SUM(CASE
            WHEN COA.ACCT_TYPE = 'R' AND JL.DR_CR_IND = 'C' THEN  JL.AMOUNT
            WHEN COA.ACCT_TYPE = 'R' AND JL.DR_CR_IND = 'D' THEN -JL.AMOUNT
            WHEN COA.ACCT_TYPE = 'X' AND JL.DR_CR_IND = 'D' THEN  JL.AMOUNT
            WHEN COA.ACCT_TYPE = 'X' AND JL.DR_CR_IND = 'C' THEN -JL.AMOUNT
            ELSE 0 END), 0) AS balance
    FROM CHART_OF_ACCOUNTS COA
    LEFT JOIN GL_JOURNAL_LINE JL ON JL.ACCT_NUMBER = COA.ACCT_NUMBER
    LEFT JOIN GL_JOURNAL GJ ON GJ.JOURNAL_ID = JL.JOURNAL_ID
        AND GJ.STATUS = 'POSTED'
    WHERE COA.ACCT_TYPE IN ('R','X')
    GROUP BY COA.ACCT_TYPE
)
SELECT
    'Total Revenue'   AS line_item,
    SUM(CASE WHEN ACCT_TYPE = 'R' THEN balance ELSE 0 END) AS amount
FROM pl
UNION ALL
SELECT
    'Total Expenses',
    SUM(CASE WHEN ACCT_TYPE = 'X' THEN balance ELSE 0 END)
FROM pl
UNION ALL
SELECT
    'Net Income (Loss)',
    SUM(CASE WHEN ACCT_TYPE = 'R' THEN balance ELSE 0 END)
    - SUM(CASE WHEN ACCT_TYPE = 'X' THEN balance ELSE 0 END)
FROM pl;

-- ──────────────────────────────────────────────────────────────
--  SECTION 6: EXECUTIVE DASHBOARD VIEWS
-- ──────────────────────────────────────────────────────────────

-- 6.1 KPI snapshot
SELECT
    'Employees'             AS metric,
    CAST(COUNT(*) AS VARCHAR(20)) AS value
FROM EMPLOYEE WHERE STATUS = 'A'
UNION ALL
SELECT
    'Active Customers',
    CAST(COUNT(*) AS VARCHAR(20))
FROM CUSTOMER WHERE STATUS = 'A'
UNION ALL
SELECT
    'Open Orders',
    CAST(COUNT(*) AS VARCHAR(20))
FROM SALES_ORDER WHERE STATUS = 'OPEN'
UNION ALL
SELECT
    'Open Order Value',
    CAST(DECIMAL(SUM(SOL.QTY_ORDERED * SOL.UNIT_PRICE),13,2) AS VARCHAR(20))
FROM SALES_ORDER SO
JOIN SALES_ORDER_LINE SOL ON SOL.ORDER_ID = SO.ORDER_ID
WHERE SO.STATUS = 'OPEN'
UNION ALL
SELECT
    'Total Inventory Value',
    CAST(DECIMAL(SUM(IB.QTY_ON_HAND * I.STD_COST),13,2) AS VARCHAR(20))
FROM INVENTORY_BALANCE IB
JOIN ITEM_MASTER I ON I.ITEM_NUMBER = IB.ITEM_NUMBER;
