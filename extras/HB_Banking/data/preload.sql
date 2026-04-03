-- Preload demo data for HB Banking ERP
-- Users (passwords hashed with SHA256)
CREATE TABLE IF NOT EXISTS users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT UNIQUE NOT NULL,
    password_hash TEXT NOT NULL,
    role TEXT NOT NULL,
    email TEXT,
    created_date TEXT DEFAULT CURRENT_TIMESTAMP
);
INSERT INTO users (username, password_hash, role, email) VALUES
    ('admin', '240be518fabd2724ddb6f04eeb1da5967448d7e831c08c8fa822809f74c720a9', 'admin', 'admin@hbbanking.com'),
    ('alice', '240be518fabd2724ddb6f04eeb1da5967448d7e831c08c8fa822809f74c720a9', 'user', 'alice@example.com'),
    ('bob', '240be518fabd2724ddb6f04eeb1da5967448d7e831c08c8fa822809f74c720a9', 'user', 'bob@example.com'),
    ('teller1', '240be518fabd2724ddb6f04eeb1da5967448d7e831c08c8fa822809f74c720a9', 'teller', 'teller@hbbanking.com'),
    ('auditor', '240be518fabd2724ddb6f04eeb1da5967448d7e831c08c8fa822809f74c720a9', 'auditor', 'audit@hbbanking.com');

-- Accounts
CREATE TABLE IF NOT EXISTS accounts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER,
    account_number TEXT UNIQUE NOT NULL,
    account_type TEXT NOT NULL,
    balance REAL NOT NULL DEFAULT 0.0,
    interest_rate REAL DEFAULT 0.0,
    opened_date TEXT DEFAULT CURRENT_TIMESTAMP,
    status TEXT DEFAULT 'active',
    FOREIGN KEY(user_id) REFERENCES users(id)
);
INSERT INTO accounts (user_id, account_number, account_type, balance, interest_rate) VALUES
    (1, '10001', 'Checking', 5000.00, 0.0),
    (2, '10002', 'Savings', 1200.00, 2.5),
    (3, '10003', 'Checking', 800.00, 0.0),
    (2, '10004', 'Credit Card', -200.00, 18.99),
    (3, '10005', 'Loan', -5000.00, 7.5),
    (4, '10006', 'Checking', 10000.00, 0.0);

-- Budgets
CREATE TABLE IF NOT EXISTS budgets (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER,
    year INTEGER,
    category TEXT,
    budgeted_amount REAL,
    spent_amount REAL DEFAULT 0.0,
    FOREIGN KEY(user_id) REFERENCES users(id)
);
INSERT INTO budgets (user_id, year, category, budgeted_amount, spent_amount) VALUES
    (1, 2026, 'Operations', 10000.00, 2500.00),
    (2, 2026, 'Personal', 3000.00, 800.00),
    (3, 2026, 'Personal', 2000.00, 1200.00),
    (2, 2026, 'Entertainment', 500.00, 150.00),
    (3, 2026, 'Groceries', 400.00, 300.00);

-- Transactions
CREATE TABLE IF NOT EXISTS transactions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    account_id INTEGER,
    date TEXT DEFAULT CURRENT_TIMESTAMP,
    description TEXT,
    amount REAL,
    category TEXT,
    reference TEXT,
    FOREIGN KEY(account_id) REFERENCES accounts(id)
);
INSERT INTO transactions (account_id, date, description, amount, category, reference) VALUES
    (1, '2026-03-01', 'Initial Deposit', 5000.00, 'Deposit', 'DEP001'),
    (2, '2026-03-02', 'Initial Deposit', 1200.00, 'Deposit', 'DEP002'),
    (3, '2026-03-03', 'Initial Deposit', 800.00, 'Deposit', 'DEP003'),
    (1, '2026-03-10', 'Office Supplies', -200.00, 'Expense', 'EXP001'),
    (2, '2026-03-12', 'ATM Withdrawal', -100.00, 'Withdrawal', 'ATM001'),
    (3, '2026-03-15', 'Groceries', -50.00, 'Expense', 'GROC001'),
    (4, '2026-03-20', 'Credit Card Payment', 200.00, 'Payment', 'PAY001'),
    (5, '2026-03-25', 'Loan Payment', -500.00, 'Payment', 'LOAN001'),
    (1, '2026-03-28', 'Salary Deposit', 3000.00, 'Income', 'SAL001');

-- Loans
CREATE TABLE IF NOT EXISTS loans (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    account_id INTEGER,
    loan_amount REAL,
    interest_rate REAL,
    term_months INTEGER,
    monthly_payment REAL,
    remaining_balance REAL,
    start_date TEXT DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY(account_id) REFERENCES accounts(id)
);
INSERT INTO loans (account_id, loan_amount, interest_rate, term_months, monthly_payment, remaining_balance) VALUES
    (5, 5000.00, 7.5, 60, 106.07, 4500.00);

-- Audit Log
CREATE TABLE IF NOT EXISTS audit_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER,
    action TEXT,
    table_name TEXT,
    record_id INTEGER,
    old_value TEXT,
    new_value TEXT,
    timestamp TEXT DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY(user_id) REFERENCES users(id)
);
INSERT INTO audit_log (user_id, action, table_name, record_id, new_value) VALUES
    (1, 'CREATE', 'users', 1, 'Admin user created'),
    (1, 'CREATE', 'accounts', 1, 'Account 10001 opened');
