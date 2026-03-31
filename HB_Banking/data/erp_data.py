import sqlite3
import hashlib
import os

DB_PATH = os.path.join(os.path.dirname(__file__), 'erp_demo.db')

def get_connection():
    return sqlite3.connect(DB_PATH)

def hash_password(password):
    return hashlib.sha256(password.encode()).hexdigest()

def authenticate_user(username, password):
    try:
        with get_connection() as conn:
            cur = conn.cursor()
            cur.execute("SELECT id, role FROM users WHERE username=? AND password_hash=?", (username, hash_password(password)))
            return cur.fetchone()  # (id, role) or None
    except sqlite3.Error as e:
        print(f"Authentication error: {e}")
        return None

def get_accounts(user_id=None):
    with get_connection() as conn:
        cur = conn.cursor()
        if user_id:
            cur.execute("SELECT id, account_number, account_type, balance, status FROM accounts WHERE user_id=? AND status='active'", (user_id,))
        else:
            cur.execute("SELECT id, account_number, account_type, balance, status FROM accounts WHERE status='active'")
        return cur.fetchall()

def get_budgets(user_id=None):
    with get_connection() as conn:
        cur = conn.cursor()
        if user_id:
            cur.execute("SELECT id, year, category, budgeted_amount, spent_amount FROM budgets WHERE user_id=?", (user_id,))
        else:
            cur.execute("SELECT id, year, category, budgeted_amount, spent_amount FROM budgets")
        return cur.fetchall()

def get_transactions(account_id=None, user_id=None):
    with get_connection() as conn:
        cur = conn.cursor()
        if account_id:
            cur.execute("SELECT id, date, description, amount, category FROM transactions WHERE account_id=?", (account_id,))
        elif user_id:
            # Get transactions for all user's accounts
            account_ids = [row[0] for row in get_accounts(user_id)]
            if account_ids:
                placeholders = ','.join('?' * len(account_ids))
                cur.execute(f"SELECT t.id, t.date, t.description, t.amount, t.category, a.account_number FROM transactions t JOIN accounts a ON t.account_id = a.id WHERE t.account_id IN ({placeholders})", account_ids)
            else:
                return []
        else:
            cur.execute("SELECT id, date, description, amount, category FROM transactions")
        return cur.fetchall()

def get_users():
    with get_connection() as conn:
        cur = conn.cursor()
        cur.execute("SELECT id, username, role, email FROM users")
        return cur.fetchall()

def add_user(username, password, role, email=""):
    with get_connection() as conn:
        cur = conn.cursor()
        cur.execute("INSERT INTO users (username, password_hash, role, email) VALUES (?, ?, ?, ?)",
                    (username, hash_password(password), role, email))
        conn.commit()
        return cur.lastrowid

def update_account_balance(account_id, amount):
    with get_connection() as conn:
        cur = conn.cursor()
        cur.execute("UPDATE accounts SET balance = balance + ? WHERE id=?", (amount, account_id))
        conn.commit()

def add_transaction(account_id, description, amount, category="Other", reference=""):
    with get_connection() as conn:
        cur = conn.cursor()
        cur.execute("INSERT INTO transactions (account_id, description, amount, category, reference) VALUES (?, ?, ?, ?, ?)",
                    (account_id, description, amount, category, reference))
        conn.commit()
        return cur.lastrowid

def log_audit(user_id, action, table_name, record_id, old_value="", new_value=""):
    with get_connection() as conn:
        cur = conn.cursor()
        cur.execute("INSERT INTO audit_log (user_id, action, table_name, record_id, old_value, new_value) VALUES (?, ?, ?, ?, ?, ?)",
                    (user_id, action, table_name, record_id, old_value, new_value))
        conn.commit()

def get_loans(user_id=None):
    with get_connection() as conn:
        cur = conn.cursor()
        if user_id:
            cur.execute("""
                SELECT l.id, l.loan_amount, l.interest_rate, l.term_months, l.monthly_payment, l.remaining_balance, a.account_number
                FROM loans l
                JOIN accounts a ON l.account_id = a.id
                WHERE a.user_id = ?
            """, (user_id,))
        else:
            cur.execute("SELECT id, loan_amount, interest_rate, term_months, monthly_payment, remaining_balance FROM loans")
        return cur.fetchall()

def add_loan(account_id, loan_amount, interest_rate, term_months):
    """Add a new loan."""
    monthly_payment = (loan_amount * (interest_rate / 100 / 12)) / (1 - (1 + interest_rate / 100 / 12) ** (-term_months))
    with get_connection() as conn:
        cur = conn.cursor()
        cur.execute("INSERT INTO loans (account_id, loan_amount, interest_rate, term_months, monthly_payment, remaining_balance) VALUES (?, ?, ?, ?, ?, ?)",
                    (account_id, loan_amount, interest_rate, term_months, monthly_payment, loan_amount))
        conn.commit()
        return cur.lastrowid

def make_loan_payment(loan_id, payment_amount):
    """Make a payment on a loan."""
    with get_connection() as conn:
        cur = conn.cursor()
        cur.execute("SELECT remaining_balance, monthly_payment FROM loans WHERE id = ?", (loan_id,))
        row = cur.fetchone()
        if not row:
            return False
        
        remaining_balance, monthly_payment = row
        if payment_amount > remaining_balance:
            payment_amount = remaining_balance
        
        new_balance = remaining_balance - payment_amount
        cur.execute("UPDATE loans SET remaining_balance = ? WHERE id = ?", (new_balance, loan_id))
        conn.commit()
        return True
