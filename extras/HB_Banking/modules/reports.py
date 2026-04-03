"""
Reporting module for HB Banking ERP
"""
from datetime import datetime, timedelta
from ..data.erp_data import get_connection


def generate_account_statement(account_id, start_date=None, end_date=None):
    """Generate account statement for a date range."""
    if not start_date:
        start_date = (datetime.now() - timedelta(days=30)).strftime('%Y-%m-%d')
    if not end_date:
        end_date = datetime.now().strftime('%Y-%m-%d')

    with get_connection() as conn:
        cur = conn.cursor()
        cur.execute("""
            SELECT t.date, t.description, t.amount, t.category, a.balance
            FROM transactions t
            JOIN accounts a ON t.account_id = a.id
            WHERE t.account_id = ? AND t.date BETWEEN ? AND ?
            ORDER BY t.date
        """, (account_id, start_date, end_date))
        transactions = cur.fetchall()

        # Calculate running balance
        cur.execute("SELECT balance FROM accounts WHERE id=?", (account_id,))
        current_balance = cur.fetchone()[0]

        # Reverse transactions to calculate running balance backwards
        running_balance = current_balance
        statement = []
        for txn in reversed(transactions):
            date, desc, amt, cat, _ = txn
            running_balance -= amt
            statement.append((date, desc, amt, cat, running_balance))

        return list(reversed(statement))


def generate_budget_report(user_id, year=2026):
    """Generate budget vs actual spending report."""
    with get_connection() as conn:
        cur = conn.cursor()
        cur.execute("""
            SELECT b.category, b.budgeted_amount, b.spent_amount,
                   (b.budgeted_amount - b.spent_amount) as variance
            FROM budgets b
            WHERE b.user_id = ? AND b.year = ?
        """, (user_id, year))
        return cur.fetchall()


def generate_income_expense_report(user_id, start_date=None, end_date=None):
    """Generate income vs expense report."""
    if not start_date:
        start_date = (datetime.now() - timedelta(days=30)).strftime('%Y-%m-%d')
    if not end_date:
        end_date = datetime.now().strftime('%Y-%m-%d')

    with get_connection() as conn:
        cur = conn.cursor()
        # Get all transactions for user's accounts
        cur.execute("""
            SELECT t.amount, t.category
            FROM transactions t
            JOIN accounts a ON t.account_id = a.id
            WHERE a.user_id = ? AND t.date BETWEEN ? AND ?
        """, (user_id, start_date, end_date))
        transactions = cur.fetchall()

        income = sum(amt for amt, cat in transactions if amt > 0)
        expenses = sum(abs(amt) for amt, cat in transactions if amt < 0)

        return {
            'income': income,
            'expenses': expenses,
            'net': income - expenses,
            'period': f"{start_date} to {end_date}"
        }


def generate_loan_report(user_id=None):
    """Generate loan summary report."""
    with get_connection() as conn:
        cur = conn.cursor()
        if user_id:
            cur.execute("""
                SELECT l.loan_amount, l.interest_rate, l.term_months, l.monthly_payment, l.remaining_balance, a.account_number
                FROM loans l
                JOIN accounts a ON l.account_id = a.id
                WHERE a.user_id = ?
            """, (user_id,))
        else:
            cur.execute("SELECT loan_amount, interest_rate, term_months, monthly_payment, remaining_balance FROM loans")
        return cur.fetchall()


def export_report_to_csv(data, filename, headers):
    """Export report data to CSV."""
    import csv
    with open(filename, 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(headers)
        writer.writerows(data)
    print(f"Report exported to {filename}")
