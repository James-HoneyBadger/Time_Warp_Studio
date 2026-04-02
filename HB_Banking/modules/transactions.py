"""
Transaction processing module for HB Banking ERP
"""
from ..data.erp_data import get_connection, update_account_balance, add_transaction, log_audit


def deposit(user_id, account_id, amount, description="Deposit", category="Deposit"):
    if amount <= 0:
        print("Deposit amount must be positive.")
        return
    update_account_balance(account_id, amount)
    txn_id = add_transaction(account_id, description, amount, category)
    log_audit(user_id, 'CREATE', 'transactions', txn_id, '', f'Deposit {amount}')
    print(f"Deposited {amount} to account {account_id}.")


def withdraw(user_id, account_id, amount, description="Withdrawal", category="Withdrawal"):
    if amount <= 0:
        print("Withdrawal amount must be positive.")
        return
    with get_connection() as conn:
        cur = conn.cursor()
        cur.execute("SELECT balance FROM accounts WHERE id=? AND user_id=?", (account_id, user_id))
        row = cur.fetchone()
        if not row or row[0] < amount:
            print("Insufficient funds.")
            return
    update_account_balance(account_id, -amount)
    txn_id = add_transaction(account_id, description, -amount, category)
    log_audit(user_id, 'CREATE', 'transactions', txn_id, '', f'Withdrawal {amount}')
    print(f"Withdrew {amount} from account {account_id}.")


def calculate_interest(account_id, days=30):
    """Calculate and apply interest to savings accounts."""
    from ..data.erp_data import get_connection, add_transaction, log_audit

    with get_connection() as conn:
        cur = conn.cursor()
        cur.execute("SELECT a.balance, a.interest_rate, u.id FROM accounts a JOIN users u ON a.user_id = u.id WHERE a.id = ? AND a.account_type = 'Savings'", (account_id,))
        row = cur.fetchone()
        if not row:
            return 0

        balance, rate, user_id = row
        if balance > 0 and rate > 0:
            interest = balance * (rate / 100) * (days / 365)
            if interest > 0:
                add_transaction(account_id, f"Interest earned ({days} days)", interest, "Interest")
                log_audit(user_id, 'CREATE', 'transactions', 0, '', f'Interest {interest:.2f} on account {account_id}')
                return interest
    return 0


def get_transaction_history(account_id, limit=50):
    with get_connection() as conn:
        cur = conn.cursor()
        cur.execute("SELECT date, description, amount, category FROM transactions WHERE account_id=? ORDER BY date DESC LIMIT ?",
                    (account_id, limit))
        return cur.fetchall()
