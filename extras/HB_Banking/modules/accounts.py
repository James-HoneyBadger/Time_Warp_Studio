"""
Account management module for HB Banking ERP
"""
from ..data.erp_data import get_connection, log_audit


def open_account(user_id, account_number, account_type, initial_balance=0.0, interest_rate=0.0):
    with get_connection() as conn:
        cur = conn.cursor()
        cur.execute("INSERT INTO accounts (user_id, account_number, account_type, balance, interest_rate) VALUES (?, ?, ?, ?, ?)",
                    (user_id, account_number, account_type, initial_balance, interest_rate))
        account_id = cur.lastrowid
        conn.commit()
        log_audit(user_id, 'CREATE', 'accounts', account_id, '', f'Account {account_number} opened')
        print(f"Account {account_number} opened for user {user_id}.")
        return account_id


def close_account(user_id, account_id):
    with get_connection() as conn:
        cur = conn.cursor()
        cur.execute("UPDATE accounts SET status='closed' WHERE id=? AND user_id=?", (account_id, user_id))
        if cur.rowcount > 0:
            conn.commit()
            log_audit(user_id, 'UPDATE', 'accounts', account_id, 'active', 'closed')
            print(f"Account {account_id} closed.")
        else:
            print("Account not found or not owned by user.")


def update_account(user_id, account_id, new_type=None, new_balance=None, new_rate=None):
    with get_connection() as conn:
        cur = conn.cursor()
        updates = []
        params = []
        if new_type:
            updates.append("account_type=?")
            params.append(new_type)
        if new_balance is not None:
            updates.append("balance=?")
            params.append(new_balance)
        if new_rate is not None:
            updates.append("interest_rate=?")
            params.append(new_rate)
        if updates:
            params.extend([account_id, user_id])
            cur.execute(f"UPDATE accounts SET {', '.join(updates)} WHERE id=? AND user_id=?", params)
            if cur.rowcount > 0:
                conn.commit()
                log_audit(user_id, 'UPDATE', 'accounts', account_id, '', f'Updated: {updates}')
                print(f"Account {account_id} updated.")
            else:
                print("Account not found or not owned by user.")


def get_account_details(account_id):
    with get_connection() as conn:
        cur = conn.cursor()
        cur.execute("SELECT * FROM accounts WHERE id=?", (account_id,))
        return cur.fetchone()
