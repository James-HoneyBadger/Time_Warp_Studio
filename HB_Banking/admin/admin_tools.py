"""
Administrative tools for HB Banking ERP
"""
from ..data.erp_data import get_users, get_accounts, get_transactions, get_budgets, get_loans, add_user
from ..security.auth import authenticate

def list_users():
    users = get_users()
    print("Users:")
    for user in users:
        print(f"  ID: {user[0]}, Username: {user[1]}, Role: {user[2]}, Email: {user[3]}")

def list_accounts():
    accounts = get_accounts()
    print("Accounts:")
    for acc in accounts:
        print(f"  ID: {acc[0]}, Number: {acc[1]}, Type: {acc[2]}, Balance: {acc[3]:.2f}, Status: {acc[4]}")

def list_transactions(limit=20):
    transactions = get_transactions()
    print(f"Recent Transactions (last {limit}):")
    for txn in transactions[-limit:]:
        print(f"  ID: {txn[0]}, Date: {txn[1]}, Desc: {txn[2]}, Amount: {txn[3]:.2f}, Category: {txn[4]}")

def add_admin_user(username, password, email):
    user_id = add_user(username, password, 'admin', email)
    if user_id:
        print(f"Admin user '{username}' added with ID {user_id}")
    else:
        print("Failed to add user")

def system_health_check():
    """Check database integrity and basic stats."""
    try:
        users = len(get_users())
        accounts = len(get_accounts())
        transactions = len(get_transactions())
        print("System Health Check:")
        print(f"  Users: {users}")
        print(f"  Accounts: {accounts}")
        print(f"  Transactions: {transactions}")
        print("  Status: OK")
    except Exception as e:
        print(f"  Status: ERROR - {e}")

def generate_user_report():
    """Generate a report of all users and their account summaries."""
    users = get_users()
    print("User Report:")
    for user in users:
        user_id, username, role, email = user
        accounts = get_accounts(user_id)
        total_balance = sum(acc[3] for acc in accounts)
        print(f"  {username} ({role}): {len(accounts)} accounts, Total Balance: ${total_balance:.2f}")

if __name__ == "__main__":
    list_users()
    print()
    list_accounts()
    print()
    system_health_check()
