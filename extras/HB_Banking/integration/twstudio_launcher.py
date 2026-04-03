"""
Integration script to launch HB Banking ERP from Time Warp Studio
"""
import subprocess
import os
import sys


def launch_hb_banking():
    """Launch HB Banking ERP GUI."""
    hb_main = os.path.join(os.path.dirname(__file__), '..', 'main.py')
    try:
        subprocess.Popen([sys.executable, hb_main])
        print("HB Banking ERP launched successfully.")
    except Exception as e:
        print(f"Failed to launch HB Banking ERP: {e}")


def get_erp_status():
    """Get basic ERP system status for Time Warp Studio."""
    try:
        from ..data.erp_data import get_users, get_accounts, get_transactions
        users = len(get_users())
        accounts = len(get_accounts())
        transactions = len(get_transactions())
        return {
            "status": "operational",
            "users": users,
            "accounts": accounts,
            "transactions": transactions
        }
    except Exception as e:
        return {"status": "error", "message": str(e)}


def perform_admin_task(task_name, **kwargs):
    """Allow Time Warp Studio to perform admin tasks."""
    try:
        if task_name == "backup":
            from ..maintenance.backup_db import backup_database
            return backup_database()
        elif task_name == "health_check":
            from ..admin.admin_tools import system_health_check
            # This prints, so we can't capture easily
            system_health_check()
            return "Health check completed"
        elif task_name == "add_user":
            from ..data.erp_data import add_user
            return add_user(kwargs['username'], kwargs['password'], kwargs['role'], kwargs.get('email', ''))
        else:
            return f"Unknown task: {task_name}"
    except Exception as e:
        return f"Task failed: {e}"


if __name__ == "__main__":
    launch_hb_banking()
