"""
Database backup utility for HB Banking ERP
"""
import shutil
import os
from datetime import datetime

DB_PATH = os.path.join(os.path.dirname(os.path.dirname(__file__)), 'data', 'erp_demo.db')

def backup_database(backup_dir="backups"):
    os.makedirs(backup_dir, exist_ok=True)
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    backup_path = os.path.join(backup_dir, f"hb_banking_backup_{timestamp}.db")
    shutil.copy2(DB_PATH, backup_path)
    print(f"Backup created at {backup_path}")
    return backup_path

def restore_database(backup_path):
    """Restore database from backup."""
    if not os.path.exists(backup_path):
        print(f"Backup file {backup_path} not found.")
        return False
    shutil.copy2(backup_path, DB_PATH)
    print(f"Database restored from {backup_path}")
    return True

def list_backups(backup_dir="backups"):
    """List available backups."""
    if not os.path.exists(backup_dir):
        print("No backups directory found.")
        return
    backups = [f for f in os.listdir(backup_dir) if f.startswith("hb_banking_backup_")]
    if not backups:
        print("No backups found.")
    else:
        print("Available backups:")
        for backup in sorted(backups, reverse=True):
            print(f"  {backup}")

if __name__ == "__main__":
    backup_database()
