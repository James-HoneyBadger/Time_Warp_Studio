import sqlite3
import os

DB_PATH = os.path.join(os.path.dirname(__file__), 'erp_demo.db')
SQL_PATH = os.path.join(os.path.dirname(__file__), 'preload.sql')


def initialize_database():
    if os.path.exists(DB_PATH):
        print(f"Database already exists at {DB_PATH}")
        return
    with sqlite3.connect(DB_PATH) as conn:
        with open(SQL_PATH, 'r') as f:
            sql = f.read()
        conn.executescript(sql)
        print(f"Database initialized at {DB_PATH} with demo data")


if __name__ == "__main__":
    initialize_database()
