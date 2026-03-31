#!/usr/bin/env python3
"""
Simple test script for HB Banking ERP
"""
import sys
import os
sys.path.insert(0, os.path.dirname(__file__))

from data.erp_data import authenticate_user, get_connection

def test_auth():
    print("Testing authentication...")
    try:
        result = authenticate_user('admin', 'admin123')
        if result:
            print(f"✅ Login successful: {result}")
        else:
            print("❌ Login failed")
    except Exception as e:
        print(f"❌ Error: {e}")

def test_db():
    print("Testing database connection...")
    try:
        with get_connection() as conn:
            cur = conn.cursor()
            cur.execute("SELECT COUNT(*) FROM users")
            count = cur.fetchone()[0]
            print(f"✅ Database OK, {count} users found")
    except Exception as e:
        print(f"❌ Database error: {e}")

if __name__ == "__main__":
    test_db()
    test_auth()
    print("Test complete!")