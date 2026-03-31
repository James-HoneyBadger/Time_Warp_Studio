"""
Setup script for HB Banking ERP database
Creates initial schema and populates with sample data.
"""
import os
import sys
sys.path.append(os.path.dirname(os.path.dirname(__file__)))

from data.init_db import initialize_database

def setup_database():
    initialize_database()
    print("HB Banking ERP database setup complete with demo data.")

if __name__ == "__main__":
    setup_database()
