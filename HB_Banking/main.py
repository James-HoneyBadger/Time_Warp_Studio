"""
HB Banking ERP — Main Entry Point
Standalone banking ERP system. Can also be launched from Time Warp Studio.
"""
import sys
import os

def main():
    print("Welcome to HB Banking ERP Suite!")
    
    # Ensure database is initialized
    from data.init_db import initialize_database
    initialize_database()
    
    # Launch GUI
    try:
        from gui_main import MainWindow, QApplication
        app = QApplication(sys.argv)
        window = MainWindow()
        window.show()
        sys.exit(app.exec())
    except ImportError:
        print("GUI not available. Use command-line interface.")
        show_cli_menu()

def show_cli_menu():
    while True:
        print("\n[1] Login\n[2] Setup\n[3] Maintenance\n[4] Exit")
        choice = input("Choose an option: ")
        
        if choice == '1':
            login_cli()
        elif choice == '2':
            setup_cli()
        elif choice == '3':
            maintenance_cli()
        elif choice == '4':
            break
        else:
            print("Invalid choice.")

def login_cli():
    from data import erp_data
    username = input("Username: ")
    password = input("Password: ")
    result = erp_data.authenticate_user(username, password)
    if result:
        user_id, role = result
        print(f"Login successful. Welcome {username} ({role})")
        # Could add CLI menu here
    else:
        print("Login failed.")

def setup_cli():
    from setup.setup_db import setup_database
    setup_database()

def maintenance_cli():
    from admin.admin_tools import system_health_check
    system_health_check()

if __name__ == "__main__":
    main()
