"""
HB Banking ERP — GUI Main Entry Point
Standalone GUI banking ERP system using PySide6.
"""
import sys
from PySide6.QtWidgets import (
    QApplication, QMainWindow, QLabel, QPushButton, QVBoxLayout, QWidget,
    QDialog, QFormLayout, QLineEdit, QDialogButtonBox, QMessageBox, QTableWidget, QTableWidgetItem, QTabWidget,
    QHBoxLayout, QComboBox, QSpinBox, QDoubleSpinBox, QTextEdit, QGroupBox
)
from PySide6.QtCore import Qt
from .data import erp_data
from .modules import accounts, transactions, reports

class MainWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("HB Banking ERP Suite")
        self.setGeometry(100, 100, 1000, 700)
        self.logged_in_user = None
        self.user_id = None
        self.user_role = None
        self.init_ui()

    def init_ui(self):
        self.login_widget = self.create_login_widget()
        self.setCentralWidget(self.login_widget)

    def show_login_dialog(self):
        """Return to login screen (logout functionality)."""
        self.logged_in_user = None
        self.user_id = None
        self.user_role = None
        self.setCentralWidget(self.login_widget)
        # Clear any existing main interface
        if hasattr(self, 'main_tabs'):
            self.main_tabs.setParent(None)
            delattr(self, 'main_tabs')

    def create_login_widget(self):
        widget = QWidget()
        layout = QVBoxLayout()
        
        title = QLabel("HB Banking ERP Suite")
        title.setStyleSheet("font-size: 24px; font-weight: bold;")
        layout.addWidget(title, alignment=Qt.AlignCenter)
        
        form_group = QGroupBox("Login")
        form_layout = QFormLayout()
        
        self.username_input = QLineEdit()
        self.password_input = QLineEdit()
        self.password_input.setEchoMode(QLineEdit.Password)
        
        form_layout.addRow("Username:", self.username_input)
        form_layout.addRow("Password:", self.password_input)
        
        login_btn = QPushButton("Login")
        login_btn.clicked.connect(self.attempt_login)
        
        form_layout.addRow(login_btn)
        form_group.setLayout(form_layout)
        layout.addWidget(form_group, alignment=Qt.AlignCenter)
        
        widget.setLayout(layout)
        return widget

    def attempt_login(self):
        username = self.username_input.text()
        password = self.password_input.text()
        
        result = erp_data.authenticate_user(username, password)
        if result:
            self.user_id, self.user_role = result
            self.logged_in_user = username
            self.show_main_interface()
        else:
            QMessageBox.warning(self, "Login Failed", "Invalid username or password.")

    def show_main_interface(self):
        self.main_tabs = QTabWidget()
        
        # Accounts tab
        self.accounts_tab = self.create_accounts_tab()
        self.main_tabs.addTab(self.accounts_tab, "Accounts")
        
        # Transactions tab
        self.transactions_tab = self.create_transactions_tab()
        self.main_tabs.addTab(self.transactions_tab, "Transactions")
        
        # Budgets tab
        self.budgets_tab = self.create_budgets_tab()
        self.main_tabs.addTab(self.budgets_tab, "Budgets")
        
        # Reports tab
        self.reports_tab = self.create_reports_tab()
        self.main_tabs.addTab(self.reports_tab, "Reports")
        
        # Loans tab
        self.loans_tab = self.create_loans_tab()
        self.main_tabs.addTab(self.loans_tab, "Loans")
        
        # Admin tab (only for admin)
        if self.user_role == 'admin':
            self.admin_tab = self.create_admin_tab()
            self.main_tabs.addTab(self.admin_tab, "Admin")
        
        # Profile tab for all users
        self.profile_tab = self.create_profile_tab()
        self.main_tabs.addTab(self.profile_tab, "Profile")
        
        self.setCentralWidget(self.main_tabs)
        self.refresh_data()

    def create_accounts_tab(self):
        widget = QWidget()
        layout = QVBoxLayout()
        
        # Accounts table
        self.accounts_table = QTableWidget()
        self.accounts_table.setColumnCount(5)
        self.accounts_table.setHorizontalHeaderLabels(["ID", "Account Number", "Type", "Balance", "Status"])
        layout.addWidget(self.accounts_table)
        
        # Buttons
        btn_layout = QHBoxLayout()
        
        deposit_btn = QPushButton("Deposit")
        deposit_btn.clicked.connect(self.show_deposit_dialog)
        btn_layout.addWidget(deposit_btn)
        
        withdraw_btn = QPushButton("Withdraw")
        withdraw_btn.clicked.connect(self.show_withdraw_dialog)
        btn_layout.addWidget(withdraw_btn)
        
        transfer_btn = QPushButton("Transfer")
        transfer_btn.clicked.connect(self.show_transfer_dialog)
        btn_layout.addWidget(transfer_btn)
        
        if self.user_role == 'admin':
            open_btn = QPushButton("Open Account")
            open_btn.clicked.connect(self.show_open_account_dialog)
            btn_layout.addWidget(open_btn)
            
            interest_btn = QPushButton("Calculate Interest")
            interest_btn.clicked.connect(self.calculate_interest)
            btn_layout.addWidget(interest_btn)
        
        refresh_btn = QPushButton("Refresh")
        refresh_btn.clicked.connect(self.refresh_accounts)
        btn_layout.addWidget(refresh_btn)
        
        layout.addLayout(btn_layout)
        widget.setLayout(layout)
        return widget

    def create_transactions_tab(self):
        widget = QWidget()
        layout = QVBoxLayout()
        
        self.transactions_table = QTableWidget()
        self.transactions_table.setColumnCount(5)
        self.transactions_table.setHorizontalHeaderLabels(["Date", "Description", "Amount", "Category", "Account"])
        layout.addWidget(self.transactions_table)
        
        refresh_btn = QPushButton("Refresh Transactions")
        refresh_btn.clicked.connect(self.refresh_transactions)
        layout.addWidget(refresh_btn)
        
        widget.setLayout(layout)
        return widget

    def create_budgets_tab(self):
        widget = QWidget()
        layout = QVBoxLayout()
        
        self.budgets_table = QTableWidget()
        self.budgets_table.setColumnCount(4)
        self.budgets_table.setHorizontalHeaderLabels(["Category", "Budgeted", "Spent", "Remaining"])
        layout.addWidget(self.budgets_table)
        
        refresh_btn = QPushButton("Refresh Budgets")
        refresh_btn.clicked.connect(self.refresh_budgets)
        layout.addWidget(refresh_btn)
        
        widget.setLayout(layout)
        return widget

    def create_loans_tab(self):
        widget = QWidget()
        layout = QVBoxLayout()
        
        self.loans_table = QTableWidget()
        self.loans_table.setColumnCount(6)
        self.loans_table.setHorizontalHeaderLabels(["Loan ID", "Amount", "Interest Rate", "Term", "Monthly Payment", "Remaining Balance"])
        layout.addWidget(self.loans_table)
        
        btn_layout = QHBoxLayout()
        
        payment_btn = QPushButton("Make Payment")
        payment_btn.clicked.connect(self.show_loan_payment_dialog)
        btn_layout.addWidget(payment_btn)
        
        if self.user_role == 'admin':
            new_loan_btn = QPushButton("New Loan")
            new_loan_btn.clicked.connect(self.show_new_loan_dialog)
            btn_layout.addWidget(new_loan_btn)
        
        refresh_btn = QPushButton("Refresh Loans")
        refresh_btn.clicked.connect(self.refresh_loans)
        btn_layout.addWidget(refresh_btn)
        
        layout.addLayout(btn_layout)
        widget.setLayout(layout)
        return widget

    def create_reports_tab(self):
        widget = QWidget()
        layout = QVBoxLayout()
        
        report_layout = QHBoxLayout()
        
        stmt_btn = QPushButton("Account Statement")
        stmt_btn.clicked.connect(self.generate_statement)
        report_layout.addWidget(stmt_btn)
        
        budget_btn = QPushButton("Budget Report")
        budget_btn.clicked.connect(self.generate_budget_report)
        report_layout.addWidget(budget_btn)
        
        income_btn = QPushButton("Income/Expense")
        income_btn.clicked.connect(self.generate_income_report)
        report_layout.addWidget(income_btn)
        
        layout.addLayout(report_layout)
        
        self.report_output = QTextEdit()
        self.report_output.setReadOnly(True)
        layout.addWidget(self.report_output)
        
        widget.setLayout(layout)
        return widget

    def create_profile_tab(self):
        widget = QWidget()
        layout = QVBoxLayout()
        
        profile_group = QGroupBox("User Profile")
        profile_layout = QFormLayout()
        
        # Display current user info
        username_label = QLabel(self.logged_in_user)
        profile_layout.addRow("Username:", username_label)
        
        role_label = QLabel(self.user_role.title())
        profile_layout.addRow("Role:", role_label)
        
        # Password change
        password_group = QGroupBox("Change Password")
        password_layout = QFormLayout()
        
        self.current_password_input = QLineEdit()
        self.current_password_input.setEchoMode(QLineEdit.Password)
        password_layout.addRow("Current Password:", self.current_password_input)
        
        self.new_password_input = QLineEdit()
        self.new_password_input.setEchoMode(QLineEdit.Password)
        password_layout.addRow("New Password:", self.new_password_input)
        
        self.confirm_password_input = QLineEdit()
        self.confirm_password_input.setEchoMode(QLineEdit.Password)
        password_layout.addRow("Confirm Password:", self.confirm_password_input)
        
        change_btn = QPushButton("Change Password")
        change_btn.clicked.connect(self.change_password)
        password_layout.addRow(change_btn)
        
        password_group.setLayout(password_layout)
        
        layout.addWidget(profile_group)
        layout.addWidget(password_group)
        layout.addStretch()
        
        widget.setLayout(layout)
        return widget

    def create_admin_tab(self):
        widget = QWidget()
        layout = QVBoxLayout()
        
        admin_layout = QHBoxLayout()
        
        users_btn = QPushButton("Manage Users")
        users_btn.clicked.connect(self.show_user_management)
        admin_layout.addWidget(users_btn)
        
        health_btn = QPushButton("System Health")
        health_btn.clicked.connect(self.show_system_health)
        admin_layout.addWidget(health_btn)
        
        backup_btn = QPushButton("Backup Database")
        backup_btn.clicked.connect(self.backup_database)
        admin_layout.addWidget(backup_btn)
        
        layout.addLayout(admin_layout)
        
        self.admin_output = QTextEdit()
        self.admin_output.setReadOnly(True)
        layout.addWidget(self.admin_output)
        
        widget.setLayout(layout)
        return widget

    def refresh_data(self):
        self.refresh_accounts()
        self.refresh_transactions()
        self.refresh_budgets()
        self.refresh_loans()

    def refresh_accounts(self):
        accounts = erp_data.get_accounts(self.user_id)
        self.accounts_table.setRowCount(len(accounts))
        for row, acc in enumerate(accounts):
            for col, value in enumerate(acc):
                self.accounts_table.setItem(row, col, QTableWidgetItem(str(value)))

    def refresh_transactions(self):
        transactions = erp_data.get_transactions(user_id=self.user_id)
        self.transactions_table.setRowCount(len(transactions))
        for row, txn in enumerate(transactions):
            for col, value in enumerate(txn):
                self.transactions_table.setItem(row, col, QTableWidgetItem(str(value)))

    def refresh_budgets(self):
        budgets = erp_data.get_budgets(self.user_id)
        self.budgets_table.setRowCount(len(budgets))
        for row, budget in enumerate(budgets):
            id_, year, category, budgeted, spent = budget
            remaining = budgeted - spent
            self.budgets_table.setItem(row, 0, QTableWidgetItem(category))
            self.budgets_table.setItem(row, 1, QTableWidgetItem(f"${budgeted:.2f}"))
            self.budgets_table.setItem(row, 2, QTableWidgetItem(f"${spent:.2f}"))
            self.budgets_table.setItem(row, 3, QTableWidgetItem(f"${remaining:.2f}"))

    def refresh_loans(self):
        loans = erp_data.get_loans(self.user_id)
        self.loans_table.setRowCount(len(loans))
        for row, loan in enumerate(loans):
            if self.user_role == 'admin':
                loan_id, amount, rate, term, payment, balance = loan
            else:
                loan_id, amount, rate, term, payment, balance, account_num = loan
            self.loans_table.setItem(row, 0, QTableWidgetItem(str(loan_id)))
            self.loans_table.setItem(row, 1, QTableWidgetItem(f"${amount:.2f}"))
            self.loans_table.setItem(row, 2, QTableWidgetItem(f"{rate:.2f}%"))
            self.loans_table.setItem(row, 3, QTableWidgetItem(f"{term} months"))
            self.loans_table.setItem(row, 4, QTableWidgetItem(f"${payment:.2f}"))
            self.loans_table.setItem(row, 5, QTableWidgetItem(f"${balance:.2f}"))

    def show_deposit_dialog(self):
        dialog = QDialog(self)
        dialog.setWindowTitle("Deposit")
        layout = QFormLayout()
        
        account_combo = QComboBox()
        accounts = erp_data.get_accounts(self.user_id)
        for acc in accounts:
            account_combo.addItem(f"{acc[1]} ({acc[2]})", acc[0])
        
        amount_input = QDoubleSpinBox()
        amount_input.setMaximum(1000000)
        amount_input.setPrefix("$")
        
        desc_input = QLineEdit("Deposit")
        
        layout.addRow("Account:", account_combo)
        layout.addRow("Amount:", amount_input)
        layout.addRow("Description:", desc_input)
        
        buttons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttons.accepted.connect(lambda: self.process_deposit(account_combo.currentData(), amount_input.value(), desc_input.text(), dialog))
        buttons.rejected.connect(dialog.reject)
        layout.addRow(buttons)
        
        dialog.setLayout(layout)
        dialog.exec()

    def process_deposit(self, account_id, amount, description, dialog):
        try:
            transactions.deposit(self.user_id, account_id, amount, description)
            dialog.accept()
            self.refresh_data()
            QMessageBox.information(self, "Success", "Deposit completed successfully.")
        except Exception as e:
            QMessageBox.warning(self, "Error", f"Deposit failed: {e}")

    def show_withdraw_dialog(self):
        dialog = QDialog(self)
        dialog.setWindowTitle("Withdraw")
        layout = QFormLayout()
        
        account_combo = QComboBox()
        accounts = erp_data.get_accounts(self.user_id)
        for acc in accounts:
            account_combo.addItem(f"{acc[1]} ({acc[2]}) - Balance: ${acc[3]:.2f}", acc[0])
        
        amount_input = QDoubleSpinBox()
        amount_input.setMaximum(1000000)
        amount_input.setPrefix("$")
        
        desc_input = QLineEdit("Withdrawal")
        
        layout.addRow("Account:", account_combo)
        layout.addRow("Amount:", amount_input)
        layout.addRow("Description:", desc_input)
        
        buttons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttons.accepted.connect(lambda: self.process_withdraw(account_combo.currentData(), amount_input.value(), desc_input.text(), dialog))
        buttons.rejected.connect(dialog.reject)
        layout.addRow(buttons)
        
        dialog.setLayout(layout)
        dialog.exec()

    def process_withdraw(self, account_id, amount, description, dialog):
        try:
            transactions.withdraw(self.user_id, account_id, amount, description)
            dialog.accept()
            self.refresh_data()
            QMessageBox.information(self, "Success", "Withdrawal completed successfully.")
        except Exception as e:
            QMessageBox.warning(self, "Error", f"Withdrawal failed: {e}")

    def show_transfer_dialog(self):
        dialog = QDialog(self)
        dialog.setWindowTitle("Transfer")
        layout = QFormLayout()
        
        from_combo = QComboBox()
        to_combo = QComboBox()
        accounts = erp_data.get_accounts(self.user_id)
        for acc in accounts:
            from_combo.addItem(f"{acc[1]} ({acc[2]}) - Balance: ${acc[3]:.2f}", acc[0])
            to_combo.addItem(f"{acc[1]} ({acc[2]})", acc[0])
        
        amount_input = QDoubleSpinBox()
        amount_input.setMaximum(1000000)
        amount_input.setPrefix("$")
        
        desc_input = QLineEdit("Transfer")
        
        layout.addRow("From Account:", from_combo)
        layout.addRow("To Account:", to_combo)
        layout.addRow("Amount:", amount_input)
        layout.addRow("Description:", desc_input)
        
        buttons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttons.accepted.connect(lambda: self.process_transfer(from_combo.currentData(), to_combo.currentData(), amount_input.value(), desc_input.text(), dialog))
        buttons.rejected.connect(dialog.reject)
        layout.addRow(buttons)
        
        dialog.setLayout(layout)
        dialog.exec()

    def process_transfer(self, from_id, to_id, amount, description, dialog):
        if from_id == to_id:
            QMessageBox.warning(self, "Error", "Cannot transfer to the same account.")
            return
        try:
            transactions.transfer(self.user_id, from_id, to_id, amount, description)
            dialog.accept()
            self.refresh_data()
            QMessageBox.information(self, "Success", "Transfer completed successfully.")
        except Exception as e:
            QMessageBox.warning(self, "Error", f"Transfer failed: {e}")

    def show_open_account_dialog(self):
        if self.user_role != 'admin':
            return
        
        dialog = QDialog(self)
        dialog.setWindowTitle("Open New Account")
        layout = QFormLayout()
        
        user_combo = QComboBox()
        users = erp_data.get_users()
        for user in users:
            user_combo.addItem(f"{user[1]} ({user[2]})", user[0])
        
        account_input = QLineEdit()
        type_combo = QComboBox()
        type_combo.addItems(["Checking", "Savings", "Credit Card", "Loan"])
        
        balance_input = QDoubleSpinBox()
        balance_input.setPrefix("$")
        
        layout.addRow("User:", user_combo)
        layout.addRow("Account Number:", account_input)
        layout.addRow("Account Type:", type_combo)
        layout.addRow("Initial Balance:", balance_input)
        
        buttons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttons.accepted.connect(lambda: self.process_open_account(user_combo.currentData(), account_input.text(), type_combo.currentText(), balance_input.value(), dialog))
        buttons.rejected.connect(dialog.reject)
        layout.addRow(buttons)
        
        dialog.setLayout(layout)
        dialog.exec()

    def process_open_account(self, user_id, account_num, acc_type, balance, dialog):
        try:
            accounts.open_account(user_id, account_num, acc_type, balance)
            dialog.accept()
            self.refresh_data()
            QMessageBox.information(self, "Success", "Account opened successfully.")
        except Exception as e:
            QMessageBox.warning(self, "Error", f"Failed to open account: {e}")

    def calculate_interest(self):
        """Calculate interest for all savings accounts."""
        try:
            from ..data.erp_data import get_accounts
            accounts_list = get_accounts()
            total_interest = 0
            for acc in accounts_list:
                if acc[2] == 'Savings':  # account_type
                    interest = transactions.calculate_interest(acc[0])
                    total_interest += interest
            self.refresh_data()
            QMessageBox.information(self, "Success", f"Interest calculated. Total interest paid: ${total_interest:.2f}")
        except Exception as e:
            QMessageBox.warning(self, "Error", f"Interest calculation failed: {e}")

    def generate_statement(self):
        # Simple statement for first account
        accounts = erp_data.get_accounts(self.user_id)
        if not accounts:
            self.report_output.setText("No accounts found.")
            return
        
        account_id = accounts[0][0]
        statement = reports.generate_account_statement(account_id)
        text = "ACCOUNT STATEMENT\n\n"
        for date, desc, amt, cat, balance in statement:
            text += f"{date}: {desc} - ${amt:.2f} ({cat}) | Balance: ${balance:.2f}\n"
        self.report_output.setText(text)

    def generate_budget_report(self):
        report = reports.generate_budget_report(self.user_id)
        text = "BUDGET REPORT\n\n"
        for category, budgeted, spent, variance in report:
            text += f"{category}: Budgeted ${budgeted:.2f}, Spent ${spent:.2f}, Variance ${variance:.2f}\n"
        self.report_output.setText(text)

    def generate_income_report(self):
        report = reports.generate_income_expense_report(self.user_id)
        text = f"INCOME/EXPENSE REPORT ({report['period']})\n\n"
        text += f"Income: ${report['income']:.2f}\n"
        text += f"Expenses: ${report['expenses']:.2f}\n"
        text += f"Net: ${report['net']:.2f}\n"
        self.report_output.setText(text)

    def show_loan_payment_dialog(self):
        dialog = QDialog(self)
        dialog.setWindowTitle("Make Loan Payment")
        layout = QFormLayout()
        
        loan_combo = QComboBox()
        loans = erp_data.get_loans(self.user_id)
        for loan in loans:
            if self.user_role == 'admin':
                loan_id, amount, rate, term, payment, balance = loan
            else:
                loan_id, amount, rate, term, payment, balance, account_num = loan
            loan_combo.addItem(f"Loan {loan_id} - Balance: ${balance:.2f}", loan_id)
        
        amount_input = QDoubleSpinBox()
        amount_input.setMaximum(100000)
        amount_input.setPrefix("$")
        
        layout.addRow("Loan:", loan_combo)
        layout.addRow("Payment Amount:", amount_input)
        
        buttons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttons.accepted.connect(lambda: self.process_loan_payment(loan_combo.currentData(), amount_input.value(), dialog))
        buttons.rejected.connect(dialog.reject)
        layout.addRow(buttons)
        
        dialog.setLayout(layout)
        dialog.exec()

    def process_loan_payment(self, loan_id, amount, dialog):
        try:
            if erp_data.make_loan_payment(loan_id, amount):
                dialog.accept()
                self.refresh_loans()
                QMessageBox.information(self, "Success", "Loan payment processed successfully.")
            else:
                QMessageBox.warning(self, "Error", "Loan payment failed.")
        except Exception as e:
            QMessageBox.warning(self, "Error", f"Payment failed: {e}")

    def show_new_loan_dialog(self):
        if self.user_role != 'admin':
            return
        
        dialog = QDialog(self)
        dialog.setWindowTitle("Create New Loan")
        layout = QFormLayout()
        
        account_combo = QComboBox()
        accounts = erp_data.get_accounts()
        for acc in accounts:
            account_combo.addItem(f"{acc[1]} ({acc[2]})", acc[0])
        
        amount_input = QDoubleSpinBox()
        amount_input.setMaximum(1000000)
        amount_input.setPrefix("$")
        
        rate_input = QDoubleSpinBox()
        rate_input.setMaximum(50)
        rate_input.setSuffix("%")
        rate_input.setValue(5.0)
        
        term_input = QSpinBox()
        term_input.setMinimum(1)
        term_input.setMaximum(360)
        term_input.setValue(60)
        
        layout.addRow("Account:", account_combo)
        layout.addRow("Loan Amount:", amount_input)
        layout.addRow("Interest Rate:", rate_input)
        layout.addRow("Term (months):", term_input)
        
        buttons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttons.accepted.connect(lambda: self.process_new_loan(account_combo.currentData(), amount_input.value(), rate_input.value(), term_input.value(), dialog))
        buttons.rejected.connect(dialog.reject)
        layout.addRow(buttons)
        
        dialog.setLayout(layout)
        dialog.exec()

    def process_new_loan(self, account_id, amount, rate, term, dialog):
        try:
            erp_data.add_loan(account_id, amount, rate, term)
            dialog.accept()
            self.refresh_loans()
            QMessageBox.information(self, "Success", "Loan created successfully.")
        except Exception as e:
            QMessageBox.warning(self, "Error", f"Failed to create loan: {e}")

    def change_password(self):
        current = self.current_password_input.text()
        new = self.new_password_input.text()
        confirm = self.confirm_password_input.text()
        
        if not current or not new or not confirm:
            QMessageBox.warning(self, "Error", "All password fields are required.")
            return
        
        if new != confirm:
            QMessageBox.warning(self, "Error", "New passwords do not match.")
            return
        
        if len(new) < 6:
            QMessageBox.warning(self, "Error", "Password must be at least 6 characters.")
            return
        
        try:
            from ..security.auth import change_password
            if change_password(self.user_id, current, new):
                QMessageBox.information(self, "Success", "Password changed successfully.")
                self.current_password_input.clear()
                self.new_password_input.clear()
                self.confirm_password_input.clear()
            else:
                QMessageBox.warning(self, "Error", "Current password is incorrect.")
        except Exception as e:
            QMessageBox.warning(self, "Error", f"Password change failed: {e}")

    def show_user_management(self):
        # Simple user list for admin
        users = erp_data.get_users()
        text = "USER MANAGEMENT\n\n"
        for user in users:
            text += f"ID: {user[0]}, Username: {user[1]}, Role: {user[2]}, Email: {user[3]}\n"
        self.admin_output.setText(text)

    def show_system_health(self):
        from admin.admin_tools import system_health_check
        # Capture print output
        import io
        from contextlib import redirect_stdout
        
        f = io.StringIO()
        with redirect_stdout(f):
            system_health_check()
        self.admin_output.setText(f.getvalue())

    def backup_database(self):
        from maintenance.backup_db import backup_database
        try:
            path = backup_database()
            QMessageBox.information(self, "Success", f"Backup created at {path}")
        except Exception as e:
            QMessageBox.warning(self, "Error", f"Backup failed: {e}")

    def show_setup_dialog(self):
        QMessageBox.information(self, "Setup", "Setup is already complete. Demo data is preloaded.")

    def show_maintenance_dialog(self):
        QMessageBox.information(self, "Maintenance", "Use the Admin tab for maintenance operations.")

    def show_user_management(self):
        dialog = QDialog(self)
        dialog.setWindowTitle("User Management")
        layout = QVBoxLayout()
        
        # User list
        users = erp_data.get_users()
        user_list = QListWidget()
        for user in users:
            user_list.addItem(f"{user[1]} ({user[2]}) - {user[3]}")
        layout.addWidget(user_list)
        
        # Add user button
        add_btn = QPushButton("Add User")
        add_btn.clicked.connect(self.show_add_user_dialog)
        layout.addWidget(add_btn)
        
        dialog.setLayout(layout)
        dialog.exec()

    def show_add_user_dialog(self):
        dialog = QDialog(self)
        dialog.setWindowTitle("Add New User")
        layout = QFormLayout()
        
        username_input = QLineEdit()
        password_input = QLineEdit()
        password_input.setEchoMode(QLineEdit.Password)
        role_combo = QComboBox()
        role_combo.addItems(["user", "teller", "admin"])
        email_input = QLineEdit()
        
        layout.addRow("Username:", username_input)
        layout.addRow("Password:", password_input)
        layout.addRow("Role:", role_combo)
        layout.addRow("Email:", email_input)
        
        buttons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttons.accepted.connect(lambda: self.process_add_user(username_input.text(), password_input.text(), role_combo.currentText(), email_input.text(), dialog))
        buttons.rejected.connect(dialog.reject)
        layout.addRow(buttons)
        
        dialog.setLayout(layout)
        dialog.exec()

    def process_add_user(self, username, password, role, email, dialog):
        try:
            from data.erp_data import add_user
            user_id = add_user(username, password, role, email)
            dialog.accept()
            QMessageBox.information(self, "Success", f"User '{username}' added successfully.")
        except Exception as e:
            QMessageBox.warning(self, "Error", f"Failed to add user: {e}")

    def show_system_health(self):
        from admin.admin_tools import system_health_check
        import io
        from contextlib import redirect_stdout
        
        f = io.StringIO()
        with redirect_stdout(f):
            system_health_check()
        self.admin_output.setText(f.getvalue())

    def backup_database(self):
        from maintenance.backup_db import backup_database
        try:
            path = backup_database()
            QMessageBox.information(self, "Success", f"Backup created at {path}")
        except Exception as e:
            QMessageBox.warning(self, "Error", f"Backup failed: {e}")


if __name__ == "__main__":
    try:
        app = QApplication(sys.argv)
        window = MainWindow()
        window.show()
        sys.exit(app.exec())
    except Exception as e:
        import traceback
        print("❌ Exception occurred in ERP GUI:")
        traceback.print_exc()
