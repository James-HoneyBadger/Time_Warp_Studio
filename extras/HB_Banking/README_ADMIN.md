# HB Banking ERP — Administrative & Maintenance Guide

## Overview
This guide covers setup, security, user management, maintenance, and integration with Time Warp Studio for the HB Banking ERP system.

## Setup
- Run `python setup/setup_db.py` to initialize the database.
- Use `admin/admin_tools.py` to list users and accounts.

## Security
- User authentication and roles are managed in `security/auth.py`.
- Add users with `add_user()`; roles: admin, teller, auditor, etc.

## Maintenance
- Backup the database: `python maintenance/backup_db.py`
- Restore by copying backup file to `hb_banking.db`.

## Integration with Time Warp Studio
- Launch from Studio: use `integration/twstudio_launcher.py`.
- Studio can perform technical/admin maintenance by invoking scripts in `setup/`, `maintenance/`, and `admin/`.

## User Tools
- Account management: `modules/accounts.py`
- Transaction processing: `modules/transactions.py`

## Extending the System
- Add new modules in `modules/` (e.g., reporting, audit logs)
- Enhance security in `security/`
- Add admin dashboards in `admin/`

## Troubleshooting
- Check database file `hb_banking.db` for integrity
- Review logs and script output for errors
