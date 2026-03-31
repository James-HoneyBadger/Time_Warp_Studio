# HB Banking ERP Suite

A full-featured, standalone banking ERP system with user security, admin, maintenance, and setup tools. Can be run independently or integrated with Time Warp Studio for technical and administrative maintenance.

## Features
- Account management (open, close, update)
- Transaction processing (deposit, withdraw, transfer)
- User authentication and security
- Administrative dashboard
- Maintenance and setup utilities
- SQL database backend
- Integration hooks for Time Warp Studio

## Directory Structure
- `modules/` — Core banking modules (accounts, transactions, reporting)
- `admin/` — Administrative tools and dashboards
- `security/` — User authentication, roles, and permissions
- `maintenance/` — Database and system maintenance scripts
- `setup/` — Initial setup and configuration tools
- `integration/` — Studio integration scripts and APIs

## Usage
- Run as a standalone application: `python main.py`
- Launch from Time Warp Studio: IDE menu → Tools → Launch HB Banking
