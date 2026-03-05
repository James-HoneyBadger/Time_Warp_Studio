"""Centralized configuration paths for Time Warp Studio.

Every module that needs to read or write user-data files should import
constants from here instead of hard-coding ``Path.home() / ".time_warp"``
scattered across the codebase.

Usage::

    from time_warp.core.config import APP_DATA_DIR, LOGS_DIR

    log_file = LOGS_DIR / "ide.log"
"""

from __future__ import annotations

from pathlib import Path

# ---- canonical application data directory --------------------------------
# All user data lives under  ~/.time_warp  (lowercase, dotfile).
APP_DATA_DIR: Path = Path.home() / ".time_warp"

# ---- sub-directories -----------------------------------------------------
LOGS_DIR: Path = APP_DATA_DIR / "logs"
DATABASES_DIR: Path = APP_DATA_DIR / "databases"
GALLERY_DIR: Path = APP_DATA_DIR / "gallery"
HISTORY_DIR: Path = APP_DATA_DIR / "history"
BUNDLES_DIR: Path = APP_DATA_DIR / "bundles"
THEMES_DIR: Path = APP_DATA_DIR / "themes"
ASSETS_DIR: Path = APP_DATA_DIR / "assets"
HARDWARE_LOGS_DIR: Path = APP_DATA_DIR / "hardware_logs"

# ---- well-known files ----------------------------------------------------
CONFIG_JSON: Path = APP_DATA_DIR / "config.json"
PROGRESS_JSON: Path = APP_DATA_DIR / "progress.json"

# ---- QSettings identifiers -----------------------------------------------
QSETTINGS_ORG: str = "TimeWarp"
QSETTINGS_APP: str = "IDE"
