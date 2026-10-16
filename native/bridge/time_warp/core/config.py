"""Centralized configuration paths for Time Warp Studio.

Every module that needs to read or write user-data files should import
constants from here instead of hard-coding ``Path.home() / ".time_warp"``
scattered across the codebase.

Usage::

    from time_warp.core.config import APP_DATA_DIR, LOGS_DIR

    log_file = LOGS_DIR / "ide.log"
"""

from __future__ import annotations

import os
from pathlib import Path

# ---- canonical application data directory --------------------------------
# All user data lives under  ~/.time_warp  (lowercase, dotfile).
#
# ``TIME_WARP_DATA_DIR`` overrides it, and the override exists because two
# callers have a real need for it:
#
#   * **A test that writes progress must not write the learner's file.** The
#     lesson tests record hints and completed checkpoints, and a suite that
#     edited the real ``progress.json`` would both corrupt somebody's place in
#     the curriculum and pass or fail depending on what a previous run left
#     behind.
#   * **A portable install** wants its data beside itself rather than in a home
#     directory it may not own.
_data_override = os.environ.get("TIME_WARP_DATA_DIR", "").strip()
APP_DATA_DIR: Path = Path(_data_override).expanduser() if _data_override else Path.home() / ".time_warp"

# ---- project root (source tree) ------------------------------------------
# Resolved relative to this file: config.py -> core/ -> time_warp/ -> Python/ -> Platforms/ -> repo root
PROJECT_ROOT: Path = Path(__file__).resolve().parent.parent.parent.parent.parent
EXAMPLES_DIR: Path = PROJECT_ROOT / "Examples"

# ---- sub-directories -----------------------------------------------------
LOGS_DIR: Path = APP_DATA_DIR / "logs"
GALLERY_DIR: Path = APP_DATA_DIR / "gallery"
HISTORY_DIR: Path = APP_DATA_DIR / "history"
BUNDLES_DIR: Path = APP_DATA_DIR / "bundles"
THEMES_DIR: Path = APP_DATA_DIR / "themes"
ASSETS_DIR: Path = APP_DATA_DIR / "assets"
WORKSPACE_DIR: Path = APP_DATA_DIR / "workspace"

# ---- well-known files ----------------------------------------------------
CONFIG_JSON: Path = APP_DATA_DIR / "config.json"
PROGRESS_JSON: Path = APP_DATA_DIR / "progress.json"

# ---- QSettings identifiers -----------------------------------------------
QSETTINGS_ORG: str = "TimeWarp"
QSETTINGS_APP: str = "IDE"
