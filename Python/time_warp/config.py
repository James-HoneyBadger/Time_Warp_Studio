"""Configuration management for Time Warp IDE.

This module handles loading, saving, and applying user settings,
including theme, fonts, window geometry, and database defaults.
"""

import json
import os
from typing import Any, Dict, Optional


DEFAULT_SETTINGS: Dict[str, Any] = {
    "theme": "light",
    "editor_font_size": 11,
    "console_font_size": 11,
    "geometry": None,
    "show_line_numbers": True,
    # Default DB connection (used by Tools > Insert MySQL Boilerplate)
    "db_host": "localhost",
    "db_user": "root",
    "db_password": "",
    "db_name": "test",
    "db_port": 3306,
}


def config_path() -> str:
    """Return the path to the configuration file.

    Creates the config directory if it doesn't exist.

    Returns:
        str: Full path to config.json
    """
    base = os.path.expanduser("~/.time_warp")
    os.makedirs(base, exist_ok=True)
    return os.path.join(base, "config.json")


def load_settings() -> Dict[str, Any]:
    """Load settings from config file.

    Returns:
        Dict[str, Any]: Loaded settings merged with defaults
    """
    settings = DEFAULT_SETTINGS.copy()
    path = config_path()

    try:
        if os.path.isfile(path):
            with open(path, "r", encoding="utf-8") as f:
                data = json.load(f)
                if isinstance(data, dict):
                    settings.update(data)
    except (OSError, json.JSONDecodeError, ValueError):
        # Ignore settings load errors and use defaults
        pass

    return settings


def save_settings(
    settings: Dict[str, Any],
    geometry: Optional[str] = None,
) -> None:
    """Save settings to config file.

    Args:
        settings: Dictionary of settings to save
        geometry: Optional window geometry string
            (e.g., "800x600+100+50")
    """
    data = dict(settings)
    if geometry:
        data["geometry"] = geometry

    try:
        with open(config_path(), "w", encoding="utf-8") as f:
            json.dump(data, f, indent=2)
    except (OSError, TypeError):
        # Silently ignore save errors
        pass


def get_theme_colors(theme: str = "light") -> Dict[str, Dict[str, str]]:
    """Get color scheme for the specified theme.

    Args:
        theme: Theme name ('light' or 'dark')

    Returns:
        Dict with 'editor', 'line_numbers', and 'console' color configs
    """
    if theme.lower() == "dark":
        return {
            "editor": {"bg": "#1e1e1e", "fg": "#d4d4d4"},
            "line_numbers": {"bg": "#2b2b2b", "fg": "#bbbbbb"},
            "console": {"bg": "#1e1e1e", "fg": "#d4d4d4"},
        }
    else:
        return {
            "editor": {"bg": "white", "fg": "black"},
            "line_numbers": {"bg": "#f0f0f0", "fg": "black"},
            "console": {"bg": "white", "fg": "black"},
        }


def validate_font_size(value: Any, default: int = 11) -> int:
    """Validate and convert font size to integer.

    Args:
        value: Value to validate
        default: Default size if validation fails

    Returns:
        int: Validated font size
    """
    try:
        return int(str(value))
    except (ValueError, TypeError):
        return default
