"""
Theme Manager Plugin for Time Warp IDE
Provides advanced theme management and custom theme creation
"""

import json
import os
from pathlib import Path

# Plugin state
_plugin_state = {"context": None, "custom_themes": {}}


def initialize(context):
    """Initialize the theme manager plugin"""
    print("🎨 Theme Manager Plugin initialized")

    # Store context for later use
    _plugin_state["context"] = context

    # Add theme menu items
    context.add_menu_item("View/Themes", "Theme Manager", show_theme_manager)
    menu_category = "View/Themes"
    menu_label = "Create Custom Theme"
    context.add_menu_item(menu_category, menu_label, create_custom_theme)
    context.add_menu_item("View/Themes", "Export Theme", export_current_theme)
    context.add_menu_item("View/Themes", "Import Theme", import_theme)

    # Add quick theme switcher to toolbar
    context.add_toolbar_button("🎨", "Theme Manager", show_theme_manager)

    # Load custom themes
    load_custom_themes()

    return True


def cleanup(context):
    """Clean up theme manager plugin resources"""
    print("🎨 Theme Manager Plugin cleaned up")
    save_custom_themes()
    # Use context param to avoid unused-argument lint warnings
    _ = context
    _plugin_state["context"] = None


def show_theme_manager():
    """Show the theme manager dialog"""
    print("🎨 Opening Theme Manager...")

    themes = get_available_themes()
    print("\nAvailable Themes:")
    print("=" * 40)

    for i, theme in enumerate(themes, 1):
        status = " (current)" if theme.get("current") else ""
        print(f"{i}. {theme['name']} - {theme['description']}{status}")

    print("\nCommands:")
    print("  switch <number> - Switch to theme")
    print("  create - Create new theme")
    print("  export - Export current theme")
    print("  import - Import theme from file")


def create_custom_theme():
    """Create a new custom theme"""
    print("🎨 Creating custom theme...")

    theme_name = input("Enter theme name: ").strip()
    if not theme_name:
        print("❌ Theme name cannot be empty")
        return

    # Get base theme
    base_themes = [
        "Dracula",
        "Monokai",
        "Solarized Dark",
        "Ocean",
        "Spring",
        "Sunset",
        "Candy",
        "Forest",
    ]
    print("Available base themes:")
    for i, theme in enumerate(base_themes, 1):
        print(f"  {i}. {theme}")

    try:
        choice = int(input("Select base theme (number): ")) - 1
        base_theme = base_themes[choice]
    except (ValueError, IndexError):
        print("❌ Invalid selection")
        return

    # Create custom theme based on base
    custom_theme = create_theme_from_base(theme_name, base_theme)

    # Add to custom themes
    _plugin_state["custom_themes"][theme_name] = custom_theme
    save_custom_themes()

    print(f"✅ Custom theme '{theme_name}' created!")
    print("You can now modify it in the theme manager.")


def export_current_theme():
    """Export the current theme to a file"""
    print("🎨 Exporting current theme...")

    # Get current theme (would need IDE integration)
    current_theme_name = "Dracula"  # Placeholder
    theme_data = get_theme_data(current_theme_name)

    if not theme_data:
        print("❌ Could not get current theme data")
        return

    filename = f"{current_theme_name.lower().replace(' ', '_')}_theme.json"
    filepath = Path.home() / "Downloads" / filename

    try:
        with open(filepath, "w", encoding="utf-8") as f:
            json.dump(theme_data, f, indent=2)
        print(f"✅ Theme exported to: {filepath}")
    except OSError as e:
        print(f"❌ Failed to export theme: {e}")


def import_theme():
    """Import a theme from a file"""
    print("🎨 Importing theme...")

    filepath = input("Enter theme file path: ").strip()
    if not filepath:
        print("❌ File path cannot be empty")
        return

    if not os.path.exists(filepath):
        print("❌ File does not exist")
        return

    try:
        with open(filepath, "r", encoding="utf-8") as f:
            theme_data = json.load(f)

        theme_name = theme_data.get("name", "Imported Theme")
        _plugin_state["custom_themes"][theme_name] = theme_data
        save_custom_themes()

        print(f"✅ Theme '{theme_name}' imported successfully!")
        print("Use the theme manager to apply it.")

    except (OSError, json.JSONDecodeError) as e:
        print(f"❌ Failed to import theme: {e}")


def get_available_themes():
    """Get list of all available themes"""
    builtin_themes = [
        {
            "name": "Dracula",
            "description": "Dark theme with purple accents",
            "builtin": True,
        },
        {
            "name": "Monokai",
            "description": "Classic dark coding theme",
            "builtin": True,
        },
        {
            "name": "Solarized Dark",
            "description": "Low-contrast dark theme",
            "builtin": True,
        },
        {
            "name": "Ocean",
            "description": "Deep blue dark theme",
            "builtin": True,
        },
        {
            "name": "Spring",
            "description": "Light green theme",
            "builtin": True,
        },
        {
            "name": "Sunset",
            "description": "Warm orange theme",
            "builtin": True,
        },
        {
            "name": "Candy",
            "description": "Bright and colorful theme",
            "builtin": True,
        },
        {
            "name": "Forest",
            "description": "Nature-inspired green theme",
            "builtin": True,
        },
    ]

    # Iterate keys of custom_themes dict directly to satisfy pylint suggestion
    custom_themes = [
        {"name": name, "description": "Custom theme", "builtin": False}
        for name in _plugin_state["custom_themes"]
    ]

    return builtin_themes + custom_themes


def create_theme_from_base(theme_name, base_theme_name):
    """Create a custom theme based on a built-in theme"""
    # This would load the actual theme data from the IDE
    # For now, return a template
    return {
        "name": theme_name,
        "description": f"Custom theme based on {base_theme_name}",
        "version": "1.0.0",
        "base_theme": base_theme_name,
        "colors": {
            "background": "#2d2d2d",
            "foreground": "#f8f8f2",
            "selection": "#44475a",
            "line_highlight": "#44475a",
            "caret": "#f8f8f0",
            "syntax": {
                "keyword": "#ff79c6",
                "string": "#f1fa8c",
                "comment": "#6272a4",
                "number": "#bd93f9",
                "function": "#50fa7b",
                "variable": "#f8f8f2",
            },
        },
        "customizable": True,
    }


def get_theme_data(theme_name):
    """Get theme data for a specific theme"""
    if theme_name in _plugin_state["custom_themes"]:
        return _plugin_state["custom_themes"][theme_name]

    # For builtin themes, return template data
    return create_theme_from_base(theme_name, theme_name)


def load_custom_themes():
    """Load custom themes from disk"""

    theme_file = Path.home() / ".time_warp" / "custom_themes.json"

    if theme_file.exists():
        try:
            with open(theme_file, "r", encoding="utf-8") as f:
                _plugin_state["custom_themes"] = json.load(f)
            count = len(_plugin_state["custom_themes"])
            print(f"🎨 Loaded {count} custom themes")
        except (OSError, json.JSONDecodeError) as e:
            print(f"❌ Failed to load custom themes: {e}")
            _plugin_state["custom_themes"] = {}
    else:
        _plugin_state["custom_themes"] = {}


def save_custom_themes():
    """Save custom themes to disk"""
    theme_dir = Path.home() / ".time_warp"
    theme_dir.mkdir(exist_ok=True)

    theme_file = theme_dir / "custom_themes.json"

    try:
        with open(theme_file, "w", encoding="utf-8") as f:
            json.dump(_plugin_state["custom_themes"], f, indent=2)
        print(f"🎨 Saved {len(_plugin_state['custom_themes'])} custom themes")
    except OSError as e:
        print(f"❌ Failed to save custom themes: {e}")


def on_theme_changed(theme_name):
    """Hook called when theme changes"""
    print(f"🎨 Theme changed to: {theme_name}")


# Plugin metadata
PLUGIN_NAME = "Theme Manager Plugin"
PLUGIN_VERSION = "1.0.0"
PLUGIN_DESCRIPTION = "Advanced theme management for Time Warp IDE"

# Ensure plugin state contains expected fields (kept for runtime introspection)
_plugin_state["context"] = None
_plugin_state["custom_themes"] = {}
