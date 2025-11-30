"""
Sample Plugin for Time Warp IDE
Demonstrates basic plugin functionality
"""


def initialize(context):
    """Initialize the plugin with the provided context"""
    print("🎨 Sample Plugin initialized")

    # Example: Add a menu item
    context.add_menu_item("Plugins/Sample", "Show Message", show_sample_message)

    # Example: Add a toolbar button
    context.add_toolbar_button("⭐", "Sample Plugin Action", show_sample_message)

    return True


def cleanup(context):
    """Clean up plugin resources"""
    print("🎨 Sample Plugin cleaned up")
    _ = context  # Reserved for future use


def show_sample_message():
    """Sample plugin action"""
    print("ℹ️ Hello from Sample Plugin!")
    print("✅ Plugin system is working correctly")


def on_file_open(file_path):
    """Hook called when a file is opened"""
    print(f"📁 Sample Plugin: File opened - {file_path}")


def on_code_execute(code):
    """Hook called when code is executed"""
    print(f"🚀 Sample Plugin: Code executed - {len(code)} characters")


# Plugin metadata
PLUGIN_NAME = "Sample Plugin"
PLUGIN_VERSION = "1.0.0"
PLUGIN_DESCRIPTION = "Demonstrates Time Warp IDE plugin capabilities"
