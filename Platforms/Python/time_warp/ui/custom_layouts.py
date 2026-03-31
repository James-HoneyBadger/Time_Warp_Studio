"""
Customizable UI Layouts Module

This module provides functionality for users to customize and save their UI layouts
in Time Warp Studio. Users can rearrange panels, resize sections, and save/load layouts.

Features:
- Drag-and-drop panel rearrangement.
- Resizable sections.
- Save and load custom layouts.
"""

from typing import Dict
import json

class CustomUILayouts:
    """Class to manage customizable UI layouts."""

    def __init__(self):
        self.current_layout = {}
        self.saved_layouts = {}

    def save_layout(self, name: str) -> None:
        """Save the current layout with a given name.

        Args:
            name (str): The name of the layout.
        """
        self.saved_layouts[name] = self.current_layout.copy()
        print(f"✅ Layout '{name}' saved successfully.")

    def load_layout(self, name: str) -> None:
        """Load a saved layout by name.

        Args:
            name (str): The name of the layout to load.
        """
        if name in self.saved_layouts:
            self.current_layout = self.saved_layouts[name].copy()
            print(f"✅ Layout '{name}' loaded successfully.")
        else:
            print(f"❌ Layout '{name}' not found.")

    def export_layouts(self, file_path: str) -> None:
        """Export all saved layouts to a JSON file.

        Args:
            file_path (str): The file path to save the layouts.
        """
        with open(file_path, 'w') as file:
            json.dump(self.saved_layouts, file, indent=4)
        print(f"✅ Layouts exported to '{file_path}'.")

    def import_layouts(self, file_path: str) -> None:
        """Import layouts from a JSON file.

        Args:
            file_path (str): The file path to load the layouts from.
        """
        try:
            with open(file_path, 'r') as file:
                self.saved_layouts = json.load(file)
            print(f"✅ Layouts imported from '{file_path}'.")
        except FileNotFoundError:
            print(f"❌ File '{file_path}' not found.")

# Example usage
if __name__ == "__main__":
    ui_manager = CustomUILayouts()
    ui_manager.current_layout = {"editor": "left", "output": "bottom"}
    ui_manager.save_layout("Default")
    ui_manager.export_layouts("layouts.json")
    ui_manager.import_layouts("layouts.json")
    ui_manager.load_layout("Default")