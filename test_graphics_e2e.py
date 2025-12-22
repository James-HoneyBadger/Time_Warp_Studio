#!/usr/bin/env python3
"""
End-to-end test for LOGO graphics rendering.
This tests the complete flow from code execution to canvas rendering.
"""

import sys
from PySide6.QtWidgets import QApplication
from PySide6.QtCore import Qt
from Platforms.Python.time_warp.ui.main_window import TimeWarpApp

def test_logo_graphics_e2e():
    """Test LOGO graphics rendering end-to-end."""
    
    # Create application
    app = QApplication(sys.argv)
    
    # Create main window
    window = TimeWarpApp()
    window.show()
    
    # Set code
    logo_code = """
FORWARD 100
RIGHT 90
FORWARD 100
RIGHT 90
FORWARD 100
RIGHT 90
FORWARD 100
"""
    
    # Set code in editor
    window.code_editor.setPlainText(logo_code)
    
    # Execute the code
    print("Executing LOGO code...")
    window.run_program()
    
    # Process events to allow execution to complete
    app.processEvents()
    
    # Wait a bit for execution to finish
    import time
    time.sleep(2)
    app.processEvents()
    
    # Check if canvas has lines
    canvas = window.output_panel.current_canvas
    if canvas:
        print(f"Canvas found!")
        print(f"Canvas has {len(canvas.lines)} lines")
        for i, line in enumerate(canvas.lines):
            print(f"  Line {i}: ({line.start_x}, {line.start_y}) -> ({line.end_x}, {line.end_y}), color={line.color}")
    else:
        print("ERROR: No canvas found!")
    
    # Check turtle state
    turtle = window.output_panel.current_canvas.turtle if canvas else None
    if turtle:
        print(f"Turtle has {len(turtle.lines)} lines")
        print(f"Turtle visible: {turtle.visible}")
        print(f"Turtle position: ({turtle.x}, {turtle.y})")
        print(f"Turtle heading: {turtle.heading}")
    else:
        print("ERROR: No turtle found!")
    
    # Try to check if Graphics tab is visible
    if window.right_tabs:
        print(f"Number of tabs: {window.right_tabs.count()}")
        current_index = window.right_tabs.currentIndex()
        current_text = window.right_tabs.tabText(current_index)
        print(f"Current tab: {current_index} ({current_text})")
        
        # Find Graphics tab
        for i in range(window.right_tabs.count()):
            print(f"  Tab {i}: {window.right_tabs.tabText(i)}")
    
    # Exit
    window.close()
    print("\nTest complete!")

if __name__ == "__main__":
    test_logo_graphics_e2e()
