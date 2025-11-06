#!/usr/bin/env python3
"""Test graphics canvas display."""

import sys
from PySide6.QtWidgets import QApplication
from PySide6.QtCore import QTimer
from time_warp.ui import MainWindow

def test_graphics():
    """Test graphics display with various patterns."""
    
    # Create app and window
    app = QApplication(sys.argv)
    window = MainWindow()
    
    # Test programs
    test_programs = [
        ("Square", """FORWARD 100
RIGHT 90
FORWARD 100
RIGHT 90
FORWARD 100
RIGHT 90
FORWARD 100
RIGHT 90"""),
        
        ("Triangle", """FORWARD 150
RIGHT 120
FORWARD 150
RIGHT 120
FORWARD 150
RIGHT 120"""),
        
        ("Star", """REPEAT 5 [
  FORWARD 200
  RIGHT 144
]"""),
        
        ("Spiral", """REPEAT 36 [
  FORWARD 100
  RIGHT 10
]""")
    ]
    
    print("Graphics Canvas Test")
    print("=" * 60)
    print()
    
    # Load the first test program
    name, program = test_programs[0]
    window.editor.setPlainText(program)
    print(f"Loaded: {name}")
    print(f"Program length: {len(program)} chars")
    print()
    print("INSTRUCTIONS:")
    print("1. The IDE window should now be visible")
    print("2. Click 'Run' (F5) to execute the program")
    print("3. Switch to the 'Graphics' tab to see the turtle drawing")
    print("4. Use mouse wheel to zoom in/out")
    print("5. Middle-click or Ctrl+Click to pan")
    print()
    print("Available test programs:")
    for i, (test_name, _) in enumerate(test_programs):
        print(f"   {i+1}. {test_name}")
    print()
    print("TIP: The Y-axis is now flipped correctly!")
    print("     (0,0) is at center, Y goes UP (standard math/Logo)")
    print()
    
    window.show()
    window.resize(1200, 800)
    
    return app.exec()

if __name__ == '__main__':
    sys.exit(test_graphics())
