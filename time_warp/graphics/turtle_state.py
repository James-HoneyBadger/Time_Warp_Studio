"""
Turtle graphics state for Logo-style drawing
Ported from Rust: time_warp_unified::graphics::mod.rs
"""

import math
from typing import List, Tuple
from dataclasses import dataclass


@dataclass
class TurtleLine:
    """
    A line segment drawn by the turtle
    
    Represents a single draw operation with start/end points, color, and width.
    Used for rendering and export to image formats.
    """
    start_x: float
    start_y: float
    end_x: float
    end_y: float
    color: Tuple[int, int, int]  # RGB
    width: float


class TurtleState:
    """
    Turtle graphics state for Logo-style drawing
    
    Maintains turtle position, heading, pen state, and drawing history.
    Canvas coordinates: (0,0) is center, Y-axis inverted (up is negative).
    
    Example:
        >>> turtle = TurtleState()
        >>> turtle.forward(100)  # Move forward 100 pixels
        >>> turtle.right(90)     # Turn right 90 degrees
        >>> turtle.forward(100)  # Draw an L shape
    """
    
    def __init__(self):
        self.x: float = 0.0
        self.y: float = 0.0
        self.heading: float = 0.0  # degrees, 0 = up
        self.pen_down: bool = True
        self.pen_color: Tuple[int, int, int] = (255, 255, 255)  # White
        self.pen_width: float = 2.0
        self.canvas_width: float = 800.0
        self.canvas_height: float = 600.0
        self.lines: List[TurtleLine] = []
        self.visible: bool = True
        self.bg_color: Tuple[int, int, int] = (10, 10, 20)  # Dark background
    
    def forward(self, distance: float):
        """Move forward, drawing if pen is down"""
        rad = math.radians(self.heading)
        old_x = self.x
        old_y = self.y
        
        self.x += distance * math.sin(rad)
        self.y -= distance * math.cos(rad)  # Y inverted in screen coords
        
        if self.pen_down:
            self.lines.append(TurtleLine(
                old_x, old_y,
                self.x, self.y,
                self.pen_color,
                self.pen_width
            ))
    
    def back(self, distance: float):
        """Move backward"""
        self.forward(-distance)
    
    def left(self, angle: float):
        """Turn left (degrees)"""
        self.heading -= angle
        self.heading = self.heading % 360.0
    
    def right(self, angle: float):
        """Turn right (degrees)"""
        self.heading += angle
        self.heading = self.heading % 360.0
    
    def goto(self, x: float, y: float):
        """Move to absolute position, drawing if pen down"""
        if self.pen_down:
            self.lines.append(TurtleLine(
                self.x, self.y,
                x, y,
                self.pen_color,
                self.pen_width
            ))
        self.x = x
        self.y = y
    
    def home(self):
        """Return to center (0,0) and reset heading"""
        self.goto(0.0, 0.0)
        self.heading = 0.0
    
    def clear(self):
        """Clear all drawn lines"""
        self.lines.clear()
    
    def reset(self):
        """Reset turtle to initial state"""
        self.x = 0.0
        self.y = 0.0
        self.heading = 0.0
        self.pen_down = True
        self.pen_color = (255, 255, 255)
        self.pen_width = 2.0
        self.lines.clear()
        self.visible = True
    
    def penup(self):
        """Lift pen (stop drawing)"""
        self.pen_down = False
    
    def pendown(self):
        """Lower pen (start drawing)"""
        self.pen_down = True
    
    def setcolor(self, r: int, g: int, b: int):
        """Set pen color (RGB 0-255)"""
        self.pen_color = (r, g, b)
    
    def setpenwidth(self, width: float):
        """Set pen width"""
        self.pen_width = width
    
    def setbgcolor(self, r: int, g: int, b: int):
        """Set background color (RGB 0-255)"""
        self.bg_color = (r, g, b)
    
    def setheading(self, angle: float):
        """Set absolute heading (0 = up, clockwise)"""
        self.heading = angle % 360.0
    
    def hideturtle(self):
        """Hide turtle cursor"""
        self.visible = False
    
    def showturtle(self):
        """Show turtle cursor"""
        self.visible = True
