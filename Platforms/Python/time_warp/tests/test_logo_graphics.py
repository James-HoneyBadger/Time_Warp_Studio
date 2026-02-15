"""
Tests for Logo Graphics and Turtle State
Verifies the core graphics engine functionality
"""

# pylint: disable=import-error,redefined-outer-name

import math

import pytest

from time_warp.graphics.turtle_state import TurtleState  # type: ignore[import-not-found]


class TestTurtleGraphics:

    @pytest.fixture
    def turtle(self):
        """Create a fresh turtle state for each test"""
        return TurtleState()

    def test_initial_state(self, turtle):
        """Test initial turtle state values"""
        assert turtle.x == 0.0
        assert turtle.y == 0.0
        assert turtle.heading == 0.0  # Facing up/north (0 degrees in standard Logo?)
        # Note: TurtleState docstring says "(0,0) is center, Y-axis inverted (up is negative)"
        # But heading conventions vary. Logo usually starts facing North.
        assert turtle.pen_down is True
        assert len(turtle.lines) == 0

    def test_movement(self, turtle):
        """Test forward movement logic"""
        # Test moving forward 100 units
        # If heading is 0 (North/Up) and Y is inverted (Up is negative),
        # then forward should decrease Y?
        # Let's check the implementation assumption by testing geometry.

        # Let's verify angle math.
        # Standard math: 0 is East, 90 is North.
        # Logo: 0 is North, 90 is East.

        # We need to verify what the engine implementation does.
        # Assuming standard Logo behavior:
        # FD 100 -> (0, -100) if 0 is North and Up is negative Y.

        start_x, start_y = turtle.x, turtle.y
        turtle.forward(100)

        # Verify a line was created
        assert len(turtle.lines) == 1
        line = turtle.lines[0]
        assert line.start_x == start_x
        assert line.start_y == start_y
        assert line.end_x == turtle.x
        assert line.end_y == turtle.y

        # Verify distance traveled
        dist = math.sqrt((turtle.x - start_x) ** 2 + (turtle.y - start_y) ** 2)
        assert math.isclose(dist, 100.0, rel_tol=1e-5)

    def test_rotation(self, turtle):
        """Test turning logic"""
        initial_heading = turtle.heading
        turtle.right(90)

        # If 0 is North, Right 90 should be East (90)
        # If using standard degrees, it might accumulate.
        assert turtle.heading != initial_heading

        turtle.left(90)
        assert math.isclose(turtle.heading, initial_heading, abs_tol=1e-5)

    def test_pen_state(self, turtle):
        """Test pen up/down behavior"""
        turtle.penup()
        assert not turtle.pen_down

        start_count = len(turtle.lines)
        turtle.forward(50)
        # Should not create line when pen is up
        assert len(turtle.lines) == start_count

        turtle.pendown()
        assert turtle.pen_down
        turtle.forward(50)
        assert len(turtle.lines) == start_count + 1

    def test_state_persistence(self, turtle):
        """Test that state is maintained between operations"""
        turtle.forward(50)
        turtle.right(90)
        turtle.forward(50)

        assert len(turtle.lines) == 2
        # Check continuity
        line1 = turtle.lines[0]
        line2 = turtle.lines[1]

        assert math.isclose(line1.end_x, line2.start_x, abs_tol=1e-5)
        assert math.isclose(line1.end_y, line2.start_y, abs_tol=1e-5)

    def test_color_handling(self, turtle):
        """Test color setting"""
        # Test RGB tuple
        turtle.setcolor(255, 0, 0)
        turtle.forward(10)
        assert turtle.lines[-1].color == (255, 0, 0)

        # Test cleaning/reset
        turtle.reset()
        assert len(turtle.lines) == 0
        assert turtle.x == 0
        assert turtle.y == 0
