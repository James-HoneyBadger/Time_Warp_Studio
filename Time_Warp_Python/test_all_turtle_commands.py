#!/usr/bin/env python3
"""Comprehensive test of all turtle graphics commands."""

from time_warp.core.interpreter import Interpreter
from time_warp.graphics.turtle_state import TurtleState


def test_command(name, code, expected_lines=None, check_fn=None):
    """Test a single command or set of commands."""
    print(f"Testing {name}...", end=" ")
    interp = Interpreter()
    turtle = TurtleState()
    interp.load_program(code)
    try:
        out = interp.execute(turtle)
        errors = [line for line in out if "❌" in line]
        if errors:
            print("❌ FAILED")
            for e in errors:
                print(f"  {e}")
            return False
        if expected_lines is not None and len(turtle.lines) != expected_lines:
            print(
                f"❌ FAILED: expected {expected_lines} lines, "
                f"got {len(turtle.lines)}"
            )
            return False
        if check_fn and not check_fn(turtle):
            print("❌ FAILED: check function failed")
            return False
        print("✅ PASSED")
        return True
    except Exception as e:
        print(f"❌ EXCEPTION: {e}")
        return False


def main():
    """Run the turtle graphics command verification."""
    print("=" * 60)
    print("TURTLE GRAPHICS COMMAND VERIFICATION")
    print("=" * 60)
    print()

    passed = 0
    failed = 0

    # Basic movement commands
    if test_command("FORWARD", "FORWARD 100", expected_lines=1):
        passed += 1
    else:
        failed += 1

    if test_command("FD (alias)", "FD 50", expected_lines=1):
        passed += 1
    else:
        failed += 1

    if test_command("BACK", "BACK 75", expected_lines=1):
        passed += 1
    else:
        failed += 1

    if test_command("BK (alias)", "BK 25", expected_lines=1):
        passed += 1
    else:
        failed += 1

    if test_command("BACKWARD (alias)", "BACKWARD 30", expected_lines=1):
        passed += 1
    else:
        failed += 1

    # Rotation commands
    if test_command("LEFT", "LEFT 90\nFORWARD 50", expected_lines=1):
        passed += 1
    else:
        failed += 1

    if test_command("LT (alias)", "LT 45\nFORWARD 50", expected_lines=1):
        passed += 1
    else:
        failed += 1

    if test_command("RIGHT", "RIGHT 90\nFORWARD 50", expected_lines=1):
        passed += 1
    else:
        failed += 1

    if test_command("RT (alias)", "RT 45\nFORWARD 50", expected_lines=1):
        passed += 1
    else:
        failed += 1

    # Expressions in rotation
    if test_command(
        "RIGHT with expression", "RIGHT 360 / 4\nFORWARD 50", expected_lines=1
    ):
        passed += 1
    else:
        failed += 1

    # Pen control
    if test_command(
        "PENUP/PENDOWN",
        "FORWARD 50\nPENUP\nFORWARD 50\nPENDOWN\nFORWARD 50",
        expected_lines=2,
    ):
        passed += 1
    else:
        failed += 1

    if test_command(
        "PU/PD (aliases)",
        "FORWARD 50\nPU\nFORWARD 50\nPD\nFORWARD 50",
        expected_lines=2,
    ):
        passed += 1
    else:
        failed += 1

    # Position commands
    if test_command("SETXY", "SETXY 100 100", expected_lines=1):
        passed += 1
    else:
        failed += 1

    if test_command("SETX", "SETX 50", expected_lines=1):
        passed += 1
    else:
        failed += 1

    if test_command("SETY", "SETY -50", expected_lines=1):
        passed += 1
    else:
        failed += 1

    if test_command("HOME", "FORWARD 100\nHOME", expected_lines=2):
        passed += 1
    else:
        failed += 1

    # Heading
    if test_command("SETHEADING", "SETHEADING 45\nFORWARD 50", expected_lines=1):
        passed += 1
    else:
        failed += 1

    if test_command("SETH (alias)", "SETH 90\nFORWARD 50", expected_lines=1):
        passed += 1
    else:
        failed += 1

    # Color commands - named colors
    if test_command(
        "SETCOLOR blue",
        "SETCOLOR blue\nFORWARD 50",
        check_fn=lambda t: t.lines and t.lines[0].color == (0, 0, 255),
    ):
        passed += 1
    else:
        failed += 1

    if test_command(
        "SETCOLOR red",
        "SETCOLOR red\nFORWARD 50",
        check_fn=lambda t: t.lines and t.lines[0].color == (255, 0, 0),
    ):
        passed += 1
    else:
        failed += 1

    if test_command(
        "SETCOLOR green",
        "SETCOLOR green\nFORWARD 50",
        check_fn=lambda t: t.lines and t.lines[0].color == (0, 255, 0),
    ):
        passed += 1
    else:
        failed += 1

    # Color commands - hex
    if test_command(
        "SETCOLOR #FF69B4",
        "SETCOLOR #FF69B4\nFORWARD 50",
        check_fn=lambda t: t.lines and t.lines[0].color == (255, 105, 180),
    ):
        passed += 1
    else:
        failed += 1

    # Color commands - RGB
    if test_command(
        "SETCOLOR RGB",
        "SETCOLOR 128 64 200\nFORWARD 50",
        check_fn=lambda t: t.lines and t.lines[0].color == (128, 64, 200),
    ):
        passed += 1
    else:
        failed += 1

    if test_command(
        "SETPENCOLOR",
        "SETPENCOLOR 200 100 50\nFORWARD 50",
        check_fn=lambda t: t.lines and t.lines[0].color == (200, 100, 50),
    ):
        passed += 1
    else:
        failed += 1

    if test_command(
        "SETPC (alias)",
        "SETPC 50 150 250\nFORWARD 50",
        check_fn=lambda t: t.lines and t.lines[0].color == (50, 150, 250),
    ):
        passed += 1
    else:
        failed += 1

    # Pen width
    if test_command(
        "PENWIDTH",
        "PENWIDTH 10\nFORWARD 50",
        check_fn=lambda t: t.lines and t.lines[0].width == 10,
    ):
        passed += 1
    else:
        failed += 1

    if test_command(
        "SETPENWIDTH",
        "SETPENWIDTH 5\nFORWARD 50",
        check_fn=lambda t: t.lines and t.lines[0].width == 5,
    ):
        passed += 1
    else:
        failed += 1

    if test_command(
        "SETPW (alias)",
        "SETPW 8\nFORWARD 50",
        check_fn=lambda t: t.lines and t.lines[0].width == 8,
    ):
        passed += 1
    else:
        failed += 1

    if test_command(
        "SETPENSIZE (alias)",
        "SETPENSIZE 12\nFORWARD 50",
        check_fn=lambda t: t.lines and t.lines[0].width == 12,
    ):
        passed += 1
    else:
        failed += 1

    # Background color
    if test_command(
        "SETBGCOLOR",
        "SETBGCOLOR 10 20 30",
        check_fn=lambda t: t.bg_color == (10, 20, 30),
    ):
        passed += 1
    else:
        failed += 1

    if test_command(
        "SETBG (alias)",
        "SETBG 50 100 150",
        check_fn=lambda t: t.bg_color == (50, 100, 150),
    ):
        passed += 1
    else:
        failed += 1

    # Clear screen
    if test_command("CLEARSCREEN", "FORWARD 50\nCLEARSCREEN", expected_lines=0):
        passed += 1
    else:
        failed += 1

    if test_command("CS (alias)", "FORWARD 50\nCS", expected_lines=0):
        passed += 1
    else:
        failed += 1

    if test_command("CLEAR (alias)", "FORWARD 50\nCLEAR", expected_lines=0):
        passed += 1
    else:
        failed += 1

    # Turtle visibility
    if test_command("HIDETURTLE", "HIDETURTLE", check_fn=lambda t: not t.visible):
        passed += 1
    else:
        failed += 1

    if test_command("HT (alias)", "HT", check_fn=lambda t: not t.visible):
        passed += 1
    else:
        failed += 1

    if test_command(
        "SHOWTURTLE", "HIDETURTLE\nSHOWTURTLE", check_fn=lambda t: t.visible
    ):
        passed += 1
    else:
        failed += 1

    if test_command("ST (alias)", "HT\nST", check_fn=lambda t: t.visible):
        passed += 1
    else:
        failed += 1

    # REPEAT command
    if test_command(
        "REPEAT single-line", "REPEAT 4 [FORWARD 50 RIGHT 90]", expected_lines=4
    ):
        passed += 1
    else:
        failed += 1

    # User's reported issue
    if test_command(
        "User's code (SETCOLOR blue + PENWIDTH)",
        """SETCOLOR blue
PENWIDTH 10
REPEAT 36 [
  FORWARD 120
  BACK 120
  RIGHT 10
]""",
        expected_lines=72,  # 36 * 2 lines (forward + back)
        check_fn=lambda t: t.lines
        and t.lines[0].color == (0, 0, 255)
        and t.lines[0].width == 10,
    ):
        passed += 1
    else:
        failed += 1

    print()
    print("=" * 60)
    print(f"RESULTS: {passed} passed, {failed} failed")
    print("=" * 60)

    if failed > 0:
        exit(1)


if __name__ == "__main__":
    main()
