#!/usr/bin/env python3
"""Comprehensive test of all language commands."""

import sys

from time_warp.core.interpreter import Interpreter, Language
from time_warp.graphics.turtle_state import TurtleState


def test_pilot_commands():
    """Test all PILOT commands."""
    print("=" * 70)
    print("TESTING PILOT COMMANDS")
    print("=" * 70)
    print()

    results = []

    # T: - Type (print)
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program(
            language=Language.PILOT,
            program_text="T:Hello World\nE:",
        )
        output = interp.execute(turtle)
        assert "Hello World" in str(output)
        results.append(("T: (Type/Print)", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("T: (Type/Print)", f"❌ {e}"))

    # C: - Compute
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program(
            language=Language.PILOT, program_text="C:X = 5 + 3\nU:X\nE:"
        )
        output = interp.execute(turtle)
        assert "8" in str(output)
        results.append(("C: (Compute)", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("C: (Compute)", f"❌ {e}"))

    # U: - Use (print variable)
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program(
            language=Language.PILOT,
            program_text="C:Y = 42\nU:Y\nE:",
        )
        output = interp.execute(turtle)
        assert "42" in str(output)
        results.append(("U: (Use/Print Variable)", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("U: (Use/Print Variable)", f"❌ {e}"))

    # J: - Jump
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program(
            language=Language.PILOT,
            program_text="T:Start\nJ:END\nT:Skipped\nL:END\nT:End\nE:",
        )
        output = interp.execute(turtle)
        assert "Start" in str(output) and "End" in str(output)
        assert "Skipped" not in str(output)
        results.append(("J: (Jump)", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("J: (Jump)", f"❌ {e}"))

    # L: - Label
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program(
            language=Language.PILOT, program_text="L:START\nT:At label\nE:"
        )
        output = interp.execute(turtle)
        assert "At label" in str(output)
        results.append(("L: (Label)", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("L: (Label)", f"❌ {e}"))

    # R: - Remark (comment)
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program(
            language=Language.PILOT,
            program_text="R:This is a comment\nT:Text\nE:",
        )
        output = interp.execute(turtle)
        assert "Text" in str(output)
        assert "comment" not in str(output)
        results.append(("R: (Remark/Comment)", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("R: (Remark/Comment)", f"❌ {e}"))

    # E: - End
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program(
            language=Language.PILOT,
            program_text="T:Before end\nE:\nT:After end",
        )
        output = interp.execute(turtle)
        assert "Before end" in str(output)
        assert "After end" not in str(output)
        results.append(("E: (End)", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("E: (End)", f"❌ {e}"))

    # Print results
    for cmd, status in results:
        print(f"  {status} {cmd}")

    passed = sum(1 for _, s in results if s == "✅")
    print()
    print(f"PILOT: {passed}/{len(results)} commands passed")
    print()
    return passed == len(results)


def test_basic_commands():
    """Test all BASIC commands."""
    print("=" * 70)
    print("TESTING BASIC COMMANDS")
    print("=" * 70)
    print()

    results = []

    # PRINT
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program('10 PRINT "Hello"\n20 PRINT 123')
        output = interp.execute(turtle)
        assert "Hello" in str(output) and "123" in str(output)
        results.append(("PRINT", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("PRINT", f"❌ {e}"))

    # LET
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program("10 LET X = 10\n20 PRINT X")
        output = interp.execute(turtle)
        assert "10" in str(output)
        results.append(("LET", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("LET", f"❌ {e}"))

    # GOTO
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program(
            '10 PRINT "Start"\n20 GOTO 40\n30 PRINT "Skip"\n40 PRINT "End"'
        )
        output = interp.execute(turtle)
        assert "Start" in str(output) and "End" in str(output)
        assert "Skip" not in str(output)
        results.append(("GOTO", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("GOTO", f"❌ {e}"))

    # IF/THEN
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program('10 X = 5\n20 IF X = 5 THEN PRINT "Yes"')
        output = interp.execute(turtle)
        assert "Yes" in str(output)
        results.append(("IF/THEN", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("IF/THEN", f"❌ {e}"))

    # FOR/NEXT
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program("10 FOR I = 1 TO 3\n20 PRINT I\n30 NEXT I")
        output = interp.execute(turtle)
        assert "1" in str(output) and "2" in str(output) and "3" in str(output)
        results.append(("FOR/NEXT", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("FOR/NEXT", f"❌ {e}"))

    # GOSUB/RETURN
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program('10 GOSUB 30\n20 END\n30 PRINT "Sub"\n40 RETURN')
        output = interp.execute(turtle)
        assert "Sub" in str(output)
        results.append(("GOSUB/RETURN", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("GOSUB/RETURN", f"❌ {e}"))

    # REM
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program('10 REM This is a comment\n20 PRINT "OK"')
        output = interp.execute(turtle)
        assert "OK" in str(output)
        assert "comment" not in str(output)
        results.append(("REM", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("REM", f"❌ {e}"))

    # END
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program('10 PRINT "Before"\n20 END\n30 PRINT "After"')
        output = interp.execute(turtle)
        assert "Before" in str(output)
        assert "After" not in str(output)
        results.append(("END", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("END", f"❌ {e}"))

    # CLS
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program("10 CLS")
        output = interp.execute(turtle)
        results.append(("CLS", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("CLS", f"❌ {e}"))

    # SCREEN
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program("10 SCREEN 11")
        output = interp.execute(turtle)
        results.append(("SCREEN", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("SCREEN", f"❌ {e}"))

    # LOCATE
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program("10 LOCATE 5, 10")
        output = interp.execute(turtle)
        results.append(("LOCATE", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("LOCATE", f"❌ {e}"))

    # Print results
    for cmd, status in results:
        print(f"  {status} {cmd}")

    passed = sum(1 for _, s in results if s == "✅")
    print()
    print(f"BASIC: {passed}/{len(results)} commands passed")
    print()
    return passed == len(results)


def test_logo_commands():
    """Test all Logo commands."""
    print("=" * 70)
    print("TESTING LOGO COMMANDS")
    print("=" * 70)
    print()

    results = []

    # FORWARD/FD
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program(language=Language.LOGO, program_text="FORWARD 50")
        output = interp.execute(turtle)
        assert len(turtle.lines) > 0
        results.append(("FORWARD/FD", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("FORWARD/FD", f"❌ {e}"))

    # BACK/BK
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program(language=Language.LOGO, program_text="BACK 50")
        output = interp.execute(turtle)
        assert len(turtle.lines) > 0
        results.append(("BACK/BK", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("BACK/BK", f"❌ {e}"))

    # LEFT/LT
    try:
        interp = Interpreter()
        turtle = TurtleState()
        start_heading = turtle.heading
        interp.load_program(language=Language.LOGO, program_text="LEFT 90")
        output = interp.execute(turtle)
        assert turtle.heading != start_heading
        results.append(("LEFT/LT", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("LEFT/LT", f"❌ {e}"))

    # RIGHT/RT
    try:
        interp = Interpreter()
        turtle = TurtleState()
        start_heading = turtle.heading
        interp.load_program(language=Language.LOGO, program_text="RIGHT 90")
        output = interp.execute(turtle)
        assert turtle.heading != start_heading
        results.append(("RIGHT/RT", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("RIGHT/RT", f"❌ {e}"))

    # PENUP/PU
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program(language=Language.LOGO, program_text="PENUP")
        output = interp.execute(turtle)
        assert not turtle.pen_down
        results.append(("PENUP/PU", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("PENUP/PU", f"❌ {e}"))

    # PENDOWN/PD
    try:
        interp = Interpreter()
        turtle = TurtleState()
        turtle.pen_down = False
        interp.load_program(language=Language.LOGO, program_text="PENDOWN")
        output = interp.execute(turtle)
        assert turtle.pen_down
        results.append(("PENDOWN/PD", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("PENDOWN/PD", f"❌ {e}"))

    # HOME
    try:
        interp = Interpreter()
        turtle = TurtleState()
        turtle.x = 100
        turtle.y = 100
        interp.load_program(language=Language.LOGO, program_text="HOME")
        output = interp.execute(turtle)
        assert abs(turtle.x) < 1 and abs(turtle.y) < 1
        results.append(("HOME", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("HOME", f"❌ {e}"))

    # CLEARSCREEN/CS
    try:
        interp = Interpreter()
        turtle = TurtleState()
        turtle.lines.append(None)  # Add dummy line
        interp.load_program(language=Language.LOGO, program_text="CLEARSCREEN")
        output = interp.execute(turtle)
        assert len(turtle.lines) == 0
        results.append(("CLEARSCREEN/CS", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("CLEARSCREEN/CS", f"❌ {e}"))

    # HIDETURTLE/HT
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program(language=Language.LOGO, program_text="HIDETURTLE")
        output = interp.execute(turtle)
        assert not turtle.visible
        results.append(("HIDETURTLE/HT", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("HIDETURTLE/HT", f"❌ {e}"))

    # SHOWTURTLE/ST
    try:
        interp = Interpreter()
        turtle = TurtleState()
        turtle.visible = False
        interp.load_program(language=Language.LOGO, program_text="SHOWTURTLE")
        output = interp.execute(turtle)
        assert turtle.visible
        results.append(("SHOWTURTLE/ST", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("SHOWTURTLE/ST", f"❌ {e}"))

    # SETXY
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program(
            language=Language.LOGO,
            program_text="SETXY 50 100",
        )
        output = interp.execute(turtle)
        assert abs(turtle.x - 50) < 1 and abs(turtle.y - 100) < 1
        results.append(("SETXY", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("SETXY", f"❌ {e}"))

    # SETX
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program(language=Language.LOGO, program_text="SETX 75")
        output = interp.execute(turtle)
        assert abs(turtle.x - 75) < 1
        results.append(("SETX", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("SETX", f"❌ {e}"))

    # SETY
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program(language=Language.LOGO, program_text="SETY 80")
        output = interp.execute(turtle)
        assert abs(turtle.y - 80) < 1
        results.append(("SETY", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("SETY", f"❌ {e}"))

    # SETHEADING/SETH
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program(
            language=Language.LOGO,
            program_text="SETHEADING 180",
        )
        output = interp.execute(turtle)
        assert abs(turtle.heading - 180) < 1
        results.append(("SETHEADING/SETH", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("SETHEADING/SETH", f"❌ {e}"))

    # SETPENCOLOR/SETPC
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program(
            language=Language.LOGO,
            program_text="SETPENCOLOR 255 0 0",
        )
        output = interp.execute(turtle)
        assert turtle.pen_color == (255, 0, 0)
        results.append(("SETPENCOLOR/SETPC", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("SETPENCOLOR/SETPC", f"❌ {e}"))

    # SETBGCOLOR/SETBG
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program(
            language=Language.LOGO,
            program_text="SETBGCOLOR 0 0 255",
        )
        output = interp.execute(turtle)
        assert turtle.bg_color == (0, 0, 255)
        results.append(("SETBGCOLOR/SETBG", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("SETBGCOLOR/SETBG", f"❌ {e}"))

    # SETPENWIDTH/SETPW
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program(
            language=Language.LOGO,
            program_text="SETPENWIDTH 5",
        )
        output = interp.execute(turtle)
        assert turtle.pen_width == 5
        results.append(("SETPENWIDTH/SETPW", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("SETPENWIDTH/SETPW", f"❌ {e}"))

    # REPEAT
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program(
            language=Language.LOGO,
            program_text="REPEAT 4 [ FORWARD 50 RIGHT 90 ]",
        )
        output = interp.execute(turtle)
        assert len(turtle.lines) >= 4
        results.append(("REPEAT", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("REPEAT", f"❌ {e}"))

    # PRINT
    try:
        interp = Interpreter()
        turtle = TurtleState()
        interp.load_program('PRINT "Hello"')
        output = interp.execute(turtle)
        assert "Hello" in str(output)
        results.append(("PRINT", "✅"))
    except Exception as e:  # pylint: disable=broad-exception-caught
        results.append(("PRINT", f"❌ {e}"))

    # Print results
    for cmd, status in results:
        print(f"  {status} {cmd}")

    passed = sum(1 for _, s in results if s == "✅")
    print()
    print(f"Logo: {passed}/{len(results)} commands passed")
    print()
    return passed == len(results)


def main():
    """Run all command tests."""
    print()
    print("╔" + "=" * 68 + "╗")
    title = "TIME WARP IDE COMMAND VERIFICATION"
    padding = (68 - len(title)) // 2
    print(f"║{' ' * padding}{title}{' ' * (68 - len(title) - padding)}║")
    print("╚" + "=" * 68 + "╝")
    print()

    all_passed = True

    # Test each language
    pilot_ok = test_pilot_commands()
    basic_ok = test_basic_commands()
    logo_ok = test_logo_commands()

    all_passed = pilot_ok and basic_ok and logo_ok

    # Final summary
    print("=" * 70)
    print("FINAL SUMMARY")
    print("=" * 70)
    print()

    print(
        f"  {'✅' if pilot_ok else '❌'} PILOT Commands: "
        f"{'PASSED' if pilot_ok else 'FAILED'}"
    )
    print(
        f"  {'✅' if basic_ok else '❌'} BASIC Commands: "
        f"{'PASSED' if basic_ok else 'FAILED'}"
    )
    print(
        f"  {'✅' if logo_ok else '❌'} Logo Commands:  "
        f"{'PASSED' if logo_ok else 'FAILED'}"
    )
    print()

    if all_passed:
        print("╔" + "=" * 68 + "╗")
        print("║" + " " * 18 + "✅ ALL COMMANDS VERIFIED ✅" + " " * 23 + "║")
        print("╚" + "=" * 68 + "╝")
        return 0
    else:
        print("╔" + "=" * 68 + "╗")
        print("║" + " " * 20 + "❌ SOME TESTS FAILED ❌" + " " * 22 + "║")
        print("╚" + "=" * 68 + "╝")
        return 1


if __name__ == "__main__":
    sys.exit(main())
