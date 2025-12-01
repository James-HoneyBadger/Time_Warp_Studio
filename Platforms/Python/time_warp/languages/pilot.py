"""
PILOT language executor for Time Warp IDE.
Handles PILOT-specific commands and syntax.
"""

import re
import time
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


def execute_pilot(
    interpreter: "Interpreter",
    command: str,
    turtle: "TurtleState",
) -> str:
    """Execute PILOT language command."""
    cmd = command.strip()
    if not cmd or len(cmd) < 2:
        return ""

    cmd_type = cmd[0].upper()
    if cmd_type == "*" or command.startswith("L:"):
        return ""
    if len(cmd) < 2 or cmd[1] != ":":
        return f"❌ Invalid PILOT command: {command}\n"

    rest = cmd[2:].strip()

    if cmd_type == "T":
        text = interpreter.interpolate_text(rest)
        interpreter.output.append(text)
        return text + "\n"
    if cmd_type == "A":
        var_name = rest.strip()
        if not var_name:
            return "❌ A: requires variable name\n"
        # Start async input request
        interpreter.start_input_request("? ", var_name, is_numeric=False)
        return ""
    if cmd_type == "M":
        pattern = rest.strip()
        if not pattern:
            interpreter.last_match_succeeded = False
            return ""
        last_input = interpreter.last_input
        regex_pattern = "^" + pattern.replace("*", ".*") + "$"
        try:
            interpreter.last_match_succeeded = bool(
                re.match(regex_pattern, str(last_input), re.IGNORECASE)
            )
        except re.error:
            interpreter.last_match_succeeded = False
        return ""
    if cmd_type == "Y":
        if interpreter.last_match_succeeded:
            label = rest.strip()
            if label:
                interpreter.jump_to_label(label)
        return ""
    if cmd_type == "N":
        if not interpreter.last_match_succeeded:
            label = rest.strip()
            if label:
                interpreter.jump_to_label(label)
        return ""
    if cmd_type == "C":
        if "=" not in rest:
            return "❌ C: requires format: var = expression\n"
        parts = rest.split("=", 1)
        var_name = parts[0].strip()
        expr = parts[1].strip()
        if not var_name:
            return "❌ C: requires variable name\n"
        try:
            result = interpreter.evaluate_expression(expr)
            interpreter.variables[var_name] = result
        except (ValueError, TypeError) as e:
            return f"❌ Error in C: {e}\n"
        return ""
    if cmd_type == "J":
        label = rest.strip()
        if label:
            interpreter.jump_to_label(label)
        return ""
    if cmd_type == "U":
        if "=" in rest:
            # Treat as Compute (alias for C:)
            return execute_pilot(interpreter, "C:" + rest, turtle)

        var_name = rest.strip()
        if not var_name:
            return "❌ U: requires variable name\n"
        value = interpreter.variables.get(var_name, "")
        text = str(value)
        interpreter.output.append(text)
        return text + "\n"
    if cmd_type == "P":
        # P: Pause - wait for user to press Enter
        interpreter.start_input_request(
            "Press Enter to continue...", "_pause", is_numeric=False
        )
        return ""
    if cmd_type == "B":
        # B: Branch - conditional jump based on expression
        if "=" not in rest:
            return "❌ B: requires format: condition=label\n"
        parts = rest.split("=", 1)
        condition = parts[0].strip()
        label = parts[1].strip()
        if not label:
            return "❌ B: requires label\n"
        try:
            result = interpreter.evaluate_expression(condition)
            if result != 0:  # Non-zero means true
                interpreter.jump_to_label(label)
        except (ValueError, TypeError) as e:
            return f"❌ Error in B: condition: {e}\n"
        return ""
    if cmd_type == "S":
        # S: Subroutine call
        label = rest.strip()
        if label:
            # Push current line for return
            interpreter.subroutine_stack.append(interpreter.current_line)
            interpreter.jump_to_label(label)
        return ""
    if cmd_type == "R":
        # R: Return from subroutine
        if interpreter.subroutine_stack:
            return_line = interpreter.subroutine_stack.pop()
            interpreter.current_line = return_line
        return ""
    if cmd_type == "G":
        # G: Graphics command - integrate with turtle graphics
        return _pilot_graphics_command(interpreter, rest, turtle)
    if cmd_type == "F":
        # F: File operations
        return _pilot_file_command(interpreter, rest)
    if cmd_type == "D":
        # D: Delay/timer
        try:
            delay = float(rest.strip())

            time.sleep(min(delay, 10.0))  # Max 10 second delay
        except ValueError:
            return "❌ D: requires numeric delay in seconds\n"
        return ""
    if cmd_type == "L":
        return ""
    if cmd_type == "E":
        interpreter.running = False
        return ""
    return f"❌ Unknown PILOT command: {cmd_type}:\n"


def _pilot_graphics_command(
    interpreter: "Interpreter",
    command: str,
    turtle: "TurtleState",
) -> str:
    """Handle PILOT graphics commands integrated with turtle graphics."""
    # Normalize command: replace commas with spaces to handle "CMD x, y"
    normalized_command = command.replace(",", " ")
    parts = normalized_command.strip().split()
    if not parts:
        return "❌ G: requires graphics command\n"

    cmd = parts[0].upper()
    args = parts[1:]

    if cmd in ("FORWARD", "FD"):
        if not args:
            return "❌ G: FORWARD requires distance\n"
        try:
            distance = interpreter.evaluate_expression(args[0])
            turtle.forward(distance)
        except (ValueError, TypeError):
            return "❌ G: Invalid distance\n"
    elif cmd in ("BACK", "BK"):
        if not args:
            return "❌ G: BACK requires distance\n"
        try:
            distance = interpreter.evaluate_expression(args[0])
            turtle.back(distance)
        except (ValueError, TypeError):
            return "❌ G: Invalid distance\n"
    elif cmd in ("LEFT", "LT"):
        if not args:
            return "❌ G: LEFT requires angle\n"
        try:
            angle = interpreter.evaluate_expression(args[0])
            turtle.left(angle)
        except (ValueError, TypeError):
            return "❌ G: Invalid angle\n"
    elif cmd in ("RIGHT", "RT"):
        if not args:
            return "❌ G: RIGHT requires angle\n"
        try:
            angle = interpreter.evaluate_expression(args[0])
            turtle.right(angle)
        except (ValueError, TypeError):
            return "❌ G: Invalid angle\n"
    elif cmd in ("PENUP", "PU"):
        turtle.penup()
    elif cmd in ("PENDOWN", "PD"):
        turtle.pendown()
    elif cmd == "HOME":
        turtle.home()
    elif cmd in ("CLEAR", "CS"):
        turtle.clear()
    elif cmd == "SETXY":
        if len(args) < 2:
            return "❌ G: SETXY requires x,y coordinates\n"
        try:
            x = interpreter.evaluate_expression(args[0])
            y = interpreter.evaluate_expression(args[1])
            turtle.goto(x, y)
        except (ValueError, TypeError):
            return "❌ G: Invalid coordinates\n"
    elif cmd == "CIRCLE":
        if not args:
            return "❌ G: CIRCLE requires radius\n"
        try:
            radius = interpreter.evaluate_expression(args[0])
            # Draw circle using turtle graphics
            turtle.circle(radius)
        except (ValueError, TypeError):
            return "❌ G: Invalid radius\n"
    elif cmd == "SETBGCOLOR":
        if len(args) < 3:
            return "❌ G: SETBGCOLOR requires r, g, b\n"
        try:
            r = int(interpreter.evaluate_expression(args[0]))
            g = int(interpreter.evaluate_expression(args[1]))
            b = int(interpreter.evaluate_expression(args[2]))
            turtle.setbgcolor(r, g, b)
        except (ValueError, TypeError):
            return "❌ G: Invalid color values\n"
    elif cmd == "SETPENWIDTH":
        if not args:
            return "❌ G: SETPENWIDTH requires width\n"
        try:
            width = interpreter.evaluate_expression(args[0])
            turtle.setpenwidth(width)
        except (ValueError, TypeError):
            return "❌ G: Invalid width\n"
    else:
        return f"❌ Unknown graphics command: {cmd}\n"

    return ""


def _pilot_file_command(interpreter: "Interpreter", command: str) -> str:
    """Handle PILOT file operations."""
    parts = command.strip().split()
    if not parts:
        return "❌ F: requires file operation\n"

    operation = parts[0].upper()
    args = parts[1:]

    if operation == "OPEN":
        if len(args) < 2:
            return "❌ F: OPEN requires filename and mode\n"
        filename = args[0]
        mode = args[1].upper()
        try:
            if mode in ("READ", "R"):
                # pylint: disable=consider-using-with
                interpreter.open_files[filename] = open(
                    filename,
                    "r",
                    encoding="utf-8",
                )
            elif mode in ("WRITE", "W"):
                # pylint: disable=consider-using-with
                interpreter.open_files[filename] = open(
                    filename,
                    "w",
                    encoding="utf-8",
                )
            elif mode in ("APPEND", "A"):
                # pylint: disable=consider-using-with
                interpreter.open_files[filename] = open(
                    filename,
                    "a",
                    encoding="utf-8",
                )
            else:
                return f"❌ F: Unknown file mode: {mode}\n"
        except (IOError, OSError) as e:
            return f"❌ F: Error opening file: {e}\n"

    elif operation == "CLOSE":
        if not args:
            return "❌ F: CLOSE requires filename\n"
        filename = args[0]
        if filename in interpreter.open_files:
            try:
                interpreter.open_files[filename].close()
                del interpreter.open_files[filename]
            except (IOError, OSError) as e:
                return f"❌ F: Error closing file: {e}\n"
        else:
            return f"❌ F: File not open: {filename}\n"

    elif operation == "READ":
        if len(args) < 2:
            return "❌ F: READ requires filename and variable\n"
        filename = args[0]
        var_name = args[1]
        if filename not in interpreter.open_files:
            return f"❌ F: File not open: {filename}\n"
        try:
            line = interpreter.open_files[filename].readline().strip()
            # Store numeric values in numeric variable store, otherwise string store
            try:
                v = float(line)
                interpreter.variables[var_name] = v
            except ValueError:
                interpreter.string_variables[var_name] = line
        except (IOError, OSError) as e:
            return f"❌ F: Error reading file: {e}\n"

    elif operation == "WRITE":
        if len(args) < 2:
            return "❌ F: WRITE requires filename and data\n"
        filename = args[0]
        data = " ".join(args[1:])
        if filename not in interpreter.open_files:
            return f"❌ F: File not open: {filename}\n"
        try:
            interpreter.open_files[filename].write(data + "\n")
        except (IOError, OSError) as e:
            return f"❌ F: Error writing file: {e}\n"

    else:
        return f"❌ Unknown file operation: {operation}\n"

    return ""
