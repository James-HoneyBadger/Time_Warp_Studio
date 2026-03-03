"""
PILOT language executor for Time Warp Studio.
Handles PILOT-specific commands and syntax.
"""

import re
import time
from typing import TYPE_CHECKING

from ..logging_config import get_logger
from ..utils.validators import (
    ValidationError,
    validate_arg_count,
    validate_file_path,
    validate_variable_name,
)

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState

logger = get_logger(__name__)


def execute_pilot(
    interpreter: "Interpreter",
    command: str,
    turtle: "TurtleState",
) -> str:
    """Execute PILOT language command.

    PILOT commands have the format: X: or XY: or XN: where:
    - X is the command letter (T, A, M, C, J, etc.)
    - Y suffix means "execute if last match succeeded"
    - N suffix means "execute if last match failed"
    """
    cmd = command.strip()
    if not cmd or len(cmd) < 2:
        return ""

    # Check for label definition (*label) or L: command
    if cmd[0] == "*" or cmd.upper().startswith("L:"):
        return ""

    # REMARK (and REM) are comment lines — ignore
    cmd_upper = cmd.upper()
    if cmd_upper.startswith("REMARK") or cmd_upper.startswith("REM ") or cmd_upper == "REM":
        return ""

    # ── Long-form keyword mapping ────────────────────────────────────────
    # Accept long-form PILOT commands (PRINT, ACCEPT, COMPUTE, MATCH, JUMP,
    # USE, STOP, PAUSE, TYPE, GRAPHIC) and convert to canonical X: format.
    _LONGFORM_MAP = {
        "PRINT": "T",
        "TYPE": "T",
        "ACCEPT": "A",
        "COMPUTE": "C",
        "MATCH": "M",
        "JUMP": "J",
        "USE": "U",
        "STOP": "E",
        "END": "E",
        "PAUSE": "P",
        "GRAPHIC": "G",
        "FILE": "F",
        "DELAY": "D",
        "LINK": "S",
    }
    for keyword, letter in _LONGFORM_MAP.items():
        if cmd_upper == keyword or cmd_upper.startswith(keyword + " "):
            rest_text = cmd[len(keyword):].strip()
            # For COMPUTE, normalise "var value" → "var = value" if no "="
            if letter == "C" and "=" not in rest_text and " " in rest_text:
                parts = rest_text.split(None, 1)
                rest_text = f"{parts[0]} = {parts[1]}" if len(parts) == 2 else rest_text
            # STOP/END has no argument
            if letter == "E":
                interpreter.running = False
                return ""
            return execute_pilot(interpreter, f"{letter}:{rest_text}", turtle)

    # TU (subroutine call via long-form "TU label")
    if cmd_upper.startswith("TU "):
        label = cmd[3:].strip()
        if label:
            if not hasattr(interpreter, "subroutine_stack"):
                interpreter.subroutine_stack = []
            interpreter.subroutine_stack.append(interpreter.current_line + 1)
            interpreter.jump_to_label(label)
        return ""

    # Parse command prefix - handle both X: and XY:/XN: formats
    colon_pos = cmd.find(":")
    if colon_pos < 1:
        return f"❌ Invalid PILOT command: {command}\n"

    prefix = cmd[:colon_pos].upper()
    rest = cmd[colon_pos + 1 :].strip()

    # Extract base command and conditional suffix
    if len(prefix) == 1:
        cmd_type = prefix
        condition = None
    elif len(prefix) == 2 and prefix[1] in ("Y", "N"):
        cmd_type = prefix[0]
        condition = prefix[1]
    else:
        return f"❌ Invalid PILOT command: {command}\n"

    # Check condition before executing
    if condition == "Y" and not interpreter.last_match_succeeded:
        return ""  # Skip - condition not met
    if condition == "N" and interpreter.last_match_succeeded:
        return ""  # Skip - condition not met

    if cmd_type == "T":
        # Substitute $variable and #variable references with their values
        import re as _re_t
        def _subst_var_t(m):
            vn = m.group(1)
            val = None
            vnu = vn.upper()
            # Check string variables first (e.g. ANSWER$)
            if vnu + "$" in interpreter.string_variables:
                val = interpreter.string_variables[vnu + "$"]
            elif vn + "$" in interpreter.string_variables:
                val = interpreter.string_variables[vn + "$"]
            # Then check general variables
            elif vn in interpreter.variables:
                val = interpreter.variables[vn]
            elif vnu in interpreter.variables:
                val = interpreter.variables[vnu]
            if val is not None:
                if isinstance(val, float) and val == int(val):
                    return str(int(val))
                return str(val)
            return m.group(0)
        rest = _re_t.sub(r'\$([A-Za-z_][A-Za-z0-9_]*)', _subst_var_t, rest)
        rest = _re_t.sub(r'#([A-Za-z_][A-Za-z0-9_]*)', _subst_var_t, rest)
        text = interpreter.interpolate_text(rest)
        interpreter.output.append(text)
        return text + "\n"
    if cmd_type == "A":
        prompt_text = rest.strip() if rest.strip() else ""
        # In PILOT, A: accepts input into the built-in ANSWER variable.
        # If the rest text starts with $ it names a specific variable.
        if prompt_text.startswith("$"):
            var_name = prompt_text[1:].strip().upper() + "$"
        else:
            var_name = "ANSWER$"
        display_prompt = (prompt_text + " ") if prompt_text and not prompt_text.startswith("$") else "? "
        # Start async input request — store as string
        interpreter.start_input_request(display_prompt, var_name, is_numeric=False)
        return ""
    if cmd_type == "M":
        pattern = rest.strip()
        if not pattern:
            interpreter.last_match_succeeded = False
            return ""
        last_input = str(interpreter.last_input).strip().upper()

        # PILOT M: command matches against comma-separated alternatives
        # M:YES,YEAH,YEP,Y means match if input equals any of these
        alternatives = [p.strip().upper() for p in pattern.split(",")]

        # Check each alternative - support wildcards with *
        interpreter.last_match_succeeded = False
        for alt in alternatives:
            if "*" in alt:
                # Convert wildcard pattern to regex
                # Escape special regex chars, then replace escaped * with .*
                escaped_alt = re.escape(alt)
                regex_pattern = "^" + escaped_alt.replace(r"\*", ".*") + "$"
                try:
                    if re.match(regex_pattern, last_input, re.IGNORECASE):
                        interpreter.last_match_succeeded = True
                        break
                except re.error:
                    pass
            else:
                # Exact match (case-insensitive)
                if last_input == alt:
                    interpreter.last_match_succeeded = True
                    break
        return ""
    if cmd_type == "Y":
        if interpreter.last_match_succeeded:
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

        # Substitute $variable references with their values
        import re as _re
        def _subst_var(m):
            vn = m.group(1)
            val = None
            if vn in interpreter.variables:
                val = interpreter.variables[vn]
            elif vn.upper() in interpreter.variables:
                val = interpreter.variables[vn.upper()]
            if val is not None:
                if isinstance(val, float) and val == int(val):
                    return str(int(val))
                return str(val)
            return m.group(0)
        expr = _re.sub(r'\$([A-Za-z_][A-Za-z0-9_]*)', _subst_var, expr)

        try:
            validate_variable_name(var_name, allow_suffix=True)
            result = interpreter.evaluate_expression(expr)
            interpreter.variables[var_name.upper()] = result
            logger.debug("PILOT C: %s = %s", var_name, result)
        except ValidationError as e:
            return f"❌ {e}\n"
        except (ValueError, TypeError, ZeroDivisionError) as e:
            logger.error("PILOT C: evaluation failed: %s", e)
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
            # Push next line for return (so R: returns to the line after S:)
            interpreter.subroutine_stack.append(interpreter.current_line + 1)
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
    # Split command and arguments
    parts = command.strip().split(maxsplit=1)
    if not parts:
        return "❌ G: requires graphics command\n"

    cmd = parts[0].upper()
    arg_str = parts[1] if len(parts) > 1 else ""

    # Helper to evaluate single argument
    def eval_arg(s):
        return interpreter.evaluate_expression(s)

    if cmd in ("FORWARD", "FD"):
        if not arg_str:
            return "❌ G: FORWARD requires distance\n"
        try:
            distance = eval_arg(arg_str)
            turtle.forward(distance)
        except (ValueError, TypeError):
            return "❌ G: Invalid distance\n"
    elif cmd in ("BACK", "BK"):
        if not arg_str:
            return "❌ G: BACK requires distance\n"
        try:
            distance = eval_arg(arg_str)
            turtle.back(distance)
        except (ValueError, TypeError):
            return "❌ G: Invalid distance\n"
    elif cmd in ("LEFT", "LT"):
        if not arg_str:
            return "❌ G: LEFT requires angle\n"
        try:
            angle = eval_arg(arg_str)
            turtle.left(angle)
        except (ValueError, TypeError):
            return "❌ G: Invalid angle\n"
    elif cmd in ("RIGHT", "RT"):
        if not arg_str:
            return "❌ G: RIGHT requires angle\n"
        try:
            angle = eval_arg(arg_str)
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
        if "," in arg_str:
            args = arg_str.split(",")
        else:
            args = arg_str.split()

        if len(args) < 2:
            return "❌ G: SETXY requires x,y coordinates\n"
        try:
            x = eval_arg(args[0])
            y = eval_arg(args[1])
            turtle.goto(x, y)
        except (ValueError, TypeError):
            return "❌ G: Invalid coordinates\n"
    elif cmd == "CIRCLE":
        if not arg_str:
            return "❌ G: CIRCLE requires radius\n"
        try:
            radius = eval_arg(arg_str)
            turtle.circle(radius)
        except (ValueError, TypeError):
            return "❌ G: Invalid radius\n"
    elif cmd == "SETBGCOLOR":
        if "," in arg_str:
            args = arg_str.split(",")
        else:
            args = arg_str.split()

        if len(args) < 3:
            return "❌ G: SETBGCOLOR requires r, g, b\n"
        try:
            r = int(eval_arg(args[0]))
            g = int(eval_arg(args[1]))
            b = int(eval_arg(args[2]))
            turtle.setbgcolor(r, g, b)
        except (ValueError, TypeError):
            return "❌ G: Invalid color values\n"
    elif cmd == "SETPENCOLOR":
        if "," in arg_str:
            args = arg_str.split(",")
        else:
            args = arg_str.split()

        if len(args) < 3:
            return "❌ G: SETPENCOLOR requires r, g, b\n"
        try:
            r = int(eval_arg(args[0]))
            g = int(eval_arg(args[1]))
            b = int(eval_arg(args[2]))
            turtle.setcolor(r, g, b)
        except (ValueError, TypeError):
            return "❌ G: Invalid color values\n"
    elif cmd == "SETPENWIDTH":
        if not arg_str:
            return "❌ G: SETPENWIDTH requires width\n"
        try:
            width = eval_arg(arg_str)
            turtle.setpenwidth(width)
        except (ValueError, TypeError):
            return "❌ G: Invalid width\n"
    else:
        return f"❌ Unknown graphics command: {cmd}\n"

    return ""


def _pilot_file_command(interpreter: "Interpreter", command: str) -> str:
    """Handle PILOT file operations (OPEN, CLOSE, READ, WRITE).

    Returns:
        Status or error message
    """
    parts = command.strip().split()
    if not parts:
        return "❌ F: requires file operation\n"

    operation = parts[0].upper()
    args = parts[1:]

    if operation == "OPEN":
        try:
            validate_arg_count(args, 2, f"F: {operation}")
            filename = args[0]
            mode = args[1].upper()

            # Validate filename is safe
            validate_file_path(filename, base_dir=".")

            if mode not in ("READ", "R", "WRITE", "W", "APPEND", "A"):
                return f"❌ F: OPEN requires mode R|W|A, got: {mode}\n"

            try:
                if mode in ("READ", "R"):
                    # pylint: disable=consider-using-with
                    interpreter.open_files[filename] = open(
                        filename,
                        "r",
                        encoding="utf-8",
                    )
                    logger.info("PILOT file OPEN read: %s", filename)
                elif mode in ("WRITE", "W"):
                    # pylint: disable=consider-using-with
                    interpreter.open_files[filename] = open(
                        filename,
                        "w",
                        encoding="utf-8",
                    )
                    logger.info("PILOT file OPEN write: %s", filename)
                elif mode in ("APPEND", "A"):
                    # pylint: disable=consider-using-with
                    interpreter.open_files[filename] = open(
                        filename,
                        "a",
                        encoding="utf-8",
                    )
                    logger.info("PILOT file OPEN append: %s", filename)
            except (IOError, OSError) as e:
                logger.error("PILOT file open error: %s", e)
                return f"❌ F: Error opening file '{filename}': {e}\n"
        except ValidationError as e:
            return f"❌ {e}\n"

    elif operation == "CLOSE":
        try:
            validate_arg_count(args, 1, f"F: {operation}")
            filename = args[0]

            if filename not in interpreter.open_files:
                return f"❌ F: File not open: {filename}\n"

            try:
                interpreter.open_files[filename].close()
                del interpreter.open_files[filename]
                logger.info("PILOT file closed: %s", filename)
            except (IOError, OSError) as e:
                logger.error("PILOT file close error: %s", e)
                return f"❌ F: Error closing file: {e}\n"
        except ValidationError as e:
            return f"❌ {e}\n"

    elif operation == "READ":
        try:
            validate_arg_count(args, 2, f"F: {operation}")
            filename = args[0]
            var_name = args[1]

            validate_variable_name(var_name, allow_suffix=True)

            if filename not in interpreter.open_files:
                return f"❌ F: File not open: {filename}\n"

            try:
                file_obj = interpreter.open_files[filename]
                line = file_obj.readline().strip()
                # Store numeric values in numeric variable store
                try:
                    v = float(line)
                    interpreter.variables[var_name] = v
                except ValueError:
                    interpreter.string_variables[var_name] = line
                logger.debug("PILOT file read: %s from %s", var_name, filename)
            except (IOError, OSError) as e:
                logger.error("PILOT file read error: %s", e)
                return f"❌ F: Error reading file: {e}\n"
        except ValidationError as e:
            return f"❌ {e}\n"

    elif operation == "WRITE":
        try:
            validate_arg_count(args, 2, f"F: {operation}")
            filename = args[0]
            data = " ".join(args[1:])

            if filename not in interpreter.open_files:
                return f"❌ F: File not open: {filename}\n"

            try:
                interpreter.open_files[filename].write(data + "\n")
                logger.debug("PILOT file write: %s", filename)
            except (IOError, OSError) as e:
                logger.error("PILOT file write error: %s", e)
                return f"❌ F: Error writing file: {e}\n"
        except ValidationError as e:
            return f"❌ {e}\n"

    else:
        logger.error("Unknown PILOT file operation: %s", operation)
        return f"❌ Unknown file operation: {operation}\n"

    return ""
