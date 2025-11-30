"""
BASIC language executor for Time Warp IDE.
Handles BASIC-specific commands and syntax.
"""

import math
import re
from typing import TYPE_CHECKING, List

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState

# Runtime imports moved inside functions to avoid circular imports


def _strip_comment(args: str) -> str:
    """Strip trailing comments from arguments."""
    # Simple stripping of : REM and '
    # Note: Does not handle quotes correctly (e.g. PRINT "A : REM B")
    # For demos this is sufficient.
    upper = args.upper()
    if " : REM" in upper:
        idx = upper.find(" : REM")
        return args[:idx].strip()
    if " :REM" in upper:
        idx = upper.find(" :REM")
        return args[:idx].strip()
    # Handle ' comment
    # if "'" in args: ... (might break strings)
    return args


def _split_statements(command: str) -> List[str]:
    """Split command into statements by colon, respecting quotes."""
    statements = []
    current = ""
    in_quotes = False
    for ch in command:
        if ch == '"':
            in_quotes = not in_quotes
            current += ch
        elif ch == ":" and not in_quotes:
            statements.append(current.strip())
            current = ""
        else:
            current += ch
    if current.strip():
        statements.append(current.strip())
    return statements


# Function is large by design; keep pylint refactor checks disabled for it.
# pylint: disable=R0911,R0912,R0915
def execute_basic(
    interpreter: "Interpreter",
    command: str,
    turtle: "TurtleState",
) -> str:  # pylint: disable=R0911,R0912,R0915
    """Execute BASIC language command."""
    # Import here to avoid circular imports; helpers import what they need.

    # Strip leading/trailing whitespace from the command line
    command = command.strip()

    # Handle multi-statement lines
    if (
        ":" in command
        and not command.upper().startswith("REM")
        and not command.upper().startswith("'")
    ):
        statements = _split_statements(command)
        if len(statements) > 1:
            output = ""
            for stmt in statements:
                output += execute_basic(interpreter, stmt, turtle)
            return output

    cmd = command.upper()
    if cmd.startswith("REM") or cmd.startswith("'"):
        return ""
    if cmd.startswith("PRINT ") or cmd == "PRINT":
        args = command[6:] if len(command) > 6 else ""
        return _basic_print(interpreter, _strip_comment(args))
    if cmd.startswith("INPUT "):
        return _basic_input(interpreter, _strip_comment(command[6:]))
    if cmd.startswith("IF "):
        return _basic_if(interpreter, _strip_comment(command[3:]), turtle)
    if cmd.startswith("GOTO "):
        return _basic_goto(interpreter, _strip_comment(command[5:]))
    if cmd.startswith("GOSUB "):
        return _basic_gosub(interpreter, _strip_comment(command[6:]))
    if cmd == "RETURN":
        return _basic_return(interpreter)
    if cmd == "END":
        return _basic_end(interpreter)
    if cmd.startswith("LET "):
        return _basic_let(interpreter, _strip_comment(command[4:]))
    if cmd.startswith("FOR "):
        return _basic_for(interpreter, _strip_comment(command[4:]))
    if cmd.startswith("NEXT"):
        return _basic_next(interpreter, _strip_comment(command[4:]))
    if cmd.startswith("WHILE "):
        return _basic_while(interpreter, _strip_comment(command[6:]))
    if cmd == "WEND":
        return _basic_wend(interpreter)
    if cmd.startswith("DO"):
        return _basic_do(interpreter, _strip_comment(command[3:]))
    if cmd.startswith("LOOP"):
        return _basic_loop(interpreter, _strip_comment(command[5:]))
    if cmd.startswith("SELECT "):
        return _basic_select(interpreter, _strip_comment(command[11:]))
    if cmd.startswith("CASE "):
        return _basic_case(interpreter, _strip_comment(command[5:]))
    if cmd == "END SELECT":
        return _basic_end_select(interpreter)
    if cmd.startswith("SUB "):
        return _basic_sub(interpreter, _strip_comment(command[4:]))
    if cmd == "END SUB":
        return _basic_end_sub(interpreter)
    if cmd.startswith("FUNCTION "):
        return _basic_function(interpreter, _strip_comment(command[9:]))
    if cmd == "END FUNCTION":
        return _basic_end_function(interpreter)
    if cmd.startswith("CALL "):
        return _basic_call(interpreter, _strip_comment(command[5:]))
    if cmd.startswith("RUN"):
        return _basic_run(interpreter, _strip_comment(command[4:]))
    if cmd.startswith("SYSTEM"):
        return _basic_system(interpreter, _strip_comment(command[7:]))
    if cmd.startswith("DIM "):
        return _basic_dim(interpreter, _strip_comment(command[4:]))
    if "=" in cmd and not cmd.startswith("IF ") and not cmd.startswith("FOR "):
        return _basic_let(interpreter, _strip_comment(command))
    if cmd.startswith("COLOR "):
        return _basic_color(interpreter, _strip_comment(command[6:]))
    if cmd.startswith("WIDTH "):
        return _basic_width(interpreter, _strip_comment(command[6:]))
    if cmd.startswith("OPEN "):
        return _basic_open(interpreter, _strip_comment(command[5:]))
    if cmd.startswith("CLOSE"):
        return _basic_close(interpreter, _strip_comment(command[6:]))
    if cmd.startswith("GET "):
        return _basic_get(interpreter, _strip_comment(command[4:]))
    if cmd.startswith("PUT "):
        return _basic_put(interpreter, _strip_comment(command[4:]))
    if cmd.startswith("REM ") or cmd == "REM":
        return ""
    if cmd == "CLS":
        turtle.clear()
        interpreter.text_lines.clear()
        return "🎨 Screen cleared\n"
    if cmd.startswith("SCREEN "):
        return _basic_screen(interpreter, _strip_comment(command[7:]))
    if cmd.startswith("LOCATE "):
        return _basic_locate(interpreter, _strip_comment(command[7:]))
    if cmd.startswith("LINE "):
        return _basic_line(interpreter, _strip_comment(command[5:]), turtle)
    if cmd.startswith("CIRCLE "):
        return _basic_circle(interpreter, _strip_comment(command[7:]), turtle)
    if cmd.startswith("DATA "):
        return ""
    if cmd.startswith("READ "):
        return _basic_read(interpreter, _strip_comment(command[5:]))
    if cmd.startswith("RESTORE"):
        return _basic_restore(interpreter, _strip_comment(command[7:]))
    # Type default declarations
    if cmd.startswith("DEFINT "):
        interpreter.set_default_type_for_spec(
            "int",
            _strip_comment(command[7:]),
        )
        return ""
    if cmd.startswith("DEFLNG "):
        interpreter.set_default_type_for_spec(
            "long",
            _strip_comment(command[7:]),
        )
        return ""
    if cmd.startswith("DEFSNG "):
        interpreter.set_default_type_for_spec(
            "single",
            _strip_comment(command[7:]),
        )
        return ""
    if cmd.startswith("DEFDBL "):
        interpreter.set_default_type_for_spec(
            "double",
            _strip_comment(command[7:]),
        )
        return ""
    if cmd.startswith("DEFSTR "):
        interpreter.set_default_type_for_spec(
            "string",
            _strip_comment(command[7:]),
        )
        return ""
    return f"❌ Unknown BASIC command: {command}\n"


# Keep local complexity for the BASIC print formatter — it's intentionally
# complex for many formatting cases. Disable too-many-locals here.
# pylint: disable=R0914
def _basic_print(interpreter: "Interpreter", args: str) -> str:
    if not args.strip():
        return "\n"

    def _format_numeric(val: float, preferred: str | None = None) -> str:
        # Preferred types 'int'/'long' print without decimals
        if preferred in ("int", "long"):
            return str(int(val))
        # For floats, use general format to avoid trailing .0/.000
        return f"{val:g}"

    parts: List[str] = []
    current = ""
    in_quotes = False
    for ch in args:
        if ch == '"':
            in_quotes = not in_quotes
            current += ch
        elif ch in (";", ",") and not in_quotes:
            if current.strip():
                parts.append(current.strip())
            current = ""
        else:
            current += ch
    if current.strip():
        parts.append(current.strip())
    if not parts:
        return "\n"
    out_items: List[str] = []
    for item in parts:
        item_trim = item.strip()
        item_upper = item_trim.upper()

        # Handle string literals
        if (
            item_trim.startswith('"')
            and item_trim.endswith('"')
            and len(item_trim) >= 2
        ):
            out_items.append(item_trim[1:-1])
        # Handle INKEY$ special case
        elif item_upper == "INKEY$":
            out_items.append("")
        # Handle string variables (end with $)
        elif item_upper.endswith("$"):
            if item_upper in interpreter.string_variables:
                out_items.append(interpreter.string_variables[item_upper])
            else:
                out_items.append("")  # Undefined string variable is empty
        # Handle numeric variables and expressions
        else:
            # Typed numeric variable by suffix
            if item_upper and item_upper[-1] in ("%", "&", "!", "#"):
                val = interpreter.get_numeric_value(item_upper)
                tmap = {"%": "int", "&": "long", "!": "single", "#": "double"}
                preferred = tmap.get(item_upper[-1])
                if val is None:
                    out_items.append("")
                else:
                    out_items.append(_format_numeric(val, preferred))
            # Simple base-name numeric variable
            elif item_upper in interpreter.variables:
                # Determine preferred type from defaults using public API
                _base, t = interpreter.get_var_base_and_type(item_upper)
                val = interpreter.variables[item_upper]
                out_items.append(_format_numeric(val, t))
            else:
                # Try to evaluate as expression
                try:
                    value = interpreter.evaluate_expression(item_trim)
                    out_items.append(_format_numeric(value))
                except (ValueError, TypeError, ZeroDivisionError):
                    # If evaluation fails, use interpolation as fallback
                    out_items.append(interpreter.interpolate_text(item_trim))
    result = " ".join(out_items)
    return result + "\n"


def _basic_let(interpreter: "Interpreter", args: str) -> str:
    if "=" not in args:
        return "❌ LET requires format: variable = expression\n"
    parts = args.split("=", 1)
    var_name = parts[0].strip().upper()
    expr = parts[1].strip()
    if not var_name:
        return "❌ LET requires variable name\n"
    # String assignment
    if var_name.endswith("$"):
        if expr.startswith('"') and expr.endswith('"'):
            interpreter.set_typed_variable(var_name, expr[1:-1])
        else:
            interpreter.set_typed_variable(var_name, str(expr))
        return ""
    try:
        result = interpreter.evaluate_expression(expr)
        interpreter.set_typed_variable(var_name, result)
    except (ValueError, TypeError, ZeroDivisionError) as e:
        return f"❌ Error in LET: {e} (expr: '{expr}')\n"
    return ""


def _basic_input(interpreter: "Interpreter", args: str) -> str:
    var_name = args.strip().upper()
    prompt = "? "
    if '"' in args:
        match = re.match(r'"([^"]*)"[;,]?\s*(.+)', args)
        if match:
            prompt = match.group(1) + " "
            var_name = match.group(2).strip().upper()
    if not var_name:
        return "❌ INPUT requires variable name\n"
    interpreter.start_input_request(
        prompt,
        var_name,
        not var_name.endswith("$"),
    )
    return ""


def _basic_if(
    interpreter: "Interpreter",
    args: str,
    turtle: "TurtleState",
) -> str:
    args_upper = args.upper()
    if " THEN " not in args_upper:
        return "❌ IF requires THEN keyword\n"
    then_pos = args_upper.find(" THEN ")
    condition = args[:then_pos].strip()
    then_part = args[then_pos + 6 :].strip()
    try:
        result = interpreter.evaluate_expression(condition)
        condition_true = abs(result) > 0.0001
    except (ValueError, TypeError, ZeroDivisionError) as e:
        return f"❌ Error in IF condition: {e}\n"
    if condition_true and then_part:
        if then_part.isdigit():
            return _basic_goto(interpreter, then_part)
        return execute_basic(interpreter, then_part, turtle)
    return ""


def _basic_goto(interpreter: "Interpreter", args: str) -> str:
    target = args.strip()
    if not target:
        return "❌ GOTO requires line number\n"
    try:
        line_num = int(target)
        if line_num not in interpreter.line_number_map:
            return f"❌ GOTO {line_num} failed: line not found\n"
        target_idx = interpreter.line_number_map[line_num]
        interpreter.current_line = target_idx - 1
    except ValueError:
        return f"❌ Invalid line number: {target}\n"
    except (KeyError, IndexError) as e:
        return f"❌ Error in GOTO: {e}\n"
    return ""


def _basic_for(interpreter: "Interpreter", args: str) -> str:
    # pylint: disable=import-outside-toplevel
    from ..core.interpreter import ForContext

    match = re.match(
        r"(\w+)\s*=\s*(.+?)\s+TO\s+(.+?)(?:\s+STEP\s+(.+))?$",
        args.upper(),
    )
    if not match:
        return "❌ FOR requires format: var = start TO end [STEP step]\n"
    var_name = match.group(1)
    start_expr = match.group(2)
    end_expr = match.group(3)
    step_expr = match.group(4) if match.group(4) else "1"
    try:
        start_val = interpreter.evaluate_expression(start_expr)
        end_val = interpreter.evaluate_expression(end_expr)
        step_val = interpreter.evaluate_expression(step_expr)
        interpreter.set_typed_variable(var_name, start_val)

        context = ForContext(
            var_name=var_name,
            end_value=end_val,
            step=step_val,
            for_line=interpreter.current_line,
        )
        interpreter.for_stack.append(context)
    except (ValueError, TypeError, ZeroDivisionError) as e:
        return f"❌ Error in FOR: {e}\n"
    return ""


def _basic_next(interpreter: "Interpreter", _args: str) -> str:
    if not interpreter.for_stack:
        return "❌ NEXT without FOR\n"
    context = interpreter.for_stack[-1]
    current_val = interpreter.get_numeric_value(context.var_name) or 0
    new_val = current_val + context.step
    interpreter.set_typed_variable(context.var_name, new_val)
    if context.step > 0:
        should_continue = new_val <= context.end_value
    else:
        should_continue = new_val >= context.end_value
    if should_continue:
        interpreter.current_line = context.for_line
    else:
        interpreter.for_stack.pop()
    return ""


def _basic_gosub(interpreter: "Interpreter", args: str) -> str:
    target = args.strip()
    if not target:
        return "❌ GOSUB requires line number\n"
    try:
        line_num = int(target)
        interpreter.gosub_stack.append(interpreter.current_line)
        interpreter.jump_to_line_number(line_num)
    except ValueError:
        return f"❌ Invalid line number: {target}\n"
    except (KeyError, IndexError) as e:
        return f"❌ Error in GOSUB: {e}\n"
    return ""


def _basic_return(interpreter: "Interpreter") -> str:
    if not interpreter.gosub_stack:
        return "❌ RETURN without GOSUB\n"
    return_line = interpreter.gosub_stack.pop()
    interpreter.current_line = return_line + 1
    return ""


def _basic_screen(interpreter: "Interpreter", args: str) -> str:
    """SCREEN mode[, width, height] - Set screen mode"""
    # pylint: disable=import-outside-toplevel
    from ..core.interpreter import ScreenMode

    parts = args.split(",")
    if not parts:
        return "❌ SCREEN requires mode parameter\n"

    try:
        mode = int(parts[0].strip())

        if mode == 0:  # Text mode
            interpreter.screen_mode = ScreenMode.TEXT
            if len(parts) >= 3:
                cols = int(parts[1].strip())
                rows = int(parts[2].strip())
                interpreter.screen_mode.cols = cols
                interpreter.screen_mode.rows = rows
            cols, rows = (
                interpreter.screen_mode.cols,
                interpreter.screen_mode.rows,
            )
            return f"🎨 Text mode ({cols}x{rows})\n"
        if mode == 1:  # Graphics mode
            interpreter.screen_mode = ScreenMode.GRAPHICS
            if len(parts) >= 3:
                width = int(parts[1].strip())
                height = int(parts[2].strip())
                interpreter.screen_mode.width = width
                interpreter.screen_mode.height = height
            width, height = (
                interpreter.screen_mode.width,
                interpreter.screen_mode.height,
            )
            return f"🎨 Graphics mode ({width}x{height})\n"
        return f"❌ Unsupported SCREEN mode: {mode}\n"
    except ValueError as e:
        return f"❌ Invalid SCREEN parameters: {e}\n"


def _basic_locate(interpreter: "Interpreter", args: str) -> str:
    parts = args.split(",")
    if len(parts) < 2:
        return "❌ LOCATE requires row, col\n"
    try:
        row = int(interpreter.evaluate_expression(parts[0].strip()))
        col = int(interpreter.evaluate_expression(parts[1].strip()))
        interpreter.cursor_row = max(0, min(24, row - 1))
        interpreter.cursor_col = max(0, min(79, col - 1))
    except (ValueError, TypeError, ZeroDivisionError) as e:
        return f"❌ LOCATE error: {e}\n"
    return ""


def _basic_line(
    _interpreter: "Interpreter",
    args: str,
    t: "TurtleState",
) -> str:
    """Draw line in BASIC graphics. Syntax: LINE (x1,y1)-(x2,y2)"""
    # Try QBasic syntax: (x1, y1)-(x2, y2)
    # Regex to match (x1, y1)-(x2, y2)
    # We use a flexible regex that handles spaces
    match = re.match(
        r"\(\s*([^,]+)\s*,\s*([^,]+)\s*\)\s*-\s*"
        r"\(\s*([^,]+)\s*,\s*([^,]+)\s*\)"
        r"(?:,\s*(.+))?",
        args,
    )

    x1_str, y1_str, x2_str, y2_str = "", "", "", ""
    # color_str = None  # Not used yet

    if match:
        x1_str = match.group(1)
        y1_str = match.group(2)
        x2_str = match.group(3)
        y2_str = match.group(4)
        # if match.group(5):
        #     color_str = match.group(5)
    else:
        # Try standard syntax: x1, y1, x2, y2
        parts = args.split(",")
        if len(parts) >= 4:
            x1_str = parts[0]
            y1_str = parts[1]
            x2_str = parts[2]
            y2_str = parts[3]
            # if len(parts) > 4:
            #     color_str = parts[4]
        else:
            return "❌ LINE requires x1,y1,x2,y2 coordinates\n"

    try:
        # Evaluate expressions for coordinates
        x1 = _interpreter.evaluate_expression(x1_str)
        y1 = _interpreter.evaluate_expression(y1_str)
        x2 = _interpreter.evaluate_expression(x2_str)
        y2 = _interpreter.evaluate_expression(y2_str)

        # Move to start position without drawing
        old_pen = t.pen_down
        t.penup()
        t.goto(x1, y1)

        # Draw line to end position
        t.pendown()
        t.goto(x2, y2)

        # Restore pen state
        if not old_pen:
            t.penup()

        return ""
    except (ValueError, TypeError, ZeroDivisionError) as e:
        return f"❌ LINE error: {e} (args: '{args}')\n"


def _basic_circle(i: "Interpreter", args: str, t: "TurtleState") -> str:
    """Draw circle in BASIC graphics. Syntax: CIRCLE (x,y),r"""
    # Try QBasic syntax: (x, y), radius
    match = re.match(r"\(\s*([^,]+)\s*,\s*([^,]+)\s*\)\s*,\s*(.+)", args)

    x_str, y_str, r_str = "", "", ""

    if match:
        x_str, y_str, r_str = match.group(1), match.group(2), match.group(3)
    else:
        # Try standard syntax: x, y, radius
        parts = args.split(",")
        if len(parts) >= 3:
            x_str, y_str, r_str = parts[0], parts[1], parts[2]
        else:
            return "❌ CIRCLE requires x,y,radius\n"

    try:
        center_x = i.evaluate_expression(x_str)
        center_y = i.evaluate_expression(y_str)
        radius = i.evaluate_expression(r_str)

        if radius <= 0:
            return "❌ CIRCLE radius must be positive\n"

        # Draw circle using turtle graphics
        # Move to starting position on circle
        old_pen = t.pen_down
        t.penup()
        t.goto(center_x + radius, center_y)
        t.pendown()

        # Draw circle in segments
        segments = 36  # Good approximation for a circle
        angle_step = 360.0 / segments

        for step in range(segments + 1):
            angle = step * angle_step
            rad = math.radians(angle)
            x = center_x + radius * math.cos(rad)
            y = center_y + radius * math.sin(rad)
            t.goto(x, y)

        # Restore pen state
        if not old_pen:
            t.penup()

        return ""
    except (ValueError, TypeError, ZeroDivisionError) as e:
        return f"❌ CIRCLE error: {e}\n"


def _basic_run(_interpreter: "Interpreter", _args: str) -> str:
    """RUN [filename] - Execute program"""
    # For now, just reset the interpreter
    _interpreter.reset()
    return "🚀 Program reset\n"


def _basic_system(_interpreter: "Interpreter", _args: str) -> str:
    """SYSTEM - Return to system prompt"""
    # In an IDE environment, this might just reset
    _interpreter.reset()
    return "ℹ️ Returned to system prompt\n"


def _basic_while(interpreter: "Interpreter", args: str) -> str:
    """WHILE condition - Start while loop"""
    try:
        # Evaluate the condition
        result = interpreter.evaluate_expression(args.strip())
        if result != 0:  # Non-zero means true in BASIC
            interpreter.basic_while_stack.append(args.strip())
            return f"🔄 WHILE {args.strip()} (true)\n"
        # Skip to WEND - for now, just note it
        interpreter.basic_while_stack.append("")  # Marker for skipped
        return f"🔄 WHILE {args.strip()} (false, skipping)\n"
    except (ValueError, TypeError, ZeroDivisionError) as e:
        return f"❌ WHILE syntax error: {e}\n"


def _basic_wend(interpreter: "Interpreter") -> str:
    """WEND - End while loop"""
    if not interpreter.basic_while_stack:
        return "❌ WEND without WHILE\n"

    condition = interpreter.basic_while_stack.pop()
    if condition:  # If condition was true
        try:
            result = interpreter.evaluate_expression(condition)
            if result != 0:
                # In a real interpreter, this would jump back to WHILE
                # For now, just indicate the loop continues
                interpreter.basic_while_stack.append(condition)  # Push back
                return "🔄 WEND - condition still true, continuing loop\n"
            return "🔄 WEND - condition false, exiting loop\n"
        except (ValueError, TypeError, ZeroDivisionError) as e:
            return f"❌ WEND evaluation error: {e}\n"
    else:
        return "🔄 WEND - exiting skipped loop\n"


def _basic_do(interpreter: "Interpreter", args: str) -> str:
    """DO [WHILE condition|UNTIL condition] - Start do loop"""
    args = args.strip()
    if not args:
        # Simple DO
        interpreter.basic_do_stack.append(("", ""))
        return "🔄 DO (unconditional)\n"
    if args.upper().startswith("WHILE "):
        condition = args[6:].strip()
        interpreter.basic_do_stack.append(("WHILE", condition))
        return f"🔄 DO WHILE {condition}\n"
    if args.upper().startswith("UNTIL "):
        condition = args[6:].strip()
        interpreter.basic_do_stack.append(("UNTIL", condition))
        return f"🔄 DO UNTIL {condition}\n"
    return f"❌ Invalid DO syntax: {args}\n"


def _basic_loop(interpreter: "Interpreter", args: str) -> str:
    """LOOP [WHILE condition|UNTIL condition] - End do loop"""
    if not interpreter.basic_do_stack:
        return "❌ LOOP without DO\n"

    loop_type, condition = interpreter.basic_do_stack.pop()
    args = args.strip()

    if not args and not condition:
        # Simple LOOP
        interpreter.basic_do_stack.append((loop_type, condition))  # Continue
        return "🔄 LOOP - continuing\n"

    # Check condition
    if args.upper().startswith("WHILE "):
        loop_condition = args[6:].strip()
        try:
            result = interpreter.evaluate_expression(loop_condition)
            if result != 0:  # True
                interpreter.basic_do_stack.append((loop_type, condition))
                return f"🔄 LOOP WHILE {loop_condition} (true, continuing)\n"
            return f"🔄 LOOP WHILE {loop_condition} (false, exiting)\n"
        except (ValueError, TypeError, ZeroDivisionError) as e:
            return f"❌ LOOP WHILE evaluation error: {e}\n"

    if args.upper().startswith("UNTIL "):
        loop_condition = args[6:].strip()
        try:
            result = interpreter.evaluate_expression(loop_condition)
            if result == 0:  # False (UNTIL waits for true)
                interpreter.basic_do_stack.append((loop_type, condition))
                return f"🔄 LOOP UNTIL {loop_condition} (false, continuing)\n"
            return f"🔄 LOOP UNTIL {loop_condition} (true, exiting)\n"
        except (ValueError, TypeError, ZeroDivisionError) as e:
            return f"❌ LOOP UNTIL evaluation error: {e}\n"

    # LOOP with no condition specified - check original DO condition
    if loop_type == "WHILE" and condition:
        try:
            result = interpreter.evaluate_expression(condition)
            if result != 0:  # True
                interpreter.basic_do_stack.append((loop_type, condition))  # Continue
                return f"🔄 LOOP (while {condition} true, continuing)\n"
            return f"🔄 LOOP (while {condition} false, exiting)\n"
        except (ValueError, TypeError, ZeroDivisionError) as e:
            return f"❌ LOOP evaluation error: {e}\n"

    if loop_type == "UNTIL" and condition:
        try:
            result = interpreter.evaluate_expression(condition)
            if result == 0:  # False (UNTIL waits for true)
                interpreter.basic_do_stack.append((loop_type, condition))  # Continue
                return f"🔄 LOOP (until {condition} false, continuing)\n"
            return f"🔄 LOOP (until {condition} true, exiting)\n"
        except (ValueError, TypeError, ZeroDivisionError) as e:
            return f"❌ LOOP evaluation error: {e}\n"

    # Simple DO or infinite loop
    interpreter.basic_do_stack.append((loop_type, condition))
    return "🔄 LOOP - continuing\n"


def _basic_select(interpreter: "Interpreter", args: str) -> str:
    """SELECT CASE expression - Start select case"""
    interpreter.basic_select_expression = args.strip()
    interpreter.basic_in_select = True
    return ""


def _basic_case(interpreter: "Interpreter", args: str) -> str:
    """CASE value - Case in select"""
    if not interpreter.basic_in_select:
        return "❌ CASE without SELECT\n"

    # Simplified: just evaluate if case matches
    try:
        select_value = interpreter.evaluate_expression(
            interpreter.basic_select_expression
        )
        case_value = interpreter.evaluate_expression(args.strip())
        if select_value == case_value:
            return "✅ Case matched\n"
        return "⏭️ Case skipped\n"
    except (ValueError, TypeError):
        return "❌ Invalid CASE expression\n"


def _basic_end_select(_interpreter: "Interpreter") -> str:
    """END SELECT - End select case"""
    _interpreter.basic_in_select = False
    _interpreter.basic_select_expression = ""
    return ""


def _basic_sub(_interpreter: "Interpreter", _args: str) -> str:
    """SUB name[(params)] - Define subroutine"""
    # Simplified: just store that we're in a sub
    _interpreter.basic_in_sub = True
    return ""


def _basic_end_sub(_interpreter: "Interpreter") -> str:
    """END SUB - End subroutine"""
    _interpreter.basic_in_sub = False
    return ""


def _basic_function(_interpreter: "Interpreter", _args: str) -> str:
    """FUNCTION name[(params)] - Define function"""
    _interpreter.basic_in_function = True
    return ""


def _basic_end_function(_interpreter: "Interpreter") -> str:
    """END FUNCTION - End function"""
    _interpreter.basic_in_function = False
    return ""


def _basic_call(_interpreter: "Interpreter", args: str) -> str:
    """CALL subroutine[(args)] - Call subroutine"""
    return f"📞 Called subroutine: {args.strip()}\n"


def _basic_dim(interpreter: "Interpreter", args: str) -> str:
    """DIM variable[(dimensions)] - Declare array"""
    parts = args.split(",")
    for part in parts:
        part = part.strip()
        if "(" not in part or not part.endswith(")"):
            return f"❌ Invalid DIM syntax: {part}\n"

        # Extract name and size from the form NAME(size)
        name_part = part[: part.find("(")].strip().upper()
        size_part = part[part.find("(") + 1 : -1].strip()

        try:
            size = int(interpreter.evaluate_expression(size_part))
            # Create array initialized to 0.0
            # BASIC arrays are usually 0 to size (inclusive)
            interpreter.arrays[name_part] = [0.0] * (size + 1)
        except (ValueError, TypeError, ZeroDivisionError) as e:
            return f"❌ Error in DIM {name_part}: {e}\n"

    return ""


def _basic_read(interpreter: "Interpreter", args: str) -> str:
    """READ var1, var2... - Read from DATA"""
    vars_list = [v.strip().upper() for v in args.split(",")]
    for var_name in vars_list:
        if interpreter.data_pointer >= len(interpreter.data_values):
            return "❌ Out of DATA\n"

        val_str = interpreter.data_values[interpreter.data_pointer]
        interpreter.data_pointer += 1

        try:
            # Try to parse as number
            val = float(val_str)
            interpreter.set_typed_variable(var_name, val)
        except ValueError:
            # Store as string variable if it's a string
            interpreter.set_typed_variable(var_name, val_str)

    return ""


def _basic_restore(interpreter: "Interpreter", _args: str) -> str:
    """RESTORE [line] - Reset DATA pointer"""
    interpreter.data_pointer = 0
    return ""


def _basic_color(_interpreter: "Interpreter", args: str) -> str:
    """COLOR [foreground][,background] - Set colors"""
    parts = args.split(",")
    fg = "default"
    bg = "default"
    if len(parts) >= 1:
        fg = parts[0].strip()
    if len(parts) >= 2:
        bg = parts[1].strip()
    return f"🎨 Set color: FG={fg}, BG={bg}\n"


def _basic_width(_interpreter: "Interpreter", args: str) -> str:
    """WIDTH columns - Set screen width"""
    try:
        width = int(args.strip())
        return f"📐 Set width to {width} columns\n"
    except ValueError:
        return "❌ WIDTH requires numeric value\n"


def _basic_open(_interpreter: "Interpreter", args: str) -> str:
    """OPEN filename FOR mode AS #filenumber - Open file"""
    return f"📂 Opened file: {args.strip()}\n"


def _basic_close(_interpreter: "Interpreter", args: str) -> str:
    """CLOSE [#filenumber] - Close file"""
    return f"📂 Closed file: {args.strip()}\n"


def _basic_get(_interpreter: "Interpreter", args: str) -> str:
    """GET #filenumber, record - Read random record"""
    return f"📖 Read record: {args.strip()}\n"


def _basic_put(_interpreter: "Interpreter", args: str) -> str:
    """PUT #filenumber, record - Write random record"""
    return f"📝 Wrote record: {args.strip()}\n"


def _basic_end(interpreter: "Interpreter") -> str:
    """END - Stop execution"""
    interpreter.running = False
    return ""
