"""
BASIC language executor for Time Warp IDE.
Handles BASIC-specific commands and syntax.
"""

from typing import TYPE_CHECKING, List
import re
import math

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


def execute_basic(
    interpreter: "Interpreter",
    command: str,
    turtle: "TurtleState",
) -> str:
    cmd = command.strip().upper()
    if cmd.startswith("PRINT ") or cmd == "PRINT":
        args = command[6:] if len(command) > 6 else ""
        return _basic_print(interpreter, args)
    if cmd.startswith("LET "):
        return _basic_let(interpreter, command[4:])
    if cmd.startswith("FOR "):
        return _basic_for(interpreter, command[4:])
    if "=" in cmd and not cmd.startswith("IF ") and not cmd.startswith("FOR "):
        return _basic_let(interpreter, command)
    if cmd.startswith("INPUT "):
        return _basic_input(interpreter, command[6:])
    if cmd.startswith("IF "):
        return _basic_if(interpreter, command[3:], turtle)
    if cmd.startswith("GOTO "):
        return _basic_goto(interpreter, command[5:])
    if cmd.startswith("NEXT"):
        args = command[5:] if len(command) > 5 else ""
        return _basic_next(interpreter, args)
    if cmd.startswith("GOSUB "):
        return _basic_gosub(interpreter, command[6:])
    if cmd == "RETURN":
        return _basic_return(interpreter)
    if cmd == "END":
        interpreter.running = False
        return ""
    if cmd.startswith("REM ") or cmd == "REM":
        return ""
    if cmd == "CLS":
        turtle.clear()
        interpreter.text_lines.clear()
        return "🎨 Screen cleared\n"
    if cmd.startswith("SCREEN "):
        return _basic_screen(interpreter, command[7:])
    if cmd.startswith("LOCATE "):
        return _basic_locate(interpreter, command[7:])
    if cmd.startswith("LINE "):
        return _basic_line(interpreter, command[5:], turtle)
    if cmd.startswith("CIRCLE "):
        return _basic_circle(interpreter, command[7:], turtle)
    return f"❌ Unknown BASIC command: {command}\n"


def _basic_print(interpreter: "Interpreter", args: str) -> str:
    if not args.strip():
        interpreter.output.append("")
        return "\n"
    parts: List[str] = []
    current = ""
    in_quotes = False
    for ch in args:
        if ch == '"':
            in_quotes = not in_quotes
            current += ch
        elif ch == "," and not in_quotes:
            if current.strip():
                parts.append(current.strip())
            current = ""
        else:
            current += ch
    if current.strip():
        parts.append(current.strip())
    if not parts:
        interpreter.output.append("")
        return "\n"
    out_items: List[str] = []
    for item in parts:
        item_trim = item.strip()
        if (
            item_trim.startswith('"')
            and item_trim.endswith('"')
            and len(item_trim) >= 2
        ):
            out_items.append(item_trim[1:-1])
        elif item_trim.upper() == "INKEY$":
            out_items.append("")
        else:
            try:
                value = interpreter.evaluate_expression(item_trim)
                out_items.append(str(value))
            except Exception:
                if item_trim in interpreter.string_variables:
                    out_items.append(interpreter.string_variables[item_trim])
                elif item_trim in interpreter.variables:
                    out_items.append(str(interpreter.variables[item_trim]))
                else:
                    out_items.append(interpreter.interpolate_text(item_trim))
    result = " ".join(out_items)
    interpreter.output.append(result)
    return result + "\n"


def _basic_let(interpreter: "Interpreter", args: str) -> str:
    if "=" not in args:
        return "❌ LET requires format: variable = expression\n"
    parts = args.split("=", 1)
    var_name = parts[0].strip().upper()
    expr = parts[1].strip()
    if not var_name:
        return "❌ LET requires variable name\n"
    if var_name.endswith("$"):
        if expr.startswith('"') and expr.endswith('"'):
            interpreter.string_variables[var_name] = expr[1:-1]
        else:
            interpreter.string_variables[var_name] = str(expr)
        return ""
    try:
        result = interpreter.evaluate_expression(expr)
        interpreter.variables[var_name] = result
    except Exception as e:
        return f"❌ Error in LET: {e}\n"
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
    interpreter.start_input_request(prompt, var_name, not var_name.endswith("$"))
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
    except Exception as e:
        return f"❌ Error in IF condition: {e}\n"
    if condition_true and then_part:
        if then_part.isdigit():
            return _basic_goto(interpreter, then_part)
        else:
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
    except Exception as e:
        return f"❌ Error in GOTO: {e}\n"
    return ""


def _basic_for(interpreter: "Interpreter", args: str) -> str:
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
        interpreter.variables[var_name] = start_val
        from ..core.interpreter import ForContext

        context = ForContext(
            var_name=var_name,
            end_value=end_val,
            step=step_val,
            for_line=interpreter.current_line,
        )
        interpreter.for_stack.append(context)
    except Exception as e:
        return f"❌ Error in FOR: {e}\n"
    return ""


def _basic_next(interpreter: "Interpreter", _args: str) -> str:
    if not interpreter.for_stack:
        return "❌ NEXT without FOR\n"
    context = interpreter.for_stack[-1]
    current_val = interpreter.variables.get(context.var_name, 0)
    new_val = current_val + context.step
    interpreter.variables[context.var_name] = new_val
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
    except Exception as e:
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
    parts = args.split(",")
    if not parts:
        return "❌ SCREEN requires mode parameter\n"

    try:
        mode = int(parts[0].strip())
        from ..core.interpreter import ScreenMode

        if mode == 0:  # Text mode
            interpreter.screen_mode = ScreenMode.TEXT
            if len(parts) >= 3:
                cols = int(parts[1].strip())
                rows = int(parts[2].strip())
                interpreter.screen_mode.cols = cols
                interpreter.screen_mode.rows = rows
            cols, rows = (interpreter.screen_mode.cols, interpreter.screen_mode.rows)
            return f"🎨 Text mode ({cols}x{rows})\n"
        elif mode == 1:  # Graphics mode
            interpreter.screen_mode = ScreenMode.GRAPHICS
            if len(parts) >= 3:
                width = int(parts[1].strip())
                height = int(parts[2].strip())
                interpreter.screen_mode.width = width
                interpreter.screen_mode.height = height
            w, h = (interpreter.screen_mode.width, interpreter.screen_mode.height)
            return f"🎨 Graphics mode ({w}x{h})\n"
        elif mode == 2:  # High-res graphics
            interpreter.screen_mode = ScreenMode.GRAPHICS
            interpreter.screen_mode.width = 1024
            interpreter.screen_mode.height = 768
            return "🎨 High-res graphics mode activated (1024x768)\n"
        else:
            return f"❌ Unknown screen mode: {mode}\n"
    except ValueError as e:
        return f"❌ Invalid SCREEN parameters: {e}\n"


def _basic_locate(interpreter: "Interpreter", args: str) -> str:
    parts = args.split(",")
    if len(parts) < 2:
        return "❌ LOCATE requires row, col\n"
    try:
        row = int(parts[0].strip())
        col = int(parts[1].strip())
        interpreter.cursor_row = max(0, min(24, row - 1))
        interpreter.cursor_col = max(0, min(79, col - 1))
    except ValueError:
        return "❌ LOCATE requires numeric row and column\n"
    return ""


def _basic_line(_interpreter: "Interpreter", args: str, t: "TurtleState") -> str:
    """Draw line in BASIC graphics. Syntax: LINE x1,y1,x2,y2"""
    parts = args.split(",")
    if len(parts) != 4:
        return "❌ LINE requires x1,y1,x2,y2 coordinates\n"
    try:
        x1 = float(parts[0].strip())
        y1 = float(parts[1].strip())
        x2 = float(parts[2].strip())
        y2 = float(parts[3].strip())

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
    except ValueError:
        return "❌ LINE requires numeric coordinates\n"


def _basic_circle(i: "Interpreter", args: str, t: "TurtleState") -> str:
    """Draw circle in BASIC graphics. Syntax: CIRCLE x,y,radius"""
    parts = args.split(",")
    if len(parts) != 3:
        return "❌ CIRCLE requires x,y,radius\n"
    try:
        center_x = float(parts[0].strip())
        center_y = float(parts[1].strip())
        radius = float(parts[2].strip())

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

        for i in range(segments + 1):
            angle = i * angle_step
            rad = math.radians(angle)
            x = center_x + radius * math.cos(rad)
            y = center_y + radius * math.sin(rad)
            t.goto(x, y)

        # Restore pen state
        if not old_pen:
            t.penup()

        return ""
    except ValueError:
        return "❌ CIRCLE requires numeric coordinates and radius\n"
