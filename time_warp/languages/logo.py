"""
Logo language executor for Time Warp IDE.
Handles Logo-specific commands and syntax.
"""

from typing import TYPE_CHECKING, List
import re

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


def execute_logo(
    interpreter: "Interpreter",
    command: str,
    turtle: "TurtleState",
) -> str:
    cmd = command.strip().upper()
    words = cmd.split()
    if not words:
        return ""
    cmd_name = words[0]
    args = words[1:] if len(words) > 1 else []
    # User-defined procedures
    if cmd_name in interpreter.logo_procedures:
        return _logo_call_procedure(interpreter, cmd_name, args, turtle)
    if cmd_name in ["FORWARD", "FD"]:
        return _logo_forward(interpreter, turtle, args)
    if cmd_name in ["BACK", "BK", "BACKWARD"]:
        return _logo_back(interpreter, turtle, args)
    if cmd_name in ["LEFT", "LT"]:
        return _logo_left(interpreter, turtle, args)
    if cmd_name in ["RIGHT", "RT"]:
        return _logo_right(interpreter, turtle, args)
    if cmd_name in ["PENUP", "PU"]:
        turtle.penup()
        return ""
    if cmd_name in ["PENDOWN", "PD"]:
        turtle.pendown()
        return ""
    if cmd_name == "HOME":
        turtle.home()
        return ""
    if cmd_name in ["CLEARSCREEN", "CS", "CLEAR"]:
        turtle.clear()
        return ""
    if cmd_name in ["HIDETURTLE", "HT"]:
        turtle.hideturtle()
        return ""
    if cmd_name in ["SHOWTURTLE", "ST"]:
        turtle.showturtle()
        return ""
    if cmd_name == "SETXY":
        return _logo_setxy(interpreter, turtle, args)
    if cmd_name == "SETX":
        return _logo_setx(interpreter, turtle, args)
    if cmd_name == "SETY":
        return _logo_sety(interpreter, turtle, args)
    if cmd_name in ["SETHEADING", "SETH"]:
        return _logo_setheading(interpreter, turtle, args)
    if cmd_name in ["SETPENCOLOR", "SETPC"]:
        return _logo_setpencolor(interpreter, turtle, args)
    if cmd_name in ["SETCOLOR"]:
        return _logo_setcolor(interpreter, turtle, args)
    if cmd_name in ["SETBGCOLOR", "SETBG"]:
        return _logo_setbgcolor(interpreter, turtle, args)
    if cmd_name in ["SETPENWIDTH", "SETPW", "PENWIDTH", "SETPENSIZE"]:
        return _logo_setpenwidth(interpreter, turtle, args)
    if cmd_name == "REPEAT":
        return _logo_repeat(interpreter, turtle, command)
    if cmd_name == "IF":
        return _logo_if(interpreter, turtle, command)
    if cmd_name == "STOP":
        return _logo_stop(interpreter)
    if cmd_name == "TO":
        return _logo_to(interpreter, command)
    if cmd_name == "END":
        return _logo_end_procedure(interpreter)
    if cmd_name == "PRINT":
        return _logo_print(interpreter, " ".join(args))
    return f"❌ Unknown Logo command: {cmd_name}\n"


def _logo_eval_arg(interpreter: "Interpreter", arg: str) -> float:
    try:
        if arg.startswith(":"):
            var_name = arg[1:].upper()
            return interpreter.variables.get(var_name, 0)
        return interpreter.evaluate_expression(arg)
    except Exception:
        return 0.0


def _logo_eval_expr_str(interpreter: "Interpreter", expr: str) -> float:
    """Evaluate a Logo expression string with :VAR names and spaces."""
    # Replace :VAR with VAR for evaluator
    expr_norm = re.sub(r":([A-Za-z_][A-Za-z0-9_]*)", r"\1", expr)
    try:
        return interpreter.evaluate_expression(expr_norm)
    except Exception:
        return 0.0


def _logo_forward(
    interpreter: "Interpreter",
    turtle: "TurtleState",
    args: List[str],
) -> str:
    if not args:
        return "❌ FORWARD requires distance\n"
    distance = _logo_eval_arg(interpreter, args[0])
    turtle.forward(distance)
    return ""


def _logo_back(
    interpreter: "Interpreter",
    turtle: "TurtleState",
    args: List[str],
) -> str:
    if not args:
        return "❌ BACK requires distance\n"
    distance = _logo_eval_arg(interpreter, args[0])
    turtle.back(distance)
    return ""


def _logo_left(
    interpreter: "Interpreter",
    turtle: "TurtleState",
    args: List[str],
) -> str:
    if not args:
        return "❌ LEFT requires angle\n"
    angle_expr = " ".join(args)
    angle = _logo_eval_expr_str(interpreter, angle_expr)
    turtle.left(angle)
    return ""


def _logo_right(
    interpreter: "Interpreter",
    turtle: "TurtleState",
    args: List[str],
) -> str:
    if not args:
        return "❌ RIGHT requires angle\n"
    angle_expr = " ".join(args)
    angle = _logo_eval_expr_str(interpreter, angle_expr)
    turtle.right(angle)
    return ""


def _logo_setxy(
    interpreter: "Interpreter",
    turtle: "TurtleState",
    args: List[str],
) -> str:
    if len(args) < 2:
        return "❌ SETXY requires x and y coordinates\n"
    x = _logo_eval_arg(interpreter, args[0])
    y = _logo_eval_arg(interpreter, args[1])
    turtle.goto(x, y)
    return ""


def _logo_setx(
    interpreter: "Interpreter",
    turtle: "TurtleState",
    args: List[str],
) -> str:
    if not args:
        return "❌ SETX requires x coordinate\n"
    x = _logo_eval_arg(interpreter, args[0])
    turtle.setx(x)
    return ""


def _logo_sety(
    interpreter: "Interpreter",
    turtle: "TurtleState",
    args: List[str],
) -> str:
    if not args:
        return "❌ SETY requires y coordinate\n"
    y = _logo_eval_arg(interpreter, args[0])
    turtle.sety(y)
    return ""


def _logo_setheading(
    interpreter: "Interpreter",
    turtle: "TurtleState",
    args: List[str],
) -> str:
    if not args:
        return "❌ SETHEADING requires angle\n"
    angle = _logo_eval_arg(interpreter, args[0])
    turtle.setheading(angle)
    return ""


def _logo_setpencolor(
    interpreter: "Interpreter",
    turtle: "TurtleState",
    args: List[str],
) -> str:
    if not args:
        return "❌ SETPENCOLOR requires color\n"
    color = _logo_eval_arg(interpreter, args[0])
    turtle.pencolor(color)
    return ""


def _logo_setcolor(
    interpreter: "Interpreter",
    turtle: "TurtleState",
    args: List[str],
) -> str:
    if not args:
        return "❌ SETCOLOR requires color\n"
    color = _logo_eval_arg(interpreter, args[0])
    turtle.pencolor(color)
    return ""


def _logo_setbgcolor(
    interpreter: "Interpreter",
    turtle: "TurtleState",
    args: List[str],
) -> str:
    if not args:
        return "❌ SETBGCOLOR requires color\n"
    color = _logo_eval_arg(interpreter, args[0])
    turtle.bgcolor(color)
    return ""


def _logo_setpenwidth(
    interpreter: "Interpreter",
    turtle: "TurtleState",
    args: List[str],
) -> str:
    if not args:
        return "❌ SETPENWIDTH requires width\n"
    width = _logo_eval_arg(interpreter, args[0])
    turtle.pensize(width)
    return ""


def _logo_repeat(
    interpreter: "Interpreter",
    turtle: "TurtleState",
    command: str,
) -> str:
    # REPEAT count [commands]
    parts = command.upper().split("[", 1)
    if len(parts) < 2:
        return "❌ REPEAT requires [commands]\n"
    header = parts[0].strip()
    body = "[" + parts[1]
    header_words = header.split()
    if len(header_words) < 2:
        return "❌ REPEAT requires count\n"
    count_str = header_words[1]
    try:
        count = int(_logo_eval_expr_str(interpreter, count_str))
    except Exception:
        return f"❌ Invalid REPEAT count: {count_str}\n"
    # Extract commands between [ and ]
    if "[" not in body or "]" not in body:
        return "❌ REPEAT requires [commands]\n"
    start = body.find("[") + 1
    end = body.rfind("]")
    if start >= end:
        return "❌ REPEAT commands malformed\n"
    commands_text = body[start:end].strip()
    # Parse commands from space-separated tokens
    tokens = commands_text.split()
    commands = []
    i = 0
    while i < len(tokens):
        cmd = tokens[i].upper()
        i += 1
        # Collect arguments until next command or end
        args = []
        while i < len(tokens) and not tokens[i].upper() in [
            "FORWARD",
            "FD",
            "BACK",
            "BK",
            "BACKWARD",
            "LEFT",
            "LT",
            "RIGHT",
            "RT",
            "PENUP",
            "PU",
            "PENDOWN",
            "PD",
            "HOME",
            "CLEARSCREEN",
            "CS",
            "CLEAR",
            "HIDETURTLE",
            "HT",
            "SHOWTURTLE",
            "ST",
            "SETXY",
            "SETX",
            "SETY",
            "SETHEADING",
            "SETH",
            "SETPENCOLOR",
            "SETPC",
            "SETCOLOR",
            "SETBGCOLOR",
            "SETBG",
            "SETPENWIDTH",
            "SETPW",
            "PENWIDTH",
            "SETPENSIZE",
            "REPEAT",
            "TO",
            "END",
            "PRINT",
        ]:
            args.append(tokens[i])
            i += 1
        full_cmd = cmd + " " + " ".join(args) if args else cmd
        commands.append(full_cmd)
    for _ in range(count):
        for cmd in commands:
            if cmd:
                result = execute_logo(interpreter, cmd, turtle)
                if result:
                    return result
    return ""


def _logo_if(
    interpreter: "Interpreter",
    turtle: "TurtleState",
    command: str,
) -> str:
    # IF condition [commands]
    parts = command.upper().split("[", 1)
    if len(parts) < 2:
        return "❌ IF requires [commands]\n"
    header = parts[0].strip()
    body = "[" + parts[1]
    header_words = header.split()
    if len(header_words) < 2:
        return "❌ IF requires condition\n"
    condition_str = " ".join(header_words[1:])
    try:
        condition = _logo_eval_expr_str(interpreter, condition_str)
    except Exception:
        return f"❌ Invalid IF condition: {condition_str}\n"
    # Only execute if condition is true (non-zero)
    if condition != 0:
        # Extract commands between [ and ]
        if "[" not in body or "]" not in body:
            return "❌ IF requires [commands]\n"
        start = body.find("[") + 1
        end = body.rfind("]")
        if start >= end:
            return "❌ IF commands malformed\n"
        commands_text = body[start:end].strip()
        # Execute the commands as Logo code
        return execute_logo(interpreter, commands_text, turtle)
    return ""


def _logo_stop(interpreter: "Interpreter") -> str:
    # STOP command - return special marker to indicate procedure should stop
    return "STOP_PROCEDURE"


def _logo_to(
    interpreter: "Interpreter",
    command: str,
) -> str:
    # TO procedure_name [params] commands END
    # Extract procedure body between TO and END
    upper_cmd = command.upper()
    to_pos = upper_cmd.find("TO ")
    if to_pos == -1:
        return "❌ Invalid TO command\n"

    # Find the procedure name
    after_to = command[to_pos + 3 :].strip()
    name_end = after_to.find(" ")
    if name_end == -1:
        name_end = len(after_to)
    proc_name = after_to[:name_end].strip()

    # Find END to extract body
    end_pos = upper_cmd.find(" END")
    if end_pos == -1:
        return "❌ TO requires END\n"

    # Extract body (everything between procedure name and END)
    body_start = to_pos + 3 + name_end
    body = command[body_start:end_pos].strip()

    # Store just the procedure body
    interpreter.logo_procedures[proc_name] = body
    return ""


def _logo_end_procedure(interpreter: "Interpreter") -> str:
    # END marks end of procedure definition
    return ""


def _parse_logo_commands(body: str) -> List[str]:
    """Parse Logo procedure body into complete commands, handling brackets."""
    commands = []
    lines = body.split("\n")
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        if not line:
            i += 1
            continue

        # Check if this line starts a bracketed command
        if "[" in line and "]" not in line:
            # Multi-line bracketed command - collect until closing bracket
            cmd_lines = [line]
            bracket_count = line.count("[") - line.count("]")
            i += 1
            while i < len(lines) and bracket_count > 0:
                next_line = lines[i].strip()
                cmd_lines.append(next_line)
                bracket_count += next_line.count("[") - next_line.count("]")
                i += 1
            commands.append("\n".join(cmd_lines))
        else:
            # Single line command
            commands.append(line)
            i += 1

    return commands


def _logo_call_procedure(
    interpreter: "Interpreter",
    proc_name: str,
    args: List[str],
    turtle: "TurtleState",
) -> str:
    # Call user-defined procedure
    if proc_name not in interpreter.logo_procedures:
        return f"❌ Procedure {proc_name} not defined\n"

    proc_body = interpreter.logo_procedures[proc_name]

    # Handle parameters - evaluate arguments and set interpreter variables
    # Save old values to restore later
    old_values = {}
    if args:
        if len(args) >= 1:
            # Evaluate the argument expression
            size_value = _logo_eval_expr_str(interpreter, args[0])
            old_values["SIZE"] = interpreter.variables.get("SIZE")
            interpreter.variables["SIZE"] = size_value
        if len(args) >= 2:
            # Evaluate the argument expression
            depth_value = _logo_eval_expr_str(interpreter, args[1])
            old_values["DEPTH"] = interpreter.variables.get("DEPTH")
            interpreter.variables["DEPTH"] = depth_value

    try:
        # Execute the procedure body, handling multi-line bracketed commands
        # Parse the body into complete commands
        commands = _parse_logo_commands(proc_body)

        for cmd in commands:
            cmd = cmd.strip()
            if cmd:
                result = execute_logo(interpreter, cmd, turtle)
                # Check if STOP was encountered
                if "STOP_PROCEDURE" in result:
                    # Remove the STOP marker and stop execution
                    return result.replace("STOP_PROCEDURE", "")
                if result:
                    return result  # Return any error
        return ""
    finally:
        # Restore old variable values
        for var, value in old_values.items():
            if value is not None:
                interpreter.variables[var] = value
            else:
                interpreter.variables.pop(var, None)


def _logo_print(interpreter: "Interpreter", args: str) -> str:
    if not args.strip():
        interpreter.output.append("")
        return "\n"
    # Simple print for Logo
    interpreter.output.append(args)
    return args + "\n"
