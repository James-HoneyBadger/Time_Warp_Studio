"""
PILOT language executor for Time Warp IDE.
Handles PILOT-specific commands and syntax.
"""

from typing import TYPE_CHECKING
import re

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


def execute_pilot(
    interpreter: "Interpreter",
    command: str,
    turtle: "TurtleState",
) -> str:
    cmd = command.strip()
    if not cmd or len(cmd) < 2:
        return ""

    cmd_type = cmd[0].upper()
    if len(cmd) < 2 or cmd[1] != ":":
        return f"❌ Invalid PILOT command: {command}\n"

    rest = cmd[2:].strip()

    if cmd_type == "T":
        text = interpreter.interpolate_text(rest)
        interpreter.output.append(text)
        return text + "\n"
    elif cmd_type == "A":
        var_name = rest.strip()
        if not var_name:
            return "❌ A: requires variable name\n"
        # Start async input request
        interpreter.start_input_request("? ", var_name, is_numeric=False)
        return ""
    elif cmd_type == "M":
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
    elif cmd_type == "Y":
        if interpreter.last_match_succeeded:
            label = rest.strip()
            if label:
                interpreter.jump_to_label(label)
        return ""
    elif cmd_type == "N":
        if not interpreter.last_match_succeeded:
            label = rest.strip()
            if label:
                interpreter.jump_to_label(label)
        return ""
    elif cmd_type == "C":
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
        except Exception as e:
            return f"❌ Error in C: {e}\n"
        return ""
    elif cmd_type == "U":
        var_name = rest.strip()
        if not var_name:
            return "❌ U: requires variable name\n"
        value = interpreter.variables.get(var_name, "")
        text = str(value)
        interpreter.output.append(text)
        return text + "\n"
    elif cmd_type == "J":
        label = rest.strip()
        if label:
            interpreter.jump_to_label(label)
        return ""
    elif cmd_type == "L":
        return ""
    elif cmd_type == "E":
        interpreter.running = False
        return ""
    elif cmd_type == "R":
        return ""
    else:
        return f"❌ Unknown PILOT command: {cmd_type}:\n"
