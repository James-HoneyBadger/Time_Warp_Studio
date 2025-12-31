"""
Logo language executor for Time Warp IDE.
Handles Logo-specific commands and syntax.
"""

# pylint: disable=too-many-lines

import random
import re
from typing import TYPE_CHECKING, Any, List, Optional

from ..logging_config import get_logger
from ..utils.validators import (
    ValidationError,
    validate_arg_count,
    validate_range,
)

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState

logger = get_logger(__name__)

# Compiled regex patterns for performance
_VAR_PATTERN = re.compile(r":([A-Za-z_][A-Za-z0-9_]*)")
_RANDOM_PATTERN = re.compile(r"RANDOM\s+([A-Za-z0-9_.]+)", re.IGNORECASE)
_BRACKET_PATTERN = re.compile(r'\[|\]|"[^\s]*|[^\[\]\s]+')

# Color name to RGB mapping
COLOR_NAMES = {
    "BLACK": (0, 0, 0),
    "WHITE": (255, 255, 255),
    "RED": (255, 0, 0),
    "GREEN": (0, 255, 0),
    "BLUE": (0, 0, 255),
    "YELLOW": (255, 255, 0),
    "CYAN": (0, 255, 255),
    "MAGENTA": (255, 0, 255),
    "GRAY": (128, 128, 128),
    "GREY": (128, 128, 128),
}

LOGO_COMMANDS = {
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
    "SETPOSITION",
    "SETPOS",
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
    "IF",
    "IFELSE",
    "STOP",
    "OUTPUT",
    "TO",
    "END",
    "MAKE",
    # "THING", # Reporter
    "PRINT",
    "SHOW",
    "TYPE",
    "ARC",
    "FILLED",
    "LABEL",
    "WAIT",
    "BYE",
    "OP",
    "FOREVER",
    # "REPCOUNT", # Reporter
}


def _split_logo_commands(
    text: str,
    interpreter: Optional["Interpreter"] = None,
) -> List[str]:
    """
    Splits a Logo command string into individual commands,
    respecting brackets and nesting.
    """
    commands = []
    current_command = []
    bracket_depth = 0
    in_to_block = False

    # Tokenize: brackets, quoted strings, or other words
    # Handle "WORD (quote at start)
    tokens = _BRACKET_PATTERN.findall(text)

    for token in tokens:
        if token == "[":
            bracket_depth += 1
            current_command.append(token)
        elif token == "]":
            bracket_depth -= 1
            current_command.append(token)
        else:
            # If depth is 0 and token is a known command, start new command
            # BUT only if we have a current command accumulated.

            # Special handling for TO ... END blocks
            if bracket_depth == 0:
                if token.upper() == "TO":
                    if current_command and not in_to_block:
                        commands.append(" ".join(current_command))
                        current_command = []
                    in_to_block = True
                elif token.upper() == "END" and in_to_block:
                    current_command.append(token)
                    commands.append(" ".join(current_command))
                    current_command = []
                    in_to_block = False
                    continue

            if in_to_block:
                current_command.append(token)
                continue

            is_command = token.upper() in LOGO_COMMANDS
            if not is_command and interpreter and bracket_depth == 0:
                # Check if it's a user-defined procedure
                is_command = token.upper() in interpreter.logo_procedures

            if bracket_depth == 0 and is_command:
                if current_command:
                    commands.append(" ".join(current_command))
                    current_command = []
            current_command.append(token)

    if current_command:
        commands.append(" ".join(current_command))

    return commands


def execute_logo(
    interpreter: "Interpreter",
    command: str,
    turtle: Optional["TurtleState"] = None,
) -> str:
    """
    Executes a Logo command string.
    Handles multiple commands on a single line by splitting them.
    """
    commands = _split_logo_commands(command, interpreter)
    outputs = []
    for cmd in commands:
        result = _execute_single_logo_command(interpreter, cmd, turtle)
        outputs.append(result)
        if "STOP_PROCEDURE" in result:
            break
    return "".join(outputs)


def _execute_single_logo_command(
    interpreter: "Interpreter",
    command: str,
    turtle: Optional["TurtleState"],
) -> str:
    # Strip comment
    comment_idx = command.find(";")
    if comment_idx != -1:
        command = command[:comment_idx]

    cmd = command.strip().upper()
    if not cmd:
        return ""
    words = cmd.split()
    if not words:
        return ""
    cmd_name = words[0]
    args = words[1:] if len(words) > 1 else []
    # Commands that require a TurtleState (graphics) to be available
    needs_turtle = {
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
        "IF",
        "TO",
        "END",
        "ARC",
        "FILLED",
    }
    if turtle is None and cmd_name in needs_turtle:
        return "❌ Graphics not available for this command"
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
        if turtle is None:
            return "❌ Graphics not available for this command\n"
        turtle.penup()
        return ""
    if cmd_name in ["PENDOWN", "PD"]:
        if turtle is None:
            return "❌ Graphics not available for this command\n"
        turtle.pendown()
        return ""
    if cmd_name == "HOME":
        if turtle is None:
            return "❌ Graphics not available for this command\n"
        turtle.home()
        return ""
    if cmd_name in ["CLEARSCREEN", "CS", "CLEAR"]:
        if turtle is None:
            return "❌ Graphics not available for this command\n"
        turtle.clear()
        return ""
    if cmd_name in ["HIDETURTLE", "HT"]:
        if turtle is None:
            return "❌ Graphics not available for this command\n"
        turtle.hideturtle()
        return ""
    if cmd_name in ["SHOWTURTLE", "ST"]:
        if turtle is None:
            return "❌ Graphics not available for this command\n"
        turtle.showturtle()
        return ""
    if cmd_name == "SETXY":
        return _logo_setxy(interpreter, turtle, args)
    if cmd_name in ["SETPOSITION", "SETPOS"]:
        return _logo_setposition(interpreter, turtle, args)
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
    if cmd_name == "OUTPUT":
        return _logo_output(interpreter, args)
    if cmd_name == "TO":
        return _logo_to(interpreter, command)
    if cmd_name == "END":
        return _logo_end_procedure(interpreter)
    if cmd_name == "MAKE":
        return _logo_make(interpreter, args)
    if cmd_name == "THING":
        return _logo_thing(interpreter, args)
    if cmd_name == "PRINT":
        return _logo_print(interpreter, args)
    if cmd_name == "SHOW":
        return _logo_show(interpreter, args)
    if cmd_name == "TYPE":
        return _logo_type(interpreter, args)
    # Data structure operations
    if cmd_name == "WORD":
        return _logo_word(interpreter, args)
    if cmd_name == "LIST":
        return _logo_list(interpreter, args)
    if cmd_name == "SENTENCE":
        return _logo_sentence(interpreter, args)
    if cmd_name == "FIRST":
        return _logo_first(interpreter, args)
    if cmd_name == "LAST":
        return _logo_last(interpreter, args)
    if cmd_name == "BUTFIRST":
        return _logo_butfirst(interpreter, args)
    if cmd_name == "BUTLAST":
        return _logo_butlast(interpreter, args)
    if cmd_name == "ITEM":
        return _logo_item(interpreter, args)
    if cmd_name == "COUNT":
        return _logo_count(interpreter, args)
    # Arithmetic operations
    if cmd_name in ["SUM", "+"]:
        return _logo_sum(interpreter, args)
    if cmd_name in ["DIFFERENCE", "-"]:
        return _logo_difference(interpreter, args)
    if cmd_name in ["PRODUCT", "*"]:
        return _logo_product(interpreter, args)
    if cmd_name in ["QUOTIENT", "/"]:
        return _logo_quotient(interpreter, args)
    if cmd_name == "RANDOM":
        return _logo_random(interpreter, args)
    # Control structures
    if cmd_name == "IFELSE":
        return _logo_ifelse(interpreter, turtle, cmd)
    if cmd_name == "FOREVER":
        return _logo_forever(interpreter, turtle, command)
    if cmd_name == "REPCOUNT":
        return _logo_repcount(interpreter)
    # Graphics enhancements
    if cmd_name == "ARC":
        return _logo_arc(interpreter, turtle, args)
    if cmd_name == "FILLED":
        return _logo_filled(interpreter, turtle, command)
    if cmd_name == "LABEL":
        return _logo_label(interpreter, args)
    return f"❌ Unknown Logo command: {cmd_name}\n"


def _logo_eval_arg(interpreter: "Interpreter", arg: str) -> float:
    """Evaluate a single Logo argument (variable reference or expression).

    Args:
        interpreter: Interpreter instance
        arg: Single argument string

    Returns:
        Numeric value

    Raises:
        ValueError: If argument cannot be evaluated
    """
    if arg.startswith(":"):
        var_name = arg[1:].upper()
        if var_name not in interpreter.variables:
            raise ValueError(f"Undefined variable: {arg}")
        return interpreter.variables[var_name]

    try:
        return _logo_eval_expr_str(interpreter, arg)
    except (ValueError, TypeError, ZeroDivisionError) as e:
        raise ValueError(f"Cannot evaluate argument '{arg}': {e}") from e


def _handle_prefix_mult(expr: str) -> str:
    """
    Handle (* A B C) -> (A * B * C)
    Iterative implementation to avoid regex recursion limits.
    """
    result = []
    i = 0
    n = len(expr)
    while i < n:
        if expr[i] == "(":
            # Check for (*
            j = i + 1
            while j < n and expr[j].isspace():
                j += 1

            if j < n and expr[j] == "*":
                # Found (*
                # Check if there are nested parens before the closing )
                k = j + 1
                # Skip whitespace after *
                while k < n and expr[k].isspace():
                    k += 1

                # Scan for )
                content_start = k
                has_nested = False
                while k < n:
                    if expr[k] == "(":
                        has_nested = True
                        break
                    if expr[k] == ")":
                        break
                    k += 1

                if k < n and expr[k] == ")" and not has_nested:
                    # Found simple (* ... )
                    content = expr[content_start:k]
                    parts = content.split()
                    new_content = " * ".join(parts)
                    result.append(f"({new_content})")
                    i = k + 1
                    continue

        result.append(expr[i])
        i += 1
    return "".join(result)


def _logo_eval_expr_str(interpreter: "Interpreter", expr: str) -> float:
    """Evaluate a Logo expression string with :VAR names and spaces.

    Args:
        interpreter: Interpreter instance
        expr: Expression string (may contain :VAR references)

    Returns:
        Numeric result of expression

    Raises:
        ValueError: If expression cannot be evaluated
    """
    # Replace :VAR with VAR for evaluator
    expr_norm = _VAR_PATTERN.sub(r"\1", expr)

    # Handle RANDOM N -> FLOOR(RND * N)
    # Matches RANDOM followed by number or variable
    expr_norm = _RANDOM_PATTERN.sub(
        r"FLOOR(RND * \1)",
        expr_norm,
    )

    # Handle prefix multiplication (* A B ...) -> (A * B * ...)
    # Only handles simple cases without nested parentheses
    expr_norm = _handle_prefix_mult(expr_norm)

    # Replace = with == for Python eval, but protect >=, <=, !=
    # First replace >= with __GE__, <= with __LE__, <> with __NE__
    expr_norm = (
        expr_norm.replace(">=", "__GE__")
        .replace("<=", "__LE__")
        .replace("<>", "__NE__")
    )
    # Replace = with ==
    expr_norm = expr_norm.replace("=", "==")
    # Restore
    expr_norm = (
        expr_norm.replace("__GE__", ">=")
        .replace("__LE__", "<=")
        .replace("__NE__", "!=")
    )

    try:
        return interpreter.evaluate_expression(expr_norm)
    except (ValueError, TypeError, ZeroDivisionError) as e:
        raise ValueError(
            f"Cannot evaluate Logo expression '{expr}': {e}"
        ) from e


def _logo_forward(
    interpreter: "Interpreter",
    turtle: Optional["TurtleState"],
    args: List[str],
) -> str:
    """FORWARD distance - Move turtle forward.

    Args:
        interpreter: Interpreter instance
        turtle: Turtle graphics state
        args: Distance arguments

    Returns:
        Status or error message
    """
    try:
        # Consume arguments intelligently to handle expressions like (* 10 20)
        consumed = _consume_logo_args(args, 1)
        validate_arg_count(consumed, 1, "FORWARD")

        distance = _logo_eval_arg(interpreter, consumed[0])
        if turtle is None:
            return "❌ Graphics not available for this command\n"
        turtle.forward(distance)
        logger.debug("FORWARD %s", distance)
    except ValidationError as e:
        return f"❌ {e}\n"
    except ValueError as e:
        print(f"DEBUG: FORWARD error: {e}")
        logger.error("FORWARD error: %s", e)
        return f"❌ {e}\n"

    return ""


def _logo_back(
    interpreter: "Interpreter",
    turtle: Optional["TurtleState"],
    args: List[str],
) -> str:
    try:
        consumed = _consume_logo_args(args, 1)
        validate_arg_count(consumed, 1, "BACK")
        distance = _logo_eval_arg(interpreter, consumed[0])
        if turtle is None:
            return "❌ Graphics not available for this command\n"
        turtle.back(distance)
    except ValidationError as e:
        return f"❌ {e}\n"
    except ValueError as e:
        return f"❌ {e}\n"
    return ""


def _logo_left(
    interpreter: "Interpreter",
    turtle: Optional["TurtleState"],
    args: List[str],
) -> str:
    try:
        consumed = _consume_logo_args(args, 1)
        validate_arg_count(consumed, 1, "LEFT")
        angle = _logo_eval_arg(interpreter, consumed[0])
        if turtle is None:
            return "❌ Graphics not available for this command\n"
        turtle.left(angle)
    except ValidationError as e:
        return f"❌ {e}\n"
    except ValueError as e:
        return f"❌ {e}\n"
    return ""


def _logo_right(
    interpreter: "Interpreter",
    turtle: Optional["TurtleState"],
    args: List[str],
) -> str:
    try:
        consumed = _consume_logo_args(args, 1)
        validate_arg_count(consumed, 1, "RIGHT")
        angle = _logo_eval_arg(interpreter, consumed[0])
        if turtle is None:
            return "❌ Graphics not available for this command\n"
        turtle.right(angle)
    except ValidationError as e:
        return f"❌ {e}\n"
    except ValueError as e:
        return f"❌ {e}\n"
    return ""


def _logo_setxy(
    interpreter: "Interpreter",
    turtle: Optional["TurtleState"],
    args: List[str],
) -> str:
    """SETXY x y - Set turtle absolute position.

    Args:
        interpreter: Interpreter instance
        turtle: Turtle graphics state
        args: X and Y coordinate arguments

    Returns:
        Status or error message
    """
    try:
        consumed = _consume_logo_args(args, 2)
        validate_arg_count(consumed, 2, "SETXY")
        x = _logo_eval_arg(interpreter, consumed[0])
        y = _logo_eval_arg(interpreter, consumed[1])
        if turtle is None:
            return "❌ Graphics not available for this command\n"
        turtle.goto(x, y)
        logger.debug("SETXY %s %s", x, y)
    except ValidationError as e:
        return f"❌ {e}\n"
    except ValueError as e:
        logger.error("SETXY error: %s", e)
        return f"❌ {e}\n"

    return ""


def _logo_setposition(
    interpreter: "Interpreter",
    turtle: Optional["TurtleState"],
    args: List[str],
) -> str:
    """SETPOSITION [x y] - Set turtle absolute position from list.

    Args:
        interpreter: Interpreter instance
        turtle: Turtle graphics state
        args: List argument containing X and Y

    Returns:
        Status or error message
    """
    try:
        # Expecting one argument which is a list [x y]
        # But the parser might have split it if it wasn't careful,
        # or passed it as a single string "[x y]"

        if not args:
            return "❌ SETPOSITION requires a list argument [x y]\n"

        arg = " ".join(args)
        if arg.startswith("[") and arg.endswith("]"):
            content = arg[1:-1].strip()
            parts = content.split()
            if len(parts) != 2:
                return "❌ SETPOSITION list must contain exactly 2 numbers\n"

            x = _logo_eval_arg(interpreter, parts[0])
            y = _logo_eval_arg(interpreter, parts[1])

            if turtle is None:
                return "❌ Graphics not available for this command\n"

            turtle.goto(x, y)
            logger.debug("SETPOSITION %s %s", x, y)
        else:
            # Maybe it was passed as two arguments?
            # Some dialects allow SETPOS x y
            if len(args) == 2:
                return _logo_setxy(interpreter, turtle, args)
            return "❌ SETPOSITION requires a list argument [x y]\n"

    except ValidationError as e:
        return f"❌ {e}\n"
    except ValueError as e:
        logger.error("SETPOSITION error: %s", e)
        return f"❌ {e}\n"

    return ""


def _logo_setx(
    interpreter: "Interpreter",
    turtle: Optional["TurtleState"],
    args: List[str],
) -> str:
    if not args:
        return "❌ SETX requires x coordinate\n"
    x_expr = " ".join(args)
    x = _logo_eval_expr_str(interpreter, x_expr)
    if turtle is None:
        return "❌ Graphics not available for this command\n"
    turtle.setx(x)
    return ""


def _logo_sety(
    interpreter: "Interpreter",
    turtle: Optional["TurtleState"],
    args: List[str],
) -> str:
    if not args:
        return "❌ SETY requires y coordinate\n"
    y_expr = " ".join(args)
    y = _logo_eval_expr_str(interpreter, y_expr)
    if turtle is None:
        return "❌ Graphics not available for this command\n"
    turtle.sety(y)
    return ""


def _logo_setheading(
    interpreter: "Interpreter",
    turtle: Optional["TurtleState"],
    args: List[str],
) -> str:
    if not args:
        return "❌ SETHEADING requires angle\n"
    angle_expr = " ".join(args)
    angle = _logo_eval_expr_str(interpreter, angle_expr)
    if turtle is None:
        return "❌ Graphics not available for this command\n"
    turtle.setheading(angle)
    return ""


def _logo_setpencolor(
    interpreter: "Interpreter",
    turtle: Optional["TurtleState"],
    args: List[str],
) -> str:
    return _logo_setcolor(interpreter, turtle, args)


def _logo_setcolor(
    interpreter: "Interpreter",
    turtle: Optional["TurtleState"],
    args: List[str],
) -> str:
    if not args:
        return "❌ SETCOLOR requires color\n"
    if len(args) == 1:
        # Named color or hex
        color_str = args[0].strip().strip('"')
        if turtle is None:
            return "❌ Graphics not available for this command\n"
        turtle.pencolor(color_str)
    elif len(args) == 3:
        # RGB values
        try:
            r = int(_logo_eval_arg(interpreter, args[0]))
            g = int(_logo_eval_arg(interpreter, args[1]))
            b = int(_logo_eval_arg(interpreter, args[2]))
            if turtle is None:
                return "❌ Graphics not available for this command\n"
            turtle.setcolor(r, g, b)
        except ValueError:
            return "❌ SETCOLOR RGB values must be integers\n"
    else:
        return "❌ SETCOLOR requires 1 color name/hex or 3 RGB values\n"
    return ""


def _logo_setbgcolor(
    interpreter: "Interpreter",
    turtle: Optional["TurtleState"],
    args: List[str],
) -> str:
    if not args:
        return "❌ SETBGCOLOR requires color\n"
    if len(args) == 1:
        # Named color or hex
        color_str = args[0].strip().strip('"')
        # For bgcolor, we only support RGB for now, but could extend
        if color_str.upper() in COLOR_NAMES:
            if turtle is None:
                return "❌ Graphics not available for this command\n"
            turtle.setbgcolor(*COLOR_NAMES[color_str.upper()])
        else:
            return "❌ SETBGCOLOR only supports named colors for now\n"
    elif len(args) == 3:
        # RGB values
        try:
            r = int(_logo_eval_arg(interpreter, args[0]))
            g = int(_logo_eval_arg(interpreter, args[1]))
            b = int(_logo_eval_arg(interpreter, args[2]))
            if turtle is None:
                return "❌ Graphics not available for this command\n"
            turtle.setbgcolor(r, g, b)
        except ValueError:
            return "❌ SETBGCOLOR RGB values must be integers\n"
    else:
        return "❌ SETBGCOLOR requires 1 color name or 3 RGB values\n"
    return ""


def _logo_setpenwidth(
    interpreter: "Interpreter",
    turtle: Optional["TurtleState"],
    args: List[str],
) -> str:
    """SETPENWIDTH width - Set pen drawing width.

    Args:
        interpreter: Interpreter instance
        turtle: Turtle graphics state
        args: Width argument

    Returns:
        Status or error message
    """
    try:
        validate_arg_count(args, 1, "SETPENWIDTH")
        width_expr = " ".join(args)
        width = _logo_eval_expr_str(interpreter, width_expr)
        validate_range(int(width), 1, 100, "pen width")
        if turtle is None:
            return "❌ Graphics not available for this command\n"
        turtle.setpenwidth(width)
        logger.debug("SETPENWIDTH %s", width)
    except ValidationError as e:
        return f"❌ {e}\n"
    except ValueError as e:
        logger.error("SETPENWIDTH error: %s", e)
        return f"❌ {e}\n"

    return ""


def _logo_repeat(
    interpreter: "Interpreter",
    turtle: Optional["TurtleState"],
    command: str,
) -> str:
    # REPEAT count [commands]
    start_bracket = command.find("[")
    if start_bracket == -1:
        return "❌ REPEAT requires [commands]\n"

    header = command[:start_bracket].strip()
    end_bracket = command.rfind("]")
    if end_bracket == -1 or end_bracket < start_bracket:
        return "❌ REPEAT requires [commands]\n"

    body_content = command[start_bracket + 1 : end_bracket].strip()

    if not header.upper().startswith("REPEAT"):
        return "❌ Invalid REPEAT command\n"

    count_str = header[6:].strip()
    if not count_str:
        return "❌ REPEAT requires count\n"

    try:
        count = int(_logo_eval_expr_str(interpreter, count_str))
    except (ValueError, TypeError, ZeroDivisionError):
        return f"❌ Invalid REPEAT count: {count_str}\n"

    output = ""
    for _ in range(count):
        output += execute_logo(interpreter, body_content, turtle)

    return output


def _logo_if(
    interpreter: "Interpreter",
    turtle: Optional["TurtleState"],
    command: str,
) -> str:
    # IF condition [commands]
    start_bracket = command.find("[")
    if start_bracket == -1:
        return "❌ IF requires [commands]\n"

    header = command[:start_bracket].strip()
    end_bracket = command.rfind("]")
    if end_bracket == -1 or end_bracket < start_bracket:
        return "❌ IF requires [commands]\n"

    body_content = command[start_bracket + 1 : end_bracket].strip()

    if not header.upper().startswith("IF"):
        return "❌ Invalid IF command\n"

    condition_str = header[2:].strip()
    if not condition_str:
        return "❌ IF requires condition\n"

    try:
        condition = _logo_eval_expr_str(interpreter, condition_str)
        # print(f"DEBUG: IF condition '{condition_str}' -> {condition}")
    except (ValueError, TypeError, ZeroDivisionError):
        return f"❌ Invalid IF condition: {condition_str}\n"

    # Only execute if condition is true (non-zero)
    if condition != 0:
        return execute_logo(interpreter, body_content, turtle)
    return ""


def _logo_stop(_interpreter: "Interpreter") -> str:
    # STOP command - return special marker to indicate procedure should stop
    return "STOP_PROCEDURE"


def _logo_output(interpreter: "Interpreter", args: List[str]) -> str:
    """OUTPUT value - Return value from procedure"""
    if not args:
        return "❌ OUTPUT requires value\n"

    try:
        # Evaluate the return value
        # We need to consume all remaining args as the expression
        expr = " ".join(args)
        val = _logo_eval_expr_str(interpreter, expr)
        # Return value followed by STOP marker
        # The caller (_logo_call_procedure) will strip the marker
        return f"{val}STOP_PROCEDURE"
    except (ValueError, TypeError) as e:
        return f"❌ Invalid OUTPUT value: {e}\n"


def _logo_to(
    interpreter: "Interpreter",
    command: str,
) -> str:
    # TO procedure_name [params] commands END
    # Extract procedure body between TO and END
    upper_cmd = command.upper()
    to_pos = upper_cmd.find("TO ")
    if to_pos == -1:
        if upper_cmd.startswith("TO "):
            to_pos = 0
        else:
            return "❌ Invalid TO command\n"

    end_pos = upper_cmd.find(" END")
    if end_pos == -1:
        return "❌ TO requires END\n"

    # Get the content between TO and END
    content = command[to_pos:end_pos].strip()

    lines = content.split("\n")
    header = lines[0].strip()

    header_parts = header.split()
    if len(header_parts) < 2:
        return "❌ Invalid TO command\n"

    proc_name = header_parts[1].upper()
    params = []

    # Handle single-line vs multi-line
    if len(lines) > 1:
        # Multi-line: TO NAME :ARGS \n BODY
        for part in header_parts[2:]:
            if part.startswith(":"):
                params.append(part[1:].upper())
        body = "\n".join(lines[1:])
    else:
        # Single-line: TO NAME :ARGS BODY
        body_parts = []
        in_params = True
        for i in range(2, len(header_parts)):
            part = header_parts[i]
            if in_params and part.startswith(":"):
                params.append(part[1:].upper())
            else:
                in_params = False
                body_parts.append(part)
        body = " ".join(body_parts)

    interpreter.logo_procedure_params[proc_name] = params
    interpreter.logo_procedures[proc_name] = body
    return ""


def _logo_end_procedure(_interpreter: "Interpreter") -> str:
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


def _consume_logo_args(args: List[str], num_params: int) -> List[str]:
    """
    Consume tokens from args to form num_params arguments,
    handling infix operators and parentheses.
    """
    consumed_args = []
    current_idx = 0
    operators = {"+", "-", "*", "/", "%", "^", "=", "<", ">", "<=", ">=", "<>"}

    for _ in range(num_params):
        if current_idx >= len(args):
            break

        current_arg_tokens = []
        balance = 0

        while current_idx < len(args):
            token = args[current_idx]
            current_arg_tokens.append(token)
            current_idx += 1

            balance += token.count("(")
            balance -= token.count(")")

            if balance == 0:
                # Check if we should stop
                # If token is operator, continue (e.g. '2 +')
                if token in operators:
                    continue

                # If next token is operator, continue (e.g. '2' then '+')
                if current_idx < len(args) and args[current_idx] in operators:
                    continue

                # Otherwise, we are done with this argument
                break

        consumed_args.append(" ".join(current_arg_tokens))

    return consumed_args


def _logo_call_procedure(
    interpreter: "Interpreter",
    proc_name: str,
    args: List[str],
    turtle: Optional["TurtleState"],
) -> str:
    # Call user-defined procedure
    if proc_name not in interpreter.logo_procedures:
        return f"❌ Procedure {proc_name} not defined\n"

    proc_body = interpreter.logo_procedures[proc_name]
    params = interpreter.logo_procedure_params.get(proc_name, [])

    # Handle parameters - evaluate arguments and set interpreter variables
    # Save old values to restore later
    old_values = {}

    # Map args to params
    num_params = len(params)

    # DEBUG
    depth = interpreter.variables.get("__RECURSION_DEPTH__", 0)
    interpreter.variables["__RECURSION_DEPTH__"] = depth + 1

    # Use smart argument consumption
    used_args = _consume_logo_args(args, num_params)

    if len(used_args) < num_params:
        return f"❌ Procedure {proc_name} requires {num_params} inputs\n"

    for i, param_name in enumerate(params):
        arg_expr = used_args[i]
        # Evaluate the argument expression
        val = _logo_eval_expr_str(interpreter, arg_expr)
        old_values[param_name] = interpreter.variables.get(param_name)
        interpreter.variables[param_name] = val

    try:
        # Execute the procedure body, handling multi-line bracketed commands
        # Parse the body into complete commands
        commands = _parse_logo_commands(proc_body)
        output = ""

        for cmd in commands:
            cmd = cmd.strip()
            if cmd:
                # Pass interpreter to execute_logo for nested calls
                result = execute_logo(interpreter, cmd, turtle)

                # Check if STOP was encountered
                if "STOP_PROCEDURE" in result:
                    # Remove the STOP marker and stop execution
                    output += result.replace("STOP_PROCEDURE", "")
                    return output

                # Check for error (simple heuristic)
                if result.startswith("❌"):
                    return output + result

                output += result
        return output
    finally:
        # Restore old variable values
        for var, value in old_values.items():
            if value is not None:
                interpreter.variables[var] = value
            else:
                interpreter.variables.pop(var, None)


def _logo_print(interpreter: "Interpreter", args: List[str]) -> str:
    if not args:
        interpreter.output.append("")
        return "\n"

    # Check for variable access :VAR
    if len(args) == 1 and args[0].startswith(":"):
        var_name = args[0][1:].upper()
        val: Any = None
        if var_name in interpreter.variables:
            val = interpreter.variables[var_name]
        elif var_name in interpreter.string_variables:
            val = interpreter.string_variables[var_name]
        elif var_name in interpreter.logo_lists:
            val = interpreter.logo_lists[var_name]

        if val is not None:
            val_str = str(val)
            interpreter.output.append(val_str)
            return val_str + "\n"

    # Check if this is a function call
    func_names = [
        "SUM",
        "DIFFERENCE",
        "PRODUCT",
        "QUOTIENT",
        "RANDOM",
        "WORD",
        "LIST",
        "SENTENCE",
        "FIRST",
        "LAST",
        "BUTFIRST",
        "BUTLAST",
        "ITEM",
        "THING",
    ]
    if len(args) >= 1 and args[0].upper() in func_names:
        # Execute the function and print result
        result = execute_logo(interpreter, " ".join(args), None)
        # Remove the trailing newline if present
        if result.endswith("\n"):
            result = result[:-1]
        interpreter.output.append(result)
        return result + "\n"
    # Otherwise treat as literal
    output = " ".join(args)
    interpreter.output.append(output)
    return output + "\n"


# Variable and data operations
def _logo_make(interpreter: "Interpreter", args: List[str]) -> str:
    """MAKE variable value - Set a variable to a value"""
    if len(args) < 2:
        return "❌ MAKE requires variable name and value\n"
    var_name = args[0].strip('"')  # Remove quotes if present
    value_expr = " ".join(args[1:])

    # Try to evaluate as number first
    try:
        # Use _logo_eval_expr_str to handle RANDOM syntax and :VAR substitution
        value = _logo_eval_expr_str(interpreter, value_expr)
        interpreter.variables[var_name.upper()] = value
        return ""
    except (ValueError, TypeError):
        # If not a number, treat as string or list
        if value_expr.startswith('"') and value_expr.endswith('"'):
            # Quoted string
            interpreter.string_variables[var_name.upper()] = value_expr[1:-1]
        elif value_expr.startswith("[") and value_expr.endswith("]"):
            # List
            list_content = value_expr[1:-1].strip()
            if list_content:
                items = [item.strip() for item in list_content.split()]
                interpreter.logo_lists[var_name.upper()] = items
            else:
                interpreter.logo_lists[var_name.upper()] = []
        else:
            # Default to string
            interpreter.string_variables[var_name.upper()] = value_expr
    return ""


def _logo_thing(interpreter: "Interpreter", args: List[str]) -> str:
    """THING variable - Get the value of a variable"""
    if not args:
        return "❌ THING requires variable name\n"
    var_name = args[0].strip('"').upper()

    if var_name in interpreter.variables:
        return str(interpreter.variables[var_name])
    if var_name in interpreter.string_variables:
        return interpreter.string_variables[var_name]
    if var_name in interpreter.logo_lists:
        return str(interpreter.logo_lists[var_name])
    if var_name in interpreter.logo_arrays:
        return str(interpreter.logo_arrays[var_name])
    return f"❌ Variable {var_name} not found\n"


def _logo_show(interpreter: "Interpreter", args: List[str]) -> str:
    """SHOW value - Display value with brackets around lists"""
    if not args:
        return "❌ SHOW requires value\n"
    value = " ".join(args)
    interpreter.output.append(value)
    return value + "\n"


def _logo_type(interpreter: "Interpreter", args: List[str]) -> str:
    """TYPE value - Display value without newline"""
    if not args:
        return "❌ TYPE requires value\n"
    value = " ".join(args)
    interpreter.output.append(value)
    return value


# Data structure operations
def _logo_word(interpreter: "Interpreter", args: List[str]) -> str:
    """WORD word1 word2 - Concatenate words"""
    if len(args) < 2:
        return "❌ WORD requires at least two arguments\n"

    parts = []
    for arg in args:
        # Evaluate each argument
        try:
            val = interpreter.evaluate_expression(arg)
            parts.append(str(int(val)) if val == int(val) else str(val))
        except (ValueError, TypeError):
            parts.append(arg.strip('"'))

    return "".join(parts)


def _logo_list(interpreter: "Interpreter", args: List[str]) -> str:
    """LIST item1 item2 - Create a list from items"""
    result: List[Any] = []
    for arg in args:
        try:
            val = interpreter.evaluate_expression(arg)
            result.append(val)
        except (ValueError, TypeError):
            result.append(arg.strip('"'))
    interpreter.logo_lists["_TEMP_LIST"] = result
    return str(result)


def _logo_sentence(interpreter: "Interpreter", args: List[str]) -> str:
    """SENTENCE list1 list2 - Combine lists or create from items"""
    result = []
    for arg in args:
        if arg.startswith("[") and arg.endswith("]"):
            # It's a list
            list_content = arg[1:-1].strip()
            if list_content:
                items = [item.strip() for item in list_content.split()]
                result.extend(items)
        else:
            try:
                val = interpreter.evaluate_expression(arg)
                result.append(str(val))
            except (ValueError, TypeError):
                result.append(arg.strip('"'))
    return str(result)


def _logo_first(interpreter: "Interpreter", args: List[str]) -> str:
    """FIRST list/word - Get first item or character"""
    if not args:
        return "❌ FIRST requires argument\n"

    consumed = _consume_logo_args(args, 1)
    arg = consumed[0]

    # Check if arg is a variable reference :VAR
    if arg.startswith(":"):
        var_name = arg[1:].upper()
        if var_name in interpreter.logo_lists:
            items = interpreter.logo_lists[var_name]
            return items[0] if items else ""
        if var_name in interpreter.string_variables:
            s = interpreter.string_variables[var_name]
            return s[0] if s else ""
        if var_name in interpreter.variables:
            val = interpreter.variables[var_name]
            return str(val)[0] if str(val) else ""

    if arg.startswith("[") and arg.endswith("]"):
        # List
        list_content = arg[1:-1].strip()
        if list_content:
            items = [item.strip() for item in list_content.split()]
            return items[0] if items else ""
        return ""
    # Word or variable
    try:
        val = interpreter.evaluate_expression(arg)
        return str(val)
    except (ValueError, TypeError):
        word = arg.strip('"')
        return word[0] if word else ""


def _logo_last(interpreter: "Interpreter", args: List[str]) -> str:
    """LAST list/word - Get last item or character"""
    if not args:
        return "❌ LAST requires argument\n"

    consumed = _consume_logo_args(args, 1)
    arg = consumed[0]

    # Check if arg is a variable reference :VAR
    if arg.startswith(":"):
        var_name = arg[1:].upper()
        if var_name in interpreter.logo_lists:
            items = interpreter.logo_lists[var_name]
            return items[-1] if items else ""
        if var_name in interpreter.string_variables:
            s = interpreter.string_variables[var_name]
            return s[-1] if s else ""
        if var_name in interpreter.variables:
            val = interpreter.variables[var_name]
            return str(val)[-1] if str(val) else ""

    if arg.startswith("[") and arg.endswith("]"):
        # List
        list_content = arg[1:-1].strip()
        if list_content:
            items = [item.strip() for item in list_content.split()]
            return items[-1] if items else ""
        return ""
    # Word or variable
    try:
        val = interpreter.evaluate_expression(arg)
        s = str(val)
        return s[-1] if s else ""
    except (ValueError, TypeError):
        word = arg.strip('"')
        return word[-1] if word else ""


def _logo_butfirst(interpreter: "Interpreter", args: List[str]) -> str:
    """BUTFIRST list/word - Remove first item or character"""
    if not args:
        return "❌ BUTFIRST requires argument\n"

    consumed = _consume_logo_args(args, 1)
    arg = consumed[0]

    # Check if arg is a variable reference :VAR
    if arg.startswith(":"):
        var_name = arg[1:].upper()
        if var_name in interpreter.logo_lists:
            items = interpreter.logo_lists[var_name]
            return str(items[1:]) if len(items) > 1 else "[]"
        if var_name in interpreter.string_variables:
            s = interpreter.string_variables[var_name]
            return s[1:] if len(s) > 1 else ""
        if var_name in interpreter.variables:
            val = interpreter.variables[var_name]
            s = str(val)
            return s[1:] if len(s) > 1 else ""

    if arg.startswith("[") and arg.endswith("]"):
        # List
        list_content = arg[1:-1].strip()
        if list_content:
            items = [item.strip() for item in list_content.split()]
            return str(items[1:]) if len(items) > 1 else "[]"
        return "[]"
    # Word or variable
    try:
        val = interpreter.evaluate_expression(arg)
        s = str(val)
        return s[1:] if len(s) > 1 else ""
    except (ValueError, TypeError):
        word = arg.strip('"')
        return word[1:] if len(word) > 1 else ""


def _logo_butlast(interpreter: "Interpreter", args: List[str]) -> str:
    """BUTLAST list/word - Remove last item or character"""
    if not args:
        return "❌ BUTLAST requires argument\n"

    consumed = _consume_logo_args(args, 1)
    arg = consumed[0]

    # Check if arg is a variable reference :VAR
    if arg.startswith(":"):
        var_name = arg[1:].upper()
        if var_name in interpreter.logo_lists:
            items = interpreter.logo_lists[var_name]
            return str(items[:-1]) if len(items) > 1 else "[]"
        if var_name in interpreter.string_variables:
            s = interpreter.string_variables[var_name]
            return s[:-1] if len(s) > 1 else ""
        if var_name in interpreter.variables:
            val = interpreter.variables[var_name]
            s = str(val)
            return s[:-1] if len(s) > 1 else ""

    if arg.startswith("[") and arg.endswith("]"):
        # List
        list_content = arg[1:-1].strip()
        if list_content:
            items = [item.strip() for item in list_content.split()]
            return str(items[:-1]) if len(items) > 1 else "[]"
        return "[]"
    # Word or variable
    try:
        val = interpreter.evaluate_expression(arg)
        s = str(val)
        return s[:-1] if len(s) > 1 else ""
    except (ValueError, TypeError):
        word = arg.strip('"')
        return word[:-1] if len(word) > 1 else ""


def _logo_item(interpreter: "Interpreter", args: List[str]) -> str:
    """ITEM index thing - Get item at index from list/word"""
    if len(args) < 2:
        return "❌ ITEM requires index and thing\n"

    consumed = _consume_logo_args(args, 2)
    if len(consumed) < 2:
        return "❌ ITEM requires index and thing\n"

    try:
        index = (
            int(interpreter.evaluate_expression(consumed[0])) - 1
        )  # 1-indexed
    except (ValueError, TypeError):
        return "❌ Invalid index\n"

    thing = consumed[1]

    # Check if thing is a variable reference :VAR
    if thing.startswith(":"):
        var_name = thing[1:].upper()
        if var_name in interpreter.logo_lists:
            items = interpreter.logo_lists[var_name]
            if 0 <= index < len(items):
                return items[index]
            return ""
        if var_name in interpreter.string_variables:
            s = interpreter.string_variables[var_name]
            if 0 <= index < len(s):
                return s[index]
            return ""
        if var_name in interpreter.variables:
            val = interpreter.variables[var_name]
            s = str(val)
            if 0 <= index < len(s):
                return s[index]
            return ""

    if thing.startswith("[") and thing.endswith("]"):
        # List
        list_content = thing[1:-1].strip()
        if list_content:
            items = [item.strip() for item in list_content.split()]
            if 0 <= index < len(items):
                return items[index]
        return ""
    # Word
    try:
        val = interpreter.evaluate_expression(thing)
        s = str(val)
        if 0 <= index < len(s):
            return s[index]
    except (ValueError, TypeError):
        word = thing.strip('"')
        if 0 <= index < len(word):
            return word[index]
    return ""


def _logo_count(interpreter: "Interpreter", args: List[str]) -> str:
    """COUNT thing - Get length of list or word"""
    if not args:
        return "❌ COUNT requires argument\n"
    arg = args[0]

    if arg.startswith("[") and arg.endswith("]"):
        # List
        list_content = arg[1:-1].strip()
        if list_content:
            items = [item.strip() for item in list_content.split()]
            return str(len(items))
        return "0"
    # Word or variable
    try:
        val = interpreter.evaluate_expression(arg)
        return str(len(str(val)))
    except (ValueError, TypeError):
        word = arg.strip('"')
        return str(len(word))


# Arithmetic operations
def _logo_sum(interpreter: "Interpreter", args: List[str]) -> str:
    """SUM num1 num2 - Add numbers"""
    if len(args) < 2:
        return "❌ SUM requires two numbers\n"
    try:
        num1 = interpreter.evaluate_expression(args[0])
        num2 = interpreter.evaluate_expression(args[1])
        return str(num1 + num2)
    except (ValueError, TypeError):
        return "❌ Invalid numbers for SUM\n"


def _logo_difference(interpreter: "Interpreter", args: List[str]) -> str:
    """DIFFERENCE num1 num2 - Subtract numbers"""
    if len(args) < 2:
        return "❌ DIFFERENCE requires two numbers\n"
    try:
        num1 = interpreter.evaluate_expression(args[0])
        num2 = interpreter.evaluate_expression(args[1])
        return str(num1 - num2)
    except (ValueError, TypeError):
        return "❌ Invalid numbers for DIFFERENCE\n"


def _logo_product(interpreter: "Interpreter", args: List[str]) -> str:
    """PRODUCT num1 num2 - Multiply numbers"""
    if len(args) < 2:
        return "❌ PRODUCT requires two numbers\n"
    try:
        num1 = interpreter.evaluate_expression(args[0])
        num2 = interpreter.evaluate_expression(args[1])
        return str(num1 * num2)
    except (ValueError, TypeError):
        return "❌ Invalid numbers for PRODUCT\n"


def _logo_quotient(interpreter: "Interpreter", args: List[str]) -> str:
    """QUOTIENT num1 num2 - Divide numbers"""
    if len(args) < 2:
        return "❌ QUOTIENT requires two numbers\n"
    try:
        num1 = interpreter.evaluate_expression(args[0])
        num2 = interpreter.evaluate_expression(args[1])
        if num2 == 0:
            return "❌ Division by zero\n"
        return str(num1 / num2)
    except (ValueError, TypeError):
        return "❌ Invalid numbers for QUOTIENT\n"


def _logo_random(interpreter: "Interpreter", args: List[str]) -> str:
    """RANDOM limit - Generate random number"""

    if not args:
        return "❌ RANDOM requires limit\n"
    try:
        limit = int(interpreter.evaluate_expression(args[0]))
        return str(random.randint(0, limit - 1))
    except (ValueError, TypeError):
        return "❌ Invalid limit for RANDOM\n"


# Control structures
def _logo_ifelse(
    interpreter: "Interpreter", turtle: Optional["TurtleState"], command: str
) -> str:
    """IFELSE condition [true_commands] [false_commands]"""
    # Find condition
    # It's between IFELSE and first [
    start_bracket1 = command.find("[")
    if start_bracket1 == -1:
        return "❌ IFELSE requires [true] [false] blocks\n"

    header = command[:start_bracket1].strip()
    header_words = header.split()
    if len(header_words) < 2:
        return "❌ IFELSE requires condition\n"

    condition_str = header_words[1]

    # Find first block end
    # We need to scan from start_bracket1 to find matching ]
    bracket_depth = 0
    end_bracket1 = -1
    for i in range(start_bracket1, len(command)):
        char = command[i]
        if char == "[":
            bracket_depth += 1
        elif char == "]":
            bracket_depth -= 1
            if bracket_depth == 0:
                end_bracket1 = i
                break

    if end_bracket1 == -1:
        return "❌ IFELSE first block malformed\n"

    true_block = command[start_bracket1 + 1 : end_bracket1].strip()

    # Find second block
    remaining = command[end_bracket1 + 1 :]
    start_bracket2 = remaining.find("[")
    if start_bracket2 == -1:
        return "❌ IFELSE requires second [false] block\n"

    # Adjust index to be relative to command
    start_bracket2 += end_bracket1 + 1

    bracket_depth = 0
    end_bracket2 = -1
    for i in range(start_bracket2, len(command)):
        char = command[i]
        if char == "[":
            bracket_depth += 1
        elif char == "]":
            bracket_depth -= 1
            if bracket_depth == 0:
                end_bracket2 = i
                break

    if end_bracket2 == -1:
        return "❌ IFELSE second block malformed\n"

    false_block = command[start_bracket2 + 1 : end_bracket2].strip()

    try:
        condition = _logo_eval_expr_str(interpreter, condition_str)
    except (ValueError, TypeError, ZeroDivisionError):
        return f"❌ Invalid IFELSE condition: {condition_str}\n"

    if condition != 0:
        return execute_logo(interpreter, true_block, turtle)
    return execute_logo(interpreter, false_block, turtle)


def _logo_forever(
    interpreter: "Interpreter", turtle: Optional["TurtleState"], command: str
) -> str:
    """FOREVER [commands] - Repeat commands forever"""
    parts = command.upper().split("[", 1)
    if len(parts) < 2:
        return "❌ FOREVER requires [commands]\n"

    body = "[" + parts[1]
    if "[" not in body or "]" not in body:
        return "❌ FOREVER requires [commands]\n"

    start = body.find("[") + 1
    end = body.rfind("]")
    if start >= end:
        return "❌ FOREVER commands malformed\n"

    commands_text = body[start:end].strip()
    # For now, just execute once to avoid infinite loops
    return execute_logo(interpreter, commands_text, turtle)


def _logo_repcount(_interpreter: "Interpreter") -> str:
    """REPCOUNT - Get current repeat iteration (simplified)"""
    return "1"


# Graphics enhancements
def _logo_arc(
    interpreter: "Interpreter",
    turtle: Optional["TurtleState"],
    args: List[str],
) -> str:
    """ARC angle radius - Draw an arc"""
    if len(args) < 2:
        return "❌ ARC requires angle and radius\n"
    try:
        angle = interpreter.evaluate_expression(args[0])
        radius = interpreter.evaluate_expression(args[1])
        if turtle is None:
            return "❌ Graphics not available for this command\n"
        turtle.circle(radius, angle)
        return ""
    except (ValueError, TypeError):
        return "❌ Invalid parameters for ARC\n"


def _logo_filled(
    interpreter: "Interpreter", turtle: Optional["TurtleState"], command: str
) -> str:
    """FILLED color [commands] - Fill area with color"""
    parts = command.upper().split("[", 1)
    if len(parts) < 2:
        return "❌ FILLED requires color and [commands]\n"

    body = "[" + parts[1]

    # For now, just execute the commands without filling
    if "[" not in body or "]" not in body:
        return "❌ FILLED requires [commands]\n"

    start = body.find("[") + 1
    end = body.rfind("]")
    if start >= end:
        return "❌ FILLED commands malformed\n"

    commands_text = body[start:end].strip()
    return execute_logo(interpreter, commands_text, turtle)


def _logo_label(interpreter: "Interpreter", args: List[str]) -> str:
    """LABEL text - Draw text at turtle position"""
    if not args:
        return "❌ LABEL requires text\n"
    text = " ".join(args).strip('"')
    # For now, just print to output
    interpreter.output.append(text)
    return text + "\n"
