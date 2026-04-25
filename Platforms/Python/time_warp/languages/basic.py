# pylint: disable=too-many-lines
"""
BASIC language executor for Time Warp Studio.
Handles BASIC-specific commands and syntax.
"""

import math
import re
from typing import TYPE_CHECKING, Any, List, Optional

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState

# Import string evaluator for string functions
from ..utils.string_evaluator import StringExpressionEvaluator


def _basic_eval_expr(interpreter: "Interpreter", expr: str) -> float:
    """Pre-process UDT field references, then delegate to evaluate_expression."""

    # Replace VAR.FIELD references in expression with their numeric values
    def _field_sub(m):
        obj_name = m.group(1).upper()
        field_name = m.group(2).upper()
        obj = interpreter.variables.get(obj_name)
        if isinstance(obj, dict):
            val = obj.get(field_name, 0)
            try:
                v = float(val)
                return str(int(v)) if v == int(v) else str(v)
            except (TypeError, ValueError):
                return str(val)
        return m.group(0)

    expanded = re.sub(
        r"\b([A-Za-z_][A-Za-z0-9_]*)\s*\.\s*([A-Za-z_][A-Za-z0-9_]*)\b",
        _field_sub,
        expr,
    )
    return interpreter.evaluate_expression(expanded)


# Runtime imports moved inside functions to avoid circular imports

# NOTE: parser_patterns.py provides pre-compiled regex patterns that could
# replace the inline patterns below. Currently unused pending integration.

# Compiled regex patterns for performance (optimized for BASIC parsing)
_PRINT_PATTERN = re.compile(r'"([^"]*)"[;,]?\s*(.+)')
_FOR_PATTERN = re.compile(
    r"(\w+)\s*=\s*(.+?)\s+TO\s+(.+?)(?:\s+STEP\s+(.+))?$",
    re.IGNORECASE,
)
_LINE_PATTERN = re.compile(r"\(\s*([^,]+)\s*,\s*([^,]+)\s*\)\s*,\s*(.+)")
_VAR_PATTERN = re.compile(r"^[A-Za-z_][A-Za-z0-9_$]*$")


def _has_string_concat(expr: str) -> bool:
    """Detect if expression has '+' concatenation between string parts.

    Returns True for patterns like:
      "A" + "B"
      "Line " + STR$(I)
      NAME$ + " is cool"
    Returns False for purely numeric expressions like "3 + 4".
    """
    in_str = False
    str_char = ""
    depth = 0
    has_str_part = False
    has_plus_outside = False
    for ch in expr:
        if in_str:
            if ch == str_char:
                in_str = False
        elif ch in "\"'":
            in_str = True
            str_char = ch
            has_str_part = True
        elif ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
        elif ch == "+" and depth == 0:
            has_plus_outside = True
    # Also detect $ in the expression (string variables/functions)
    if "$" in expr:
        has_str_part = True
    return has_str_part and has_plus_outside


def _try_string_comparison(interpreter: "Interpreter", condition: str) -> "bool | None":
    """Try to evaluate a string comparison condition.

    Handles patterns like:
        NAME$ = ""
        A$ <> "hello"
        X$ = Y$
        "hello" = NAME$

    Returns True/False if this is a string comparison, or None if not.
    """
    # Check if this looks like a string comparison (has $ variable or "" literal)
    has_string_indicator = "$" in condition or '""' in condition

    if not has_string_indicator:
        return None

    # Try each comparison operator (check <> and >= and <= before = < >)
    for op_str, op_func in [
        ("<>", lambda a, b: a != b),
        (">=", lambda a, b: a >= b),
        ("<=", lambda a, b: a <= b),
        ("=", lambda a, b: a == b),
        ("<", lambda a, b: a < b),
        (">", lambda a, b: a > b),
    ]:
        if op_str in condition:
            parts = condition.split(op_str, 1)
            if len(parts) == 2:
                lhs = _resolve_string_value(interpreter, parts[0].strip())
                rhs = _resolve_string_value(interpreter, parts[1].strip())
                if lhs is not None and rhs is not None:
                    return op_func(lhs, rhs)

    return None


def _resolve_string_value(interpreter: "Interpreter", expr: str) -> "str | None":
    """Resolve a string expression to its value for comparison.

    Returns the string value, or None if not resolvable as a string.
    """
    # String literal: "hello" or ""
    if expr.startswith('"') and expr.endswith('"'):
        return expr[1:-1]

    expr_upper = expr.upper()

    # String variable: NAME$, A$, etc.
    if expr_upper.endswith("$"):
        return interpreter.string_variables.get(expr_upper, "")

    # String function call: LEFT$(...), MID$(...), etc.
    if "$(" in expr_upper:
        try:
            evaluator = StringExpressionEvaluator(
                interpreter.string_variables,
                interpreter.variables,
            )
            return str(evaluator.evaluate(expr))
        except (ValueError, TypeError, AttributeError, KeyError):
            return None

    return None


def _strip_comment(args: str) -> str:
    """Strip trailing comments from arguments, respecting quoted strings.

    Args:
        args: Argument string (may contain REM comments)

    Returns:
        Arguments with comments removed

    Example:
        >>> _strip_comment('"HELLO : REM not a comment"')
        '"HELLO : REM not a comment"'
        >>> _strip_comment('PRINT X : REM this is a comment')
        'PRINT X'
    """
    in_quotes = False
    i = 0
    while i < len(args):
        ch = args[i]

        # Toggle quote state
        if ch == '"':
            in_quotes = not in_quotes
            i += 1
            continue

        # Check for REM comment (only outside quotes)
        if not in_quotes and args[i:].upper().startswith(": REM"):
            return args[:i].strip()
        if not in_quotes and args[i:].upper().startswith(":REM"):
            return args[:i].strip()

        i += 1

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


def _basic_def_fn(interpreter: "Interpreter", command: str) -> str:
    """Handle DEF FNA(X) = X * X + 1 style user-defined functions."""
    # Parse: DEF FNname(param) = expression
    m = re.match(
        r"^DEF\s+FN(\w+)\s*\(([^)]*)\)\s*=\s*(.+)$",
        command.strip(),
        re.IGNORECASE,
    )
    if not m:
        return "❌ Invalid DEF FN syntax (expected DEF FNA(X) = expr)\n"
    fn_name = "FN" + m.group(1).upper()  # e.g., "FNA"
    params = [p.strip().upper() for p in m.group(2).split(",") if p.strip()]
    body_expr = m.group(3).strip()
    # Store on interpreter for later use by evaluate_expression
    if not hasattr(interpreter, "basic_user_fns"):
        interpreter.basic_user_fns = {}
    interpreter.basic_user_fns[fn_name] = (params, body_expr)
    return ""


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

    # Ignore label-only lines (commands ending in colon not followed by commands)
    if command.endswith(":") and " " not in command:
        return ""

    # Handle multi-statement lines
    if (
        ":" in command
        and not command.upper().startswith("REM")
        and not command.upper().startswith("'")
        and not command.upper().startswith("IF ")
    ):
        statements = _split_statements(command)
        if len(statements) > 1:
            outputs = []
            for stmt in statements:
                outputs.append(execute_basic(interpreter, stmt, turtle))
            return "".join(outputs)

    cmd = command.upper()

    # ── Block IF skip-mode ────────────────────────────────────────────────
    # When a block IF condition was false we skip lines until ELSE / END IF.
    if not hasattr(interpreter, "basic_if_stack"):
        interpreter.basic_if_stack = []  # list of dicts

    if interpreter.basic_if_stack:
        top = interpreter.basic_if_stack[-1]
        # Skipping mode — only react to structural keywords at the correct depth
        if top.get("skip"):
            # Track nested IFs so we don't pop early
            if cmd.startswith("IF ") and (
                cmd.rstrip().endswith("THEN") or cmd.rstrip().endswith("THEN ")
            ):
                top["depth"] = top.get("depth", 0) + 1
                return ""
            depth = top.get("depth", 0)
            if cmd in ("ENDIF", "END IF"):
                if depth > 0:
                    top["depth"] = depth - 1
                    return ""
                interpreter.basic_if_stack.pop()
                return ""
            if cmd == "ELSE" and depth == 0:
                # Found matching ELSE — stop skipping (run ELSE branch)
                top["skip"] = False
                return ""
            return ""  # skip this line
        # We are in a block IF executing (not skipping) — handle END IF / ELSE
        if cmd in ("ENDIF", "END IF"):
            interpreter.basic_if_stack.pop()
            return ""
        if cmd == "ELSE":
            # We already executed the THEN branch; skip to END IF
            top["skip"] = True
            return ""
    elif cmd in ("ENDIF", "END IF", "ELSE"):
        return ""  # stray structural keyword; ignore

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
    # TYPE typename / END TYPE — user defined type (VB-classic)
    if cmd.startswith("TYPE ") and not cmd.startswith("TYPE$"):
        return _basic_type_def(interpreter, _strip_comment(command[5:]))
    if cmd in ("END TYPE", "ENDTYPE"):
        return ""  # handled inside _basic_type_def by scanning
    if (
        "=" in cmd
        and not cmd.startswith("IF ")
        and not cmd.startswith("FOR ")
        and not cmd.startswith("DEF FN")
    ):
        return _basic_let(interpreter, _strip_comment(command))
    if cmd.startswith("COLOR "):
        return _basic_color(interpreter, _strip_comment(command[6:]), turtle)
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
        return _basic_screen(interpreter, _strip_comment(command[7:]), turtle)
    if cmd.startswith("LOCATE "):
        return _basic_locate(interpreter, _strip_comment(command[7:]))
    if cmd.startswith("LINE "):
        return _basic_line(interpreter, _strip_comment(command[5:]), turtle)
    if cmd.startswith("CIRCLE "):
        return _basic_circle(interpreter, _strip_comment(command[7:]), turtle)
    if cmd.startswith("PSET "):
        return _basic_pset(interpreter, _strip_comment(command[5:]), turtle)
    if cmd.startswith("PRESET "):
        return _basic_preset(interpreter, _strip_comment(command[7:]), turtle)
    if cmd.startswith("PAINT "):
        return _basic_paint(interpreter, _strip_comment(command[6:]), turtle)
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
    # DEF FN user-defined functions: DEF FNA(X) = X * X + 1
    if cmd.startswith("DEF FN") or cmd.startswith("DEF  FN"):
        return _basic_def_fn(interpreter, _strip_comment(command))
    # Game support commands
    if cmd == "BEEP":
        return _basic_beep(interpreter)
    if cmd.startswith("SOUND "):
        return _basic_sound(interpreter, _strip_comment(command[6:]))
    if cmd.startswith("SPEED "):
        return _basic_speed(interpreter, _strip_comment(command[6:]), turtle)
    if cmd.startswith("SPRITE "):
        return _basic_sprite(interpreter, _strip_comment(command[7:]))
    if cmd.startswith("ON TIMER"):
        return _basic_on_timer(interpreter, _strip_comment(command[9:]))
    if cmd == "TIMER ON":
        return _basic_timer_on(interpreter)
    if cmd == "TIMER OFF":
        return _basic_timer_off(interpreter)
    # Music and speech commands
    if cmd.startswith("PLAY "):
        return _basic_play(interpreter, _strip_comment(command[5:]))
    if cmd.startswith("SAY "):
        return _basic_say(interpreter, _strip_comment(command[4:]))
    # Shape commands
    if cmd.startswith("SHAPE "):
        return _basic_shape(interpreter, _strip_comment(command[6:]), turtle)
    # Particle commands
    if cmd.startswith("PARTICLE "):
        return _basic_particle(interpreter, _strip_comment(command[9:]))
    # Fractal commands
    if cmd == "FRACTAL" or cmd.startswith("FRACTAL "):
        args = _strip_comment(command[7:]).strip() if len(command) > 7 else ""
        return _basic_fractal(interpreter, args, turtle)
    # Gamepad commands
    if cmd.startswith("JOYINIT"):
        return _basic_joyinit(interpreter)
    # Memory/Port commands (Phase 5 integration)
    if cmd.startswith("POKE "):
        return _basic_poke(interpreter, _strip_comment(command[5:]))
    if cmd.startswith("PEEK "):
        return _basic_peek(interpreter, _strip_comment(command[5:]))
    if cmd.startswith("OUT "):
        return _basic_out(interpreter, _strip_comment(command[4:]))
    if cmd.startswith("IN "):
        return _basic_in(interpreter, _strip_comment(command[3:]))
    if cmd.startswith("SHELL "):
        return _basic_shell(interpreter, _strip_comment(command[6:]))
    # ── Error handling ─────────────────────────────────────────────────
    # ON ERROR GOTO <line>  — set error handler line
    if cmd.startswith("ON ERROR GOTO"):
        return _basic_on_error_goto(interpreter, _strip_comment(command[13:]))
    if cmd == "ON ERROR":
        # synonym for ON ERROR GOTO 0 — disable handler
        interpreter.basic_error_handler_line = 0
        return ""
    if cmd.startswith("RESUME NEXT"):
        return _basic_resume_next(interpreter)
    if cmd.startswith("RESUME"):
        return _basic_resume(interpreter, _strip_comment(command[6:]))
    # RANDOMIZE [TIMER] — seed the random number generator
    if cmd.startswith("RANDOMIZE"):
        import random as _rng

        _rng.seed()
        return ""
    # --- Full Logo command and block blending ---
    _execute_logo: Optional[Any] = None
    try:
        from .logo import LOGO_COMMANDS, execute_logo as _imported_logo

        _execute_logo = _imported_logo
    except ImportError:
        LOGO_COMMANDS = set()

    # Helper: detect if a line or block is likely Logo
    def is_logo_line(line: str) -> bool:
        if not line.strip():
            return False
        first = line.strip().split()[0].upper()
        # Allow MAKE, REPEAT, TO, and any LOGO_COMMANDS
        if first in LOGO_COMMANDS or first in {"MAKE", "REPEAT", "TO", "END", "IFELSE", "IF", "FOREACH", "MAP", "FILTER", "REDUCE", "WHILE", "UNTIL"}:
            return True
        # Allow Logo variable assignment (MAKE, :, ")
        if line.strip().startswith("MAKE ") or line.strip().startswith(":") or line.strip().startswith('"'):
            return True
        # Allow bracketed blocks
        if "[" in line or "]" in line:
            return True
        # Allow Logo comments (;) and variable usage
        if line.strip().startswith(";"):
            return True
        return False

    # If the line is likely Logo, or contains Logo block syntax, forward to Logo executor
    if is_logo_line(command) and _execute_logo:
        return _execute_logo(interpreter, command, turtle)

    # If the line is not recognized as BASIC, but is not empty, try to accumulate a Logo block
    if _execute_logo and command.strip():
        # Try to detect start of a Logo block (e.g., REPEAT ... [ ... ])
        if any(kw in command.upper() for kw in ["REPEAT", "TO", "FOREACH", "MAP", "FILTER", "REDUCE", "WHILE", "UNTIL"]):
            # Accumulate lines until block end (matching brackets)
            block_lines = [command]
            open_brackets = command.count("[") - command.count("]")
            interpreter.logo_block_buffer = getattr(interpreter, "logo_block_buffer", [])
            interpreter.logo_block_buffer.extend(block_lines)
            if open_brackets > 0:
                # Wait for more lines in the block
                return ""
            else:
                # Block is complete, send to Logo
                block = "\n".join(interpreter.logo_block_buffer)
                interpreter.logo_block_buffer = []
                return _execute_logo(interpreter, block, turtle)
        # If already accumulating a Logo block
        if hasattr(interpreter, "logo_block_buffer") and interpreter.logo_block_buffer:
            interpreter.logo_block_buffer.append(command)
            open_brackets = sum(line.count("[") - line.count("]") for line in interpreter.logo_block_buffer)
            if open_brackets > 0:
                return ""
            else:
                block = "\n".join(interpreter.logo_block_buffer)
                interpreter.logo_block_buffer = []
                return _execute_logo(interpreter, block, turtle)

    return f"❌ Unknown BASIC command: {command}\n"


# Keep local complexity for the BASIC print formatter — it's intentionally
# complex for many formatting cases. Disable too-many-locals here.
# pylint: disable=R0914
def _basic_print(interpreter: "Interpreter", args: str) -> str:
    if not args.strip():
        return "\n"

    suppress_newline = args.strip().endswith(";") or args.strip().endswith(",")

    def _format_numeric(val: float, preferred: str | None = None) -> str:
        # Preferred types 'int'/'long' print without decimals
        if preferred in ("int", "long"):
            return str(int(val))
        # For floats, use general format to avoid trailing .0/.000
        return f"{val:g}"

    # Split parts respecting parentheses and quotes
    parts: List[str] = []
    current = ""
    in_quotes = False
    paren_depth = 0
    for ch in args:
        if ch == '"':
            in_quotes = not in_quotes
            current += ch
        elif not in_quotes:
            if ch == "(":
                paren_depth += 1
                current += ch
            elif ch == ")":
                paren_depth -= 1
                current += ch
            elif ch in (";", ",") and paren_depth == 0:
                if current.strip():
                    parts.append(current.strip())
                current = ""
            else:
                current += ch
        else:
            current += ch
    if current.strip():
        parts.append(current.strip())
    if not parts:
        return "\n"

    out_items: List[str] = []
    string_eval = StringExpressionEvaluator(
        string_variables=interpreter.string_variables,
        numeric_variables=interpreter.variables,
        string_arrays=getattr(interpreter, "string_arrays", {}),
        numeric_arrays=interpreter.arrays,
    )

    for item in parts:
        item_trim = item.strip()
        item_upper = item_trim.upper()

        # ── Detect string expressions that need the string evaluator ──
        # String concat: "A" + "B", VAR$ + "text", or expressions with STR$/CHR$/etc.
        _is_string_expr = False
        if _has_string_concat(item_trim):
            _is_string_expr = True
        elif re.search(
            r"\b(STR|CHR|MID|LEFT|RIGHT|INSTR|UPPER|LOWER|TRIM|LTRIM|RTRIM|SPACE|STRING)\$?\s*\(",
            item_upper,
        ):
            _is_string_expr = True

        if _is_string_expr:
            try:
                result = string_eval.evaluate(item_trim)
                out_items.append(str(result))
            except (ValueError, TypeError):
                try:
                    value = interpreter.evaluate_expression(item_trim)
                    out_items.append(_format_numeric(value))
                except (ValueError, TypeError, ZeroDivisionError):
                    out_items.append(interpreter.interpolate_text(item_trim))
            continue

        # Handle simple string literals (no concat)
        if (
            item_trim.startswith('"')
            and item_trim.endswith('"')
            and len(item_trim) >= 2
            and '"' not in item_trim[1:-1]
        ):
            out_items.append(item_trim[1:-1])
        # Handle INKEY$ - get key from buffer
        elif item_upper == "INKEY$":
            out_items.append(interpreter.get_inkey())
        # Handle TIME$ - current time as HH:MM:SS
        elif item_upper == "TIME$":
            # pylint: disable=import-outside-toplevel
            from ..features.game_support import get_game_state

            out_items.append(get_game_state().get_time_string())
        # Handle DATE$ - current date as MM-DD-YYYY
        elif item_upper == "DATE$":
            # pylint: disable=import-outside-toplevel
            from ..features.game_support import get_game_state

            out_items.append(get_game_state().get_date_string())
        # Handle TIMER - seconds since midnight
        elif item_upper == "TIMER":
            # pylint: disable=import-outside-toplevel
            from ..features.game_support import get_game_state

            out_items.append(str(int(get_game_state().get_timer_value())))
        # Handle string variables (end with $) or string array elements
        elif item_upper.endswith("$") or (
            item_upper.endswith(")") and "$(" in item_upper
        ):
            # Check for string array element: NAMES$(I)
            if "(" in item_trim and item_trim.endswith(")"):
                arr_name = item_trim.split("(", 1)[0].upper()
                idx_expr = item_trim.split("(", 1)[1][:-1]
                if arr_name.endswith("$") and arr_name in interpreter.string_arrays:
                    try:
                        idx = int(interpreter.evaluate_expression(idx_expr))
                        arr = interpreter.string_arrays[arr_name]
                        if 0 <= idx < len(arr):
                            out_items.append(arr[idx])
                        else:
                            out_items.append("")
                    except (ValueError, TypeError):
                        out_items.append("")
                else:
                    out_items.append("")
            elif item_upper in interpreter.string_variables:
                out_items.append(interpreter.string_variables[item_upper])
            else:
                out_items.append("")  # Undefined string variable is empty
        # Handle string functions (LEN, LEFT, RIGHT, MID, INSTR, UPPER, LOWER,
        # TRIM, STR, VAL)
        elif any(
            item_upper.startswith(func)
            for func in [
                "LEN(",
                "LEFT(",
                "RIGHT(",
                "MID(",
                "INSTR(",
                "UPPER(",
                "LOWER(",
                "TRIM(",
                "LTRIM(",
                "RTRIM(",
                "SPACE(",
                "SPACE$(",
                "STR(",
                "VAL(",
            ]
        ):
            try:
                result = string_eval.evaluate(item_trim)
                out_items.append(str(result))
            except (ValueError, TypeError):
                # If string eval fails, try numeric eval
                try:
                    value = interpreter.evaluate_expression(item_trim)
                    out_items.append(_format_numeric(value))
                except (ValueError, TypeError, ZeroDivisionError):
                    out_items.append(interpreter.interpolate_text(item_trim))
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
                num_val = interpreter.variables[item_upper]
                out_items.append(_format_numeric(float(num_val), t))
            else:
                # Try to evaluate as expression
                try:
                    value = interpreter.evaluate_expression(item_trim)
                    out_items.append(_format_numeric(value))
                except (ValueError, TypeError, ZeroDivisionError):
                    # If evaluation fails, use interpolation as fallback
                    out_items.append(interpreter.interpolate_text(item_trim))
    result = " ".join(out_items)
    return result if suppress_newline else result + "\n"


def _basic_let(interpreter: "Interpreter", args: str) -> str:
    """LET variable = expression - Assign variable value.

    Args:
        interpreter: Interpreter instance
        args: Variable assignment statement

    Returns:
        Error message if parsing/evaluation fails, empty string on success
    """
    from ..logging_config import get_logger
    from ..utils.validators import ValidationError, validate_variable_name

    logger = get_logger(__name__)

    if "=" not in args:
        return "❌ LET requires format: variable = expression\n"

    parts = args.split("=", 1)
    var_name = parts[0].strip().upper()
    expr = parts[1].strip()
    expr_upper = expr.upper()

    if not var_name:
        return "❌ LET requires variable name\n"

    # Check for array assignment: VAR(INDEX) = EXPR  or  VAR$(INDEX) = EXPR
    if "(" in var_name and var_name.endswith(")"):
        array_name = var_name.split("(", 1)[0]
        index_expr = var_name.split("(", 1)[1][:-1]
        try:
            idx = int(interpreter.evaluate_expression(index_expr))
            # String array?
            if array_name.endswith("$") and array_name in interpreter.string_arrays:
                string_eval = StringExpressionEvaluator(
                    string_variables=interpreter.string_variables,
                    numeric_variables=interpreter.variables,
                    string_arrays=getattr(interpreter, "string_arrays", {}),
                    numeric_arrays=interpreter.arrays,
                )
                try:
                    str_val = string_eval.evaluate(expr)
                except (ValueError, TypeError):
                    str_val = str(expr)
                arr = interpreter.string_arrays[array_name]
                if 0 <= idx < len(arr):
                    arr[idx] = str_val
                    return ""
                return f"❌ Array index out of bounds: {idx}\n"
            # Numeric array
            val = interpreter.evaluate_expression(expr)
            if array_name in interpreter.arrays:
                num_arr = interpreter.arrays[array_name]
                if 0 <= idx < len(num_arr):
                    num_arr[idx] = val
                    return ""
                return f"❌ Array index out of bounds: {idx}\n"
            return f"❌ Array not defined: {array_name}\n"
        except (ValueError, TypeError) as e:
            return f"❌ Invalid array index or value: {e}\n"

    # Check for field assignment: VAR.FIELD = EXPR
    if "." in var_name:
        dot_idx = var_name.index(".")
        obj_name = var_name[:dot_idx]
        field_name = var_name[dot_idx + 1 :]
        obj = interpreter.variables.get(obj_name)
        if isinstance(obj, dict):
            if field_name.endswith("$"):
                obj[field_name] = str(expr).strip('"').strip("'")
            else:
                try:
                    obj[field_name] = interpreter.evaluate_expression(expr)
                except (ValueError, TypeError):
                    obj[field_name] = 0
            return ""
        return f"❌ {obj_name} is not a user-defined type instance\n"

    try:
        validate_variable_name(var_name, allow_suffix=True)

        # String assignment
        if var_name.endswith("$"):
            if expr.startswith('"') and expr.endswith('"'):
                interpreter.set_typed_variable(var_name, expr[1:-1])
            # Handle special string functions
            elif expr_upper == "INKEY$":
                interpreter.set_typed_variable(var_name, interpreter.get_inkey())
            elif expr_upper == "TIME$":
                # pylint: disable=import-outside-toplevel
                from ..features.game_support import get_game_state

                interpreter.set_typed_variable(
                    var_name, get_game_state().get_time_string()
                )
            elif expr_upper == "DATE$":
                # pylint: disable=import-outside-toplevel
                from ..features.game_support import get_game_state

                interpreter.set_typed_variable(
                    var_name, get_game_state().get_date_string()
                )
            else:
                # Try to evaluate as string expression
                string_eval = StringExpressionEvaluator(
                    string_variables=interpreter.string_variables,
                    numeric_variables=interpreter.variables,
                    string_arrays=getattr(interpreter, "string_arrays", {}),
                    numeric_arrays=interpreter.arrays,
                )
                try:
                    str_result = string_eval.evaluate(expr)
                    interpreter.set_typed_variable(var_name, str(str_result))
                except (ValueError, TypeError):
                    # Fallback to literal string if evaluation fails
                    interpreter.set_typed_variable(var_name, str(expr))
            logger.debug("LET %s = {expr}", var_name)
            return ""

        # Handle TIMER special variable
        if expr_upper == "TIMER":
            # pylint: disable=import-outside-toplevel
            from ..features.game_support import get_game_state

            interpreter.set_typed_variable(var_name, get_game_state().get_timer_value())
            logger.debug("LET %s = TIMER", var_name)
            return ""

        try:
            result = interpreter.evaluate_expression(expr)
            interpreter.set_typed_variable(var_name, result)
            logger.debug("LET %s = {result}", var_name)
        except (ValueError, TypeError, ZeroDivisionError) as e:
            # Fallback: try string evaluator for functions that return numbers
            # (LEN, INSTR, VAL)
            try:
                string_eval = StringExpressionEvaluator(
                    string_variables=interpreter.string_variables,
                    numeric_variables=interpreter.variables,
                    string_arrays=getattr(interpreter, "string_arrays", {}),
                    numeric_arrays=interpreter.arrays,
                )
                str_result = string_eval.evaluate(expr)
                interpreter.set_typed_variable(var_name, float(str_result))
            except (ValueError, TypeError, ZeroDivisionError):
                logger.error("LET evaluation error: %s", e)
                return f"❌ Error in LET: {e} (expr: '{expr}')\n"
    except ValidationError as e:
        return f"❌ {e}\n"

    return ""


def _basic_input(interpreter: "Interpreter", args: str) -> str:
    # pylint: disable=import-outside-toplevel
    from ..logging_config import get_logger
    from ..utils.validators import ValidationError, validate_variable_name

    logger = get_logger(__name__)

    var_name = args.strip().upper()
    prompt = "? "
    if '"' in args:
        match = _PRINT_PATTERN.match(args)
        if match:
            prompt = match.group(1) + " "
            var_name = match.group(2).strip().upper()

    if not var_name:
        logger.error("INPUT: Missing variable name")
        return "❌ INPUT requires variable name\n"

    try:
        # Validate variable name format
        validate_variable_name(var_name, allow_suffix=True)
        logger.debug(
            "INPUT: Requesting input for variable '%s' with prompt '%s'",
            var_name,
            prompt,
        )
        interpreter.start_input_request(
            prompt,
            var_name,
            not var_name.endswith("$"),
        )
        return ""
    except ValidationError as e:
        logger.error("INPUT validation failed: %s", e)
        return f"❌ {e}\n"


def _basic_if(
    interpreter: "Interpreter",
    args: str,
    turtle: "TurtleState",
) -> str:
    args_upper = args.upper()

    # Locate THEN keyword — support both " THEN " (with stuff after) and
    # trailing "THEN" (block IF).
    then_pos = -1
    if " THEN " in args_upper:
        then_pos = args_upper.find(" THEN ")
    elif args_upper.rstrip().endswith("THEN"):
        then_pos = args_upper.rstrip().rfind("THEN") - 1
        if then_pos < 0:
            then_pos = -1

    if then_pos == -1:
        return "❌ IF requires THEN keyword\n"

    condition = args[: then_pos + 1].strip()
    then_part = args[then_pos + 6 :].strip() if (then_pos + 6) < len(args) else ""

    # --- String comparison handling ---
    # Detect conditions like: NAME$ = "", A$ <> "hello", X$ = Y$
    _str_cmp_result = _try_string_comparison(interpreter, condition)
    if _str_cmp_result is not None:
        condition_true = _str_cmp_result
    else:
        try:
            result = interpreter.evaluate_expression(condition)
            condition_true = abs(result) > 0.0001
        except (ValueError, TypeError, ZeroDivisionError) as e:
            return f"❌ Error in IF condition: {e}\n"

    # Block IF: nothing after THEN → multi-line
    if not then_part:
        if not hasattr(interpreter, "basic_if_stack"):
            interpreter.basic_if_stack = []
        interpreter.basic_if_stack.append(
            {
                "skip": not condition_true,
                "depth": 0,
            }
        )
        return ""

    # Inline IF...THEN...ELSE
    else_part = ""
    then_upper = then_part.upper()
    else_pos = then_upper.find(" ELSE ")
    if else_pos != -1:
        else_part = then_part[else_pos + 6 :].strip()
        then_part = then_part[:else_pos].strip()

    if condition_true and then_part:
        if then_part.isdigit():
            return _basic_goto(interpreter, then_part)
        return execute_basic(interpreter, then_part, turtle)
    elif not condition_true and else_part:
        if else_part.isdigit():
            return _basic_goto(interpreter, else_part)
        return execute_basic(interpreter, else_part, turtle)
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
        interpreter.current_line = target_idx
    except ValueError:
        return f"❌ Invalid line number: {target}\n"
    except (KeyError, IndexError) as e:
        return f"❌ Error in GOTO: {e}\n"
    return ""


def _basic_on_error_goto(interpreter: "Interpreter", args: str) -> str:
    """ON ERROR GOTO <line> — set error handler, or 0 to disable."""
    target = args.strip()
    if not target:
        return "❌ ON ERROR GOTO requires a line number\n"
    try:
        line_num = int(target)
    except ValueError:
        return f"❌ Invalid line number for ON ERROR GOTO: {target}\n"
    if line_num == 0:
        interpreter.basic_error_handler_line = 0
    else:
        if line_num not in interpreter.line_number_map:
            return f"❌ ON ERROR GOTO {line_num}: line not found\n"
        interpreter.basic_error_handler_line = line_num
    return ""


def _basic_resume(interpreter: "Interpreter", args: str) -> str:
    """RESUME [<line>] — return to the line that caused the error."""
    target = args.strip()
    if target and target.isdigit():
        # RESUME <line> — jump to specific line
        line_num = int(target)
        if line_num not in interpreter.line_number_map:
            return f"❌ RESUME {line_num}: line not found\n"
        interpreter.current_line = interpreter.line_number_map[line_num]
        return ""
    # RESUME with no args: re-execute the error line
    err_line = getattr(interpreter, "basic_error_line", None)
    if err_line is not None and err_line in interpreter.line_number_map:
        interpreter.current_line = interpreter.line_number_map[err_line]
    return ""


def _basic_resume_next(interpreter: "Interpreter") -> str:
    """RESUME NEXT — continue from the line after the one that errored."""
    err_line = getattr(interpreter, "basic_error_line", None)
    if err_line is not None and err_line in interpreter.line_number_map:
        target_idx = interpreter.line_number_map[err_line] + 1
        if target_idx < len(interpreter.program_lines):
            interpreter.current_line = target_idx
    return ""


def _basic_for(interpreter: "Interpreter", args: str) -> str:
    """FOR var = start TO end [STEP step] - Loop from start to end.

    Args:
        interpreter: Interpreter instance
        args: Loop control expression

    Returns:
        Error message if parsing fails, empty string on success
    """
    # pylint: disable=import-outside-toplevel
    from ..core.interpreter import ForContext
    from ..logging_config import get_logger
    from ..utils.validators import ValidationError, validate_variable_name

    logger = get_logger(__name__)

    match = _FOR_PATTERN.match(args.upper())
    if not match:
        return "❌ FOR requires format: var = start TO end [STEP step]\n"

    var_name = match.group(1)
    start_expr = match.group(2)
    end_expr = match.group(3)
    step_expr = match.group(4) if match.group(4) else "1"

    try:
        validate_variable_name(var_name, allow_suffix=True)
        start_val = interpreter.evaluate_expression(start_expr)
        end_val = interpreter.evaluate_expression(end_expr)
        step_val = interpreter.evaluate_expression(step_expr)

        if step_val == 0:
            return "❌ FOR STEP cannot be 0\n"

        interpreter.set_typed_variable(var_name, start_val)

        context = ForContext(
            var_name=var_name,
            end_value=end_val,
            step=step_val,
            for_line=interpreter.current_line,
        )
        interpreter.for_stack.append(context)
        logger.debug("FOR %s = {start_val} TO {end_val} STEP {step_val}", var_name)
    except ValidationError as e:
        return f"❌ {e}\n"
    except (ValueError, TypeError, ZeroDivisionError) as e:
        logger.error("FOR error: %s", e)
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
        interpreter.current_line = context.for_line + 1
    else:
        interpreter.for_stack.pop()
    return ""


def _basic_gosub(interpreter: "Interpreter", args: str) -> str:
    """GOSUB line_number - Call subroutine at line number.

    Args:
        interpreter: Interpreter instance
        args: Target line number

    Returns:
        Error message if validation fails, empty string on success
    """
    from ..logging_config import get_logger
    from ..utils.validators import ValidationError, validate_numeric

    logger = get_logger(__name__)
    target = args.strip()

    if not target:
        return "❌ GOSUB requires line number\n"

    try:
        line_num = validate_numeric(target, param_name="line number")
        interpreter.gosub_stack.append(interpreter.current_line)
        interpreter.jump_to_line_number(int(line_num))
        logger.debug("GOSUB to line %s", int(line_num))
    except ValidationError as e:
        return f"❌ {e}\n"
    except (KeyError, IndexError) as e:
        logger.error("GOSUB failed: %s", e)
        return f"❌ Error in GOSUB: {e}\n"

    return ""


def _basic_return(interpreter: "Interpreter") -> str:
    """RETURN - Return from subroutine.

    Returns:
        Error message if GOSUB stack is empty, empty string on success
    """
    from ..logging_config import get_logger

    logger = get_logger(__name__)

    if not interpreter.gosub_stack:
        logger.warning("RETURN executed without matching GOSUB")
        return "❌ RETURN without GOSUB\n"

    try:
        return_line = interpreter.gosub_stack.pop()
        interpreter.current_line = return_line + 1
        logger.debug("RETURN to line %s", return_line + 2)
    except IndexError as e:
        logger.error("RETURN failed: %s", e)
        return f"❌ Error in RETURN: {e}\n"

    return ""


def _basic_screen(
    interpreter: "Interpreter", args: str, _turtle: Optional["TurtleState"] = None
) -> str:
    """SCREEN mode[, width, height] - Set screen mode.

    Args:
        interpreter: Interpreter instance
        args: Screen mode and optional dimensions
        turtle: Turtle graphics state

    Returns:
        Status message or error

    Raises:
        Returns error message string on validation failure
    """
    # pylint: disable=import-outside-toplevel
    from ..core.interpreter import ScreenMode
    from ..logging_config import get_logger
    from ..utils.validators import (
        ValidationError,
        validate_numeric,
        validate_range,
    )

    logger = get_logger(__name__)

    parts = args.split(",")
    if not parts:
        return "❌ SCREEN requires mode parameter\n"

    try:
        mode_str = validate_numeric(parts[0].strip(), param_name="screen mode")
        mode = int(mode_str)

        # Validate mode is 0 or 1
        validate_range(mode, 0, 1, "screen mode")

        if mode == 0:  # Text mode
            interpreter.screen_mode = ScreenMode.TEXT
            if len(parts) >= 3:
                try:
                    cols_str = validate_numeric(parts[1].strip(), param_name="columns")
                    rows_str = validate_numeric(parts[2].strip(), param_name="rows")
                    cols = int(cols_str)
                    rows = int(rows_str)
                    validate_range(cols, 40, 120, "columns")
                    validate_range(rows, 20, 50, "rows")
                    interpreter.screen_config.cols = cols
                    interpreter.screen_config.rows = rows
                except ValidationError as e:
                    return f"❌ {e}\n"
            cols, rows = (
                interpreter.screen_config.cols,
                interpreter.screen_config.rows,
            )
            logger.info("Set TEXT mode (%sx%s)", cols, rows)
            return f"🎨 Text mode ({cols}x{rows})\n"

        if mode == 1:  # Graphics mode
            interpreter.screen_mode = ScreenMode.GRAPHICS
            if len(parts) >= 3:
                try:
                    width_str = validate_numeric(parts[1].strip(), param_name="width")
                    height_str = validate_numeric(parts[2].strip(), param_name="height")
                    width = int(width_str)
                    height = int(height_str)
                    validate_range(width, 320, 1920, "width")
                    validate_range(height, 200, 1200, "height")
                    interpreter.screen_config.width = width
                    interpreter.screen_config.height = height
                except ValidationError as e:
                    return f"❌ {e}\n"
            width, height = (
                interpreter.screen_config.width,
                interpreter.screen_config.height,
            )
            logger.info("Set GRAPHICS mode (%sx%s)", width, height)
            return f"🎨 Graphics mode ({width}x{height})\n"

        return f"❌ Unsupported SCREEN mode: {mode}\n"

    except ValueError as e:
        return f"❌ Invalid SCREEN parameters: {e}\n"


def _basic_locate(interpreter: "Interpreter", args: str) -> str:
    """LOCATE row, col - Move cursor to position.

    Args:
        interpreter: Interpreter instance
        args: Row and column coordinates

    Returns:
        Error message if parsing fails, empty string on success
    """
    from ..logging_config import get_logger
    from ..utils.validators import (
        ValidationError,
        validate_numeric,
        validate_range,
    )

    logger = get_logger(__name__)

    parts = args.split(",")
    if len(parts) < 2:
        return "❌ LOCATE requires row, col\n"

    try:
        row_str = validate_numeric(parts[0].strip(), param_name="row")
        col_str = validate_numeric(parts[1].strip(), param_name="column")

        row = int(interpreter.evaluate_expression(row_str))
        col = int(interpreter.evaluate_expression(col_str))

        validate_range(row, 1, 25, "row")
        validate_range(col, 1, 80, "column")

        interpreter.cursor_row = max(0, min(24, row - 1))
        interpreter.cursor_col = max(0, min(79, col - 1))
        logger.debug("LOCATE %s,{col}", row)
    except ValidationError as e:
        return f"❌ {e}\n"
    except (ValueError, TypeError, ZeroDivisionError) as e:
        logger.error("LOCATE error: %s", e)
        return f"❌ LOCATE error: {e}\n"

    return ""


def _basic_line(
    _interpreter: "Interpreter",
    args: str,
    t: "TurtleState",
) -> str:
    """Draw line in BASIC graphics. Syntax: LINE (x1,y1)-(x2,y2)[,color][,B|BF]

    Args:
        _interpreter: Interpreter instance
        args: Line parameters
        t: Turtle graphics state

    Returns:
        Error message or empty string

    Examples:
        LINE (0,0)-(100,100)              ; Draw line in current color
        LINE (0,0)-(100,100),5            ; Draw line in palette color 5
        LINE (10,10)-(100,100),15,B       ; Draw rectangle outline
        LINE (10,10)-(100,100),4,BF       ; Draw filled rectangle
    """
    # Try QBasic syntax: (x1, y1)-(x2, y2)[,color][,B|BF]
    match = re.match(
        r"\(\s*([^,]+)\s*,\s*([^,]+)\s*\)\s*-\s*"
        r"\(\s*([^,]+)\s*,\s*([^,]+)\s*\)"
        r"(?:\s*,\s*([^,]+?))?"
        r"(?:\s*,\s*(B|BF|b|bf))?"
        r"\s*$",
        args,
    )

    x1_str, y1_str, x2_str, y2_str, color_str, mode = "", "", "", "", None, None

    if match:
        x1_str = match.group(1)
        y1_str = match.group(2)
        x2_str = match.group(3)
        y2_str = match.group(4)
        color_str = match.group(5)
        mode = match.group(6).upper() if match.group(6) else None
    else:
        # Try standard syntax: x1, y1, x2, y2[, color][, B|BF]
        parts = [p.strip() for p in args.split(",")]
        if len(parts) >= 4:
            x1_str = parts[0]
            y1_str = parts[1]
            x2_str = parts[2]
            y2_str = parts[3]
            color_str = parts[4] if len(parts) > 4 else None
            if len(parts) > 5:
                mode = parts[5].upper()
        else:
            return "❌ LINE requires (x1,y1)-(x2,y2)\n"

    try:
        # Evaluate coordinates
        x1 = _interpreter.evaluate_expression(x1_str)
        y1 = _interpreter.evaluate_expression(y1_str)
        x2 = _interpreter.evaluate_expression(x2_str)
        y2 = _interpreter.evaluate_expression(y2_str)

        # Resolve color
        color = t.pen_color
        if color_str:
            color = t.resolve_color(color_str, default=t.pen_color)

        # Handle drawing modes
        if mode == "B":
            # Draw rectangle outline (four lines)
            t.draw_line(x1, y1, x2, y1, color=color, width=t.pen_width)
            t.draw_line(x2, y1, x2, y2, color=color, width=t.pen_width)
            t.draw_line(x2, y2, x1, y2, color=color, width=t.pen_width)
            t.draw_line(x1, y2, x1, y1, color=color, width=t.pen_width)
        elif mode == "BF":
            # Draw filled rectangle
            t.draw_rect(x1, y1, x2, y2, color=color, fill_color=color)
        else:
            # Draw simple line
            t.draw_line(x1, y1, x2, y2, color=color, width=t.pen_width)

        return ""
    except (ValueError, TypeError, ZeroDivisionError) as e:
        return f"❌ LINE error: {e}\n"


def _basic_circle(i: "Interpreter", args: str, t: "TurtleState") -> str:
    """Draw circle in BASIC graphics. Syntax: CIRCLE (x,y),r[,color[,start,end]]

    Args:
        i: Interpreter instance
        args: Circle parameters
        t: Turtle graphics state

    Returns:
        Error message or empty string

    Examples:
        CIRCLE (100,100),50               ; Draw circle at (100,100) radius 50
        CIRCLE (100,100),50,4             ; Draw in palette color 4
        CIRCLE (100,100),50,4,0,3.14158   ; Draw arc from 0 to π radians
    """
    # Try QBasic syntax: (x, y), radius[, color[, start, end]]
    match = re.match(
        r"\(\s*([^,]+)\s*,\s*([^,]+)\s*\)\s*,\s*([^,)]+)"
        r"(?:\s*,\s*([^,)]+))?"
        r"(?:\s*,\s*([^,)]+))?"
        r"(?:\s*,\s*([^,)]+))?",
        args,
    )

    x_str, y_str, r_str, color_str = "", "", "", None
    start_angle_str, end_angle_str = None, None

    if match:
        x_str = match.group(1)
        y_str = match.group(2)
        r_str = match.group(3)
        color_str = match.group(4)
        start_angle_str = match.group(5)
        end_angle_str = match.group(6)
    else:
        # Try standard syntax: x, y, radius[, color[, start, end]]
        parts = args.split(",")
        if len(parts) >= 3:
            x_str = parts[0]
            y_str = parts[1]
            r_str = parts[2]
            color_str = parts[3] if len(parts) > 3 else None
            start_angle_str = parts[4] if len(parts) > 4 else None
            end_angle_str = parts[5] if len(parts) > 5 else None
        else:
            return "❌ CIRCLE requires (x,y),radius\n"

    try:
        # Evaluate coordinates and radius
        center_x = i.evaluate_expression(x_str)
        center_y = i.evaluate_expression(y_str)
        radius = i.evaluate_expression(r_str)

        if radius <= 0:
            return "❌ CIRCLE radius must be positive\n"

        # Resolve color
        color = t.pen_color
        if color_str:
            color = t.resolve_color(color_str, default=t.pen_color)

        # Handle arc (start/end angles in radians)
        start_angle = 0.0
        end_angle = 360.0
        if start_angle_str:
            start_angle = math.degrees(i.evaluate_expression(start_angle_str))
        if end_angle_str:
            end_angle = math.degrees(i.evaluate_expression(end_angle_str))

        # Draw arc via turtle.circle (which uses heading 0=up)
        extent = end_angle - start_angle
        if extent == 0:
            extent = 360.0

        # Move to starting position on circle
        old_pen = t.pen_down
        t.penup()
        t.goto(center_x + radius, center_y)
        t.pendown()

        # Temporarily set pen color
        old_color = t.pen_color
        t.setcolor(color[0], color[1], color[2])

        # Draw circle/arc
        segments = max(12, int(abs(extent) / 10))
        angle_step = extent / segments

        for step in range(segments + 1):
            angle = start_angle + step * angle_step
            rad = math.radians(angle)
            x = center_x + radius * math.cos(rad)
            y = center_y + radius * math.sin(rad)
            t.goto(x, y)

        # For complete circles, close the path
        if extent >= 360.0:
            t.goto(center_x + radius, center_y)

        # Restore pen state
        t.setcolor(old_color[0], old_color[1], old_color[2])
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


def _basic_pset(i: "Interpreter", args: str, t: "TurtleState") -> str:
    """PSET (x, y)[, color] - Set a pixel (paint set).

    Args:
        i: Interpreter instance
        args: Coordinates and optional color
        t: Turtle graphics state

    Returns:
        Error message or empty string

    Example:
        PSET (100,100)               ; Draw pixel at (100,100) in current color
        PSET (100,100),15            ; Draw pixel in color 15 (white)
    """
    # Parse (x, y)[, color]
    match = re.match(
        r"\(\s*([^,]+)\s*,\s*([^,]+)\s*\)(?:\s*,\s*(.+))?",
        args,
    )

    if not match:
        return "❌ PSET requires (x,y)\n"

    try:
        x = i.evaluate_expression(match.group(1))
        y = i.evaluate_expression(match.group(2))

        color = t.pen_color
        if match.group(3):
            color_str = match.group(3).strip()
            color = t.resolve_color(color_str, default=t.pen_color)

        t.draw_point(x, y, color=color)
        return ""
    except (ValueError, TypeError, ZeroDivisionError) as e:
        return f"❌ PSET error: {e}\n"


def _basic_preset(i: "Interpreter", args: str, t: "TurtleState") -> str:
    """PRESET (x, y)[, color] - Erase a pixel (preset to background).

    Args:
        i: Interpreter instance
        args: Coordinates and optional color
        t: Turtle graphics state

    Returns:
        Error message or empty string

    Example:
        PRESET (100,100)             ; Erase pixel (set to background color)
        PRESET (100,100),0           ; Erase pixel (set to color 0, black)
    """
    # Parse (x, y)[, color]
    match = re.match(
        r"\(\s*([^,]+)\s*,\s*([^,]+)\s*\)(?:\s*,\s*(.+))?",
        args,
    )

    if not match:
        return "❌ PRESET requires (x,y)\n"

    try:
        x = i.evaluate_expression(match.group(1))
        y = i.evaluate_expression(match.group(2))

        # Default to background color (or black)
        color = t.bg_color
        if match.group(3):
            color_str = match.group(3).strip()
            color = t.resolve_color(color_str, default=t.bg_color)

        t.draw_point(x, y, color=color)
        return ""
    except (ValueError, TypeError, ZeroDivisionError) as e:
        return f"❌ PRESET error: {e}\n"


def _basic_paint(_i: "Interpreter", args: str, t: "TurtleState") -> str:
    """PAINT (x, y)[, fill_color[, border_color]] - Fill a closed region.

    Args:
        i: Interpreter instance
        args: Starting coordinates, fill color, optional border color
        t: Turtle graphics state

    Returns:
        Error message or empty string

    Example:
        PAINT (150,150),4            ; Fill region starting at (150,150) with color 4
        PAINT (150,150),15,1         ; Fill with color 15, stop at color 1 borders
    """
    # For now, apply fill to the last fillable shape (polygon/rect)
    # This is a simplified implementation
    parts = [p.strip() for p in args.split(",")]

    if len(parts) < 1:
        return "❌ PAINT requires coordinates\n"

    try:
        # Fill color
        fill_color = (255, 255, 255)  # Default white
        if len(parts) >= 2:
            fill_color = t.resolve_color(parts[1], default=fill_color)

        # If there's a last fillable shape, fill it
        if t.fill_last_shape(fill_color):
            return ""
        else:
            return "ℹ️ No shape to fill\n"
    except (ValueError, TypeError) as e:
        return f"❌ PAINT error: {e}\n"


def _basic_system(_interpreter: "Interpreter", _args: str) -> str:
    """SYSTEM - Return to system prompt"""
    # In an IDE environment, this might just reset
    _interpreter.reset()
    return "ℹ️ Returned to system prompt\n"


def _find_matching_wend(interpreter: "Interpreter", start_idx: int) -> int:
    """Find index of matching WEND for WHILE at start_idx."""
    depth = 0
    for i in range(start_idx + 1, len(interpreter.program_lines)):
        cmd = interpreter.program_lines[i][1].strip().upper()
        if cmd.startswith("WHILE "):
            depth += 1
        elif cmd == "WEND":
            if depth == 0:
                return i
            depth -= 1
    return -1


def _basic_while(interpreter: "Interpreter", args: str) -> str:
    """WHILE condition - Start while loop"""
    try:
        # Evaluate the condition
        result = interpreter.evaluate_expression(args.strip())
        if result != 0:  # Non-zero means true in BASIC
            # Push current line index to stack
            interpreter.basic_while_stack.append(interpreter.current_line)
            return ""

        # Condition false: skip to WEND
        wend_idx = _find_matching_wend(interpreter, interpreter.current_line)
        if wend_idx == -1:
            return "❌ WHILE without WEND\n"

        # Jump past WEND (line_changed path does not increment)
        interpreter.current_line = wend_idx + 1
        return ""
    except (ValueError, TypeError, ZeroDivisionError) as e:
        return f"❌ WHILE syntax error: {e}\n"


def _basic_wend(interpreter: "Interpreter") -> str:
    """WEND - End while loop"""
    if not interpreter.basic_while_stack:
        return "❌ WEND without WHILE\n"

    while_idx = interpreter.basic_while_stack.pop()
    # Jump back to WHILE to re-evaluate condition
    # (line_changed path does NOT increment current_line)
    interpreter.current_line = while_idx
    return ""


def _find_matching_loop(interpreter: "Interpreter", start_idx: int) -> int:
    """Find index of matching LOOP for DO at start_idx."""
    depth = 0
    for i in range(start_idx + 1, len(interpreter.program_lines)):
        cmd = interpreter.program_lines[i][1].strip().upper()
        if cmd.startswith("DO"):
            depth += 1
        elif cmd.startswith("LOOP"):
            if depth == 0:
                return i
            depth -= 1
    return -1


def _basic_do(interpreter: "Interpreter", args: str) -> str:
    """DO [WHILE/UNTIL cond] - Start do loop"""
    # Handle DO WHILE/UNTIL condition if present
    args = args.strip().upper()
    should_run = True

    if args.startswith("WHILE "):
        cond = args[6:].strip()
        try:
            val = interpreter.evaluate_expression(cond)
            should_run = val != 0
        except (ValueError, TypeError):
            return "❌ Invalid DO WHILE condition\n"
    elif args.startswith("UNTIL "):
        cond = args[6:].strip()
        try:
            val = interpreter.evaluate_expression(cond)
            should_run = val == 0
        except (ValueError, TypeError):
            return "❌ Invalid DO UNTIL condition\n"

    if should_run:
        interpreter.basic_do_stack.append((interpreter.current_line, "DO"))
        return ""

    # Skip to LOOP
    loop_idx = _find_matching_loop(interpreter, interpreter.current_line)
    if loop_idx == -1:
        return "❌ DO without LOOP\n"
    interpreter.current_line = loop_idx + 1
    return ""


def _basic_loop(interpreter: "Interpreter", args: str) -> str:
    """LOOP [WHILE/UNTIL cond] - End do loop"""
    if not interpreter.basic_do_stack:
        return "❌ LOOP without DO\n"

    do_idx, _ = interpreter.basic_do_stack.pop()

    # Check exit condition
    args = args.strip().upper()
    should_loop = True

    if args.startswith("WHILE "):
        cond = args[6:].strip()
        try:
            val = interpreter.evaluate_expression(cond)
            should_loop = val != 0
        except (ValueError, TypeError):
            return "❌ Invalid LOOP WHILE condition\n"
    elif args.startswith("UNTIL "):
        cond = args[6:].strip()
        try:
            val = interpreter.evaluate_expression(cond)
            should_loop = val == 0  # Loop until true (so loop while false)
        except (ValueError, TypeError):
            return "❌ Invalid LOOP UNTIL condition\n"

    if should_loop:
        # Jump back to DO (re-execute DO check if any)
        # line_changed path does NOT increment, so set to exact index
        interpreter.current_line = do_idx

    return ""


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


def _basic_sub(interpreter: "Interpreter", args: str) -> str:
    """SUB name[(params)] - Define subroutine.

    When execution flow reaches this line (definition site), skip past
    END SUB so the body isn't executed immediately.  The block was
    already registered during the load-time prescan.
    """
    args = args.strip()
    if not args:
        return "❌ SUB requires a name\n"

    name = args.split("(")[0].strip().upper()
    definition = interpreter.basic_subs.get(name)

    if definition and "end_line" in definition:
        # Skip past END SUB (main loop will increment current_line)
        interpreter.current_line = definition["end_line"]
    else:
        # Fallback: scan forward
        start_line = interpreter.current_line
        for i in range(start_line + 1, len(interpreter.program_lines)):
            _, line_text = interpreter.program_lines[i]
            if line_text.strip().upper() == "END SUB":
                interpreter.current_line = i
                break
    return ""


def _basic_end_sub(interpreter: "Interpreter") -> str:
    """END SUB - Return from subroutine call."""
    if not interpreter.basic_call_stack:
        # Just a definition boundary, nothing to return from
        interpreter.basic_in_sub = False
        return ""

    # Pop call frame and restore state
    frame = interpreter.basic_call_stack.pop()
    # Restore numeric variables that were overwritten by parameters
    for var_name, old_value in frame.get("saved_vars", {}).items():
        if old_value is None:
            interpreter.variables.pop(var_name, None)
        else:
            interpreter.variables[var_name] = old_value
    # Restore string variables
    for var_name, old_value in frame.get("saved_str_vars", {}).items():
        if old_value is None:
            interpreter.string_variables.pop(var_name, None)
        else:
            interpreter.string_variables[var_name] = old_value
    # Return to the line after the CALL (main loop detects line_changed
    # and does continue without incrementing, so we add 1 here)
    interpreter.current_line = frame["return_line"] + 1
    interpreter.basic_in_sub = False
    return ""


def _basic_function(interpreter: "Interpreter", args: str) -> str:
    """FUNCTION name[(params)] - Define function.

    When execution flow reaches this line, skip past END FUNCTION.
    The block was already registered during the load-time prescan.
    """
    args = args.strip()
    if not args:
        return "❌ FUNCTION requires a name\n"

    name = args.split("(")[0].strip().upper()
    definition = interpreter.basic_functions.get(name)

    if definition and "end_line" in definition:
        interpreter.current_line = definition["end_line"]
    else:
        # Fallback: scan forward
        start_line = interpreter.current_line
        for i in range(start_line + 1, len(interpreter.program_lines)):
            _, line_text = interpreter.program_lines[i]
            if line_text.strip().upper() == "END FUNCTION":
                interpreter.current_line = i
                break
    return ""


def _basic_end_function(interpreter: "Interpreter") -> str:
    """END FUNCTION - Return from function call."""
    if not interpreter.basic_call_stack:
        interpreter.basic_in_function = False
        return ""

    frame = interpreter.basic_call_stack.pop()
    func_name = frame.get("func_name", "")

    # The function return value is stored in a variable matching the
    # function name (e.g., FUNCTION ADD sets ADD = result)
    return_value = interpreter.variables.get(func_name, 0)

    # Restore numeric variables
    for var_name, old_value in frame.get("saved_vars", {}).items():
        if old_value is None:
            interpreter.variables.pop(var_name, None)
        else:
            interpreter.variables[var_name] = old_value
    # Restore string variables
    for var_name, old_value in frame.get("saved_str_vars", {}).items():
        if old_value is None:
            interpreter.string_variables.pop(var_name, None)
        else:
            interpreter.string_variables[var_name] = old_value

    # Place return value in the variable namespace so caller can use it
    interpreter.variables[func_name] = return_value
    # Return to the line after the CALL
    interpreter.current_line = frame["return_line"] + 1
    interpreter.basic_in_function = False
    return ""


def _basic_call(interpreter: "Interpreter", args: str) -> str:
    """CALL subroutine[(args)] - Call a SUB or FUNCTION by name."""
    from ..logging_config import get_logger

    logger = get_logger(__name__)

    args = args.strip()
    if not args:
        logger.error("CALL: Missing subroutine name")
        return "❌ CALL requires subroutine name\n"

    # Parse: CALL MySub(expr1, expr2)
    if "(" in args:
        sub_name = args[: args.index("(")].strip().upper()
        arg_str = args[args.index("(") + 1 :]
        if arg_str.endswith(")"):
            arg_str = arg_str[:-1]
        call_args = [a.strip() for a in arg_str.split(",") if a.strip()]
    else:
        sub_name = args.upper()
        call_args = []

    # Look up in subs first, then functions
    is_function = False
    definition = interpreter.basic_subs.get(sub_name)
    if definition is None:
        definition = interpreter.basic_functions.get(sub_name)
        is_function = True
    if definition is None:
        logger.error("CALL: Subroutine '%s' not defined", sub_name)
        return f"❌ Subroutine not found: {sub_name}\n"

    params = definition["params"]

    # Evaluate argument expressions
    evaluated_args: list[Any] = []
    for arg_expr in call_args:
        try:
            val = interpreter.evaluate_expression(arg_expr)
            evaluated_args.append(val)
        except (ValueError, TypeError):
            # Might be a string
            evaluated_args.append(arg_expr.strip('"'))

    # Save current variables that will be overwritten by params
    saved_vars = {}  # numeric vars backup
    saved_str_vars = {}  # string vars backup
    for i, param_name in enumerate(params):
        if param_name.endswith("$"):
            # String parameter — use string_variables
            saved_str_vars[param_name] = interpreter.string_variables.get(param_name)
            if i < len(evaluated_args):
                interpreter.string_variables[param_name] = str(evaluated_args[i])
            else:
                interpreter.string_variables[param_name] = ""
        else:
            # Numeric parameter — use variables
            saved_vars[param_name] = interpreter.variables.get(param_name)
            if i < len(evaluated_args):
                interpreter.variables[param_name] = evaluated_args[i]
            else:
                interpreter.variables[param_name] = 0  # default

    # Push call frame
    interpreter.basic_call_stack.append(
        {
            "return_line": interpreter.current_line,
            "saved_vars": saved_vars,
            "saved_str_vars": saved_str_vars,
            "func_name": sub_name if is_function else "",
        }
    )

    # Jump to sub body (main loop detects line_changed and does NOT
    # increment, so set directly to the first body line)
    interpreter.current_line = definition["start_line"]
    if is_function:
        interpreter.basic_in_function = True
    else:
        interpreter.basic_in_sub = True
    logger.debug("CALL: Jumping to %s at line %d", sub_name, definition["start_line"])
    return ""


def _basic_type_def(interpreter: "Interpreter", args: str) -> str:
    """TYPE typename ... END TYPE - Define a user-defined type (VB-classic)."""
    type_name = args.strip().upper()
    if not type_name:
        return "❌ TYPE requires a name\n"
    if not hasattr(interpreter, "basic_types"):
        interpreter.basic_types = {}
    fields: dict[str, Any] = {}
    # Scan forward through program_lines to collect field declarations
    scan_line = interpreter.current_line + 1
    while scan_line < len(interpreter.program_lines):
        _, line_text = interpreter.program_lines[scan_line]
        ln_up = line_text.strip().upper()
        # Handle "NNN END TYPE" (numbered lines) or plain "END TYPE"
        # Strip leading line number if present
        stripped = re.sub(r"^\d+\s+", "", line_text.strip())
        stripped_up = stripped.upper()
        if stripped_up in ("END TYPE", "ENDTYPE") or ln_up in ("END TYPE", "ENDTYPE"):
            interpreter.current_line = scan_line
            break
        # Field declaration: fieldname AS type
        fm = re.match(
            r"^\s*([A-Za-z_][A-Za-z0-9_]*\$?)\s+AS\s+([A-Za-z_][A-Za-z0-9_]*)\s*$",
            stripped,
            re.IGNORECASE,
        )
        if fm:
            fname = fm.group(1).strip().upper()
            ftype = fm.group(2).strip().upper()
            if ftype == "STRING":
                fields[fname] = ""
            elif ftype == "BOOLEAN":
                fields[fname] = False
            else:
                fields[fname] = 0.0
        scan_line += 1
    interpreter.basic_types[type_name] = fields
    return ""


def _basic_dim(interpreter: "Interpreter", args: str) -> str:
    """DIM variable[(dimensions)] [AS type] - Declare array or typed variable"""
    # pylint: disable=import-outside-toplevel
    from ..logging_config import get_logger
    from ..utils.validators import (
        ValidationError,
        validate_numeric,
        validate_variable_name,
    )

    logger = get_logger(__name__)

    parts = args.split(",")
    for part in parts:
        part = part.strip()
        # DIM varname AS typename — user-defined type instance
        as_m = re.match(
            r"^([A-Za-z_][A-Za-z0-9_]*\$?)\s+AS\s+([A-Za-z_][A-Za-z0-9_]*)$",
            part,
            re.IGNORECASE,
        )
        if as_m:
            var_name = as_m.group(1).strip().upper()
            type_name = as_m.group(2).strip().upper()
            basic_types = getattr(interpreter, "basic_types", {})
            if type_name in basic_types:
                # Create an instance (copy of default field values)
                interpreter.variables[var_name] = dict(basic_types[type_name])
            else:
                interpreter.variables[var_name] = {}
            continue
        if "(" not in part or not part.endswith(")"):
            logger.error("DIM: Invalid syntax '%s'", part)
            return f"❌ Invalid DIM syntax: {part}\n"

        # Extract name and size from the form NAME(size)
        name_part = part[: part.find("(")].strip().upper()
        size_part = part[part.find("(") + 1 : -1].strip()

        try:
            # Validate array name format
            validate_variable_name(name_part, allow_suffix=True)

            # Validate and evaluate array size
            size_str = validate_numeric(size_part, "array dimension")
            size = int(interpreter.evaluate_expression(size_str))

            if size < 0:
                logger.error("DIM: Negative array dimension for %s", name_part)
                return f"❌ Array dimension must be non-negative: {size}\n"

            # Create array — string arrays (ending with $) store strings,
            # numeric arrays store floats.
            if name_part.endswith("$"):
                interpreter.string_arrays[name_part] = [""] * (size + 1)
            else:
                interpreter.arrays[name_part] = [0.0] * (size + 1)
            logger.debug("DIM: Created array '%s' with size {size}", name_part)
        except ValidationError as e:
            logger.error("DIM validation failed for %s: {e}", name_part)
            return f"❌ {e}\n"
        except (ValueError, TypeError, ZeroDivisionError) as e:
            logger.error("DIM evaluation error for %s: {e}", name_part)
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


def _basic_color(
    _interpreter: "Interpreter",
    args: str,
    turtle: Optional["TurtleState"] = None,
) -> str:
    """COLOR [foreground][,background] - Set pen color and background.

    Args:
        interpreter: Interpreter instance
        args: Color arguments (attribute, background)
        turtle: Turtle graphics state

    Returns:
        Status message or empty string
    """
    parts = args.split(",")
    if not args.strip():
        return ""

    try:
        # First argument: pen color (resolve as palette or RGB)
        if len(parts) >= 1 and parts[0].strip():
            fg_str = parts[0].strip()
            if turtle:
                color_val = turtle.resolve_color(fg_str, default=turtle.pen_color)
                turtle.setcolor(color_val[0], color_val[1], color_val[2])

        # Second argument: background color
        if len(parts) >= 2 and parts[1].strip():
            bg_str = parts[1].strip()
            if turtle:
                bg_val = turtle.resolve_color(bg_str, default=turtle.bg_color)
                turtle.setbgcolor(bg_val[0], bg_val[1], bg_val[2])
                return "🎨 Colors set\n"

        return "🎨 Pen color set\n"
    except (ValueError, TypeError) as e:
        return f"❌ COLOR error: {e}\n"


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


# ============================================================================
# Game Support Commands
# ============================================================================


def _basic_beep(_interpreter: "Interpreter") -> str:
    """BEEP - Play system beep sound."""
    # pylint: disable=import-outside-toplevel
    from ..features.game_support import get_game_state

    game = get_game_state()
    game.sound.beep()
    return "🔊 BEEP\n"


def _basic_sound(interpreter: "Interpreter", args: str) -> str:
    """SOUND frequency, duration - Play a tone.

    frequency: Hz (37-32767)
    duration: clock ticks (18.2 ticks = 1 second)
    """
    # pylint: disable=import-outside-toplevel
    from ..features.game_support import get_game_state

    parts = args.split(",")
    if len(parts) < 2:
        return "❌ SOUND requires frequency, duration\n"

    try:
        freq = int(interpreter.evaluate_expression(parts[0].strip()))
        duration = int(interpreter.evaluate_expression(parts[1].strip()))
        # Convert ticks to milliseconds (18.2 ticks/sec)
        duration_ms = int(duration * 1000 / 18.2)

        game = get_game_state()
        game.sound.play_tone(freq, duration_ms)
        return f"🔊 SOUND {freq}Hz for {duration_ms}ms\n"
    except (ValueError, TypeError) as e:
        return f"❌ SOUND error: {e}\n"


def _basic_speed(
    _interpreter: "Interpreter",
    args: str,
    turtle: "TurtleState",
) -> str:
    """SPEED n - Set turtle animation speed (0=fastest, 10=slowest)."""
    # pylint: disable=import-outside-toplevel
    from ..features.game_support import get_game_state

    try:
        speed = int(args.strip())
        speed = max(0, min(10, speed))  # Clamp to 0-10
        game = get_game_state()
        game.turtle_speed = speed

        # Also set on turtle state if available
        if hasattr(turtle, "animation_speed"):
            turtle.animation_speed = speed

        return f"🐢 Turtle speed set to {speed}\n"
    except (ValueError, TypeError):
        return "❌ SPEED requires numeric value (0-10)\n"


def _basic_sprite(interpreter: "Interpreter", args: str) -> str:
    """SPRITE name, x, y, width, height - Define/update a sprite for collision.

    SPRITE name, x, y, width, height - Create/update sprite
    SPRITE name, x, y - Move existing sprite
    SPRITE name, OFF - Remove sprite
    """
    # pylint: disable=import-outside-toplevel
    from ..features.game_support import get_game_state

    parts = [p.strip() for p in args.split(",")]
    if len(parts) < 2:
        return "❌ SPRITE requires name and coordinates\n"

    name = parts[0].upper()
    game = get_game_state()

    # Handle SPRITE name, OFF
    if len(parts) == 2 and parts[1].upper() == "OFF":
        game.collision.remove_sprite(name)
        return f"🎮 Removed sprite: {name}\n"

    try:
        x = interpreter.evaluate_expression(parts[1])
        y = interpreter.evaluate_expression(parts[2]) if len(parts) > 2 else 0

        if len(parts) >= 5:
            # Full definition
            w = interpreter.evaluate_expression(parts[3])
            h = interpreter.evaluate_expression(parts[4])
            game.collision.register_sprite(name, x, y, w, h)
            return f"🎮 Sprite {name} at ({x}, {y}) size {w}x{h}\n"
        else:
            # Update position only
            game.collision.update_sprite(name, x, y)
            return f"🎮 Moved sprite {name} to ({x}, {y})\n"
    except (ValueError, TypeError, IndexError) as e:
        return f"❌ SPRITE error: {e}\n"


def _basic_on_timer(_interpreter: "Interpreter", args: str) -> str:
    """ON TIMER(n) GOSUB line - Set up timer event.

    n = interval in seconds
    """
    # pylint: disable=import-outside-toplevel
    from ..features.game_support import get_game_state

    # Parse: ON TIMER(n) GOSUB line
    pattern = r"\((\d+(?:\.\d+)?)\)\s*GOSUB\s*(\d+)"
    match = re.match(pattern, args.strip(), re.IGNORECASE)
    if not match:
        return "❌ ON TIMER requires format: ON TIMER(n) GOSUB line\n"

    try:
        interval = float(match.group(1))
        target_line = int(match.group(2))

        game = get_game_state()
        game.timer.set_interval(1, interval, target_line, enabled=False)
        return f"⏱️ Timer set: every {interval}s GOSUB {target_line}\n"
    except (ValueError, TypeError) as e:
        return f"❌ ON TIMER error: {e}\n"


def _basic_timer_on(_interpreter: "Interpreter") -> str:
    """TIMER ON - Enable timer events."""
    # pylint: disable=import-outside-toplevel
    from ..features.game_support import get_game_state

    game = get_game_state()
    game.timer.enable_interval(1, True)
    return "⏱️ Timer enabled\n"


def _basic_timer_off(_interpreter: "Interpreter") -> str:
    """TIMER OFF - Disable timer events."""
    # pylint: disable=import-outside-toplevel
    from ..features.game_support import get_game_state

    game = get_game_state()
    game.timer.enable_interval(1, False)
    return "⏱️ Timer disabled\n"


# ============================================================================
# Music and Speech Commands
# ============================================================================


def _basic_play(_interpreter: "Interpreter", args: str) -> str:
    """PLAY mml$ - Play music using MML notation.

    Examples:
        PLAY "CDEFGAB"           - Play a scale
        PLAY "T120 L4 CDEFG"     - Set tempo 120, quarter notes
        PLAY "O4 C E G >C"       - Octave 4, then up an octave
    """
    # pylint: disable=import-outside-toplevel
    from ..features.music import get_music_player

    # Remove quotes if present
    mml = args.strip()
    if mml.startswith('"') and mml.endswith('"'):
        mml = mml[1:-1]

    if not mml:
        return "❌ PLAY requires MML string\n"

    try:
        player = get_music_player()
        wav_data = player.parse_and_generate(mml, "square")

        # Store for later playback
        _interpreter.last_music_data = wav_data
        return f"🎵 Playing: {mml[:30]}{'...' if len(mml) > 30 else ''}\n"

    except Exception as e:  # pylint: disable=broad-except
        return f"❌ PLAY error: {e}\n"


def _basic_say(_interpreter: "Interpreter", args: str) -> str:
    """SAY text$ - Speak text using text-to-speech.

    Examples:
        SAY "Hello world"
        SAY message$
    """
    # pylint: disable=import-outside-toplevel
    from ..features.speech import get_synthesizer

    # Remove quotes if present
    text = args.strip()
    if text.startswith('"') and text.endswith('"'):
        text = text[1:-1]
    elif text.upper() in _interpreter.string_variables:
        text = _interpreter.string_variables[text.upper()]

    if not text:
        return "❌ SAY requires text\n"

    synth = get_synthesizer()
    return synth.say(text)


# ============================================================================
# Shape Commands
# ============================================================================


def _basic_shape(interpreter: "Interpreter", args: str, turtle: "TurtleState") -> str:
    """SHAPE name, size [, fill] - Draw a pre-built shape.

    Shapes: POLYGON, STAR, HEART, ARROW, SPIRAL, GEAR, CROSS, DIAMOND
    Examples:
        SHAPE STAR, 50         - 5-pointed star, size 50
        SHAPE POLYGON 6, 40    - Hexagon, size 40
        SHAPE HEART, 30, 1     - Filled heart, size 30
    """
    # pylint: disable=import-outside-toplevel
    from ..features.shapes import get_shape_library

    parts = [p.strip() for p in args.split(",")]
    if len(parts) < 2:
        return "❌ SHAPE requires name and size\n"

    shape_name = parts[0].upper()
    lib = get_shape_library()

    # Check for shape with parameter (e.g., "POLYGON 6")
    shape_parts = shape_name.split()
    param = None
    if len(shape_parts) > 1:
        shape_name = shape_parts[0]
        try:
            param = int(shape_parts[1])
        except ValueError:
            pass

    try:
        size = interpreter.evaluate_expression(parts[1])
        fill = len(parts) > 2 and parts[2].strip() not in ("0", "FALSE", "")

        if shape_name == "POLYGON":
            sides = param if param else 6
            return lib.draw_polygon(turtle, sides, size, fill)
        elif shape_name == "STAR":
            points = param if param else 5
            return lib.draw_star(turtle, points, size, fill=fill)
        elif shape_name == "HEART":
            return lib.draw_heart(turtle, size, fill)
        elif shape_name == "ARROW":
            return lib.draw_arrow(turtle, size, fill=fill)
        elif shape_name == "SPIRAL":
            turns = param if param else 3
            return lib.draw_spiral(turtle, turns, 5, size)
        elif shape_name == "GEAR":
            teeth = param if param else 12
            return lib.draw_gear(turtle, teeth, size, fill=fill)
        elif shape_name == "CROSS":
            return lib.draw_cross(turtle, size, fill=fill)
        elif shape_name == "DIAMOND":
            return lib.draw_diamond(turtle, size, fill=fill)
        else:
            shapes = ", ".join(lib.list_shapes())
            return f"❌ Unknown shape: {shape_name}. Available: {shapes}\n"

    except (ValueError, TypeError) as e:
        return f"❌ SHAPE error: {e}\n"


# ============================================================================
# Particle Commands
# ============================================================================


def _basic_particle(interpreter: "Interpreter", args: str) -> str:
    """PARTICLE effect, x, y [, intensity] - Create particle effect.

    Effects: EXPLOSION, FIRE, SMOKE, SPARKLE, RAIN, SNOW, CONFETTI, TRAIL
    Examples:
        PARTICLE EXPLOSION, 100, 100
        PARTICLE FIRE, playerX, playerY
        PARTICLE EXPLOSION, 100, 100, 2    - Double intensity
    """
    # pylint: disable=import-outside-toplevel
    from ..features.particles import get_particle_system

    parts = [p.strip() for p in args.split(",")]
    if len(parts) < 3:
        return "❌ PARTICLE requires effect, x, y\n"

    effect = parts[0].upper()

    try:
        x = interpreter.evaluate_expression(parts[1])
        y = interpreter.evaluate_expression(parts[2])
        intensity = 1.0
        if len(parts) > 3:
            intensity = interpreter.evaluate_expression(parts[3])

        psys = get_particle_system()
        psys.create_effect(effect, x, y, intensity=intensity)

        # Trigger one update to emit burst particles
        psys.update(1.0)

        return f"✨ Created {effect} effect at ({x}, {y})\n"

    except (ValueError, TypeError) as e:
        return f"❌ PARTICLE error: {e}\n"


def _basic_fractal(interpreter: "Interpreter", args: str, turtle: "TurtleState") -> str:
    """FRACTAL name [, iterations [, size]] - Draw L-System fractal.

    Available fractals: KOCH, SIERPINSKI, DRAGON, PLANT, TREE, HILBERT,
                        PEANO, GOSPER, LEVY, SQUARE, CRYSTAL, RINGS,
                        BUSH, SEAWEED, PENROSE

    Examples:
        FRACTAL KOCH           - Koch snowflake (default 4 iterations)
        FRACTAL DRAGON, 10     - Dragon curve with 10 iterations
        FRACTAL PLANT, 5, 8    - Plant with 5 iterations, step size 8
    """
    # pylint: disable=import-outside-toplevel
    from ..features.fractals import get_fractal_generator

    parts = [p.strip() for p in args.split(",")]
    if not parts or not parts[0]:
        # Show available fractals
        gen = get_fractal_generator()
        names = ", ".join(gen.get_preset_names())
        return f"ℹ️ Available fractals: {names}\n"

    name = parts[0].upper()

    try:
        iterations = 4
        step_size = 10.0

        if len(parts) > 1:
            iterations = int(interpreter.evaluate_expression(parts[1]))
        if len(parts) > 2:
            step_size = float(interpreter.evaluate_expression(parts[2]))

        # Clamp iterations to prevent runaway
        iterations = max(1, min(iterations, 12))

        gen = get_fractal_generator()
        return gen.draw_preset(turtle, name, iterations, step_size)

    except (ValueError, TypeError) as e:
        return f"❌ FRACTAL error: {e}\n"


def _basic_poke(interpreter: "Interpreter", args: str) -> str:
    """POKE address, value - Write value to memory address (simulated).

    Syntax: POKE <address>, <value>
    """
    # pylint: disable=import-outside-toplevel
    from ..logging_config import get_logger
    from ..utils.validators import ValidationError, validate_numeric

    logger = get_logger(__name__)

    if "," not in args:
        logger.error("POKE: Missing comma separator")
        return "❌ POKE requires: POKE address, value\n"

    try:
        # Parse address and value
        parts = [p.strip() for p in args.split(",", 1)]
        if len(parts) != 2:
            logger.error("POKE: Invalid format")
            return "❌ POKE requires: POKE address, value\n"

        addr_str = parts[0]
        value_str = parts[1]

        # Validate numeric values
        addr_str = validate_numeric(addr_str, "memory address")
        value_str = validate_numeric(value_str, "memory value")

        # Evaluate expressions
        address = int(interpreter.evaluate_expression(addr_str))
        value = int(interpreter.evaluate_expression(value_str))

        # Range checks
        if address < 0 or address > 65535:
            logger.error("POKE: Address out of range: %s", address)
            return "❌ POKE address must be 0-65535\n"

        if value < 0 or value > 255:
            logger.error("POKE: Value out of range: %s", value)
            return "❌ POKE value must be 0-255\n"

        # Store in simulated memory via hardware simulator
        success = interpreter.hardware.poke(address, value)
        if not success:
            return "❌ POKE failed\n"
        logger.debug("POKE: Wrote %s to address {address}", value)
        return ""

    except ValidationError as e:
        logger.error("POKE validation failed: %s", e)
        return f"❌ {e}\n"
    except (ValueError, TypeError, ZeroDivisionError) as e:
        logger.error("POKE evaluation error: %s", e)
        return f"❌ POKE error: {e}\n"


def _basic_peek(interpreter: "Interpreter", args: str) -> str:
    """PEEK(address) - Read value from memory address (simulated).

    Used as a function: X = PEEK(address)
    Returns the byte value at the given address.
    """
    # pylint: disable=import-outside-toplevel
    from ..logging_config import get_logger
    from ..utils.validators import ValidationError, validate_numeric

    logger = get_logger(__name__)

    try:
        # Validate numeric argument
        args = validate_numeric(args.strip(), "memory address")

        # Evaluate expression
        address = int(interpreter.evaluate_expression(args))

        # Range check
        if address < 0 or address > 65535:
            logger.error("PEEK: Address out of range: %s", address)
            return f"❌ PEEK address must be 0-65535: got {address}\n"

        # Retrieve from simulated memory via hardware simulator
        value = interpreter.hardware.peek(address)
        logger.debug("PEEK: Read %s from address {address}", value)

        # This is typically used in expressions like: X = PEEK(address)
        # The value needs to be stored in a return value
        # For now, return a success message (integration with evaluator needed)
        return f"ℹ️ PEEK({address}) = {value}\n"

    except ValidationError as e:
        logger.error("PEEK validation failed: %s", e)
        return f"❌ {e}\n"
    except (ValueError, TypeError, ZeroDivisionError) as e:
        logger.error("PEEK evaluation error: %s", e)
        return f"❌ PEEK error: {e}\n"


def _basic_out(interpreter: "Interpreter", args: str) -> str:
    """OUT port, value - Write value to port (simulated).

    Syntax: OUT <port>, <value>
    """
    # pylint: disable=import-outside-toplevel
    from ..logging_config import get_logger
    from ..utils.validators import ValidationError, validate_numeric

    logger = get_logger(__name__)

    if "," not in args:
        logger.error("OUT: Missing comma separator")
        return "❌ OUT requires: OUT port, value\n"

    try:
        # Parse port and value
        parts = [p.strip() for p in args.split(",", 1)]
        if len(parts) != 2:
            logger.error("OUT: Invalid format")
            return "❌ OUT requires: OUT port, value\n"

        port_str = parts[0]
        value_str = parts[1]

        # Validate numeric values
        port_str = validate_numeric(port_str, "port number")
        value_str = validate_numeric(value_str, "port value")

        # Evaluate expressions
        port = int(interpreter.evaluate_expression(port_str))
        value = int(interpreter.evaluate_expression(value_str))

        # Range checks
        if port < 0 or port > 65535:
            logger.error("OUT: Port out of range: %s", port)
            return "❌ OUT port must be 0-65535\n"

        if value < 0 or value > 255:
            logger.error("OUT: Value out of range: %s", value)
            return "❌ OUT value must be 0-255\n"

        # Store in simulated ports via hardware simulator
        success = interpreter.hardware.out(port, value)
        if not success:
            return "❌ OUT failed\n"
        logger.debug("OUT: Wrote %s to port {port}", value)
        return ""

    except ValidationError as e:
        logger.error("OUT validation failed: %s", e)
        return f"❌ {e}\n"
    except (ValueError, TypeError, ZeroDivisionError) as e:
        logger.error("OUT evaluation error: %s", e)
        return f"❌ OUT error: {e}\n"


def _basic_in(interpreter: "Interpreter", args: str) -> str:
    """IN(port) - Read value from port (simulated).

    Used as a function: X = IN(port)
    Returns the byte value from the given port.
    """
    # pylint: disable=import-outside-toplevel
    from ..logging_config import get_logger
    from ..utils.validators import ValidationError, validate_numeric

    logger = get_logger(__name__)

    try:
        # Validate numeric argument
        args = validate_numeric(args.strip(), "port number")

        # Evaluate expression
        port = int(interpreter.evaluate_expression(args))

        # Range check
        if port < 0 or port > 65535:
            logger.error("IN: Port out of range: %s", port)
            return f"❌ IN port must be 0-65535: got {port}\n"

        # Retrieve from simulated ports via hardware simulator
        value = interpreter.hardware.inp(port)
        logger.debug("IN: Read %s from port {port}", value)

        # This is typically used in expressions like: X = IN(port)
        return f"ℹ️ IN({port}) = {value}\n"

    except ValidationError as e:
        logger.error("IN validation failed: %s", e)
        return f"❌ {e}\n"
    except (ValueError, TypeError, ZeroDivisionError) as e:
        logger.error("IN evaluation error: %s", e)
        return f"❌ IN error: {e}\n"


def _basic_shell(_interpreter: "Interpreter", args: str) -> str:
    """SHELL command - Execute system shell command.

    Syntax: SHELL "command"

    Note: Disabled in IDE for security reasons. Returns informational message.
    """
    # pylint: disable=import-outside-toplevel
    from ..logging_config import get_logger

    logger = get_logger(__name__)

    if not args.strip():
        logger.error("SHELL: Missing command")
        return "❌ SHELL requires command string\n"

    # Extract command from quotes if present
    cmd = args.strip()
    if cmd.startswith('"') and cmd.endswith('"'):
        cmd = cmd[1:-1]

    # Check for empty command
    if not cmd:
        return "❌ SHELL requires a command string\n"

    # For security, log but don't execute in IDE
    logger.warning("SHELL command attempted (blocked for security): %s", cmd)
    return f"ℹ️ SHELL commands are disabled in IDE for security. Command: {cmd}\n"


def _basic_joyinit(_interpreter: "Interpreter") -> str:
    """JOYINIT - Initialize gamepad/joystick support.

    After calling JOYINIT, use STICK() and STRIG() functions:
        STICK(0) - X-axis of left stick (-1 to 1)
        STICK(1) - Y-axis of left stick (-1 to 1)
        STICK(2) - X-axis of right stick (-1 to 1)
        STICK(3) - Y-axis of right stick (-1 to 1)
        STRIG(n) - Button n state (0 or 1)
    """
    # pylint: disable=import-outside-toplevel
    from ..features.gamepad import get_gamepad_manager

    try:
        manager = get_gamepad_manager()
        if manager.available:
            manager.start()
            count = len(manager.gamepads)
            if count > 0:
                return f"🎮 Gamepad initialized ({count} device(s) found)\n"
            return f"🎮 Gamepad system ready (backend: {manager.backend_name})\n"
        return "❌ No gamepad backend available (install pygame or inputs)\n"
    except (ImportError, RuntimeError) as e:
        return f"❌ JOYINIT error: {e}\n"
