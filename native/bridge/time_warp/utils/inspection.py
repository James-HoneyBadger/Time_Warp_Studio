"""Turns a finished run into the two tables the IDE shows.

The IDE has a Variables tab and a Telemetry HUD, and both are the *engine's*
answer rather than the panel's guess: the value rendered here is the value the
engine holds, and the counters here are the engine's own counters
(`Interpreter.get_telemetry_snapshot`). A C++ implementation of "what is this
variable worth" would be a second answer to a question the engine has already
answered, and the one that drifts would be the one the learner reads.

Rows rather than a schema, because both panels are tables: the shape that crosses
the bridge is the shape that gets displayed, so adding a counter is one entry in a
list here and nothing anywhere else.

Pure functions over plain data -- no interpreter, no turtle, no I/O -- so the
tests can describe a snapshot without running a program.
"""

from __future__ import annotations

from typing import Any, Dict, List, Mapping, Optional

#: The longest a single value is rendered before it is cut short. A learner who
#: wants all of a 10,000-character string has the program's own output.
MAX_VALUE_CHARS = 60

#: The most rows a table gets. A BASIC program with 5,000 live strings would
#: otherwise build a panel nobody can read and a JSON line nobody wants.
MAX_ROWS = 200

#: What a value's type is called in the panel. The engine's Python types are not
#: the language's types -- BASIC numbers *are* floats -- so the names here are the
#: words a learner of these languages would use.
_TYPE_NAMES = {
    "int": "number",
    "float": "number",
    "str": "text",
    "bool": "boolean",
    "list": "list",
    "tuple": "list",
    "dict": "record",
    "set": "set",
}


def type_name(value: Any) -> str:
    """The word the panel uses for *value*'s type."""
    if value is None:
        return "empty"
    return _TYPE_NAMES.get(type(value).__name__, type(value).__name__)


def render_value(value: Any) -> str:
    """Renders *value* the way the panel shows it.

    Deliberately *not* `repr` everywhere: `42.0` for the number a BASIC program
    stored as `42` is technically honest and pedagogically wrong -- the learner
    wrote `42`, and the engine's float is an implementation detail of the
    interpreter, not of their program.
    """
    if value is None:
        return "empty"
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, float):
        if value.is_integer() and abs(value) < 1e15:
            return str(int(value))
        return f"{value:g}"
    if isinstance(value, str):
        # Quotes only when there is something to disambiguate: an empty string is
        # otherwise a blank cell, which reads as a bug rather than as "".
        text = value if value else '""'
        return _elide(text)
    if isinstance(value, (list, tuple, dict, set)):
        return _elide(repr(value))
    return _elide(str(value))


def _elide(text: str) -> str:
    single = " ".join(text.splitlines())
    if len(single) <= MAX_VALUE_CHARS:
        return single
    return single[: MAX_VALUE_CHARS - 1] + "\u2026"


def variable_rows(variables: Mapping[str, Any]) -> List[Dict[str, str]]:
    """The variables table: name, type and value, sorted, capped.

    Sorted case-insensitively because `a` and `B` in one list is a list a reader
    has to search; the engine's own dict order is a statement about its
    hashing, not about the program.
    """
    rows = [
        {"name": str(name), "type": type_name(value), "value": render_value(value)}
        for name, value in variables.items()
    ]
    rows.sort(key=lambda row: row["name"].lower())
    if len(rows) > MAX_ROWS:
        hidden = len(rows) - MAX_ROWS
        rows = rows[:MAX_ROWS]
        rows.append(
            {
                "name": f"(and {hidden} more)",
                "type": "",
                "value": "raise the limit? see MAX_ROWS in utils/inspection.py",
            }
        )
    return rows


def telemetry_rows(
    snapshot: Mapping[str, Any],
    *,
    output_lines: Optional[int] = None,
    drawing_commands: Optional[int] = None,
) -> List[Dict[str, str]]:
    """The telemetry HUD: labelled values, in the order a reader wants them.

    The two optional counts come from the worker rather than the snapshot,
    because they count what was *sent to the IDE* -- the output lines that
    reached the console and the drawing commands that reached the canvas -- which
    the engine has no reason to know.
    """
    turtle = snapshot.get("turtle") or {}
    rows: List[Dict[str, str]] = [
        {"label": "Language", "value": str(snapshot.get("language", "?"))},
        {
            "label": "Steps",
            "value": f"{_number(snapshot.get('steps_used'))} of "
            f"{_number(snapshot.get('max_steps'))}",
        },
        {"label": "Engine time", "value": f"{_number(snapshot.get('elapsed_ms'))} ms"},
        {
            "label": "Steps per second",
            "value": _number(snapshot.get("steps_per_sec")),
        },
        {"label": "Last line", "value": _number(snapshot.get("current_line"))},
        {
            "label": "Recursion depth",
            "value": _number(snapshot.get("recursion_depth")),
        },
        {"label": "Call stack", "value": _number(_length(snapshot.get("call_stack")))},
        {"label": "Variables", "value": _number(_length(snapshot.get("variables")))},
        {"label": "Turtle", "value": _turtle_pose(turtle)},
        {"label": "Pen", "value": "down" if turtle.get("pen_down", True) else "up"},
        {
            "label": "Ink",
            "value": f"{_number(turtle.get('lines'))} segments, "
            f"{_number(turtle.get('shapes'))} shapes",
        },
    ]

    if output_lines is not None:
        rows.append({"label": "Output lines", "value": _number(output_lines)})
    if drawing_commands is not None:
        rows.append({"label": "Drawing commands", "value": _number(drawing_commands)})

    # BASIC has a byte-addressable memory window; no other language does, so the
    # row appears only where it means something -- the snapshot builds the list
    # unconditionally, so its presence is not a signal, the language is.
    ram = snapshot.get("basic_ram_window")
    if ram and str(snapshot.get("language", "")).upper().startswith("BASIC"):
        rows.append({"label": "Memory (first 16 bytes)", "value": _hex_bytes(ram)})

    return rows


def _turtle_pose(turtle: Mapping[str, Any]) -> str:
    if not turtle:
        return "no turtle"
    return (
        f"x {_number(turtle.get('x'))}, y {_number(turtle.get('y'))}, "
        f"heading {_number(turtle.get('heading'))}\u00b0"
        f"{'' if turtle.get('visible', True) else ', hidden'}"
    )


def _hex_bytes(values: Any) -> str:
    try:
        return " ".join(f"{int(value) & 0xFF:02X}" for value in values)
    except (TypeError, ValueError):
        return "?"


def _number(value: Any) -> str:
    """Formats a counter: thousands separated, no trailing `.0`."""
    if value is None:
        return "?"
    if isinstance(value, bool):
        return "true" if value else "false"
    try:
        number = float(value)
    except (TypeError, ValueError):
        return str(value)
    if number.is_integer():
        return f"{int(number):,}"
    return f"{number:,.1f}"


def _length(value: Any) -> Optional[int]:
    try:
        return len(value)  # type: ignore[arg-type]
    except TypeError:
        return None
