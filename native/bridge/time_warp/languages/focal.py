"""FOCAL-69 language executor for Time Warp Studio.

FOCAL — FOrmula CALculator — is DEC's line-numbered teaching language from 1969.
It was built to be taught in an afternoon: ten commands, one number type, and line
labels of the form ``NN.NN`` so that a group of five lines can be named as a unit.

Executed line by line, like BASIC and COMAL.

Statements — the historic single-letter form and the full name both work:

    ASK / A      prompt for a value
    TYPE / T     print text, formats and expressions
    SET / S      assign
    IF / I       three-way branch on the sign of an expression
    FOR / F      loop over the rest of the line
    DO / D       run a group, returning when the program runs off the end
    GOTO / G     jump to a group
    COMMENT / C  the rest of the line is text
    ERASE / E    forget variables
    QUIT / Q     stop

Turtle graphics are a **Time Warp extension**, as they are for COMAL and REXX:
1969 FOCAL had no graphics at all. ``FORWARD``/``FD``, ``BACK``/``BK``,
``RIGHT``/``RT``, ``LEFT``/``LT``, ``PENUP``/``PU``, ``PENDOWN``/``PD``, ``HOME``.

Three details are faithful on purpose, because a teaching language that quietly
differs from the one in the book teaches the wrong thing:

  * **Branching is by sign, not by comparison.** FOCAL has no relational
    operators. ``IF (X) 1.1,2.1,3.1`` goes to 1.1 when X is negative, 2.1 when it
    is zero and 3.1 when it is positive. An omitted target falls through to the
    next line, so ``IF (X) ,,3.1`` reads "if X is positive, go to 3.1".
  * **``DO`` returns at the end of the program.** There is no RETURN statement in
    FOCAL-69; a group called with ``DO`` is expected to end where the program
    ends. A nested ``DO`` is bounded by a depth limit so a cycle reports itself
    instead of hanging.
  * **Undefined variables are 0.** FOCAL zeroes every variable at load. This is
    worth stating because the BASIC executor does the opposite, and echoing a
    name back is the failure mode that makes a typo look like a result.

One thing is *not* a reproduction, and is the only place this implementation
chose rather than transcribed: FOCAL's exact output field widths. ``%n`` here
means "n decimal places" and the default is 2, which is a chosen default rather
than whatever a PDP-8 printed.

Two further choices are worth knowing, because both could reasonably go the other
way:

  * **After a loop the control variable keeps the last value the body saw**, so
    ``FOR I=1,4`` leaves I at 4 rather than at the 5 that failed the test. That is
    the less surprising of the two for a learner reading the program back.
  * **``!`` suppresses the newline inside one TYPE, not across statements.** The
    engine logs each statement's output separately, so the newline between two
    lines is added by the logger rather than by the statement. ``T "A"!"B"``
    prints ``AB``; ``T "A"!`` followed by ``T "B"`` cannot, and says so here
    rather than looking like it should have worked.
  * **An unrecognised command is an error, not a comment.** Some FOCAL listings
    are annotated by writing text after the line number; here that is what
    ``C``/``COMMENT`` is for, and ``TIPE`` reports itself instead of printing
    nothing. Silence is how a typo passes for a working program.
"""

from __future__ import annotations

import math
import random
import re
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


class FocalError(Exception):
    """Raised while evaluating a FOCAL line; reported with the ❌ prefix."""


#: Every command the executor dispatches on, for the command inventory.
FOCAL_COMMANDS = {
    "ASK",
    "TYPE",
    "SET",
    "IF",
    "FOR",
    "DO",
    "GOTO",
    "COMMENT",
    "ERASE",
    "QUIT",
    "FORWARD",
    "BACK",
    "RIGHT",
    "LEFT",
    "PENUP",
    "PENDOWN",
    "HOME",
}

#: Letter and word forms of each command. The single letters are FOCAL's own
#: abbreviations, not an addition.
_ALIASES: dict[str, str] = {
    "A": "ASK",
    "ASK": "ASK",
    "T": "TYPE",
    "TYPE": "TYPE",
    "S": "SET",
    "SET": "SET",
    "I": "IF",
    "IF": "IF",
    "F": "FOR",
    "FOR": "FOR",
    "D": "DO",
    "DO": "DO",
    "G": "GOTO",
    "GOTO": "GOTO",
    "C": "COMMENT",
    "COMMENT": "COMMENT",
    "E": "ERASE",
    "ERASE": "ERASE",
    "Q": "QUIT",
    "QUIT": "QUIT",
}

#: Turtle words. Kept separate from the aliases because ``F`` is FOR and
#: ``FORWARD`` starts with it: a prefix match would read ``FORWARD`` as a loop.
_TURTLE: dict[str, str] = {
    "FORWARD": "forward",
    "FD": "forward",
    "BACK": "back",
    "BK": "back",
    "RIGHT": "right",
    "RT": "right",
    "LEFT": "left",
    "LT": "left",
    "PENUP": "penup",
    "PU": "penup",
    "PENDOWN": "pendown",
    "PD": "pendown",
    "HOME": "home",
}

_LABEL_RE = re.compile(r"^\s*(\d{1,2}\.\d{1,2})\s*(.*)$")
_TOKEN_RE = re.compile(
    r"\s*(?:"
    r"(?P<number>\d+(?:\.\d*)?(?:[Ee][+-]?\d+)?|\.\d+(?:[Ee][+-]?\d+)?)"
    r"|(?P<name>[A-Za-z][A-Za-z0-9_]*)"
    r"|(?P<op>[+\-*/^(),])"
    r")"
)

#: Functions FOCAL writes without parentheses.
_NO_ARGUMENT_FUNCTIONS = frozenset({"FPI", "FRAN"})

#: FOCAL's function set. Single argument each, except FPI and FRAN.
_FUNCTIONS: dict[str, Any] = {
    "FABS": abs,
    "FSQT": math.sqrt,
    "FEXP": math.exp,
    "FLOG": math.log,
    "FSIN": math.sin,
    "FCOS": math.cos,
    "FATN": math.atan,
    "FITR": math.trunc,
    "FRAN": lambda: random.random(),
    "FSGN": lambda value: (value > 0) - (value < 0),
    "FPI": lambda: math.pi,
}

#: Guards against a FOCAL program that cannot terminate. The engine's own
#: iteration cap covers the outer loop; a ``DO`` runs inside one executor call,
#: so it needs its own.
_MAX_STEPS = 1_000_000
_MAX_DO_DEPTH = 64


# ---------------------------------------------------------------------------
# Per-run state
# ---------------------------------------------------------------------------


def _state(interp: "Interpreter") -> dict[str, Any]:
    """Executor state for this run, hung off the interpreter.

    A fresh ``Interpreter`` is built per run, so this needs no generation check:
    a new program cannot inherit the previous one's labels or return stack.
    """
    state = interp.__dict__.get("_focal_state")
    if state is None:
        state = {
            "labels": {},
            "fmt": 2,
            "do_depth": 0,
            "steps": 0,
        }
        interp.__dict__["_focal_state"] = state
    return state


def _code_of(interp: "Interpreter", index: int) -> str:
    """The source text of line *index*.

    ``program_lines`` holds ``(line_number, text)`` pairs, and FOCAL's ``01.10``
    is not an integer, so the label is still part of the text here.
    """
    lines = interp.program_lines
    if 0 <= index < len(lines):
        return lines[index][1]
    return ""


def _label_key(label: str) -> tuple[int, int]:
    """A label as its (group, member) numbers.

    FOCAL's label is two numbers with a dot between them, not a decimal, so
    ``01.10`` and ``1.10`` name the same line while ``1.01`` names a different
    one. Comparing the text would make every reference as fussy as its spelling.
    """
    group, _, member = label.partition(".")
    return (int(group), int(member))


def _build_labels(interp: "Interpreter", state: dict[str, Any]) -> None:
    """Index every labelled line so GOTO and DO can resolve a target.

    Built once, lazily, from the program text rather than from the interpreter's
    line numbers — see ``_code_of``.
    """
    for index, (_number, text) in enumerate(interp.program_lines):
        match = _LABEL_RE.match(text or "")
        if match:
            state["labels"][_label_key(match.group(1))] = index


def _resolve(state: dict[str, Any], label: str) -> int:
    target = state["labels"].get(_label_key(label))
    if target is None:
        raise FocalError(f"no line labelled {label}")
    return target


# ---------------------------------------------------------------------------
# Expressions
# ---------------------------------------------------------------------------


def _tokenize(text: str) -> list[tuple[str, str]]:
    tokens: list[tuple[str, str]] = []
    position = 0
    while position < len(text):
        match = _TOKEN_RE.match(text, position)
        if match is None:
            if text[position].isspace():
                position += 1
                continue
            raise FocalError(f"unexpected character {text[position]!r}")
        kind = match.lastgroup or ""
        tokens.append((kind, match.group(kind)))
        position = match.end()
    return tokens


class _Parser:
    """Recursive descent over a token list, with FOCAL's precedence rules."""

    def __init__(self, tokens: list[tuple[str, str]], variables: dict[str, float]):
        self.tokens = tokens
        self.index = 0
        self.variables = variables

    # -- token helpers ----------------------------------------------------
    def _peek(self) -> tuple[str, str] | None:
        return self.tokens[self.index] if self.index < len(self.tokens) else None

    def _take(self) -> tuple[str, str]:
        token = self._peek()
        if token is None:
            raise FocalError("expression ended early")
        self.index += 1
        return token

    def _accept(self, value: str) -> bool:
        token = self._peek()
        if token is not None and token[1] == value:
            self.index += 1
            return True
        return False

    # -- grammar ----------------------------------------------------------
    def expression(self) -> float:
        value = self.term()
        while True:
            token = self._peek()
            if token is None or token[1] not in ("+", "-"):
                return value
            self.index += 1
            right = self.term()
            value = value + right if token[1] == "+" else value - right

    def term(self) -> float:
        value = self.power()
        while True:
            token = self._peek()
            if token is None or token[1] not in ("*", "/"):
                return value
            self.index += 1
            right = self.power()
            if token[1] == "*":
                value *= right
            else:
                if right == 0:
                    raise FocalError("division by zero")
                value /= right

    def power(self) -> float:
        value = self.unary()
        if self._accept("^"):
            # Right-associative, so 2^3^2 is 2^(3^2).
            return value ** self.power()
        return value

    def unary(self) -> float:
        if self._accept("-"):
            return -self.unary()
        if self._accept("+"):
            return self.unary()
        return self.primary()

    def primary(self) -> float:
        kind, text = self._take()
        if kind == "number":
            return float(text)
        if kind == "op" and text == "(":
            value = self.expression()
            if not self._accept(")"):
                raise FocalError("missing )")
            return value
        if kind == "name":
            name = text.upper()
            if self._accept("("):
                arguments: list[float] = []
                if not self._accept(")"):
                    arguments.append(self.expression())
                    while self._accept(","):
                        arguments.append(self.expression())
                    if not self._accept(")"):
                        raise FocalError("missing )")
                return self._call(name, arguments)
            if name in _NO_ARGUMENT_FUNCTIONS:
                # FOCAL writes these without parentheses: `T FPI`, not `T FPI()`.
                # Requiring the parentheses made the documented form print 0.00,
                # because the name then fell through to the variable lookup.
                return self._call(name, [])
            if name in _FUNCTIONS:
                raise FocalError(f"{name} needs an argument, as in {name}(X)")
            # A variable that has never been assigned is zero, as FOCAL's loader
            # leaves it. Returning the name instead would put "X" on the screen
            # where a number belongs.
            raw = self.variables.get(name, 0.0)
            try:
                return float(raw)
            except (TypeError, ValueError) as exc:
                raise FocalError(f"{name} is not a number") from exc
        raise FocalError(f"unexpected {text!r} in expression")

    def _call(self, name: str, arguments: list[float]) -> float:
        handler = _FUNCTIONS.get(name)
        if handler is None:
            raise FocalError(f"unknown function {name}")
        if name in _NO_ARGUMENT_FUNCTIONS:
            if not arguments:
                return float(handler())
            if name == "FRAN" and len(arguments) == 1:
                # FRAN takes an optional seed, as it does in FOCAL.
                random.seed(int(arguments[0]))
                return float(handler())
            raise FocalError(f"{name} takes no argument")
        if len(arguments) != 1:
            raise FocalError(f"{name} takes one argument")
        try:
            return float(handler(arguments[0]))
        except ValueError as exc:
            raise FocalError(f"{name}: {exc}") from exc


def _evaluate(text: str, variables: dict[str, float]) -> float:
    parser = _Parser(_tokenize(text), variables)
    value = parser.expression()
    if parser.index != len(parser.tokens):
        raise FocalError(f"trailing text in expression: {text.strip()!r}")
    return value


def _format_number(value: float, digits: int) -> str:
    """Print with *digits* decimals. See the module docstring on this choice."""
    if digits <= 0:
        return f"{value:.0f}"
    return f"{value:.{digits}f}"


# ---------------------------------------------------------------------------
# Output helpers
# ---------------------------------------------------------------------------


def _split_top_level(text: str, separator: str = ",") -> list[str]:
    """Split on *separator* outside quotes and outside parentheses."""
    parts: list[str] = []
    current: list[str] = []
    depth = 0
    in_string = False
    for character in text:
        if character == '"':
            in_string = not in_string
        elif not in_string:
            if character == "(":
                depth += 1
            elif character == ")":
                depth -= 1
            elif character == separator and depth == 0:
                parts.append("".join(current))
                current = []
                continue
        current.append(character)
    parts.append("".join(current))
    return parts


def _type_statement(
    arguments: str,
    variables: dict[str, float],
    state: dict[str, Any],
) -> str:
    """TYPE: literal text, ``%n`` formats, ``!`` for no newline, expressions."""
    newline = True
    pieces: list[str] = []
    for item in _split_top_level(arguments):
        # Split into literals and expressions *before* deciding what each part
        # is, so a format or a "!" glued to a string is still seen: T "A"! is a
        # literal plus a newline suppression, not the text A"!.
        for piece in _split_literals(item.strip()):
            piece = piece.strip()
            if not piece:
                continue
            if piece.startswith('"'):
                pieces.append(_unquote(piece))
                continue
            if piece == "!":
                newline = False
                continue
            # %n sets the field for what follows, and prints nothing itself.
            format_match = re.fullmatch(r"%(\d)?", piece)
            if format_match:
                state["fmt"] = int(format_match.group(1) or 0)
                continue
            _reject_unquoted_word(piece, variables)
            pieces.append(_format_number(_evaluate(piece, variables), state["fmt"]))
    return "".join(pieces) + ("\n" if newline else "")


def _reject_unquoted_word(piece: str, variables: dict[str, float]) -> None:
    """Refuse TYPE HELLO where TYPE "HELLO" was meant.

    FOCAL zeroes every variable at load, so an unassigned name legitimately
    prints 0 — and that is exactly what makes a forgotten pair of quotes look
    like a working program. One-letter names are left alone, because in FOCAL
    they are the variables a program really uses; a longer name that has never
    been assigned is a typo far more often than it is a value.
    """
    word = piece.strip()
    if not re.fullmatch(r"[A-Za-z][A-Za-z0-9_]*", word):
        return
    if len(word) == 1 or word.upper() in variables or word.upper() in _FUNCTIONS:
        return
    raise FocalError(f"{word} is not a variable; text needs quotes")


def _split_literals(item: str) -> list[str]:
    """Split ``"text"X`` into a literal and an expression."""
    pieces: list[str] = []
    current: list[str] = []
    in_string = False
    for character in item:
        if character == '"':
            if in_string:
                current.append(character)
                pieces.append("".join(current))
                current = []
                in_string = False
                continue
            if current and "".join(current).strip():
                pieces.append("".join(current))
            current = [character]
            in_string = True
            continue
        current.append(character)
    if current and "".join(current).strip():
        pieces.append("".join(current))
    return pieces


def _unquote(text: str) -> str:
    text = text.strip()
    if text.startswith('"') and text.endswith('"') and len(text) >= 2:
        return text[1:-1]
    if text.startswith('"'):
        return text[1:]
    return text


# ---------------------------------------------------------------------------
# Statements
# ---------------------------------------------------------------------------


def _statement(
    interp: "Interpreter",
    turtle: "TurtleState",
    text: str,
    state: dict[str, Any],
) -> tuple[int | None, str]:
    """Run one statement.

    Returns ``(jump_target_or_None, output)``. The target is an absolute line
    index, which keeps a GOTO inside a ``DO`` group inside that group.
    """
    stripped = text.strip()
    # Stripped here rather than by the caller because a DO group reaches this
    # function directly, and a labelled line inside a group would otherwise be
    # read as a command called "02.10".
    match = _LABEL_RE.match(stripped)
    if match:
        stripped = match.group(2).strip()
    if not stripped:
        return None, ""

    head, _, rest = stripped.partition(" ")
    word = head.upper()

    # A turtle word before the alias table: "FORWARD" shares a first letter with
    # FOR but is not a loop.
    if word in _TURTLE:
        return None, _turtle_statement(turtle, _TURTLE[word], rest, interp.variables)

    command = _ALIASES.get(word)
    if command is None:
        raise FocalError(f"unknown command {word!r}")

    if command == "TYPE":
        return None, _type_statement(rest, interp.variables, state)
    if command == "COMMENT":
        return None, ""
    if command == "SET":
        return None, _set_statement(interp, rest, state)
    if command == "ASK":
        return None, _ask_statement(interp, rest, state)
    if command == "IF":
        return _if_statement(interp, rest, state), ""
    if command == "FOR":
        return None, _for_statement(interp, turtle, rest, state)
    if command == "GOTO":
        return _resolve(state, _label_of(rest)), ""
    if command == "DO":
        return None, _do_statement(interp, turtle, rest, state)
    if command == "ERASE":
        return None, _erase_statement(interp, rest)
    if command == "QUIT":
        interp.running = False
        return None, ""
    raise FocalError(f"unhandled command {command!r}")  # pragma: no cover


def _label_of(text: str) -> str:
    match = re.search(r"\d{1,2}\.\d{1,2}", text)
    if match is None:
        raise FocalError(f"expected a line label (NN.NN) in {text.strip()!r}")
    return match.group(0)


def _set_statement(
    interp: "Interpreter", arguments: str, state: dict[str, Any]
) -> str:
    """SET assigns; several assignments may share a line, comma-separated."""
    for assignment in _split_top_level(arguments):
        assignment = assignment.strip()
        if not assignment:
            continue
        name, separator, expression = assignment.partition("=")
        if not separator:
            raise FocalError(f"SET needs NAME=value, got {assignment!r}")
        key = name.strip().upper()
        if not re.fullmatch(r"[A-Za-z][A-Za-z0-9_]*", key):
            raise FocalError(f"bad variable name {name.strip()!r}")
        value = _evaluate(expression, interp.variables)
        interp.variables[key] = value
        interp.variables[key] = value
    return ""


def _ask_statement(interp: "Interpreter", arguments: str, state: dict[str, Any]) -> str:
    """ASK prompts, then returns; the answer arrives on the next call.

    The engine resumes at the line *after* this one and stores the reply in the
    named variable, which is why nothing is echoed here.
    """
    prompt = ""
    rest = arguments.strip()
    if rest.startswith('"'):
        end = rest.find('"', 1)
        if end == -1:
            raise FocalError("unterminated prompt")
        prompt = rest[1:end]
        rest = rest[end + 1 :]
    # A comma is accepted before the name because every other FOCAL statement
    # separates its items with one, so `ASK "Number? ", N` is what the habit
    # produces. Without this it was an error naming the comma.
    name = rest.strip().lstrip(",").strip().upper()
    if not re.fullmatch(r"[A-Za-z][A-Za-z0-9_]*", name):
        raise FocalError(f"ASK needs a variable name, got {rest.strip()!r}")
    # The answer is written into interp.variables by provide_input(), which is
    # why nothing is stored here: a private dict held the prompt's answer apart
    # from the variable the program then read, and the program printed 0.00.
    interp.variables.setdefault(name, 0.0)
    interp.start_input_request(prompt, name, True)
    return ""


def _if_statement(interp: "Interpreter", arguments: str, state: dict[str, Any]) -> int | None:
    """IF (expr) negative,zero,positive — the sign form, not a comparison."""
    open_at = arguments.find("(")
    close_at = arguments.rfind(")")
    if open_at == -1 or close_at < open_at:
        raise FocalError("IF needs a parenthesised expression, e.g. IF (X) 1.1,2.1,3.1")
    value = _evaluate(arguments[open_at + 1 : close_at], interp.variables)
    targets = [item.strip() for item in _split_top_level(arguments[close_at + 1 :])]
    if len(targets) > 3:
        raise FocalError("IF takes at most three targets")
    if value < 0:
        chosen = targets[0] if targets else ""
    elif value == 0:
        chosen = targets[1] if len(targets) > 1 else ""
    else:
        chosen = targets[2] if len(targets) > 2 else ""
    if not chosen:
        return None
    return _resolve(state, _label_of(chosen))


def _for_statement(
    interp: "Interpreter",
    turtle: "TurtleState",
    arguments: str,
    state: dict[str, Any],
) -> str:
    """FOR V=a,b,c; body — the body is the rest of the same line.

    One value runs once; two run from the first to the second in steps of one;
    three run from the first towards the third in steps of the second.
    """
    name, separator, remainder = arguments.partition("=")
    if not separator:
        raise FocalError("FOR needs V=values; body")
    variable = name.strip().upper()
    if not re.fullmatch(r"[A-Za-z][A-Za-z0-9_]*", variable):
        raise FocalError(f"bad FOR variable {name.strip()!r}")

    head, separator, body = remainder.partition(";")
    if not separator:
        raise FocalError("FOR needs a ; before its body")
    # The body is the rest of the line, and `;` separates statements within it:
    # `FOR I=1,4;FD 50;RT 90` is four sides of a square, not a syntax error.
    body_parts = [part for part in _split_top_level(body, ";") if part.strip()]
    if not body_parts:
        raise FocalError("FOR needs a statement after its ;")
    numbers = [
        _evaluate(part, interp.variables)
        for part in _split_top_level(head)
        if part.strip()
    ]
    if not numbers:
        raise FocalError("FOR needs at least one value")

    if len(numbers) == 1:
        start, step, limit = numbers[0], 1.0, None
    elif len(numbers) == 2:
        start, step, limit = numbers[0], 1.0, numbers[1]
    else:
        start, step, limit = numbers[0], numbers[1], numbers[2]
    if step == 0:
        raise FocalError("FOR step cannot be zero")

    output: list[str] = []
    value = start
    while True:
        state["steps"] += 1
        if state["steps"] > _MAX_STEPS:
            raise FocalError("FOR loop exceeded the step limit")
        interp.variables[variable] = value
        for part in body_parts:
            target, text = _statement(interp, turtle, part, state)
            output.append(text)
            if target is not None:
                raise FocalError("GOTO inside a FOR body is not supported")
            if not interp.running:
                break
        if not interp.running:
            break
        if limit is None:
            break
        value += step
        if step > 0 and value > limit:
            break
        if step < 0 and value < limit:
            break
    return "".join(output)


def _do_statement(
    interp: "Interpreter",
    turtle: "TurtleState",
    arguments: str,
    state: dict[str, Any],
) -> str:
    """DO runs a group from its label to the end of the program, then returns."""
    if state["do_depth"] >= _MAX_DO_DEPTH:
        raise FocalError(f"DO nested more than {_MAX_DO_DEPTH} deep")
    start = _resolve(state, _label_of(arguments))
    state["do_depth"] += 1
    try:
        return _run_group(interp, turtle, start, state)
    finally:
        state["do_depth"] -= 1


def _run_group(
    interp: "Interpreter",
    turtle: "TurtleState",
    start: int,
    state: dict[str, Any],
) -> str:
    """Run from *start* until the program ends or QUIT.

    This is FOCAL's return rule: a group called with DO is not bounded by a
    RETURN statement but by the end of the stored program.
    """
    output: list[str] = []
    index = start
    total = len(interp.program_lines)
    while index < total and interp.running:
        state["steps"] += 1
        if state["steps"] > _MAX_STEPS:
            raise FocalError("execution exceeded the step limit")
        target, text = _statement(interp, turtle, _code_of(interp, index), state)
        output.append(text)
        index = target if target is not None else index + 1
    return "".join(output)


def _erase_statement(interp: "Interpreter", arguments: str) -> str:
    """ERASE ALL, or ERASE followed by the variables to forget."""
    words = [word.strip().upper() for word in arguments.replace(",", " ").split()]
    if not words:
        raise FocalError("ERASE needs ALL or a list of variables")
    if words[0] == "ALL":
        interp.variables.clear()
        return ""
    for word in words:
        interp.variables.pop(word, None)
    return ""


def _turtle_statement(
    turtle: "TurtleState",
    action: str,
    rest: str,
    variables: dict[str, float],
) -> str:
    """Turtle extension: FORWARD/FD, BACK/BK, RIGHT/RT, LEFT/LT and pen control.

    The argument is an expression, so ``FORWARD 2*50`` and ``FORWARD D`` both
    work. With no argument the turtle does nothing and says so, rather than
    treating the empty string as zero and reporting a success it did not have.

    Movement prints nothing, as it does in Logo, COMAL and REXX: a program that
    draws a thousand segments would otherwise put a thousand lines in the
    console, and the drawing is the output.
    """
    if action == "home":
        turtle.home()
        return ""
    if action in ("penup", "pendown"):
        if action == "penup":
            turtle.penup()
        else:
            turtle.pendown()
        return ""
    text = rest.strip()
    if not text:
        raise FocalError(f"{action.upper()} needs a distance or angle")
    value = _evaluate(text, variables)
    if action == "forward":
        turtle.forward(value)
    elif action == "back":
        turtle.back(value)
    elif action == "right":
        turtle.right(value)
    else:
        turtle.left(value)
    return ""


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def execute_focal(
    interpreter: "Interpreter",
    source: str,
    turtle: "TurtleState",
) -> str:
    """Execute one line of FOCAL source and return the text to display."""
    state = _state(interpreter)
    if not state["labels"]:
        _build_labels(interpreter, state)

    try:
        target, output = _statement(interpreter, turtle, source or "", state)
    except FocalError as exc:
        return f"❌ FOCAL: {exc}\n"
    except (ValueError, TypeError, OverflowError, ArithmeticError) as exc:
        # Arithmetic on a value the parser accepted but the maths rejects, e.g.
        # FSQT of a negative number, must not escape as a raw Python error.
        return f"❌ FOCAL: {exc}\n"

    if target is not None:
        interpreter.current_line = target
    return output
