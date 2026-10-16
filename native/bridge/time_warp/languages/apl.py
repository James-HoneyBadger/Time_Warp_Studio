"""APL language executor for Time Warp Studio.

APL (A Programming Language, Iverson, 1962; the interactive APL\\360, 1966) is an
array language with two features that make it unlike anything else here: **there
is no operator precedence**, and most functions work on whole arrays rather than
on single numbers. ``+/⍳10`` is 55. That is the value of having it in the lineup:
it is the one language in the studio where the *shape* of the data is the program.

Executed line by line. Each line is an expression, and its value is displayed —
APL's original interface was exactly this, and it is what makes the language feel
like a calculator you can write a program with.

    ⍳5            1 2 3 4 5
    2 3 ⍴ ⍳6      a 2-by-3 table
    +/⍳100        5050
    A ← ⍳4        5 line, silent; A is now 1 2 3 4
    1 2 3 + 10    11 12 13  (the left argument is the whole strand)
    ∇ R ← DOUBLE X
      R ← X × 2
    ∇
    DOUBLE 21     42

Two spellings for every function. The glyphs are APL's own and are what a book
shows; the keyword forms (``IOTA``, ``RHO``, ``TIMES``, ``REDUCE`` …) are accepted
so a keyboard that cannot produce ``⍳`` is not a keyboard that cannot write APL.

Values are numbers, characters, or nested arrays. ``¯`` is negative (``¯3``), ``*``
is power, and ``×`` is times — APL is old enough that ``*`` meant exponentiation
before C made it a multiplication sign.

Turtle graphics are a **Time Warp extension**, as they are for COMAL, REXX and
FOCAL: ``FD``/``FORWARD``, ``BK``, ``RT``, ``LT``, ``PU``, ``PD``, ``HOME``,
``CS``, and the read-only ``TX``, ``TY`` and ``THETA``. Those names are two
letters on purpose: ``X``, ``Y`` and ``H`` would be ordinary APL variables, and
taking them from the program to report the turtle's position would break the most
natural line anyone writes.

The subset is deliberately bounded, and says so rather than failing obscurely:

  * **Scalar functions are pervasive** — ``1 2 3 + 10`` is ``11 12 13`` — and the
    scalar/structural split is the real APL one.
  * **Operators are ``/`` reduce and ``\\`` scan.** APL's operator *axes* (``+/[2]``)
    are not implemented, so reduce and scan work along the outermost axis.
  * **``∘.`` outer product, ``⍤`` rank, ``⍤``/``⍣`` composition and the system
    functions ``⎕`` are not implemented.** A program using one is told which
    symbol stopped it.
  * **``→`` branch and labels are not implemented**; a program is a sequence of
    expressions. Combined with the ``∇`` function form, that covers everything the
    language is taught for.
"""

from __future__ import annotations

import math
import random
from typing import Any, Callable, TYPE_CHECKING

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


class AplError(Exception):
    """Raised while parsing or evaluating APL; reported with the ❌ prefix."""


#: Function words this executor understands, for the command inventory.
APL_COMMANDS = {
    "IOTA",
    "RHO",
    "TIMES",
    "DIVIDE",
    "POWER",
    "LOG",
    "CEILING",
    "FLOOR",
    "MAX",
    "MIN",
    "ABS",
    "RESIDUE",
    "SIGNUM",
    "RECIPROCAL",
    "REVERSE",
    "ROTATE",
    "TAKE",
    "DROP",
    "TRANSPOSE",
    "RAVEL",
    "CATENATE",
    "MEMBERSHIP",
    "NOT",
    "AND",
    "OR",
    "NAND",
    "NOR",
    "GRADEUP",
    "GRADEDOWN",
    "FORMAT",
    "ROLL",
    "REDUCE",
    "SCAN",
    "FD",
    "BK",
    "RT",
    "LT",
    "PU",
    "PD",
    "HOME",
    "CS",
}

#: Keyword spellings, mapped to the glyph they stand for. One function, two ways
#: of writing it; the glyph is what the language is written in.
_ALIASES: dict[str, str] = {
    "PLUS": "+",
    "MINUS": "-",
    "TIMES": "×",
    "DIVIDE": "÷",
    "POWER": "*",
    "LOG": "⍟",
    "CEILING": "⌈",
    "FLOOR": "⌊",
    "MAX": "⌈",
    "MIN": "⌊",
    "ABS": "|",
    "RESIDUE": "|",
    "SIGNUM": "×",
    "RECIPROCAL": "÷",
    "RHO": "⍴",
    "SHAPE": "⍴",
    "RESHAPE": "⍴",
    "IOTA": "⍳",
    "INDEXOF": "⍳",
    "REVERSE": "⌽",
    "ROTATE": "⌽",
    "TAKE": "↑",
    "DROP": "↓",
    "TRANSPOSE": "⍉",
    "RAVEL": ",",
    "CATENATE": ",",
    "MEMBERSHIP": "∈",
    "NOT": "~",
    "AND": "∧",
    "OR": "∨",
    "NAND": "⍲",
    "NOR": "⍱",
    "EQUAL": "=",
    "NOTEQUAL": "≠",
    "LESS": "<",
    "LESSEQ": "≤",
    "GREATER": ">",
    "GREATEREQ": "≥",
    "GRADEUP": "⍋",
    "GRADEDOWN": "⍒",
    "FORMAT": "⍕",
    "ROLL": "?",
    "REDUCE": "/",
    "SCAN": "\\",
}

#: Turtle words. Also a Time Warp extension, and all monadic.
_TURTLE_WORDS = {"FD", "FORWARD", "BK", "BACK", "RT", "RIGHT", "LT", "LEFT"}
_TURTLE_NILADIC = {"PU", "PENUP", "PD", "PENDOWN", "HOME", "CS", "CLEAR"}

#: Everything else the tokenizer must know as a single symbol.
_SYMBOLS = set("+-×÷*⍟⌈⌊|⍴⍳⌽↑↓⍉,∈~∧∨⍲⍱=≠<≤>≥⍋⍒⍕?/\\()←⍝")

_NILADIC_TURTLE = ("PU", "PENUP", "PD", "PENDOWN", "HOME", "CS", "CLEAR")

#: Keyboard spellings for the comparison glyphs.
_TWO_CHARACTER_SPELLINGS = {">=": "≥", "<=": "≤", "!=": "≠", "==": "="}


# ---------------------------------------------------------------------------
# Values
# ---------------------------------------------------------------------------


def _is_array(value: Any) -> bool:
    return isinstance(value, list)


def _scalar(value: Any) -> bool:
    return isinstance(value, (int, float))


def _items(value: Any) -> list:
    """A scalar as a one-element vector, for the structural functions."""
    return value if _is_array(value) else [value]


def _each(function: Callable[..., Any], *values: Any) -> Any:
    """Apply *function* at matching positions, keeping the array shape.

    This is what "pervasive" means in APL, and why ``1 2 3 + 10`` and
    ``1 2 3 + 4 5 6`` both work while ``1 2 3 + 4 5`` is a length error.
    """
    arrays = [value for value in values if _is_array(value)]
    if not arrays:
        return function(*values)
    lengths = {len(value) for value in arrays}
    if len(lengths) != 1:
        raise AplError(
            "length error: " + " and ".join(str(len(value)) for value in arrays)
        )
    length = lengths.pop()
    if any(not len(value) for value in arrays):
        return []
    result = []
    for index in range(length):
        arguments = [
            value[index] if _is_array(value) else value for value in values
        ]
        result.append(_each(function, *arguments))
    return result


def _rank(value: Any) -> int:
    return 1 + _rank(value[0]) if _is_array(value) and value else 0


def _shape_of(value: Any) -> list[int]:
    if not _is_array(value):
        return []
    shape = [len(value)]
    if value:
        inner = _shape_of(value[0])
        shape.extend(inner)
    return shape


def _raze(value: Any) -> list:
    """Every scalar in *value*, in reading order."""
    if not _is_array(value):
        return [value]
    result: list = []
    for item in value:
        result.extend(_raze(item))
    return result


def _from_flat(shape: list[int], flat: list) -> Any:
    """Rebuild a value of *shape* from a flat list of scalars."""
    if not shape:
        if not flat:
            raise AplError("not enough elements for the shape")
        return flat[0]
    length = shape[0]
    if length == 0:
        return []
    if not flat:
        raise AplError("not enough elements for the shape")
    if len(shape) == 1:
        # Reshape repeats its data cyclically: 3⍴⍳2 is 1 2 1.
        return [flat[index % len(flat)] for index in range(length)]
    total = 1
    for size in shape[1:]:
        total *= size
    if total == 0:
        return [[] for _ in range(length)]
    return [
        _from_flat(shape[1:], flat[index * total : (index + 1) * total])
        for index in range(length)
    ]


def _reshape(left: Any, right: Any) -> Any:
    shape = [int(item) for item in _raze(left)]
    if not shape:
        return right
    flat = _raze(right)
    if not flat:
        raise AplError("reshape of an empty array")
    return _from_flat(shape, flat)


# ---------------------------------------------------------------------------
# Scalar functions
# ---------------------------------------------------------------------------


def _divide(left: Any, right: Any) -> Any:
    if right == 0:
        raise AplError("division by zero")
    return left / right


def _residue(left: Any, right: Any) -> Any:
    """``X | Y`` is Y modulo X, and the result takes X's sign, as in APL."""
    if left == 0:
        return right
    if right == 0:
        return 0
    quotient = math.floor(right / left) if left * right > 0 else math.ceil(right / left)
    return right - left * quotient


def _log(left: Any, right: Any) -> Any:
    if right <= 0:
        raise AplError("logarithm of a non-positive number")
    return math.log(right, left) if left > 0 and left != 1 else math.log(right)


def _roll(right: Any) -> Any:
    return random.randint(1, int(right))


def _signum(right: Any) -> Any:
    return (right > 0) - (right < 0)


#: ``glyph: (monadic, dyadic)``. A ``None`` is a function that has only the one
#: valence, and a program that uses the missing one is told so.
_SCALARS: dict[str, tuple[Callable | None, Callable | None]] = {
    "+": (None, lambda left, right: left + right),
    "-": (lambda right: -right, lambda left, right: left - right),
    "×": (_signum, lambda left, right: left * right),
    "÷": (lambda right: 1 / right if right else _fail("reciprocal of zero"),
          _divide),
    "*": (lambda right: math.exp(right), lambda left, right: left ** right),
    "⍟": (lambda right: math.log(right) if right > 0 else _fail("log of a non-positive"),
          lambda left, right: math.log(right, left)),
    "⌈": (math.ceil, lambda left, right: max(left, right)),
    "⌊": (math.floor, lambda left, right: min(left, right)),
    "|": (abs, _residue),
    "=": (None, lambda left, right: int(left == right)),
    "≠": (None, lambda left, right: int(left != right)),
    "<": (None, lambda left, right: int(left < right)),
    "≤": (None, lambda left, right: int(left <= right)),
    ">": (None, lambda left, right: int(left > right)),
    "≥": (None, lambda left, right: int(left >= right)),
    "∧": (None, lambda left, right: int(bool(left) and bool(right))),
    "∨": (None, lambda left, right: int(bool(left) or bool(right))),
    "⍲": (None, lambda left, right: int(not (left and right))),
    "⍱": (None, lambda left, right: int(not (left or right))),
    "~": (lambda right: int(not right) if not _is_array(right) else _each(lambda x: int(not x), right), None),
    "?": (_roll, lambda left, right: [random.randint(1, int(right)) for _ in range(int(left))]),
}


def _fail(message: str):
    raise AplError(message)


# ---------------------------------------------------------------------------
# Structural functions
# ---------------------------------------------------------------------------


def _iota(right: Any) -> Any:
    """``⍳n`` counts to n; ``⍳a b c`` makes an array of that shape."""
    if _is_array(right):
        return _from_flat([int(item) for item in _raze(right)], list(range(1, _product(right) + 1)))
    count = int(right)
    if count < 0:
        raise AplError("IOTA needs a non-negative count")
    return list(range(1, count + 1))


def _product(value: Any) -> int:
    total = 1
    for item in _raze(value):
        total *= int(item)
    return total


def _index_of(left: Any, right: Any) -> Any:
    haystack = _raze(left)
    def look(item: Any) -> int:
        return haystack.index(item) + 1 if item in haystack else len(haystack) + 1
    return _each(look, right) if _is_array(right) else look(right)


def _reverse(right: Any) -> Any:
    return list(reversed(right)) if _is_array(right) else right


def _rotate(left: Any, right: Any) -> Any:
    if not _is_array(right) or not right:
        return right
    count = int(left) % len(right)
    return right[count:] + right[:count] if count else list(right)


def _take(left: Any, right: Any) -> Any:
    count = int(left)
    items = _items(right)
    if count >= 0:
        return (items + [0] * (count - len(items)))[:count]
    return ([0] * (-count - len(items)) + items)[-count:] if -count > len(items) else items[count:]


def _drop(left: Any, right: Any) -> Any:
    count = int(left)
    items = _items(right)
    if count >= 0:
        return items[count:]
    return items[:count] if count else items


def _transpose(right: Any) -> Any:
    if not _is_array(right) or not right or not _is_array(right[0]):
        return right
    return [list(row) for row in zip(*right)]


def _ravel(right: Any) -> Any:
    return _raze(right) if _is_array(right) else right


def _catenate(left: Any, right: Any) -> Any:
    return _items(left) + _items(right)


def _membership(left: Any, right: Any) -> Any:
    items = _raze(left)
    return _each(lambda item: int(item in items), right) if _is_array(right) else int(right in items)


def _grade_up(right: Any) -> Any:
    items = _items(right)
    return sorted(range(1, len(items) + 1), key=lambda position: items[position - 1])


def _grade_down(right: Any) -> Any:
    items = _items(right)
    return sorted(range(1, len(items) + 1), key=lambda position: items[position - 1], reverse=True)


def _format(right: Any) -> Any:
    """``⍕`` is the character form of a value, packed into a single string."""
    # Not the character path: 42 is the number forty-two, and rendering it as
    # chr(42) printed "*" where the digits belonged.
    return _display(right, nested=False)


def _reduce(operation: str, right: Any) -> Any:
    """``+/ 1 2 3`` folds the scalar function over the outermost axis."""
    items = _items(right)
    if not items:
        return 0
    total = items[0]
    for item in items[1:]:
        total = _apply_scalar(operation, total, item)
    return total


def _scan(operation: str, right: Any) -> Any:
    items = _items(right)
    if not items:
        return []
    result = [items[0]]
    for item in items[1:]:
        result.append(_apply_scalar(operation, result[-1], item))
    return result


def _apply_scalar(glyph: str, left: Any, right: Any) -> Any:
    entry = _SCALARS.get(glyph)
    if entry is None or entry[1] is None:
        raise AplError(f"{glyph} is not usable as a dyadic function here")
    return _each(entry[1], left, right)


# ---------------------------------------------------------------------------
# Display
# ---------------------------------------------------------------------------


def _number(value: Any) -> str:
    """APL's own number spelling: ``¯`` for negative, no trailing ``.0``."""
    if isinstance(value, bool):
        return "1" if value else "0"
    if isinstance(value, int) or (isinstance(value, float) and value.is_integer()):
        number = int(value)
        return f"¯{abs(number)}" if number < 0 else str(number)
    text = f"{value:g}"
    return text.replace("-", "¯")


def _display(value: Any, nested: bool = False) -> str:
    """Render a value the way APL prints it: scalars in a row, arrays as rows."""
    if _is_array(value):
        if value and _is_array(value[0]) and all(_is_array(item) for item in value):
            return "\n".join(_row(item) for item in value)
        return _row(value)
    if isinstance(value, str):
        return value
    if isinstance(value, (int, float)) and not isinstance(value, bool):
        if nested and isinstance(value, int) and 31 < value < 0x110000:
            return chr(value)
        return _number(value)
    return str(value)


def _row(value: Any) -> str:
    if not _is_array(value):
        return value if isinstance(value, str) else _number(value)
    parts = []
    for item in value:
        if _is_array(item):
            parts.append("(" + _row(item) + ")")
        elif isinstance(item, str):
            parts.append(item)
        else:
            parts.append(_number(item))
    return " ".join(parts)


# ---------------------------------------------------------------------------
# Tokenizer
# ---------------------------------------------------------------------------


def _tokenize(text: str) -> list[str]:
    """Symbols, numbers, names and strings.

    ``¯`` is APL's negative sign, and it is a numeric prefix rather than a
    function, so it is folded into the literal here instead of being evaluated.
    """
    tokens: list[str] = []
    index = 0
    while index < len(text):
        character = text[index]
        if character.isspace():
            index += 1
            continue
        if character in "'\"":
            closing = text.find(character, index + 1)
            if closing == -1:
                raise AplError("unterminated string")
            tokens.append(text[index : closing + 1])
            index = closing + 1
            continue
        if character == "¯" or character.isdigit() or (character == "." and index + 1 < len(text) and text[index + 1].isdigit()):
            start = index
            if character == "¯":
                index += 1
            while index < len(text) and (text[index].isdigit() or text[index] == "."):
                index += 1
            # Exponent form, as in 1e3 or 2E¯4.
            if index < len(text) and text[index] in "eE":
                index += 1
                if index < len(text) and text[index] == "¯":
                    index += 1
                while index < len(text) and text[index].isdigit():
                    index += 1
            tokens.append(text[start:index])
            continue
        if character.isalpha() or character == "_":
            start = index
            while index < len(text) and (text[index].isalnum() or text[index] == "_"):
                index += 1
            tokens.append(text[start:index].upper())
            continue
        if character in _SYMBOLS:
            # >= and <= are accepted as keyboard spellings of ≥ and ≤, because a
            # keyboard that cannot produce the glyph is not a reason to be unable
            # to write the comparison.
            two = text[index : index + 2]
            if two in _TWO_CHARACTER_SPELLINGS:
                tokens.append(_TWO_CHARACTER_SPELLINGS[two])
                index += 2
                continue
            tokens.append(character)
            index += 1
            continue
        raise AplError(f"cannot read {character!r}")
    return tokens


def _is_operand(token: str, state: dict[str, Any] | None = None) -> bool:
    """Whether *token* names a value rather than a function.

    This is the decision APL makes by position, so getting it wrong turns a
    function into an array element without any error to show for it.
    """
    if token.startswith("'") or token.startswith('"'):
        return True
    if token[:1] in ("¯", ".") or token[:1].isdigit():
        return True
    if token == "(":
        return False
    if token == ")":
        return True
    if _is_function(token, state):
        return False
    if token in _SYMBOLS or token in ("/", "\\", "←"):
        return False
    return True


#: Structural functions, kept apart from the scalars because their arguments are
#: whole arrays rather than positions.
_STRUCTURAL: dict[str, tuple[Callable | None, Callable | None]] = {
    "⍳": (_iota, _index_of),
    "⍴": (_shape_of, _reshape),
    "⌽": (_reverse, _rotate),
    "↑": (None, _take),
    "↓": (None, _drop),
    "⍉": (_transpose, None),
    ",": (_ravel, _catenate),
    "∈": (None, _membership),
    "⍋": (_grade_up, None),
    "⍒": (_grade_down, None),
    "⍕": (_format, None),
}


def _value_of(token: str, variables: dict[str, Any]) -> Any:
    if token.startswith("'") or token.startswith('"'):
        return token[1:-1]
    if token[:1] == "¯" or token[:1].isdigit() or token[:1] == ".":
        number = token.replace("¯", "-").replace("E-", "e-")
        if "e" in number or "E" in number:
            return float(number.replace("E", "e"))
        return float(number) if "." in number else int(number)
    return variables.get(token)


def _canonical(token: str) -> str:
    upper = token.upper()
    return _ALIASES.get(upper, upper)


# ---------------------------------------------------------------------------
# Evaluation
# ---------------------------------------------------------------------------


def _strand(
    tokens: list[str],
    index: int,
    variables: dict[str, Any],
    turtle: Any,
    state: dict[str, Any],
) -> tuple[Any, int]:
    """The run of adjacent operands ending at *index*, as one value.

    This is what makes ``1 2 3 + 10`` equal ``11 12 13`` rather than
    ``1 2 13``: the argument to the left of a function is the *whole* adjacent
    group of values, and a group of one is just that value.
    """
    items: list[Any] = []
    while index >= 0 and _is_operand(tokens[index], state):
        item, index = _operand(tokens, index, variables, turtle, state)
        items.append(item)
    if not items:
        raise AplError("an operand is missing")
    if len(items) == 1:
        return items[0], index
    return list(reversed(items)), index


def _expression(
    tokens: list[str],
    variables: dict[str, Any],
    turtle: Any,
    state: dict[str, Any],
) -> Any:
    """Evaluate a token list right to left, with no precedence.

    That is the whole of APL's grammar. Values are gathered into strands, then a
    function takes the strand on its right as its argument — and the strand on
    its left too, when there is one, which is what decides its valence.
    """
    index = len(tokens) - 1
    if index < 0:
        raise AplError("there is no expression here")
    if tokens[index].upper() in _TURTLE_NILADIC:
        # PU, PD, HOME and CS take no operand at all, so they cannot go through
        # the value-then-fold path the rest of the expression uses.
        value = _apply(tokens[index], None, None, turtle, state)
        index -= 1
    else:
        value, index = _strand(tokens, index, variables, turtle, state)

    while index >= 0:
        token = tokens[index]
        if token == "(":
            raise AplError("unmatched (")
        if not _is_function(token, state):
            raise AplError(f"{token} is not a function, but a function belongs here")
        if index - 1 >= 0 and _is_operand(tokens[index - 1], state):
            left, index = _strand(tokens, index - 1, variables, turtle, state)
            value = _apply(token, left, value, turtle, state)
        else:
            value = _apply(token, None, value, turtle, state)
            index -= 1
    return value


def _is_function(token: str, state: dict[str, Any] | None = None) -> bool:
    upper = token.upper()
    if upper in _TURTLE_WORDS or upper in _TURTLE_NILADIC:
        return True
    canonical = _ALIASES.get(upper, token)
    if canonical in _SCALARS or canonical in _STRUCTURAL:
        return True
    # A user-defined function is a function. Kept per run rather than module-wide:
    # two interpreters in one process must not see each other's definitions.
    return state is not None and upper in state.get("functions", {})


def _operand(
    tokens: list[str],
    index: int,
    variables: dict[str, Any],
    turtle: Any,
    state: dict[str, Any],
) -> tuple[Any, int]:
    """The value at *index*, and the index to continue from."""
    token = tokens[index]
    if token == ")":
        depth = 0
        start = index
        while index >= 0:
            if tokens[index] == ")":
                depth += 1
            elif tokens[index] == "(":
                depth -= 1
                if depth == 0:
                    break
            index -= 1
        if index < 0:
            raise AplError("unmatched )")
        # _evaluate, not _expression: a group may hold an operator of its own,
        # as the +/ in (+/X) does.
        return _evaluate(tokens[index + 1 : start], variables, turtle, state), index - 1
    if token == "(":
        raise AplError("unmatched (")
    value = _value_of(token, variables)
    if value is None and token.upper() not in _NILADIC_TURTLE and _is_operand(token, state):
        # A name that was never assigned. APL would say so rather than invent a
        # value, and a typo that evaluates to nothing is the failure mode worth
        # avoiding.
        raise AplError(f"{token} has no value")
    return value, index - 1


def _apply(
    token: str,
    left: Any,
    right: Any,
    turtle: Any,
    state: dict[str, Any],
) -> Any:
    """Apply a function, monadically when *left* is None."""
    upper = token.upper()
    if upper in _TURTLE_WORDS or upper in _TURTLE_NILADIC:
        return _turtle(upper, left, right, turtle, state)
    if upper in state.get("functions", {}):
        return _user_function(upper, left, right, state)

    canonical = _ALIASES.get(upper, token)

    # The two operators derive a function from a scalar function: +/ and +\ .
    if canonical in ("/", "\\"):
        return _fail("an operator needs a function on its left, as in +/")
    if token in ("/", "\\"):
        return _fail("an operator needs a function on its left, as in +/")
    return _apply_canonical(canonical, left, right)


def _apply_canonical(canonical: str, left: Any, right: Any) -> Any:
    entry = _STRUCTURAL.get(canonical)
    if entry is not None:
        monad, dyad = entry
        if left is None:
            if monad is None:
                raise AplError(f"{canonical} has no monadic form")
            return monad(right)
        if dyad is None:
            raise AplError(f"{canonical} has no dyadic form")
        return dyad(left, right)
    entry = _SCALARS.get(canonical)
    if entry is None:
        raise AplError(f"{canonical} is not a function this build knows")
    monad, dyad = entry
    if left is None:
        if monad is None:
            raise AplError(f"{canonical} has no monadic form")
        return _each(monad, right)
    if dyad is None:
        raise AplError(f"{canonical} has no dyadic form")
    return _each(dyad, left, right)


# ---------------------------------------------------------------------------
# Operators (reduce and scan) and user-defined functions
# ---------------------------------------------------------------------------


def _split_operator(tokens: list[str]) -> tuple[str, int, str] | None:
    """Find ``f/`` or ``f\\`` — an operator with its function to the left.

    Returns ``(function, index, operator)``, where *operator* is the canonical
    ``/`` or ``\\`` and *index* is where it sits. The token is canonicalised
    before it is compared, which is what makes the keyword spellings REDUCE and
    SCAN work: they are aliases for those two symbols, and comparing the raw token
    meant the aliases were documented as accepted and then reported as unknown
    variables.

    Only at the top level. A ``/`` inside parentheses belongs to the expression in
    there, and treating it as the operator for the whole line is what turned
    ``(+/X) ÷ ⍴X`` into a fragment with an unmatched bracket.
    """
    depth = 0
    for index, token in enumerate(tokens):
        if token == "(":
            depth += 1
            continue
        if token == ")":
            depth -= 1
            continue
        if depth:
            continue
        if _canonical(token) not in ("/", "\\"):
            continue
        if index == 0:
            raise AplError("an operator needs a function on its left, as in +/")
        return _canonical(tokens[index - 1]), index, _canonical(token)
    return None


def _evaluate(
    tokens: list[str],
    variables: dict[str, Any],
    turtle: Any,
    state: dict[str, Any],
) -> Any:
    operator = _split_operator(tokens)
    if operator is None:
        return _expression(tokens, variables, turtle, state)
    glyph, index, kind = operator
    if glyph not in _SCALARS:
        raise AplError(f"{glyph} cannot be used with {kind}")
    # The derived function's argument is everything to the right, which may
    # itself contain operators.
    right = _evaluate(tokens[index + 1 :], variables, turtle, state)
    return _reduce(glyph, right) if kind == "/" else _scan(glyph, right)


def _user_function(name: str, left: Any, right: Any, state: dict[str, Any]) -> Any:
    entry = state["functions"][name]
    parameters = entry["parameters"]
    body = entry["body"]
    # A function gets its own copy of the variables, so a parameter cannot
    # overwrite the caller's value of the same name.
    inner = {"functions": state["functions"], "variables": dict(state["variables"])}
    if len(parameters) == 1:
        inner["variables"][parameters[0]] = right
    elif len(parameters) == 2:
        inner["variables"][parameters[0]] = left
        inner["variables"][parameters[1]] = right
    else:
        raise AplError(f"{name} takes one or two arguments")
    result: Any = None
    for line in body:
        result, _assigned = _statement(line, None, inner)
    return result


# ---------------------------------------------------------------------------
# Statements
# ---------------------------------------------------------------------------


def _statement(
    line: str,
    turtle: Any,
    state: dict[str, Any],
) -> tuple[Any, bool]:
    """Run one line. Returns ``(value, assigned)``.

    ``assigned`` is what decides whether anything is displayed: APL shows a
    result, and says nothing at all when a line only stored one.
    """
    text = _strip_comment(line)
    if not text:
        return None, True
    if text.startswith("∇"):
        raise AplError("a function definition cannot be nested inside another")

    variables: dict[str, Any] = state["variables"]
    tokens = _tokenize(text)
    if len(tokens) >= 2 and tokens[1] == "←":
        name = tokens[0].upper()
        if not name.replace("_", "").isalnum():
            raise AplError(f"{tokens[0]} is not a name")
        value = _evaluate(tokens[2:], variables, turtle, state)
        variables[name] = value
        return value, True

    value = _evaluate(tokens, variables, turtle, state)
    return value, False


def _strip_comment(line: str) -> str:
    index = line.find("⍝")
    return (line if index < 0 else line[:index]).strip()


# ---------------------------------------------------------------------------
# Turtle extension
# ---------------------------------------------------------------------------


def _turtle(
    word: str,
    left: Any,
    right: Any,
    turtle: "TurtleState",
    state: dict[str, Any],
) -> Any:
    """Turtle words as APL functions, so they compose like everything else.

    The line is marked silent: a drawing command is not a value to display, and
    a square would otherwise print eight zeroes.
    """
    state["quiet"] = True
    amount = left if left is not None else right
    if word in ("PU", "PENUP"):
        turtle.penup()
        return 0
    if word in ("PD", "PENDOWN"):
        turtle.pendown()
        return 0
    if word == "HOME":
        turtle.home()
        return 0
    if word in ("CS", "CLEAR"):
        turtle.clear()
        return 0
    if amount is None:
        raise AplError(f"{word} needs a distance or angle")
    if word in ("FD", "FORWARD"):
        turtle.forward(amount)
    elif word in ("BK", "BACK"):
        turtle.back(amount)
    elif word in ("RT", "RIGHT"):
        turtle.right(amount)
    else:
        turtle.left(amount)
    return 0


def _turtle_state_value(turtle: "TurtleState", name: str) -> Any:
    """One of the turtle's own numbers, as an integer when it is one.

    ``round`` returns a float, and a float that is whole still prints as ``0.0``
    in a language whose numbers are otherwise exact.
    """
    raw = {"x": turtle.x, "y": turtle.y, "heading": turtle.heading}[name]
    rounded = round(raw, 6)
    return int(rounded) if float(rounded).is_integer() else rounded


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def _run_state(interp: "Interpreter") -> dict[str, Any]:
    state = interp.__dict__.get("_apl_state")
    if state is None:
        state = {"variables": {}, "functions": {}, "defining": None}
        interp.__dict__["_apl_state"] = state
    return state


def execute_apl(
    interpreter: "Interpreter",
    source: str,
    turtle: "TurtleState",
) -> str:
    """Execute one line of APL and return the text to display."""
    state = _run_state(interpreter)
    variables: dict[str, Any] = state["variables"]
    # The turtle's position and heading, under names that cannot collide with a
    # program's own. X, Y and H would all be ordinary APL variables -- `X <- 5`
    # is the most natural line anyone writes -- so the turtle's are two letters.
    variables["TX"] = _turtle_state_value(turtle, "x")
    variables["TY"] = _turtle_state_value(turtle, "y")
    variables["THETA"] = _turtle_state_value(turtle, "heading")

    text = (source or "").strip()

    # A function definition, written the way Dyalog's traditional editor does:
    # ∇ F X on the header line, the body, then a line holding only ∇.
    if state["defining"] is not None:
        if text == "∇":
            header, body = state["defining"]
            state["defining"] = None
            if not body:
                return "❌ APL: a function needs a body between the ∇ lines\n"
            state["functions"][header["name"]] = {
                "parameters": header["parameters"],
                "body": body,
            }
            return f"✅ APL: {header['name']} defined\n"
        if text.startswith("∇"):
            state["defining"] = None
            return "❌ APL: a definition was already open\n"
        state["defining"][1].append(source.strip())
        return ""

    if text.startswith("∇"):
        try:
            header = _header(text[1:].strip())
        except AplError as exc:
            return f"❌ APL: {exc}\n"
        state["defining"] = (header, [])
        return ""

    if not _strip_comment(text):
        return ""

    try:
        value, assigned = _statement(text, turtle, state)
    except AplError as exc:
        return f"❌ APL: {exc}\n"
    except (ValueError, TypeError, OverflowError, ZeroDivisionError) as exc:
        return f"❌ APL: {exc}\n"
    except RecursionError:
        return "❌ APL: the expression is nested too deeply\n"

    return "" if assigned or state.pop("quiet", False) else _display(value) + "\n"


def _header(text: str) -> dict[str, Any]:
    """Parse a ``∇`` header line.

    APL decides valence by position here too, so the shape of the line is the
    whole specification:

        ∇ F X            F with one argument
        ∇ R ← F X        F with one argument and a result
        ∇ R ← X F Y      F with two arguments and a result
        ∇ F              F with no arguments
    """
    if not text:
        raise AplError("a definition needs a header, as in ∇ R ← DOUBLE X")
    result: str | None = None
    rest = text
    if "←" in rest:
        result, _, rest = rest.partition("←")
        result = result.strip().upper()
    words = rest.split()
    if not words:
        raise AplError("a definition needs a function name")
    if len(words) == 1:
        name, parameters = words[0].upper(), []
    elif len(words) == 2:
        name, parameters = words[0].upper(), [words[1].upper()]
    elif len(words) == 3:
        # Dyadic: the name sits between its two arguments.
        name = words[1].upper()
        parameters = [words[0].upper(), words[2].upper()]
    else:
        raise AplError("one or two arguments is the limit")
    return {"name": name, "parameters": parameters, "result": result}
