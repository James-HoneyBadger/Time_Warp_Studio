"""Smalltalk language executor for Time Warp Studio.

Educational Smalltalk interpreter — whole-program execution.
Implements a teaching subset of Smalltalk-80:

  - Transcript show: 'text'.  /  Transcript showCr: 'text'.
  - Transcript show: obj printString.
  - | var1 var2 | — temporary variable declaration
  - var := expr.  — assignment
  - Integer, Float, Boolean, String, Array, OrderedCollection
  - Basic message sends:
      Unary:   obj printString, obj class, obj factorial, etc.
      Binary:  a + b, a = b, a < b, etc.
      Keyword: obj at: key, coll add: item, etc.
  - Blocks: [stmts] value, [:arg | stmts] value: x
  - Control: (cond) ifTrue: [block] ifFalse: [block]
             (cond) ifFalse: [block]
             (cond) ifTrue: [block]
             n timesRepeat: [block]
             start to: end do: [:i | block]
             start to: end by: step do: [:i | block]
             collection do: [:each | block]
             [cond] whileTrue: [block]
             [cond] whileFalse: [block]
  - Array literals: #(1 2 3), #('a' 'b')
  - OrderedCollection: new, add:, addFirst:, remove:, size, do:,
                       collect:, select:, reject:, inject:into:
  - String: ,  (concat), size, copyFrom:to:, includes:, reversed,
            asUppercase, asLowercase, asInteger, asFloat
  - Number: + - * / // \\ = < > <= >= sqrt abs factorial max: min:
  - Turtle graphics: forward:, backward:, left:, right:, penUp, penDown,
                     color:red:green:blue:, home, setheading:
  - Class definition (basic): recognized but class methods are no-ops
  - printNl — print and newline
"""

from __future__ import annotations

import math
import re
from typing import TYPE_CHECKING, Any, Dict, List, Optional, Tuple

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def execute_smalltalk(
    interpreter: "Interpreter", source: str, turtle: "TurtleState"
) -> str:
    """Execute a complete Smalltalk program and return all output as a string."""
    env = SmalltalkEnvironment(interpreter, turtle)
    return env.run(source)


# ---------------------------------------------------------------------------
# Runtime value types
# ---------------------------------------------------------------------------


_UNDEF = object()  # Nil sentinel


class _STObject:
    """Base wrapper for Smalltalk objects."""

    def __init__(self, value: Any) -> None:
        self.value = value

    def st_class(self) -> str:
        return type(self).__name__

    def print_string(self) -> str:
        return str(self.value)


class _STOrderedCollection(_STObject):
    def __init__(self, items: Optional[List[Any]] = None) -> None:
        super().__init__(None)
        self.items: List[Any] = list(items) if items else []

    def print_string(self) -> str:
        inner = " ".join(_st_print(v) for v in self.items)
        return f"OrderedCollection ({inner} )"

    def st_class(self) -> str:
        return "OrderedCollection"


# ---------------------------------------------------------------------------
# Control-flow exceptions
# ---------------------------------------------------------------------------


class _STReturn(Exception):
    def __init__(self, value: Any = _UNDEF) -> None:
        self.value = value


class _STError(Exception):
    pass


# ---------------------------------------------------------------------------
# Value helpers
# ---------------------------------------------------------------------------


def _st_print(v: Any) -> str:
    if v is None or v is _UNDEF:
        return "nil"
    if isinstance(v, bool):
        return "true" if v else "false"
    if isinstance(v, float) and v == int(v):
        return str(int(v))
    if isinstance(v, _STOrderedCollection):
        return v.print_string()
    if isinstance(v, list):
        inner = " ".join(_st_print(x) for x in v)
        return f"({inner} )"
    if isinstance(v, str):
        return f"'{v}'"
    return str(v)


def _is_truthy(v: Any) -> bool:
    if v is None or v is _UNDEF:
        return False
    if isinstance(v, bool):
        return v
    if isinstance(v, (int, float)):
        return v != 0
    if isinstance(v, str):
        return v.lower() not in ("false", "nil", "")
    return True


def _to_num(v: Any) -> float:
    if isinstance(v, bool):
        return 1.0 if v else 0.0
    try:
        return float(v)
    except (ValueError, TypeError):
        return 0.0


# ---------------------------------------------------------------------------
# Tokeniser/parser helpers
# ---------------------------------------------------------------------------


def _tokenise_source(source: str) -> List[str]:
    """Split source into statements delimited by '.' respecting strings/brackets."""
    stmts: List[str] = []
    buf: List[str] = []
    depth_sq = depth_brace = 0
    in_sq = in_dq = False
    i = 0
    n = len(source)
    while i < n:
        c = source[i]
        if in_sq:
            buf.append(c)
            if c == "'":
                in_sq = False
        elif c == "'":
            in_sq = True
            buf.append(c)
        elif c == '"':
            # Smalltalk comments are in double quotes
            j = source.find('"', i + 1)
            i = j if j >= 0 else n
        elif c in "[{(":
            depth_brace += 1
            buf.append(c)
        elif c in "]})":
            depth_brace -= 1
            buf.append(c)
        elif c == "." and depth_brace == 0:
            stmt = "".join(buf).strip()
            if stmt:
                stmts.append(stmt)
            buf = []
        elif c == "!":
            # chunk separator — ignore
            pass
        else:
            buf.append(c)
        i += 1
    remainder = "".join(buf).strip()
    if remainder:
        stmts.append(remainder)
    return stmts


# ---------------------------------------------------------------------------
# Environment
# ---------------------------------------------------------------------------


class SmalltalkEnvironment:
    """State container and executor for a Smalltalk program."""

    MAX_LOOPS = 50_000

    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState") -> None:
        self._interp = interpreter
        self._turtle = turtle
        self._vars: Dict[str, Any] = {
            "true": True,
            "false": False,
            "nil": None,
            "Transcript": _UNDEF,  # sentinel for Transcript sends
        }
        self._output_buf: str = ""
        self._output: List[str] = []
        self._input_used = False

    # ------------------------------------------------------------------
    # Output
    # ------------------------------------------------------------------

    def _emit(self, text: str) -> None:
        combined = self._output_buf + text
        lines = combined.split("\n")
        for line in lines[:-1]:
            self._output.append(line)
        self._output_buf = lines[-1]

    def _flush(self) -> None:
        if self._output_buf:
            self._output.append(self._output_buf)
            self._output_buf = ""

    # ------------------------------------------------------------------
    # Variable access
    # ------------------------------------------------------------------

    def _get(self, name: str) -> Any:
        return self._vars.get(name, None)

    def _set(self, name: str, value: Any) -> None:
        self._vars[name] = value

    # ------------------------------------------------------------------
    # Entry point
    # ------------------------------------------------------------------

    def run(self, source: str) -> str:
        # Remove class definitions (basic support: skip class ... end class / !)
        source = self._strip_class_defs(source)

        stmts = _tokenise_source(source)
        try:
            for stmt in stmts:
                stmt = stmt.strip()
                if not stmt:
                    continue
                self._exec_stmt(stmt)
        except _STReturn:
            pass
        except _STError as e:
            self._emit(f"❌ {e}\n")
        except Exception as e:
            self._emit(f"❌ Runtime error: {e}\n")
        self._flush()
        result = [ln for ln in self._output]
        return "\n".join(result) + ("\n" if result else "")

    def _strip_class_defs(self, source: str) -> str:
        """Remove class definition syntax (not executed)."""
        # Handle: Object subclass: #Name instanceVariableNames: ... etc.
        source = re.sub(r"\w+\s+subclass:\s*#\w+[^!]*!", "", source)
        source = re.sub(r"!\s*\w+\s+methodsFor:[^!]*!", "", source)
        source = re.sub(r"!\s*\w+\s+class\s+methodsFor:[^!]*!", "", source)
        return source

    # ------------------------------------------------------------------
    # Statement executor
    # ------------------------------------------------------------------

    def _exec_stmt(self, stmt: str) -> Any:
        stmt = stmt.strip()
        if not stmt:
            return None

        # Temp variable declaration: | var1 var2 ... |
        m = re.match(r"^\|\s*([\w\s]+)\s*\|(.*)$", stmt, re.DOTALL)
        if m:
            for v in m.group(1).split():
                if v not in self._vars:
                    self._set(v, None)
            rest = m.group(2).strip()
            if rest:
                return self._exec_stmt(rest)
            return None

        # Assignment: var := expr
        m = re.match(r"^([A-Za-z_]\w*)\s*:=\s*(.+)$", stmt, re.DOTALL)
        if m:
            val = self._eval_expr(m.group(2).strip())
            self._set(m.group(1), val)
            return val

        # Return: ^ expr
        if stmt.startswith("^"):
            val = self._eval_expr(stmt[1:].strip())
            raise _STReturn(val)

        return self._eval_expr(stmt)

    # ------------------------------------------------------------------
    # Expression evaluator
    # ------------------------------------------------------------------

    def _eval_expr(self, expr: str) -> Any:
        expr = expr.strip()
        if not expr:
            return None

        # Cascade: obj msg1; msg2; msg3
        if ";" in expr:
            # Only split cascades at top level
            parts = self._split_cascade(expr)
            if len(parts) > 1:
                receiver_expr = parts[0]
                receiver = self._eval_expr(receiver_expr)
                for msg in parts[1:]:
                    receiver = self._send_message(receiver, msg.strip())
                return receiver

        # Block value evaluation: [stmts] value / [stmts] value: arg
        # This is handled inside _send_message — here we just parse
        # Full message expression (keyword > binary > unary)
        return self._eval_message_expr(expr)

    def _split_cascade(self, expr: str) -> List[str]:
        """Split 'receiver msg1; msg2' into [receiver msg1, msg2]."""
        parts: List[str] = []
        buf: List[str] = []
        depth = 0
        in_sq = False
        for c in expr:
            if in_sq:
                buf.append(c)
                if c == "'":
                    in_sq = False
            elif c == "'":
                in_sq = True
                buf.append(c)
            elif c in "[({":
                depth += 1
                buf.append(c)
            elif c in "])}":
                depth -= 1
                buf.append(c)
            elif c == ";" and depth == 0:
                parts.append("".join(buf).strip())
                buf = []
            else:
                buf.append(c)
        if buf:
            parts.append("".join(buf).strip())
        return parts

    def _eval_message_expr(self, expr: str) -> Any:
        """Evaluate a full message expression: receiver [keyword: arg]* or binary."""
        expr = expr.strip()
        if not expr:
            return None

        # Try to parse as keyword message: receiver kw1: arg1 kw2: arg2 ...
        m_kw = re.match(
            r"^(.+?)\s+([a-z]\w*:(?:\s*\S+\s+[a-z]\w*:)*\s*.+)$", expr, re.DOTALL
        )
        if m_kw:
            # Try to find keyword pattern
            kw_match = self._parse_keyword_send(expr)
            if kw_match:
                receiver_str, selector, kw_args = kw_match
                receiver = self._eval_primary_chain(receiver_str)
                return self._send_keyword(receiver, selector, kw_args)

        # Binary or unary chain
        return self._eval_primary_chain(expr)

    def _parse_keyword_send(
        self, expr: str
    ) -> Optional[Tuple[str, str, List[str]]]:
        """Return (receiver_str, selector, [args]) for a keyword message, or None."""
        # Keywords are: word: arg word: arg ...
        # We need to find the LAST keyword send at the top level
        kw_re = re.compile(r"\b([a-z]\w*:)\s*")
        # Find all keyword selectors at depth 0
        depth = 0
        in_sq = False
        first_kw_pos = -1
        selector_parts: List[str] = []
        arg_parts: List[str] = []
        i = 0
        n = len(expr)
        state = "receiver"
        recv_end = 0
        cur_arg_start = 0
        current_kw = ""

        while i < n:
            c = expr[i]
            if in_sq:
                if c == "'":
                    in_sq = False
                i += 1
                continue
            if c == "'":
                in_sq = True
                i += 1
                continue
            if c in "[({":
                depth += 1
                i += 1
                continue
            if c in "])}":
                depth -= 1
                i += 1
                continue
            if depth == 0:
                m = kw_re.match(expr, i)
                if m:
                    if state == "receiver":
                        recv_end = i
                        state = "keyword"
                    else:
                        arg_parts.append(expr[cur_arg_start:i].strip())
                    selector_parts.append(m.group(1))
                    cur_arg_start = m.end()
                    i = m.end()
                    continue
            i += 1

        if not selector_parts:
            return None

        arg_parts.append(expr[cur_arg_start:].strip())
        receiver_str = expr[:recv_end].strip()
        if not receiver_str:
            return None
        selector = "".join(selector_parts)
        return (receiver_str, selector, arg_parts)

    def _eval_primary_chain(self, expr: str) -> Any:
        """Evaluate a chain of unary and binary messages."""
        expr = expr.strip()
        if not expr:
            return None

        # Evaluate primary + chain of unary/binary messages
        # Split into tokens respecting brackets and strings
        tokens = self._split_message_chain(expr)
        if not tokens:
            return self._eval_primary(expr)

        receiver = self._eval_primary(tokens[0])
        i = 1
        while i < len(tokens):
            tok = tokens[i]
            # Binary message: operator arg
            if re.match(r"^[+\-*/<>=,~&|@%\\!?]+$|^//|^\\\\", tok):
                if i + 1 < len(tokens):
                    arg = self._eval_primary(tokens[i + 1])
                    receiver = self._send_binary(receiver, tok, arg)
                    i += 2
                    continue
            # Unary message
            if re.match(r"^[a-z]\w*$", tok):
                receiver = self._send_unary(receiver, tok)
                i += 1
                continue
            # Keyword message continuation — shouldn't happen here
            break
        return receiver

    def _split_message_chain(self, expr: str) -> List[str]:
        """Split 'receiver msg1 msg2 op arg' at the top level."""
        tokens: List[str] = []
        buf: List[str] = []
        depth = 0
        in_sq = False
        i = 0
        n = len(expr)
        while i < n:
            c = expr[i]
            if in_sq:
                buf.append(c)
                if c == "'":
                    in_sq = False
                i += 1
                continue
            if c == "'":
                in_sq = True
                buf.append(c)
                i += 1
                continue
            if c in "[({":
                depth += 1
                buf.append(c)
                i += 1
                continue
            if c in "])}":
                depth -= 1
                buf.append(c)
                i += 1
                continue
            if c == " " and depth == 0:
                tok = "".join(buf).strip()
                if tok:
                    tokens.append(tok)
                buf = []
                i += 1
                continue
            buf.append(c)
            i += 1
        tok = "".join(buf).strip()
        if tok:
            tokens.append(tok)
        return tokens

    def _eval_primary(self, expr: str) -> Any:
        expr = expr.strip()
        if not expr:
            return None

        # nil
        if expr == "nil":
            return None
        # true/false
        if expr == "true":
            return True
        if expr == "false":
            return False

        # Integer literal
        if re.match(r"^-?\d+$", expr):
            return int(expr)
        # Float literal
        if re.match(r"^-?\d+\.\d+$", expr):
            return float(expr)
        # Hex literal: 16rFF
        m = re.match(r"^(\d+)r([0-9A-Fa-f]+)$", expr)
        if m:
            try:
                return int(m.group(2), int(m.group(1)))
            except ValueError:
                return 0

        # Negative
        if expr.startswith("-") and len(expr) > 1:
            val = self._eval_primary(expr[1:])
            if isinstance(val, (int, float)):
                return -val

        # String literal
        if expr.startswith("'") and expr.endswith("'") and len(expr) >= 2:
            return expr[1:-1].replace("''", "'")

        # Symbol literal
        if expr.startswith("#") and not expr.startswith("#("):
            return expr[1:]

        # Array literal #(...)
        if expr.startswith("#(") and expr.endswith(")"):
            return self._parse_array_literal(expr[2:-1])

        # Block [...]
        if expr.startswith("[") and expr.endswith("]"):
            return ("__block__", expr[1:-1])

        # Parenthesised
        if expr.startswith("(") and expr.endswith(")"):
            return self._eval_expr(expr[1:-1])

        # Unary message on literal: Number class, etc.
        # Already handled in _eval_primary_chain

        # Class names / variable
        if expr[0].isupper():
            # Check if it's a known variable first
            val = self._get(expr)
            # Return string class name so class-side messages (new, etc.) work
            return val if val is not None else expr
        return self._get(expr)

    def _parse_array_literal(self, inner: str) -> List[Any]:
        items: List[Any] = []
        for token in inner.split():
            if re.match(r"^-?\d+\.\d+$", token):
                items.append(float(token))
            elif re.match(r"^-?\d+$", token):
                items.append(int(token))
            elif token.startswith("'") and token.endswith("'"):
                items.append(token[1:-1])
            elif token == "true":
                items.append(True)
            elif token == "false":
                items.append(False)
            elif token == "nil":
                items.append(None)
            else:
                items.append(token)  # symbol
        return items

    # ------------------------------------------------------------------
    # Message dispatch
    # ------------------------------------------------------------------

    def _send_unary(self, receiver: Any, selector: str) -> Any:  # noqa: C901
        # Block value
        if isinstance(receiver, tuple) and receiver[0] == "__block__":
            if selector == "value":
                return self._eval_block(receiver[1], [])
        # Numbers
        if isinstance(receiver, (int, float)):
            if selector == "printString":
                return _st_print(receiver)
            if selector == "printNl":
                self._emit(_st_print(receiver).strip("'") + "\n")
                return receiver
            if selector == "factorial":
                n = int(receiver)
                r = 1
                for k in range(2, n + 1):
                    r *= k
                return r
            if selector == "sqrt":
                return math.sqrt(float(receiver))
            if selector == "abs":
                return abs(receiver)
            if selector == "negated":
                return -receiver
            if selector == "isZero":
                return receiver == 0
            if selector == "positive":
                return receiver >= 0
            if selector == "negative":
                return receiver < 0
            if selector == "even":
                return int(receiver) % 2 == 0
            if selector == "odd":
                return int(receiver) % 2 != 0
            if selector == "class":
                return "Integer" if isinstance(receiver, int) else "Float"
            if selector == "asFloat":
                return float(receiver)
            if selector == "asInteger":
                return int(receiver)
            if selector == "asString":
                return _st_print(receiver).strip("'")
            if selector == "ceiling":
                return math.ceil(receiver)
            if selector == "floor":
                return math.floor(receiver)
            if selector == "truncated":
                return int(receiver)
            if selector == "rounded":
                return round(receiver)
            if selector == "reciprocal":
                return 1.0 / receiver if receiver != 0 else None
        # Strings
        if isinstance(receiver, str):
            if selector == "printString":
                return f"'{receiver}'"
            if selector == "printNl":
                self._emit(receiver + "\n")
                return receiver
            if selector == "size":
                return len(receiver)
            if selector == "reversed":
                return receiver[::-1]
            if selector == "asUppercase":
                return receiver.upper()
            if selector == "asLowercase":
                return receiver.lower()
            if selector == "trimSeparators":
                return receiver.strip()
            if selector == "isEmpty":
                return len(receiver) == 0
            if selector == "notEmpty":
                return len(receiver) > 0
            if selector == "asInteger":
                try:
                    return int(receiver)
                except ValueError:
                    return 0
            if selector == "asFloat":
                try:
                    return float(receiver)
                except ValueError:
                    return 0.0
            if selector == "asSymbol":
                return receiver
            if selector == "class":
                return "String"
        # Booleans
        if isinstance(receiver, bool):
            if selector == "printString":
                return "true" if receiver else "false"
            if selector == "printNl":
                self._emit(("true" if receiver else "false") + "\n")
                return receiver
            if selector == "not":
                return not receiver
            if selector == "class":
                return "Boolean"
        # nil
        if receiver is None:
            if selector == "printString":
                return "nil"
            if selector == "isNil":
                return True
            if selector == "notNil":
                return False
        # Collections
        if isinstance(receiver, (list, _STOrderedCollection)):
            items = receiver.items if isinstance(receiver, _STOrderedCollection) else receiver
            if selector == "printString":
                return _st_print(receiver)
            if selector == "printNl":
                self._emit(_st_print(receiver) + "\n")
                return receiver
            if selector == "size":
                return len(items)
            if selector == "isEmpty":
                return len(items) == 0
            if selector == "notEmpty":
                return len(items) > 0
            if selector == "first":
                return items[0] if items else None
            if selector == "last":
                return items[-1] if items else None
            if selector == "reversed":
                new = list(reversed(items))
                if isinstance(receiver, _STOrderedCollection):
                    return _STOrderedCollection(new)
                return new
            if selector == "asOrderedCollection":
                return _STOrderedCollection(list(items))
            if selector == "asArray":
                return list(items)
            if selector == "removeLast":
                if isinstance(receiver, _STOrderedCollection):
                    return receiver.items.pop() if receiver.items else None
                return items.pop() if items else None
            if selector == "removeFirst":
                if isinstance(receiver, _STOrderedCollection):
                    return receiver.items.pop(0) if receiver.items else None
                return items.pop(0) if items else None
        # Transcript
        if receiver is _UNDEF:
            if selector == "nl":
                self._emit("\n")
                return receiver
            if selector == "cr":
                self._emit("\n")
                return receiver
        # Turtle: object setheading etc.
        # Class-side messages (receiver is class name as string)
        if isinstance(receiver, str) and receiver[0:1].isupper():
            if selector == "new":
                if receiver == "OrderedCollection":
                    return _STOrderedCollection()
                if receiver in ("Array", "Set", "Bag"):
                    return []
                if receiver == "Dictionary":
                    return {}
            if selector == "basicNew":
                return _STOrderedCollection() if receiver == "OrderedCollection" else []

        # Generic
        if selector == "printString":
            return _st_print(receiver)
        if selector == "printNl":
            self._emit(_st_print(receiver).strip("'") + "\n")
            return receiver
        if selector == "yourself":
            return receiver
        if selector == "isNil":
            return receiver is None
        if selector == "notNil":
            return receiver is not None
        if selector == "class":
            return type(receiver).__name__
        return receiver

    def _send_binary(self, receiver: Any, op: str, arg: Any) -> Any:
        if op == ",":  # string concat
            return str(_st_display(receiver)) + str(_st_display(arg))
        if isinstance(receiver, (int, float)) and isinstance(arg, (int, float)):
            if op == "+":
                return receiver + arg
            if op == "-":
                return receiver - arg
            if op == "*":
                return receiver * arg
            if op == "/":
                if arg == 0:
                    raise _STError("Division by zero")
                result = receiver / arg
                return int(result) if result == int(result) else result
            if op == "//":
                if arg == 0:
                    raise _STError("Division by zero")
                return int(receiver // arg)
            if op == "\\\\":
                if arg == 0:
                    raise _STError("Modulo by zero")
                return receiver % arg
            if op == "**":
                return receiver ** arg
            if op == "=":
                return receiver == arg
            if op == "~=":
                return receiver != arg
            if op == "<":
                return receiver < arg
            if op == ">":
                return receiver > arg
            if op == "<=":
                return receiver <= arg
            if op == ">=":
                return receiver >= arg
        if isinstance(receiver, bool) and isinstance(arg, bool):
            if op == "&":
                return receiver and arg
            if op == "|":
                return receiver or arg
        if isinstance(receiver, str) and isinstance(arg, str):
            if op == "=":
                return receiver == arg
            if op == "<":
                return receiver < arg
            if op == ">":
                return receiver > arg
            if op == "<=":
                return receiver <= arg
            if op == ">=":
                return receiver >= arg
            if op == "~=":
                return receiver != arg
        # Generic equality
        if op == "=":
            return receiver == arg
        if op == "~=":
            return receiver != arg
        if op == "==":
            return receiver is arg
        if op == "~~":
            return receiver is not arg
        return None

    def _send_keyword(self, receiver: Any, selector: str, args: List[str]) -> Any:  # noqa: C901
        """Evaluate keyword message args then dispatch."""
        eargs = [self._eval_expr(a) for a in args]

        # Transcript
        if receiver is _UNDEF:
            if selector == "show:":
                val = eargs[0]
                self._emit(_st_display(val))
                return receiver
            if selector == "showCr:":
                val = eargs[0]
                self._emit(_st_display(val) + "\n")
                return receiver
            if selector == "print:":
                self._emit(_st_print(eargs[0]))
                return receiver
            if selector in ("nl:", "cr:"):
                self._emit("\n")
                return receiver

        # Boolean control
        if isinstance(receiver, bool):
            if selector == "ifTrue:":
                if receiver:
                    return self._eval_block_arg(eargs[0], [])
                return None
            if selector == "ifFalse:":
                if not receiver:
                    return self._eval_block_arg(eargs[0], [])
                return None
            if selector in ("ifTrue:ifFalse:", "ifFalse:ifTrue:"):
                if selector.startswith("ifTrue"):
                    tb, fb = eargs[0], eargs[1]
                else:
                    fb, tb = eargs[0], eargs[1]
                if receiver:
                    return self._eval_block_arg(tb, [])
                else:
                    return self._eval_block_arg(fb, [])
            if selector == "and:":
                if not receiver:
                    return False
                return _is_truthy(self._eval_block_arg(eargs[0], []))
            if selector == "or:":
                if receiver:
                    return True
                return _is_truthy(self._eval_block_arg(eargs[0], []))

        # Block messages
        if isinstance(receiver, tuple) and receiver[0] == "__block__":
            if selector == "value:":
                return self._eval_block(receiver[1], [eargs[0]])
            if selector == "value:value:":
                return self._eval_block(receiver[1], [eargs[0], eargs[1]])
            if selector == "whileTrue:":
                count = 0
                while True:
                    if count >= self.MAX_LOOPS:
                        raise _STError("whileTrue: max iterations exceeded")
                    count += 1
                    cond = self._eval_block(receiver[1], [])
                    if not _is_truthy(cond):
                        break
                    self._eval_block_arg(eargs[0], [])
                return None
            if selector == "whileFalse:":
                count = 0
                while True:
                    if count >= self.MAX_LOOPS:
                        raise _STError("whileFalse: max iterations exceeded")
                    count += 1
                    cond = self._eval_block(receiver[1], [])
                    if _is_truthy(cond):
                        break
                    self._eval_block_arg(eargs[0], [])
                return None
            if selector == "whileTrue":
                count = 0
                while True:
                    if count >= self.MAX_LOOPS:
                        raise _STError("whileTrue max iterations exceeded")
                    count += 1
                    cond = self._eval_block(receiver[1], [])
                    if not _is_truthy(cond):
                        break
                return None

        # Number messages
        if isinstance(receiver, (int, float)):
            if selector == "timesRepeat:":
                n = int(receiver)
                for _ in range(n):
                    self._eval_block_arg(eargs[0], [])
                return None
            if selector == "to:do:":
                stop = eargs[0]
                blk = eargs[1]
                count = 0
                i = receiver
                while i <= stop:
                    if count >= self.MAX_LOOPS:
                        break
                    count += 1
                    self._eval_block_arg(blk, [i])
                    i += 1
                return None
            if selector == "to:by:do:":
                stop = eargs[0]
                step = eargs[1]
                blk = eargs[2]
                count = 0
                i = receiver
                step_v = _to_num(step)
                stop_v = _to_num(stop)
                while (step_v > 0 and i <= stop_v) or (step_v < 0 and i >= stop_v):
                    if count >= self.MAX_LOOPS:
                        break
                    count += 1
                    self._eval_block_arg(blk, [i])
                    i += step_v
                return None
            if selector == "to:collect:":
                stop = int(eargs[0])
                blk = eargs[1]
                result = []
                for i in range(int(receiver), stop + 1):
                    result.append(self._eval_block_arg(blk, [i]))
                return result
            if selector == "max:":
                return max(receiver, _to_num(eargs[0]))
            if selector == "min:":
                return min(receiver, _to_num(eargs[0]))
            if selector == "raisedTo:":
                return receiver ** _to_num(eargs[0])
            if selector == "rem:":
                d = _to_num(eargs[0])
                return receiver % d if d != 0 else None
            if selector == "quo:":
                d = _to_num(eargs[0])
                return int(receiver / d) if d != 0 else None
            if selector == "gcd:":
                import math as _math
                return _math.gcd(int(receiver), int(_to_num(eargs[0])))
            if selector == "bitAnd:":
                return int(receiver) & int(_to_num(eargs[0]))
            if selector == "bitOr:":
                return int(receiver) | int(_to_num(eargs[0]))
            if selector == "bitShift:":
                shift = int(_to_num(eargs[0]))
                if shift >= 0:
                    return int(receiver) << shift
                return int(receiver) >> (-shift)
            if selector == "printString:":
                base = int(_to_num(eargs[0]))
                return format(int(receiver), f"0{base}")
            if selector == "printPaddedWith:to:":
                pad_char = str(eargs[0])
                width = int(_to_num(eargs[1]))
                return _st_print(receiver).lstrip("'").rstrip("'").rjust(width, pad_char)

        # String messages
        if isinstance(receiver, str):
            if selector == "copyFrom:to:":
                s = int(_to_num(eargs[0])) - 1
                e = int(_to_num(eargs[1]))
                return receiver[s:e]
            if selector == "indexOf:":
                try:
                    idx = receiver.index(str(eargs[0]))
                    return idx + 1
                except ValueError:
                    return 0
            if selector == "includes:":
                return str(eargs[0]) in receiver
            if selector == "replaceAll:with:":
                return receiver.replace(str(eargs[0]), str(eargs[1]))
            if selector == "startsWith:":
                return receiver.startswith(str(eargs[0]))
            if selector == "endsWith:":
                return receiver.endswith(str(eargs[0]))
            if selector == "substrings:":
                return receiver.split(str(eargs[0]))
            if selector == "substrings":
                return receiver.split()
            if selector == "matchPattern:":
                pattern = str(eargs[0]).replace("*", ".*").replace("#", ".")
                return bool(re.fullmatch(pattern, receiver, re.IGNORECASE))

        # Collection messages
        if isinstance(receiver, (list, _STOrderedCollection)):
            items = receiver.items if isinstance(receiver, _STOrderedCollection) else receiver
            is_oc = isinstance(receiver, _STOrderedCollection)

            if selector == "do:":
                for item in list(items):
                    self._eval_block_arg(eargs[0], [item])
                return None
            if selector == "collect:":
                result = [self._eval_block_arg(eargs[0], [x]) for x in items]
                return _STOrderedCollection(result) if is_oc else result
            if selector == "select:":
                result = [x for x in items if _is_truthy(self._eval_block_arg(eargs[0], [x]))]
                return _STOrderedCollection(result) if is_oc else result
            if selector == "reject:":
                result = [x for x in items if not _is_truthy(self._eval_block_arg(eargs[0], [x]))]
                return _STOrderedCollection(result) if is_oc else result
            if selector == "detect:":
                for x in items:
                    if _is_truthy(self._eval_block_arg(eargs[0], [x])):
                        return x
                return None
            if selector == "inject:into:":
                acc = eargs[0]
                blk = eargs[1]
                for x in items:
                    acc = self._eval_block_arg(blk, [acc, x])
                return acc
            if selector == "at:":
                idx = int(_to_num(eargs[0])) - 1  # 1-based
                return items[idx] if 0 <= idx < len(items) else None
            if selector == "at:put:":
                idx = int(_to_num(eargs[0])) - 1
                val = eargs[1]
                if is_oc:
                    while len(receiver.items) <= idx:
                        receiver.items.append(None)
                    receiver.items[idx] = val
                else:
                    while len(receiver) <= idx:
                        receiver.append(None)
                    receiver[idx] = val
                return val
            if selector == "add:":
                val = eargs[0]
                if is_oc:
                    receiver.items.append(val)
                else:
                    receiver.append(val)
                return val
            if selector == "addFirst:":
                val = eargs[0]
                if is_oc:
                    receiver.items.insert(0, val)
                else:
                    receiver.insert(0, val)
                return val
            if selector == "addLast:":
                val = eargs[0]
                if is_oc:
                    receiver.items.append(val)
                else:
                    receiver.append(val)
                return val
            if selector == "remove:":
                val = eargs[0]
                try:
                    if is_oc:
                        receiver.items.remove(val)
                    else:
                        receiver.remove(val)
                except ValueError:
                    pass
                return val
            if selector == "remove:ifAbsent:":
                val = eargs[0]
                try:
                    if is_oc:
                        receiver.items.remove(val)
                    else:
                        receiver.remove(val)
                    return val
                except ValueError:
                    return self._eval_block_arg(eargs[1], [])
            if selector == "includes:":
                return eargs[0] in items
            if selector == "with:":
                return _STOrderedCollection([eargs[0]])
            if selector == "copyFrom:to:":
                s = int(_to_num(eargs[0])) - 1
                e = int(_to_num(eargs[1]))
                result = items[s:e]
                return _STOrderedCollection(result) if is_oc else result
            if selector == "indexOf:":
                try:
                    return items.index(eargs[0]) + 1
                except ValueError:
                    return 0
            if selector == "allSatisfy:":
                return all(_is_truthy(self._eval_block_arg(eargs[0], [x])) for x in items)
            if selector == "anySatisfy:":
                return any(_is_truthy(self._eval_block_arg(eargs[0], [x])) for x in items)
            if selector == "doWithIndex:":
                for idx, item in enumerate(items, 1):
                    self._eval_block_arg(eargs[0], [item, idx])
                return None
            if selector in ("with:collect:", "withIndex:"):
                pass

        # Class constructors
        if isinstance(receiver, str) or receiver is None:
            name = receiver if isinstance(receiver, str) else ""
            if name == "OrderedCollection" and selector == "new":
                return _STOrderedCollection()
            if name == "OrderedCollection" and selector == "new:":
                return _STOrderedCollection()
            if name == "OrderedCollection" and selector == "with:":
                return _STOrderedCollection([eargs[0]])
            if name == "OrderedCollection" and selector == "withAll:":
                return _STOrderedCollection(list(eargs[0]) if eargs[0] else [])
            if name == "Array" and selector == "new:":
                return [None] * int(_to_num(eargs[0]))
            if name == "Array" and selector == "with:":
                return [eargs[0]]
            if name == "Array" and selector in ("with:with:", "with:with:with:"):
                return list(eargs)
            if name == "String" and selector == "new:":
                return " " * int(_to_num(eargs[0]))
            if name == "Character" and selector == "value:":
                try:
                    return chr(int(_to_num(eargs[0])))
                except Exception:
                    return ""

        # Turtle graphics
        t = self._turtle
        recv_name = receiver if isinstance(receiver, str) else ""
        if recv_name in ("Turtle", "turtle") or receiver is t:
            tbl: Dict[str, Any] = {
                "forward:": lambda a: t.forward(_to_num(a[0])),
                "backward:": lambda a: t.backward(_to_num(a[0])),
                "fd:": lambda a: t.forward(_to_num(a[0])),
                "bk:": lambda a: t.backward(_to_num(a[0])),
                "left:": lambda a: t.left(_to_num(a[0])),
                "right:": lambda a: t.right(_to_num(a[0])),
                "penUp": lambda a: t.pen_up(),
                "penDown": lambda a: t.pen_down(),
                "home": lambda a: t.home(),
                "setheading:": lambda a: t.set_heading(_to_num(a[0])),
            }
            if selector in tbl:
                tbl[selector](eargs)
                return receiver
            if selector == "color:red:green:blue:":
                t.set_color(int(_to_num(eargs[0])), int(_to_num(eargs[1])), int(_to_num(eargs[2])))
                return receiver

        # Generic class methods
        recv_class = receiver if isinstance(receiver, str) else type(receiver).__name__
        if selector == "new":
            return None
        if selector == "print:":
            self._emit(_st_display(eargs[0]))
            return receiver

        return None

    def _send_message(self, receiver: Any, msg: str) -> Any:
        """Dispatch an additional cascade message."""
        msg = msg.strip()
        # Keyword
        receiver_str = "Transcript" if receiver is _UNDEF else str(receiver)
        kw_match = self._parse_keyword_send(receiver_str + " " + msg)
        if kw_match:
            _, selector, kw_args = kw_match
            return self._send_keyword(receiver, selector, kw_args)
        # Binary
        m = re.match(r"^([+\-*/<>=,~&|@%\\!?]+|//|\\\\\\\\.)\s*(.+)$", msg)
        if m:
            arg = self._eval_expr(m.group(2).strip())
            return self._send_binary(receiver, m.group(1), arg)
        # Unary
        return self._send_unary(receiver, msg)

    # ------------------------------------------------------------------
    # Block evaluation
    # ------------------------------------------------------------------

    def _eval_block(self, body: str, args: List[Any]) -> Any:
        body = body.strip()
        # Extract parameters: [:a :b | ...]
        params: List[str] = []
        m = re.match(r"^\s*((?::\w+\s*)+)\|(.*)$", body, re.DOTALL)
        if m:
            params = [p[1:] for p in m.group(1).split()]
            body = m.group(2).strip()

        # Save only block-parameter bindings (they're local to the block)
        saved_params: Dict[str, Any] = {}
        for i, p in enumerate(params):
            saved_params[p] = self._vars.get(p, _UNDEF)
            self._vars[p] = args[i] if i < len(args) else None

        result: Any = None
        stmts = _tokenise_source(body)
        try:
            for stmt in stmts:
                stmt = stmt.strip()
                if not stmt:
                    continue
                result = self._exec_stmt(stmt)
        except _STReturn as r:
            result = r.value

        # Restore ONLY block-parameter bindings (outer vars stay modified)
        for p in params:
            old = saved_params.get(p, _UNDEF)
            if old is _UNDEF:
                self._vars.pop(p, None)
            else:
                self._vars[p] = old

        return result

    def _eval_block_arg(self, blk: Any, args: List[Any]) -> Any:
        if isinstance(blk, tuple) and blk[0] == "__block__":
            return self._eval_block(blk[1], args)
        return blk


# ---------------------------------------------------------------------------
# Display helpers
# ---------------------------------------------------------------------------


def _st_display(v: Any) -> str:
    """User-facing string (no surrounding quotes for strings)."""
    if v is None:
        return "nil"
    if isinstance(v, bool):
        return "true" if v else "false"
    if isinstance(v, str):
        return v
    if isinstance(v, float) and v == int(v):
        return str(int(v))
    if isinstance(v, _STOrderedCollection):
        return v.print_string()
    if isinstance(v, list):
        inner = " ".join(_st_print(x) for x in v)
        return f"({inner} )"
    return str(v)
