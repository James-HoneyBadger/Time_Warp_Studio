"""Shared utilities for language executors.

Extracts common patterns that were previously duplicated across multiple
executor files.
"""

from __future__ import annotations

from typing import Any, List


def upper_preserve_strings(line: str) -> str:
    """Uppercase source code but preserve quoted string literals.

    Used by Assembly, COBOL, and Fortran executors to normalise keywords
    while leaving string content untouched.

    >>> upper_preserve_strings('move "Hello World" to ws-name')
    'MOVE "Hello World" TO WS-NAME'
    """
    result: list[str] = []
    in_str = False
    qchar = ""
    for ch in line:
        if in_str:
            result.append(ch)
            if ch == qchar:
                in_str = False
        elif ch in ('"', "'"):
            in_str = True
            qchar = ch
            result.append(ch)
        else:
            result.append(ch.upper())
    return "".join(result)


def split_args(text: str, sep: str = ",") -> List[str]:
    """Split argument string respecting parentheses and string literals.

    Correctly handles nested parentheses and quoted strings so that
    ``split_args("foo(a,b), bar")`` returns ``["foo(a,b)", "bar"]``.
    """
    args: list[str] = []
    depth = 0
    current: list[str] = []
    in_str = False
    qchar = ""

    for ch in text:
        if in_str:
            current.append(ch)
            if ch == qchar:
                in_str = False
            continue
        if ch in ('"', "'"):
            in_str = True
            qchar = ch
            current.append(ch)
            continue
        if ch == "(":
            depth += 1
            current.append(ch)
        elif ch == ")":
            depth -= 1
            current.append(ch)
        elif ch == sep and depth == 0:
            args.append("".join(current).strip())
            current = []
        else:
            current.append(ch)

    tail = "".join(current).strip()
    if tail:
        args.append(tail)
    return args


def unquote(s: str) -> str:
    """Remove surrounding single or double quotes from a string.

    >>> unquote('"hello"')
    'hello'
    >>> unquote("'world'")
    'world'
    >>> unquote('plain')
    'plain'
    """
    if len(s) >= 2 and s[0] == s[-1] and s[0] in ('"', "'"):
        return s[1:-1]
    return s


def to_num(val: Any) -> float:
    """Coerce an arbitrary value to float, returning 0.0 on failure."""
    try:
        return float(val)
    except (TypeError, ValueError):
        return 0.0


def to_num_or_str(val: Any):
    """Coerce to float if possible, otherwise return str."""
    try:
        return float(val)
    except (TypeError, ValueError):
        return str(val)


class LanguageError(Exception):
    """Base exception class for language executor errors."""


class LanguageStop(Exception):
    """Base exception for normal program termination."""


class StructuredError(LanguageError):
    """Executor error with source location and plain-English hint.

    All new executor error paths should raise or return this so the main
    window can highlight the offending line in the editor and show a hint.

    Attributes:
        message:  Short human-readable description of the error.
        line:     1-based source line number, or ``None`` if unknown.
        column:   0-based column offset, or ``None`` if unknown.
        hint:     Optional longer explanation / "did you mean?" suggestion.

    >>> str(StructuredError("Undefined variable", line=5, hint="Did you declare X?"))
    "❌ Line 5: Undefined variable — Did you declare X?"
    """

    def __init__(
        self,
        message: str,
        *,
        line: int | None = None,
        column: int | None = None,
        hint: str | None = None,
    ) -> None:
        self.message = message
        self.line = line
        self.column = column
        self.hint = hint
        super().__init__(str(self))

    def __str__(self) -> str:
        prefix = f"❌ Line {self.line}: " if self.line is not None else "❌ "
        body = prefix + self.message
        if self.hint:
            body += f" — {self.hint}"
        return body

    def format_output(self) -> str:
        """Return the full error string suitable for the output panel."""
        return str(self)


# ---------------------------------------------------------------------------
# Tokenizer helpers
# ---------------------------------------------------------------------------

def tokenize(line: str) -> list[str]:
    """Split *line* into tokens, respecting quoted strings.

    Tokens are whitespace-separated.  Quoted strings (single or double quotes)
    are kept as a single token, including the surrounding quotes.

    >>> tokenize('PRINT "Hello World" 42')
    ['PRINT', '"Hello World"', '42']
    """
    tokens: list[str] = []
    current: list[str] = []
    in_str = False
    qchar = ""
    for ch in line:
        if in_str:
            current.append(ch)
            if ch == qchar:
                in_str = False
        elif ch in ('"', "'"):
            in_str = True
            qchar = ch
            current.append(ch)
        elif ch.isspace():
            if current:
                tokens.append("".join(current))
                current = []
        else:
            current.append(ch)
    if current:
        tokens.append("".join(current))
    return tokens


# ---------------------------------------------------------------------------
# Loop/stack helpers
# ---------------------------------------------------------------------------

class LoopStack:
    """Generic stack for tracking nested loop contexts.

    Used by line-by-line executors (BASIC, Logo, Pascal, Forth …) to record
    pending loop structures without an explicit AST.

    Example usage in a FOR-loop executor::

        loops = LoopStack()
        loops.push({"type": "FOR", "var": "I", "limit": 10, "line": 5})
        ...
        ctx = loops.peek()
        if ...:
            loops.pop()
    """

    def __init__(self) -> None:
        self._stack: list = []

    def push(self, ctx: Any) -> None:
        """Push a new loop context onto the stack."""
        self._stack.append(ctx)

    def pop(self) -> Any:
        """Pop and return the topmost context.  Raises IndexError if empty."""
        return self._stack.pop()

    def peek(self) -> Any | None:
        """Return the topmost context without removing it, or None if empty."""
        return self._stack[-1] if self._stack else None

    def is_empty(self) -> bool:
        """Return True when no open loops are recorded."""
        return len(self._stack) == 0

    def __len__(self) -> int:
        return len(self._stack)

    def clear(self) -> None:
        """Remove all contexts."""
        self._stack.clear()


class VariableScope:
    """Simple two-level variable scope (global + one local level).

    Provides a dict-like interface for reading and writing variables.
    Local scope is activated via ``push_local()`` and exited with
    ``pop_local()``.

    >>> scope = VariableScope({"X": 1})
    >>> scope["Y"] = 2
    >>> scope.push_local({"X": 99})
    >>> scope["X"]
    99
    >>> scope.pop_local()
    >>> scope["X"]
    1
    """

    def __init__(self, initial: dict | None = None) -> None:
        self._globals: dict = dict(initial or {})
        self._locals: list[dict] = []

    def push_local(self, bindings: dict | None = None) -> None:
        """Enter a new local scope with optional initial bindings."""
        self._locals.append(dict(bindings or {}))

    def pop_local(self) -> dict:
        """Exit the current local scope and return its bindings."""
        if self._locals:
            return self._locals.pop()
        return {}

    def __getitem__(self, key: str) -> Any:
        if self._locals and key in self._locals[-1]:
            return self._locals[-1][key]
        return self._globals[key]

    def __setitem__(self, key: str, value: Any) -> None:
        if self._locals:
            self._locals[-1][key] = value
        else:
            self._globals[key] = value

    def set_global(self, key: str, value: Any) -> None:
        """Always write to the global scope, even inside a local scope."""
        self._globals[key] = value

    def get(self, key: str, default: Any = None) -> Any:
        """Return the value for *key* or *default* if not found."""
        try:
            return self[key]
        except KeyError:
            return default

    def __contains__(self, key: str) -> bool:
        if self._locals and key in self._locals[-1]:
            return True
        return key in self._globals

    def as_flat_dict(self) -> dict:
        """Return a merged snapshot: local values shadow globals."""
        result = dict(self._globals)
        for scope in self._locals:
            result.update(scope)
        return result


def did_you_mean(name: str, candidates: list[str], max_dist: int = 2) -> str | None:
    """Return the closest match to *name* from *candidates*, or None.

    Uses simple Levenshtein distance.  Returns None if no candidate is within
    *max_dist* edit distance.

    >>> did_you_mean("PRNT", ["PRINT", "INPUT", "LET"])
    'PRINT'
    """
    def _dist(a: str, b: str) -> int:
        if a == b:
            return 0
        rows, cols = len(a) + 1, len(b) + 1
        dp = list(range(cols))
        for i in range(1, rows):
            prev = dp[:]
            dp[0] = i
            for j in range(1, cols):
                cost = 0 if a[i - 1] == b[j - 1] else 1
                dp[j] = min(dp[j - 1] + 1, prev[j] + 1, prev[j - 1] + cost)
        return dp[-1]

    best: str | None = None
    best_d = max_dist + 1
    for cand in candidates:
        d = _dist(name.upper(), cand.upper())
        if d < best_d:
            best_d = d
            best = cand
    return best if best_d <= max_dist else None
