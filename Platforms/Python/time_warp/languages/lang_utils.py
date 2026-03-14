"""Shared utilities for language executors.

Extracts common patterns that were previously duplicated across multiple
executor files.
"""

from __future__ import annotations

import re
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
