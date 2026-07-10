"""Helpers for persisting editor breakpoints across sessions."""

from __future__ import annotations

from typing import Any


def encode_breakpoints(
    breakpoints: set[int], conditions: dict[int, str] | None = None
) -> list[dict[str, Any]]:
    """Convert editor breakpoint state into a QSettings-friendly payload."""
    conditions = conditions or {}
    payload: list[dict[str, Any]] = []
    for line in sorted(int(line) for line in breakpoints):
        entry: dict[str, Any] = {"line": line}
        condition = conditions.get(line, "").strip()
        if condition:
            entry["condition"] = condition
        payload.append(entry)
    return payload


def decode_breakpoints(raw: Any) -> tuple[set[int], dict[int, str]]:
    """Decode persisted breakpoint state.

    Accepts both the new structured payload and the legacy list-of-lines format.
    """
    if not raw:
        return set(), {}

    if isinstance(raw, (list, tuple)):
        if raw and isinstance(raw[0], dict):
            lines: set[int] = set()
            conditions: dict[int, str] = {}
            for item in raw:
                if not isinstance(item, dict):
                    continue
                line = item.get("line")
                if not str(line).isdigit():
                    continue
                line_number = int(line)
                lines.add(line_number)
                condition = str(item.get("condition", "")).strip()
                if condition:
                    conditions[line_number] = condition
            return lines, conditions

        return {int(n) for n in raw if str(n).isdigit()}, {}

    return set(), {}