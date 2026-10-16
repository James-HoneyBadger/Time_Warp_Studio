#!/usr/bin/env python3
"""Extract the command literals each language executor dispatches on.

This is the *implemented* side of the command inventory, read out of the code so
it cannot inherit the documentation's optimism. It is a **candidate generator,
not a verdict**: a dispatch literal can also be a syntax character (`op == ","`
in the Scheme reader) or an operator (`<=` in COMAL), and no machine can tell a
command from a punctuation mark by shape alone. The command table in
`native/bridge/time_warp/tests/command_table.py` is the curated answer; this
exists so that "not tested" is measured against something rather than assumed.

Four dispatch styles are recognised:
  * ``cmd == "PRINT"``             equality against a command variable
  * ``cmd.startswith("FRACTAL ")`` prefix dispatch
  * ``cmd in ("A", "B")``          membership tests
  * ``NAME = {"A", "B"}``          module-level command tables

Usage:
    python3 native/tools/command_inventory.py                 # counts per language
    python3 native/tools/command_inventory.py --language logo # one list
"""

from __future__ import annotations

import argparse
import ast
import json
import pathlib
import sys

LANGUAGES = pathlib.Path(__file__).resolve().parents[1] / "bridge" / "time_warp" / "languages"

#: Names that hold "the thing being dispatched on" in the executors.
COMMAND_VARIABLES = {
    "cmd",
    "cmd_name",
    "cmd_type",
    "command",
    "command_type",
    "kw",
    "keyword",
    "name",
    "op",
    "stmt",
    "statement",
    "token",
    "unit",
    "verb",
}

#: Module-level names that are command tables rather than data.
TABLE_HINTS = ("COMMAND", "COMMANDS", "KEYWORDS", "OPS", "OPERATORS", "VERBS")

#: Literals a dispatcher compares against that are not commands. The comparison
#: harvests these because the same variable sometimes holds a token rather than a
#: statement, and a few are no-space spellings of a real command. Kept as an
#: explicit list so the completeness figure counts commands, and so "not a
#: command" is a written decision rather than a silent skip.
NOT_COMMANDS = frozenset(
    {
        "\u274c",  # the error prefix every executor returns
        "ENDTYPE",  # no-space spelling of END TYPE, which has its own row
        "TYPE$",  # only ever a guard ("not cmd.startswith('TYPE$')"), never a dispatch
        "#T",  # Scheme booleans: values with a reader syntax, not commands
        "#F",
        "#TRUE",
        "#FALSE",
        # Smalltalk's lexer token kinds and its three literals. `token.kind ==
        # "ident"` and `token.text == "self"` are the parser asking about shapes,
        # not dispatch on a selector, and a message send never looks like any of
        # these.
        "binary",
        "char",
        "hash-array",
        "ident",
        "keyword",
        "number",
        "self",
        "super",
        "symbol",
        "true",
        "false",
        "nil",
        # The CHIP-8 number prefix, which `_number` tests for so that 0x41 is 65.
        "0x",
    }
)

#: NOT_COMMANDS, upper-cased once. `is_command_literal` compares against
#: `literal.upper()`, so a lowercase entry in the set above ("nil", "false",
#: "binary") would never match and would read as a written decision while doing
#: nothing. Keeping both spellings apart means the set above can stay in the case
#: the language actually writes.
_NOT_COMMANDS_UPPER = frozenset(name.upper() for name in NOT_COMMANDS)


def is_command_literal(literal: str) -> bool:
    """Whether a harvested literal names a command rather than punctuation.

    Punctuation and operators (``"``, ``#``, ``+``, ``<=``) are what a token
    comparison looks like; a command has a letter in it. A leading colon is the
    statement separator, so ``: REM`` is the REM command written after one.
    """
    text = literal.strip().lstrip(":").strip()
    if not text or literal.strip().upper() in _NOT_COMMANDS_UPPER:
        return False
    return any(character.isalpha() for character in text)


def _text(node: ast.AST) -> str | None:
    if isinstance(node, ast.Constant) and isinstance(node.value, str):
        return node.value
    return None


class _Finder(ast.NodeVisitor):
    def __init__(self) -> None:
        self.equality: set[str] = set()
        self.prefix: set[str] = set()
        self.membership: set[str] = set()
        self.tables: dict[str, list[str]] = {}

    def visit_Compare(self, node: ast.Compare) -> None:
        operands = [node.left, *node.comparators]
        for index, operator in enumerate(node.ops):
            if not isinstance(operator, (ast.Eq, ast.In)):
                continue
            left, right = operands[index], operands[index + 1]
            names = {n.id for n in ast.walk(left) if isinstance(n, ast.Name)}
            if not (names & COMMAND_VARIABLES):
                continue
            if isinstance(operator, ast.Eq):
                for candidate in (right, left):
                    value = _text(candidate)
                    if value:
                        self.equality.add(value)
            else:
                for element in getattr(right, "elts", []):
                    value = _text(element)
                    if value:
                        self.membership.add(value)
        self.generic_visit(node)

    def visit_Call(self, node: ast.Call) -> None:
        func = node.func
        if isinstance(func, ast.Attribute) and func.attr == "startswith" and node.args:
            value = _text(node.args[0])
            if value:
                self.prefix.add(value)
        self.generic_visit(node)

    def visit_Assign(self, node: ast.Assign) -> None:
        for target in node.targets:
            if not isinstance(target, ast.Name):
                continue
            if not any(hint in target.id.upper() for hint in TABLE_HINTS):
                continue
            values = [v for v in (_text(e) for e in getattr(node.value, "elts", [])) if v]
            if values:
                self.tables[target.id] = sorted(values)
        self.generic_visit(node)


def collect(languages: pathlib.Path = LANGUAGES) -> dict[str, dict[str, object]]:
    """The candidate command inventory, keyed by module name."""
    inventory: dict[str, dict[str, object]] = {}
    for path in sorted(languages.glob("*.py")):
        if path.name in {"__init__.py", "base.py"}:
            continue
        finder = _Finder()
        finder.visit(ast.parse(path.read_text(encoding="utf-8")))

        dispatched = {text.strip() for text in finder.equality | finder.membership}
        dispatched |= {text.strip() for text in finder.prefix}
        from_tables: set[str] = set()
        for values in finder.tables.values():
            from_tables.update(values)

        inventory[path.stem] = {
            "dispatch_literals": sorted(
                name for name in dispatched if name and is_command_literal(name)
            ),
            "not_commands": sorted(
                name for name in dispatched if name and not is_command_literal(name)
            ),
            "tables": finder.tables,
            # A module-level table is not automatically a command table: Logo's
            # operator table holds "%", "+" and "<=", which are not commands and
            # would otherwise sit in the report forever as untested.
            "table_commands": sorted(
                name for name in from_tables if is_command_literal(name)
            ),
        }
    return inventory


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--language", help="print this language's candidates")
    parser.add_argument("--json", action="store_true", help="emit JSON instead of a summary")
    options = parser.parse_args(argv)

    inventory = collect()

    if options.language:
        entry = inventory.get(options.language.lower())
        if entry is None:
            print(f"no such language: {options.language}", file=sys.stderr)
            return 1
        print(", ".join(entry["dispatch_literals"]))
        return 0

    if options.json:
        json.dump(inventory, sys.stdout, indent=1, sort_keys=True)
        return 0

    print(f"{'language':10s} {'dispatched':>11s} {'in tables':>10s} {'union':>7s}")
    for language, entry in sorted(inventory.items()):
        dispatched = set(entry["dispatch_literals"])
        tables = set(entry["table_commands"])
        print(
            f"{language:10s} {len(dispatched):11d} {len(tables):10d} "
            f"{len(dispatched | tables):7d}"
        )
    print()
    print("Candidates only: punctuation and operators appear here too. The curated")
    print("table and its waivers are the record of what is actually exercised.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
