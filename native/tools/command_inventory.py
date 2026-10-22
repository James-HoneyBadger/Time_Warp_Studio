#!/usr/bin/env python3
"""Extract the command literals each language executor dispatches on.

This is the *implemented* side of the command inventory, read out of the code so
it cannot inherit the documentation's optimism. It is a **candidate generator,
not a verdict**: a dispatch literal can also be a syntax character (`op == ","`)
or an operator (`<=`), and no machine can tell a command from a punctuation mark
by shape alone. The command table in `native/tools/command_table.py` is the
curated answer; this exists so that "not tested" is measured against something
rather than assumed.

#### Why it reads C++

It parsed the Python executors with `ast` while those existed. The engines that
ship are the C++ ones in `native/src/lang/`, so measuring completeness anywhere
else measured the thing that was deleted. The C++ has no single dispatch idiom, so
the scan recognises the four shapes this codebase actually uses:

  * ``cmd == "PRINT"``                    equality against a command variable
  * ``name == "FORWARD" || name == "FD"`` either side of a comparison chain
  * ``starts_with_upper(text, "TO ")``     prefix dispatch
  * ``{"FORWARD", "FD", "BACK", ...}``     brace lists of literals, which is how
                                           Logo and COMAL name their words

A regex scan is a blunt instrument, and blunter than an AST walk was: a literal in
a *message* (`"not an instruction"`) can appear here as a candidate. That is
accepted rather than hidden -- the tool says "candidate", the curated table is the
record, and a false candidate costs one waiver review while a missed command costs
an untested command.

Usage:
    python3 native/tools/command_inventory.py                 # counts per language
    python3 native/tools/command_inventory.py --language logo # one list
"""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys

LANG_SOURCES = pathlib.Path(__file__).resolve().parents[1] / "src" / "lang"

#: Which source files belong to which language. Scheme is not in `src/lang/`: it is
#: the separate machine in `src/scheme/`, and its dispatch is the special-form table
#: there.
SOURCES = {
    "basic": ("basic.cpp", "basic_statements.cpp", "basic_text.cpp",
              "basic_features.cpp", "basic_library.cpp"),
    "pilot": ("pilot.cpp",),
    "logo": ("logo.cpp",),
    "comal": ("comal.cpp",),
    "rexx": ("rexx.cpp",),
    "focal": ("focal.cpp",),
    "apl": ("apl.cpp",),
    "smalltalk": ("smalltalk.cpp", "smalltalk_library.cpp", "smalltalk_driver.cpp"),
    "chip8": ("chip8.cpp",),
    "forth": ("forth.cpp",),
}

#: Names that hold "the thing being dispatched on" in the executors.
COMMAND_VARIABLES = {
    "cmd",
    "cmd_name",
    "cmd_type",
    "command",
    "command_type",
    "kw",
    "keyword",
    # APL dispatches on the statement's leading glyph or word; CHIP-8 on the
    # mnemonic the assembler split off; Smalltalk compares a whole send against a
    # selector name; COMAL upper-cases the statement into `up` and compares that.
    # All of them are "the thing being dispatched on".
    "glyph",
    "mnemonic",
    "up",
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
        "TYPE$(",  # the same guard, with its call parenthesis attached
        "Undefined variable:",  # an error message compared against, not a command
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
    if any(character.isalpha() for character in text):
        return True
    # APL's commands *are* symbols: `⍳` is index-of and `⌽` is reverse, and a rule
    # that wanted a letter called the whole language untested. Anything outside
    # ASCII is taken as a word rather than as punctuation; punctuation in these
    # sources is ASCII.
    return any(ord(character) > 127 for character in text)


#: `cmd == "PRINT"`, `name == "FORWARD"`, `op != "<="`, either way round. The left
#: side has to be one of `COMMAND_VARIABLES` so that `status == "ok"` is not a
#: candidate.
_EQUALITY = re.compile(
    r'(?:\b(\w+)\s*[!=]=\s*"([^"\n]{1,40})")|(?:"([^"\n]{1,40})"\s*[!=]=\s*(\w+)\b)'
)
#: `starts_with_upper(line, "TO ")` and its bare `starts_with` sibling.
_PREFIX = re.compile(r'\b(?:starts_with_upper|starts_with)\s*\(\s*[^,()]+,\s*"([^"\n]{1,40})"')
#: `add(result.object, "isKindOf:", {"aClass"}, ...)` -- Smalltalk registers a
#: message send as the first string argument of a call to `add`, because its
#: "commands" are selectors in a class library rather than literals compared in a
#: switch.
_REGISTRATION = re.compile(r'\badd\s*\(\s*[^,()]+,\s*"([^"\n]{1,40})"')
#: Forth's dictionary is a table of `{name, Primitive::X}` pairs rather than a
#: switch on a string, so the equality scan sees only the control words. One
#: dedicated pattern reads the primitive table; the pairs interleave identifiers
#: with the quotes, which is why `_TABLE` cannot.
_FORTH_PAIR = re.compile(r'\{"([^"\n]{1,40})",\s*Primitive::\w+\}')

#: A brace list made *only* of quoted literals: `{"FORWARD", "FD", "BACK"}`.
#: The stricter shape matters: `{ throw StError("super sent " + selector + ", but no
#: superclass of " + ...) }` is a code block, and a rule that only asked for a brace
#: group containing three quoted strings harvested its message clauses as commands.
_TABLE = re.compile(r'\{\s*((?:"[^"\n]{1,40}"\s*,?\s*){3,})\}')
_QUOTED = re.compile(r'"([^"\n]{1,40})"')

#: A literal that is really a C++ string in a message rather than a command. Only
#: used to keep the *report* readable; `is_command_literal` does the deciding.
_MESSAGE_HINTS = (" ", ":", "'", "(", ")", "%", "\\n")


def scan_source(text: str) -> dict[str, set[str] | dict[str, list[str]]]:
    """The candidate literals in one source file, by the shape that produced them."""
    equality: set[str] = set()
    prefix: set[str] = set()
    tables: dict[str, list[str]] = {}

    for match in _EQUALITY.finditer(text):
        variable, right = match.group(1), match.group(2)
        left, value = match.group(3), match.group(4)
        if variable is not None:
            if variable in COMMAND_VARIABLES and right is not None:
                equality.add(right)
        elif left is not None and value in COMMAND_VARIABLES:
            equality.add(left)

    for match in _PREFIX.finditer(text):
        prefix.add(match.group(1))

    for match in _REGISTRATION.finditer(text):
        equality.add(match.group(1))

    for match in _FORTH_PAIR.finditer(text):
        equality.add(match.group(1))

    for match in _TABLE.finditer(text):
        literals = _QUOTED.findall(match.group(1))
        # Three or more in one brace list is a word table; two is a pair of
        # alternatives written inline, which equality already found.
        if len(literals) >= 3:
            tables[literals[0]] = sorted(set(literals))

    return {"equality": equality, "prefix": prefix, "tables": tables}


def collect(sources: dict[str, tuple[str, ...]] | None = None) -> dict[str, dict[str, object]]:
    """The candidate command inventory, keyed by language."""
    sources = sources if sources is not None else SOURCES
    inventory: dict[str, dict[str, object]] = {}
    for language, filenames in sources.items():
        equality: set[str] = set()
        prefix: set[str] = set()
        tables: dict[str, list[str]] = {}
        for filename in filenames:
            path = LANG_SOURCES / filename
            if not path.is_file():
                continue
            scanned = scan_source(path.read_text(encoding="utf-8"))
            equality |= scanned["equality"]
            prefix |= scanned["prefix"]
            for name, values in scanned["tables"].items():
                tables[f"{filename}:{name}"] = values

        dispatched = {text.strip() for text in equality | prefix}
        from_tables: set[str] = set()
        for values in tables.values():
            from_tables.update(values)

        inventory[language] = {
            "dispatch_literals": sorted(
                name for name in dispatched if name and is_command_literal(name)
            ),
            "not_commands": sorted(
                name for name in dispatched if name and not is_command_literal(name)
            ),
            "tables": tables,
            # A brace list is not automatically a command table: Logo's operator
            # table holds "%", "+" and "<=", which are not commands and would
            # otherwise sit in the report forever as untested.
            "table_commands": sorted(
                name for name in from_tables if is_command_literal(name)
            ),
        }
    return inventory


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--language", help="print this language's candidates")
    parser.add_argument(
        "--json", action="store_true", help="emit JSON instead of a summary"
    )
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
