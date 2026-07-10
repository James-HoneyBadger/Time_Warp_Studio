#!/usr/bin/env python3
"""Inventory the interpreter surface area for Time Warp Studio.

This script is a review aid: it scans every language executor module, extracts
its public and command-oriented symbols, and pairs each language with its
corresponding tests and example programs. The output is intended to support a
methodical review of every command, setting, and helper path.
"""

from __future__ import annotations

import argparse
import ast
import importlib
import json
import re
import sys
from dataclasses import dataclass, asdict
from pathlib import Path
from typing import Iterable


REPO_ROOT = Path(__file__).resolve().parents[1]
PYTHON_ROOT = REPO_ROOT / "Platforms" / "Python"
LANG_DIR = PYTHON_ROOT / "time_warp" / "languages"
TEST_DIR = PYTHON_ROOT / "time_warp" / "tests"
EXAMPLES_DIR = REPO_ROOT / "Examples"

sys.path.insert(0, str(PYTHON_ROOT))

core_mod = importlib.import_module("time_warp.core.interpreter")
Language = getattr(core_mod, "Language")


MODULE_LANGUAGE_ALIASES = {
    "c_lang_fixed": "C",
    "python_lang": "PYTHON_LANG",
}

MODULE_EXAMPLE_ALIASES = {
    "c_lang_fixed": "c",
    "python_lang": "python",
}


@dataclass(frozen=True)
class SymbolInfo:
    """A discovered symbol from a language module."""

    name: str
    kind: str
    line: int


@dataclass(frozen=True)
class ModuleAudit:
    """One language module and its review metadata."""

    module: str
    language: str
    executor: str | None
    symbol_count: int
    command_symbols: list[str]
    classes: list[str]
    tests: list[str]
    examples: list[str]


class _SymbolVisitor(ast.NodeVisitor):
    def __init__(self) -> None:
        self.symbols: list[SymbolInfo] = []
        self._class_stack: list[str] = []

    def visit_FunctionDef(self, node: ast.FunctionDef) -> None:
        kind = "method" if self._class_stack else "function"
        qualname = ".".join(self._class_stack + [node.name])
        self.symbols.append(SymbolInfo(name=qualname, kind=kind, line=node.lineno))
        self.generic_visit(node)

    def visit_AsyncFunctionDef(self, node: ast.AsyncFunctionDef) -> None:
        kind = "method" if self._class_stack else "async_function"
        qualname = ".".join(self._class_stack + [node.name])
        self.symbols.append(SymbolInfo(name=qualname, kind=kind, line=node.lineno))
        self.generic_visit(node)

    def visit_ClassDef(self, node: ast.ClassDef) -> None:
        self.symbols.append(SymbolInfo(name=node.name, kind="class", line=node.lineno))
        self._class_stack.append(node.name)
        self.generic_visit(node)
        self._class_stack.pop()


def _language_name_for_module(module_name: str) -> str:
    return MODULE_LANGUAGE_ALIASES.get(module_name, module_name.upper())


def _example_folder_for_module(module_name: str) -> str:
    return MODULE_EXAMPLE_ALIASES.get(module_name, module_name)


def _collect_symbols(path: Path) -> list[SymbolInfo]:
    tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
    visitor = _SymbolVisitor()
    visitor.visit(tree)
    return visitor.symbols


def _discover_tests() -> dict[str, list[str]]:
    language_tests: dict[str, list[str]] = {}
    for test_path in sorted(TEST_DIR.glob("test_lang_*.py")):
        content = test_path.read_text(encoding="utf-8")
        for language_name in sorted(set(re.findall(r"Language\.([A-Z0-9_]+)", content))):
            language_tests.setdefault(language_name, []).append(str(test_path.relative_to(REPO_ROOT)))
    return language_tests


def _discover_examples(folder: str) -> list[str]:
    folder_path = EXAMPLES_DIR / folder
    if not folder_path.exists():
        return []
    return [str(path.relative_to(REPO_ROOT)) for path in sorted(folder_path.iterdir()) if path.is_file()]


def _module_paths() -> Iterable[Path]:
    for path in sorted(LANG_DIR.glob("*.py")):
        if path.stem in {"__init__", "base", "lang_utils", "parser_patterns"}:
            continue
        yield path


def collect_inventory() -> list[ModuleAudit]:
    """Collect interpreter audit metadata for every language module."""
    language_tests = _discover_tests()
    inventory: list[ModuleAudit] = []
    for path in _module_paths():
        module_name = path.stem
        language_name = _language_name_for_module(module_name)
        symbols = _collect_symbols(path)
        executor_names = [sym.name for sym in symbols if sym.name.startswith("execute_")]
        command_symbols = [
            sym.name
            for sym in symbols
            if sym.name.startswith(("_cmd_", "_call_", "_eval_", "_exec_", "_try_"))
        ]
        classes = [sym.name for sym in symbols if sym.kind == "class"]
        inventory.append(
            ModuleAudit(
                module=module_name,
                language=language_name,
                executor=executor_names[0] if executor_names else None,
                symbol_count=len(symbols),
                command_symbols=command_symbols,
                classes=classes,
                tests=language_tests.get(language_name, []),
                examples=_discover_examples(_example_folder_for_module(module_name)),
            )
        )
    return inventory


def _format_markdown(inventory: list[ModuleAudit]) -> str:
    lines = ["# Interpreter Audit Inventory", ""]
    for entry in inventory:
        lines.append(f"## {entry.language} ({entry.module})")
        lines.append(f"- executor: {entry.executor or 'missing'}")
        lines.append(f"- symbols: {entry.symbol_count}")
        lines.append(f"- command helpers: {len(entry.command_symbols)}")
        lines.append(f"- classes: {', '.join(entry.classes) if entry.classes else 'none'}")
        lines.append(f"- tests: {', '.join(entry.tests) if entry.tests else 'none'}")
        lines.append(f"- examples: {len(entry.examples)} file(s)")
        lines.append("")
    return "\n".join(lines)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--format",
        choices=("markdown", "json"),
        default="markdown",
        help="Output format for the inventory.",
    )
    args = parser.parse_args(argv)

    inventory = collect_inventory()
    if args.format == "json":
        print(json.dumps([asdict(item) for item in inventory], indent=2, sort_keys=True))
    else:
        print(_format_markdown(inventory))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())