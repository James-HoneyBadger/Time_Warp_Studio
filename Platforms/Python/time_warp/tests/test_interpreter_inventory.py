from __future__ import annotations

from pathlib import Path
import importlib.util
import sys

from time_warp.core.interpreter import Language
from time_warp.core.language_registry import LANGUAGE_METADATA, extensions_for


SCRIPT_PATH = Path(__file__).resolve().parents[4] / "Scripts" / "audit_interpreters.py"
SPEC = importlib.util.spec_from_file_location("audit_interpreters", SCRIPT_PATH)
assert SPEC is not None and SPEC.loader is not None
audit_interpreters = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = audit_interpreters
SPEC.loader.exec_module(audit_interpreters)


def test_inventory_covers_every_language_module():
    inventory = audit_interpreters.collect_inventory()
    assert inventory, "Expected at least one interpreter module in the audit inventory"

    modules = {entry.module for entry in inventory}
    expected_modules = {
        "basic",
        "pilot",
        "logo",
        "c_lang_fixed",
        "prolog",
        "pascal",
        "forth",
        "brainfuck",
        "python_lang",
    }

    assert expected_modules.issubset(modules)

    for entry in inventory:
        assert entry.executor is not None, f"Missing executor in {entry.module}"
        assert entry.symbol_count > 0, f"No symbols found in {entry.module}"


def test_language_registry_matches_active_enum() -> None:
    assert set(LANGUAGE_METADATA) == {language.name for language in Language}


def test_language_registry_resolves_all_extensions() -> None:
    for language_name, metadata in LANGUAGE_METADATA.items():
        language = Language[language_name]
        assert language.friendly_name() == metadata.display_name
        assert metadata.folder_name
        assert metadata.execution_mode in {"line", "whole"}
        for extension in extensions_for(language_name):
            assert Language.from_extension(f".{extension}") is language