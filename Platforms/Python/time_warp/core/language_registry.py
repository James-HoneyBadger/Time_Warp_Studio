"""Canonical metadata for the active Time Warp Studio languages."""

from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True)
class LanguageMetadata:
    """User-facing and file-discovery metadata for one language."""

    display_name: str
    folder_name: str
    execution_mode: str
    extensions: tuple[str, ...]


LANGUAGE_METADATA: dict[str, LanguageMetadata] = {
    "BASIC": LanguageMetadata("BASIC", "basic", "line", ("bas",)),
    "PILOT": LanguageMetadata("PILOT", "pilot", "line", ("pilot",)),
    "LOGO": LanguageMetadata("Logo", "logo", "line", ("logo",)),
    "C": LanguageMetadata("C", "c", "line", ("c",)),
    "PROLOG": LanguageMetadata(
        "Prolog", "prolog", "line", ("pl", "pro", "prolog")
    ),
    "PASCAL": LanguageMetadata("Pascal", "pascal", "line", ("pas",)),
    "FORTH": LanguageMetadata("Forth", "forth", "line", ("f", "forth", "fs")),
    "BRAINFUCK": LanguageMetadata("Brainfuck", "brainfuck", "whole", ("bf",)),
    "PYTHON_LANG": LanguageMetadata("Python", "python", "whole", ("py",)),
}


def metadata_for(language_name: str) -> LanguageMetadata:
    """Return metadata for an enum member name."""
    return LANGUAGE_METADATA[language_name]


def language_name_for_extension(extension: str) -> str:
    """Return the active language name associated with a file extension."""
    normalized = extension.lower().lstrip(".")
    for language_name, metadata in LANGUAGE_METADATA.items():
        if normalized in metadata.extensions:
            return language_name
    return "BASIC"


def extensions_for(language_name: str) -> tuple[str, ...]:
    """Return all supported extensions for an active language."""
    return metadata_for(language_name).extensions