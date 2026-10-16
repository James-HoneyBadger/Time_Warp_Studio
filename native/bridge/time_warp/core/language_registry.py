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
    "BASIC": LanguageMetadata("BASIC", "basic", "line", ("bas", "bsc")),
    "PILOT": LanguageMetadata("PILOT", "pilot", "line", ("pilot", "plt")),
    "LOGO": LanguageMetadata("Logo", "logo", "line", ("logo", "lg")),
    "COMAL": LanguageMetadata("COMAL", "comal", "line", ("comal", "cml")),
    "REXX": LanguageMetadata("REXX", "rexx", "line", ("rexx", "rex")),
    "FOCAL": LanguageMetadata("FOCAL", "focal", "line", ("foc", "focal")),
    "CHIP8": LanguageMetadata("CHIP-8", "chip8", "whole", ("ch8", "c8")),
    "APL": LanguageMetadata("APL", "apl", "line", ("apl", "dyalog")),
    "SCHEME": LanguageMetadata(
        "Scheme", "scheme", "whole", ("scm", "ss", "scheme")
    ),
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