// SPDX-License-Identifier: MIT
//
// The languages this IDE runs, and how a file name maps onto one.
//
// Mirrors `Language` and `Language.from_extension()` in the Python
// implementation (`core/interpreter.py`), including the file extensions, so the
// two agree about what a `.logo` file is. The bridge that runs the unported
// languages keys off this, and so will a syntax highlighter when there is one:
// nothing in the tree recolours source today, and `docs/ROADMAP.md` records it
// as unported rather than merely unfinished.

#pragma once

#include <QList>
#include <QString>
#include <QStringList>

namespace timewarp::ui {

/// One of the six languages the IDE teaches, or `Unknown`.
///
/// The order matches the Python enum. New languages are append-only for the same
/// reason they are there: persisted state and saved sessions reference these by
/// identity, not by position.
enum class Language {
    Basic,
    Pilot,
    Logo,
    Comal,
    Rexx,
    Scheme,
    Focal,
    Chip8,
    Apl,
    Unknown,
};

/// Every real language, in the order the UI lists them (BASIC first).
[[nodiscard]] QList<Language> allLanguages();

/// The language a new, unsaved document starts as: BASIC, matching the Python
/// IDE. The Python registry's `language_name_for_extension` also falls back to
/// BASIC for an unrecognised extension; that fallback is the *caller's* decision
/// here, so `languageForFile` answers `Unknown` and reports the truth about the
/// file name instead of guessing.
[[nodiscard]] Language defaultLanguage();

/// The canonical name as the UI shows it: "BASIC", "Logo", "REXX", "Scheme".
[[nodiscard]] QString displayName(Language language);

/// Lowercase key used for settings, highlighting rules and protocol payloads:
/// "basic", "logo", "rexx".
[[nodiscard]] QString settingsKey(Language language);

/// Inverse of `settingsKey`. Returns `Unknown` for a key this build does not
/// know, which is what makes a settings file written by a newer build harmless.
[[nodiscard]] Language languageFromKey(const QString& key);

/// The extensions that select a language, e.g. `.bas`, `.logo`, `.rexx`.
/// `COMAL` answers with two, which is why this is a list rather than one string.
[[nodiscard]] QStringList extensions(Language language);

/// Infers the language from a file name or path. Case-insensitive on the
/// extension; `Unknown` when nothing matches, so the caller decides the fallback
/// rather than being handed a silent guess.
[[nodiscard]] Language languageForFile(const QString& fileNameOrPath);

/// The full name filter for an "open file" dialog, e.g.
/// `All programs (*.bas *.pilot *.logo *.comal *.rexx *.scm);;BASIC (*.bas);;…`
[[nodiscard]] QString openFileFilter();

}  // namespace timewarp::ui
