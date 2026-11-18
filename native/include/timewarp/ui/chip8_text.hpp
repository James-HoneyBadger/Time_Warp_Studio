// SPDX-License-Identifier: AGPL-3.0-only
//
// A CHIP-8 program as text, so a document can hold one.
//
// A listing is ordinary source. A binary ROM is not: it is a series of bytes, of
// which some do not survive a trip through a `QTextDocument`, which normalises
// carriage returns and no-break spaces. Those two are stored as private-use code
// points, which a Latin-1 decode can never produce, so the codec is a bijection
// over 0x00-0xFF and a ROM saved after being opened is the same program.

#pragma once

#include <QByteArray>
#include <QString>
#include <QStringList>

namespace timewarp::ui {

/// The extensions a CHIP-8 program may use: a listing or a ROM, and the names the
/// emulator's ROM library already browses for.
[[nodiscard]] QStringList chip8RomExtensions();

/// Whether *fileNameOrPath* names a CHIP-8 program. Case-insensitive on the
/// extension, like `languageForFile`.
[[nodiscard]] bool isChip8RomFile(const QString& fileNameOrPath);

/// Decodes a ROM's bytes into text, and `romTextToBytes` reverses it.
[[nodiscard]] QString romBytesToText(const QByteArray& bytes);
[[nodiscard]] QByteArray romTextToBytes(const QString& text);

}  // namespace timewarp::ui
