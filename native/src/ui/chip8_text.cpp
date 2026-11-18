// SPDX-License-Identifier: AGPL-3.0-only

#include "timewarp/ui/chip8_text.hpp"

#include <QFileInfo>

namespace timewarp::ui {

namespace {

/// The bytes a `QTextDocument` would rewrite: `'\r'` becomes `'\n'` and the
/// no-break space becomes an ordinary space. The private-use range cannot come
/// from a Latin-1 decode, so each mapping stays a bijection over 0x00-0xFF.
constexpr char16_t kCarriageReturnCodePoint = 0xE00D;
constexpr char16_t kNoBreakSpaceCodePoint = 0xE0A0;

}  // namespace

QStringList chip8RomExtensions() {
    return {QStringLiteral("ch8"), QStringLiteral("c8"), QStringLiteral("rom"),
            QStringLiteral("bin")};
}

bool isChip8RomFile(const QString& fileNameOrPath) {
    const QString suffix = QFileInfo{fileNameOrPath}.suffix().toLower();
    return chip8RomExtensions().contains(suffix);
}

QString romBytesToText(const QByteArray& bytes) {
    QString text;
    text.reserve(bytes.size());
    for (const char raw : bytes) {
        const auto byte = static_cast<unsigned char>(raw);
        if (byte == 0x0D) {
            text.append(QChar{kCarriageReturnCodePoint});
        } else if (byte == 0xA0) {
            text.append(QChar{kNoBreakSpaceCodePoint});
        } else {
            text.append(QChar{static_cast<char16_t>(byte)});
        }
    }
    return text;
}

QByteArray romTextToBytes(const QString& text) {
    QByteArray bytes;
    bytes.reserve(text.size());
    for (const QChar character : text) {
        if (character.unicode() == kCarriageReturnCodePoint) {
            bytes.append('\r');
        } else if (character.unicode() == kNoBreakSpaceCodePoint) {
            bytes.append(static_cast<char>(0xA0));
        } else {
            bytes.append(static_cast<char>(character.unicode() & 0xFF));
        }
    }
    return bytes;
}

}  // namespace timewarp::ui
