// SPDX-License-Identifier: MIT

#include "timewarp/ui/language.hpp"

#include <array>
#include <string_view>

#include <QFileInfo>

namespace timewarp::ui {

namespace {

/// One row of the language table.
///
/// The extensions are exactly those in the Python implementation's
/// `core/language_registry.py`, alternates included: a file the Python IDE calls
/// BASIC must be one this build calls BASIC, or the two disagree about what they
/// are running.
struct LanguageInfo {
    Language language;
    std::string_view display;   ///< "BASIC", "Logo", "REXX", "Scheme"
    std::string_view key;       ///< "basic", "logo", "rexx" — settings and protocol
    std::string_view extension1;
    std::string_view extension2;  ///< empty when the language has only one
};

constexpr std::array<LanguageInfo, 10> kLanguages{{
    {Language::Basic, "BASIC", "basic", "bas", "bsc"},
    {Language::Pilot, "PILOT", "pilot", "pilot", "plt"},
    {Language::Logo, "Logo", "logo", "logo", "lg"},
    {Language::Comal, "COMAL", "comal", "comal", "cml"},
    {Language::Rexx, "REXX", "rexx", "rexx", "rex"},
    {Language::Scheme, "Scheme", "scheme", "scm", "ss"},
    {Language::Focal, "FOCAL", "focal", "foc", "focal"},
    {Language::Chip8, "CHIP-8", "chip8", "ch8", "c8"},
    {Language::Apl, "APL", "apl", "apl", "dyalog"},
    {Language::Smalltalk, "Smalltalk", "smalltalk", "st", "smalltalk"},
}};

/// Scheme has three extensions, which the two-column table above cannot hold, so
/// it is handled explicitly rather than by widening every row.
constexpr std::array<std::string_view, 1> kSchemeExtraExtensions{"scheme"};

/// The bytes a `QTextDocument` would rewrite: `'\r'` becomes `'\n'` and the
/// no-break space becomes an ordinary space. The private-use range cannot come
/// from a Latin-1 decode, so each mapping stays a bijection over 0x00-0xFF.
constexpr char16_t kCarriageReturnCodePoint = 0xE00D;
constexpr char16_t kNoBreakSpaceCodePoint = 0xE0A0;

[[nodiscard]] const LanguageInfo* find(Language language) {
    for (const LanguageInfo& info : kLanguages) {
        if (info.language == language) {
            return &info;
        }
    }
    return nullptr;
}

}  // namespace

Language defaultLanguage() { return Language::Basic; }

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

QString displayName(Language language) {
    const LanguageInfo* info = find(language);
    return info != nullptr ? QString::fromUtf8(info->display) : QStringLiteral("—");
}

QString settingsKey(Language language) {
    const LanguageInfo* info = find(language);
    return info != nullptr ? QString::fromUtf8(info->key) : QStringLiteral("unknown");
}

QStringList extensions(Language language) {
    const LanguageInfo* info = find(language);
    if (info == nullptr) {
        return {};
    }
    QStringList result{QString::fromUtf8(info->extension1)};
    if (!info->extension2.empty()) {
        result << QString::fromUtf8(info->extension2);
    }
    if (language == Language::Scheme) {
        for (const std::string_view extra : kSchemeExtraExtensions) {
            result << QString::fromUtf8(extra);
        }
    }
    return result;
}

QList<Language> allLanguages() {
    QList<Language> languages;
    for (const LanguageInfo& info : kLanguages) {
        languages << info.language;
    }
    return languages;
}

Language languageFromKey(const QString& key) {
    for (const LanguageInfo& info : kLanguages) {
        if (key == QString::fromUtf8(info.key)) {
            return info.language;
        }
    }
    return Language::Unknown;
}

Language languageForFile(const QString& fileNameOrPath) {
    if (fileNameOrPath.isEmpty()) {
        return Language::Unknown;
    }
    // `suffix()` on a name with no dot returns the whole name, so an extensionless
    // file is checked against the table and simply will not match.
    const QString suffix = QFileInfo{fileNameOrPath}.suffix().toLower();
    if (suffix.isEmpty()) {
        return Language::Unknown;
    }
    for (const LanguageInfo& info : kLanguages) {
        if (suffix == QString::fromUtf8(info.extension1)) {
            return info.language;
        }
        if (!info.extension2.empty() && suffix == QString::fromUtf8(info.extension2)) {
            return info.language;
        }
    }
    for (const std::string_view extra : kSchemeExtraExtensions) {
        if (suffix == QString::fromUtf8(extra)) {
            return Language::Scheme;
        }
    }
    return Language::Unknown;
}

QString openFileFilter() {
    QStringList allPatterns;
    for (const LanguageInfo& info : kLanguages) {
        for (const QString& extension : extensions(info.language)) {
            allPatterns << QStringLiteral("*.") + extension;
        }
    }

    QStringList filters;
    filters << QStringLiteral("All programs (%1)").arg(allPatterns.join(QLatin1Char(' ')));
    for (const LanguageInfo& info : kLanguages) {
        QStringList patterns;
        for (const QString& extension : extensions(info.language)) {
            patterns << QStringLiteral("*.") + extension;
        }
        filters << QStringLiteral("%1 (%2)")
                       .arg(displayName(info.language), patterns.join(QLatin1Char(' ')));
    }
    filters << QStringLiteral("All files (*)");
    return filters.join(QStringLiteral(";;"));
}

}  // namespace timewarp::ui
