// SPDX-License-Identifier: MIT

#include "timewarp/ui/language.hpp"

#include "timewarp/core/qt_names.hpp"
#include "timewarp/lang/registry.hpp"

#include <array>
#include <string_view>

#include <QFileInfo>

namespace timewarp::ui {

namespace {

/// The registry's row for a UI language.
///
/// The one fact -- display name, key, extensions -- lives in
/// `timewarp/lang/registry.hpp`. This enum adds `Unknown`, and it maps by position:
/// the two enumerations list the same ten languages in the same order, which is
/// what the `static_assert` below holds.
constexpr timewarp::lang::Language toRegistry(Language language) {
    return static_cast<timewarp::lang::Language>(static_cast<int>(language));
}

static_assert(static_cast<int>(Language::Forth) ==
                  static_cast<int>(timewarp::lang::Language::Forth),
              "the UI enum and the registry list the same languages in the same order");

[[nodiscard]] const timewarp::lang::LanguageRow* row(Language language) {
    if (language == Language::Unknown) {
        return nullptr;
    }
    return timewarp::lang::language_row(toRegistry(language));
}

}  // namespace

Language defaultLanguage() { return Language::Basic; }

QString displayName(Language language) {
    const timewarp::lang::LanguageRow* info = row(language);
    return info != nullptr ? timewarp::core::fromUtf8(info->display) : QStringLiteral("—");
}

QString settingsKey(Language language) {
    const timewarp::lang::LanguageRow* info = row(language);
    return info != nullptr ? timewarp::core::fromUtf8(info->key) : QStringLiteral("unknown");
}

QStringList extensions(Language language) {
    QStringList result;
    for (const std::string_view extension : timewarp::lang::extensions(toRegistry(language))) {
        result << timewarp::core::fromUtf8(extension);
    }
    return result;
}

QList<Language> allLanguages() {
    QList<Language> languages;
    for (const timewarp::lang::LanguageRow& info : timewarp::lang::kLanguageTable) {
        languages << static_cast<Language>(static_cast<int>(info.language));
    }
    return languages;
}

Language languageFromKey(const QString& key) {
    // Case-sensitive on purpose: a settings key is written lowercase by this build,
    // and `ui_document_tests` pins `"Logo"` as unknown so a key from somewhere else
    // fails loudly instead of being silently accepted. The *protocol* lookup in
    // `lang::language_from_key` is the lenient one, because a payload may say "BASIC".
    const timewarp::lang::LanguageRow* info =
        timewarp::lang::language_row_for_key(key.toStdString());
    return info != nullptr ? static_cast<Language>(static_cast<int>(info->language))
                           : Language::Unknown;
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
    const timewarp::lang::LanguageRow* info =
        timewarp::lang::language_row_for_extension(suffix.toStdString());
    return info != nullptr ? static_cast<Language>(static_cast<int>(info->language))
                           : Language::Unknown;
}

QString openFileFilter() {
    QStringList allPatterns;
    for (const timewarp::lang::LanguageRow& info : timewarp::lang::kLanguageTable) {
        for (const QString& extension :
             extensions(static_cast<Language>(static_cast<int>(info.language)))) {
            allPatterns << QStringLiteral("*.") + extension;
        }
    }

    QStringList filters;
    filters << QStringLiteral("All programs (%1)").arg(allPatterns.join(QLatin1Char(' ')));
    for (const timewarp::lang::LanguageRow& info : timewarp::lang::kLanguageTable) {
        const auto language = static_cast<Language>(static_cast<int>(info.language));
        QStringList patterns;
        for (const QString& extension : extensions(language)) {
            patterns << QStringLiteral("*.") + extension;
        }
        filters << QStringLiteral("%1 (%2)")
                       .arg(displayName(language), patterns.join(QLatin1Char(' ')));
    }
    filters << QStringLiteral("All files (*)");
    return filters.join(QStringLiteral(";;"));
}

}  // namespace timewarp::ui
