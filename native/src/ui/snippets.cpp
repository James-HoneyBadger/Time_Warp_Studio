// SPDX-License-Identifier: MIT

#include "timewarp/ui/snippets.hpp"

#include "timewarp/core/share_files.hpp"

#include <QCoreApplication>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>

namespace timewarp::ui {
namespace {

constexpr auto kCursor = "$CURSOR$";
constexpr auto kSelection = "$SELECTION$";

}  // namespace

QString Snippet::id() const {
    // Derived rather than stored: one source of truth, and a rename in the JSON
    // cannot leave an id behind pointing at nothing.
    QString slug = name.toLower();
    slug.replace(QLatin1Char(' '), QLatin1Char('-'));
    return settingsKey(language) + QLatin1Char('/') + slug;
}

QString snippetRoot() {
    return timewarp::core::shareFile("TIME_WARP_SNIPPETS", QStringLiteral("snippets.json"));
}

QList<Snippet> loadSnippets(QString* error) {
    QList<Snippet> snippets;
    const QString path = snippetRoot();
    if (path.isEmpty()) {
        if (error != nullptr) {
            *error = QStringLiteral("No snippet catalogue was found.");
        }
        return snippets;
    }
    QFile file{path};
    if (!file.open(QIODevice::ReadOnly)) {
        if (error != nullptr) {
            *error = QStringLiteral("Cannot read %1.").arg(path);
        }
        return snippets;
    }
    QJsonParseError parse{};
    const QJsonDocument document = QJsonDocument::fromJson(file.readAll(), &parse);
    file.close();
    if (document.isNull()) {
        if (error != nullptr) {
            *error = QStringLiteral("%1 is not valid JSON: %2").arg(path, parse.errorString());
        }
        return snippets;
    }

    const QJsonObject languages = document.object().value(QStringLiteral("languages")).toObject();
    for (auto entry = languages.begin(); entry != languages.end(); ++entry) {
        // The key is the settings key ("chip8"), so the display name and the
        // Language value come from the one table that holds them.
        const Language language = languageFromKey(entry.key());
        if (language == Language::Unknown) {
            continue;  // a catalogue from a newer build: not this build's problem
        }
        for (const QJsonValue& value : entry.value().toArray()) {
            const QJsonObject record = value.toObject();
            Snippet snippet;
            snippet.name = record.value(QStringLiteral("name")).toString();
            snippet.description = record.value(QStringLiteral("description")).toString();
            snippet.language = language;
            snippet.body = record.value(QStringLiteral("body")).toString();
            if (snippet.name.isEmpty() || snippet.body.isEmpty()) {
                continue;
            }
            if (!snippet.body.endsWith(QLatin1Char('\n'))) {
                // A body that does not end in a newline glues the next line the
                // user types onto the last line of the snippet.
                snippet.body.append(QLatin1Char('\n'));
            }
            snippets.append(snippet);
        }
    }
    if (error != nullptr) {
        error->clear();
    }
    return snippets;
}

SnippetInsertion resolveSnippet(const QString& body, const QString& selection) {
    SnippetInsertion result;
    QString text = body;
    text.replace(QLatin1String(kSelection), selection);

    const int at = text.indexOf(QLatin1String(kCursor));
    if (at < 0) {
        result.text = text;
        result.caret = text.size();
        return result;
    }
    text.remove(at, int(QLatin1String(kCursor).size()));
    result.text = text;
    result.caret = at;
    return result;
}

}  // namespace timewarp::ui
