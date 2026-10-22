// SPDX-License-Identifier: MIT
//
// Finding the checkout and the binaries the gates drive, plus the command table
// they share.

#include "gates.hpp"

#include <QCoreApplication>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QProcessEnvironment>
#include <QStandardPaths>

namespace timewarp::gates {

namespace {

/// The two build trees the Makefile produces, in the order they are tried.
constexpr const char* kBuildTrees[] = {"build", "build-debug"};

QString withExecutableSuffix(const QString& path) {
#ifdef Q_OS_WIN
    return path + QStringLiteral(".exe");
#else
    return path;
#endif
}

}  // namespace

QString repositoryRoot() {
    // The gates live in `native/tools/gates`, so the checkout is three levels up
    // from the executable when it sits in a build tree under the repository. The
    // search walks up looking for the marker rather than counting directories: a
    // build outside the tree (`/tmp/tw-build`) still finds the repository it was
    // configured from through the paths compiled in below.
    const QString configured = QStringLiteral(TIME_WARP_SOURCE_DIR);
    if (!configured.isEmpty() && QFileInfo::exists(configured + QStringLiteral("/VERSION"))) {
        return QDir::cleanPath(configured);
    }
    QDir directory{QCoreApplication::applicationDirPath()};
    for (int depth = 0; depth < 6; ++depth) {
        if (QFileInfo::exists(directory.filePath(QStringLiteral("VERSION"))) &&
            QFileInfo::exists(directory.filePath(QStringLiteral("native/CMakeLists.txt")))) {
            return QDir::cleanPath(directory.absolutePath());
        }
        if (!directory.cdUp()) {
            break;
        }
    }
    return QDir::cleanPath(QDir::currentPath());
}

std::optional<QString> locateBinary(const QString& name) {
    const QString variable = QStringLiteral("TIME_WARP_") + name.toUpper();
    const QString override = QProcessEnvironment::systemEnvironment().value(variable);
    if (!override.isEmpty()) {
        const QFileInfo candidate{override};
        return candidate.isFile() ? std::optional<QString>{candidate.absoluteFilePath()}
                                  : std::nullopt;
    }
    const QString root = repositoryRoot();
    for (const char* tree : kBuildTrees) {
        const QString path = withExecutableSuffix(
            QDir{root}.filePath(QStringLiteral("native/%1/%2").arg(QLatin1String{tree}, name)));
        const QFileInfo candidate{path};
        if (candidate.isFile()) {
            return QDir::cleanPath(candidate.absoluteFilePath());
        }
    }
    const QString on_path = QStandardPaths::findExecutable(name);
    if (!on_path.isEmpty()) {
        return on_path;
    }
    return std::nullopt;
}

std::optional<CommandTable> loadCommandTable(QString* error) {
    const QString path = repositoryRoot() + QStringLiteral("/native/share/command_rows.json");
    QFile file{path};
    if (!file.open(QIODevice::ReadOnly)) {
        if (error != nullptr) {
            *error = QStringLiteral("cannot read %1: %2").arg(path, file.errorString());
        }
        return std::nullopt;
    }
    QJsonParseError parse_error{};
    const QJsonDocument document = QJsonDocument::fromJson(file.readAll(), &parse_error);
    if (parse_error.error != QJsonParseError::NoError || !document.isObject()) {
        if (error != nullptr) {
            *error = QStringLiteral("%1 is not a JSON object: %2")
                         .arg(path, parse_error.errorString());
        }
        return std::nullopt;
    }
    const QJsonObject object = document.object();
    CommandTable table;
    table.scratch = object.value(QStringLiteral("scratch")).toString();
    const QJsonArray rows = object.value(QStringLiteral("rows")).toArray();
    table.rows.reserve(static_cast<std::size_t>(rows.size()));
    for (const QJsonValue& value : rows) {
        const QJsonObject record = value.toObject();
        Row row;
        row.language = record.value(QStringLiteral("language")).toString();
        row.command = record.value(QStringLiteral("command")).toString();
        row.source = record.value(QStringLiteral("source")).toString();
        row.expect = record.value(QStringLiteral("expect")).toString();
        for (const QJsonValue& extra : record.value(QStringLiteral("also")).toArray()) {
            row.also.append(extra.toString());
        }
        if (row.language.isEmpty() || row.source.isEmpty() || row.expect.isEmpty()) {
            if (error != nullptr) {
                *error = QStringLiteral("a row in %1 is missing a language, a source or an "
                                        "expectation")
                             .arg(path);
            }
            return std::nullopt;
        }
        table.rows.push_back(std::move(row));
    }
    for (const QJsonValue& value : object.value(QStringLiteral("waivers")).toArray()) {
        const QJsonObject record = value.toObject();
        table.waivers.push_back(Waiver{record.value(QStringLiteral("language")).toString(),
                                       record.value(QStringLiteral("command")).toString(),
                                       record.value(QStringLiteral("reason")).toString()});
    }
    return table;
}

QString languageKey(const QString& name) {
    QString key;
    key.reserve(name.size());
    for (const QChar character : name) {
        if (character.isLetterOrNumber()) {
            key.append(character.toLower());
        }
    }
    return key;
}

}  // namespace timewarp::gates
