// SPDX-License-Identifier: MIT

#include "timewarp/ui/machine_image.hpp"

#include "timewarp/ui/app_paths.hpp"

#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonParseError>

namespace timewarp::ui {

namespace {

constexpr auto kFormatKey = "format";
constexpr auto kWrittenByKey = "writtenBy";

[[nodiscard]] QJsonObject programsToJson(const QMap<QString, QString>& programs) {
    QJsonObject object;
    for (auto it = programs.cbegin(); it != programs.cend(); ++it) {
        object.insert(it.key(), it.value());
    }
    return object;
}

[[nodiscard]] QMap<QString, QString> programsFromJson(const QJsonObject& object) {
    QMap<QString, QString> programs;
    for (auto it = object.begin(); it != object.end(); ++it) {
        if (it.value().isString() && !it.value().toString().isEmpty()) {
            programs.insert(it.key(), it.value().toString());
        }
    }
    return programs;
}

}  // namespace

QString MachineImage::defaultDirectory() { return dataDirFor(QStringLiteral("machines")); }

QString MachineImage::resolvePath(const QString& name) {
    const QString trimmed = name.trimmed();
    if (trimmed.isEmpty()) {
        return QString{};
    }
    if (trimmed.contains(QLatin1Char('/')) || trimmed.contains(QLatin1Char('\\')) ||
        trimmed.endsWith(QStringLiteral(".hb1"), Qt::CaseInsensitive)) {
        return trimmed;
    }
    return QDir{defaultDirectory()}.filePath(trimmed.toUpper() + QStringLiteral(".hb1"));
}

QJsonObject MachineImage::toJson() const {
    QJsonObject root;
    root.insert(QString::fromLatin1(kFormatKey), kFormat);
    root.insert(QString::fromLatin1(kWrittenByKey),
                QStringLiteral("Time Warp Studio HB-1 machine image"));
    root.insert(QStringLiteral("name"), name);
    root.insert(QStringLiteral("language"), language);
    root.insert(QStringLiteral("mode"), mode);
    root.insert(QStringLiteral("programs"), programsToJson(programs));
    root.insert(QStringLiteral("config"), config.toJson());
    root.insert(QStringLiteral("volume"), volume);
    return root;
}

bool MachineImage::fromJson(const QJsonObject& root, MachineImage* image, QString* error) {
    const auto fail = [error](const QString& reason) {
        if (error != nullptr) {
            *error = reason;
        }
        return false;
    };
    if (image == nullptr) {
        return fail(QStringLiteral("no image to fill"));
    }
    // The format is read first and checked first. A newer file is refused before a
    // single other field is looked at, because the alternative is a machine that
    // silently loses the fields this build does not know about -- and then saves
    // itself back over the file.
    const QJsonValue format = root.value(QString::fromLatin1(kFormatKey));
    if (!format.isDouble()) {
        return fail(QStringLiteral("this is not a machine image (no format field)"));
    }
    const int written = format.toInt();
    if (written > kFormat) {
        return fail(QStringLiteral("this machine image is format %1; this build reads up "
                                   "to format %2")
                        .arg(written)
                        .arg(kFormat));
    }
    if (written < 1) {
        return fail(QStringLiteral("this machine image has format %1, which is not a "
                                   "format")
                        .arg(written));
    }

    MachineImage result;
    const QString name = root.value(QStringLiteral("name")).toString();
    if (!name.isEmpty()) {
        result.name = name;
    }
    const QString language = root.value(QStringLiteral("language")).toString();
    if (!language.isEmpty()) {
        result.language = language;
    }
    result.mode = root.value(QStringLiteral("mode")).toInt(result.mode);
    result.programs = programsFromJson(root.value(QStringLiteral("programs")).toObject());
    result.config = RetroComputerSettings::fromJson(root.value(QStringLiteral("config")).toObject());
    result.volume = root.value(QStringLiteral("volume")).toObject();
    *image = result;
    return true;
}

bool MachineImage::save(const QString& path, QString* error) const {
    const auto fail = [error](const QString& reason) {
        if (error != nullptr) {
            *error = reason;
        }
        return false;
    };
    const QFileInfo info{path};
    if (info.absoluteDir().path().isEmpty() || !QDir{}.mkpath(info.absolutePath())) {
        return fail(QStringLiteral("cannot create %1").arg(info.absolutePath()));
    }
    QFile file{path};
    if (!file.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
        return fail(QStringLiteral("cannot write %1").arg(path));
    }
    const QByteArray bytes = QJsonDocument{toJson()}.toJson(QJsonDocument::Indented);
    if (file.write(bytes) != bytes.size()) {
        return fail(QStringLiteral("could not finish writing %1").arg(path));
    }
    return true;
}

bool MachineImage::load(const QString& path, MachineImage* image, QString* error) {
    const auto fail = [error](const QString& reason) {
        if (error != nullptr) {
            *error = reason;
        }
        return false;
    };
    QFile file{path};
    if (!file.exists()) {
        return fail(QStringLiteral("no machine image at %1").arg(path));
    }
    if (!file.open(QIODevice::ReadOnly)) {
        return fail(QStringLiteral("cannot read %1").arg(path));
    }
    QJsonParseError parse_error;
    const QJsonDocument document = QJsonDocument::fromJson(file.readAll(), &parse_error);
    if (parse_error.error != QJsonParseError::NoError || !document.isObject()) {
        return fail(QStringLiteral("%1 is not a machine image").arg(path));
    }
    return fromJson(document.object(), image, error);
}

}  // namespace timewarp::ui
