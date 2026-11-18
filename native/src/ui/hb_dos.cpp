// SPDX-License-Identifier: MIT

#include "timewarp/ui/hb_dos.hpp"

#include <algorithm>

#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonParseError>

#include "timewarp/ui/app_paths.hpp"

namespace timewarp::ui {

namespace {

constexpr auto kDosName = "HB DOS 1.0";
constexpr auto kDefaultVolume = "HBWARP";

/// The errors a user sees, without the `?` the machine prints in front of them.
constexpr auto kBadName = "BAD FILE NAME";
constexpr auto kNotFound = "FILE NOT FOUND";
constexpr auto kLocked = "FILE LOCKED";
constexpr auto kFull = "DISK FULL";
constexpr auto kExists = "FILE EXISTS";
constexpr auto kNoDisk = "NO DISK";
constexpr auto kDiskError = "DISK ERROR";
constexpr auto kUnreadable = "DISK UNREADABLE";

bool fail(QString* error, const char* message) {
    if (error != nullptr) {
        *error = QString::fromLatin1(message);
    }
    return false;
}

}  // namespace

HbDos::HbDos(QString image_path) : image_path_(std::move(image_path)) {}

QString HbDos::defaultImagePath() {
    const QString directory = dataDirFor(QStringLiteral("disks"));
    if (directory.isEmpty()) {
        return {};
    }
    return directory + QStringLiteral("/HBWARP.dsk");
}

QString HbDos::normaliseName(const QString& raw, QString* error) {
    QString name = raw.trimmed();
    if (name.size() >= 2) {
        const QChar first = name.front();
        const QChar last = name.back();
        if ((first == QLatin1Char('"') && last == QLatin1Char('"')) ||
            (first == QLatin1Char('\'') && last == QLatin1Char('\''))) {
            name = name.mid(1, name.size() - 2).trimmed();
        }
    }
    name = name.toUpper();
    if (name.isEmpty() || name.size() > kMaxNameLength) {
        if (error != nullptr) {
            *error = QString::fromLatin1(kBadName);
        }
        return {};
    }
    for (const QChar character : name) {
        if (!(character.isLetterOrNumber() || character == QLatin1Char(' ') ||
              character == QLatin1Char('-') || character == QLatin1Char('_') ||
              character == QLatin1Char('.'))) {
            if (error != nullptr) {
                *error = QString::fromLatin1(kBadName);
            }
            return {};
        }
    }
    return name;
}

int HbDos::sectorsFor(const QString& source) {
    const int bytes = source.toUtf8().size();
    return std::max(1, (bytes + kSectorSize - 1) / kSectorSize);
}

int HbDos::indexOf(const QString& name) const {
    for (int index = 0; index < files_.size(); ++index) {
        if (files_.at(index).name == name) {
            return index;
        }
    }
    return -1;
}

const DosFile* HbDos::find(const QString& name) const {
    const QString wanted = normaliseName(name);
    if (wanted.isEmpty()) {
        return nullptr;
    }
    const int index = indexOf(wanted);
    return index < 0 ? nullptr : &files_.at(index);
}

int HbDos::usedSectors() const {
    int used = 0;
    for (const DosFile& file : files_) {
        used += sectorsFor(file.source);
    }
    return used;
}

int HbDos::freeSectors() const {
    return std::max(0, kTotalSectors - usedSectors());
}

bool HbDos::open(QString* error) {
    files_.clear();
    volume_name_ = QString::fromLatin1(kDefaultVolume);
    if (image_path_.isEmpty()) {
        return fail(error, kNoDisk);
    }

    QFile image{image_path_};
    if (!image.exists()) {
        // A machine that has never saved anything still has a formatted disk.
        return true;
    }
    if (!image.open(QIODevice::ReadOnly)) {
        return fail(error, kDiskError);
    }

    QJsonParseError parse_error;
    const QJsonDocument document = QJsonDocument::fromJson(image.readAll(), &parse_error);
    if (parse_error.error != QJsonParseError::NoError || !document.isObject()) {
        return fail(error, kUnreadable);
    }

    return fromJson(document.object(), error);
}

QJsonObject HbDos::toJson() const {
    QJsonArray entries;
    for (const DosFile& file : files_) {
        QJsonObject entry;
        entry.insert(QStringLiteral("name"), file.name);
        entry.insert(QStringLiteral("language"), file.language);
        entry.insert(QStringLiteral("source"), file.source);
        entry.insert(QStringLiteral("locked"), file.locked);
        entries.append(entry);
    }
    QJsonObject root;
    root.insert(QStringLiteral("dos"), QString::fromLatin1(kDosName));
    root.insert(QStringLiteral("volume"), volume_name_);
    root.insert(QStringLiteral("files"), entries);
    return root;
}

bool HbDos::fromJson(const QJsonObject& root, QString* error) {
    if (!root.contains(QStringLiteral("files")) && !root.contains(QStringLiteral("volume"))) {
        return fail(error, kUnreadable);
    }
    volume_name_ = QString::fromLatin1(kDefaultVolume);
    files_.clear();
    const QString volume = root.value(QStringLiteral("volume")).toString();
    if (!volume.isEmpty()) {
        volume_name_ = volume;
    }
    for (const QJsonValue value : root.value(QStringLiteral("files")).toArray()) {
        const QJsonObject entry = value.toObject();
        DosFile file;
        file.name = entry.value(QStringLiteral("name")).toString();
        file.language = entry.value(QStringLiteral("language")).toString();
        file.source = entry.value(QStringLiteral("source")).toString();
        file.locked = entry.value(QStringLiteral("locked")).toBool();
        if (!file.name.isEmpty()) {
            files_.append(file);
        }
    }
    return true;
}

bool HbDos::commit(QString* error) {
    if (image_path_.isEmpty()) {
        return fail(error, kNoDisk);
    }

    const QJsonObject root = toJson();

    const QFileInfo info{image_path_};
    if (!QDir{}.mkpath(info.absolutePath())) {
        return fail(error, kDiskError);
    }
    QFile image{image_path_};
    if (!image.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
        return fail(error, kDiskError);
    }
    const QByteArray bytes = QJsonDocument{root}.toJson(QJsonDocument::Indented);
    if (image.write(bytes) != bytes.size()) {
        return fail(error, kDiskError);
    }
    return true;
}

bool HbDos::write(const QString& name, const QString& language, const QString& source,
                  QString* error) {
    const QString wanted = normaliseName(name, error);
    if (wanted.isEmpty()) {
        return false;
    }

    const int existing = indexOf(wanted);
    if (existing >= 0 && files_.at(existing).locked) {
        return fail(error, kLocked);
    }
    const int after = usedSectors() -
                      (existing >= 0 ? sectorsFor(files_.at(existing).source) : 0) +
                      sectorsFor(source);
    if (after > kTotalSectors) {
        return fail(error, kFull);
    }

    const QList<DosFile> backup = files_;
    DosFile file{wanted, language, source, false};
    if (existing >= 0) {
        files_[existing] = file;
    } else {
        files_.append(file);
    }
    if (!commit(error)) {
        files_ = backup;
        return false;
    }
    return true;
}

bool HbDos::erase(const QString& name, QString* error) {
    const QString wanted = normaliseName(name, error);
    if (wanted.isEmpty()) {
        return false;
    }
    const int index = indexOf(wanted);
    if (index < 0) {
        return fail(error, kNotFound);
    }
    if (files_.at(index).locked) {
        return fail(error, kLocked);
    }

    const QList<DosFile> backup = files_;
    files_.removeAt(index);
    if (!commit(error)) {
        files_ = backup;
        return false;
    }
    return true;
}

bool HbDos::rename(const QString& from, const QString& to, QString* error) {
    const QString source = normaliseName(from, error);
    if (source.isEmpty()) {
        return false;
    }
    const QString target = normaliseName(to, error);
    if (target.isEmpty()) {
        return false;
    }

    const int index = indexOf(source);
    if (index < 0) {
        return fail(error, kNotFound);
    }
    if (indexOf(target) >= 0 && target != source) {
        return fail(error, kExists);
    }

    const QList<DosFile> backup = files_;
    files_[index].name = target;
    if (!commit(error)) {
        files_ = backup;
        return false;
    }
    return true;
}

bool HbDos::setLocked(const QString& name, bool locked, QString* error) {
    const QString wanted = normaliseName(name, error);
    if (wanted.isEmpty()) {
        return false;
    }
    const int index = indexOf(wanted);
    if (index < 0) {
        return fail(error, kNotFound);
    }

    const QList<DosFile> backup = files_;
    files_[index].locked = locked;
    if (!commit(error)) {
        files_ = backup;
        return false;
    }
    return true;
}

bool HbDos::read(const QString& name, DosFile* file, QString* error) const {
    const QString wanted = normaliseName(name, error);
    if (wanted.isEmpty()) {
        return false;
    }
    const int index = indexOf(wanted);
    if (index < 0) {
        return fail(error, kNotFound);
    }
    if (file != nullptr) {
        *file = files_.at(index);
    }
    return true;
}

bool HbDos::format(QString* error) {
    const QList<DosFile> backup = files_;
    const QString volume = volume_name_;
    files_.clear();
    volume_name_ = QString::fromLatin1(kDefaultVolume);
    if (!commit(error)) {
        files_ = backup;
        volume_name_ = volume;
        return false;
    }
    return true;
}

}  // namespace timewarp::ui
