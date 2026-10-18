// SPDX-License-Identifier: MIT

#include "timewarp/ui/autosave.hpp"

#include <algorithm>

#include <QCryptographicHash>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>

#include "timewarp/ui/app_paths.hpp"

namespace timewarp::ui {
namespace {

constexpr auto kManifestName = "manifest.json";
constexpr auto kManifestVersion = 1;

/// A name for a snapshot file that cannot collide and cannot escape the
/// directory: a digest of the document, and the time.
///
/// SHA-1 rather than `qHash`: Qt's string hash is seeded per process, so the
/// same document would get a different name in every session, and a manifest
/// that is lost could not be matched to its files by looking at them.
[[nodiscard]] QString snapshotFileName(const QString& document, qint64 now) {
    const QByteArray digest = QCryptographicHash::hash(document.toUtf8(),
                                                       QCryptographicHash::Sha1)
                                  .toHex();
    return QStringLiteral("%1-%2.bak").arg(QString::fromLatin1(digest.left(16))).arg(now);
}

}  // namespace

AutosaveStore::AutosaveStore(QString directory)
    : directory_(directory.isEmpty()
                     // Resolved, not created: `dataDirFor` makes the directory,
                     // and a user who never turns autosave on should not find
                     // one. `save` creates it.
                     ? QDir{dataDir()}.filePath(QStringLiteral("autosave"))
                     : std::move(directory)) {}

QString AutosaveStore::manifestPath() const {
    return directory_.isEmpty() ? QString{}
                                : QDir{directory_}.filePath(QLatin1String(kManifestName));
}

QList<Snapshot> AutosaveStore::loadManifest() const {
    QList<Snapshot> snapshots;
    const QString path = manifestPath();
    if (path.isEmpty()) {
        return snapshots;
    }
    QFile file{path};
    if (!file.open(QIODevice::ReadOnly)) {
        return snapshots;  // no manifest is an empty store, not an error
    }
    const QJsonDocument document = QJsonDocument::fromJson(file.readAll());
    file.close();
    for (const QJsonValue& value : document.object().value(QStringLiteral("snapshots")).toArray()) {
        const QJsonObject record = value.toObject();
        Snapshot snapshot;
        snapshot.document = record.value(QStringLiteral("document")).toString();
        snapshot.file = record.value(QStringLiteral("file")).toString();
        snapshot.timestamp = qint64(record.value(QStringLiteral("timestamp")).toDouble());
        snapshot.bytes = qint64(record.value(QStringLiteral("bytes")).toDouble());
        // A record whose text has gone is dropped rather than offered: the
        // history exists to give work back, and it cannot give back a file that
        // is not there.
        if (!snapshot.document.isEmpty() && !snapshot.file.isEmpty() &&
            QFileInfo{QDir{directory_}.filePath(snapshot.file)}.isFile()) {
            snapshots.append(snapshot);
        }
    }
    return snapshots;
}

bool AutosaveStore::writeManifest(const QList<Snapshot>& snapshots) const {
    const QString path = manifestPath();
    if (path.isEmpty() || !QDir{}.mkpath(directory_)) {
        return false;
    }
    QJsonArray records;
    for (const Snapshot& snapshot : snapshots) {
        QJsonObject record;
        record.insert(QStringLiteral("document"), snapshot.document);
        record.insert(QStringLiteral("file"), snapshot.file);
        record.insert(QStringLiteral("timestamp"), double(snapshot.timestamp));
        record.insert(QStringLiteral("bytes"), double(snapshot.bytes));
        records.append(record);
    }
    QJsonObject root;
    root.insert(QStringLiteral("version"), kManifestVersion);
    root.insert(QStringLiteral("snapshots"), records);

    // Written through a temporary file and renamed: a crash halfway through
    // writing the manifest would otherwise lose the whole history, which is the
    // one moment the history matters.
    const QString temporary = path + QStringLiteral(".tmp");
    QFile file{temporary};
    if (!file.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
        return false;
    }
    const QByteArray bytes = QJsonDocument{root}.toJson(QJsonDocument::Indented);
    const bool written = file.write(bytes) == bytes.size();
    file.close();
    if (!written) {
        QFile::remove(temporary);
        return false;
    }
    QFile::remove(path);
    return QFile::rename(temporary, path);
}

Snapshot AutosaveStore::save(const QString& document, const QString& text, qint64 now) {
    Snapshot snapshot;
    if (document.isEmpty() || directory_.isEmpty() || !QDir{}.mkpath(directory_)) {
        return snapshot;
    }
    snapshot.document = document;
    snapshot.file = snapshotFileName(document, now);
    snapshot.timestamp = now;
    snapshot.bytes = text.toUtf8().size();

    const QString path = QDir{directory_}.filePath(snapshot.file);
    QFile file{path};
    if (!file.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
        return {};
    }
    const QByteArray bytes = text.toUtf8();
    if (file.write(bytes) != bytes.size()) {
        file.close();
        QFile::remove(path);
        return {};
    }
    file.close();

    QList<Snapshot> snapshots = loadManifest();
    // An identical timestamp is one snapshot, not two: the window's timer and a
    // test can both ask inside the same millisecond.
    snapshots.erase(std::remove_if(snapshots.begin(), snapshots.end(),
                                   [&snapshot](const Snapshot& existing) {
                                       return existing.document == snapshot.document &&
                                              existing.timestamp == snapshot.timestamp;
                                   }),
                    snapshots.end());
    snapshots.prepend(snapshot);

    // Prune this document's older snapshots, and delete their files: a store
    // that only forgets the manifest leaves the bytes behind forever.
    QList<Snapshot> kept;
    int seen_for_document = 0;
    for (const Snapshot& existing : snapshots) {
        if (existing.document == snapshot.document) {
            ++seen_for_document;
            if (seen_for_document > kKeepPerDocument) {
                QFile::remove(QDir{directory_}.filePath(existing.file));
                continue;
            }
        }
        kept.append(existing);
    }
    if (!writeManifest(kept)) {
        // The text is on disk but unlisted, which is a snapshot nobody can find.
        // Say so by returning nothing rather than a snapshot the caller would
        // record as safe.
        QFile::remove(path);
        return {};
    }
    return snapshot;
}

QList<Snapshot> AutosaveStore::versions(const QString& document) const {
    QList<Snapshot> mine;
    for (const Snapshot& snapshot : loadManifest()) {
        if (snapshot.document == document) {
            mine.append(snapshot);
        }
    }
    std::sort(mine.begin(), mine.end(), [](const Snapshot& left, const Snapshot& right) {
        return left.timestamp > right.timestamp;
    });
    return mine;
}

QList<Snapshot> AutosaveStore::all() const {
    QList<Snapshot> snapshots = loadManifest();
    std::sort(snapshots.begin(), snapshots.end(),
              [](const Snapshot& left, const Snapshot& right) {
                  return left.timestamp > right.timestamp;
              });
    return snapshots;
}

QList<Snapshot> AutosaveStore::newerThan(const QString& path, qint64 mtime_ms) const {
    QList<Snapshot> newer;
    for (const Snapshot& snapshot : versions(path)) {
        if (snapshot.timestamp > mtime_ms) {
            newer.append(snapshot);
        }
    }
    return newer;
}

QString AutosaveStore::read(const Snapshot& snapshot) const {
    QFile file{QDir{directory_}.filePath(snapshot.file)};
    if (!file.open(QIODevice::ReadOnly)) {
        return {};
    }
    return QString::fromUtf8(file.readAll());
}

}  // namespace timewarp::ui
