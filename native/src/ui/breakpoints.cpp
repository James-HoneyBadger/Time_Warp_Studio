// SPDX-License-Identifier: MIT

#include "timewarp/ui/breakpoints.hpp"

#include <QFileInfo>
#include <QSettings>
#include <QStringList>
#include <QUrl>

#include <algorithm>

namespace timewarp::ui::breakpoints {

namespace {

constexpr auto kRoot = "breakpoints";

/// The path as one settings-key component.
///
/// A key is a path of groups, and a filesystem path is not one: on POSIX it
/// starts with '/', which reads as an empty group, and on Windows it starts with
/// a drive colon, which QSettings will not take in a group name at all -- the key
/// came out as "breakpointsC:/…", and `prune` then rebuilt "/C:/…", which never
/// exists, so every breakpoint on Windows was deleted on the next prune.
/// Percent-encoding makes the key the same shape on every platform, and one key
/// for one file.
[[nodiscard]] QString encode(const QString& path) {
    return QString::fromLatin1(QUrl::toPercentEncoding(path));
}

[[nodiscard]] QString decode(const QString& key) {
    return QUrl::fromPercentEncoding(key.toUtf8());
}

}  // namespace

QString keyFor(const QString& path) {
    if (path.isEmpty()) {
        return {};
    }
    return QString::fromLatin1(kRoot) + QLatin1Char('/') + encode(path);
}

QSet<int> load(QSettings& settings, const QString& path) {
    QSet<int> lines;
    if (path.isEmpty()) {
        return lines;
    }
    const QString stored = settings.value(keyFor(path)).toString();
    const QStringList parts = stored.split(QLatin1Char(','), Qt::SkipEmptyParts);
    for (const QString& part : parts) {
        bool ok = false;
        const int line = part.trimmed().toInt(&ok);
        if (ok && line >= 0) {
            lines.insert(line);
        }
    }
    return lines;
}

void save(QSettings& settings, const QString& path, const QSet<int>& lines) {
    if (path.isEmpty()) {
        // An untitled document has no key to hang them on, and inventing one would
        // attach breakpoints to whatever was saved there next.
        return;
    }
    if (lines.isEmpty()) {
        settings.remove(keyFor(path));
        return;
    }
    QList<int> sorted{lines.begin(), lines.end()};
    std::sort(sorted.begin(), sorted.end());
    QStringList parts;
    parts.reserve(sorted.size());
    for (const int line : sorted) {
        parts.append(QString::number(line));
    }
    settings.setValue(keyFor(path), parts.join(QLatin1Char(',')));
}

int prune(QSettings& settings) {
    int dropped = 0;
    // Inside the group, because a key containing `/` is a subgroup rather than a
    // top-level key: `childKeys()` would report an empty list and prune nothing.
    settings.beginGroup(QLatin1String(kRoot));
    const QStringList keys = settings.allKeys();
    for (const QString& key : keys) {
        // The key is the encoded path (see `keyFor`). A relative path cannot be
        // resolved from here and is dropped -- the alternative is breakpoints that
        // reappear on whatever file happens to match.
        const QString path = decode(key);
        if (path.isEmpty() || !QFileInfo::exists(path)) {
            settings.remove(key);
            ++dropped;
        }
    }
    settings.endGroup();
    return dropped;
}

}  // namespace timewarp::ui::breakpoints
