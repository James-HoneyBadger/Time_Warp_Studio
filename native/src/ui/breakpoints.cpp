// SPDX-License-Identifier: MIT

#include "timewarp/ui/breakpoints.hpp"

#include <QFileInfo>
#include <QSettings>
#include <QStringList>

#include <algorithm>

namespace timewarp::ui::breakpoints {

namespace {

constexpr auto kRoot = "breakpoints";

}  // namespace

QString keyFor(const QString& path) { return QString::fromLatin1(kRoot) + path; }

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
        // The stored key had the leading separator of an absolute path stripped by
        // the group, so it is put back before asking the filesystem. A relative
        // path cannot be resolved from here and is dropped -- the alternative is
        // breakpoints that reappear on whatever file happens to match.
        const QString path = QLatin1Char('/') + key;
        if (!QFileInfo::exists(path)) {
            settings.remove(key);
            ++dropped;
        }
    }
    settings.endGroup();
    return dropped;
}

}  // namespace timewarp::ui::breakpoints
