// SPDX-License-Identifier: MIT
//
// Breakpoints, per document, in `QSettings`.
//
// A comma-separated list of zero-based lines under `breakpoints/<path>`. The
// removed implementation used a JSON blob per document; a list of numbers is
// something a person can read and fix, and the only thing a breakpoint set has
// to survive is being written down.
//
// The store is separate from both the editor and the window so it can be tested
// without either: what is worth asserting is the round trip, the empty-set
// behaviour and the pruning of files that are gone.

#pragma once

#include <QSet>
#include <QString>

class QSettings;

namespace timewarp::ui::breakpoints {

/// The settings key a document's breakpoints are stored under.
///
/// `breakpoints` and the absolute path joined rather than with a separator of
/// their own: QSettings already uses `/` for its own nesting, and a doubled slash
/// from an absolute path is a key nobody can find in the file.
[[nodiscard]] QString keyFor(const QString& path);

/// The lines stored for `path`; empty when nothing is stored or the value cannot
/// be read. A broken value is treated as no breakpoints rather than as a reason
/// to refuse to start.
[[nodiscard]] QSet<int> load(QSettings& settings, const QString& path);

/// Stores `lines`. An empty set removes the key, so a document that has no
/// breakpoints leaves no trace in the settings file.
void save(QSettings& settings, const QString& path, const QSet<int>& lines);

/// Removes every stored entry whose file no longer exists -- a deleted or moved
/// program otherwise accumulates breakpoints for ever. Returns how many entries
/// were dropped.
int prune(QSettings& settings);

}  // namespace timewarp::ui::breakpoints
