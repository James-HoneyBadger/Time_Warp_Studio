// SPDX-License-Identifier: MIT
//
// A line-by-line comparison of two documents.
//
// Version History could show a saved copy and could open it, but it could not say
// what had *changed*: the choice between "restore this" and "keep what I have" was
// made by reading two documents and remembering. This is the missing half, and it is
// a pure function -- two strings in, a list of lines out -- so the interesting part
// is testable without a window and reusable by anything that compares text (a lesson
// diff, a project check, the assistant).
//
// #### The algorithm, and its bound
//
// Longest common subsequence over lines, which is the diff everybody recognises: a
// line that moved is a removal and an addition, and an edited line is one of each in
// the same place. The common prefix and suffix are stripped first, because that is
// what makes a one-line edit in a two-thousand-line file cheap.
//
// The table is bounded. Beyond `kMaxCells` the middle is reported as removed and
// added wholesale rather than spent on memory: a diff of two unrelated ten-thousand
// line files is not a diff a person reads, and answering it slowly would be worse
// than answering it plainly.

#pragma once

#include <QList>
#include <QString>

namespace timewarp::ui {

enum class DiffKind {
    /// A line both documents have, in place.
    Context,
    Added,
    Removed,
};

struct DiffLine {
    DiffKind kind = DiffKind::Context;
    QString text;
};

/// How many lines were added and removed.
struct DiffCounts {
    int added = 0;
    int removed = 0;
    [[nodiscard]] bool empty() const noexcept { return added == 0 && removed == 0; }
};

/// Splits a document into lines without inventing a trailing empty one.
[[nodiscard]] QStringList diffLines(const QString& text);

/// The comparison of `before` with `after`, in document order.
[[nodiscard]] QList<DiffLine> compareDocuments(const QString& before, const QString& after);

[[nodiscard]] DiffCounts countChanges(const QList<DiffLine>& lines);

/// The changes as unified text -- `-` and `+` prefixes, `@@` hunk headers, and
/// `context` unchanged lines around each change. Empty when the documents are the
/// same.
[[nodiscard]] QString unifiedDiff(const QString& before, const QString& after, int context = 3);

/// "+2 −1", or "no changes". The one-line summary a status bar shows.
[[nodiscard]] QString diffSummary(const QList<DiffLine>& lines);

}  // namespace timewarp::ui
