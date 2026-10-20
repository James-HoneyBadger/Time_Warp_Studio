// SPDX-License-Identifier: MIT
//
// The profile report: which lines ran, how often, and where the time went,
// longest first. Shown after a run made with View > Profile Run ticked.
//
// Ported from the removed application's `utils/profiler.py` and its "profile
// report shown in output". The report is a table here rather than console text,
// because the interesting question is "which line is slow", and a column of
// numbers answers it faster than a paragraph.
//
// The entries arrive already sorted by the engine; the dialog does not sort
// them again, so what it shows is exactly what was measured.

#pragma once

#include <QDialog>
#include <QList>
#include <QString>

#include <cstdint>

class QTableWidget;

namespace timewarp::ui {

/// One line the run executed, as the report shows it.
struct ProfileEntry {
    int line = 0;           ///< The line as the program numbers it.
    QString source;         ///< The source text, for a report that can be read.
    std::uint64_t hits = 0; ///< How many times the line ran.
    double elapsed_ms = 0.0;
};

class ProfileReportDialog final : public QDialog {
    Q_OBJECT

public:
    /// `entries` is longest-first; `total_ms` is the run's wall clock, used for
    /// the share column and the heading.
    ProfileReportDialog(QString document, QList<ProfileEntry> entries, double total_ms,
                        QWidget* parent = nullptr);

    /// What the dialog lists, for the tests: a table is the one part of a dialog
    /// a test can read without a screenshot.
    [[nodiscard]] int rowCount() const;
    [[nodiscard]] int lineAt(int row) const;
    [[nodiscard]] std::uint64_t hitsAt(int row) const;

private:
    QList<ProfileEntry> entries_;
    QTableWidget* table_ = nullptr;
};

}  // namespace timewarp::ui
