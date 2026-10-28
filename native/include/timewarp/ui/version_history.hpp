// SPDX-License-Identifier: MIT
//
// The version history: what autosave has kept for this document, newest first,
// with the text beside it.
//
// Ported from `ui/version_history_dialog.py` (203 lines). One deliberate
// difference from the original: restoring always opens a **new tab** and never
// writes the file. Choosing the wrong version should cost a tab, not the work
// that was there.
//
// The second difference is the comparison. A list of timestamps and a preview of
// one copy answers "what did it look like", never "what did I change since" -- and
// that is the question somebody deciding whether to restore is actually asking. The
// **Compare with what is open** tick shows the same choice as a unified diff against
// the text in the editor, coloured line by line.

#pragma once

#include <QDialog>
#include <QList>

#include "timewarp/ui/autosave.hpp"

class QCheckBox;
class QLabel;
class QListWidget;
class QPlainTextEdit;
class QPushButton;
class QStackedWidget;

namespace timewarp::ui {

class AutosaveStore;

class VersionHistoryDialog final : public QDialog {
    Q_OBJECT

public:
    /// `store` is borrowed: the window owns it, and the dialog lives for a visit.
    ///
    /// `current_text` is what the editor holds now, which is what a snapshot is
    /// compared with. Empty means there is nothing to compare against, and the
    /// comparison is offered but explains that rather than showing an empty diff.
    VersionHistoryDialog(const AutosaveStore& store, QString document,
                         QString current_text = QString{}, QWidget* parent = nullptr);

    /// The text of the version the user chose, empty when they cancelled.
    [[nodiscard]] QString chosenText() const { return chosen_text_; }
    /// How many versions the dialog listed. For the tests.
    [[nodiscard]] int versionCount() const;
    /// The comparison as it is on screen, empty when it is not being shown.
    [[nodiscard]] QString diffText() const;
    /// The two views and the tick that chooses between them, for the tests.
    [[nodiscard]] QPlainTextEdit* preview() const noexcept { return preview_; }
    [[nodiscard]] QPlainTextEdit* comparison() const noexcept { return diff_; }
    [[nodiscard]] QCheckBox* compareBox() const noexcept { return compare_; }

private:
    void showSelected();
    /// Rebuilds the diff view from the selected snapshot and the current text, and
    /// colours its lines.
    void refreshComparison();
    void showPage();

    const AutosaveStore& store_;
    QString document_;
    QString current_text_;
    QList<Snapshot> snapshots_;
    QString chosen_text_;
    QListWidget* list_ = nullptr;
    QStackedWidget* panes_ = nullptr;
    QPlainTextEdit* preview_ = nullptr;
    QPlainTextEdit* diff_ = nullptr;
    QCheckBox* compare_ = nullptr;
    QLabel* status_ = nullptr;
    QPushButton* restore_ = nullptr;
};

}  // namespace timewarp::ui
