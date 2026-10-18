// SPDX-License-Identifier: MIT
//
// The version history: what autosave has kept for this document, newest first,
// with the text beside it.
//
// Ported from `ui/version_history_dialog.py` (203 lines). One deliberate
// difference from the original: restoring always opens a **new tab** and never
// writes the file. Choosing the wrong version should cost a tab, not the work
// that was there.

#pragma once

#include <QDialog>
#include <QList>

#include "timewarp/ui/autosave.hpp"

class QLabel;
class QListWidget;
class QPlainTextEdit;
class QPushButton;

namespace timewarp::ui {

class AutosaveStore;

class VersionHistoryDialog final : public QDialog {
    Q_OBJECT

public:
    /// `store` is borrowed: the window owns it, and the dialog lives for a visit.
    VersionHistoryDialog(const AutosaveStore& store, QString document, QWidget* parent = nullptr);

    /// The text of the version the user chose, empty when they cancelled.
    [[nodiscard]] QString chosenText() const { return chosen_text_; }
    /// How many versions the dialog listed. For the tests.
    [[nodiscard]] int versionCount() const;

private:
    void showSelected();

    const AutosaveStore& store_;
    QString document_;
    QList<Snapshot> snapshots_;
    QString chosen_text_;
    QListWidget* list_ = nullptr;
    QPlainTextEdit* preview_ = nullptr;
    QLabel* status_ = nullptr;
    QPushButton* restore_ = nullptr;
};

}  // namespace timewarp::ui
