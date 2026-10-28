// SPDX-License-Identifier: MIT
//
// The command reference: every command, the smallest program that runs it, and what
// it does -- read from the same table the gates drive.
//
// `native/share/command_rows.json` is the command examination's data: 566 rows, one
// per command, each a program and what should happen. `time_warp_gates commands` runs
// everything in it, so a row that is wrong fails the build. This page shows the same
// rows to a person, which makes the reference and the gate one fact rather than two
// documents that agree until somebody edits one. It is the reason the table is
// installed with the application.
//
// The panel never runs anything: **Try it** puts the row's program in a new document
// and the Run key is the user's to press. A reference that executed what you clicked
// would be a reference that could loop forever on a typo.

#pragma once

#include <QFrame>
#include <QList>
#include <QString>

#include "timewarp/ui/language.hpp"

class QComboBox;
class QLabel;
class QLineEdit;
class QPlainTextEdit;
class QPushButton;
class QTreeWidget;

namespace timewarp::ui {

/// One row of the table: a command, the smallest program that exercises it, and the
/// answer the gates expect.
struct CommandRow {
    /// The row's language as the table spells it: `BASIC`, `CHIP8`, `SMALLTALK`. It
    /// is the enumerator name, so `languageFromKey(language.toLower())` maps it.
    QString language;
    QString command;
    QString source;
    /// `prints:TEXT`, `draws`, `error`, `any` or `optional`.
    QString expect;
};

/// Reads the table. `error` is set when there is no table or it cannot be read; an
/// empty list with no error is a table with no rows.
[[nodiscard]] QList<CommandRow> loadCommandRows(const QString& path, QString* error = nullptr);

/// Where the table is, or empty when this build has none. `TIME_WARP_COMMANDS`
/// overrides it, as the other catalogues do.
[[nodiscard]] QString commandRowsPath();

/// The panel: a language chooser, a filter, the rows, and the selected row's program.
class CommandReferencePanel final : public QFrame {
    Q_OBJECT

public:
    explicit CommandReferencePanel(QWidget* parent = nullptr);

    /// Reads the table. Called once, when the page is first raised: a user who never
    /// opens the reference should not pay for parsing it.
    void ensureLoaded();
    /// Pre-selects the document's language, so the page opens on what is in front of
    /// the user.
    void setLanguage(Language language);
    /// The language whose rows are listed.
    [[nodiscard]] Language language() const;
    /// How many rows the filter is showing.
    [[nodiscard]] int visibleRowCount() const;
    /// Sets the filter text as if it had been typed.
    void setFilter(const QString& text);
    /// The program of the selected row, or empty.
    [[nodiscard]] QString selectedSource() const;
    [[nodiscard]] QString statusText() const;
    /// The table's row count for the current language, before filtering.
    [[nodiscard]] int rowCount() const;

signals:
    /// The user pressed **Try it**: put this program in a document for them.
    void tryRequested(const QString& languageKey, const QString& source, const QString& name);
    void statusMessage(const QString& message);

private:
    void buildUi();
    void rebuild();
    void updateDetail();
    void chooseTryIt();

    QList<CommandRow> rows_;
    bool loaded_ = false;
    QString load_error_;
    QComboBox* language_ = nullptr;
    QLineEdit* filter_ = nullptr;
    QTreeWidget* list_ = nullptr;
    QPlainTextEdit* detail_ = nullptr;
    QLabel* effect_ = nullptr;
    QPushButton* try_ = nullptr;
    QLabel* status_ = nullptr;
};

}  // namespace timewarp::ui
