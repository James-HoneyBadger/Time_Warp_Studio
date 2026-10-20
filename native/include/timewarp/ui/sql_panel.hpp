// SPDX-License-Identifier: MIT
//
// The Database page: open a SQLite file, browse its tables and run SQL.
//
// This is a manager rather than a query box: the table list is live, a table can
// be selected or described, scripts of several statements run in one go, and the
// result grid says how many rows came back or changed. The same connection is
// what a program's `SQL` statement uses, so what the learner types here is what
// the program sees.
//
// Every mutation is a signal. The window owns the connection and the dialogs, for
// the same reason the Project panel does not open files: one path through the
// model is one place for it to be saved.

#pragma once

#include <QFrame>
#include <QString>

#include "timewarp/sql/database.hpp"

class QLabel;
class QListWidget;
class QListWidgetItem;
class QToolButton;
class QPlainTextEdit;
class QPushButton;
class QTableWidget;

namespace timewarp::ui {

class SqlPanel final : public QFrame {
    Q_OBJECT

public:
    explicit SqlPanel(QWidget* parent = nullptr);

    /// What is open, and the tables in it. An empty `path` shows the empty state.
    void setDatabase(const QString& path, const QStringList& tables);
    [[nodiscard]] QString databasePath() const;
    void setTables(const QStringList& tables);
    [[nodiscard]] int tableCount() const;

    /// Databases opened before, newest first. The panel owns the menu; the window
    /// owns the list because it is persisted with the other session state.
    void setRecentDatabases(const QStringList& paths);

    /// The SQL in the editor, and a way to set it from a table click.
    [[nodiscard]] QString query() const;
    void setQuery(const QString& sql);
    void focusQuery();

    /// Shows one statement's result: columns and rows, or the error.
    void setResult(const timewarp::sql::QueryResult& result);
    /// A one-line answer that is not a result, e.g. "Saved".
    void setStatus(const QString& text, bool error = false);

    [[nodiscard]] int resultRowCount() const;
    [[nodiscard]] int resultColumnCount() const;
    [[nodiscard]] QString statusText() const;

signals:
    void openDatabaseRequested();
    void newDatabaseRequested();
    void recentDatabaseRequested(const QString& path);
    void runRequested(const QString& sql);
    void refreshRequested();
    /// The user asked for one table's `CREATE` statement.
    void schemaRequested(const QString& table);

private:
    void buildUi();
    void runCurrent();
    void activateTable(QListWidgetItem* item);

    QLabel* path_ = nullptr;
    QLabel* status_ = nullptr;
    QToolButton* recent_ = nullptr;
    QPushButton* run_ = nullptr;
    QPushButton* refresh_ = nullptr;
    QListWidget* tables_ = nullptr;
    QPlainTextEdit* editor_ = nullptr;
    QTableWidget* results_ = nullptr;
    QWidget* empty_state_ = nullptr;
    QWidget* work_area_ = nullptr;
};

}  // namespace timewarp::ui
