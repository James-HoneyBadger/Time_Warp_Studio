// SPDX-License-Identifier: MIT

#include "timewarp/core/qt_gui_compat.hpp"
#include "timewarp/ui/sql_panel.hpp"

#include <QDir>
#include <QFontDatabase>
#include <QKeySequence>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QListWidget>
#include <QMenu>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QSplitter>
#include <QTableWidget>
#include <QTextCursor>
#include <QToolButton>
#include <QVBoxLayout>

namespace timewarp::ui {

namespace {

QString quotedTable(const QString& table) {
    QString escaped = table;
    escaped.replace(QLatin1Char('"'), QStringLiteral("\"\""));
    return QStringLiteral("\"%1\"").arg(escaped);
}

}  // namespace

SqlPanel::SqlPanel(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("sqlPanel"));
    setMinimumSize(200, 120);
    buildUi();
    setDatabase(QString{}, {});
}

void SqlPanel::buildUi() {
    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(6, 6, 6, 6);
    layout->setSpacing(4);

    auto* header = new QHBoxLayout;
    header->setSpacing(4);
    path_ = new QLabel;
    path_->setObjectName(QStringLiteral("sqlDatabasePath"));
    path_->setWordWrap(false);
    path_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    header->addWidget(path_, 1);
    auto* open = new QPushButton{tr("Open...")};
    open->setObjectName(QStringLiteral("sqlOpenDatabase"));
    header->addWidget(open);
    auto* create = new QPushButton{tr("New...")};
    create->setObjectName(QStringLiteral("sqlNewDatabase"));
    header->addWidget(create);
    recent_ = new QToolButton;
    recent_->setObjectName(QStringLiteral("sqlRecentDatabases"));
    recent_->setText(tr("Recent"));
    recent_->setPopupMode(QToolButton::InstantPopup);
    recent_->setMenu(new QMenu{recent_});
    header->addWidget(recent_);
    layout->addLayout(header);
    connect(open, &QPushButton::clicked, this, &SqlPanel::openDatabaseRequested);
    connect(create, &QPushButton::clicked, this, &SqlPanel::newDatabaseRequested);

    empty_state_ = new QWidget;
    auto* empty_layout = new QVBoxLayout{empty_state_};
    empty_layout->setContentsMargins(0, 12, 0, 0);
    auto* empty_text = new QLabel(
        tr("No database is open.\n\nOpen a SQLite file or create one, then run "
           "SQL here or from a program with `SQL SELECT ...`."));
    empty_text->setWordWrap(true);
    empty_layout->addWidget(empty_text);
    auto* empty_row = new QHBoxLayout;
    auto* empty_open = new QPushButton{tr("Open Database...")};
    auto* empty_new = new QPushButton{tr("New Database...")};
    empty_row->addWidget(empty_open);
    empty_row->addWidget(empty_new);
    empty_row->addStretch(1);
    empty_layout->addLayout(empty_row);
    empty_layout->addStretch(1);
    connect(empty_open, &QPushButton::clicked, this, &SqlPanel::openDatabaseRequested);
    connect(empty_new, &QPushButton::clicked, this, &SqlPanel::newDatabaseRequested);
    layout->addWidget(empty_state_, 1);

    work_area_ = new QWidget;
    auto* work = new QHBoxLayout{work_area_};
    work->setContentsMargins(0, 0, 0, 0);
    work->setSpacing(4);

    auto* left = new QWidget;
    auto* left_layout = new QVBoxLayout{left};
    left_layout->setContentsMargins(0, 0, 0, 0);
    left_layout->setSpacing(2);
    auto* tables_label = new QLabel{tr("Tables")};
    left_layout->addWidget(tables_label);
    tables_ = new QListWidget;
    tables_->setObjectName(QStringLiteral("sqlTables"));
    tables_->setMinimumWidth(130);
    tables_->setContextMenuPolicy(Qt::CustomContextMenu);
    left_layout->addWidget(tables_, 1);
    auto* table_buttons = new QHBoxLayout;
    table_buttons->setSpacing(2);
    auto* describe = new QPushButton{tr("Schema")};
    describe->setObjectName(QStringLiteral("sqlSchemaButton"));
    table_buttons->addWidget(describe);
    refresh_ = new QPushButton{tr("Refresh")};
    refresh_->setObjectName(QStringLiteral("sqlRefreshTables"));
    table_buttons->addWidget(refresh_);
    table_buttons->addStretch(1);
    left_layout->addLayout(table_buttons);
    work->addWidget(left);

    auto* right = new QWidget;
    auto* right_layout = new QVBoxLayout{right};
    right_layout->setContentsMargins(0, 0, 0, 0);
    right_layout->setSpacing(2);
    editor_ = new QPlainTextEdit;
    editor_->setObjectName(QStringLiteral("sqlEditor"));
    editor_->setPlaceholderText(
        tr("SELECT * FROM scores;\nCREATE TABLE scores (name TEXT, score INTEGER);"));
    QFont mono = QFontDatabase::systemFont(QFontDatabase::FixedFont);
    editor_->setFont(mono);
    editor_->setMaximumHeight(110);
    right_layout->addWidget(editor_);
    auto* run_row = new QHBoxLayout;
    run_row->setSpacing(4);
    run_ = new QPushButton{tr("Run")};
    run_->setObjectName(QStringLiteral("sqlRun"));
    run_->setShortcut(timewarp::core::keySequence(Qt::CTRL, Qt::Key_Return));
    run_row->addWidget(run_);
    run_row->addStretch(1);
    right_layout->addLayout(run_row);
    results_ = new QTableWidget;
    results_->setObjectName(QStringLiteral("sqlResults"));
    results_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    results_->setAlternatingRowColors(true);
    results_->setSelectionMode(QAbstractItemView::SingleSelection);
    results_->verticalHeader()->setVisible(false);
    right_layout->addWidget(results_, 1);
    work->addWidget(right, 1);

    layout->addWidget(work_area_, 1);

    status_ = new QLabel;
    status_->setObjectName(QStringLiteral("sqlStatus"));
    status_->setWordWrap(true);
    layout->addWidget(status_);

    connect(run_, &QPushButton::clicked, this, [this] { runCurrent(); });
    connect(refresh_, &QPushButton::clicked, this, &SqlPanel::refreshRequested);
    connect(describe, &QPushButton::clicked, this, [this] {
        if (QListWidgetItem* item = tables_->currentItem(); item != nullptr) {
            emit schemaRequested(item->text());
        } else {
            setStatus(tr("Select a table first."), true);
        }
    });
    connect(tables_, &QListWidget::itemDoubleClicked, this,
            [this](QListWidgetItem* item) { activateTable(item); });
    connect(tables_, &QListWidget::customContextMenuRequested, this,
            [this](const QPoint& position) {
                QListWidgetItem* item = tables_->itemAt(position);
                if (item == nullptr) {
                    return;
                }
                QMenu menu{this};
                menu.addAction(tr("Select First 100 Rows"), this,
                               [this, item] { activateTable(item); });
                menu.addAction(tr("Show Schema"), this,
                               [this, item] { emit schemaRequested(item->text()); });
                menu.addAction(tr("Use in Editor"), this, [this, item] {
                    setQuery(quotedTable(item->text()));
                    focusQuery();
                });
                menu.exec(tables_->viewport()->mapToGlobal(position));
            });
}

void SqlPanel::setDatabase(const QString& path, const QStringList& tables) {
    const bool open = !path.isEmpty();
    path_->setText(open ? QDir::toNativeSeparators(path)
                        : tr("No database open"));
    path_->setToolTip(open ? QDir::toNativeSeparators(path) : QString{});
    empty_state_->setVisible(!open);
    work_area_->setVisible(open);
    setTables(tables);
    if (open) {
        setStatus(tr("%1 table%2").arg(tables.size())
                      .arg(tables.size() == 1 ? QString{} : QStringLiteral("s")));
    } else {
        setStatus(QString{});
    }
}

QString SqlPanel::databasePath() const { return path_->text(); }

void SqlPanel::setTables(const QStringList& tables) {
    tables_->clear();
    for (const QString& table : tables) {
        auto* item = new QListWidgetItem{table};
        item->setToolTip(tr("Double-click to select the first 100 rows"));
        tables_->addItem(item);
    }
}

int SqlPanel::tableCount() const { return tables_->count(); }

void SqlPanel::setRecentDatabases(const QStringList& paths) {
    QMenu* menu = recent_->menu();
    menu->clear();
    for (const QString& path : paths) {
        QAction* action = menu->addAction(QDir::toNativeSeparators(path));
        connect(action, &QAction::triggered, this,
                [this, path] { emit recentDatabaseRequested(path); });
    }
    if (paths.isEmpty()) {
        menu->addAction(tr("(none)"))->setEnabled(false);
    }
}

QString SqlPanel::query() const { return editor_->toPlainText(); }

void SqlPanel::setQuery(const QString& sql) { editor_->setPlainText(sql); }

void SqlPanel::focusQuery() {
    editor_->setFocus(Qt::ShortcutFocusReason);
    editor_->moveCursor(QTextCursor::End);
}

void SqlPanel::runCurrent() {
    const QString sql = editor_->toPlainText().trimmed();
    if (sql.isEmpty()) {
        setStatus(tr("Type a SQL statement first."), true);
        return;
    }
    emit runRequested(sql);
}

void SqlPanel::activateTable(QListWidgetItem* item) {
    if (item == nullptr) {
        return;
    }
    const QString sql = QStringLiteral("SELECT * FROM %1 LIMIT 100;")
                            .arg(quotedTable(item->text()));
    setQuery(sql);
    emit runRequested(sql);
}

void SqlPanel::setResult(const timewarp::sql::QueryResult& result) {
    results_->clear();
    results_->setRowCount(0);
    results_->setColumnCount(0);
    if (!result.ok()) {
        setStatus(QStringLiteral("\u274C %1").arg(result.error), true);
        return;
    }

    if (!result.columns.isEmpty()) {
        results_->setColumnCount(result.columns.size());
        results_->setHorizontalHeaderLabels(result.columns);
        results_->setRowCount(result.rows.size());
        for (int row = 0; row < result.rows.size(); ++row) {
            const QStringList& values = result.rows.at(row);
            for (int column = 0; column < result.columns.size() && column < values.size();
                 ++column) {
                results_->setItem(row, column, new QTableWidgetItem{values.at(column)});
            }
        }
        results_->resizeColumnsToContents();
        const int last = result.columns.size() - 1;
        if (last >= 0) {
            results_->horizontalHeader()->setSectionResizeMode(last, QHeaderView::Stretch);
        }
    }

    QString status = result.columns.isEmpty()
                         ? (result.changes > 0
                                ? tr("%1 row%2 changed")
                                      .arg(result.changes)
                                      .arg(result.changes == 1 ? QString{}
                                                               : QStringLiteral("s"))
                                : tr("Statement executed"))
                         : tr("%1 row%2 returned")
                               .arg(result.rows.size())
                               .arg(result.rows.size() == 1 ? QString{} : QStringLiteral("s"));
    if (result.truncated) {
        status += tr(" (first %1 shown)").arg(timewarp::sql::Database::kMaxRows);
    }
    setStatus(status);
}

void SqlPanel::setStatus(const QString& text, bool error) {
    status_->setText(text);
    status_->setStyleSheet(error ? QStringLiteral("color: #ff6b6b;") : QString{});
}

int SqlPanel::resultRowCount() const { return results_->rowCount(); }

int SqlPanel::resultColumnCount() const { return results_->columnCount(); }

QString SqlPanel::statusText() const { return status_->text(); }

}  // namespace timewarp::ui
