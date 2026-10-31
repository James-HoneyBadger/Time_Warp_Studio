// SPDX-License-Identifier: MIT

#include "timewarp/ui/panels.hpp"
#include "timewarp/core/qt_names.hpp"

#include <cmath>

#include <QComboBox>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QPushButton>
#include <QSpinBox>
#include <QTableWidget>
#include <QVBoxLayout>

namespace timewarp::ui {

// ---------------------------------------------------------------------------
// VariablesPanel
// ---------------------------------------------------------------------------

VariablesPanel::VariablesPanel(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("variablesPanel"));
    setMinimumSize(160, 100);

    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(6, 6, 6, 6);
    layout->setSpacing(4);

    filter_ = new QLineEdit;
    filter_->setObjectName(QStringLiteral("variablesFilter"));
    filter_->setPlaceholderText(tr("Filter by name"));
    filter_->setClearButtonEnabled(true);
    layout->addWidget(filter_);

    table_ = new QTableWidget{0, 3};
    table_->setObjectName(QStringLiteral("variablesTable"));
    table_->setHorizontalHeaderLabels({tr("Name"), tr("Type"), tr("Value")});
    table_->verticalHeader()->setVisible(false);
    table_->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    table_->horizontalHeader()->setSectionResizeMode(1, QHeaderView::ResizeToContents);
    table_->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Stretch);
    // Without a floor, an empty table sizes the type column to its header text
    // and clips the word "Type" -- which reads as a broken table rather than an
    // empty one.
    table_->horizontalHeader()->setMinimumSectionSize(56);
    // Read-only because the engine holds the value: an edit here would have
    // nowhere to go, and a table that looked editable and discarded the edit is
    // worse than one that does not offer.
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    table_->setSelectionBehavior(QAbstractItemView::SelectRows);
    layout->addWidget(table_, 1);

    status_ = new QLabel;
    status_->setObjectName(QStringLiteral("variablesStatus"));
    layout->addWidget(status_);

    connect(filter_, &QLineEdit::textChanged, this, [this](const QString&) { applyFilter(); });
    applyFilter();
}

void VariablesPanel::setRows(const QList<timewarp::ui::VariableRow>& rows) {
    rows_ = rows;
    applyFilter();
}

void VariablesPanel::setFilter(const QString& text) { filter_->setText(text); }

int VariablesPanel::rowCount() const { return static_cast<int>(rows_.size()); }

int VariablesPanel::visibleRowCount() const { return table_->rowCount(); }

void VariablesPanel::applyFilter() {
    const QString needle = filter_->text().trimmed();
    table_->setRowCount(0);

    int shown = 0;
    for (const timewarp::ui::VariableRow& row : rows_) {
        if (!needle.isEmpty() && !row.name.contains(needle, Qt::CaseInsensitive)) {
            continue;
        }
        const int index = table_->rowCount();
        table_->insertRow(index);
        table_->setItem(index, 0, new QTableWidgetItem{row.name});
        table_->setItem(index, 1, new QTableWidgetItem{row.type});
        table_->setItem(index, 2, new QTableWidgetItem{row.value});
        ++shown;
    }

    if (rows_.isEmpty()) {
        // "No variables" after a run is a fact about the program, so it is worth
        // saying; "no variables" before one would be a claim nobody has earned.
        status_->setText(tr("nothing to show yet \u2014 run a program"));
    } else if (shown == rows_.size()) {
        status_->setText(tr("%n variable(s)", "", shown));
    } else {
        status_->setText(tr("%1 of %2 variables").arg(shown).arg(rows_.size()));
    }
}

// ---------------------------------------------------------------------------
// TelemetryPanel
// ---------------------------------------------------------------------------

TelemetryPanel::TelemetryPanel(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("telemetryPanel"));
    setMinimumSize(160, 100);

    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(6, 6, 6, 6);

    table_ = new QTableWidget{0, 2};
    table_->setObjectName(QStringLiteral("telemetryTable"));
    table_->setHorizontalHeaderLabels({tr("Measure"), tr("Value")});
    table_->verticalHeader()->setVisible(false);
    table_->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    table_->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    layout->addWidget(table_, 1);

    setRows({});
}

void TelemetryPanel::setRows(const QList<timewarp::ui::TelemetryRow>& rows) {
    table_->setRowCount(0);
    if (rows.isEmpty()) {
        // One row stating the state, rather than an empty grid that looks broken.
        table_->insertRow(0);
        table_->setItem(0, 0, new QTableWidgetItem{tr("Nothing yet")});
        table_->setItem(0, 1, new QTableWidgetItem{tr("run a program")});
        return;
    }
    for (const timewarp::ui::TelemetryRow& row : rows) {
        const int index = table_->rowCount();
        table_->insertRow(index);
        table_->setItem(index, 0, new QTableWidgetItem{row.label});
        table_->setItem(index, 1, new QTableWidgetItem{row.value});
    }
}

int TelemetryPanel::rowCount() const {
    // The placeholder row is not a measurement.
    if (table_->rowCount() == 1 && table_->item(0, 0) != nullptr &&
        table_->item(0, 0)->text() == tr("Nothing yet")) {
        return 0;
    }
    return table_->rowCount();
}

QString TelemetryPanel::valueOf(const QString& label) const {
    for (int row = 0; row < table_->rowCount(); ++row) {
        const QTableWidgetItem* name = table_->item(row, 0);
        const QTableWidgetItem* value = table_->item(row, 1);
        if (name != nullptr && value != nullptr && name->text() == label) {
            return value->text();
        }
    }
    return {};
}

// ---------------------------------------------------------------------------
// DebugPanel
// ---------------------------------------------------------------------------

DebugPanel::DebugPanel(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("debugPanel"));
    setMinimumSize(160, 100);

    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(6, 6, 6, 6);
    layout->setSpacing(4);

    status_ = new QLabel;
    status_->setObjectName(QStringLiteral("debugStatus"));
    status_->setWordWrap(true);
    layout->addWidget(status_);

    reason_ = new QLabel;
    reason_->setObjectName(QStringLiteral("debugReason"));
    reason_->setWordWrap(true);
    layout->addWidget(reason_);

    auto* transport = new QHBoxLayout;
    transport->setSpacing(4);
    const auto button = [transport](const QString& text, const QString& name) {
        auto* item = new QPushButton{text};
        item->setObjectName(name);
        transport->addWidget(item);
        return item;
    };
    continue_ = button(tr("Continue"), QStringLiteral("debugContinue"));
    pause_ = button(tr("Pause"), QStringLiteral("debugPause"));
    step_into_ = button(tr("Step In"), QStringLiteral("debugStepInto"));
    step_over_ = button(tr("Step Over"), QStringLiteral("debugStepOver"));
    step_out_ = button(tr("Step Out"), QStringLiteral("debugStepOut"));
    stop_ = button(tr("Stop"), QStringLiteral("debugStop"));
    layout->addLayout(transport);

    connect(continue_, &QPushButton::clicked, this, &DebugPanel::continueRequested);
    connect(pause_, &QPushButton::clicked, this, &DebugPanel::pauseRequested);
    connect(step_into_, &QPushButton::clicked, this, &DebugPanel::stepIntoRequested);
    connect(step_over_, &QPushButton::clicked, this, &DebugPanel::stepOverRequested);
    connect(step_out_, &QPushButton::clicked, this, &DebugPanel::stepOutRequested);
    connect(stop_, &QPushButton::clicked, this, &DebugPanel::stopRequested);

    // Walking a run back needs somewhere to walk to, so the depth is a run setting
    // rather than a button: a session started with "off" has nothing behind it, and
    // the panel would otherwise be offering a control the engine cannot honour.
    auto* back_row = new QHBoxLayout;
    back_row->setSpacing(4);
    back_row->addWidget(new QLabel{tr("History")});
    history_ = new QComboBox;
    history_->setObjectName(QStringLiteral("debugHistory"));
    for (const int depth : {0, 100, 500, 2000}) {
        history_->addItem(depth == 0 ? tr("off") : QString::number(depth), depth);
    }
    history_->setToolTip(tr("Statements this run remembers, so it can be walked back"));
    back_row->addWidget(history_);
    back_ = new QPushButton{tr("\u25c0 Back")};
    back_->setObjectName(QStringLiteral("debugBack"));
    back_->setToolTip(tr("Go back one statement, with the bindings it had"));
    back_row->addWidget(back_);
    back_ten_ = new QPushButton{tr("\u25c0\u25c0 10")};
    back_ten_->setObjectName(QStringLiteral("debugBackTen"));
    back_ten_->setToolTip(tr("Go back ten statements, or as far as this run kept"));
    back_row->addWidget(back_ten_);
    back_row->addStretch(1);
    layout->addLayout(back_row);

    connect(back_, &QPushButton::clicked, this, &DebugPanel::requestRewind);
    connect(back_ten_, &QPushButton::clicked, this, &DebugPanel::requestRewindTen);
    connect(history_, QOverload<int>::of(&QComboBox::currentIndexChanged), this, [this] {
        emit historyChanged(historyDepth());
        // The choice applies to the next run, so a button that cannot move right
        // now must not stay enabled because the menu was just set.
        setRewindAvailable(rewind_available_ && historyDepth() > 0,
                           tr("Set History above, then run again to keep any"));
    });

    auto* breakpoint_row = new QHBoxLayout;
    breakpoint_row->setSpacing(4);
    breakpoint_row->addWidget(new QLabel{tr("Line")});
    line_ = new QSpinBox;
    line_->setObjectName(QStringLiteral("debugLine"));
    line_->setRange(1, 999999);
    breakpoint_row->addWidget(line_);
    add_ = new QPushButton{tr("Add")};
    add_->setObjectName(QStringLiteral("debugAddBreakpoint"));
    breakpoint_row->addWidget(add_);
    remove_ = new QPushButton{tr("Remove")};
    remove_->setObjectName(QStringLiteral("debugRemoveBreakpoint"));
    breakpoint_row->addWidget(remove_);
    layout->addLayout(breakpoint_row);

    connect(add_, &QPushButton::clicked, this, [this] { addBreakpoint(); });
    connect(remove_, &QPushButton::clicked, this, [this] { removeBreakpoint(); });

    breakpoints_ = new QListWidget;
    breakpoints_->setObjectName(QStringLiteral("debugBreakpoints"));
    breakpoints_->setMaximumHeight(90);
    layout->addWidget(breakpoints_);

    auto* stack_label = new QLabel{tr("Call stack")};
    layout->addWidget(stack_label);
    stack_ = new QListWidget;
    stack_->setObjectName(QStringLiteral("debugStack"));
    stack_->setMaximumHeight(110);
    layout->addWidget(stack_);

    auto* variables_label = new QLabel{tr("Variables here")};
    layout->addWidget(variables_label);
    variables_ = new QTableWidget{0, 2};
    variables_->setObjectName(QStringLiteral("debugVariables"));
    variables_->setHorizontalHeaderLabels({tr("Name"), tr("Value")});
    variables_->verticalHeader()->setVisible(false);
    variables_->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    variables_->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
    variables_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    layout->addWidget(variables_, 1);
    // Double-clicking a binding is how a user who does not know a variable's
    // spelling asks to watch it change.
    connect(variables_, &QTableWidget::cellDoubleClicked, this,
            [this](int row, int) {
                if (QTableWidgetItem* item = variables_->item(row, 0); item != nullptr) {
                    addDataBreakpoint(item->text());
                }
            });

    pause_reason_ = new QLabel;
    pause_reason_->setObjectName(QStringLiteral("debugPauseReason"));
    pause_reason_->setWordWrap(true);
    layout->addWidget(pause_reason_);

    auto* watch_label = new QLabel{tr("Watch expressions")};
    layout->addWidget(watch_label);
    auto* watch_row = new QHBoxLayout;
    watch_row->setSpacing(4);
    watch_input_ = new QLineEdit;
    watch_input_->setObjectName(QStringLiteral("debugWatchInput"));
    watch_input_->setPlaceholderText(tr("X * 2"));
    watch_row->addWidget(watch_input_, 1);
    watch_add_ = new QPushButton{tr("Add")};
    watch_add_->setObjectName(QStringLiteral("debugAddWatch"));
    watch_row->addWidget(watch_add_);
    watch_remove_ = new QPushButton{tr("Remove")};
    watch_remove_->setObjectName(QStringLiteral("debugRemoveWatch"));
    watch_row->addWidget(watch_remove_);
    layout->addLayout(watch_row);
    watches_ = new QTableWidget{0, 2};
    watches_->setObjectName(QStringLiteral("debugWatches"));
    watches_->setHorizontalHeaderLabels({tr("Expression"), tr("Value")});
    watches_->verticalHeader()->setVisible(false);
    watches_->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    watches_->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
    watches_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    watches_->setSelectionBehavior(QAbstractItemView::SelectRows);
    watches_->setMaximumHeight(100);
    layout->addWidget(watches_);

    connect(watch_add_, &QPushButton::clicked, this, [this] { addWatch(); });
    connect(watch_remove_, &QPushButton::clicked, this, [this] { removeWatch(); });
    connect(watch_input_, &QLineEdit::returnPressed, this, [this] { addWatch(); });

    auto* data_label = new QLabel{tr("Break when a variable changes")};
    layout->addWidget(data_label);
    auto* data_row = new QHBoxLayout;
    data_row->setSpacing(4);
    data_input_ = new QLineEdit;
    data_input_->setObjectName(QStringLiteral("debugDataBreakpointInput"));
    data_input_->setPlaceholderText(tr("name"));
    data_row->addWidget(data_input_, 1);
    data_add_ = new QPushButton{tr("Add")};
    data_add_->setObjectName(QStringLiteral("debugAddDataBreakpoint"));
    data_row->addWidget(data_add_);
    data_remove_ = new QPushButton{tr("Remove")};
    data_remove_->setObjectName(QStringLiteral("debugRemoveDataBreakpoint"));
    data_row->addWidget(data_remove_);
    layout->addLayout(data_row);
    data_breakpoints_ = new QListWidget;
    data_breakpoints_->setObjectName(QStringLiteral("debugDataBreakpoints"));
    data_breakpoints_->setMaximumHeight(70);
    layout->addWidget(data_breakpoints_);
    connect(data_add_, &QPushButton::clicked, this,
            [this] { addDataBreakpoint(data_input_->text()); });
    connect(data_remove_, &QPushButton::clicked, this,
            [this] { removeDataBreakpoint(); });
    connect(data_input_, &QLineEdit::returnPressed, this,
            [this] { addDataBreakpoint(data_input_->text()); });

    setRewindAvailable(false, QString{});
    setAvailable(false, tr("Run a Scheme program to step through it."));
}

void DebugPanel::setHistoryDepth(int depth) {
    const int index = history_->findData(depth);
    // An unrecognised depth is the closest thing the menu offers rather than a
    // silent switch to "off": a stored 1000 should keep 500, not nothing.
    if (index < 0) {
        int closest = 0;
        for (int i = 0; i < history_->count(); ++i) {
            if (history_->itemData(i).toInt() <= depth) {
                closest = i;
            }
        }
        history_->setCurrentIndex(closest);
        return;
    }
    history_->setCurrentIndex(index);
}

int DebugPanel::historyDepth() const { return history_->currentData().toInt(); }

bool DebugPanel::isRewindAvailable() const { return rewind_available_; }

void DebugPanel::setRewindAvailable(bool available, const QString& reason) {
    rewind_available_ = available;
    back_->setEnabled(available);
    back_ten_->setEnabled(available);
    back_->setToolTip(available ? tr("Go back one statement, with the bindings it had")
                                : reason);
    back_ten_->setToolTip(
        available ? tr("Go back ten statements, or as far as this run kept") : reason);
}

void DebugPanel::requestRewind() {
    if (back_->isEnabled()) {
        emit rewindRequested(1);
    }
}

void DebugPanel::requestRewindTen() {
    if (back_ten_->isEnabled()) {
        emit rewindRequested(10);
    }
}

void DebugPanel::setAvailable(bool available, const QString& reason) {
    const bool was_available = available_;
    available_ = available;
    reason_->setText(reason);
    reason_->setVisible(!available);
    for (QPushButton* button : {continue_, pause_, step_into_, step_over_, step_out_, stop_}) {
        button->setEnabled(available);
    }
    add_->setEnabled(available);
    remove_->setEnabled(available);
    line_->setEnabled(available);
    watch_input_->setEnabled(available);
    watch_add_->setEnabled(available);
    watch_remove_->setEnabled(available);
    data_input_->setEnabled(available);
    data_add_->setEnabled(available);
    data_remove_->setEnabled(available);
    if (!available) {
        // The lists stay readable: what the last run left is still true even when
        // the current document cannot be stepped.
        status_->setText(tr("not steppable"));
    } else if (!was_available) {
        // Only when capability returns, not on every document refresh: a tab
        // switch while a run is paused must not erase "paused at line 20" or the
        // reason a data breakpoint stopped it.
        setPaused(false, 0, QString{});
    }
}

bool DebugPanel::isAvailable() const { return available_; }

void DebugPanel::setPaused(bool paused, int line, const QString& procedure) {
    if (!available_) {
        return;
    }
    // "running" would be a lie after the run has finished, and the panel cannot
    // tell the two apart: all it knows is that nothing is paused. The status bar
    // is where "running" and "ready" are said.
    if (!paused) {
        status_->setText(tr("not paused"));
        setPauseReason(QString{});
        // Nothing is paused, so nothing can be walked back: a Back button that
        // stayed alive between pauses would be asking the engine to move a run
        // that is not stopped.
        if (rewind_available_) {
            setRewindAvailable(false, tr("Pause the run first"));
        }
        return;
    }
    // Line 0 means the program has no source line for the next unit (a binary
    // CHIP-8 ROM), so there is nothing to number.
    const QString where =
        procedure.isEmpty() ? tr("paused at line %1").arg(line)
                            : tr("paused at line %1 in %2").arg(line).arg(procedure);
    status_->setText(line > 0 ? where : tr("paused before the next instruction"));
}

void DebugPanel::setPauseReason(const QString& reason) {
    pause_reason_->setText(reason);
    pause_reason_->setVisible(!reason.isEmpty());
}

void DebugPanel::setWatches(const QStringList& expressions) {
    watches_->setRowCount(0);
    for (const QString& expression : expressions) {
        const QString trimmed = expression.trimmed();
        if (trimmed.isEmpty()) {
            continue;
        }
        const int row = watches_->rowCount();
        watches_->insertRow(row);
        watches_->setItem(row, 0, new QTableWidgetItem{trimmed});
        watches_->setItem(row, 1, new QTableWidgetItem{QStringLiteral("\u2014")});
    }
}

QStringList DebugPanel::watches() const {
    QStringList expressions;
    for (int row = 0; row < watches_->rowCount(); ++row) {
        if (QTableWidgetItem* item = watches_->item(row, 0); item != nullptr) {
            expressions.append(item->text());
        }
    }
    return expressions;
}

void DebugPanel::setWatchValues(const QList<WatchRow>& values) {
    // The expressions are the contract; values are matched by row and a value for
    // an expression that is no longer listed is ignored rather than appended.
    watches_->setRowCount(values.size());
    for (int row = 0; row < values.size(); ++row) {
        const WatchRow& entry = values.at(row);
        watches_->setItem(row, 0, new QTableWidgetItem{entry.expression});
        auto* value = new QTableWidgetItem{entry.value};
        if (!entry.ok) {
            value->setToolTip(entry.error);
            value->setForeground(palette().brush(QPalette::Disabled, QPalette::Text));
        }
        watches_->setItem(row, 1, value);
    }
}

void DebugPanel::setDataBreakpoints(const QStringList& names) {
    data_breakpoints_->clear();
    for (const QString& name : names) {
        if (!name.trimmed().isEmpty()) {
            data_breakpoints_->addItem(name.trimmed());
        }
    }
}

QStringList DebugPanel::dataBreakpoints() const {
    QStringList names;
    for (int row = 0; row < data_breakpoints_->count(); ++row) {
        names.append(data_breakpoints_->item(row)->text());
    }
    return names;
}

void DebugPanel::addWatch() {
    const QString expression = watch_input_->text().trimmed();
    if (expression.isEmpty()) {
        return;
    }
    for (int row = 0; row < watches_->rowCount(); ++row) {
        if (watches_->item(row, 0) != nullptr &&
            watches_->item(row, 0)->text() == expression) {
            watch_input_->clear();
            return;
        }
    }
    const int row = watches_->rowCount();
    watches_->insertRow(row);
    watches_->setItem(row, 0, new QTableWidgetItem{expression});
    watches_->setItem(row, 1, new QTableWidgetItem{QStringLiteral("\u2014")});
    watch_input_->clear();
    emitWatches();
}

void DebugPanel::removeWatch() {
    const int row = watches_->currentRow();
    if (row < 0) {
        return;
    }
    watches_->removeRow(row);
    emitWatches();
}

void DebugPanel::addDataBreakpoint(const QString& name) {
    const QString trimmed = name.trimmed();
    if (trimmed.isEmpty()) {
        return;
    }
    const QList<QListWidgetItem*> existing =
        data_breakpoints_->findItems(trimmed, Qt::MatchFixedString | Qt::MatchCaseSensitive);
    if (!existing.isEmpty()) {
        data_input_->clear();
        return;
    }
    data_breakpoints_->addItem(trimmed);
    data_input_->clear();
    emitDataBreakpoints();
}

void DebugPanel::removeDataBreakpoint() {
    if (QListWidgetItem* item = data_breakpoints_->currentItem(); item != nullptr) {
        delete data_breakpoints_->takeItem(data_breakpoints_->row(item));
        emitDataBreakpoints();
    }
}

void DebugPanel::emitWatches() { emit watchesChanged(watches()); }

void DebugPanel::emitDataBreakpoints() {
    emit dataBreakpointsChanged(dataBreakpoints());
}

void DebugPanel::setStack(const QList<Frame>& frames) {
    stack_->clear();
    for (const Frame& frame : frames) {
        stack_->addItem(tr("%1 (line %2)").arg(frame.procedure).arg(frame.line));
    }
    if (frames.isEmpty()) {
        stack_->addItem(tr("(empty)"));
    }
}

void DebugPanel::setVariables(const QList<timewarp::ui::VariableRow>& rows) {
    variables_->setRowCount(0);
    for (const timewarp::ui::VariableRow& row : rows) {
        const int index = variables_->rowCount();
        variables_->insertRow(index);
        variables_->setItem(index, 0, new QTableWidgetItem{row.name});
        variables_->setItem(index, 1, new QTableWidgetItem{row.value});
    }
}

void DebugPanel::setBreakpoints(const QList<int>& lines) {
    breakpoints_->clear();
    for (const int line : lines) {
        breakpoints_->addItem(tr("line %1").arg(line));
    }
}

QList<int> DebugPanel::breakpoints() const {
    QList<int> lines;
    for (int row = 0; row < breakpoints_->count(); ++row) {
        // The items read "line 12"; the number after the space is the breakpoint.
        lines.append(breakpoints_->item(row)->text().section(QLatin1Char(' '), 1).toInt());
    }
    return lines;
}

void DebugPanel::setLineNumber(int line) { line_->setValue(line); }

int DebugPanel::lineNumber() const { return line_->value(); }

void DebugPanel::addBreakpoint() {
    const int line = line_->value();
    if (line <= 0) {
        return;
    }
    for (int row = 0; row < breakpoints_->count(); ++row) {
        if (breakpoints_->item(row)->text() == tr("line %1").arg(line)) {
            return;  // Already there: a set, not a list.
        }
    }
    breakpoints_->addItem(tr("line %1").arg(line));
    emitBreakpoints();
}

void DebugPanel::removeBreakpoint() {
    const QString wanted = tr("line %1").arg(line_->value());
    for (int row = 0; row < breakpoints_->count(); ++row) {
        if (breakpoints_->item(row)->text() == wanted) {
            delete breakpoints_->takeItem(row);
            emitBreakpoints();
            return;
        }
    }
}

void DebugPanel::requestContinue() { emit continueRequested(); }
void DebugPanel::requestPause() { emit pauseRequested(); }
void DebugPanel::requestStepInto() { emit stepIntoRequested(); }
void DebugPanel::requestStepOver() { emit stepOverRequested(); }
void DebugPanel::requestStepOut() { emit stepOutRequested(); }
void DebugPanel::requestStop() { emit stopRequested(); }

void DebugPanel::emitBreakpoints() { emit breakpointsChanged(breakpoints()); }

void DebugPanel::toggleBreakpointAt(int line) {
    if (line <= 0) {
        return;
    }
    setLineNumber(line);
    const QString wanted = tr("line %1").arg(line);
    for (int row = 0; row < breakpoints_->count(); ++row) {
        if (breakpoints_->item(row)->text() == wanted) {
            delete breakpoints_->takeItem(row);
            emitBreakpoints();
            return;
        }
    }
    breakpoints_->addItem(wanted);
    emitBreakpoints();
}

// ---------------------------------------------------------------------------
// TurtleInspector and TurtleCommandLog
// ---------------------------------------------------------------------------

namespace {

/// Up to three decimals without the trailing zeros: a log line for `RIGHT 90`
/// should read 90, not 90.000.
QString plain_number(double value) {
    QString text = QString::number(value, 'f', 3);
    if (text.contains(QLatin1Char{'.'})) {
        while (text.endsWith(QLatin1Char{'0'})) {
            text.chop(1);
        }
        if (text.endsWith(QLatin1Char{'.'})) {
            text.chop(1);
        }
    }
    if (text == QLatin1String{"-0"}) {
        text = QStringLiteral("0");
    }
    return text;
}

QString colour_text(std::uint32_t colour) {
    return QStringLiteral("#%1").arg(colour & 0xFFFFFFu, 6, 16, QLatin1Char{'0'}).toUpper();
}

/// A command's text payload, quoted and clipped. A sprite name or a `DRAWTEXT`
/// string can be arbitrarily long, and one of them must not push every argument
/// off the visible line.
QString quoted_text(std::string_view text) {
    QString value = timewarp::core::fromUtf8(text);
    if (value.size() > 24) {
        value = value.left(23) + QChar{0x2026};
    }
    return QLatin1Char{'"'} + value + QLatin1Char{'"'};
}

QString point_count_text(std::size_t count) {
    return count == 1 ? QStringLiteral("1 point") : QStringLiteral("%1 points").arg(count);
}

}  // namespace

TurtleInspector::TurtleInspector(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("turtleInspector"));
    setFrameShape(QFrame::NoFrame);
    // The strip takes its height from its font, so the text-size ladder grows it
    // rather than clipping it, and the canvas keeps the rest.
    setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);

    auto* row = new QHBoxLayout{this};
    row->setContentsMargins(6, 1, 6, 1);
    row->setSpacing(14);

    pose_ = new QLabel{this};
    pose_->setObjectName(QStringLiteral("turtlePose"));
    pose_->setAccessibleName(tr("Turtle position and heading"));
    pen_ = new QLabel{this};
    pen_->setObjectName(QStringLiteral("turtlePen"));
    pen_->setAccessibleName(tr("Pen state, colour and width"));
    commands_label_ = new QLabel{this};
    commands_label_->setObjectName(QStringLiteral("turtleCommands"));
    commands_label_->setAccessibleName(tr("Drawing commands received"));
    screen_label_ = new QLabel{this};
    screen_label_->setObjectName(QStringLiteral("turtleScreen"));
    screen_label_->setAccessibleName(tr("Screen mode"));

    row->addWidget(pose_);
    row->addWidget(pen_);
    row->addWidget(screen_label_);
    row->addStretch(1);
    row->addWidget(commands_label_);

    clear();
}

void TurtleInspector::showPose(const QPointF& position, double heading, bool pen_down,
                               const QColor& pen_colour, double pen_width) {
    // The canvas keeps whatever heading it was turned to, which may be past a
    // full circle or negative; the strip shows the heading a person would name.
    double normalised = std::fmod(heading, 360.0);
    if (normalised < 0.0) {
        normalised += 360.0;
    }
    pose_->setText(tr("X %1   Y %2   Heading %3\u00B0")
                       .arg(plain_number(position.x()), plain_number(position.y()),
                            plain_number(normalised)));
    pen_->setText(tr("Pen %1   %2   %3 px")
                      .arg(pen_down ? tr("DOWN") : tr("UP"), pen_colour.name().toUpper(),
                           plain_number(pen_width)));
}

void TurtleInspector::setCommandCount(int count) {
    commands_ = count;
    commands_label_->setText(count == 1 ? tr("1 command") : tr("%1 commands").arg(count));
}

void TurtleInspector::setScreenMode(int width, int height) {
    if (width <= 0 || height <= 0) {
        screen_label_->clear();
        return;
    }
    screen_label_->setText(tr("SCREEN %1x%2").arg(width).arg(height));
}

void TurtleInspector::clear() {
    showPose(QPointF{0.0, 0.0}, 0.0, true, QColor{Qt::white}, 1.0);
    setCommandCount(0);
    setScreenMode(0, 0);
}

QString TurtleInspector::poseText() const { return pose_->text(); }

QString TurtleInspector::penText() const { return pen_->text(); }

QString TurtleInspector::screenText() const { return screen_label_->text(); }

TurtleCommandLog::TurtleCommandLog(QWidget* parent) : QPlainTextEdit(parent) {
    setObjectName(QStringLiteral("turtleLog"));
    setAccessibleName(tr("Turtle Command Log"));
    setReadOnly(true);
    setFont(QFont{QStringLiteral("Courier New"), 10});
    setPlaceholderText(tr("Turtle \U0001F422 commands appear here\u2026"));
    // A command is one line and its arguments are its columns, so wrapping would
    // turn one command into two and break the alignment that makes the log
    // scannable.
    setLineWrapMode(QPlainTextEdit::NoWrap);
    setMaximumBlockCount(kMaxLines);
}

void TurtleCommandLog::appendCommand(const timewarp::canvas::CanvasCommandView& view) {
    ++count_;
    // Numbered, because "the third FORWARD" is how somebody refers to a command
    // when comparing the log with the program that emitted it.
    appendPlainText(QStringLiteral("%1  %2")
                        .arg(count_, 5, 10, QLatin1Char{' '})
                        .arg(describe(view)));
}

void TurtleCommandLog::clearLog() {
    count_ = 0;
    clear();
}

QString TurtleCommandLog::describe(const timewarp::canvas::CanvasCommandView& view) {
    using timewarp::canvas::CanvasCommandKind;

    const timewarp::canvas::CanvasCommand& command = view.command;
    // `to_string` answers a `string_view`, and Qt 5's `fromLatin1` has no such
    // overload: the compatibility seam is where that difference lives.
    const QString name =
        timewarp::core::fromUtf8(timewarp::canvas::to_string(command.kind)).toUpper();

    switch (command.kind) {
        case CanvasCommandKind::Forward:
        case CanvasCommandKind::Back:
        case CanvasCommandKind::Left:
        case CanvasCommandKind::Right:
        case CanvasCommandKind::SetX:
        case CanvasCommandKind::SetY:
        case CanvasCommandKind::SetHeading:
        case CanvasCommandKind::SetPenWidth:
            return name + QLatin1Char{' '} + plain_number(command.x);

        case CanvasCommandKind::Goto:
            return QStringLiteral("%1 %2 %3")
                .arg(name, plain_number(command.x), plain_number(command.y));

        case CanvasCommandKind::SetPenColor:
        case CanvasCommandKind::SetBackgroundColor:
        case CanvasCommandKind::FillLastShape:
            return name + QLatin1Char{' '} + colour_text(command.color);

        case CanvasCommandKind::SetPenStyle:
            return QStringLiteral("%1 %2 %3")
                .arg(name, QString::number(command.index), plain_number(command.x));

        case CanvasCommandKind::SetSvgaMode:
        case CanvasCommandKind::SetFramebuffer:
            return QStringLiteral("%1 %2\u00D7%3")
                .arg(name, plain_number(command.x), plain_number(command.y));

        case CanvasCommandKind::SetPaletteColor:
            return QStringLiteral("%1 %2 %3")
                .arg(name, QString::number(command.index), colour_text(command.color));

        case CanvasCommandKind::CyclePalette:
            return name + QLatin1Char{' '} + QString::number(command.index);

        case CanvasCommandKind::Circle:
            // The extent is a full circle unless the program asked for an arc.
            return command.y == 360.0
                       ? name + QLatin1Char{' '} + plain_number(command.x)
                       : QStringLiteral("%1 %2 %3")
                             .arg(name, plain_number(command.x), plain_number(command.y));

        case CanvasCommandKind::DrawPoint:
            return QStringLiteral("%1 %2 %3 %4")
                .arg(name, plain_number(command.x), plain_number(command.y),
                     colour_text(command.color));

        case CanvasCommandKind::DrawPolyline:
        case CanvasCommandKind::BezierCurve:
            return QStringLiteral("%1 %2 %3")
                .arg(name, point_count_text(view.point_count()), colour_text(command.color));

        case CanvasCommandKind::DrawRect:
        case CanvasCommandKind::DrawEllipse:
        case CanvasCommandKind::DrawRectGradient:
        case CanvasCommandKind::DrawEllipseGradient:
            return QStringLiteral("%1 %2 %3 %4 %5 %6")
                .arg(name, plain_number(command.x), plain_number(command.y),
                     plain_number(command.w), plain_number(command.h),
                     colour_text(command.color));

        case CanvasCommandKind::DrawArc:
            return QStringLiteral("%1 %2 %3 %4 %5 %6 %7")
                .arg(name, plain_number(command.x), plain_number(command.y),
                     plain_number(command.w), plain_number(command.h),
                     plain_number(command.a), plain_number(command.b));

        case CanvasCommandKind::DrawText:
            return QStringLiteral("%1 %2 at %3 %4 (size %5)")
                .arg(name, quoted_text(view.text), plain_number(command.x),
                     plain_number(command.y), plain_number(command.w));

        case CanvasCommandKind::PutPixels:
            return QStringLiteral("%1 %2 %3 %4 %5")
                .arg(name, plain_number(command.x), plain_number(command.y),
                     plain_number(command.w), plain_number(command.h));

        case CanvasCommandKind::DefineSprite:
            return QStringLiteral("%1 %2 %3 %4 %5 %6")
                .arg(name, quoted_text(view.text), plain_number(command.x),
                     plain_number(command.y), plain_number(command.w),
                     plain_number(command.h));

        case CanvasCommandKind::PlaceSprite:
        case CanvasCommandKind::MoveSprite:
            return QStringLiteral("%1 %2 %3 %4")
                .arg(name, quoted_text(view.text), plain_number(command.x),
                     plain_number(command.y));

        case CanvasCommandKind::ShowSprite:
        case CanvasCommandKind::HideSprite:
            return name + QLatin1Char{' '} + quoted_text(view.text);

        case CanvasCommandKind::None:
        case CanvasCommandKind::Home:
        case CanvasCommandKind::PenUp:
        case CanvasCommandKind::PenDown:
        case CanvasCommandKind::BeginFill:
        case CanvasCommandKind::EndFill:
        case CanvasCommandKind::ShowTurtle:
        case CanvasCommandKind::HideTurtle:
        case CanvasCommandKind::Clear:
        case CanvasCommandKind::Reset:
        case CanvasCommandKind::ClearFramebuffer:
            break;
    }
    return name;
}

}  // namespace timewarp::ui
