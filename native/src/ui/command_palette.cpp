// SPDX-License-Identifier: MIT

#include "timewarp/ui/command_palette.hpp"

#include <QAction>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QMainWindow>
#include <QMenu>
#include <QMenuBar>
#include <QShowEvent>
#include <QVBoxLayout>

namespace timewarp::ui {

namespace {

/// Collects the leaf actions under `menu` in display order.
void collect(QMenu* menu, QList<QAction*>* out) {
    for (QAction* action : menu->actions()) {
        if (action->isSeparator() || !action->isEnabled()) {
            continue;
        }
        if (QMenu* submenu = action->menu()) {
            collect(submenu, out);
        } else {
            out->append(action);
        }
    }
}

/// The row's text: what the menu says, without the mnemonic, and the shortcut
/// beside it. The mnemonic ampersand is stripped because "&Save" would filter as
/// "save" only by luck -- "&Save As" would need an ampersand typed to match.
QString rowText(const QAction* action) {
    QString text = action->text();
    text.remove(QLatin1Char('&'));
    // A toolbar entry carries an emoji prefix; the palette is text, so it is
    // shown but the filter ignores it (see `matches`).
    const QString shortcut = action->shortcut().toString(QKeySequence::NativeText);
    if (shortcut.isEmpty()) {
        return text;
    }
    return QStringLiteral("%1    (%2)").arg(text, shortcut);
}

/// Case-insensitive match on the command's words and its shortcut, ignoring the
/// emoji a toolbar entry may start with.
bool matches(const QString& haystack, const QString& needle) {
    if (needle.isEmpty()) {
        return true;
    }
    return haystack.contains(needle, Qt::CaseInsensitive);
}

}  // namespace

CommandPalette::CommandPalette(QWidget* parent) : QDialog(parent) {
    setObjectName(QStringLiteral("commandPalette"));
    setWindowTitle(tr("Command Palette"));
    setModal(true);
    resize(520, 380);

    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(10, 10, 10, 10);
    layout->setSpacing(6);

    search_ = new QLineEdit{this};
    search_->setObjectName(QStringLiteral("commandPaletteSearch"));
    search_->setPlaceholderText(tr("Type a command \u2026  (\u2191\u2193 to choose, "
                                   "Return to run)"));
    search_->setClearButtonEnabled(true);
    layout->addWidget(search_);

    list_ = new QListWidget{this};
    list_->setObjectName(QStringLiteral("commandPaletteList"));
    list_->setAlternatingRowColors(true);
    layout->addWidget(list_, 1);

    auto* hint = new QLabel{tr("Esc closes. The list is the menu bar, filtered."), this};
    hint->setObjectName(QStringLiteral("commandPaletteHint"));
    layout->addWidget(hint);

    connect(search_, &QLineEdit::textChanged, this,
            [this](const QString& text) { setFilter(text); });
    connect(list_, &QListWidget::itemActivated, this, [this](QListWidgetItem* item) {
        activate(list_->row(item));
    });
    // Enter runs the highlighted row, and the arrow keys move between rows while
    // focus stays in the search field: a palette that makes you Tab into the list
    // is a palette nobody uses twice.
    search_->installEventFilter(this);
    list_->installEventFilter(this);
    setFocusProxy(search_);
}

void CommandPalette::populate(QWidget* window) {
    actions_.clear();
    QList<QAction*> found;
    if (auto* main = qobject_cast<QMainWindow*>(window)) {
        for (QAction* action : main->menuBar()->actions()) {
            if (QMenu* menu = action->menu()) {
                collect(menu, &found);
            }
        }
    }
    // Two menus can hold the same action (the toolbar's Save is the File menu's
    // Save); it should appear once.
    for (QAction* action : found) {
        if (!actions_.contains(action)) {
            actions_.append(action);
        }
    }
    setFilter(search_->text());
}

void CommandPalette::setFilter(const QString& text) {
    list_->clear();
    const QString needle = text.trimmed();
    for (QAction* action : actions_) {
        const QString row = rowText(action);
        if (!matches(row, needle)) {
            continue;
        }
        auto* item = new QListWidgetItem{row, list_};
        item->setData(Qt::UserRole, reinterpret_cast<quintptr>(action));
    }
    if (list_->count() > 0) {
        list_->setCurrentRow(0);
    }
}

int CommandPalette::commandCount() const { return list_->count(); }

QList<QString> CommandPalette::visibleCommands() const {
    QList<QString> rows;
    rows.reserve(list_->count());
    for (int index = 0; index < list_->count(); ++index) {
        rows.append(list_->item(index)->text());
    }
    return rows;
}

bool CommandPalette::activate(int index) {
    if (index < 0 || index >= list_->count()) {
        return false;
    }
    auto* action = reinterpret_cast<QAction*>(
        list_->item(index)->data(Qt::UserRole).value<quintptr>());
    if (action == nullptr) {
        return false;
    }
    // Closed before the action runs: an action may open a modal dialog (Export
    // asks for a path), and a modal opened from inside a modal leaves the
    // palette on screen underneath it.
    accept();
    action->trigger();
    return true;
}

void CommandPalette::activateFirst() {
    if (list_->count() == 0) {
        return;
    }
    activate(list_->currentRow() >= 0 ? list_->currentRow() : 0);
}

void CommandPalette::showEvent(QShowEvent* event) {
    QDialog::showEvent(event);
    search_->clear();
    search_->setFocus(Qt::PopupFocusReason);
    if (list_->count() > 0) {
        list_->setCurrentRow(0);
    }
}

bool CommandPalette::eventFilter(QObject* watched, QEvent* event) {
    if (event->type() != QEvent::KeyPress) {
        return QDialog::eventFilter(watched, event);
    }
    auto* key = static_cast<QKeyEvent*>(event);
    switch (key->key()) {
        case Qt::Key_Return:
        case Qt::Key_Enter:
            activateFirst();
            return true;
        case Qt::Key_Down:
            if (list_->currentRow() + 1 < list_->count()) {
                list_->setCurrentRow(list_->currentRow() + 1);
            }
            return true;
        case Qt::Key_Up:
            if (list_->currentRow() > 0) {
                list_->setCurrentRow(list_->currentRow() - 1);
            }
            return true;
        default:
            break;
    }
    return QDialog::eventFilter(watched, event);
}

}  // namespace timewarp::ui
