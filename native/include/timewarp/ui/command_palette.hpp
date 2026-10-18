// SPDX-License-Identifier: MIT
//
// The command palette: one keystroke to any action in the window.
//
// The Python implementation had one (`command_palette.py`, Ctrl+Shift+P, and
// F10 on the function-key bar). It is the single feature that makes a program
// with three menus and two toolbars feel small: a learner who does not know
// where "Export Drawing" lives types three letters of it.
//
// #### What it lists, and why that is the whole story
//
// Every `QAction` in the menu bar, read from the window at open time rather than
// registered by hand. A hand-maintained list is the thing that goes stale the
// first time somebody adds a menu entry, and the failure is silent: the action
// works, the palette just does not know it exists. Reading the menus means the
// palette cannot list something that is not there, cannot miss something that
// is, and honours `enabled` -- a greyed menu entry is not offered as a command.
//
// It shows the action's shortcut, and it filters on the text *and* the shortcut,
// so typing "F9" finds Run.

#pragma once

#include <QDialog>
#include <QList>
#include <QString>

class QAction;
class QEvent;
class QLineEdit;
class QListWidget;
class QListWidgetItem;
class QShowEvent;

namespace timewarp::ui {

/// A modal-on-top command chooser. Construct, `populate()` with the window's
/// actions, `exec()`; the chosen action is triggered before `exec()` returns.
class CommandPalette final : public QDialog {
    Q_OBJECT

public:
    explicit CommandPalette(QWidget* parent = nullptr);

    /// Rebuilds the list from `parent`'s action hierarchy: every menu's entries,
    /// including submenus, skipping separators, disabled entries and empty menus.
    void populate(QWidget* window);

    /// How many commands the list holds. For a test: a palette that opened onto
    /// nothing would still look right on screen.
    [[nodiscard]] int commandCount() const;

    /// The visible (filtered) rows, in display order.
    [[nodiscard]] QList<QString> visibleCommands() const;

    /// Applies `text` as the filter. Public so a test can filter without typing.
    void setFilter(const QString& text);

    /// Runs the row at `index` in the filtered list and closes. Returns false
    /// when there is nothing at that row.
    bool activate(int index);

    /// Runs the top row, which is what Return does.
    void activateFirst();

    /// The command list, so a test can assert the palette saw an action it did
    /// not know about before.
    [[nodiscard]] QListWidget* list() const noexcept { return list_; }

protected:
    /// Arrow keys and Return while the search field has focus.
    bool eventFilter(QObject* watched, QEvent* event) override;
    /// Re-opened: an empty search and the first row selected, because a palette
    /// that remembered the last filter would open onto a list of one.
    void showEvent(QShowEvent* event) override;

private:
    void rebuildVisible();

    QLineEdit* search_ = nullptr;
    QListWidget* list_ = nullptr;
    /// The actions the list represents, parallel to the list's items, so the
    /// row that is chosen is the action that is triggered.
    QList<QAction*> actions_;
};

}  // namespace timewarp::ui
