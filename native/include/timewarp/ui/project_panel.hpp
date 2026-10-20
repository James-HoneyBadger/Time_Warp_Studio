// SPDX-License-Identifier: MIT
//
// The Projects panel: the project's programs, grouped the way the user grouped
// them.
//
// A view, like the outline: every edit is a signal and the window decides what it
// means. The panel never changes the model itself, so a group rename, a file move
// and a drag-and-drop between groups are one path through `project::Project` --
// and that path is saved once, not once per widget.
//
// Groups are containers in the tree and names on the files. They are *not*
// directories: moving a program between groups moves it in the project, never on
// disk, which is what makes a project safe to reorganise.

#pragma once

#include <QFrame>
#include <QString>
#include <QStringList>

#include "timewarp/project/project.hpp"

class QAction;
class QDropEvent;
class QLabel;
class QPoint;
class QTreeWidget;
class QTreeWidgetItem;

namespace timewarp::ui {

class ProjectPanel final : public QFrame {
    Q_OBJECT

public:
    explicit ProjectPanel(QWidget* parent = nullptr);

    /// Replaces what the panel shows. The panel keeps its own copy, so the tree
    /// cannot disagree with an open dialog about what was added.
    void setProject(const timewarp::project::Project& project);
    /// Shows the empty state: no project open, and the two ways to start one.
    void clearProject();

    [[nodiscard]] QString projectName() const;
    [[nodiscard]] int fileCount() const;
    [[nodiscard]] QStringList groupNames() const;
    [[nodiscard]] bool hasFile(const QString& path) const;
    /// The group `path` is shown under, or empty for the ungrouped section.
    [[nodiscard]] QString groupOf(const QString& path) const;
    /// Selects `path` in the tree if it is present. Used when the editor opens a
    /// file so the panel follows the user rather than the other way round.
    void selectFile(const QString& path);

    /// The tree, so a test can double-click an item. The panel is a view, and a
    /// view is exercised through its widget.
    [[nodiscard]] QTreeWidget* tree() const noexcept;

signals:
    void newProjectRequested();
    void openProjectRequested();
    void addFilesRequested(const QString& group);
    /// Add every recognised program under a folder, grouped by its subfolders.
    void addFolderRequested();
    void addGroupRequested();
    /// The user asked for a new group for one file: the window creates it and
    /// moves that file in, so "new group" from a file's menu is one edit.
    void fileToNewGroupRequested(const QString& path);
    void renameGroupRequested(const QString& group);
    void removeGroupRequested(const QString& group);
    void renameProjectRequested();
    void closeProjectRequested();
    void revealRequested();
    void removeSelectionRequested();
    /// A program was chosen for removal from the project.
    void removeFileRequested(const QString& path);
    /// A program in the project was double-clicked, or chosen from its menu.
    void fileActivated(const QString& path);
    /// A program was dropped on another group.
    void fileMovedToGroup(const QString& path, const QString& group);
    /// Files from outside the project were dropped on a group.
    void filesDropped(const QStringList& paths, const QString& group);

private:
    void buildUi();
    void rebuildTree();
    void showContextMenu(const QPoint& position);
    void handleDrop(QTreeWidgetItem* target, QDropEvent* event);
    [[nodiscard]] QTreeWidgetItem* itemForPath(const QString& path) const;
    [[nodiscard]] QString groupForItem(const QTreeWidgetItem* item) const;
    [[nodiscard]] QString fileForItem(const QTreeWidgetItem* item) const;
    void openItem(QTreeWidgetItem* item);

    timewarp::project::Project project_;
    QLabel* title_ = nullptr;
    QLabel* subtitle_ = nullptr;
    QLabel* empty_ = nullptr;
    QWidget* tools_ = nullptr;
    QTreeWidget* tree_ = nullptr;
};

}  // namespace timewarp::ui
