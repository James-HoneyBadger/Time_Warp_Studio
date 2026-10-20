// SPDX-License-Identifier: MIT

#include "timewarp/ui/project_panel.hpp"

#include <QAbstractItemView>
#include <QAction>
#include <QApplication>
#include <QDir>
#include <QDropEvent>
#include <QFileInfo>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonValue>
#include <QLabel>
#include <QPalette>
#include <QMenu>
#include <QMimeData>
#include <QPoint>
#include <QStyle>
#include <QToolButton>
#include <QTreeWidget>
#include <QUrl>
#include <QVBoxLayout>

#include <functional>
#include <utility>

namespace timewarp::ui {

namespace {

constexpr int kKindRole = Qt::UserRole;
constexpr int kPathRole = Qt::UserRole + 1;
constexpr int kGroupRole = Qt::UserRole + 2;

enum class ItemKind { Project, Group, File };

constexpr auto kFileMime = "application/x-timewarp-project-files";

/// A tree that reports a drop instead of applying it.
///
/// QTreeWidget's own internal move would delete and recreate the row, which
/// loses the model's roles and lets the tree disagree with the project the next
/// time it is refreshed. Reporting the move keeps the model the only owner of
/// "which group is this file in".
class ProjectTree final : public QTreeWidget {
public:
    using DropHandler = std::function<void(QTreeWidgetItem*, QDropEvent*)>;

    explicit ProjectTree(QWidget* parent) : QTreeWidget(parent) {}

    void setDropHandler(DropHandler handler) { handler_ = std::move(handler); }

protected:
    QMimeData* mimeData(const QList<QTreeWidgetItem*>& items) const override {
        QJsonArray paths;
        for (const QTreeWidgetItem* item : items) {
            if (item->data(0, kKindRole).toInt() != static_cast<int>(ItemKind::File)) {
                continue;
            }
            const QString path = item->data(0, kPathRole).toString();
            if (!path.isEmpty()) {
                paths.append(path);
            }
        }
        if (paths.isEmpty()) {
            return QTreeWidget::mimeData(items);
        }
        auto* payload = new QMimeData;
        payload->setData(QLatin1String{kFileMime},
                         QJsonDocument{paths}.toJson(QJsonDocument::Compact));
        // The plain-text form is what a text editor accepts, and it costs one
        // line. It is not how this widget reads a drop.
        QStringList names;
        for (const QJsonValue& value : paths) {
            names.append(value.toString());
        }
        payload->setText(names.join(QLatin1Char('\n')));
        return payload;
    }

    void dropEvent(QDropEvent* event) override {
        if (handler_) {
            handler_(itemAt(event->position().toPoint()), event);
        }
        if (!event->isAccepted()) {
            event->ignore();
        }
    }

    QStringList mimeTypes() const override {
        QStringList types = QTreeWidget::mimeTypes();
        types.append(QLatin1String{kFileMime});
        types.append(QStringLiteral("text/uri-list"));
        return types;
    }

private:
    DropHandler handler_;
};

QString normalised(const QString& path) {
    return QDir::cleanPath(QFileInfo{path}.absoluteFilePath());
}

}  // namespace

ProjectPanel::ProjectPanel(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("projectPanel"));
    setFrameShape(QFrame::StyledPanel);
    setMinimumWidth(170);
    buildUi();
    clearProject();
}

void ProjectPanel::buildUi() {
    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(6, 6, 6, 6);
    layout->setSpacing(4);

    title_ = new QLabel;
    title_->setObjectName(QStringLiteral("projectTitle"));
    QFont title_font = title_->font();
    title_font.setBold(true);
    title_->setFont(title_font);
    title_->setWordWrap(true);
    layout->addWidget(title_);

    subtitle_ = new QLabel;
    subtitle_->setObjectName(QStringLiteral("projectRoot"));
    subtitle_->setWordWrap(true);
    subtitle_->setEnabled(false);
    layout->addWidget(subtitle_);

    tools_ = new QWidget;
    tools_->setObjectName(QStringLiteral("projectTools"));
    auto* tools = new QHBoxLayout{tools_};
    tools->setContentsMargins(0, 0, 0, 0);
    tools->setSpacing(2);

    const auto tool = [this, tools](const QString& text, const QString& tip,
                                    const QString& name, auto&& slot) {
        auto* button = new QToolButton;
        button->setObjectName(name);
        button->setText(text);
        button->setToolTip(tip);
        button->setAutoRaise(true);
        button->setToolButtonStyle(Qt::ToolButtonTextOnly);
        connect(button, &QToolButton::clicked, this, std::forward<decltype(slot)>(slot));
        tools->addWidget(button);
        return button;
    };
    tool(QStringLiteral("+ Project"), tr("Create a project"), QStringLiteral("projectNew"),
         [this] { emit newProjectRequested(); });
    tool(QStringLiteral("Open"), tr("Open a project"), QStringLiteral("projectOpen"),
         [this] { emit openProjectRequested(); });
    tool(QStringLiteral("+ Files"), tr("Add programs to the project"),
         QStringLiteral("projectAddFiles"), [this] { emit addFilesRequested(QString{}); });
    tool(QStringLiteral("+ Folder"), tr("Add every program under a folder"),
         QStringLiteral("projectAddFolder"), [this] { emit addFolderRequested(); });
    tool(QStringLiteral("+ Group"), tr("Create a group"), QStringLiteral("projectAddGroup"),
         [this] { emit addGroupRequested(); });
    tools->addStretch(1);
    layout->addWidget(tools_);

    empty_ = new QLabel(
        tr("No project open.\n\nCreate one to collect the programs for a class, a "
           "lesson or a single piece of work, and arrange them into groups."));
    empty_->setObjectName(QStringLiteral("projectEmpty"));
    empty_->setWordWrap(true);
    empty_->setAlignment(Qt::AlignTop | Qt::AlignLeft);
    empty_->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
    // Stretch 1 with top alignment keeps the explanation under the title instead
    // of letting the layout distribute it down the empty dock.
    layout->addWidget(empty_, 1, Qt::AlignTop);

    auto* tree = new ProjectTree{this};
    tree_ = tree;
    tree_->setObjectName(QStringLiteral("projectTree"));
    tree_->setHeaderHidden(true);
    tree_->setColumnCount(1);
    tree_->setRootIsDecorated(true);
    tree_->setIndentation(14);
    tree_->setUniformRowHeights(true);
    tree_->setSelectionMode(QAbstractItemView::ExtendedSelection);
    tree_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    tree_->setContextMenuPolicy(Qt::CustomContextMenu);
    tree_->setDragEnabled(true);
    tree_->setAcceptDrops(true);
    tree_->setDropIndicatorShown(true);
    tree_->setDefaultDropAction(Qt::MoveAction);
    tree_->setDragDropMode(QAbstractItemView::DragDrop);
    tree->setDropHandler([this](QTreeWidgetItem* target, QDropEvent* event) {
        handleDrop(target, event);
    });
    connect(tree_, &QTreeWidget::itemDoubleClicked, this,
            [this](QTreeWidgetItem* item, int) { openItem(item); });
    connect(tree_, &QTreeWidget::customContextMenuRequested, this,
            [this](const QPoint& position) { showContextMenu(position); });
    layout->addWidget(tree_, 1);
}

void ProjectPanel::setProject(const timewarp::project::Project& project) {
    project_ = project;
    rebuildTree();
}

void ProjectPanel::clearProject() {
    project_ = timewarp::project::Project{};
    rebuildTree();
}

void ProjectPanel::rebuildTree() {
    tree_->clear();
    const bool open = project_.isValid();
    title_->setText(open ? project_.name() : QString{});
    // The dock's title already says "Projects"; repeating it in the empty state
    // was two headings and no information.
    title_->setVisible(open);
    subtitle_->setText(open ? QDir::toNativeSeparators(project_.root())
                            : tr("Project > New Project to begin"));
    subtitle_->setToolTip(open ? QDir::toNativeSeparators(project_.filePath()) : QString{});
    empty_->setVisible(!open);
    tree_->setVisible(open);
    // The toolbar stays: New and Open are exactly what the empty state needs, and
    // Add Files/Group refuse politely until a project exists.
    tools_->setVisible(true);
    if (!open) {
        return;
    }

    const QIcon folder_icon = style()->standardIcon(QStyle::SP_DirIcon);
    const QIcon file_icon = style()->standardIcon(QStyle::SP_FileIcon);
    const auto add_file_item = [this, file_icon](QTreeWidgetItem* parent,
                                                 const timewarp::project::ProjectFile& file) {
        const bool missing = !QFileInfo::exists(file.path);
        auto* item = new QTreeWidgetItem{parent};
        item->setData(0, kKindRole, static_cast<int>(ItemKind::File));
        item->setData(0, kPathRole, file.path);
        item->setData(0, kGroupRole, file.group);
        item->setText(0, (missing ? tr("\u26A0 ") : QString{}) + project_.displayPath(file.path));
        item->setToolTip(0, missing ? tr("Missing: %1").arg(QDir::toNativeSeparators(file.path))
                                    : QDir::toNativeSeparators(file.path));
        item->setIcon(0, file_icon);
        if (missing) {
            item->setForeground(0, palette().brush(QPalette::Disabled, QPalette::Text));
        }
    };

    const auto add_group_item = [this, folder_icon](const QString& group) {
        auto* item = new QTreeWidgetItem{tree_};
        item->setData(0, kKindRole, static_cast<int>(ItemKind::Group));
        item->setData(0, kGroupRole, group);
        item->setText(0, group);
        item->setIcon(0, folder_icon);
        item->setFlags(item->flags() & ~Qt::ItemIsDragEnabled);
        item->setExpanded(true);
        return item;
    };

    QStringList ungrouped;
    for (const timewarp::project::ProjectFile& file : project_.files()) {
        if (file.group.isEmpty()) {
            ungrouped.append(file.path);
        }
    }
    if (!ungrouped.isEmpty()) {
        QTreeWidgetItem* section = add_group_item(QString{});
        section->setText(0, tr("Ungrouped"));
        for (const timewarp::project::ProjectFile& file : project_.files()) {
            if (file.group.isEmpty()) {
                add_file_item(section, file);
            }
        }
    }

    for (const QString& group : project_.groups()) {
        QTreeWidgetItem* section = add_group_item(group);
        for (const timewarp::project::ProjectFile& file : project_.files()) {
            if (file.group == group) {
                add_file_item(section, file);
            }
        }
    }

    if (tree_->topLevelItemCount() == 0) {
        auto* hint = new QTreeWidgetItem{tree_};
        hint->setText(0, tr("No programs yet. Use + Files to add one."));
        hint->setFlags(Qt::NoItemFlags);
        hint->setForeground(0, palette().brush(QPalette::Disabled, QPalette::Text));
    }
}

QString ProjectPanel::projectName() const { return project_.name(); }

int ProjectPanel::fileCount() const { return project_.files().size(); }

QStringList ProjectPanel::groupNames() const { return project_.groups(); }

bool ProjectPanel::hasFile(const QString& path) const { return project_.contains(path); }

QString ProjectPanel::groupOf(const QString& path) const { return project_.groupOf(path); }

QTreeWidget* ProjectPanel::tree() const noexcept { return tree_; }

void ProjectPanel::selectFile(const QString& path) {
    if (QTreeWidgetItem* item = itemForPath(path); item != nullptr) {
        tree_->setCurrentItem(item);
        if (QTreeWidgetItem* parent = item->parent(); parent != nullptr) {
            parent->setExpanded(true);
        }
        tree_->scrollToItem(item);
    }
}

QTreeWidgetItem* ProjectPanel::itemForPath(const QString& path) const {
    const QString wanted = normalised(path);
    for (int top = 0; top < tree_->topLevelItemCount(); ++top) {
        QTreeWidgetItem* section = tree_->topLevelItem(top);
        for (int child = 0; child < section->childCount(); ++child) {
            QTreeWidgetItem* item = section->child(child);
            if (item->data(0, kKindRole).toInt() != static_cast<int>(ItemKind::File)) {
                continue;
            }
            if (normalised(item->data(0, kPathRole).toString()) == wanted) {
                return item;
            }
        }
    }
    return nullptr;
}

QString ProjectPanel::groupForItem(const QTreeWidgetItem* item) const {
    if (item == nullptr) {
        return {};
    }
    const auto kind = static_cast<ItemKind>(item->data(0, kKindRole).toInt());
    if (kind == ItemKind::Group) {
        return item->data(0, kGroupRole).toString();
    }
    if (kind == ItemKind::File) {
        return item->data(0, kGroupRole).toString();
    }
    return {};
}

QString ProjectPanel::fileForItem(const QTreeWidgetItem* item) const {
    if (item == nullptr ||
        static_cast<ItemKind>(item->data(0, kKindRole).toInt()) != ItemKind::File) {
        return {};
    }
    return item->data(0, kPathRole).toString();
}

void ProjectPanel::openItem(QTreeWidgetItem* item) {
    const QString path = fileForItem(item);
    if (!path.isEmpty()) {
        emit fileActivated(path);
    }
}

void ProjectPanel::showContextMenu(const QPoint& position) {
    if (!project_.isValid()) {
        QMenu menu{this};
        menu.addAction(tr("New Project..."), this, [this] { emit newProjectRequested(); });
        menu.addAction(tr("Open Project..."), this, [this] { emit openProjectRequested(); });
        menu.exec(tree_->viewport()->mapToGlobal(position));
        return;
    }

    QTreeWidgetItem* item = tree_->itemAt(position);
    QMenu menu{this};
    const QString file = fileForItem(item);
    const QString group = groupForItem(item);

    if (!file.isEmpty()) {
        menu.addAction(tr("Open"), this, [this, file] { emit fileActivated(file); });
        QMenu* move = menu.addMenu(tr("Move to Group"));
        const auto add_move = [this, move, file](const QString& text, const QString& target) {
            QAction* action = move->addAction(text);
            action->setCheckable(true);
            action->setChecked(project_.groupOf(file) == target);
            connect(action, &QAction::triggered, this, [this, file, target] {
                emit fileMovedToGroup(file, target);
            });
        };
        add_move(tr("Ungrouped"), QString{});
        for (const QString& existing : project_.groups()) {
            add_move(existing, existing);
        }
        move->addSeparator();
        move->addAction(tr("New Group..."), this,
                        [this, file] { emit fileToNewGroupRequested(file); });
        menu.addSeparator();
        menu.addAction(tr("Remove from Project"), this,
                       [this, file] { emit removeFileRequested(file); });
    } else if (!group.isEmpty() || (item != nullptr &&
                                    item->data(0, kKindRole).toInt() ==
                                        static_cast<int>(ItemKind::Group))) {
        menu.addAction(tr("Add Files..."), this,
                       [this, group] { emit addFilesRequested(group); });
        if (!group.isEmpty()) {
            menu.addAction(tr("Rename Group..."), this,
                           [this, group] { emit renameGroupRequested(group); });
            menu.addAction(tr("Remove Group"), this,
                           [this, group] { emit removeGroupRequested(group); });
        }
    } else {
        menu.addAction(tr("Add Files..."), this,
                       [this] { emit addFilesRequested(QString{}); });
        menu.addAction(tr("Add Folder..."), this, [this] { emit addFolderRequested(); });
        menu.addAction(tr("New Group..."), this, [this] { emit addGroupRequested(); });
        menu.addSeparator();
        menu.addAction(tr("Rename Project..."), this,
                       [this] { emit renameProjectRequested(); });
        menu.addAction(tr("Reveal Project Folder"), this, [this] { emit revealRequested(); });
        menu.addSeparator();
        menu.addAction(tr("Close Project"), this, [this] { emit closeProjectRequested(); });
    }
    menu.exec(tree_->viewport()->mapToGlobal(position));
}

void ProjectPanel::handleDrop(QTreeWidgetItem* target, QDropEvent* event) {
    const QString group = groupForItem(target);
    const QMimeData* mime = event->mimeData();
    if (mime == nullptr) {
        return;
    }

    if (mime->hasFormat(QLatin1String{kFileMime})) {
        const QJsonDocument document =
            QJsonDocument::fromJson(mime->data(QLatin1String{kFileMime}));
        if (!document.isArray()) {
            return;
        }
        bool moved = false;
        for (const QJsonValue& value : document.array()) {
            const QString path = value.toString();
            if (!path.isEmpty() && project_.groupOf(path) != group) {
                emit fileMovedToGroup(path, group);
                moved = true;
            }
        }
        if (moved) {
            event->setDropAction(Qt::MoveAction);
            event->accept();
        }
        return;
    }

    if (mime->hasUrls()) {
        QStringList paths;
        for (const QUrl& url : mime->urls()) {
            if (!url.isLocalFile()) {
                continue;
            }
            const QString path = url.toLocalFile();
            if (QFileInfo{path}.isFile()) {
                paths.append(path);
            }
        }
        if (!paths.isEmpty()) {
            emit filesDropped(paths, group);
            event->setDropAction(Qt::CopyAction);
            event->accept();
        }
    }
}

}  // namespace timewarp::ui
