// SPDX-License-Identifier: MIT

#include "main_window_internal.hpp"

#include "timewarp/core/qt_gui_compat.hpp"
#include "timewarp/ui/main_window.hpp"

#include "timewarp/lang/executor.hpp"
#include "timewarp/lang/watch.hpp"
#include "timewarp/ui/native_runner.hpp"

#include <QAction>
#include <QActionGroup>
#include <QApplication>
#include <QCloseEvent>
#include <QClipboard>
#include <QComboBox>
#include <QDateTime>
#include <QDesktopServices>
#include <QDir>
#include <QDirIterator>
#include <QDockWidget>
#include <QFile>
#include <QFileDialog>
#include <QFileInfo>
#include <QFont>
#include <QHBoxLayout>
#include <QInputDialog>
#include <QKeySequence>
#include <QLabel>
#include <QLineEdit>
#include <QMenu>
#include <QMenuBar>
#include <QMessageBox>
#include <QPageLayout>
#include <QPlainTextEdit>
#ifdef TIME_WARP_HAVE_PRINT
#include <QPrintDialog>
#include <QPrinter>
#endif
#include <QPushButton>
#include <QSaveFile>
#include <QSet>
#include <QSettings>
#include <QShortcut>
#include <QSignalBlocker>
#include <QSlider>
#include <QSplitter>
#include <QStatusBar>
#include <QTabWidget>
#include <QTimer>
#include <QTextDocument>
#include <QToolBar>
#include <QToolButton>
#include <QUrl>
#include <QVBoxLayout>

#include <algorithm>
#include <functional>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include "timewarp/canvas/command_queue.hpp"
#include "timewarp/canvas/host_adapter.hpp"
#include "timewarp/canvas/qt_bridge.hpp"
#include "timewarp/ui/app.hpp"
#include "timewarp/ui/app_paths.hpp"
#include "timewarp/ui/benchmark.hpp"
#include "timewarp/ui/breakpoints.hpp"
#include "timewarp/ui/chip8_window.hpp"
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/classroom_marker.hpp"
#include "timewarp/ui/command_palette.hpp"
#include "timewarp/ui/export.hpp"
#include "timewarp/ui/find_bar.hpp"
#include "timewarp/ui/find_in_files.hpp"
#include "timewarp/ui/formatter.hpp"
#include "timewarp/ui/help_browser.hpp"
#include "timewarp/ui/panels.hpp"
#include "timewarp/ui/preferences.hpp"
#include "timewarp/ui/profile_report.hpp"
#include "timewarp/ui/project_panel.hpp"
#include "timewarp/ui/retro_computer.hpp"
#include "timewarp/ui/repl_runner.hpp"
#include "timewarp/ui/sql_panel.hpp"
#include "timewarp/sql/database.hpp"
#include "timewarp/ui/resizable_tab_widget.hpp"
#include "timewarp/ui/scrubber.hpp"
#include "timewarp/ui/snippet_dialog.hpp"
#include "timewarp/ui/snippets.hpp"
#include "timewarp/ui/sound_player.hpp"
#include "timewarp/ui/tour.hpp"
#include "timewarp/ui/syntax_highlighter.hpp"
#include "timewarp/ui/terminal_widget.hpp"
#include "timewarp/ui/themes.hpp"
#include "timewarp/ui/turtle_canvas.hpp"
#include "timewarp/ui/version_history.hpp"
#include "timewarp/ui/ui_style.hpp"

// Supplied by CMake from the repository's VERSION file, so the window title and
// the packaged version cannot drift apart. The fallback keeps a standalone
// compile of this file working.
#ifndef TIME_WARP_VERSION
#define TIME_WARP_VERSION "0.0.0"
#endif

namespace timewarp::ui {

// The settings keys and the small helpers the sections share; they were an
// anonymous namespace in the one-file version, and a `using` is what keeps the call
// sites reading the same.
using namespace detail;

// ---------------------------------------------------------------------------
// SQL
// ---------------------------------------------------------------------------

namespace {

constexpr auto kSqlFileFilter =
    "SQLite databases (*.db *.sqlite *.sqlite3);;All files (*)";

}  // namespace

void MainWindow::buildSqlPanel() {
    if (sql_database_ == nullptr) {
        sql_database_ = std::make_shared<timewarp::sql::Database>();
    }
    sql_panel_ = new SqlPanel{right_tabs_};
    right_tabs_->addTab(sql_panel_, tr("\U0001F5C4 Database"));
    connect(sql_panel_, &SqlPanel::openDatabaseRequested, this,
            &MainWindow::openSqlDatabaseDialog);
    connect(sql_panel_, &SqlPanel::newDatabaseRequested, this,
            &MainWindow::newSqlDatabaseDialog);
    connect(sql_panel_, &SqlPanel::runRequested, this, &MainWindow::runSqlQuery);
    connect(sql_panel_, &SqlPanel::refreshRequested, this, &MainWindow::refreshSqlTables);
    connect(sql_panel_, &SqlPanel::schemaRequested, this, &MainWindow::showSqlSchema);
    connect(sql_panel_, &SqlPanel::recentDatabaseRequested, this, [this](const QString& path) {
        static_cast<void>(openSqlDatabase(path));
    });
    sql_panel_->setRecentDatabases(
        QSettings{}.value(QLatin1String(kSettingRecentDatabases)).toStringList());
}

void MainWindow::noteRecentDatabase(const QString& path) {
    if (path.isEmpty()) {
        return;
    }
    QSettings settings;
    QStringList paths =
        settings.value(QLatin1String(kSettingRecentDatabases)).toStringList();
    paths.removeAll(path);
    paths.prepend(path);
    while (paths.size() > kRecentDatabaseLimit) {
        paths.removeLast();
    }
    settings.setValue(QLatin1String(kSettingRecentDatabases), paths);
    settings.setValue(QLatin1String(kSettingSqlDatabase), path);
    if (sql_panel_ != nullptr) {
        sql_panel_->setRecentDatabases(paths);
    }
}

bool MainWindow::openSqlDatabase(const QString& path, bool silent) {
    if (path.trimmed().isEmpty()) {
        return false;
    }
    if (sql_database_ == nullptr) {
        sql_database_ = std::make_shared<timewarp::sql::Database>();
    }
    QString error;
    if (!sql_database_->open(path, &error)) {
        if (!silent) {
            reportError(tr("The database could not be opened: %1").arg(error));
            if (sql_panel_ != nullptr) {
                sql_panel_->setStatus(error, true);
            }
        }
        return false;
    }
    noteRecentDatabase(path);
    const QString displayed = path == QLatin1String(":memory:")
                                  ? tr("(in memory)")
                                  : QDir::toNativeSeparators(path);
    if (sql_panel_ != nullptr) {
        sql_panel_->setDatabase(displayed, sql_database_->tables());
    }
    if (repl_runner_ != nullptr) {
        const std::shared_ptr<timewarp::sql::Database> database = sql_database_;
        repl_runner_->setSqlCallback([database](std::string_view sql) {
            return database->executeForOutput(sql);
        });
    }
    setExecutionState(tr("ready"), tr("Database %1 open").arg(displayed));
    return true;
}

void MainWindow::openSqlDatabaseDialog() {
    const QString start =
        QSettings{}.value(QLatin1String(kSettingSqlDatabase)).toString();
    const QString path = QFileDialog::getOpenFileName(
        this, tr("Open SQLite Database"),
        start.isEmpty() ? QDir::homePath() : QFileInfo{start}.absolutePath(),
        QLatin1String(kSqlFileFilter));
    if (!path.isEmpty()) {
        static_cast<void>(openSqlDatabase(path));
    }
}

void MainWindow::newSqlDatabaseDialog() {
    const QString suggestion =
        QDir{QDir::homePath()}.filePath(QStringLiteral("studio.db"));
    QString path = QFileDialog::getSaveFileName(this, tr("New SQLite Database"),
                                                suggestion,
                                                QLatin1String(kSqlFileFilter));
    if (path.isEmpty()) {
        return;
    }
    if (QFileInfo{path}.suffix().isEmpty()) {
        path += QStringLiteral(".db");
    }
    if (QFileInfo::exists(path)) {
        const auto answer = QMessageBox::question(
            this, tr("Database already exists"),
            tr("%1 already exists. Open it instead?")
                .arg(QDir::toNativeSeparators(path)));
        if (answer == QMessageBox::Yes) {
            static_cast<void>(openSqlDatabase(path));
        }
        return;
    }
    static_cast<void>(openSqlDatabase(path));
}

void MainWindow::restoreSqlDatabase() {
    const QString path =
        QSettings{}.value(QLatin1String(kSettingSqlDatabase)).toString();
    if (path.isEmpty()) {
        return;
    }
    if (path != QLatin1String(":memory:") && !QFileInfo::exists(path)) {
        QSettings{}.remove(QLatin1String(kSettingSqlDatabase));
        return;
    }
    if (!openSqlDatabase(path, true)) {
        QSettings{}.remove(QLatin1String(kSettingSqlDatabase));
    }
}

void MainWindow::runSqlQuery(const QString& sql) {
    if (sql_database_ == nullptr || !sql_database_->isOpen()) {
        if (sql_panel_ != nullptr) {
            sql_panel_->setStatus(tr("Open or create a database first."), true);
        }
        return;
    }
    const timewarp::sql::QueryResult result = sql_database_->execute(sql);
    if (sql_panel_ != nullptr) {
        sql_panel_->setResult(result);
    }
    // A script may have created or dropped a table; the list is cheap to rebuild
    // and a stale one is a table the user can see but cannot open.
    refreshSqlTables();
    setExecutionState(result.ok() ? tr("ready") : tr("error"),
                      result.ok() ? tr("SQL: done") : tr("SQL: %1").arg(result.error));
}

void MainWindow::refreshSqlTables() {
    if (sql_panel_ == nullptr) {
        return;
    }
    sql_panel_->setTables(sql_database_ != nullptr && sql_database_->isOpen()
                              ? sql_database_->tables()
                              : QStringList{});
}

void MainWindow::showSqlSchema(const QString& table) {
    if (sql_database_ == nullptr || !sql_database_->isOpen() || sql_panel_ == nullptr) {
        return;
    }
    const QString schema = sql_database_->schema(table);
    sql_panel_->setQuery(schema + QLatin1Char('\n'));
    sql_panel_->focusQuery();
    sql_panel_->setStatus(schema.startsWith(QChar{0x274C})
                              ? QStringLiteral("\u274C Table not found")
                              : QStringLiteral("\u2139\uFE0F Schema shown in the editor"));
}

// ---------------------------------------------------------------------------
// Projects
// ---------------------------------------------------------------------------

QString MainWindow::activeProjectPath() const {
    return project_.isValid() ? project_.filePath() : QString{};
}

void MainWindow::refreshProjectPanel() {
    if (project_panel_ == nullptr) {
        return;
    }
    project_panel_->setProject(project_);
    if (project_dock_ != nullptr) {
        project_dock_->setWindowTitle(
            project_.isValid()
                ? tr("\U0001F5C2 %1").arg(project_.name())
                : tr("\U0001F5C2 Projects"));
    }
}

bool MainWindow::saveProject() {
    if (!project_.isValid()) {
        return false;
    }
    QString error;
    if (!project_.save(&error)) {
        reportError(tr("The project could not be saved: %1").arg(error));
        QMessageBox::warning(this, tr("Project not saved"), error);
        return false;
    }
    // The active path lives in settings only once the file it names exists.
    QSettings{}.setValue(QLatin1String(kSettingActiveProject), project_.filePath());
    return true;
}

void MainWindow::noteRecentProject(const QString& path) {
    if (path.isEmpty()) {
        return;
    }
    QSettings settings;
    QStringList paths = settings.value(QLatin1String(kSettingRecentProjects)).toStringList();
    paths.removeAll(path);
    paths.prepend(path);
    while (paths.size() > kRecentProjectLimit) {
        paths.removeLast();
    }
    settings.setValue(QLatin1String(kSettingRecentProjects), paths);
    settings.setValue(QLatin1String(kSettingActiveProject), path);
}

void MainWindow::newProjectDialog() {
    const QString start =
        project_.isValid() ? project_.root() : QDir::homePath();
    const QString folder = QFileDialog::getExistingDirectory(
        this, tr("Choose the folder for the new project"), start);
    if (folder.isEmpty()) {
        return;
    }
    bool accepted = false;
    const QString suggested =
        QFileInfo{folder}.fileName().isEmpty() ? tr("New Project")
                                              : QFileInfo{folder}.fileName();
    const QString name = QInputDialog::getText(
        this, tr("New Project"), tr("Project name:"), QLineEdit::Normal, suggested,
        &accepted);
    if (!accepted || name.trimmed().isEmpty()) {
        return;
    }
    timewarp::project::Project created =
        timewarp::project::Project::create(name.trimmed(), folder);
    if (QFileInfo::exists(created.filePath())) {
        const auto answer = QMessageBox::question(
            this, tr("Project already exists"),
            tr("%1 already exists. Open that project instead?")
                .arg(QDir::toNativeSeparators(created.filePath())));
        if (answer == QMessageBox::Yes) {
            static_cast<void>(openProject(created.filePath()));
        }
        return;
    }
    project_ = created;
    if (!saveProject()) {
        project_ = timewarp::project::Project{};
        refreshProjectPanel();
        return;
    }
    noteRecentProject(project_.filePath());
    refreshProjectPanel();
    setExecutionState(tr("ready"), tr("Project %1 created").arg(project_.name()));
}

void MainWindow::openProjectDialog() {
    const QString start = project_.isValid() ? project_.root() : QDir::homePath();
    const QString path = QFileDialog::getOpenFileName(
        this, tr("Open Project"), start, timewarp::project::openFileFilter());
    if (!path.isEmpty()) {
        static_cast<void>(openProject(path));
    }
}

bool MainWindow::openProject(const QString& path, bool silent) {
    timewarp::project::Project loaded;
    QString error;
    if (!loaded.load(path, &error)) {
        if (!silent) {
            reportError(tr("The project could not be opened: %1").arg(error));
            QMessageBox::warning(this, tr("Project not opened"), error);
        }
        return false;
    }
    project_ = loaded;
    noteRecentProject(project_.filePath());
    refreshProjectPanel();
    setExecutionState(tr("ready"), tr("Project %1 opened").arg(project_.name()));
    return true;
}

void MainWindow::closeProject() {
    if (!project_.isValid()) {
        return;
    }
    const QString name = project_.name();
    project_ = timewarp::project::Project{};
    QSettings settings;
    settings.remove(QLatin1String(kSettingActiveProject));
    refreshProjectPanel();
    setExecutionState(tr("ready"), tr("Project %1 closed").arg(name));
}

void MainWindow::restoreProject() {
    const QString path =
        QSettings{}.value(QLatin1String(kSettingActiveProject)).toString();
    if (path.isEmpty()) {
        return;
    }
    if (!QFileInfo::exists(path)) {
        // A project that has been moved or deleted is not a startup error. It is
        // also not silently reopened as an empty project: the recent menu still
        // names it, so the user can find it again.
        QSettings{}.remove(QLatin1String(kSettingActiveProject));
        return;
    }
    if (!openProject(path, true)) {
        QSettings{}.remove(QLatin1String(kSettingActiveProject));
    }
}

void MainWindow::addFilesToProject(const QString& group) {
    if (!project_.isValid()) {
        setExecutionState(tr("ready"), tr("Create or open a project first."));
        return;
    }
    QString start = project_.root();
    if (CodeEditor* editor = currentEditor(); editor != nullptr) {
        const QString path = pathOf(editor);
        if (!path.isEmpty()) {
            start = QFileInfo{path}.absolutePath();
        }
    }
    const QStringList paths = QFileDialog::getOpenFileNames(
        this, group.isEmpty() ? tr("Add programs to %1").arg(project_.name())
                              : tr("Add programs to %1").arg(group),
        start, openFileFilter());
    if (paths.isEmpty()) {
        return;
    }
    const int added = project_.addFiles(paths, group);
    if (added > 0 && saveProject()) {
        refreshProjectPanel();
        setExecutionState(tr("ready"),
                          added == 1 ? tr("Added 1 program to the project")
                                     : tr("Added %1 programs to the project").arg(added));
    }
}

void MainWindow::addFolderToProject() {
    if (!project_.isValid()) {
        setExecutionState(tr("ready"), tr("Create or open a project first."));
        return;
    }
    const QString folder = QFileDialog::getExistingDirectory(
        this, tr("Add every program under a folder"), project_.root());
    if (folder.isEmpty()) {
        return;
    }

    const QString base = QDir::cleanPath(folder);
    const QString folder_name = QFileInfo{base}.fileName();
    QStringList added;
    QDirIterator iterator{base, QDir::Files, QDirIterator::Subdirectories};
    while (iterator.hasNext()) {
        const QString path = iterator.next();
        if (languageForFile(path) == Language::Unknown) {
            continue;
        }
        const QString relative = QDir{base}.relativeFilePath(path);
        QString group = QFileInfo{relative}.path();
        if (group == QLatin1String(".")) {
            group.clear();
        }
        if (base != QDir::cleanPath(project_.root()) && !folder_name.isEmpty()) {
            group = group.isEmpty() ? folder_name : folder_name + QLatin1Char('/') + group;
        }
        if (project_.addFile(path, group)) {
            added.append(path);
        }
    }
    if (added.isEmpty()) {
        setExecutionState(tr("ready"), tr("No programs were found in that folder."));
        return;
    }
    if (saveProject()) {
        refreshProjectPanel();
        setExecutionState(tr("ready"),
                          added.size() == 1
                              ? tr("Added 1 program from the folder")
                              : tr("Added %1 programs from the folder").arg(added.size()));
    }
}

void MainWindow::addGroupToProject() {
    if (!project_.isValid()) {
        setExecutionState(tr("ready"), tr("Create or open a project first."));
        return;
    }
    bool accepted = false;
    const QString name = QInputDialog::getText(
        this, tr("New Group"), tr("Group name:"), QLineEdit::Normal, QString{}, &accepted);
    if (!accepted || !project_.addGroup(name)) {
        return;
    }
    if (saveProject()) {
        refreshProjectPanel();
    }
}

void MainWindow::addNewGroupForFile(const QString& path) {
    if (!project_.isValid()) {
        return;
    }
    bool accepted = false;
    const QString name = QInputDialog::getText(
        this, tr("New Group"), tr("Group name:"), QLineEdit::Normal, QString{}, &accepted);
    if (!accepted || !project_.addGroup(name)) {
        return;
    }
    project_.setGroup(path, name);
    if (saveProject()) {
        refreshProjectPanel();
        project_panel_->selectFile(path);
    }
}

void MainWindow::renameProjectGroup(const QString& group) {
    if (!project_.isValid()) {
        return;
    }
    bool accepted = false;
    const QString name = QInputDialog::getText(
        this, tr("Rename Group"), tr("New name:"), QLineEdit::Normal, group, &accepted);
    if (!accepted || !project_.renameGroup(group, name)) {
        return;
    }
    if (saveProject()) {
        refreshProjectPanel();
    }
}

void MainWindow::removeProjectGroup(const QString& group) {
    if (!project_.isValid()) {
        return;
    }
    const auto answer = QMessageBox::question(
        this, tr("Remove Group"),
        tr("Remove the group \"%1\"?\n\nIts programs stay in the project, ungrouped.")
            .arg(group));
    if (answer != QMessageBox::Yes || !project_.removeGroup(group)) {
        return;
    }
    if (saveProject()) {
        refreshProjectPanel();
    }
}

void MainWindow::moveProjectFile(const QString& path, const QString& group) {
    if (!project_.isValid()) {
        return;
    }
    if (!project_.setGroup(path, group)) {
        return;
    }
    if (saveProject()) {
        refreshProjectPanel();
        project_panel_->selectFile(path);
    }
}

void MainWindow::renameProject() {
    if (!project_.isValid()) {
        return;
    }
    bool accepted = false;
    const QString name = QInputDialog::getText(
        this, tr("Rename Project"), tr("Project name:"), QLineEdit::Normal,
        project_.name(), &accepted);
    if (!accepted || name.trimmed().isEmpty()) {
        return;
    }
    project_.setName(name);
    if (saveProject()) {
        refreshProjectPanel();
    }
}

void MainWindow::revealProjectFolder() {
    if (!project_.isValid()) {
        return;
    }
    QDesktopServices::openUrl(QUrl::fromLocalFile(project_.root()));
}

}  // namespace timewarp::ui
