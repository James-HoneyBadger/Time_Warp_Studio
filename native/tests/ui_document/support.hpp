// SPDX-License-Identifier: MIT
//
// Tests for the editor and the document lifecycle.
//
// These cover the parts of the IDE that touch the user's files, so the
// interesting cases are the ones where getting it wrong loses work: opening a
// file that is already open, a byte-order mark left in the first line, saving
// over a file that is being written, and the unsaved-changes prompt that decides
// whether a close destroys anything.
//
// Runs offscreen, so it needs no display and belongs in CI alongside the rest.
//
// #### What is deliberately not tested here
//
// Any path that ends in a modal dialog. `QFileDialog` and `QMessageBox` block
// until a person answers them, so a test that triggers one does not fail, it
// hangs -- and a hang in CI is worse than a missing assertion. The unreadable
// file and cancelled-save paths are therefore covered by hand and described in
// the README rather than asserted here. Everything else in the lifecycle is
// exercised through the same entry points the dialogs call.

#include <cmath>
#include <cstdio>
#include <functional>

#include <QAction>
#include <QApplication>
#include <QComboBox>
#include <QCoreApplication>
#include <QElapsedTimer>
#include <QFile>
#include <QFileInfo>
#include <QLabel>
#include <QPlainTextEdit>
#include <QSettings>
#include <QStatusBar>
#include <QTableWidget>
#include <QTabWidget>
#include <QTemporaryDir>
#include <QTextBlock>
#include <QTextCursor>
#include <QTextDocument>
#include <QThread>

#include "check.hpp"
#include "temp_dir.hpp"
#include "timewarp/canvas/command_queue.hpp"
#include "timewarp/ui/app_paths.hpp"
#include "timewarp/ui/autosave.hpp"
#include "timewarp/ui/breakpoints.hpp"
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/find_bar.hpp"
#include "timewarp/ui/language.hpp"
#include "timewarp/ui/main_window.hpp"
#include "timewarp/ui/native_runner.hpp"
#include "timewarp/ui/panels.hpp"
#include "timewarp/ui/resizable_tab_widget.hpp"
#include "timewarp/ui/snippets.hpp"
#include "timewarp/ui/terminal_widget.hpp"
#include "timewarp/ui/themes.hpp"
#include "timewarp/ui/turtle_canvas.hpp"

namespace tw::ui_document {

using timewarp::ui::CodeEditor;
using timewarp::ui::FindBar;
using timewarp::ui::Language;
using timewarp::ui::MainWindow;
using timewarp::ui::TelemetryPanel;
using timewarp::ui::TurtleCanvas;

inline void announce(const char* name) {
    std::printf("-- %s\n", name);
    std::fflush(stdout);
}

/// Spins the event loop until `done` holds, or gives up.
///
/// Needed because a run happens on another thread and reports back through
/// queued signals: without pumping, a test either races the runner or deadlocks
/// waiting for a slot that can only run when it yields.
[[nodiscard]] inline bool waitFor(const std::function<bool()>& done, int timeout_ms = 15000) {
    QElapsedTimer timer;
    timer.start();
    while (!done() && timer.elapsed() < timeout_ms) {
        QApplication::processEvents(QEventLoop::AllEvents, 10);
        QThread::msleep(1);
    }
    return done();
}

/// The editor in the window's current tab.
[[nodiscard]] inline CodeEditor* currentEditorOf(const MainWindow& window) {
    return qobject_cast<CodeEditor*>(window.editorTabs()->currentWidget());
}

/// The text of a status-bar badge, by object name.
[[nodiscard]] inline QString badgeText(const MainWindow& window, const char* name) {
    const auto* label = window.statusBar()->findChild<QLabel*>(QString::fromLatin1(name));
    return label == nullptr ? QString{} : label->text();
}

/// Writes `contents` to `dir/name` and returns the full path.
[[nodiscard]] inline QString writeFile(const QTemporaryDir& dir, const QString& name,
                                const QByteArray& contents) {
    const QString path = dir.filePath(name);
    QFile file{path};
    if (!file.open(QIODevice::WriteOnly)) {
        return {};
    }
    file.write(contents);
    file.close();
    return path;
}

/// The console and the error panel, by object name.
[[nodiscard]] inline QPlainTextEdit* viewOf(const MainWindow& window, const char* name) {
    return window.findChild<QPlainTextEdit*>(QString::fromLatin1(name));
}

}  // namespace tw::ui_document
