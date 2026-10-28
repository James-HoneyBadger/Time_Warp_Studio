// SPDX-License-Identifier: MIT
//
// Structural tests for the ported main-window layout.
//
// A layout translation is easy to get "almost right" and impossible to notice:
// swap two children of a splitter, drop a `setStretchFactor`, forget
// `setChildrenCollapsible(false)`, and the window still looks correct until
// somebody resizes it and a pane they cannot recover. These tests assert the
// structure and the *sizing behaviour* rather than the pixels, so a regression
// fails here instead of in front of a user.
//
// Runs offscreen, so it needs no display and belongs in CI alongside the rest.
//
// The suite is one file's worth of cases split across `ui_layout/*.cpp`; this
// header is what they share: the widgets they name, the windows they build and
// the size assertions they read back.

#include <atomic>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <memory>
#include <string>
#include <vector>

#include <QAction>
#include <QElapsedTimer>
#include <QThread>
#include <QTimer>
#include <QApplication>
#include <QCheckBox>
#include <QComboBox>
#include <QDir>
#include <QDockWidget>
#include <QFile>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QProgressBar>
#include <QSet>
#include <QTextBrowser>
#include <QKeySequence>
#include <QMargins>
#include <QMenu>
#include <QMenuBar>
#include <QMetaMethod>
#include <QPainter>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QScrollBar>
#include <QSettings>
#include <QShortcut>
#include <QSlider>
#include <QSpinBox>
#include <QSplitter>
#include <QStatusBar>
#include <QTabBar>
#include <QTabWidget>
#include <QTableWidget>
#include <QTextCursor>
#include <QtEndian>
#include <QTemporaryDir>
#include <QToolBar>
#include <QToolButton>
#include <QTreeWidget>
#include <QVBoxLayout>
#include <QWidgetAction>

#include "timewarp/core/qt_gui_compat.hpp"
#include "check.hpp"
#include "temp_dir.hpp"
#include "timewarp/canvas/command.hpp"
#include "timewarp/canvas/command_queue.hpp"
#ifdef TIME_WARP_HAVE_AI
#include "timewarp/ui/ai_panel.hpp"
#include "timewarp/ui/ai_settings.hpp"
#endif
#include "timewarp/ui/autosave.hpp"
#include "timewarp/ui/chip8_window.hpp"
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/command_palette.hpp"
#include "timewarp/ui/command_reference.hpp"
#include "timewarp/ui/find_bar.hpp"
#include "timewarp/ui/find_in_files.hpp"
#include "timewarp/ui/main_window.hpp"
#include "timewarp/ui/native_runner.hpp"
#include "timewarp/ui/panels.hpp"
#include "timewarp/ui/preferences.hpp"
#include "timewarp/project/project.hpp"
#include "timewarp/sql/database.hpp"
#include "timewarp/ui/profile_report.hpp"
#include "timewarp/ui/project_panel.hpp"
#include "timewarp/ui/sql_panel.hpp"
#include "timewarp/ui/scrubber.hpp"
#include "timewarp/ui/sound_player.hpp"

#include "timewarp/ui/snippet_dialog.hpp"
#include "timewarp/ui/snippets.hpp"
#include "timewarp/ui/standalone.hpp"
#include "timewarp/ui/terminal_widget.hpp"
#include "timewarp/ui/themes.hpp"
#include "timewarp/ui/tour.hpp"
#include "timewarp/ui/turtle_canvas.hpp"
#include "timewarp/ui/version_history.hpp"
#include "timewarp/ui/resizable_tab_widget.hpp"

/// Pumps the event loop until `done()`, so a signal from a runner thread has
/// somewhere to arrive.
template <typename Predicate>
bool spin_until(Predicate done, int timeout_ms = 10000) {
    QElapsedTimer timer;
    timer.start();
    while (!done()) {
        QApplication::processEvents();
        if (timer.elapsed() > timeout_ms) {
            return false;
        }
        QThread::msleep(2);
    }
    return true;
}

namespace tw::ui_layout {

#ifdef TIME_WARP_HAVE_AI
using timewarp::ui::AiPanel;
using timewarp::ui::AiSettingsDialog;
#endif
using timewarp::ui::CommandReferencePanel;
using timewarp::ui::CrtOverlay;
using timewarp::ui::CodeEditor;
using timewarp::ui::FunctionKeyBar;
using timewarp::ui::ImmediateModePanel;
using timewarp::ui::DebugPanel;
using timewarp::ui::LessonPanel;
using timewarp::ui::ProjectPanel;
using timewarp::ui::SqlPanel;
using timewarp::ui::Language;
using timewarp::ui::MainWindow;
using timewarp::ui::ResizableTabWidget;
using timewarp::ui::TelemetryPanel;
using timewarp::ui::VariablesPanel;

inline void announce(const char* name) {
    std::printf("-- %s\n", name);
    std::fflush(stdout);
}

/// A *shown* window.
///
/// Splitter geometry only means anything after Qt has laid the window out: an
/// unshown QSplitter still holds degenerate sizes, and the `setSizes()` in the
/// constructor is only a hint until the first layout pass. Asserting 720/480 on a
/// window that has never been shown tests nothing but Qt's placeholder values.
[[nodiscard]] inline std::unique_ptr<MainWindow> shownWindow(int width = 1200,
                                                      int height = 800) {
    auto window = std::make_unique<MainWindow>();
    window->resize(width, height);
    window->show();
    QApplication::processEvents();
    QApplication::processEvents();
    return window;
}

/// The share of the splitter's extent taken by one child, so the assertion holds
/// at whatever size the window happens to be laid out at.
[[nodiscard]] inline double fraction(const QSplitter* splitter, int index) {
    const QList<int> sizes = splitter->sizes();
    if (sizes.size() < 2) {
        return 0.0;
    }
    const int total = sizes.at(0) + sizes.at(1);
    return total > 0 ? static_cast<double>(sizes.at(index)) / total : 0.0;
}

/// `text` without its keyboard mnemonics, so "Zoom &In" matches "Zoom In".
inline QString withoutMnemonic(const QString& text) {
    return QString{text}.remove(QLatin1Char('&'));
}

/// The menu bar entry whose text is `title` (mnemonics included).
inline QMenu* menuNamed(MainWindow& window, const QString& title) {
    for (QAction* action : window.menuBar()->actions()) {
        if (withoutMnemonic(action->text()) == withoutMnemonic(title)) {
            return action->menu();
        }
    }
    return nullptr;
}

/// The first entry in `menu` whose text contains `fragment`, ignoring mnemonics.
inline QAction* entryContaining(QMenu* menu, const QString& fragment) {
    if (menu == nullptr) {
        return nullptr;
    }
    for (QAction* action : menu->actions()) {
        if (withoutMnemonic(action->text()).contains(fragment, Qt::CaseInsensitive)) {
            return action;
        }
    }
    return nullptr;
}

}  // namespace tw::ui_layout
