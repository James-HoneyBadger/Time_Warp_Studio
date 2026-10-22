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
#include "timewarp/ui/autosave.hpp"
#include "timewarp/ui/chip8_window.hpp"
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/command_palette.hpp"
#include "timewarp/ui/find_bar.hpp"
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

/// A page that asks for a lot of room, to prove size hints follow it.
class BulkyPage final : public QWidget {
public:
    explicit BulkyPage(QWidget* parent = nullptr) : QWidget(parent) {}
    [[nodiscard]] QSize sizeHint() const override { return {1400, 900}; }
    [[nodiscard]] QSize minimumSizeHint() const override { return {1400, 900}; }
};

/// A page with a valid but small hint, which still has to clear the floor.
class SmallPage final : public QWidget {
public:
    explicit SmallPage(QWidget* parent = nullptr) : QWidget(parent) {}
    [[nodiscard]] QSize sizeHint() const override { return {60, 40}; }
    [[nodiscard]] QSize minimumSizeHint() const override { return {60, 40}; }
};

[[nodiscard]] inline QString tabTitles(const QTabWidget* tabs) {
    QStringList titles;
    for (int index = 0; index < tabs->count(); ++index) {
        titles << tabs->tabText(index);
    }
    return titles.join(QLatin1Char('|'));
}

/// `QSplitter` has no stretch-factor getter -- only `setStretchFactor` -- and Qt
/// implements that by writing the child's size policy. Reading the policy back is
/// therefore the only way to assert a stretch factor directly; the independent
/// check is the resize-ratio test at the end of this file. If a future Qt stops
/// keeping the two in step, these will disagree, which is the signal to look.
[[nodiscard]] inline int horizontalStretch(const QSplitter* splitter, int index) {
    return splitter->widget(index)->sizePolicy().horizontalStretch();
}

[[nodiscard]] inline int verticalStretch(const QSplitter* splitter, int index) {
    return splitter->widget(index)->sizePolicy().verticalStretch();
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

/// A window that has restored its saved layout, in the order the header
/// documents: construct, resize, restore, *then* show.
///
/// The order matters. `restoreGeometry()` applied to an already-visible window
/// lets Qt re-fit it to the screen instead of honouring the stored size -- under
/// the offscreen platform a 1200x800 window came back as 900x774, which is its
/// minimum width. Restoring first avoids the whole question.
[[nodiscard]] inline std::unique_ptr<MainWindow> restoredWindow(int width = 1200,
                                                        int height = 800) {
    auto window = std::make_unique<MainWindow>();
    window->resize(width, height);
    window->restoreLayout();
    window->show();
    QApplication::processEvents();
    QApplication::processEvents();
    return window;
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

/// Renders the overlay into an image and counts the pixels it touched, which is
/// the only way to say "paints nothing" without looking at a screen.
[[nodiscard]] inline int overlayInk(CrtOverlay& overlay) {
    overlay.resize(80, 60);
    QImage image{80, 60, QImage::Format_ARGB32_Premultiplied};
    image.fill(Qt::transparent);
    QPainter painter{&image};
    overlay.render(&painter);
    painter.end();
    int inked = 0;
    for (int y = 0; y < image.height(); ++y) {
        for (int x = 0; x < image.width(); ++x) {
            if (image.pixelColor(x, y).alpha() > 0) {
                ++inked;
            }
        }
    }
    return inked;
}

}  // namespace tw::ui_layout
