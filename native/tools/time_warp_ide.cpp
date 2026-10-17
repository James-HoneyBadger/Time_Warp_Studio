// SPDX-License-Identifier: MIT
//
// The IDE's entry point.
//
// This is `native/tools/ui_gallery.cpp` grown up: the gallery existed to look at
// the translated layout, and its two jobs -- show the window, and save a picture
// of it -- are both still here, but it now takes files to open, and it is the
// program that ships rather than a harness beside it.
//
//   time_warp_ide [options] [file...]
//
// `--screenshot` runs headless in the sense that it renders and exits without
// waiting for anyone; it still needs a QPA platform, and offscreen is the right
// one for a build machine:
//
//   QT_QPA_PLATFORM=offscreen time_warp_ide --screenshot build/layout.png

#include <QApplication>
#include <algorithm>

#include <QCommandLineOption>
#include <QCommandLineParser>
#include <QPixmap>
#include <QSettings>
#include <QStringList>
#include <QTimer>

#include <cstdio>

#include "timewarp/ui/app.hpp"
#include "timewarp/ui/main_window.hpp"

namespace {

/// The size a window is forced to for `--screenshot` when none was asked for.
constexpr int kDefaultScreenshotWidth = 1400;
constexpr int kDefaultScreenshotHeight = 900;

/// Parses `WxH` into `size`. Returns false and explains itself on bad input,
/// because a typo in a screenshot size should say so rather than quietly produce
/// a differently sized picture than the one being compared.
bool parseSize(const QString& text, QSize* size) {
    const QStringList parts = text.split(QLatin1Char('x'), Qt::SkipEmptyParts);
    if (parts.size() != 2) {
        return false;
    }
    bool widthOk = false;
    bool heightOk = false;
    const int width = parts.at(0).toInt(&widthOk);
    const int height = parts.at(1).toInt(&heightOk);
    if (!widthOk || !heightOk || width <= 0 || height <= 0) {
        return false;
    }
    *size = QSize{width, height};
    return true;
}

/// Opens every positional argument, reporting the ones that would not open.
///
/// A failed file does not stop the application, and it does not raise a dialog
/// either: a mistyped name on the command line should leave the IDE open with the
/// other files loaded and the reason on stderr, rather than a modal box nobody is
/// there to answer. That is what makes `time_warp_ide --screenshot file.bad` a
/// script that fails rather than a process that hangs.
void openArguments(timewarp::ui::MainWindow* window, const QStringList& files) {
    QStringList failures;
    for (const QString& file : files) {
        if (!window->openFile(file, timewarp::ui::MainWindow::OpenFailure::Silent)) {
            failures << file;
        }
    }
    if (!failures.isEmpty()) {
        std::fprintf(stderr, "time_warp_ide: could not open %s\n",
                     qPrintable(failures.join(QStringLiteral(", "))));
    }
}

}  // namespace

int main(int argc, char* argv[]) {
    QApplication application{argc, argv};
    // Before anything constructs a QSettings, so every setting in the program --
    // layout, theme, recent files -- lands in one known place. See app.cpp for why
    // this is deliberately not the Python IDE's identity.
    timewarp::ui::applyApplicationIdentity();

    QCommandLineParser parser;
    parser.setApplicationDescription(
        QStringLiteral("Time Warp Studio \u2014 an educational IDE for BASIC, PILOT, "
                       "Logo, COMAL, REXX and Scheme."));
    parser.addHelpOption();
    parser.addVersionOption();

    const QCommandLineOption screenshotOption{
        QStringLiteral("screenshot"),
        QStringLiteral("Render the window to <path>, then exit."),
        QStringLiteral("path")};
    parser.addOption(screenshotOption);

    const QCommandLineOption sizeOption{
        QStringLiteral("size"),
        QStringLiteral("Screenshot size, as WxH. Default %1x%2.")
            .arg(kDefaultScreenshotWidth)
            .arg(kDefaultScreenshotHeight),
        QStringLiteral("WxH")};
    parser.addOption(sizeOption);

    const QCommandLineOption resetOption{
        QStringLiteral("reset"),
        QStringLiteral("Forget the saved window layout and start from the defaults.")};
    parser.addOption(resetOption);

    const QCommandLineOption panelOption{
        QStringLiteral("panel"),
        QStringLiteral("Bring this right-column panel to the front: output, "
                       "variables, telemetry, debug or lessons."),
        QStringLiteral("name")};
    parser.addOption(panelOption);

    const QCommandLineOption runOption{
        QStringLiteral("run"),
        QStringLiteral("Run the first file as soon as the window is up.")};
    parser.addOption(runOption);

    // Positional arguments are files to open. Named so `--help` documents them.
    parser.addPositionalArgument(QStringLiteral("file"),
                                 QStringLiteral("Programs to open at startup."),
                                 QStringLiteral("[file...]"));
    parser.process(application);

    if (parser.isSet(resetOption)) {
        // Clears this application's settings only; the default QSettings
        // constructor is scoped by the identity set above.
        QSettings{}.clear();
    }

    const QString screenshotPath = parser.value(screenshotOption);
    QSize screenshotSize{kDefaultScreenshotWidth, kDefaultScreenshotHeight};
    if (parser.isSet(sizeOption) && !parseSize(parser.value(sizeOption), &screenshotSize)) {
        std::fprintf(stderr, "time_warp_ide: --size expects WxH, got \"%s\"\n",
                     qPrintable(parser.value(sizeOption)));
        return 2;
    }

    timewarp::ui::MainWindow window;
    // Always before `show()`: restoring onto a visible window lets Qt re-fit the
    // geometry to the screen, so the saved position is lost on a display smaller
    // than the one it was saved on.
    window.restoreLayout();
    if (!screenshotPath.isEmpty()) {
        // A fixed size for the picture, because a screenshot that depends on the
        // machine's saved window geometry is not reproducible and so cannot be
        // compared against the previous one.
        window.resize(screenshotSize);
    }

    bool lessonsPanel = false;
    if (parser.isSet(panelOption)) {
        const QString panel = parser.value(panelOption);
        if (!window.showPanel(panel)) {
            std::fprintf(stderr,
                         "time_warp_ide: --panel expects output, variables, "
                         "telemetry, debug or lessons, got \"%s\"\n",
                         qPrintable(panel));
            return 2;
        }
        lessonsPanel = panel.trimmed().toLower() == QLatin1String("lessons");
    }

    const QStringList arguments = parser.positionalArguments();
    if (arguments.isEmpty() && screenshotPath.isEmpty()) {
        // Only when nothing was named on the command line: a file argument is a
        // request for that file, and a screenshot has to be reproducible from the
        // command line rather than from somebody's last session.
        window.restoreSession();
    }
    openArguments(&window, arguments);
    window.show();

    if (parser.isSet(runOption)) {
        // After `show()`, so the canvas has a real size to fit the drawing into:
        // a fit computed against a 0x0 widget would be recomputed on the first
        // resize anyway, but the run would also start before the window is
        // usable, which makes the interactive case feel broken.
        window.runCurrentDocument();
    }

    if (!screenshotPath.isEmpty()) {
        // After the event loop has drawn at least once, otherwise the grab catches
        // an unlaid-out window. A longer delay when a program is running, because
        // the drawing arrives from another thread: grabbing at 0ms would capture
        // a canvas that has been cleared but not yet drawn on.
        //
        // The lesson panel needs the longest wait of all: its content comes back
        // from a Python process that has to start first, so a grab at 0ms would
        // photograph an empty panel and look like a broken panel rather than a
        // fast shutter.
        int delay = 0;
        if (parser.isSet(runOption)) {
            delay = 1500;
        }
        if (lessonsPanel) {
            delay = std::max(delay, 6000);
        }
        QTimer::singleShot(delay, &application, [&window, screenshotPath] {
            const QPixmap shot = window.grab();
            if (!shot.save(screenshotPath)) {
                std::fprintf(stderr, "time_warp_ide: could not write %s\n",
                             qPrintable(screenshotPath));
                QCoreApplication::exit(3);
                return;
            }
            std::fprintf(stderr, "%s: %dx%d\n", qPrintable(screenshotPath), shot.width(),
                         shot.height());
            QCoreApplication::quit();
        });
    }

    return QApplication::exec();
}
