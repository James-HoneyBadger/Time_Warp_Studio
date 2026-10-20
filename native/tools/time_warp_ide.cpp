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
#include <QCoreApplication>
#include <QPixmap>
#include <QSettings>
#include <QStringList>
#include <QTimer>

#include <cstdio>

#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
#endif

#include "timewarp/ui/app.hpp"
#include "timewarp/ui/main_window.hpp"
#include "timewarp/ui/retro_computer.hpp"
#include "timewarp/ui/standalone.hpp"
#include "timewarp/ui/turtle_canvas.hpp"

namespace {

#ifdef _WIN32
/// Gives a GUI-subsystem process the console a prompt started it in.
///
/// The executable is linked as a Windows GUI application, so double-clicking it
/// does not leave a console window behind the IDE -- but the same build is what
/// `time_warp_ide --version`, `--help` and `--compile` are run as, and those
/// answer on stdout/stderr. A redirected stream is left alone, which is what
/// keeps `--version > version.txt` writing the file.
void attachParentConsole() {
    const HANDLE output = GetStdHandle(STD_OUTPUT_HANDLE);
    const DWORD type = output == nullptr ? FILE_TYPE_UNKNOWN : GetFileType(output);
    if (type == FILE_TYPE_DISK || type == FILE_TYPE_PIPE) {
        return;
    }
    if (!AttachConsole(ATTACH_PARENT_PROCESS)) {
        return;
    }
#ifdef _MSC_VER
    FILE* stream = nullptr;
    static_cast<void>(freopen_s(&stream, "CONOUT$", "w", stdout));
    static_cast<void>(freopen_s(&stream, "CONOUT$", "w", stderr));
#else
    // MinGW's C library has no freopen_s, but the plain call does the same job.
    static_cast<void>(freopen("CONOUT$", "w", stdout));
    static_cast<void>(freopen("CONOUT$", "w", stderr));
#endif
}
#endif

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
#ifdef _WIN32
    attachParentConsole();
#endif
    QApplication application{argc, argv};
    // Before anything constructs a QSettings, so every setting in the program --
    // layout, theme, recent files -- lands in one known place. See app.cpp for why
    // this is deliberately not the Python IDE's identity.
    timewarp::ui::applyApplicationIdentity();

    QCommandLineParser parser;
    parser.setApplicationDescription(
        QStringLiteral("Time Warp Studio \u2014 an old-school programming "
                       "environment for BASIC, APL, Logo, PILOT, FOCAL, Scheme, "
                       "CHIP-8, COMAL, REXX and Smalltalk, with the HB-1 home "
                       "computer and a live CHIP-8 inside it."));
    parser.addHelpOption();
    parser.addVersionOption();

    const QCommandLineOption screenshotOption{
        QStringLiteral("screenshot"),
        QStringLiteral("Render the window to <path>, then exit."),
        QStringLiteral("path")};
    parser.addOption(screenshotOption);

    // Kept in a variable rather than written into the literal below: a
    // preprocessor conditional cannot sit inside a macro's argument list.
#ifdef TIME_WARP_HAVE_SVG
    const QString exportFormats = QStringLiteral(".png, .jpg, .bmp, .svg");
#else
    const QString exportFormats = QStringLiteral(".png, .jpg, .bmp");
#endif
    const QCommandLineOption exportOption{
        QStringLiteral("export"),
        QStringLiteral("Write the drawing to <path> (%1), then exit. Most useful "
                       "with --run.")
            .arg(exportFormats),
        QStringLiteral("path")};
    parser.addOption(exportOption);

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
                       "variables, telemetry, debug, lessons, classroom or sql."),
        QStringLiteral("name")};
    parser.addOption(panelOption);

    const QCommandLineOption runOption{
        QStringLiteral("run"),
        QStringLiteral("Run the first file as soon as the window is up.")};
    parser.addOption(runOption);

    const QCommandLineOption computerOption{
        QStringLiteral("computer"),
        QStringLiteral("Open the HB-1 computer, booted into the first "
                       "file's language (or BASIC), and raise it. With --run, "
                       "run the computer's program.")};
    parser.addOption(computerOption);

    const QCommandLineOption compileOption{
        QStringLiteral("compile"),
        QStringLiteral("Write a standalone executable for <program> to the path "
                       "given with --output, and exit. The result runs the program "
                       "when double-clicked."),
        QStringLiteral("program")};
    parser.addOption(compileOption);

    const QCommandLineOption outputOption{
        QStringLiteral("output"),
        QStringLiteral("Where --compile writes its executable."),
        QStringLiteral("path")};
    parser.addOption(outputOption);

    const QCommandLineOption forceOption{
        QStringLiteral("force"),
        QStringLiteral("With --compile, replace the output file if it exists.")};
    parser.addOption(forceOption);

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

    // --compile is a build step, not a session: it writes the artifact and exits
    // before a window exists, so a packaging script does not have to create a GUI
    // and close it again.
    if (parser.isSet(compileOption)) {
        const QString program = parser.value(compileOption);
        const QString output = parser.value(outputOption);
        if (output.isEmpty()) {
            std::fprintf(stderr, "time_warp_ide: --compile needs --output <path>\n");
            return 2;
        }
        QString error;
        if (!timewarp::ui::standalone::compileFile(
                QCoreApplication::applicationFilePath(), program, output,
                parser.isSet(forceOption), &error)) {
            std::fprintf(stderr, "time_warp_ide: %s\n", qPrintable(error));
            return 5;
        }
        std::fprintf(stderr, "%s: compiled from %s\n", qPrintable(output),
                     qPrintable(program));
        return 0;
    }

    const QString screenshotPath = parser.value(screenshotOption);
    const QString exportPath = parser.value(exportOption);
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

    // A standalone artifact (`--compile`) carries its program in this
    // executable's tail. When there is one, this process *is* the program: no
    // session restore, no authoring UI, and it runs as soon as the window is up.
    const timewarp::ui::standalone::Embedded carried =
        timewarp::ui::standalone::embeddedIn(QCoreApplication::applicationFilePath());
    bool player = false;
    if (carried.present) {
        player = window.openStandaloneProgram(
            carried.language, carried.source,
            timewarp::ui::standalone::artifactName(
                QCoreApplication::applicationFilePath()));
        if (!player) {
            std::fprintf(stderr,
                         "time_warp_ide: this executable carries a program in an "
                         "unknown language (%s)\n",
                         qPrintable(carried.language));
            return 2;
        }
    }
    if (!screenshotPath.isEmpty()) {
        // A fixed size for the picture, because a screenshot that depends on the
        // machine's saved window geometry is not reproducible and so cannot be
        // compared against the previous one.
        window.resize(screenshotSize);
    }

    // The two panels whose content comes back from the worker process. A picture
    // of either is only worth taking once that answer has arrived.
    bool enginePanel = false;
    if (parser.isSet(panelOption)) {
        const QString panel = parser.value(panelOption);
        if (!window.showPanel(panel)) {
            std::fprintf(stderr,
                         "time_warp_ide: --panel expects output, variables, "
                         "telemetry, debug, lessons, classroom or sql, got \"%s\"\n",
                         qPrintable(panel));
            return 2;
        }
        const QString chosen = panel.trimmed().toLower();
        enginePanel = chosen == QLatin1String("lessons") || chosen == QLatin1String("classroom");
    }

    const QStringList arguments = parser.positionalArguments();
    if (arguments.isEmpty() && screenshotPath.isEmpty() && exportPath.isEmpty() &&
        !player) {
        // Only when nothing was named on the command line: a file argument is a
        // request for that file, and a screenshot has to be reproducible from the
        // command line rather than from somebody's last session.
        window.restoreSession();
    }
    openArguments(&window, arguments);
    window.show();

    if (parser.isSet(computerOption)) {
        window.openRetroComputer();
    }

    // A screenshot without --computer photographs the IDE. With it, the picture
    // is of the machine, which is what a script demonstrating the boot screen
    // wants; grabbing the parent would show the editor with a small window on
    // top of it.
    QWidget* screenshot_target = &window;
    timewarp::ui::RetroComputerWindow* computer_window = nullptr;
    if (parser.isSet(computerOption)) {
        computer_window = window.findChild<timewarp::ui::RetroComputerWindow*>();
        if (computer_window != nullptr) {
            if (!screenshotPath.isEmpty()) {
                computer_window->resize(screenshotSize);
            }
            screenshot_target = computer_window;
        }
    }

    // A first run only: the guide stores "seen" in QSettings and refuses a second
    // time. A file argument, a screenshot, an export, a named panel or a player
    // artifact is not somebody starting from scratch, so none of them show it.
    if (arguments.isEmpty() && screenshotPath.isEmpty() && exportPath.isEmpty() &&
        !player && !parser.isSet(panelOption)) {
        window.showGuidedTourIfUnseen();
    }

    const bool autorun = parser.isSet(runOption) || player;
    if (autorun) {
        // After `show()`, so the canvas has a real size to fit the drawing into:
        // a fit computed against a 0x0 widget would be recomputed on the first
        // resize anyway, but the run would also start before the window is
        // usable, which makes the interactive case feel broken.
        if (computer_window != nullptr) {
            computer_window->runProgram();
        } else {
            window.runCurrentDocument();
        }
    }

    if (!screenshotPath.isEmpty() || !exportPath.isEmpty()) {
        // After the event loop has drawn at least once, otherwise the grab catches
        // an unlaid-out window. A longer delay when a program is running, because
        // the drawing arrives from another thread: grabbing at 0ms would capture
        // a canvas that has been cleared but not yet drawn on.
        //
        // The lesson and classroom panels need the longest wait of all: their
        // content comes back from a Python process that has to start first, so a
        // grab at 0ms would photograph an empty panel and look like a broken
        // panel rather than a fast shutter.
        int delay = 0;
        if (autorun) {
            delay = 1500;
        }
        if (enginePanel) {
            delay = std::max(delay, 6000);
        }
        QTimer::singleShot(delay, &application,
                           [&window, screenshot_target, computer_window, screenshotPath,
                            exportPath] {
            if (!screenshotPath.isEmpty()) {
                const QPixmap shot = screenshot_target->grab();
                if (!shot.save(screenshotPath)) {
                    std::fprintf(stderr, "time_warp_ide: could not write %s\n",
                                 qPrintable(screenshotPath));
                    QCoreApplication::exit(3);
                    return;
                }
                std::fprintf(stderr, "%s: %dx%d\n", qPrintable(screenshotPath), shot.width(),
                             shot.height());
            }
            if (!exportPath.isEmpty()) {
                // The same call the File menu makes, so what a teacher renders from
                // a script is what a teacher would have saved by hand. With
                // --computer the program ran on the machine's canvas, so that is
                // the one holding the ink.
                QString error;
                timewarp::ui::TurtleCanvas* canvas = computer_window != nullptr
                                                         ? computer_window->graphicsCanvas()
                                                         : window.turtleCanvas();
                // `frameCount` counts shapes as well as strokes: a CHIP-8 screen
                // is all rectangles, and "nothing was drawn" about it would be a
                // lie.
                if (canvas == nullptr || canvas->frameCount() == 0) {
                    std::fprintf(stderr,
                                 "time_warp_ide: nothing was drawn, so there is nothing "
                                 "to export to %s\n",
                                 qPrintable(exportPath));
                    QCoreApplication::exit(4);
                    return;
                }
                if (!canvas->exportDrawing(exportPath, &error)) {
                    std::fprintf(stderr, "time_warp_ide: could not write %s: %s\n",
                                 qPrintable(exportPath), qPrintable(error));
                    QCoreApplication::exit(3);
                    return;
                }
                std::fprintf(stderr, "%s: %d primitives\n", qPrintable(exportPath),
                             canvas->frameCount());
            }
            QCoreApplication::quit();
        });
    }

    return QApplication::exec();
}
