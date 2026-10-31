// SPDX-License-Identifier: MIT
//
// The player: one program, and nothing you could author with.
//
// `time_warp_ide --compile` has always produced a copy of the IDE with the program
// appended, and the copy hides the authoring UI at start-up. That is a working
// player and a poor one to hand to somebody: it is a 4.7MB editor with eleven docks
// removed at run time, not a program that opens. This is the executable that opens
// one, and it links `timewarp::ui_core` -- the canvas, the runner, the sound and the
// palette -- rather than the shell.
//
// #### What it deliberately does not have
//
// No editor, no docks, no REPL bar, no SQL, no terminal, no session restore. A
// player with an editor in it is an IDE with the menus hidden, which is the thing
// this replaces. What it has is the program, the canvas it draws on, the output it
// prints, and the three keys anybody needs: R to run it again, F11 for the whole
// screen, Esc to leave.
//
// #### Where a program comes from
//
// Either appended to this executable (the artifact `--compile` writes), or named on
// the command line as source -- `.bas`, `.logo`, `.scm` -- which is what an author
// wants while iterating without a rebuild. `--screenshot` renders one frame and
// exits, so the layout can be checked without a display.

#include <cstdio>
#include <optional>

#include <QApplication>
#include <QCommandLineOption>
#include <QCommandLineParser>
#include <QFile>
#include <QFileInfo>
#include <QImage>
#include <QKeyEvent>
#include <QLabel>
#include <QPlainTextEdit>
#include <QSplitter>
#include <QElapsedTimer>
#include <QTextCursor>
#include <QTimer>
#include <QVBoxLayout>
#include <QWidget>

#include "timewarp/canvas/command_queue.hpp"
#include "timewarp/canvas/qt_bridge.hpp"
#include "timewarp/ui/language.hpp"
#include "timewarp/ui/native_runner.hpp"
#include "timewarp/ui/sound_player.hpp"
#include "timewarp/ui/standalone.hpp"
#include "timewarp/ui/themes.hpp"
#include "timewarp/ui/turtle_canvas.hpp"
#include "timewarp/ui/ui_style.hpp"

namespace {

using timewarp::ui::Language;
using timewarp::ui::TurtleCanvas;

/// What the window knows before the program has run: where it came from, what it
/// is called and who wrote it.
struct Program {
    QString language_key;
    QString source;
    QString title;
    QString author;
    int screen_width = 0;
    int screen_height = 0;

    [[nodiscard]] bool valid() const { return !language_key.isEmpty(); }
};

class PlayerWindow final : public QWidget {
public:
    explicit PlayerWindow(const Program& program) : program_{program} {
        setObjectName(QStringLiteral("playerWindow"));
        setWindowTitle(program_.title);
        setMinimumSize(320, 240);
        resize(program_.screen_width > 0 ? program_.screen_width * 2 : 800,
               program_.screen_height > 0 ? program_.screen_height * 2 : 600);

        // The default theme, applied the way the window applies one: palette
        // first, then the sheet. A player that ignored the theme would open a
        // black canvas with black ink whatever the author's palette was.
        const QString theme_name = timewarp::ui::themes::defaultName();
        const timewarp::ui::Theme& theme = timewarp::ui::themes::byName(theme_name);
        QApplication::setPalette(timewarp::ui::themes::paletteFor(theme));
        setStyleSheet(timewarp::ui::style::window() +
                      timewarp::ui::themes::stylesheetFor(theme));

        auto* layout = new QVBoxLayout{this};
        layout->setContentsMargins(0, 0, 0, 0);
        layout->setSpacing(0);

        // A title card, because a program that arrives as a file with no window
        // chrome should say what it is. It costs one row and it goes away when the
        // program draws.
        title_ = new QLabel{this};
        title_->setObjectName(QStringLiteral("playerTitle"));
        title_->setAlignment(Qt::AlignCenter);
        title_->setText(program_.author.isEmpty()
                            ? program_.title
                            : QStringLiteral("%1 — %2").arg(program_.title, program_.author));
        layout->addWidget(title_);

        splitter_ = new QSplitter{Qt::Vertical, this};
        splitter_->setObjectName(QStringLiteral("playerSplitter"));
        splitter_->setChildrenCollapsible(false);

        canvas_ = new TurtleCanvas{splitter_};
        canvas_->setObjectName(QStringLiteral("playerCanvas"));
        splitter_->addWidget(canvas_);

        output_ = new QPlainTextEdit{splitter_};
        output_->setObjectName(QStringLiteral("playerOutput"));
        output_->setReadOnly(true);
        output_->setPlaceholderText(tr("The program's output appears here."));
        output_->setMaximumBlockCount(10000);
        splitter_->addWidget(output_);
        splitter_->setSizes({600, 120});
        layout->addWidget(splitter_, 1);

        bridge_ = new timewarp::canvas::qt::CanvasBridge{queue_, this};
        bridge_->set_sink([this](timewarp::canvas::CanvasCommandView view) {
            canvas_->apply(view);
        });
        // A program that ends without a last flush would otherwise show a frame
        // behind: the safety-net drain is what makes the final picture complete.
        bridge_->set_poll_interval(std::chrono::milliseconds{16});

        sound_ = new timewarp::ui::SoundPlayer{this};

        language_ = timewarp::ui::nativeLanguage(
            timewarp::ui::languageFromKey(program_.language_key));
        if (language_.has_value()) {
            // Started once the window is up, so the first frame has somewhere to
            // land: a run that begins before `show()` draws into a canvas nobody
            // is looking at and the resize arrives mid-drawing.
            QTimer::singleShot(0, this, [this] { startRun(); });
        }
    }

    /// One frame, rendered offscreen. Used by `--screenshot` and by the test that
    /// checks a compiled program actually reaches its canvas.
    [[nodiscard]] QImage frame() {
        QApplication::processEvents();
        return canvas_->renderedDrawing(1.0);
    }

    [[nodiscard]] TurtleCanvas* turtleCanvas() const noexcept { return canvas_; }
    [[nodiscard]] QString outputText() const { return output_->toPlainText(); }
    [[nodiscard]] QString titleText() const { return title_->text(); }

    void setTelevision(bool on) {
        television_ = on;
        if (on) {
            showFullScreen();
        } else {
            showNormal();
        }
    }

    [[nodiscard]] bool finished() const noexcept { return finished_; }

protected:
    void keyPressEvent(QKeyEvent* event) override {
        switch (event->key()) {
            case Qt::Key_R:
                startRun();
                event->accept();
                return;
            case Qt::Key_F11:
                setTelevision(!television_);
                event->accept();
                return;
            case Qt::Key_Escape:
                if (television_) {
                    setTelevision(false);
                } else {
                    close();
                }
                event->accept();
                return;
            default:
                break;
        }
        QWidget::keyPressEvent(event);
    }

private:
    void startRun() {
        // The surface goes back to the state a program expects to start in: the
        // previous run's tail drained first, so a command still in the queue
        // cannot land on the new picture.
        bridge_->drainNow();
        canvas_->reset();
        output_->clear();
        finished_ = false;
        updateTitleCard();

        if (runner_ != nullptr) {
            runner_->request_stop();
            runner_->deleteLater();
        }
        runner_ = new timewarp::ui::NativeRunner{*language_, queue_, this};
        connect(runner_, &timewarp::ui::NativeRunner::outputAvailable, this,
                [this](const QString& text) { appendOutput(text); });
        connect(runner_, &timewarp::ui::NativeRunner::run_finished, this,
                [this](bool completed, const QString& error) {
                    finished_ = true;
                    if (!error.isEmpty()) {
                        appendOutput(error);
                    } else if (!completed) {
                        appendOutput(tr("— stopped —"));
                    }
                    sound_->stop();
                    bridge_->drainNow();
                    updateTitleCard();
                });
        // A player has no input dialog, and a program that asks is not broken:
        // it is a program written for a prompt. One empty answer keeps it moving
        // rather than leaving it waiting on a window that will never appear.
        connect(runner_, &timewarp::ui::NativeRunner::inputRequested, this,
                [this](const QString& prompt) {
                    appendOutput(prompt);
                    runner_->answerInput(QString{});
                });
        runner_->load_program(program_.source);
        runner_->start();
    }

    void appendOutput(const QString& text) {
        if (text.isEmpty()) {
            return;
        }
        output_->moveCursor(QTextCursor::End);
        output_->insertPlainText(text.endsWith(QLatin1Char('\n')) ? text : text + QLatin1Char('\n'));
        output_->moveCursor(QTextCursor::End);
    }

    void updateTitleCard() {
        // The card is a greeting, not a permanent banner: once the program has
        // drawn, its own window title carries the name and the row is wasted
        // space above the picture.
        const bool drew =
            canvas_->framebufferActive() || canvas_->strokeCount() > 0 ||
            canvas_->shapeCount() > 0;
        title_->setVisible(!drew);
    }

    Program program_;
    /// The engine's language, resolved once from the payload's key: the presenter
    /// names languages with the IDE's enum and the engines with their own.
    std::optional<timewarp::lang::Language> language_;
    timewarp::canvas::CanvasCommandQueue queue_;
    timewarp::canvas::qt::CanvasBridge* bridge_ = nullptr;
    TurtleCanvas* canvas_ = nullptr;
    QPlainTextEdit* output_ = nullptr;
    QSplitter* splitter_ = nullptr;
    QLabel* title_ = nullptr;
    timewarp::ui::SoundPlayer* sound_ = nullptr;
    timewarp::ui::NativeRunner* runner_ = nullptr;
    bool television_ = false;
    bool finished_ = false;
};

[[nodiscard]] bool parseSize(const QString& text, QSize* out) {
    const qsizetype cross = text.indexOf(QLatin1Char('x'));
    if (cross <= 0) {
        return false;
    }
    bool ok_width = false;
    bool ok_height = false;
    const int width = text.left(cross).toInt(&ok_width);
    const int height = text.mid(cross + 1).toInt(&ok_height);
    if (!ok_width || !ok_height || width <= 0 || height <= 0) {
        return false;
    }
    *out = QSize{width, height};
    return true;
}

}  // namespace

int main(int argc, char** argv) {
    QApplication app{argc, argv};
    QCoreApplication::setApplicationName(QStringLiteral("Time Warp Player"));
    QCoreApplication::setOrganizationName(QStringLiteral("Time Warp Studio"));
    QCoreApplication::setApplicationVersion(QString::fromLatin1(TIME_WARP_VERSION));

    QCommandLineParser parser;
    parser.setApplicationDescription(
        QStringLiteral("Runs one Time Warp Studio program and shows it. Opens an "
                       "artifact written by --compile, or the source file named "
                       "on the command line."));
    parser.addHelpOption();
    parser.addVersionOption();
    const QCommandLineOption screenshotOption{
        QStringLiteral("screenshot"),
        QStringLiteral("Render one frame to <path> and exit."),
        QStringLiteral("path")};
    parser.addOption(screenshotOption);
    const QCommandLineOption sizeOption{
        QStringLiteral("size"),
        QStringLiteral("The screenshot's size, WxH."),
        QStringLiteral("WxH")};
    parser.addOption(sizeOption);
    parser.addPositionalArgument(QStringLiteral("program"),
                                 QStringLiteral("A program to run. Without one, the "
                                                "program appended to this executable."));
    parser.process(app);

    // A source file on the command line wins over an appended payload: an author
    // iterating on a program runs `time_warp_player game.bas` in a loop, and the
    // player binary itself carries nothing.
    Program program;
    const QStringList positional = parser.positionalArguments();
    if (!positional.isEmpty()) {
        const QString path = positional.front();
        QFile file{path};
        if (!file.open(QIODevice::ReadOnly)) {
            std::fprintf(stderr, "time_warp_player: cannot read %s\n", qPrintable(path));
            return 2;
        }
        program.source = QString::fromUtf8(file.readAll());
        file.close();
        const Language language = timewarp::ui::languageForFile(path);
        if (language == Language::Unknown) {
            std::fprintf(stderr,
                         "time_warp_player: %s is not a program this build runs\n",
                         qPrintable(QFileInfo{path}.fileName()));
            return 2;
        }
        program.language_key = timewarp::ui::settingsKey(language);
        program.title = QFileInfo{path}.completeBaseName();
    } else {
        const timewarp::ui::standalone::Embedded carried =
            timewarp::ui::standalone::embeddedIn(QCoreApplication::applicationFilePath());
        if (!carried.present) {
            std::fprintf(stderr,
                         "time_warp_player: no program. Name one, or open an artifact "
                         "written by --compile.\n");
            return 2;
        }
        program.language_key = carried.language;
        program.source = carried.source;
        program.title = carried.metadata.title.isEmpty()
                            ? timewarp::ui::standalone::artifactName(
                                  QCoreApplication::applicationFilePath())
                            : carried.metadata.title;
        program.author = carried.metadata.author;
        program.screen_width = carried.metadata.screen_width;
        program.screen_height = carried.metadata.screen_height;
    }

    PlayerWindow window{program};
    if (timewarp::ui::languageFromKey(program.language_key) == Language::Unknown) {
        std::fprintf(stderr,
                     "time_warp_player: this program is in an unknown language (%s)\n",
                     qPrintable(program.language_key));
        return 2;
    }

    const QString screenshot = parser.value(screenshotOption);
    if (!screenshot.isEmpty()) {
        QSize size{800, 600};
        if (parser.isSet(sizeOption) && !parseSize(parser.value(sizeOption), &size)) {
            std::fprintf(stderr, "time_warp_player: --size expects WxH\n");
            return 2;
        }
        window.resize(size);
        window.show();
        // One frame after the run has had a chance to finish: a screenshot of a
        // player caught mid-program would not be reproducible, and this is the
        // picture a `--screenshot` in CI compares.
        QElapsedTimer timer;
        timer.start();
        while (!window.finished() && timer.elapsed() < 10000) {
            QApplication::processEvents();
        }
        QApplication::processEvents();
        const QImage image = window.grab().toImage();
        if (!image.save(screenshot)) {
            std::fprintf(stderr, "time_warp_player: cannot write %s\n",
                         qPrintable(screenshot));
            return 1;
        }
        return window.finished() ? 0 : 1;
    }

    window.show();
    return QApplication::exec();
}
