// SPDX-License-Identifier: MIT
//
// The player, tested as a program rather than as a widget.
//
// `time_warp_player` exists so a compiled program opens as a program and not as an
// editor with its menus hidden. What can go wrong with that is not reachable from a
// unit test of the window: the binary might not be shipped beside the IDE, the
// artifact might carry a payload the player cannot read, and -- the failure this
// project treats as *the* bug -- a program might run and draw nothing.
//
// So this suite spawns the real executable, headless, and looks at the frames it
// writes. `--screenshot` was built for exactly this: one frame, a fixed size, no
// window manager, and an exit code that says whether the program finished.

#include <cstdio>
#include <string>
#include <vector>

#include <QByteArray>
#include <QColor>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QImage>
#include <QProcess>
#include <QSet>
#include <QString>
#include <QStringList>
#include <QTemporaryDir>

#include "check.hpp"
#include "temp_dir.hpp"
#include "timewarp/ui/language.hpp"
#include "timewarp/ui/standalone.hpp"

namespace {

void announce(const char* name) {
    std::printf("-- %s\n", name);
    std::fflush(stdout);
}

/// The built player. CMake records its path because the suite has to *run* it.
QString playerBinary() { return QStringLiteral(TIME_WARP_TEST_PLAYER); }

struct Run {
    int exit_code = -1;
    QString output;
    QString errors;
};

Run runBinary(const QString& executable, const QStringList& arguments,
              int timeout_ms);

/// The checkout, for the example programs. The suite reads them wherever the build
/// tree is, the same way `lang_tests` reads the game it gates.
QString repositoryRoot() { return QStringLiteral(TIME_WARP_TEST_SOURCE_DIR); }

Run runPlayer(const QStringList& arguments, int timeout_ms = 60000) {
    return runBinary(playerBinary(), arguments, timeout_ms);
}

Run runBinary(const QString& executable, const QStringList& arguments,
              int timeout_ms = 60000) {
    Run result;
    QProcess process;
    QProcessEnvironment environment = QProcessEnvironment::systemEnvironment();
    // Offscreen, or a test that runs in CI with no display fails inside Qt before
    // it reaches the program being tested.
    environment.insert(QStringLiteral("QT_QPA_PLATFORM"), QStringLiteral("offscreen"));
    process.setProcessEnvironment(environment);
    process.start(executable, arguments);
    if (!process.waitForStarted(timeout_ms)) {
        result.errors = QStringLiteral("the player did not start");
        return result;
    }
    if (!process.waitForFinished(timeout_ms)) {
        process.kill();
        process.waitForFinished(2000);
        result.errors = QStringLiteral("the player did not finish");
        return result;
    }
    result.exit_code = process.exitCode();
    result.output = QString::fromUtf8(process.readAllStandardOutput());
    result.errors = QString::fromUtf8(process.readAllStandardError());
    return result;
}

/// How many distinct colours a picture has. One colour is a blank frame -- a
/// program that ran and drew nothing -- and this project treats that as a bug
/// rather than as a passing run.
int distinctColours(const QImage& image) {
    QSet<QRgb> seen;
    for (int y = 0; y < image.height(); y += 2) {
        for (int x = 0; x < image.width(); x += 2) {
            seen.insert(image.pixel(x, y));
            if (seen.size() > 8) {
                return seen.size();
            }
        }
    }
    return static_cast<int>(seen.size());
}

void test_the_player_draws_an_example() {
    announce("player draws an example");
    TW_CHECK(QFileInfo::exists(playerBinary()));
    QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    const QString shot = dir.filePath(QStringLiteral("patrol.png"));

    // `star_patrol.bas` is the game the screen-mode work gates on: it asks for a
    // SCREEN, places a sprite and reads a pixel back, so a frame of it either has
    // ink in it or the whole drawing path is broken.
    const QString program =
        QDir{repositoryRoot()}.filePath(QStringLiteral("Examples/basic/star_patrol.bas"));
    const Run run = runPlayer({QStringLiteral("--screenshot"), shot,
                               QStringLiteral("--size"), QStringLiteral("640x400"),
                               program});
    TW_CHECK_EQ(run.exit_code, 0);
    TW_CHECK(QFileInfo::exists(shot));

    const QImage image{shot};
    TW_CHECK(!image.isNull());
    // The size asked for is the size written: a screenshot that followed the
    // program's own window would not be comparable with the previous one.
    TW_CHECK_EQ(image.width(), 640);
    TW_CHECK_EQ(image.height(), 400);
    TW_CHECK(distinctColours(image) > 2);
}

void test_the_player_reports_a_program_that_does_not_run() {
    announce("player refuses a non-program");
    tw::test::TempDir dir;
    TW_CHECK(dir.valid());
    const QString notes = dir.write(QStringLiteral("notes.txt"), "hello");
    TW_CHECK(!notes.isEmpty());

    const Run run = runPlayer({notes});
    // 2 is "the arguments did not make a run", which is what a file that is not a
    // program is. The point is that it is *said*, with a code, rather than opening
    // a blank window.
    TW_CHECK_EQ(run.exit_code, 2);
    TW_CHECK(run.errors.contains(QStringLiteral("not a program")));
}

void test_a_compiled_artifact_carries_its_author() {
    announce("compiled artifact");
    QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    const QString source = dir.filePath(QStringLiteral("hello.bas"));
    {
        QFile file{source};
        TW_CHECK(file.open(QIODevice::WriteOnly));
        file.write("10 SCREEN 13\n20 FOR I = 1 TO 6\n30 LINE (10, 10)-(I * 20, 60), I, BF\n"
                   "40 NEXT I\n50 PRINT \"HELLO\"\n");
    }

    // Compiled onto the *player itself*, which is what `--compile` does when one
    // is installed beside the IDE: the artifact is a program, not a workshop.
    timewarp::ui::standalone::Metadata metadata;
    metadata.title = QStringLiteral("Six Bars");
    metadata.author = QStringLiteral("Ada");
    metadata.screen_width = 320;
    metadata.screen_height = 200;
    const QString artifact = dir.filePath(QStringLiteral("six_bars"));
    QString error;
    TW_CHECK(timewarp::ui::standalone::compileFile(playerBinary(), source, artifact, false,
                                                   metadata, &error));

    const timewarp::ui::standalone::Embedded carried =
        timewarp::ui::standalone::embeddedIn(artifact);
    TW_CHECK(carried.present);
    TW_CHECK_EQ(carried.format, 2);
    TW_CHECK_EQ(carried.metadata.title, QStringLiteral("Six Bars"));
    TW_CHECK_EQ(carried.metadata.author, QStringLiteral("Ada"));

    // And it runs: the artifact is the player with a program in it, so a
    // screenshot of it is a frame of *that program*.
    const QString shot = dir.filePath(QStringLiteral("six_bars.png"));
    // The artifact *is* the executable: it is the player with a program in it, so
    // this runs the thing a friend would double-click.
    const Run run = runBinary(artifact, {QStringLiteral("--screenshot"), shot});
    if (run.exit_code != 0) {
        std::printf("     player said: %s\n", qPrintable(run.errors.trimmed()));
    }
    TW_CHECK_EQ(run.exit_code, 0);
    const QImage image{shot};
    TW_CHECK(!image.isNull());
    TW_CHECK(distinctColours(image) > 2);
}

void test_the_player_reports_its_version() {
    announce("player version");
    const Run run = runPlayer({QStringLiteral("--version")});
    TW_CHECK_EQ(run.exit_code, 0);
    TW_CHECK(run.output.contains(QStringLiteral("Time Warp Player")));
}

}  // namespace

int main() {
    // The player writes and reads files beside itself, and the compiled artifact is
    // created in a temporary directory: a suite that needed a writable checkout
    // would fail on a read-only CI mount.
    qputenv("QT_QPA_PLATFORM", "offscreen");

    test_the_player_reports_its_version();
    test_the_player_draws_an_example();
    test_the_player_reports_a_program_that_does_not_run();
    test_a_compiled_artifact_carries_its_author();
    return tw::test::summary("player");
}
