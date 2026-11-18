// SPDX-License-Identifier: MIT
//
// The HB-1's own example programs, run on the HB-1.
//
// `Examples/hb1` holds the programs that are about the machine rather than about a
// language: the tube as memory, the phosphor ramp, the sound board, the address
// space, the canvas and a Logo garden. They are shipped, so they are a promise, and
// a promise that runs and shows nothing is the failure this project treats as the
// bug. So each one is loaded into a real machine, run, and required to leave
// something behind -- ink in the tube's memory or shapes on the canvas -- with no
// error in the console.
//
// The smoke gate runs these too, as programs, but that is the engine without a
// machine attached: `POKE 32768` lands in a sparse map nothing paints. Only this
// case can say the program did something a person would see.

#include "support.hpp"

#include "timewarp/ui/language.hpp"
#include "timewarp/ui/turtle_canvas.hpp"

namespace tw::retro {

void test_the_hb1_examples_run_on_the_machine() {
    const QString root = QStringLiteral(TIME_WARP_TEST_SOURCE_DIR) +
                         QStringLiteral("/Examples/hb1");
    const QDir directory{root};
    TW_CHECK(directory.exists());
    const QStringList files =
        directory.entryList(QStringList{QStringLiteral("*.bas"), QStringLiteral("*.logo")},
                            QDir::Files, QDir::Name);
    TW_CHECK(files.size() >= 8);

    for (const QString& name : files) {
        std::printf("-- example %s\n", qPrintable(name));
        clearMachineSettings();
        RetroComputerWindow machine;
        const QString path = directory.filePath(name);
        const timewarp::ui::Language language = timewarp::ui::languageForFile(path);
        TW_CHECK(language != timewarp::ui::Language::Unknown);
        machine.setLanguage(language);

        QFile file{path};
        TW_CHECK(file.open(QIODevice::ReadOnly));
        machine.loadProgram(QString::fromUtf8(file.readAll()));
        machine.runProgram();
        TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 20000));

        const QString printed = machine.consoleView()->toPlainText();
        // An error is a failure even if the program also drew: a demonstration
        // that stops half way teaches half a lesson.
        TW_CHECK(!printed.contains(QString::fromUtf8("\u274C")));
        TW_CHECK(!printed.isEmpty());

        const bool wrote_the_tube =
            machine.memory().screenText().trimmed() != QStringLiteral("");
        const bool drew_on_the_canvas =
            machine.graphicsCanvas() != nullptr && machine.graphicsCanvas()->frameCount() > 0;
        // Every one of these programs is about a surface, and every one uses at
        // least one of them. A run that changed neither has done nothing at all.
        TW_CHECK(wrote_the_tube || drew_on_the_canvas);
    }
}

}  // namespace tw::retro
