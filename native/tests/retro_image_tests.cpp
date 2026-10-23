// SPDX-License-Identifier: MIT
//
// Machine images: a round trip through a file, the refusals, the machine words that
// save and load one, and the disk's own refusals.

#include "retro/support.hpp"

namespace tw::retro {

void test_hb_dos_a_file_belongs_to_its_rom() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.setLanguage(Language::Logo);
    machine.loadProgram(QStringLiteral("FORWARD 100"));
    machine.submitCommand(QStringLiteral("SAVE \"SQUARE\""));

    machine.setLanguage(Language::Basic);
    machine.submitCommand(QStringLiteral("CATALOG"));
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("LOGO")));

    // LOAD cold-boots the ROM that can run the file.
    machine.submitCommand(QStringLiteral("LOAD \"SQUARE\""));
    TW_CHECK_EQ(machine.language(), Language::Logo);
    TW_CHECK_EQ(machine.program(), QStringLiteral("FORWARD 100"));
}

// A machine is a file: the ROM, the screen, every ROM's program, the phosphor and
// the disk, in one document. The round trip is what proves the image is the machine
// and not a summary of it.

void test_a_machine_survives_a_round_trip_through_a_file() {
    clearMachineSettings();
    const QString path = QDir::tempPath() + QStringLiteral("/timewarp-retro-tests/HV.hb1");
    {
        RetroComputerWindow machine;
        machine.setLanguage(Language::Logo);
        machine.loadProgram(QStringLiteral("FORWARD 100"));
        machine.submitCommand(QStringLiteral("SAVE \"SQUARE\""));
        machine.setLanguage(Language::Basic);
        machine.loadProgram(QStringLiteral("20 PRINT \"HELLO\""));
        machine.setScreenMode(RetroComputerWindow::ScreenMode::Graphics);
        RetroComputerSettings config = machine.settings();
        config.tint = PhosphorTint::Amber;
        config.screen_text_size = 16;
        config.boot_language = static_cast<int>(Language::Pilot);
        machine.setSettings(config);

        QString error;
        TW_CHECK(machine.saveMachine(path, &error));
        TW_CHECK(error.isEmpty());
        TW_CHECK(QFileInfo::exists(path));
    }
    {
        RetroComputerWindow machine;
        QString error;
        TW_CHECK(machine.loadMachine(path, &error));
        TW_CHECK(error.isEmpty());
        // The ROM and the screen it was left in. `mode` is the machine's own enum
        // order -- 0 TEXT, 1 GRAPHICS, 2 SPLIT -- which the first version of the
        // manual had backwards, so it is pinned here rather than remembered.
        TW_CHECK_EQ(machine.language(), Language::Basic);
        TW_CHECK(machine.screenMode() == RetroComputerWindow::ScreenMode::Graphics);
        // The programs, including the one belonging to the ROM it is *not* in.
        TW_CHECK_EQ(machine.program(), QStringLiteral("20 PRINT \"HELLO\""));
        machine.setLanguage(Language::Logo);
        TW_CHECK_EQ(machine.program(), QStringLiteral("FORWARD 100"));
        TW_CHECK_EQ(machine.language(), Language::Logo);
        // The configuration: phosphor, text size and the boot ROM.
        TW_CHECK(machine.settings().tint == PhosphorTint::Amber);
        TW_CHECK_EQ(machine.settings().screen_text_size, 16);
        TW_CHECK_EQ(machine.settings().boot_language, static_cast<int>(Language::Pilot));
        // And the disk, which is a volume rather than a path.
        TW_CHECK(machine.disk().find(QStringLiteral("SQUARE")) != nullptr);
    }
}

// A refusal has to be a refusal: a machine image from a newer build must not be
// half-read, and a machine that failed to load must be the machine that was there.

void test_a_machine_image_refuses_what_it_cannot_read() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.loadProgram(QStringLiteral("10 PRINT \"SAFE\""));

    const QString newer = QDir::tempPath() + QStringLiteral("/timewarp-retro-tests/FUTURE.hb1");
    {
        QFile file{newer};
        TW_CHECK(file.open(QIODevice::WriteOnly | QIODevice::Truncate));
        file.write(R"({"format": 7, "name": "FROM THE FUTURE"})");
    }
    QString error;
    TW_CHECK(!machine.loadMachine(newer, &error));
    TW_CHECK(error.contains(QStringLiteral("format 7")));
    TW_CHECK_EQ(machine.program(), QStringLiteral("10 PRINT \"SAFE\""));

    const QString junk = QDir::tempPath() + QStringLiteral("/timewarp-retro-tests/JUNK.hb1");
    {
        QFile file{junk};
        TW_CHECK(file.open(QIODevice::WriteOnly | QIODevice::Truncate));
        file.write("not a machine at all");
    }
    error.clear();
    TW_CHECK(!machine.loadMachine(junk, &error));
    TW_CHECK(error.contains(QStringLiteral("not a machine image")));
    TW_CHECK_EQ(machine.program(), QStringLiteral("10 PRINT \"SAFE\""));

    error.clear();
    TW_CHECK(!machine.loadMachine(QDir::tempPath() + QStringLiteral("/nope/NONE.hb1"), &error));
    TW_CHECK(error.contains(QStringLiteral("no machine image at")));
}

// The words, which are how a user reaches an image without a file dialog.

void test_the_machine_words_save_and_load_an_image() {
    clearMachineSettings();
    const QString path = timewarp::ui::MachineImage::resolvePath(QStringLiteral("hv"));
    QFile::remove(path);
    {
        RetroComputerWindow machine;
        machine.loadProgram(QStringLiteral("10 PRINT \"FROM THE MACHINE\""));
        machine.submitCommand(QStringLiteral("SAVE MACHINE \"HV\""));
        TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("MACHINE SAVED")));
        TW_CHECK(QFileInfo::exists(path));
    }
    {
        RetroComputerWindow machine;
        machine.submitCommand(QStringLiteral("LOAD MACHINE \"HV\""));
        TW_CHECK_EQ(machine.program(), QStringLiteral("10 PRINT \"FROM THE MACHINE\""));
    }
    {
        RetroComputerWindow machine;
        machine.submitCommand(QStringLiteral("LOAD MACHINE \"NOSUCH\""));
        TW_CHECK(machine.consoleView()->toPlainText().contains(
            QStringLiteral("NO MACHINE IMAGE")));
    }
    QFile::remove(path);
}


void test_hb_dos_refuses_bad_names_and_a_full_disk() {
    clearMachineSettings();
    const QString image = QDir::tempPath() + QStringLiteral("/hb-dos-unit.dsk");
    QFile::remove(image);
    {
        HbDos dos{image};
        QString error;
        TW_CHECK_EQ(HbDos::normaliseName(QStringLiteral("  hello  ")),
                    QStringLiteral("HELLO"));
        TW_CHECK_EQ(HbDos::normaliseName(QStringLiteral("\"HELLO\"")),
                    QStringLiteral("HELLO"));
        TW_CHECK(HbDos::normaliseName(QStringLiteral("A,B"), &error).isEmpty());
        TW_CHECK_EQ(error, QStringLiteral("BAD FILE NAME"));
        TW_CHECK(HbDos::normaliseName(QString{}, &error).isEmpty());
        TW_CHECK(HbDos::normaliseName(QString(31, QLatin1Char('X')), &error).isEmpty());

        TW_CHECK_EQ(HbDos::sectorsFor(QStringLiteral("")), 1);
        TW_CHECK_EQ(HbDos::sectorsFor(QString(257, QLatin1Char('A'))), 2);

        // One file of 512 sectors fills the volume exactly, so the next save is
        // refused rather than silently truncated.
        const QString huge(512 * HbDos::kSectorSize, QLatin1Char('A'));
        TW_CHECK(dos.write(QStringLiteral("BIG"), QStringLiteral("basic"), huge, &error));
        TW_CHECK_EQ(dos.freeSectors(), 0);
        TW_CHECK(!dos.write(QStringLiteral("MORE"), QStringLiteral("basic"),
                            QStringLiteral("x"), &error));
        TW_CHECK_EQ(error, QStringLiteral("DISK FULL"));
        TW_CHECK(dos.find(QStringLiteral("BIG")) != nullptr);
        TW_CHECK(dos.find(QStringLiteral("MORE")) == nullptr);
    }
    QFile::remove(image);
}

}  // namespace tw::retro
