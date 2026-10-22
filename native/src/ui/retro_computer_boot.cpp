// SPDX-License-Identifier: MIT
//
// Power, boot, the self test and the frame tick.

#include "retro_computer_internal.hpp"

#include <algorithm>
#include <optional>

#include <QCloseEvent>
#include <QComboBox>
#include <QFileInfo>
#include <QFont>
#include <QFrame>
#include <QHBoxLayout>
#include <QInputDialog>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QPalette>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QRegularExpression>
#include <QScrollBar>
#include <QSettings>
#include <QShortcut>
#include <QSignalBlocker>
#include <QSplitter>
#include <QTabWidget>
#include <QTextCursor>
#include <QVBoxLayout>
#include <QWidget>

#include "timewarp/canvas/command_queue.hpp"
#include "timewarp/canvas/qt_bridge.hpp"
#include "timewarp/core/qt_gui_compat.hpp"
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/native_runner.hpp"
#include "timewarp/ui/panels.hpp"
#include "timewarp/ui/repl_runner.hpp"
#include "timewarp/ui/sound_player.hpp"
#include "timewarp/ui/turtle_canvas.hpp"

namespace timewarp::ui {

using namespace retro_detail;

void RetroComputerWindow::showProgramPage() {
    if (text_tabs_ != nullptr && program_ != nullptr) {
        text_tabs_->setCurrentWidget(program_);
    }
}

void RetroComputerWindow::loadProgram(const QString& source) {
    if (program_ == nullptr) {
        return;
    }
    program_->setPlainText(source);
    programs_[static_cast<int>(language_)] = source;
    program_->document()->setModified(false);
    updateStatus();
}

void RetroComputerWindow::coldBootSequence() {
    if (!powered_ || voice_ == nullptr) {
        return;
    }
    // A cold boot is a cold boot whether it came from the switch or from a ROM
    // change: the chirp, the self-test (when the start-up message is Full), the
    // banner. One function so the two paths cannot drift.
    voice_->play(MachineVoice::Event::Boot);
    if (settings_.banner == BootBanner::Full) {
        runSelfTest();
    }
    appendBootBanner();
}

void RetroComputerWindow::reboot() {
    stopRunner();
    stopRepl();
    if (tube_ != nullptr) {
        tube_->clear();
    }
    if (bridge_ != nullptr) {
        bridge_->drainNow();
    }
    if (canvas_ != nullptr) {
        canvas_->reset();
    }
    auto_shown_graphics_ = false;
    coldBootSequence();
    text_tabs_->setCurrentWidget(tube_);
    updateStatus();
}

void RetroComputerWindow::tickFrame() {
    if (!powered_) {
        return;
    }
    memory_.tickFrame();
    // The sound latch is a register, not a queue: a program pokes a code and the
    // machine plays it on the next frame, then clears the register itself so the
    // program does not have to.
    if (const int code = memory_.takeSoundLatch(); code != 0) {
        voice_->tone(code);
    }
}

bool RetroComputerWindow::runSelfTest() {
    if (!powered_) {
        return false;
    }
    // A self-test that prints "OK" without testing anything is the same lie as a
    // hardcoded `MEM 64K`. This one walks the memory it claims to have and sums the
    // bytes of the table the banner is built from -- and it does the walking *before*
    // it prints, because the first draft cleared the memory afterwards and wiped the
    // two lines it had just written.
    int passed = 0;
    for (int address = 0; address < HbMemory::kUserRamSize; address += 257) {
        const auto pattern = static_cast<std::uint8_t>((address * 7 + 1) & 0xFF);
        if (!memory_.poke(address, pattern) || memory_.peek(address) != pattern) {
            appendConsole(QStringLiteral("?RAM FAILED AT %1").arg(address));
            return false;
        }
        ++passed;
    }
    // The RAM goes back the way the machine found it: a self-test that leaves its
    // pattern behind is a test with a side effect.
    for (int address = 0; address < HbMemory::kUserRamSize; ++address) {
        memory_.poke(address, 0);
    }
    appendConsole(QStringLiteral("HB-1 SELF TEST"));
    appendConsole(QStringLiteral("%1 BYTES FREE").arg(HbMemory::kUserRamSize));
    appendConsole(QStringLiteral("RAM CHECK OK (%1 CELLS)").arg(passed));

    // The ROM check: a sum of every ROM's version string and banner note, which is
    // the only ROM there is.
    std::uint32_t rom_sum = 0;
    int roms = 0;
    for (const Language language : allLanguages()) {
        const RomInfo rom = romFor(language);
        const std::string text = std::string{rom.version} + std::string{rom.prompt} +
                                 std::string{rom.note};
        for (const char byte : text) {
            rom_sum = rom_sum * 31 + static_cast<unsigned char>(byte);
        }
        ++roms;
    }
    const int sockets = static_cast<int>(allLanguages().size());
    if (roms != sockets) {
        appendConsole(QStringLiteral("?ROM CHECK: %1 OF %2 SOCKETS").arg(roms).arg(sockets));
        return false;
    }
    appendConsole(QStringLiteral("ROM CHECK OK (%1 ROMS, %2)").arg(roms).arg(rom_sum));
    appendConsole(QStringLiteral("HB DOS 1.0  %1").arg(dos_.imagePath().isEmpty()
                                                            ? QStringLiteral("NO DISK")
                                                            : dos_.volumeName()));
    return true;
}

void RetroComputerWindow::setPowered(bool on) {
    if (powered_ == on) {
        return;
    }
    powered_ = on;
    if (tube_ != nullptr) {
        tube_->setPowered(on);
    }
    if (input_ != nullptr) {
        input_->setEnabled(on);
        if (on) {
            input_->setFocus();
        }
    }
    run_button_->setEnabled(on);
    stop_button_->setEnabled(on);
    if (config_button_ != nullptr) {
        config_button_->setEnabled(on);
    }
    if (lamp_label_ != nullptr) {
        lamp_label_->setProperty("powered", on);
        lamp_label_->style()->unpolish(lamp_label_);
        lamp_label_->style()->polish(lamp_label_);
    }
    if (!on) {
        // Off means off: the run stops, the clock stops, the tube goes dark and says
        // nothing. A machine that keeps computing with the switch off is a screenshot.
        stopRunner();
        stopRepl();
        clock_->stop();
        updateStatus();
        return;
    }
    clock_->start();
    // `reboot` is the cold boot, chirp included: playing one here as well made a
    // power-on sound twice.
    reboot();
}

void RetroComputerWindow::togglePower() { setPowered(!powered_); }

}  // namespace timewarp::ui
