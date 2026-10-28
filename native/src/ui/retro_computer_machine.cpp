// SPDX-License-Identifier: MIT
//
// The machine behind the screen: the ROM, the runners, the canvas and the images.

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

bool RetroComputerWindow::handleMachineCommand(const QString& command) {
    const QString word = headWord(command);
    if (word == QStringLiteral("INIT")) {
        // The one word that erases a disk. It has to be said twice, and saying
        // anything else in between is a change of mind.
        if (!init_pending_) {
            init_pending_ = true;
            appendConsole(QStringLiteral("?INIT ERASES EVERY FILE ON THE DISK"));
            appendConsole(QStringLiteral("TYPE INIT AGAIN TO GO AHEAD"));
            return true;
        }
        init_pending_ = false;
        formatDisk();
        return true;
    }
    if (word == QStringLiteral("RUN")) {
        const QStringList arguments = dosArguments(command);
        if (arguments.isEmpty() || arguments.first().isEmpty()) {
            runProgram();
        } else {
            runFromDisk(arguments.first());
        }
        return true;
    }
    if (word == QStringLiteral("LIST")) {
        text_tabs_->setCurrentWidget(tube_);
        const QString source = program_->toPlainText();
        if (source.trimmed().isEmpty()) {
            appendConsole(QStringLiteral("?NO PROGRAM"));
        } else {
            const QStringList lines = source.split(QLatin1Char('\n'));
            for (const QString& line : lines) {
                appendConsole(line);
            }
        }
        return true;
    }
    if (word == QStringLiteral("NEW")) {
        stopRepl();
        rememberForUndo();
        const int erased = basicLines().size();
        program_->clear();
        programs_.remove(static_cast<int>(language_));
        clearScreenAndCanvas();
        if (erased > 0) {
            // The period's machines said nothing and meant it. This one keeps the
            // listing for one `UNDO`, and says so, because a learner losing an
            // afternoon to a two-key mistake is not a lesson worth teaching.
            appendConsole(QStringLiteral("?NEW ERASED %1 LINE(S) -- TYPE UNDO TO GET THEM BACK")
                              .arg(erased));
        }
        appendConsole(QStringLiteral("READY."));
        updateStatus();
        return true;
    }
    if (word == QStringLiteral("CLS") || word == QStringLiteral("CLEAR")) {
        clearScreenAndCanvas();
        return true;
    }
    if (word == QStringLiteral("CONFIG") || word == QStringLiteral("SETUP")) {
        openSettings();
        return true;
    }
    if (word == QStringLiteral("TEXT")) {
        setScreenMode(ScreenMode::Text);
        return true;
    }
    if (word == QStringLiteral("GRAPHICS") || word == QStringLiteral("GFX")) {
        setScreenMode(ScreenMode::Graphics);
        return true;
    }
    if (word == QStringLiteral("SPLIT")) {
        setScreenMode(ScreenMode::Split);
        return true;
    }
    if (word == QStringLiteral("EDIT")) {
        if (mode_ == ScreenMode::Graphics) {
            setScreenMode(ScreenMode::Split);
        }
        text_tabs_->setCurrentWidget(program_);
        program_->setFocus(Qt::ShortcutFocusReason);
        return true;
    }
    if (word == QStringLiteral("ROMS")) {
        text_tabs_->setCurrentWidget(tube_);
        for (const Language language : allLanguages()) {
            appendConsole(QStringLiteral("%1 %2")
                              .arg(language == language_ ? QLatin1Char('>')
                                                         : QLatin1Char(' '))
                              .arg(displayName(language)));
        }
        return true;
    }
    if (word == QStringLiteral("CATALOG") || word == QStringLiteral("CAT")) {
        listDisk();
        return true;
    }
    if (word == QStringLiteral("STATUS")) {
        showDiskStatus();
        return true;
    }
    // `SAVE MACHINE "N` and `LOAD MACHINE "N` are checked before `SAVE`/`LOAD`,
    // because the word splitter would otherwise read them as those two words with a
    // file called MACHINE.
    if (word == QStringLiteral("SAVEMACHINE") ||
        command.trimmed().startsWith(QStringLiteral("SAVE MACHINE"))) {
        saveMachineTo(machineArgument(command));
        return true;
    }
    if (word == QStringLiteral("LOADMACHINE") ||
        command.trimmed().startsWith(QStringLiteral("LOAD MACHINE"))) {
        loadMachineFrom(machineArgument(command));
        return true;
    }
    if (word == QStringLiteral("SAVE")) {
        saveToDisk(firstArgument(command));
        return true;
    }
    if (word == QStringLiteral("LOAD")) {
        loadFromDisk(firstArgument(command));
        return true;
    }
    if (word == QStringLiteral("DELETE") || word == QStringLiteral("ERA")) {
        eraseFromDisk(firstArgument(command));
        return true;
    }
    if (word == QStringLiteral("RENAME") || word == QStringLiteral("REN")) {
        const QStringList arguments = dosArguments(command);
        renameOnDisk(arguments.value(0), arguments.value(1));
        return true;
    }
    if (word == QStringLiteral("LOCK")) {
        lockOnDisk(firstArgument(command), true);
        return true;
    }
    if (word == QStringLiteral("UNLOCK")) {
        lockOnDisk(firstArgument(command), false);
        return true;
    }
    if (word == QStringLiteral("VERIFY") || word == QStringLiteral("VER")) {
        verifyOnDisk(firstArgument(command));
        return true;
    }
    if (word == QStringLiteral("TYPE")) {
        typeFromDisk(firstArgument(command));
        return true;
    }
    if (word == QStringLiteral("DIAL") || word == QStringLiteral("BBS")) {
        dialBoard();
        return true;
    }
    if (word == QStringLiteral("LLIST") || word == QStringLiteral("LPRINT")) {
        // The 1982 machine printed to a line printer; this one appends to a page in
        // the data directory, which is the same artefact a printer produced and a
        // better one to keep. LLIST prints the listing, LPRINT prints what follows
        // the word.
        if (word == QStringLiteral("LLIST")) {
            const QString source = program_->toPlainText();
            if (source.trimmed().isEmpty()) {
                appendConsole(QStringLiteral("?NO PROGRAM"));
                return true;
            }
            printToPaper(source, QStringLiteral("LISTING"));
            return true;
        }
        const QString text = firstArgument(command);
        if (text.isEmpty()) {
            appendConsole(QStringLiteral("?LPRINT WHAT"));
            return true;
        }
        printToPaper(text, QStringLiteral("LPRINT"));
        return true;
    }
    if (word == QStringLiteral("HELP") || command.trimmed() == QStringLiteral("?")) {
        text_tabs_->setCurrentWidget(tube_);
        appendConsole(QStringLiteral("RUN [\"N\"]  run the program; with a name, load N and run it"));
        appendConsole(QStringLiteral("LIST         list the program"));
        appendConsole(QStringLiteral("NEW          erase the program and the variables"));
        appendConsole(QStringLiteral("EDIT         move to the program editor"));
        appendConsole(QStringLiteral("TEXT / GFX / SPLIT   change the screen"));
        appendConsole(QStringLiteral("CLS          clear the screen and the drawing"));
        appendConsole(QStringLiteral("ROMS         list the language ROMs"));
        appendConsole(QStringLiteral("CONFIG       change the machine's settings"));
        if (language_ == Language::Basic) {
            appendConsole(QStringLiteral("10 ...       store a program line;  10  deletes it"));
        }
        appendConsole(QString());
        appendConsole(QStringLiteral("HB DOS 1.0"));
        appendConsole(QStringLiteral("CATALOG      list the disk"));
        appendConsole(QStringLiteral("SAVE \"N\"     save the program as N"));
        appendConsole(QStringLiteral("LOAD \"N\"     load N into memory"));
        appendConsole(QStringLiteral("DELETE \"N\"   erase N (LOCK protects it)"));
        appendConsole(QStringLiteral("RENAME \"A\",\"B\"  rename a file"));
        appendConsole(QStringLiteral("LOCK / UNLOCK \"N\"  protect a file"));
        appendConsole(QStringLiteral("LLIST        print the program to the paper tray"));
        appendConsole(QStringLiteral("LPRINT \"TEXT\"  print a line to the paper tray"));
        appendConsole(QStringLiteral("DIAL         call the bulletin board"));
        appendConsole(QStringLiteral("VERIFY \"N\"   compare memory with the disk"));
        appendConsole(QStringLiteral("TYPE \"N\"     print a file"));
        appendConsole(QStringLiteral("INIT         erase the whole disk (asks twice)"));
        appendConsole(QString());
        appendConsole(QStringLiteral("SAVE MACHINE \"N\"  write the whole machine to N.hb1"));
        appendConsole(QStringLiteral("LOAD MACHINE \"N\"  boot from N.hb1"));
        appendConsole(QStringLiteral("STATUS       volume and free space"));
        appendConsole(QString());
        appendConsole(QStringLiteral("THE BASIC LINE EDITOR"));
        appendConsole(QStringLiteral("AUTO [S[,N]]  number lines for you; an empty line ends it"));
        appendConsole(QStringLiteral("RENUM [S[,N]] renumber the listing and every jump in it"));
        appendConsole(QStringLiteral("DELETE 20-30  delete a range;  LIST 20-30  list one"));
        appendConsole(QStringLiteral("UNDO          put back the listing NEW erased"));
        appendConsole(QStringLiteral("TRON / TROFF  print each line's number as it runs"));
        appendConsole(QString());
        appendConsole(QStringLiteral("F9 runs; Shift+F9 stops; Ctrl+L clears; Ctrl+, configures."));
        return true;
    }
    return false;
}

void RetroComputerWindow::saveMachineTo(const QString& name) {
    const QString path = MachineImage::resolvePath(name);
    if (path.isEmpty()) {
        appendConsole(QStringLiteral("?SAVE MACHINE \"NAME\""));
        return;
    }
    QString error;
    if (!saveMachine(path, &error)) {
        appendConsole(QStringLiteral("?%1").arg(error.toUpper()));
        return;
    }
    appendConsole(QStringLiteral("MACHINE SAVED: %1").arg(path));
    appendConsole(QStringLiteral("%1 PROGRAM(S), %2 FILE(S) ON THE DISK")
                      .arg(machineImage().programs.size())
                      .arg(dos_.files().size()));
}

void RetroComputerWindow::loadMachineFrom(const QString& name) {
    const QString path = MachineImage::resolvePath(name);
    if (path.isEmpty()) {
        appendConsole(QStringLiteral("?LOAD MACHINE \"NAME\""));
        return;
    }
    if (!QFileInfo::exists(path)) {
        appendConsole(QStringLiteral("?NO MACHINE IMAGE: %1").arg(path.toUpper()));
        return;
    }
    // The console is cleared by the reboot that follows, so the line that says what
    // was loaded is printed by `reboot`'s banner path instead of here.
    QString error;
    if (!loadMachine(path, &error)) {
        appendConsole(QStringLiteral("?%1").arg(error.toUpper()));
        return;
    }
    appendConsole(QStringLiteral("MACHINE LOADED: %1").arg(path));
}

timewarp::lang::Interpreter::MachineBus RetroComputerWindow::machineBus() {
    // The engine runs on the runner's thread and the tube paints on the GUI thread.
    // A character cell is one byte and a register is one byte, so a write landing
    // while a frame is painted is a byte the next frame shows -- a tearing screen,
    // which is a thing the hardware this imitates also did. Nothing here can
    // half-update a value.
    timewarp::lang::Interpreter::MachineBus bus;
    bus.peek = [this](int address) { return memory_.peek(address); };
    bus.poke = [this](int address, std::uint8_t value) { return memory_.poke(address, value); };
    bus.set_cursor = [this](int row, int column) {
        memory_.setCursor(row, column);
        // The cursor moved, so the picture changed: a queued update is cheaper than
        // a repaint per statement and still reaches the eye on the next frame.
        if (tube_ != nullptr) {
            QMetaObject::invokeMethod(tube_, [this] { tube_->update(); }, Qt::QueuedConnection);
        }
    };
    bus.set_ink = [this](int index) { memory_.setInk(static_cast<std::uint8_t>(index)); };
    bus.set_paper = [this](int index) { memory_.setPaper(static_cast<std::uint8_t>(index)); };
    bus.write_text = [this](const std::string& line) {
        // On the engine's thread, in order with the POKEs around it. The GPU-side
        // repaint is queued; the bytes are what matter and they are already right.
        memory_.writeText(HbTube::toCharset(
                              QString::fromUtf8(line.data(), static_cast<int>(line.size()))),
                          true);
        if (tube_ != nullptr) {
            QMetaObject::invokeMethod(tube_, [this] { tube_->update(); }, Qt::QueuedConnection);
        }
    };
    bus.clear_screen = [this] {
        memory_.clear();
        if (tube_ != nullptr) {
            QMetaObject::invokeMethod(tube_, [this] { tube_->update(); }, Qt::QueuedConnection);
        }
    };
    return bus;
}

void RetroComputerWindow::ensureRepl() {
    if (repl_ != nullptr) {
        return;
    }
    const std::optional<timewarp::lang::Language> native = nativeLanguage(language_);
    if (!native.has_value() || queue_ == nullptr) {
        appendConsole(QStringLiteral("?NO LANGUAGE ROM"));
        return;
    }

    repl_ = new ReplRunner{*native, *queue_, this};
    repl_->setWakeup([this] {
        if (bridge_ != nullptr) {
            bridge_->wake();
        }
    });
    repl_->setSurfaceColor(toRgb(retroPalette(settings_.tint).canvas));
    repl_->setMachineBus(machineBus());
    connect(repl_, &ReplRunner::outputAvailable, this, [this](const QString& text) {
        // The bus wrote it to the screen; this is the scrollback. See `machineBus()`.
        if (tube_ != nullptr) {
            tube_->remember(text);
        }
    });
    connect(repl_, &ReplRunner::commandFinished, this,
            [this](bool /*ok*/, const QString& error) {
                if (runner_ == nullptr && canvas_ != nullptr) {
                    if (bridge_ != nullptr) {
                        bridge_->drainNow();
                    }
                    canvas_->fitToContent();
                }
                if (!error.isEmpty()) {
                    // A machine buzzes when a program does something wrong. It is the
                    // one sound a reader of the period remembers as *bad*, which is
                    // exactly what makes it useful.
                    if (voice_ != nullptr) {
                        voice_->play(MachineVoice::Event::Error);
                    }
                    appendConsole(QStringLiteral("?%1").arg(error));
                }
                updateStatus();
                // A prompt command has returned, so the next one can be typed.
                emit commandFinished();
            });
}

void RetroComputerWindow::stopRepl() {
    if (repl_ == nullptr) {
        return;
    }
    repl_->requestStop();
    repl_->wait();
    delete repl_;
    repl_ = nullptr;
}

void RetroComputerWindow::stopRunner() {
    if (runner_ == nullptr) {
        return;
    }
    NativeRunner* runner = runner_;
    runner->request_stop();
    runner->wait();
    if (runner_ == runner) {
        runner_ = nullptr;
        runner->deleteLater();
    }
}

void RetroComputerWindow::runProgram() {
    if (runner_ != nullptr) {
        return;
    }
    const QString source = program_ != nullptr ? program_->toPlainText() : QString{};
    if (source.trimmed().isEmpty()) {
        appendConsole(QStringLiteral("?NO PROGRAM"));
        return;
    }
    const std::optional<timewarp::lang::Language> native = nativeLanguage(language_);
    if (!native.has_value() || queue_ == nullptr) {
        appendConsole(QStringLiteral("?NO LANGUAGE ROM"));
        return;
    }

    // One engine at a time: a REPL command in flight would be a second program
    // drawing into the same tube.
    stopRepl();
    text_tabs_->setCurrentWidget(tube_);
    appendConsole(QStringLiteral("* RUN *"));
    if (bridge_ != nullptr) {
        bridge_->drainNow();
    }
    canvas_->reset();
    auto_shown_graphics_ = false;

    auto* runner = new NativeRunner{*native, *queue_, this};
    runner_ = runner;
    run_error_sounded_ = false;
    runner->set_wakeup([this] {
        if (bridge_ != nullptr) {
            bridge_->wake();
        }
    });
    runner->set_surface_color(toRgb(retroPalette(settings_.tint).canvas));
    if (sound_ == nullptr) {
        sound_ = new SoundPlayer{this};
    }
    runner->set_sound_player(sound_);
    // `TRON` is the machine's word; the trace itself is the engine's, printed through
    // the same stream as the program's output so the tube shows them interleaved the
    // way a reader of a 1982 listing expects.
    runner->set_tracing(trace_on_);
    runner->set_machine_bus(machineBus());
    runner->load_program(source);

    connect(runner, &NativeRunner::outputAvailable, this,
            [this, runner](const QString& text) {
                if (runner_ != runner) {
                    return;
                }
                // The screen already has it: the machine bus wrote the line into
                // memory in the program's own order. This is the scrollback.
                if (tube_ != nullptr) {
                    tube_->remember(text);
                }
                // An error the engine reported through its output -- which is how the
                // line-loop languages report one -- buzzes once per run.
                if (!run_error_sounded_ && text.contains(QStringLiteral("\u274C"))) {
                    run_error_sounded_ = true;
                    if (voice_ != nullptr) {
                        voice_->play(MachineVoice::Event::Error);
                    }
                }
            });
    connect(runner, &NativeRunner::inputRequested, this,
            [this, runner](const QString& prompt) {
                if (runner_ != runner) {
                    return;
                }
                bool accepted = false;
                const QString answer = QInputDialog::getText(
                    this, tr("The program is waiting for input"), prompt,
                    QLineEdit::Normal, QString{}, &accepted);
                runner->answerInput(accepted ? answer : QString{});
            });
    connect(runner, &NativeRunner::run_finished, this,
            [this, runner](bool completed, const QString& error) {
                if (runner_ != runner) {
                    return;
                }
                if (bridge_ != nullptr) {
                    bridge_->drainNow();
                }
                if (canvas_ != nullptr) {
                    canvas_->fitToContent();
                }
                if (!completed) {
                    appendConsole(QStringLiteral("?STOPPED"));
                }
                if (!error.isEmpty()) {
                    appendConsole(QStringLiteral("?%1").arg(error));
                }
                appendConsole(QStringLiteral("READY."));
                runner_ = nullptr;
                runner->deleteLater();
                if (canvas_ != nullptr && canvas_->frameCount() > 0 &&
                    mode_ == ScreenMode::Text && settings_.switch_to_split) {
                    setScreenMode(ScreenMode::Split);
                }
                updateStatus();
                emit runFinished();
            });

    updateStatus();
    runner->start();
}

void RetroComputerWindow::stopProgram() {
    if (runner_ == nullptr) {
        appendConsole(QStringLiteral("?NOT RUNNING"));
        return;
    }
    // The runner reports the stop through `run_finished`, so the console and the
    // status line are updated in one place whether the program stopped itself or
    // the user did.
    runner_->request_stop();
}

void RetroComputerWindow::clearScreenAndCanvas() {
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
    updateStatus();
}

void RetroComputerWindow::setLanguage(Language language) {
    if (prepared_ && booted_ && language == language_) {
        return;
    }
    if (booted_) {
        saveCurrentProgram();
    }
    stopRunner();
    stopRepl();

    language_ = language;
    if (language_selector_ != nullptr) {
        const QSignalBlocker blocker{language_selector_};
        const int index = language_selector_->findData(static_cast<int>(language_));
        if (index >= 0) {
            language_selector_->setCurrentIndex(index);
        }
    }
    if (program_ != nullptr) {
        program_->setLanguage(language_);
        program_->setPlainText(programs_.value(static_cast<int>(language_)));
    }
    updatePrompt();
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
    booted_ = true;
    prepared_ = true;
    updateModeControls();
    updateStatus();
    setWindowTitle(tr("HB-1 \u2014 %1").arg(displayName(language_)));
}

void RetroComputerWindow::setScreenMode(ScreenMode mode) {
    mode_ = mode;
    updateModeControls();
    updateStatus();
}

MachineImage RetroComputerWindow::machineImage() const {
    MachineImage image;
    image.name = machine_label_ != nullptr ? machine_label_->text() : QStringLiteral("HB-1");
    image.language = settingsKey(language_);
    image.mode = static_cast<int>(mode_);
    // The program in the editor is the current ROM's, and it is the newest thing
    // the machine knows: the copy in `programs_` is what was there when the ROM was
    // last switched away from.
    QHash<int, QString> programs = programs_;
    if (program_ != nullptr) {
        programs.insert(static_cast<int>(language_), program_->toPlainText());
    }
    for (auto it = programs.cbegin(); it != programs.cend(); ++it) {
        const QString key = settingsKey(static_cast<Language>(it.key()));
        if (!it.value().isEmpty()) {
            image.programs.insert(key, it.value());
        }
    }
    image.config = settings_;
    image.volume = dos_.toJson();
    return image;
}

void RetroComputerWindow::applyMachineImage(const MachineImage& image, bool boot) {
    const std::optional<Language> language = languageFromKey(image.language);
    if (!language.has_value()) {
        // A ROM this build does not have: keep the one that is in the socket rather
        // than emptying it. The image is still applied in every other respect, so a
        // machine from a build with one more language is usable rather than refused.
        appendConsole(QStringLiteral("?UNKNOWN ROM IN IMAGE: %1").arg(image.language));
    }

    // The editor's text belongs to the ROM the machine is *in now*, so it is written
    // back before the image replaces every ROM's memory -- otherwise the image's
    // program for this ROM would be overwritten by the session's on the way in.
    if (booted_) {
        saveCurrentProgram();
    }
    programs_.clear();
    for (auto it = image.programs.cbegin(); it != image.programs.cend(); ++it) {
        const std::optional<Language> rom = languageFromKey(it.key());
        if (rom.has_value()) {
            programs_.insert(static_cast<int>(*rom), it.value());
        }
    }

    // The configuration next, because the boot ROM and the boot screen in it decide
    // what a cold boot does -- and a machine that is being *booted* has to be set up
    // before it starts.
    applySettings(image.config, false);

    if (language.has_value()) {
        language_ = *language;
        if (language_selector_ != nullptr) {
            const QSignalBlocker blocker{language_selector_};
            const int index = language_selector_->findData(static_cast<int>(language_));
            if (index >= 0) {
                language_selector_->setCurrentIndex(index);
            }
        }
        if (program_ != nullptr) {
            program_->setLanguage(*language);
            program_->setPlainText(programs_.value(static_cast<int>(*language)));
        }
    }
    setScreenMode(static_cast<ScreenMode>(std::clamp(image.mode, 0, 2)));

    QString volume_error;
    if (!dos_.fromJson(image.volume, &volume_error)) {
        appendConsole(QStringLiteral("?%1").arg(volume_error));
    } else if (!dos_.commit(&volume_error)) {
        // A machine whose disk could not be written still works; the volume is in
        // memory and the next `SAVE` tries again. Saying so is the point.
        appendConsole(QStringLiteral("?%1").arg(volume_error));
    }

    if (boot) {
        if (booted_) {
            stopRunner();
            stopRepl();
        }
        reboot();
    }
    updatePrompt();
    updateStatus();
}

bool RetroComputerWindow::saveMachine(const QString& path, QString* error) const {
    return machineImage().save(path, error);
}

bool RetroComputerWindow::loadMachine(const QString& path, QString* error) {
    MachineImage image;
    if (!MachineImage::load(path, &image, error)) {
        return false;
    }
    applyMachineImage(image, true);
    return true;
}

QString RetroComputerWindow::program() const {
    return program_ != nullptr ? program_->toPlainText() : QString{};
}

}  // namespace timewarp::ui
