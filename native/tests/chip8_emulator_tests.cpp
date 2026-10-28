// SPDX-License-Identifier: MIT
//
// The live CHIP-8 machine and its window.
//
// The engine's instruction semantics are covered by `lang_tests`; this suite is
// about the things only the emulator does: frames, the clock, pause/step, the
// keypad that changes while a program runs, and the window's controls.

#include <cstdio>
#include <memory>

#include <QAction>
#include <QApplication>
#include <QEventLoop>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QPlainTextEdit>
#include <QSlider>
#include <QSpinBox>
#include <QTableWidget>
#include <QTimer>

#include "check.hpp"
#include "temp_dir.hpp"
#include "timewarp/ui/chip8_emulator.hpp"
#include "timewarp/ui/chip8_library.hpp"
#include "timewarp/ui/chip8_screen.hpp"
#include "timewarp/ui/chip8_window.hpp"

namespace {

using timewarp::ui::Chip8Emulator;
using timewarp::ui::Chip8LibraryDialog;
using timewarp::ui::Chip8Snapshot;
using timewarp::ui::Chip8Window;

void announce(const char* name) { std::printf("-- %s\n", name); }

/// LD V0, 5; LD V1, 3; ADD V0, V1; HALT. Short enough to reason about and long
/// enough that a wrong PC after Reset is visible.
constexpr const char* kAddProgram =
    "start: LD V0, 5\n"
    "       LD V1, 3\n"
    "       ADD V0, V1\n"
    "       HALT\n";

void test_a_listing_runs_and_steps() {
    announce("emulator: a listing runs and steps");
    Chip8Emulator emulator;
    QString error;
    TW_CHECK(emulator.loadSource(QString::fromLatin1(kAddProgram), &error));
    TW_CHECK(error.isEmpty());
    TW_CHECK(emulator.loaded());
    TW_CHECK(!emulator.running());

    const Chip8Snapshot loaded = emulator.snapshot();
    TW_CHECK(loaded.loaded);
    TW_CHECK_EQ(loaded.pc, 0x200);
    TW_CHECK_EQ(loaded.cycles, std::uint64_t{0});
    TW_CHECK_EQ(loaded.framebuffer.size(), std::size_t{64 * 32});

    emulator.step();
    emulator.step();
    emulator.step();
    const Chip8Snapshot stepped = emulator.snapshot();
    TW_CHECK_EQ(static_cast<int>(stepped.v[0]), 8);
    TW_CHECK(!stepped.halted);
    emulator.step();
    TW_CHECK(emulator.snapshot().halted);
}

void test_a_frame_runs_the_speed_and_ticks_the_timers() {
    announce("emulator: a frame runs its opcodes and ticks the timers");
    Chip8Emulator emulator;
    TW_CHECK(emulator.loadSource(QString::fromLatin1(
        "start: LD V0, 2\n"
        "       LD DT, V0\n"
        "       LD ST, V0\n"
        "loop:  JP loop\n")));
    emulator.setSpeed(10);
    emulator.advanceFrame();
    const Chip8Snapshot first = emulator.snapshot();
    // The three setup instructions ran and the loop did not halt; one frame's
    // timers then took one off each.
    TW_CHECK(first.cycles >= 4);
    TW_CHECK_EQ(first.delay_timer, 1);
    TW_CHECK_EQ(first.sound_timer, 1);
    TW_CHECK(!first.halted);

    emulator.advanceFrame();
    TW_CHECK_EQ(emulator.snapshot().delay_timer, 0);
    TW_CHECK_EQ(emulator.snapshot().sound_timer, 0);
}

void test_a_binary_rom_loads_byte_for_byte() {
    announce("emulator: a binary ROM loads");
    Chip8Emulator emulator;
    // LD V0, 7; HALT. The NUL is what marks this as a ROM.
    const QByteArray rom{"\x60\x07\x00\x00", 4};
    TW_CHECK(emulator.loadBytes(rom, QStringLiteral("demo.ch8")));
    emulator.advanceFrame();
    const Chip8Snapshot snapshot = emulator.snapshot();
    TW_CHECK_EQ(static_cast<int>(snapshot.v[0]), 7);
    TW_CHECK(snapshot.halted);
}

void test_a_bad_program_is_reported() {
    announce("emulator: a bad program is reported");
    Chip8Emulator emulator;
    TW_CHECK(!emulator.loadSource(QStringLiteral("start: FROB V0")));
    TW_CHECK(!emulator.error().isEmpty());
    TW_CHECK(!emulator.loaded());
    // A failed load leaves nothing to run.
    emulator.advanceFrame();
    TW_CHECK(!emulator.snapshot().loaded);
}

void test_run_pause_and_reset() {
    announce("emulator: run, pause and reset");
    Chip8Emulator emulator;
    TW_CHECK(emulator.loadSource(QString::fromLatin1(
        "start: LD V0, 1\n"
        "loop:  JP loop\n")));
    emulator.start();
    TW_CHECK(emulator.running());
    emulator.pause();
    TW_CHECK(!emulator.running());

    emulator.step();
    TW_CHECK(emulator.snapshot().cycles >= 1);

    emulator.reset();
    const Chip8Snapshot reset = emulator.snapshot();
    TW_CHECK_EQ(reset.pc, 0x200);
    TW_CHECK_EQ(reset.cycles, std::uint64_t{0});
    TW_CHECK_EQ(static_cast<int>(reset.v[0]), 0);
    TW_CHECK(!reset.halted);
}

void test_speed_is_bounded() {
    announce("emulator: speed is bounded");
    Chip8Emulator emulator;
    emulator.setSpeed(0);
    TW_CHECK_EQ(emulator.speed(), 1);
    emulator.setSpeed(timewarp::ui::kChip8MaximumSpeed + 500);
    TW_CHECK_EQ(emulator.speed(), timewarp::ui::kChip8MaximumSpeed);
    emulator.setSpeed(25);
    TW_CHECK_EQ(emulator.speed(), 25);
}

void test_the_clock_runs_without_manual_frames() {
    announce("emulator: the clock runs frames");
    Chip8Emulator emulator;
    TW_CHECK(emulator.loadSource(QString::fromLatin1(
        "start: LD V0, 1\n"
        "loop:  JP loop\n")));
    emulator.start();
    // A short event loop is enough to prove the timer is connected; the count is
    // deliberately a lower bound so a loaded machine cannot make it flaky.
    QEventLoop loop;
    QTimer::singleShot(120, &loop, &QEventLoop::quit);
    loop.exec();
    emulator.pause();
    TW_CHECK(emulator.snapshot().cycles >= 10);
}

void test_ld_key_waits_for_a_press() {
    announce("emulator: LD Vx, K waits for a press");
    Chip8Emulator emulator;
    TW_CHECK(emulator.loadSource(QString::fromLatin1(
        "start: LD V0, K\n"
        "       HALT\n")));
    emulator.advanceFrame();
    Chip8Snapshot waiting = emulator.snapshot();
    TW_CHECK(waiting.waiting_for_key);
    TW_CHECK_EQ(static_cast<int>(waiting.v[0]), 0);
    TW_CHECK(!waiting.halted);

    emulator.setKey(9, true);
    emulator.advanceFrame();
    const Chip8Snapshot done = emulator.snapshot();
    TW_CHECK(!done.waiting_for_key);
    TW_CHECK_EQ(static_cast<int>(done.v[0]), 9);
    TW_CHECK(done.halted);
}


// Rewind is the machine's own history: a snapshot every quarter second, and the
// ability to put the machine back into one. It is what turns "it died just after
// that jump" from a story into something to look at -- which is why it is asserted
// here rather than in the engine's suite, where there is no clock.

void test_rewind_keeps_history_and_goes_back() {
    announce("emulator: rewind");
    Chip8Emulator emulator;
    TW_CHECK(emulator.loadSource(QString::fromLatin1(
        "start: LD V0, 1\n"
        "loop:  JP loop\n")));
    // Freshly loaded there is one moment -- the start -- and nothing to rewind to.
    TW_CHECK_EQ(emulator.rewindCount(), 1);
    TW_CHECK_EQ(emulator.beginRewind(), 1);
    TW_CHECK_EQ(emulator.rewindCount(), 1);

    // Run a few frames' worth of history: one snapshot per stride.
    for (int frame = 0; frame < timewarp::ui::kChip8RewindStride * 3; ++frame) {
        emulator.advanceFrame();
    }
    // `beginRewind` is what the window calls when the slider is first touched: it
    // pauses and captures the live state, so the newest moment is exactly the frame
    // the user was looking at rather than the last scheduled snapshot.
    const int moments = emulator.beginRewind();
    TW_CHECK(moments >= 3);
    const std::uint64_t live_cycles = emulator.snapshot().cycles;
    TW_CHECK(live_cycles > 0);

    // The oldest moment is the machine as it was loaded.
    emulator.rewindTo(0);
    const Chip8Snapshot first = emulator.snapshot();
    TW_CHECK_EQ(first.pc, 0x200);
    TW_CHECK_EQ(first.cycles, std::uint64_t{0});
    TW_CHECK_EQ(static_cast<int>(first.v[0]), 0);
    TW_CHECK_EQ(emulator.rewindPosition(), 0);

    // The newest is where it was, so "rewind to now" gets a machine back rather
    // than one frame short of it.
    emulator.rewindTo(moments - 1);
    TW_CHECK_EQ(emulator.snapshot().cycles, live_cycles);
    TW_CHECK(!emulator.running());

    // Out of range clamps rather than crashing: a slider and a ring can disagree for
    // one event, and the machine must not be the thing that breaks when they do.
    emulator.rewindTo(9999);
    TW_CHECK_EQ(emulator.rewindPosition(), moments - 1);
    emulator.rewindTo(-4);
    TW_CHECK_EQ(emulator.rewindPosition(), 0);
}

void test_resuming_from_the_past_forgets_the_future() {
    announce("emulator: a new timeline");
    Chip8Emulator emulator;
    TW_CHECK(emulator.loadSource(QString::fromLatin1(
        "start: ADD V0, 1\n"
        "       JP start\n")));
    for (int frame = 0; frame < timewarp::ui::kChip8RewindStride * 4; ++frame) {
        emulator.advanceFrame();
    }
    const int moments = emulator.beginRewind();
    TW_CHECK(moments >= 4);
    emulator.rewindTo(1);
    TW_CHECK_EQ(emulator.rewindPosition(), 1);

    // Running from the past drops the moments that came after it: they are a future
    // the machine is no longer going to have, and a slider that offered them would
    // be lying.
    emulator.start();
    TW_CHECK(emulator.rewindPosition() < 0);
    TW_CHECK(emulator.rewindCount() <= 2);
    emulator.pause();
    TW_CHECK(emulator.rewindCount() >= 2);

    // And Reset clears the history: a new program's past is not the old one's.
    emulator.reset();
    TW_CHECK_EQ(emulator.rewindCount(), 1);
    TW_CHECK_EQ(emulator.rewindPosition(), -1);
}

#ifdef TIME_WARP_HAVE_AI
// The CHIP-8 window is a machine's own, so the assistant is a toolbar action opening
// a dialog: the listing it explains is the one beside it.
void test_the_window_offers_the_assistant() {
    announce("window: the assistant");
    Chip8Window window;
    auto* action = window.findChild<QAction*>(QStringLiteral("chip8AssistantAction"));
    TW_CHECK(action != nullptr);
    if (action == nullptr) {
        return;
    }
    TW_CHECK(window.assistantDialog() == nullptr);
    action->trigger();
    timewarp::ui::AssistantDialog* dialog = window.assistantDialog();
    TW_CHECK(dialog != nullptr);
    if (dialog == nullptr) {
        return;
    }
    TW_CHECK(dialog->panel() != nullptr);
    action->trigger();
    TW_CHECK(window.assistantDialog() == dialog);
    dialog->close();
}
#endif

void test_the_window_offers_the_rewind_strip() {
    announce("window: the rewind strip");
    Chip8Window window;
    TW_CHECK(window.loadSource(QString::fromLatin1(
        "start: LD V0, 1\n"
        "loop:  JP loop\n")));
    auto* slider = window.findChild<QSlider*>(QStringLiteral("chip8Rewind"));
    auto* label = window.findChild<QLabel*>(QStringLiteral("chip8RewindLabel"));
    TW_CHECK(slider != nullptr);
    TW_CHECK(label != nullptr);
    if (slider == nullptr || label == nullptr) {
        return;
    }
    // A freshly loaded program has no history, so the strip is inert rather than a
    // slider that does nothing when it moves.
    TW_CHECK(!slider->isEnabled());
    TW_CHECK(slider->maximum() <= 0);

    for (int frame = 0; frame < timewarp::ui::kChip8RewindStride * 3; ++frame) {
        window.emulator()->advanceFrame();
    }
    window.emulator()->pause();
    // The window refreshes on the emulator's frames; the pause is what a person
    // would have done before reaching for the slider.
    QApplication::processEvents();
    TW_CHECK(slider->isEnabled());
    TW_CHECK(slider->maximum() >= 2);

    // Dragging to the left pauses the machine and shows a moment in the past.
    slider->setValue(0);
    TW_CHECK(!window.emulator()->running());
    TW_CHECK_EQ(window.emulator()->rewindPosition(), 0);
    TW_CHECK(label->text().contains(QStringLiteral("s")));

    // And back to the right edge is the present.
    slider->setValue(slider->maximum());
    TW_CHECK_EQ(window.emulator()->snapshot().cycles > 0, true);
    TW_CHECK(label->text() == QStringLiteral("now"));
}

void test_the_window_offers_the_rom_library() {
    announce("window: the ROM library");
    Chip8Window window;
    TW_CHECK(window.findChild<QAction*>(QStringLiteral("chip8LibraryAction")) != nullptr);

    const tw::test::TempDir dir;
    TW_CHECK(dir.valid());
    dir.write("games/one.ch8", QByteArray{"\0"});
    dir.write("hires/two.ch8", QByteArray{"\0"});
    dir.write("notes.txt", "not a program");

    Chip8LibraryDialog dialog{dir.path()};
    TW_CHECK_EQ(dialog.romCount(), 2);
    // The first program is selected, so Open is one click away.
    TW_CHECK_EQ(dialog.selectedPath(), dir.path(QStringLiteral("games/one.ch8")));
    TW_CHECK_EQ(dialog.folder(), dir.path());

    // The filter hides what does not match without rebuilding the list.
    auto* filter = dialog.findChild<QLineEdit*>(QStringLiteral("chip8LibraryFilter"));
    auto* list = dialog.findChild<QListWidget*>(QStringLiteral("chip8LibraryList"));
    TW_CHECK(filter != nullptr);
    TW_CHECK(list != nullptr);
    if (filter != nullptr && list != nullptr) {
        filter->setText(QStringLiteral("two"));
        int visible = 0;
        for (int row = 0; row < list->count(); ++row) {
            visible += list->item(row)->isHidden() ? 0 : 1;
        }
        TW_CHECK_EQ(visible, 1);
    }
}

void test_a_hires_program_reaches_the_screen() {
    announce("window: hi-res reaches the screen");
    Chip8Window window;
    TW_CHECK(window.loadSource(
        "start: DW 0x00FF\n"
        "       LD I, dot\n"
        "       DRW V0, V1, 1\n"
        "       HALT\n"
        "dot:   DB 0xFF"));
    // Ten opcodes a frame covers the whole program, and the frame emits the
    // signal the window repaints from.
    window.emulator()->advanceFrame();
    const Chip8Snapshot snapshot = window.emulator()->snapshot();
    TW_CHECK_EQ(snapshot.screen_width, 128);
    TW_CHECK_EQ(snapshot.screen_height, 64);
    TW_CHECK_EQ(window.screen()->pixelSize(), QSize(128, 64));
}

void test_the_window_wires_the_screen_and_controls() {
    announce("window: the screen and controls");
    Chip8Window window;
    TW_CHECK(window.loadSource(QString::fromLatin1(kAddProgram)));
    TW_CHECK(window.emulator()->loaded());

    TW_CHECK(window.findChild<QAction*>(QStringLiteral("chip8RunAction")) != nullptr);
    TW_CHECK(window.findChild<QAction*>(QStringLiteral("chip8StepAction")) != nullptr);
    TW_CHECK(window.findChild<QAction*>(QStringLiteral("chip8ResetAction")) != nullptr);
    TW_CHECK(window.findChild<QSpinBox*>(QStringLiteral("chip8Speed")) != nullptr);
    TW_CHECK(window.findChild<QTableWidget*>(QStringLiteral("chip8Registers")) != nullptr);

    // Q is CHIP-8 key 4, the same position the keyboard table documents.
    QKeyEvent press{QEvent::KeyPress, Qt::Key_Q, Qt::NoModifier};
    QApplication::sendEvent(window.screen(), &press);
    TW_CHECK((window.emulator()->keys() & (1U << 4)) != 0U);
    QKeyEvent release{QEvent::KeyRelease, Qt::Key_Q, Qt::NoModifier};
    QApplication::sendEvent(window.screen(), &release);
    TW_CHECK_EQ(window.emulator()->keys(), 0U);

    window.emulator()->advanceFrame();
    TW_CHECK(window.emulator()->snapshot().halted);
}

void test_the_window_opens_a_file() {
    announce("window: opening a file");
    tw::test::TempDir scratch;
    TW_CHECK(scratch.valid());
    if (!scratch.valid()) {
        return;
    }
    const QString path =
        scratch.write(QStringLiteral("demo.ch8"), QByteArray{"\x60\x0B\x00\x00", 4});
    TW_CHECK(!path.isEmpty());
    if (path.isEmpty()) {
        return;
    }
    Chip8Window window;
    TW_CHECK(window.openFile(path));
    window.emulator()->advanceFrame();
    TW_CHECK_EQ(static_cast<int>(window.emulator()->snapshot().v[0]), 11);
}

void test_the_window_starts_with_the_bundled_program() {
    announce("window: the bundled program");
    Chip8Window window;
    TW_CHECK(window.loadBundledProgram());
    TW_CHECK(window.emulator()->loaded());
    TW_CHECK(window.emulator()->name().endsWith(QStringLiteral("paint.ch8")));
    // The bundled demo starts itself: opening the tool shows a live cursor
    // waiting for a key, not a blank screen behind a Run button.
    TW_CHECK(window.emulator()->snapshot().running);

    auto* listing = window.findChild<QPlainTextEdit*>(QStringLiteral("chip8Listing"));
    TW_CHECK(listing != nullptr);
    if (listing != nullptr) {
        // The tab shows the program that is loaded, not a second copy.
        TW_CHECK(listing->toPlainText().contains(QStringLiteral("paint.ch8")));
    }
    TW_CHECK(window.findChild<QAction*>(QStringLiteral("chip8AssembleAction")) != nullptr);

    // One frame reaches `LD V0, K`: the bundled program is waiting for a key,
    // which is what makes it immediately interactive.
    window.emulator()->advanceFrame();
    TW_CHECK(window.emulator()->snapshot().waiting_for_key);
    window.emulator()->setKey(5, true);
    window.emulator()->advanceFrame();
    TW_CHECK(!window.emulator()->snapshot().waiting_for_key);
}

void test_the_listing_tab_assembles_and_runs() {
    announce("window: assemble from the listing tab");
    Chip8Window window;
    auto* listing = window.findChild<QPlainTextEdit*>(QStringLiteral("chip8Listing"));
    TW_CHECK(listing != nullptr);
    if (listing == nullptr) {
        return;
    }
    listing->setPlainText(QStringLiteral(
        "; typed into the emulator\n"
        "start: LD V0, 4\n"
        "       ADD V0, 3\n"
        "       HALT\n"));
    window.assembleListing();
    TW_CHECK(window.emulator()->loaded());
    TW_CHECK(window.emulator()->error().isEmpty());
    TW_CHECK(window.emulator()->snapshot().running);

    window.emulator()->advanceFrame();
    const Chip8Snapshot snapshot = window.emulator()->snapshot();
    TW_CHECK_EQ(static_cast<int>(snapshot.v[0]), 7);
    TW_CHECK(snapshot.halted);

    // A listing that does not assemble must not unload what is running.
    listing->setPlainText(QStringLiteral("start: FROB V0\n"));
    window.assembleListing();
    TW_CHECK(!window.emulator()->error().isEmpty());
    const Chip8Snapshot kept = window.emulator()->snapshot();
    TW_CHECK(kept.loaded);
    TW_CHECK_EQ(static_cast<int>(kept.v[0]), 7);
    TW_CHECK(kept.halted);

    // A later good listing clears the error again.
    listing->setPlainText(QStringLiteral("start: LD V0, 9\n       HALT\n"));
    window.assembleListing();
    TW_CHECK(window.emulator()->error().isEmpty());
    TW_CHECK(window.emulator()->snapshot().running);
}

}  // namespace

int main(int argc, char** argv) {
    // Before QApplication exists, or Qt tries to open a real display.
    qputenv("QT_QPA_PLATFORM", "offscreen");
    // The bundled-program test must find the checkout's Examples/ even when the
    // build tree lives somewhere else.
#ifndef TIME_WARP_TEST_EXAMPLES
#define TIME_WARP_TEST_EXAMPLES ""
#endif
    qputenv("TIME_WARP_EXAMPLES", TIME_WARP_TEST_EXAMPLES);
    QApplication app{argc, argv};

    test_a_listing_runs_and_steps();
    test_a_frame_runs_the_speed_and_ticks_the_timers();
    test_a_binary_rom_loads_byte_for_byte();
    test_a_bad_program_is_reported();
    test_run_pause_and_reset();
    test_speed_is_bounded();
    test_the_clock_runs_without_manual_frames();
    test_ld_key_waits_for_a_press();
    test_rewind_keeps_history_and_goes_back();
    test_resuming_from_the_past_forgets_the_future();
    test_the_window_offers_the_assistant();
    test_the_window_offers_the_rewind_strip();
    test_the_window_offers_the_rom_library();
    test_a_hires_program_reaches_the_screen();
    test_the_window_wires_the_screen_and_controls();
    test_the_window_opens_a_file();
    test_the_window_starts_with_the_bundled_program();
    test_the_listing_tab_assembles_and_runs();
    return tw::test::summary("chip8_emulator");
}
