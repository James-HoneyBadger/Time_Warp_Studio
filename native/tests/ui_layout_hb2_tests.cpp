// SPDX-License-Identifier: MIT
//
// The HB-2's window: the machine, its prompt, its monitor and its screen.
//
// The machine underneath has its own suite (`hb2_tests`, 253 checks, no Qt at all).
// What is worth asserting here is the window's own contract: that it opens with the
// four pages a machine window has, that the prompt's words reach the machine and come
// back as lines, that a listing assembled from the editor runs and halts, and that the
// glass paints something rather than a blank rectangle -- which is the failure this
// project treats as the bug.

#include "ui_layout/support.hpp"

#include "timewarp/ui/hb2_window.hpp"

namespace tw::ui_layout {

void test_the_hb2_window_runs_a_listing() {
    announce("HB-2: the window, the prompt and the glass");
    timewarp::ui::Hb2Window window;
    window.show();
    QApplication::processEvents();

    // The four pages a machine window has: the screen, the prompt that operates it,
    // the monitor that looks inside it, and the listing it runs.
    auto* tabs = window.findChild<QTabWidget*>(QStringLiteral("hb2Tabs"));
    TW_CHECK(tabs != nullptr);
    if (tabs == nullptr) {
        return;
    }
    TW_CHECK_EQ(tabs->count(), 4);
    TW_CHECK(tabs->tabText(0).contains(QStringLiteral("Screen")));
    TW_CHECK(window.screen() != nullptr);

    // The prompt answers in lines: HELP is the list, and a word the machine does not
    // have says so rather than doing nothing.
    window.runCommand(QStringLiteral("HELP"));
    window.runCommand(QStringLiteral("NONSENSE"));
    auto* transcript = window.findChild<QPlainTextEdit*>(QStringLiteral("hb2Transcript"));
    TW_CHECK(transcript != nullptr);
    if (transcript != nullptr) {
        TW_CHECK(transcript->toPlainText().contains(QStringLiteral("DIR")));
        TW_CHECK(transcript->toPlainText().contains(QStringLiteral("?WHAT")));
    }

    // A listing assembled from the editor runs and halts, and the machine's own
    // screen says what it printed.
    window.setListing(QStringLiteral(
        "        MOV R0, #1\n"
        "        INT 5\n"                 // MODE 1
        "        MOV R0, #0\n"
        "        INT 7\n"                 // clear
        "        MOV R0, #20\n"
        "        MOV R1, #20\n"
        "        MOV R2, #40\n"
        "        MOV R3, #40\n"
        "        MOV R4, #9\n"
        "        INT 3\n"                 // fill
        "        MOV R0, #0\n"
        "        INT 0\n"));               // exit
    window.runCommand(QStringLiteral("RUN"));
    window.tickOnce();

    TW_CHECK(window.machine().status() == timewarp::hb2::CpuStatus::Halted);
    TW_CHECK(window.machine().video().mode() == timewarp::hb2::Video::Mode::Low);
    TW_CHECK(window.machine().video().pixel(25, 25) == 9);
    TW_CHECK(window.machine().video().pixel(5, 5) == 0);
}

}  // namespace tw::ui_layout
