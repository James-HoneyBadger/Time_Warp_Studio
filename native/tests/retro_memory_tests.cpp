// SPDX-License-Identifier: MIT
//
// Memory and the tube: the map, scrolling, wrapping, LOCATE, CLS, and the graphics
// characters a byte paints.

#include "retro/support.hpp"

namespace tw::retro {

void test_memory_map_is_the_machine() {
    timewarp::ui::HbMemory memory;
    // Free RAM answers what was put there, and nothing else does.
    TW_CHECK(memory.poke(0x1000, 0x41));
    TW_CHECK_EQ(static_cast<int>(memory.peek(0x1000)), 0x41);
    TW_CHECK_EQ(static_cast<int>(memory.peek(0x1001)), 0);

    // A cell is a byte in the screen area, and its ink lives in the attribute area.
    memory.setCell(0, 0, 'H');
    TW_CHECK_EQ(static_cast<int>(memory.peek(HbMemory::kScreenBase)), int{'H'});
    memory.setAttribute(0, 0, 0x0A, 0x01);
    TW_CHECK_EQ(static_cast<int>(memory.peek(HbMemory::kAttributeBase)), 0xA1);
    TW_CHECK_EQ(static_cast<int>(memory.inkAt(0, 0)), 0x0A);
    TW_CHECK_EQ(static_cast<int>(memory.paperAt(0, 0)), 0x01);

    // ROM refuses a write and keeps its contents; everything below it lands.
    TW_CHECK(!memory.writable(HbMemory::kRomBase));
    TW_CHECK(!memory.poke(HbMemory::kRomBase, 0x55));
    TW_CHECK(memory.writable(HbMemory::kRomBase - 1));
    TW_CHECK(!memory.writable(HbMemory::kSize));
    TW_CHECK(!memory.writable(-1));

    // The I/O page is registers, not storage: the cursor is one of them.
    memory.setCursor(7, 33);
    TW_CHECK_EQ(memory.cursorRow(), 7);
    TW_CHECK_EQ(memory.cursorColumn(), 33);
    TW_CHECK_EQ(static_cast<int>(memory.peek(HbMemory::kIoBase + HbMemory::kCursorRow)), 7);
    memory.tickFrame();
    TW_CHECK_EQ(memory.frame(), 1);
    memory.setBorder(4);
    TW_CHECK_EQ(static_cast<int>(memory.border()), 4);
}

// A scroll that only moved a paint offset would leave PEEK answering with what used
// to be on the screen -- the exact lie this class exists to end.

void test_scrolling_moves_memory_and_peek_agrees() {
    timewarp::ui::HbMemory memory;
    memory.setCell(0, 0, 'A');
    memory.setCell(1, 0, 'B');
    memory.setCell(HbMemory::kRows - 1, 0, 'Z');
    memory.scrollUp();

    TW_CHECK_EQ(memory.cell(0, 0), std::uint8_t{'B'});
    TW_CHECK_EQ(memory.cell(HbMemory::kRows - 2, 0), std::uint8_t{'Z'});
    TW_CHECK_EQ(memory.cell(HbMemory::kRows - 1, 0), std::uint8_t{' '});
    TW_CHECK_EQ(static_cast<int>(memory.peek(HbMemory::kScreenBase)), int{'B'});
    // And the machine leaves a flag a program can notice the scroll by.
    TW_CHECK(memory.scrolled());
    memory.clearScrolled();
    TW_CHECK(!memory.scrolled());
}


void test_writing_text_wraps_and_scrolls() {
    timewarp::ui::HbMemory memory;
    // 80 columns: the 81st character goes to the next row, at the same ink.
    QString long_line(81, QLatin1Char('x'));
    memory.setInk(0x0C);
    memory.writeText(long_line);
    TW_CHECK_EQ(memory.rowText(0).size(), 80);
    TW_CHECK_EQ(memory.cell(1, 0), std::uint8_t{'x'});
    TW_CHECK_EQ(memory.inkAt(1, 0), 0x0C);
    // The write left the cursor at the start of the following row.
    TW_CHECK_EQ(memory.cursorRow(), 2);
    TW_CHECK_EQ(memory.cursorColumn(), 0);

    // Writing the last row and then ending the line scrolls: the text moves up one
    // and the new bottom row is blank, which is what a teletype does.
    memory.setCursor(HbMemory::kRows - 1, 0);
    memory.writeText(QStringLiteral("LAST"));
    TW_CHECK_EQ(memory.rowText(HbMemory::kRows - 2), QStringLiteral("LAST"));
    TW_CHECK_EQ(memory.rowText(HbMemory::kRows - 1), QString{});
    TW_CHECK(memory.scrolled());

    memory.home();
    TW_CHECK_EQ(memory.cursorRow(), 0);
    TW_CHECK_EQ(memory.cursorColumn(), 0);
}


void test_memory_checksum_changes_when_a_byte_does() {
    timewarp::ui::HbMemory memory;
    const std::uint16_t before = memory.checksum();
    memory.poke(0x2000, 1);
    TW_CHECK(memory.checksum() != before);
    memory.clear();
    TW_CHECK_EQ(memory.checksum(), before);
}

// -- the screen is memory ---------------------------------------------------
//
// The claim of 20.1: a character POKEd into screen memory is on the tube, and
// `LOCATE` moves the cursor `PRINT` then writes at. Before it, POKE wrote a map
// nothing painted and LOCATE validated its arguments and returned.


void test_a_poked_character_is_on_the_screen() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("10 POKE 32768, 72"));
    machine.submitCommand(QStringLiteral("20 POKE 32769, 73"));
    machine.submitCommand(QStringLiteral("RUN"));
    TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 4000));

    // 0x8000 is row 0, column 0 of the tube, and PEEK reads the same bytes back.
    TW_CHECK(machine.memory().screenText().startsWith(QStringLiteral("HI")));
    TW_CHECK_EQ(static_cast<int>(machine.memory().peek(HbMemory::kScreenBase)), int{'H'});
    // Ink and paper are in the attribute page: the cell is not a colour, it is a nibble.
    TW_CHECK(machine.memory().inkAt(0, 0) != 0);
}


void test_locate_positions_the_print_that_follows() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("10 LOCATE 5, 10"));
    machine.submitCommand(QStringLiteral("20 PRINT \"HERE\""));
    machine.submitCommand(QStringLiteral("RUN"));
    TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 4000));

    // Row 5, column 10 is cell 4*80 + 9 in the screen area.
    const QString row = machine.memory().rowText(4);
    TW_CHECK(row.mid(9, 4) == QStringLiteral("HERE"));
    TW_CHECK_EQ(static_cast<int>(machine.memory().peek(HbMemory::kScreenBase + 4 * 80 + 9)),
                int{'H'});
    // Nothing was written on the row above it: the banner is there, "HERE" is not.
    TW_CHECK(!machine.memory().rowText(3).contains(QStringLiteral("HERE")));
}


void test_cls_clears_screen_memory() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("10 POKE 32768, 42"));
    machine.submitCommand(QStringLiteral("20 CLS"));
    machine.submitCommand(QStringLiteral("RUN"));
    TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 4000));
    // The POKEd character is gone. The machine still says what it did -- a *log* said
    // "Screen cleared" and a screen that said nothing would be indistinguishable from
    // one that crashed -- but the ink it cleared is not there.
    TW_CHECK(static_cast<int>(machine.memory().peek(HbMemory::kScreenBase)) != 42);
    TW_CHECK(!machine.memory().screenText().contains(QLatin1Char('*')));
}

// A POKE into ROM cannot land, and a program that tries has to be told: the silent
// failure is the one this project treats as the bug.

void test_a_poke_into_rom_is_refused_out_loud() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("10 POKE 61440, 1"));
    machine.submitCommand(QStringLiteral("RUN"));
    TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 4000));
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("is ROM")));
}

// Writing past the bottom of the tube scrolls *memory*: what PEEK answers afterwards
// is what is on the screen.

void test_printing_past_the_bottom_scrolls_the_screen() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("10 FOR I = 1 TO 30"));
    machine.submitCommand(QStringLiteral("20 PRINT I"));
    machine.submitCommand(QStringLiteral("30 NEXT I"));
    machine.submitCommand(QStringLiteral("RUN"));
    TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 6000));
    // Thirty numbers went past a twenty-five row tube, so the banner that was on row
    // zero has scrolled off it -- and every number is still in the transcript, which
    // is what a reader scrolls back through.
    TW_CHECK(machine.memory().rowText(0) != QStringLiteral("*** HB-1 ***"));
    const QString transcript = machine.consoleView()->toPlainText();
    TW_CHECK(transcript.contains(QStringLiteral("29")));
    TW_CHECK(transcript.contains(QStringLiteral("30")));
}

// The graphics characters are the reason a machine with only a text screen could
// draw a picture: code 128 is a solid block, and POKEing it darkens a whole cell.

void test_a_graphics_character_paints_its_cell() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("10 CLS"));
    machine.submitCommand(QStringLiteral("20 POKE 32768, 128"));
    machine.submitCommand(QStringLiteral("30 POKE 32769, 32"));
    machine.submitCommand(QStringLiteral("RUN"));
    TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 4000));

    HbTube* tube = machine.consoleView();
    TW_CHECK(tube != nullptr);
    tube->resize(tube->sizeHint());
    const QImage frame = tube->grab().toImage();
    const QRect block = tube->cellRect(0, 0).adjusted(1, 1, -1, -1);
    const QRect blank = tube->cellRect(0, 1).adjusted(1, 1, -1, -1);
    // The block's middle is ink; the space beside it is paper. A cell of a solid
    // block that came out empty would be a character the tube cannot draw, which is
    // what the sixteen graphics codes exist to prevent.
    TW_CHECK(frame.pixelColor(block.center()) != frame.pixelColor(blank.center()));
    TW_CHECK(frame.pixelColor(block.center()).lightness() >
             frame.pixelColor(blank.center()).lightness());
}

// -- the machine's own voice, its clock and its switch -----------------------
//
// Four things a machine does that a window cannot: it keeps time, it makes a noise
// when nothing asked it to, it counts its own memory at power-on, and it can be
// switched off.

}  // namespace tw::retro
