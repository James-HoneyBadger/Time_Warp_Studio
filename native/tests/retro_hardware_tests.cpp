// SPDX-License-Identifier: MIT
//
// The hardware: the voice, the latch, the power switch, the frame register and the
// self test.

#include "retro/support.hpp"

namespace tw::retro {

void test_the_machine_sounds_like_a_machine() {
    clearMachineSettings();
    RetroComputerWindow machine;
    MachineVoice* voice = machine.voice();
    TW_CHECK(voice != nullptr);
    // A cold boot chirps.
    TW_CHECK_EQ(voice->count(MachineVoice::Event::Boot), 1);
    // Typing clicks, once per line.
    machine.submitCommand(QStringLiteral("PRINT 1"));
    TW_CHECK_EQ(voice->count(MachineVoice::Event::Key), 1);
    // A disk word rattles.
    machine.submitCommand(QStringLiteral("SAVE \"NOISE\""));
    TW_CHECK_EQ(voice->count(MachineVoice::Event::Disk), 1);
    // And an error buzzes, which is the sound a reader of the period remembers as
    // *bad* -- and it is why the event is its own.
    machine.loadProgram(QStringLiteral("10 NOTACOMMAND"));
    machine.submitCommand(QStringLiteral("RUN"));
    TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 4000));
    TW_CHECK_EQ(voice->count(MachineVoice::Event::Error), 1);

    // Every event has a shape a test can see, and no event is silent.
    for (const MachineVoice::Event event :
         {MachineVoice::Event::Boot, MachineVoice::Event::Key, MachineVoice::Event::Disk,
          MachineVoice::Event::Error, MachineVoice::Event::Tape}) {
        TW_CHECK(!MachineVoice::tonesFor(event).empty());
    }
}

// Muting is a promise about sound, not about history: the machine still did the thing.

void test_a_muted_machine_is_silent_but_still_counts() {
    clearMachineSettings();
    RetroComputerWindow machine;
    MachineVoice* voice = machine.voice();
    RetroComputerSettings quiet = machine.settings();
    quiet.voice_muted = true;
    machine.setSettings(quiet);
    TW_CHECK(voice->muted());

    const int sounded_before = voice->sounded();
    const int boot_before = voice->count(MachineVoice::Event::Boot);
    machine.reboot();
    TW_CHECK_EQ(voice->sounded(), sounded_before);
    TW_CHECK_EQ(voice->count(MachineVoice::Event::Boot), boot_before + 1);

    // And again with the volume at zero: audible nowhere, counted everywhere.
    quiet.voice_muted = false;
    quiet.voice_volume = 0;
    machine.setSettings(quiet);
    voice->play(MachineVoice::Event::Key);
    TW_CHECK_EQ(voice->sounded(), sounded_before);
}

// The sound latch is a register: a program pokes a code and the machine plays it on
// the next frame. A machine with no audio still clears it, so a program is not left
// sounding the same note forever.

void test_poking_the_sound_latch_reaches_the_voice() {
    clearMachineSettings();
    RetroComputerWindow machine;
    TW_CHECK(machine.voice() != nullptr);
    const int before = machine.voice()->requested();
    machine.submitCommand(QStringLiteral("10 POKE 40965, 30"));
    machine.submitCommand(QStringLiteral("RUN"));
    TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 4000));
    // The clock ticks the latch; advance a frame by hand so the test does not sleep.
    machine.clock()->advance();
    TW_CHECK(machine.voice()->requested() > before);
    TW_CHECK_EQ(static_cast<int>(machine.memory().peek(HbMemory::kIoBase +
                                                      HbMemory::kSoundLatch)), 0);
}

// A self-test that prints "OK" without testing anything is the same lie as a
// hardcoded MEM line. This one walks the RAM and sums the ROM table, and says what
// it found.

void test_the_self_test_counts_the_memory_it_has() {
    clearMachineSettings();
    RetroComputerWindow machine;
    const QString boot = machine.consoleView()->toPlainText();
    TW_CHECK(boot.contains(QStringLiteral("HB-1 SELF TEST")));
    TW_CHECK(boot.contains(QStringLiteral("BYTES FREE")));
    TW_CHECK(boot.contains(QStringLiteral("RAM CHECK OK")));
    TW_CHECK(boot.contains(QStringLiteral("ROM CHECK OK (11 ROMS")));

    // Short and Off are promises about how much the machine says: neither can print a
    // self-test and still keep them.
    RetroComputerSettings short_banner = machine.settings();
    short_banner.banner = BootBanner::Short;
    machine.setSettings(short_banner);
    machine.reboot();
    TW_CHECK(!machine.consoleView()->toPlainText().contains(QStringLiteral("SELF TEST")));
}


void test_the_power_switch_turns_the_machine_off_and_on() {
    clearMachineSettings();
    RetroComputerWindow machine;
    TW_CHECK(machine.powered());
    const int boots = machine.voice()->count(MachineVoice::Event::Boot);

    machine.togglePower();
    TW_CHECK(!machine.powered());
    TW_CHECK(!machine.promptInput()->isEnabled());
    TW_CHECK(!machine.clock()->running());
    // Off is off: a line typed at a switched-off machine does nothing at all.
    const QString before = machine.consoleView()->toPlainText();
    machine.submitCommand(QStringLiteral("PRINT 1"));
    TW_CHECK_EQ(machine.consoleView()->toPlainText(), before);

    machine.togglePower();
    TW_CHECK(machine.powered());
    TW_CHECK(machine.promptInput()->isEnabled());
    TW_CHECK(machine.clock()->running());
    // Powering on is a cold boot, and a cold boot chirps.
    TW_CHECK_EQ(machine.voice()->count(MachineVoice::Event::Boot), boots + 1);
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("SELF TEST")));
}

// The clock is the machine's, and the frame register is how a program reads it.

void test_the_frame_register_counts_the_clock() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.clock()->stop();
    const std::uint16_t start = machine.memory().frame();
    machine.clock()->advance();
    machine.clock()->advance();
    machine.clock()->advance();
    TW_CHECK_EQ(machine.memory().frame(), static_cast<std::uint16_t>(start + 3));
}

// -- the BASIC line editor --------------------------------------------------
//
// A 1982 machine's BASIC is judged by its editor. These are the words the ROM
// had been missing, asserted through the prompt the way a user reaches them.

}  // namespace tw::retro
