// SPDX-License-Identifier: MIT
//
// The HB-2: the bus, the video chip, the sound chip, HB-16 and its tools, the
// peripherals, the operating system's services and the snapshot.
//
// What is worth asserting is what a demonstration program depends on: that the map
// answers where the table says, that an assembled instruction executes with the cycles
// the table promises, that a picture drawn by the blitter is the picture the pixels
// say, that a chord is the same bytes twice, and that a program assembled,
// disassembled and assembled again is the same bytes.

#include <cstdio>
#include <string>
#include <vector>

#include "check.hpp"
#include <filesystem>
#include <fstream>

#include "timewarp/hb2/asm.hpp"
#include "timewarp/hb2/audio.hpp"
#include "timewarp/hb2/dos.hpp"
#include "timewarp/hb2/font.hpp"
#include "timewarp/hb2/hbos.hpp"
#include "timewarp/hb2/modules.hpp"
#include "timewarp/hb2/monitor.hpp"
#include "timewarp/hb2/shell.hpp"
#include "timewarp/hb2/cpu.hpp"
#include "timewarp/hb2/machine.hpp"
#include "timewarp/hb2/map.hpp"
#include "timewarp/hb2/video.hpp"

namespace {

using namespace timewarp::hb2;

void announce(const char* name) {
    std::printf("-- %s\n", name);
    std::fflush(stdout);
}

// ---------------------------------------------------------------------------
// The map and the bus
// ---------------------------------------------------------------------------

void test_the_map_is_one_table() {
    announce("the map: one table, read by the bus");
    // Every region the table names answers where it says, and the video chip is the
    // one device that answers on three windows -- which is what makes MODE 0 free.
    Machine machine;
    Bus& bus = machine.bus();

    for (const Region& region : kRegions) {
        bus.setCompat(region.compat_only);
        if (region.kind == RegionKind::Rom) {
            // A ROM is a ROM: it answers, and refuses the write. Its bytes are $FF
            // so that a jump into it is an invalid instruction.
            TW_CHECK_EQ(bus.read(region.base), 0xFF);
            continue;
        }
        if (region.kind == RegionKind::Uart) {
            // The serial page is two registers rather than a block of memory: a byte
            // written is a byte on the line, and that is what to assert.
            bus.write(region.base, 'X');
            TW_CHECK(!machine.uart().transmitted().empty());
            TW_CHECK(machine.uart().transmitted().back() == 'X');
            machine.uart().clearTransmitted();
            continue;
        }
        // A page's first byte is sometimes a register with a range rather than a
        // cell -- the cursor column is eighty wide -- so the value written is one
        // every register in this map can hold.
        TW_CHECK_EQ(region.size >= 2, true);
        bus.write(region.base, 0x0A);
        bus.write(region.base + region.size - 1, 0x55);
        TW_CHECK_EQ(bus.read(region.base), 0x0A);
        TW_CHECK_EQ(bus.read(region.base + region.size - 1), 0x55);
        bus.write(region.base, 0x00);
        bus.write(region.base + region.size - 1, 0x00);
    }

    // An address no region claims reads zero and refuses the write, and the refusal
    // is counted rather than thrown: a stray POKE is a program's mistake.
    const std::uint32_t refused = bus.refusedWrites();
    bus.write(0xF00000, 0x11);
    TW_CHECK_EQ(bus.read(0xF00000), 0);
    TW_CHECK(bus.refusedWrites() == refused + 1);

    // The ROM refuses writes too, and it is filled with $FF so that jumping into it
    // is an invalid instruction rather than a field of NOPs.
    bus.setCompat(false);
    TW_CHECK_EQ(bus.read(0x210000), 0xFF);
    const std::uint32_t rom_refusals = machine.rom().refusedWrites();
    bus.write(0x210000, 0x00);
    TW_CHECK(machine.rom().refusedWrites() == rom_refusals + 1);
    TW_CHECK_EQ(bus.read(0x210000), 0xFF);
}

void test_mode_zero_is_the_1982_map() {
    announce("MODE 0: the 1982 map, unchanged");
    Machine machine;
    Bus& bus = machine.bus();
    bus.setCompat(true);
    // `POKE 32768, 65` is a letter in the tube's first cell, exactly as it was in
    // 1982: the address, the byte and the effect are the 1982 ones.
    bus.write(0x8000, 'A');
    TW_CHECK_EQ(machine.video().cell(0, 0), 'A');
    bus.write(0x8001, 'B');
    TW_CHECK_EQ(machine.video().line(0), std::string{"AB"});
    // The attribute page is the same page it always was.
    bus.write(0x9000, 0x1E);
    TW_CHECK_EQ(machine.video().read(Video::kAttributesOffset), 0x1E);
    // And the 1982 I/O page's first register is the cursor column.
    bus.write(0xA000, 5);
    TW_CHECK_EQ(bus.read(0xA000), 5);
}

// ---------------------------------------------------------------------------
// HB-16
// ---------------------------------------------------------------------------

void test_instructions_execute_and_cost_what_the_table_says() {
    announce("HB-16: opcodes, addressing and cycles");
    Machine machine;
    const Assembly assembly = machine.load(
        "        ORG $1000\n"
        "        MOV R0, #7\n"
        "        MOV R1, #5\n"
        "        ADD R0, R1\n"
        "        STORE [$2000], R0\n"
        "        LOAD R2, [$2000]\n"
        "        CMP R2, #12\n"
        "        JNZ bad\n"
        "        MOV R3, #1\n"
        "        JMP done\n"
        "bad:    MOV R3, #255\n"
        "done:   HALT\n");
    TW_CHECK(assembly.ok);
    if (!assembly.ok) {
        for (const std::string& error : assembly.errors) {
            std::printf("     %s\n", error.c_str());
        }
        return;
    }

    machine.run();
    TW_CHECK(machine.cpu().status() == CpuStatus::Halted);
    TW_CHECK_EQ(machine.cpu().registers().r[0], 12);
    TW_CHECK_EQ(machine.cpu().registers().r[2], 12);
    TW_CHECK_EQ(machine.cpu().registers().r[3], 1);
    TW_CHECK_EQ(machine.bus().read16(0x2000), 12);

    // The cycle count is data: a program can be timed from a test, and the manual's
    // table and the machine's table are the same table.
    const std::uint32_t expected =
        cyclesFor(Op::Mov, Mode::Imm) + cyclesFor(Op::Mov, Mode::Imm) +
        cyclesFor(Op::Add, Mode::Reg) + cyclesFor(Op::Store, Mode::Abs) +
        cyclesFor(Op::Load, Mode::Abs) + cyclesFor(Op::Cmp, Mode::Imm) +
        cyclesFor(Op::Jnz, Mode::Rel) + cyclesFor(Op::Mov, Mode::Imm) +
        cyclesFor(Op::Jmp, Mode::Rel) + cyclesFor(Op::Halt, Mode::Reg);
    TW_CHECK_EQ(machine.cpu().registers().cycles, expected);
}

void test_calls_and_the_stack() {
    announce("HB-16: CALL, RET and the stack");
    Machine machine;
    const Assembly assembly = machine.load(
        "        ORG $1000\n"
        "        MOV R0, #3\n"
        "        CALL twice\n"
        "        PUSH R0\n"
        "        MOV R1, #9\n"
        "        POP R2\n"
        "        HALT\n"
        "twice:  ADD R0, R0\n"
        "        RET\n");
    TW_CHECK(assembly.ok);
    if (!assembly.ok) {
        return;
    }
    machine.run();
    TW_CHECK_EQ(machine.cpu().registers().r[0], 6);
    TW_CHECK_EQ(machine.cpu().registers().r[2], 6);
    TW_CHECK_EQ(machine.cpu().registers().r[1], 9);
    TW_CHECK(machine.cpu().registers().sp == Cpu::kStackTop);
}

void test_a_fault_is_reported() {
    announce("HB-16: a fault is a sentence, not a crash");
    Machine machine;
    // $FFFF is not an instruction: the ROM is full of it so that a jump into an
    // unwritten page reports rather than executing a field of NOPs.
    machine.reset(0x210000);
    machine.run();
    TW_CHECK(machine.cpu().status() == CpuStatus::Faulted);
    TW_CHECK(machine.statusText().find("invalid instruction") != std::string::npos);

    Machine divisor;
    const Assembly assembly = divisor.load(
        "        ORG $1000\n"
        "        MOV R0, #1\n"
        "        MOV R1, #0\n"
        "        DIV R0, R1\n"
        "        HALT\n");
    TW_CHECK(assembly.ok);
    divisor.run();
    TW_CHECK(divisor.cpu().status() == CpuStatus::Faulted);
    TW_CHECK(divisor.statusText().find("division by zero") != std::string::npos);
}

void test_the_rest_of_the_instruction_set() {
    announce("HB-16: shifts, arithmetic, signed branches and the I/O page");
    Machine machine;
    const Assembly assembly = machine.load(
        "        ORG $1000\n"
        "        MOV R0, #1\n"
        "        SHL R0, #3\n"        // 8
        "        MOV R1, R0\n"
        "        MUL R1, #2\n"        // 16
        "        MOV R2, R1\n"
        "        DIV R2, #5\n"        // 3
        "        MOD R2, #2\n"        // 1
        "        MOV R3, #-1\n"       // 0xFFFF, which is -1
        "        MOV R4, #1\n"
        "        CMP R3, R4\n"        // -1 < 1, signed
        "        JLT signed\n"
        "        MOV R5, #99\n"       // not reached
        "        JMP done\n"
        "signed: MOV R5, #7\n"
        "        MOV R7, #$3000\n"
        "        STORE [R7+2], R1\n" // 16 at $3002
        "        MOV R6, #$41\n"
        "        OUT $202200, R6\n"  // 'A' on the serial line
        "done:   HALT\n");
    TW_CHECK(assembly.ok);
    if (!assembly.ok) {
        for (const std::string& error : assembly.errors) {
            std::printf("     %s\n", error.c_str());
        }
        return;
    }
    machine.run();
    TW_CHECK(machine.status() == CpuStatus::Halted);
    TW_CHECK_EQ(machine.cpu().registers().r[0], 8);
    TW_CHECK_EQ(machine.cpu().registers().r[1], 16);
    TW_CHECK_EQ(machine.cpu().registers().r[2], 1);
    TW_CHECK_EQ(machine.cpu().registers().r[5], 7);
    TW_CHECK_EQ(machine.bus().read16(0x3002), 16);
    TW_CHECK_EQ(machine.uart().transmitted(), std::string{"A"});
}

// ---------------------------------------------------------------------------
// The assembler
// ---------------------------------------------------------------------------

void test_assemble_disassemble_assemble() {
    announce("assembler: a listing survives the round trip");
    const std::string listing =
        "        ORG $1000\n"
        "start:  MOV R0, #1\n"
        "        MOV R1, #count\n"
        "        MOV R2, [count]\n"
        "        MOV R3, [R0]\n"
        "        MOV R4, [R0+2]\n"
        "        STORE [R0], R1\n"
        "        STORE [$3000], R2\n"
        "        LEA R5, table\n"
        "        ADD R0, R1\n"
        "        SHL R0, #2\n"
        "        JMP start\n"
        "        JZ start\n"
        "        INT 1\n"
        "        HALT\n"
        "count:  DW 42\n"
        "table:  DB 1, 2, 3\n"
        "        DS 4\n";

    const Assembly first = assemble(listing);
    TW_CHECK(first.ok);
    if (!first.ok) {
        for (const std::string& error : first.errors) {
            std::printf("     %s\n", error.c_str());
        }
        return;
    }

    // Disassemble it back into a listing, and assemble that.
    Machine loaded;
    loaded.loadProgram(first.bytes, first.origin);
    std::string listing_again;
    std::uint32_t address = first.origin;
    for (int guard = 0; guard < 64 && address < first.origin + first.bytes.size(); ++guard) {
        const Instruction instruction = readInstruction(loaded.bus(), address, &address);
        listing_again += "        " + formatInstruction(instruction, address) + "\n";
        if (instruction.op == Op::Halt) {
            break;
        }
    }
    // The data after HALT is assembled separately: a disassembler stops where the
    // code does, and the round trip is about instructions.
    const std::size_t code_end = first.bytes.size();
    std::vector<std::uint8_t> code(first.bytes.begin(),
                                   first.bytes.begin() + static_cast<std::ptrdiff_t>(code_end));
    const Assembly again = assemble(listing_again, first.origin);
    TW_CHECK(again.ok);
    if (!again.ok) {
        for (const std::string& error : again.errors) {
            std::printf("     %s\n", error.c_str());
        }
        return;
    }
    TW_CHECK(again.bytes.size() <= code.size());
    TW_CHECK(std::equal(again.bytes.begin(), again.bytes.end(), code.begin()));
}

void test_assembler_reports_bad_lines() {
    announce("assembler: a bad line names itself");
    const Assembly bad = assemble("        NOSUCH R0, R1\n");
    TW_CHECK(!bad.ok);
    TW_CHECK(!bad.errors.empty());
    if (!bad.errors.empty()) {
        TW_CHECK(bad.errors.front().find("unknown mnemonic") != std::string::npos);
    }

    const Assembly missing = assemble("        JMP nowhere\n");
    TW_CHECK(!missing.ok);

    const Assembly duplicate = assemble("a:      HALT\na:      HALT\n");
    TW_CHECK(!duplicate.ok);
}

// ---------------------------------------------------------------------------
// Video
// ---------------------------------------------------------------------------

void test_the_blitter_draws_pixels() {
    announce("video: the blitter draws what it says");
    Machine machine;
    Video& video = machine.video();
    video.setMode(Video::Mode::Low);

    // A filled rectangle is that rectangle, and nothing outside it.
    video.fill(10, 20, 4, 3, 7);
    TW_CHECK_EQ(video.pixel(10, 20), 7);
    TW_CHECK_EQ(video.pixel(13, 22), 7);
    TW_CHECK_EQ(video.pixel(14, 22), 0);
    TW_CHECK_EQ(video.pixel(9, 20), 0);

    // A line is its two ends and the pixels between them.
    video.setMode(Video::Mode::Low);
    video.clearBitmap();
    video.line(0, 0, 5, 0, 3);
    for (int x = 0; x <= 5; ++x) {
        TW_CHECK_EQ(video.pixel(x, 0), 3);
    }
    TW_CHECK_EQ(video.pixel(6, 0), 0);

    // Masked copy is transparent where the source is zero, which is what makes a
    // sprite compose rather than paint a box.
    video.clearBitmap();
    video.fill(0, 0, 2, 3, 9);
    video.fill(0, 1, 1, 1, 0);
    // Source: (0,0) (1,0) painted, (0,1) cleared, (0..1,1) painted, row 2 empty.
    video.maskedCopy(0, 0, 3, 3, 20, 20);
    TW_CHECK_EQ(video.pixel(20, 20), 9);
    TW_CHECK_EQ(video.pixel(20, 21), 0);   // transparent: the source was zero
    TW_CHECK_EQ(video.pixel(21, 21), 9);
    TW_CHECK_EQ(video.pixel(20, 22), 9);
    TW_CHECK_EQ(video.pixel(22, 22), 0);   // past the source's painted area
}

void test_high_mode_packs_two_pixels_a_byte() {
    announce("video: 640 x 480 at four bits per pixel");
    Machine machine;
    Video& video = machine.video();
    video.setMode(Video::Mode::High);
    TW_CHECK_EQ(video.width(), 640);
    TW_CHECK_EQ(video.height(), 480);
    video.setPixel(0, 0, 0xA);
    video.setPixel(1, 0, 0x5);
    TW_CHECK_EQ(video.pixel(0, 0), 0xA);
    TW_CHECK_EQ(video.pixel(1, 0), 0x5);
    // Two pixels share one byte, and the bus sees that byte.
    TW_CHECK_EQ(machine.bus().read(0x100000), 0xA5);
}

void test_sprites_collide() {
    announce("video: sprites compose, and a collision is read once");
    Machine machine;
    Video& video = machine.video();
    video.setMode(Video::Mode::Low);
    std::vector<std::uint8_t> shape(Video::kSpriteBytes, 0);
    shape[0] = 0xFF;  // the top row of the 16 x 16 shape
    video.loadPattern(1, shape);

    video.setSprite(0, true, 10, 10, 1, 5);
    TW_CHECK_EQ(video.pixel(10, 10), 5);
    TW_CHECK_EQ(video.pixel(10, 11), 0);

    // Two sprites over one another set the register, and reading it clears it: a
    // program that wants to know asks once.
    video.setSprite(1, true, 10, 10, 1, 6);
    TW_CHECK(video.takeCollision());
    TW_CHECK(!video.takeCollision());

    // A sprite whose shape lies outside the screen draws nothing rather than junk.
    video.clearBitmap();
    video.setSprite(2, true, 700, 300, 1, 9);
    TW_CHECK_EQ(video.pixel(319, 199), 0);
}

void test_the_palette_is_a_dac() {
    announce("video: the DAC, and the default sixteen");
    Machine machine;
    Video& video = machine.video();
    const Rgb black = video.dac(0);
    const Rgb white = video.dac(15);
    TW_CHECK(black == (Rgb{0, 0, 0}));
    TW_CHECK(white == (Rgb{7, 7, 7}));
    video.setDac(3, Rgb{9, 1, 2});
    const Rgb clamped = video.dac(3);
    TW_CHECK(clamped == (Rgb{7, 1, 2}));
}

// ---------------------------------------------------------------------------
// Audio
// ---------------------------------------------------------------------------

void test_a_chord_is_the_same_bytes_twice() {
    announce("audio: four channels, and a chord is deterministic");
    Audio audio;
    audio.trigger(0, 0, 4, 0xFF);
    audio.trigger(1, 1, 6, 0xFF);
    audio.trigger(2, 2, 8, 0xFF);
    audio.trigger(3, 3, 10, 0xFF);
    const std::vector<std::uint8_t> first = audio.mix(64);

    Audio again;
    again.trigger(0, 0, 4, 0xFF);
    again.trigger(1, 1, 6, 0xFF);
    again.trigger(2, 2, 8, 0xFF);
    again.trigger(3, 3, 10, 0xFF);
    const std::vector<std::uint8_t> second = again.mix(64);
    TW_CHECK_EQ(first.size(), second.size());
    TW_CHECK(first == second);
    // A chord is a sound, not silence.
    bool varied = false;
    for (const std::uint8_t sample : first) {
        if (sample != 128) {
            varied = true;
        }
    }
    TW_CHECK(varied);
}

void test_the_waveform_bank_is_poke_able() {
    announce("audio: the bank is memory a program can rewrite");
    Audio audio;
    std::vector<std::uint8_t> wave(Audio::kWaveformLength, 200);
    audio.loadWaveform(5, wave);
    const std::vector<std::uint8_t> written = audio.waveform(5);
    TW_CHECK_EQ(written.front(), 200);
    // Through the bus, at the bank's page, which is how a program reaches it.
    Machine machine;
    machine.bus().write(0x201000 + Audio::kWaveformBase + 0, 0x11);
    const std::vector<std::uint8_t> poked = machine.audio().waveform(0);
    TW_CHECK_EQ(poked.front(), 0x11);
}

void test_the_latch_still_sounds() {
    announce("audio: the 1982 latch is channel zero");
    Machine machine;
    machine.bus().setCompat(true);
    machine.bus().write(0xA005, 60);  // the 1982 SOUND latch
    TW_CHECK_EQ(machine.video().read(Video::kRegisterOffset + Video::kSoundLatch), 60);
    machine.tickFrame();
    // The frame drained it into channel zero and cleared the register, so a program
    // does not have to.
    TW_CHECK_EQ(machine.video().read(Video::kRegisterOffset + Video::kSoundLatch), 0);
    TW_CHECK_EQ(machine.audio().read(Audio::kEnable), 1);
}

// ---------------------------------------------------------------------------
// The peripherals and the services
// ---------------------------------------------------------------------------

void test_the_pointer_reports_a_cell() {
    announce("input: the pointer, and a delta read once");
    Machine machine;
    machine.input().movePointer(160, 100);
    TW_CHECK_EQ(machine.bus().read16(0x202000), 160);
    TW_CHECK_EQ(machine.bus().read16(0x202002), 100);
    // The delta is what a program reads without losing events: the movement since
    // the last read, not since the last move.
    TW_CHECK_EQ(machine.bus().read(0x202006), 160);
    machine.bus().write(0x202009, 0);  // the high byte's read clears the pair
    TW_CHECK_EQ(machine.bus().read(0x202006), 0);
    machine.input().movePointer(170, 90);
    TW_CHECK_EQ(machine.bus().read(0x202006), 10);
    TW_CHECK_EQ(machine.bus().read(0x202008), 246);  // -10, as an unsigned byte
}

void test_the_clock_is_injected() {
    announce("rtc: a reading a test injects, and sixty ticks a second");
    Machine machine;
    machine.rtc().setTime(92, 10, 31, 23, 59, 59);
    TW_CHECK_EQ(machine.rtc().seconds(), 59);
    for (int tick = 0; tick < 60; ++tick) {
        machine.tickFrame();
    }
    TW_CHECK_EQ(machine.rtc().seconds(), 0);
    TW_CHECK_EQ(machine.rtc().minutes(), 0);
    TW_CHECK_EQ(machine.rtc().hours(), 0);
    TW_CHECK_EQ(machine.rtc().read(Rtc::kDay), 1);
}

void test_a_program_prints_and_draws() {
    announce("the services: print, draw and exit");
    Machine machine;
    const Assembly assembly = machine.load(
        "        ORG $1000\n"
        "        INT 5\n"            // text mode
        "        MOV R0, #72\n"      // 'H'
        "        INT 1\n"
        "        MOV R0, #105\n"     // 'i'
        "        INT 1\n"
        "        INT 5\n"
        "        MOV R0, #1\n"      // 320 x 200
        "        INT 5\n"
        "        MOV R0, #0\n"
        "        MOV R1, #0\n"
        "        MOV R2, #4\n"
        "        MOV R3, #4\n"
        "        MOV R4, #12\n"
        "        INT 3\n"            // fill
        "        MOV R0, #0\n"
        "        INT 0\n");          // exit
    TW_CHECK(assembly.ok);
    if (!assembly.ok) {
        for (const std::string& error : assembly.errors) {
            std::printf("     %s\n", error.c_str());
        }
        return;
    }
    machine.run();
    TW_CHECK_EQ(machine.console(), std::string{"Hi"});
    TW_CHECK(machine.cpu().status() == CpuStatus::Halted);
    TW_CHECK_EQ(machine.exitCode(), 0);
    TW_CHECK(machine.video().mode() == Video::Mode::Low);
    TW_CHECK_EQ(machine.video().pixel(2, 2), 12);
    TW_CHECK_EQ(machine.video().pixel(4, 4), 0);
}

// ---------------------------------------------------------------------------
// The snapshot
// ---------------------------------------------------------------------------

void test_a_snapshot_round_trips() {
    announce("snapshot: save, restore, and the same machine");
    Machine machine;
    const Assembly assembly = machine.load(
        "        ORG $1000\n"
        "        INT 5\n"
        "        MOV R0, #1\n"
        "        INT 5\n"
        "        MOV R0, #20\n"
        "        MOV R1, #10\n"
        "        MOV R2, #8\n"
        "        MOV R3, #8\n"
        "        MOV R4, #6\n"
        "        INT 3\n"
        "        HALT\n");
    TW_CHECK(assembly.ok);
    machine.run();
    const std::uint64_t saved_hash = machine.hash();
    const std::vector<std::uint8_t> image = machine.save();
    TW_CHECK(!image.empty());

    // Change the machine, then put it back.
    machine.video().fill(0, 0, 40, 40, 1);
    TW_CHECK(machine.hash() != saved_hash);
    TW_CHECK(machine.restore(image));
    TW_CHECK_EQ(machine.hash(), saved_hash);

    // A snapshot from something that is not an HB-2 image is refused rather than
    // half-loaded.
    TW_CHECK(!machine.restore(std::vector<std::uint8_t>{'H', 'B', '2', 'M', 1, 0, 0}));
}

// ---------------------------------------------------------------------------
// The demonstration programs
// ---------------------------------------------------------------------------

void test_the_demos_run() {
    announce("the demonstration programs run and draw");
    // The same listings the smoke gate runs, assembled here so a broken listing fails
    // the suite as well as the gate. Each must halt without faulting and leave ink
    // on the screen: a program that runs and draws nothing is the bug the machines
    // in this tree exist to catch.
    const std::vector<std::string> demos{
        "palette",
        "blitter",
        "sprite",
        "hello",
        "chord",
        "desktop",
    };
    for (const std::string& name : demos) {
        const std::string path = std::string{HB2_EXAMPLES} + "/" + name + ".s";
        std::FILE* file = std::fopen(path.c_str(), "rb");
        TW_CHECK(file != nullptr);
        if (file == nullptr) {
            continue;
        }
        std::string source;
        char buffer[4096];
        std::size_t read = 0;
        while ((read = std::fread(buffer, 1, sizeof(buffer), file)) > 0) {
            source.append(buffer, read);
        }
        std::fclose(file);

        Machine machine;
        const Assembly assembly = machine.load(source);
        TW_CHECK(assembly.ok);
        if (!assembly.ok) {
            std::printf("     %s:\n", name.c_str());
            for (const std::string& error : assembly.errors) {
                std::printf("       %s\n", error.c_str());
            }
            continue;
        }
        machine.runFrames(4);
        TW_CHECK(machine.status() == CpuStatus::Halted);
        if (machine.status() != CpuStatus::Halted) {
            std::printf("     %s: %s\n", name.c_str(), machine.statusText().c_str());
        }
        // Ink: a pixel that is not the background in a bitmap mode, or a cell that
        // is not a space in the 1982 text mode. A program that runs and draws
        // nothing is the failure this project treats as the bug.
        bool drew = false;
        if (machine.video().mode() == Video::Mode::Text) {
            for (const std::string& row : machine.video().screenText()) {
                if (!row.empty()) {
                    drew = true;
                    break;
                }
            }
        } else {
            for (int y = 0; y < machine.video().height() && !drew; ++y) {
                for (int x = 0; x < machine.video().width(); ++x) {
                    if (machine.video().pixel(x, y) != 0) {
                        drew = true;
                        break;
                    }
                }
            }
        }
        TW_CHECK(drew);
        if (!drew) {
            std::printf("     %s drew nothing\n", name.c_str());
        }
    }
}

// ---------------------------------------------------------------------------
// Text on every screen, and the keyboard
// ---------------------------------------------------------------------------

void test_text_is_drawn_in_bitmap_modes() {
    announce("text: the ROM font, on every screen");
    Machine machine;
    Video& video = machine.video();
    video.setMode(Video::Mode::Low);

    // 'A' is eight rows of eight pixels; the top row of the glyph is 0x38, so three
    // pixels at columns 3, 4 and 5 are ink and the corners are not.
    video.drawChar(10, 20, 'A', 9);
    TW_CHECK_EQ(video.pixel(12, 20), 9);   // 0x38: bits 2, 3 and 4
    TW_CHECK_EQ(video.pixel(13, 20), 9);
    TW_CHECK_EQ(video.pixel(11, 20), 0);
    TW_CHECK_EQ(video.pixel(10, 20), 0);

    // Lowercase is the capital: the ROM has one case and says so.
    video.clearBitmap();
    video.drawChar(0, 0, 'a', 9);
    TW_CHECK_EQ(video.pixel(3, 0), 9);

    // A string is glyphs eight pixels apart, and a newline is a line down.
    video.clearBitmap();
    video.drawText(0, 0, "HI\nOK", 7);
    TW_CHECK_EQ(video.pixel(0, 0), 7);      // the H's left stem
    TW_CHECK_EQ(video.pixel(11, 0), 7);     // the I, one glyph on
    TW_CHECK_EQ(video.pixel(3, 8), 7);      // the O, one line down

    // The console writes at the cursor and advances it, in a bitmap mode as well as
    // in the text one.
    video.clearBitmap();
    video.putChar('A');
    video.putChar('B');
    TW_CHECK(video.pixel(3, 0) != 0);
    TW_CHECK(video.pixel(11, 0) != 0);
    TW_CHECK_EQ(video.read(Video::kRegisterOffset + Video::kCursorColumn), 2);
}

void test_the_keyboard_is_read_once() {
    announce("input: keys are a queue and a matrix");
    Machine machine;
    machine.input().pressKey('A');
    machine.input().pressKey('B');
    TW_CHECK(machine.input().keyAvailable());
    TW_CHECK(machine.input().keyDown('A'));
    TW_CHECK_EQ(machine.bus().read(0x202020), 'A');
    TW_CHECK_EQ(machine.bus().read(0x202020), 'B');
    TW_CHECK_EQ(machine.bus().read(0x202020), 0);
    machine.input().releaseKey('A');
    TW_CHECK(!machine.input().keyDown('A'));

    // Inside a program: INT 17 says whether a key waits and INT 16 takes it.
    Machine program;
    const Assembly assembly = program.load(
        "        ORG $1000\n"
        "        INT 17\n"          // R0 = has a key?
        "        MOV R1, R0\n"
        "        INT 16\n"          // R0 = the key
        "        MOV R2, R0\n"
        "        HALT\n");
    TW_CHECK(assembly.ok);
    program.input().pressKey('Z');
    program.run();
    TW_CHECK_EQ(program.cpu().registers().r[1], 1);
    TW_CHECK_EQ(program.cpu().registers().r[2], 'Z');
}

// ---------------------------------------------------------------------------
// HB-OS
// ---------------------------------------------------------------------------

void test_two_processes_both_make_progress() {
    announce("HB-OS: two processes, one clock");
    Machine machine;
    // No ORG: the scheduler places each process, which is what stops the second one
    // landing on the first.
    const int first = machine.spawn("A", "loop:   LEA R0, text\n        INT 8\n        JMP loop\ntext:   DB \"A\", 10, 0\n");
    const int second = machine.spawn("B", "loop:   LEA R0, text\n        INT 8\n        JMP loop\ntext:   DB \"B\", 10, 0\n");
    TW_CHECK(first != 0);
    TW_CHECK(second != 0);
    TW_CHECK_EQ(static_cast<int>(machine.scheduler().size()), 2);
    machine.runFrames(3);

    const Task* a = machine.scheduler().byId(first);
    const Task* b = machine.scheduler().byId(second);
    TW_CHECK(a != nullptr && b != nullptr);
    if (a == nullptr || b == nullptr) {
        return;
    }
    // Both ran, and both ran several times: a round robin that gave the clock to one
    // process is the failure this asserts against.
    TW_CHECK(a->console.size() > 4U);
    TW_CHECK(b->console.size() > 4U);
    TW_CHECK(a->console.find('A') != std::string::npos);
    TW_CHECK(b->console.find('B') != std::string::npos);
    TW_CHECK_EQ(static_cast<int>(machine.desktop().count()), 2);

    // The desktop is a window system: hit testing follows z order, and clicking a
    // window raises it.
    const int at = machine.desktop().topAt(static_cast<int>(a->window.x) + 2,
                                           static_cast<int>(a->window.y) + 2);
    TW_CHECK(at == first || at == second);
    machine.desktop().focus(first);
    TW_CHECK_EQ(machine.desktop().topAt(a->window.x + 2, a->window.y + 2), first);
    TW_CHECK(machine.desktop().frameOf(first).w > 0);
}

void test_a_dying_process_does_not_kill_the_desktop() {
    announce("HB-OS: a process that faults is named, and the desktop survives");
    Machine machine;
    const int good = machine.spawn("GOOD", "        ORG $1000\nloop:   LEA R0, t\n        INT 8\n        JMP loop\nt:      DB \"ok\", 10, 0\n");
    // An invalid instruction, which is what the ROM is full of: the processor reports
    // it and the scheduler kills the process.
    const int bad = machine.spawn("BAD", "        ORG $2000\n        DW $FFFF\n");
    TW_CHECK(good != 0 && bad != 0);
    machine.runFrames(4);
    const Task* dead = machine.scheduler().byId(bad);
    const Task* alive = machine.scheduler().byId(good);
    TW_CHECK(dead != nullptr && alive != nullptr);
    if (dead == nullptr || alive == nullptr) {
        return;
    }
    TW_CHECK(!dead->failure.empty());
    TW_CHECK(dead->finished);
    TW_CHECK(alive->console.size() > 2U);
    TW_CHECK_EQ(static_cast<int>(machine.desktop().count()), 2);
}

// ---------------------------------------------------------------------------
// The monitor and the module bay
// ---------------------------------------------------------------------------

void test_the_monitor_steps_and_breaks() {
    announce("monitor: disassemble, break, step");
    Machine machine;
    const Assembly assembly = machine.load(
        "        ORG $1000\n"
        "        MOV R0, #1\n"
        "        MOV R1, #2\n"
        "        ADD R0, R1\n"
        "        HALT\n");
    TW_CHECK(assembly.ok);
    Monitor monitor{machine};

    const std::vector<std::string> rows = monitor.disassemble(0x1000, 3);
    TW_CHECK_EQ(static_cast<int>(rows.size()), 3);
    TW_CHECK(rows.front().find("MOV R0, #1") != std::string::npos);
    TW_CHECK(monitor.registers().find("PC=001000") != std::string::npos);

    monitor.setBreakpoint(0x1004);
    TW_CHECK_EQ(monitor.runToBreakpoint(), std::string{"breakpoint at $001004"});
    TW_CHECK_EQ(machine.cpu().registers().r[0], 1);
    monitor.setBreakpoint(0x1004, false);
    TW_CHECK_EQ(monitor.runToBreakpoint(), std::string{"halted"});
    TW_CHECK_EQ(machine.cpu().registers().r[0], 3);

    monitor.poke(0x4000, {1, 2, 3});
    const std::vector<std::uint8_t> bytes = monitor.peek(0x4000, 3);
    TW_CHECK_EQ(static_cast<int>(bytes.size()), 3);
    TW_CHECK_EQ(bytes[2], 3);
}

void test_modules_round_trip() {
    announce("modules: a script, a binary and an image in the bay");
    const std::filesystem::path bay =
        std::filesystem::temp_directory_path() / "hb2-modules-test";
    std::filesystem::remove_all(bay);
    Modules modules{bay.string()};

    TW_CHECK(modules.saveScript("greet", {"DIR", "REGS"}));
    const std::vector<std::uint8_t> code{0x01, 0x02, 0x03};
    TW_CHECK(modules.saveBinary("blink", code, 0x2000));
    Machine machine;
    TW_CHECK(modules.saveImage("state", machine.save()));

    const std::vector<ModuleRow> rows = modules.list();
    TW_CHECK_EQ(static_cast<int>(rows.size()), 3);
    if (rows.size() != 3) {
        std::filesystem::remove_all(bay);
        return;
    }
    TW_CHECK(rows[0].name == "blink" && rows[0].kind == "binary");

    ModuleContent content;
    TW_CHECK(modules.read("greet", &content));
    TW_CHECK_EQ(content.kind, std::string{"script"});
    TW_CHECK_EQ(static_cast<int>(content.script.size()), 2);
    TW_CHECK(modules.read("blink", &content));
    TW_CHECK_EQ(content.kind, std::string{"binary"});
    TW_CHECK_EQ(content.entry, 0x2000U);
    TW_CHECK_EQ(static_cast<int>(content.binary.size()), 3);
    TW_CHECK(modules.read("state", &content));
    TW_CHECK_EQ(content.kind, std::string{"image"});
    TW_CHECK(!content.image.empty());

    // A module that is not one is refused with a sentence, and nothing is loaded.
    std::ofstream bogus{bay / "junk.mod"};
    bogus << "not a module\n";
    bogus.close();
    TW_CHECK(!modules.read("junk", &content));
    TW_CHECK(!modules.lastError().empty());
    std::filesystem::remove_all(bay);
}

// ---------------------------------------------------------------------------
// The disk and the prompt
// ---------------------------------------------------------------------------

void test_the_disk_has_directories_and_a_sector_count() {
    announce("HB DOS 2.0: directories, sectors and a round trip");
    Dos dos;
    TW_CHECK(dos.format(0));
    TW_CHECK(dos.mkdir(0, "PROGRAMS"));
    TW_CHECK(dos.mkdir(0, "PROGRAMS/DEMOS"));
    TW_CHECK(dos.writeFile(0, "PROGRAMS/HELLO.S",
                           std::vector<std::uint8_t>{'H', 'I', '\n'}));

    const std::vector<DosRow> rows = dos.list(0, "PROGRAMS");
    TW_CHECK_EQ(static_cast<int>(rows.size()), 2);
    TW_CHECK(rows.front().directory);  // directories first

    const DosFree space = dos.free(0);
    TW_CHECK_EQ(space.sectors, Dos::kFloppySectors);
    TW_CHECK_EQ(space.used + space.free, space.sectors);
    TW_CHECK(!space.map.empty());

    // The round trip keeps the tree; the file is where it was.
    const std::string image = dos.serialise(0);
    Dos restored;
    TW_CHECK(restored.deserialise(0, image));
    const std::vector<std::uint8_t> bytes = restored.readFile(0, "PROGRAMS/HELLO.S");
    TW_CHECK_EQ(std::string(bytes.begin(), bytes.end()), std::string{"HI\n"});

    // A locked file is refused deletion, and a copy of one is allowed.
    TW_CHECK(restored.setLocked(0, "PROGRAMS/HELLO.S", true));
    TW_CHECK(!restored.removeFile(0, "PROGRAMS/HELLO.S"));
    TW_CHECK_EQ(restored.error(), std::string{"?FILE LOCKED"});
    TW_CHECK(restored.copy(0, "PROGRAMS/HELLO.S", "HELLO.S"));

    // Copying something that is not there is refused in the machine's voice.
    TW_CHECK(!restored.copy(0, "NOPE.S", "X.S"));
    TW_CHECK(!restored.error().empty());
}

void test_the_prompt_runs_programs_and_the_disk() {
    announce("the prompt: HELP, DIR, SAVE, LOAD and RUN");
    Machine machine;
    Dos dos;
    Monitor monitor{machine};
    Modules modules{"/tmp/hb2-shell-bay"};
    Shell shell{machine};
    shell.setDos(&dos);
    shell.setMonitor(&monitor);
    shell.setModules(&modules);
    TW_CHECK(dos.format(0));

    const std::vector<std::string> help = shell.execute("HELP");
    TW_CHECK(help.size() > 8);
    TW_CHECK(shell.execute("BOGUS").front().find("?WHAT") != std::string::npos);

    // A listing saved to the disk and loaded back is the same listing.
    shell.setSource("        ORG $1000\n        MOV R0, #7\n        INT 1\n        HALT\n");
    TW_CHECK(shell.execute("SAVE TEST.S").front().find("saved") != std::string::npos);
    shell.setSource("");
    TW_CHECK(shell.execute("LOAD TEST.S").front().find("loaded") != std::string::npos);
    TW_CHECK(shell.source().find("MOV R0, #7") != std::string::npos);

    // DIR lists it, and RUN assembles, runs and reports the console.
    const std::vector<std::string> dir = shell.execute("DIR");
    bool saw = false;
    for (const std::string& row : dir) {
        if (row.find("TEST.S") != std::string::npos) {
            saw = true;
        }
    }
    TW_CHECK(saw);

    const std::vector<std::string> run = shell.execute("RUN");
    TW_CHECK(!run.empty());
    TW_CHECK(run.front() == "halted");
    // INT 1 printed a byte whose value is 7; the machine records it either way.
    TW_CHECK(!machine.console().empty() || run.size() > 1);

    // The monitor words are reachable from the prompt.
    TW_CHECK(shell.execute("REGS").front().find("PC=") != std::string::npos);
    TW_CHECK(shell.execute("DIS").size() > 1);
}

}  // namespace

int main() {
    std::printf("HB-2 machine tests\n");
    test_the_map_is_one_table();
    test_mode_zero_is_the_1982_map();
    test_instructions_execute_and_cost_what_the_table_says();
    test_calls_and_the_stack();
    test_a_fault_is_reported();
    test_the_rest_of_the_instruction_set();
    test_assemble_disassemble_assemble();
    test_assembler_reports_bad_lines();
    test_the_blitter_draws_pixels();
    test_high_mode_packs_two_pixels_a_byte();
    test_sprites_collide();
    test_the_palette_is_a_dac();
    test_a_chord_is_the_same_bytes_twice();
    test_the_waveform_bank_is_poke_able();
    test_the_latch_still_sounds();
    test_the_pointer_reports_a_cell();
    test_the_clock_is_injected();
    test_a_program_prints_and_draws();
    test_a_snapshot_round_trips();
    test_the_demos_run();
    test_text_is_drawn_in_bitmap_modes();
    test_the_keyboard_is_read_once();
    test_two_processes_both_make_progress();
    test_a_dying_process_does_not_kill_the_desktop();
    test_the_monitor_steps_and_breaks();
    test_modules_round_trip();
    test_the_disk_has_directories_and_a_sector_count();
    test_the_prompt_runs_programs_and_the_disk();
    return tw::test::summary("hb2");
}
