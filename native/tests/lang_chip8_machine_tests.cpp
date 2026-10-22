// SPDX-License-Identifier: MIT
//
// CHIP-8 instructions and the display, through the machine and the interpreter.

#include "lang/support.hpp"

namespace tw::lang_tests {

// ---------------------------------------------------------------------------
// CHIP-8: machine
// ---------------------------------------------------------------------------

void test_chip8_ld_and_add_with_a_byte() {
    const Chip8Machine machine = run_to_halt("start: LD V0, 5\n       ADD V0, 3\n       HALT");
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 8);
}

void test_chip8_add_sets_the_carry_flag() {
    const Chip8Machine machine = run_to_halt(
        "start: LD V0, 200\n       LD V1, 100\n       ADD V0, V1\n       HALT");
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 44);
    TW_CHECK_EQ(static_cast<int>(machine.v[0xF]), 1);
}

void test_chip8_subtract_sets_the_borrow_flag() {
    const Chip8Machine machine = run_to_halt(
        "start: LD V0, 5\n       LD V1, 9\n       SUB V0, V1\n       HALT");
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 252);
    TW_CHECK_EQ(static_cast<int>(machine.v[0xF]), 0);
}

void test_chip8_shr_shifts_the_named_register_in_place() {
    const Chip8Machine machine =
        run_to_halt("start: LD V0, 8\n       LD V1, 1\n       SHR V0\n       HALT");
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 4);
    TW_CHECK_EQ(static_cast<int>(machine.v[1]), 1);
    TW_CHECK_EQ(static_cast<int>(machine.v[0xF]), 0);
}

void test_chip8_logic_and_shl() {
    const Chip8Machine machine = run_to_halt(
        "start: LD V0, 0x0C\n       LD V1, 0x0A\n"
        "       LD V2, 0x0C\n       OR V2, V1\n"       // 0x0E
        "       LD V3, 0x0C\n       AND V3, V1\n"      // 0x08
        "       LD V4, 0x0C\n       XOR V4, V1\n"      // 0x06
        "       LD V5, 3\n       LD V6, 10\n       SUBN V5, V6\n"  // 7
        "       LD V7, 0x81\n       SHL V7\n"          // 0x02, VF=1
        "       HALT");
    TW_CHECK_EQ(static_cast<int>(machine.v[2]), 0x0E);
    TW_CHECK_EQ(static_cast<int>(machine.v[3]), 0x08);
    TW_CHECK_EQ(static_cast<int>(machine.v[4]), 0x06);
    TW_CHECK_EQ(static_cast<int>(machine.v[5]), 7);
    TW_CHECK_EQ(static_cast<int>(machine.v[7]), 2);
    TW_CHECK_EQ(static_cast<int>(machine.v[0xF]), 1);
}

void test_chip8_add_byte_wraps_without_carry() {
    const Chip8Machine machine =
        run_to_halt("start: LD V0, 250\n       ADD V0, 10\n       HALT");
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 4);
    // 7XNN does not touch VF; only the arithmetic group does.
    TW_CHECK_EQ(static_cast<int>(machine.v[0xF]), 0);
}

void test_chip8_index_arithmetic() {
    const Chip8Machine machine = run_to_halt(
        "start: LD I, 0x300\n       LD V0, 5\n       ADD I, V0\n       HALT");
    TW_CHECK_EQ(machine.i, 0x305);
}

void test_chip8_keypad_is_live() {
    const auto run_with_key = [](bool down) {
        const Chip8Program program = assemble(
            "start: LD V0, 3\n       SKP V0\n       LD V1, 1\n       HALT");
        Chip8Machine machine{program.memory, program.entry};
        machine.set_interactive(true);
        machine.set_key(3, true);
        if (!down) {
            machine.set_key(3, false);
        }
        while (!machine.halted()) {
            machine.step();
        }
        return machine;
    };
    TW_CHECK_EQ(static_cast<int>(run_with_key(true).v[1]), 0);   // skipped
    TW_CHECK_EQ(static_cast<int>(run_with_key(false).v[1]), 1);  // not skipped
}

void test_chip8_ld_key_waits_for_a_press() {
    const Chip8Program program =
        assemble("start: LD V0, K\n       LD V1, 9\n       HALT");
    Chip8Machine machine{program.memory, program.entry};
    machine.set_interactive(true);
    machine.step();  // LD V0, K: no key held yet
    TW_CHECK(machine.waiting_for_key());
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 0);
    const int waiting_pc = machine.pc;
    machine.step();
    TW_CHECK(machine.waiting_for_key());
    TW_CHECK_EQ(machine.pc, waiting_pc);
    machine.set_key(7, true);
    machine.step();  // the wait completes, V0 gets the key
    TW_CHECK(!machine.waiting_for_key());
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 7);
    machine.step();
    TW_CHECK_EQ(static_cast<int>(machine.v[1]), 9);

    // A whole-program run keeps the reference's one-shot read: no key is 0.
    Chip8Machine legacy{program.memory, program.entry};
    legacy.step();
    TW_CHECK(!legacy.waiting_for_key());
    TW_CHECK_EQ(static_cast<int>(legacy.v[0]), 0);
}

void test_chip8_bcd_conversion_writes_three_digits() {
    const Chip8Program program = assemble(
        "start: LD V0, 205\n       LD I, data\n       LD B, V0\n       HALT\n"
        "data: DB 0\n      DB 0\n      DB 0");
    Chip8Machine machine{program.memory, 0x200};
    while (!machine.halted()) {
        machine.step();
    }
    const std::size_t start = static_cast<std::size_t>(program.labels.at("data"));
    TW_CHECK_EQ(static_cast<int>(machine.memory[start]), 2);
    TW_CHECK_EQ(static_cast<int>(machine.memory[start + 1]), 0);
    TW_CHECK_EQ(static_cast<int>(machine.memory[start + 2]), 5);
}

void test_chip8_store_and_load_through_i() {
    const Chip8Machine machine = run_to_halt(
        "start: LD V0, 1\n       LD V1, 2\n       LD I, buf\n       LD [I], V1\n"
        "       LD I, buf\n       LD V3, [I]\n       HALT\n"
        "buf:   DB 0\n       DB 0");
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 1);
    TW_CHECK_EQ(static_cast<int>(machine.v[1]), 2);
    TW_CHECK_EQ(static_cast<int>(machine.v[3]), 0);
}

void test_chip8_block_transfer_advances_i() {
    // The 1977 interpreter leaves I one past the block; CHIP-48 and SCHIP leave
    // it where it was.
    const Chip8Program program = assemble(
        "start: LD V0, 1\n       LD V1, 2\n       LD I, buf\n"
        "       LD [I], V1\n       HALT\nbuf: DB 0\n     DB 0");
    Chip8Machine machine{program.memory, program.entry};
    while (!machine.halted()) {
        machine.step();
    }
    TW_CHECK_EQ(machine.i, program.labels.at("buf") + 2);

    const Chip8Program load = assemble(
        "start: LD I, buf\n       LD V1, [I]\n       HALT\nbuf: DB 7\n     DB 9");
    Chip8Machine loaded{load.memory, load.entry};
    while (!loaded.halted()) {
        loaded.step();
    }
    TW_CHECK_EQ(static_cast<int>(loaded.v[0]), 7);
    TW_CHECK_EQ(static_cast<int>(loaded.v[1]), 9);
    TW_CHECK_EQ(loaded.i, load.labels.at("buf") + 2);
}

void test_chip8_skips() {
    TW_CHECK_EQ(
        static_cast<int>(run_to_halt("start: LD V0, 5\n       SE V0, 5\n       LD V1, 1\n       HALT")
                             .v[1]),
        0);
    TW_CHECK_EQ(
        static_cast<int>(run_to_halt("start: LD V0, 5\n       SNE V0, 5\n       LD V1, 1\n       HALT")
                             .v[1]),
        1);
    // 5XY0 and 9XY0 are the register forms; the low nibble is not a value.
    TW_CHECK_EQ(
        static_cast<int>(run_to_halt("start: LD V0, 5\n       LD V1, 5\n"
                                     "       SE V0, V1\n       LD V2, 1\n       HALT")
                             .v[2]),
        0);
    TW_CHECK_EQ(
        static_cast<int>(run_to_halt("start: LD V0, 5\n       LD V1, 5\n"
                                     "       SNE V0, V1\n       LD V2, 1\n       HALT")
                             .v[2]),
        1);
}

void test_chip8_call_and_return() {
    const Chip8Machine machine = run_to_halt(
        "start: LD V0, 7\n       CALL sub\n       HALT\nsub:   ADD V0, 1\n       RET");
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 8);
    TW_CHECK(machine.stack.empty());
}

void test_chip8_ret_without_call_is_reported() {
    TW_CHECK(contains(run_chip8("start: RET"), "empty call stack"));
}

void test_chip8_draw_lights_the_expected_pixels() {
    // The glyph for 3 is 0xF0,0x10,0xF0,0x10,0xF0: four, one, four, one, four.
    const Chip8Machine machine = run_to_halt(
        "start: LD V0, 3\n       LD F, V0\n       LD V0, 0\n       LD V1, 0\n"
        "       DRW V0, V1, 5\n       HALT");
    TW_CHECK_EQ(lit_pixels(machine), std::size_t{14});
}

void test_chip8_drawing_twice_erases_and_reports_the_collision() {
    const Chip8Machine machine = run_to_halt(
        "start: LD I, dot\n       LD V0, 0\n       LD V1, 0\n"
        "       DRW V0, V1, 1\n       DRW V0, V1, 1\n       HALT\n"
        "dot:   DB 0x80");
    TW_CHECK_EQ(static_cast<int>(machine.v[0xF]), 1);
    TW_CHECK_EQ(lit_pixels(machine), std::size_t{0});
}

void test_chip8_pixels_wrap_around_the_screen() {
    const Chip8Machine machine = run_to_halt(
        "start: LD I, pair\n       LD V0, 63\n       LD V1, 0\n"
        "       DRW V0, V1, 1\n       HALT\npair:  DB 0xC0");
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[0]), 1);
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[63]), 1);
}

void test_chip8_hires_mode_changes_the_grid_and_starts_blank() {
    const Chip8Machine hires = run_to_halt("start: DW 0x00FF\n       HALT");
    TW_CHECK(hires.hi_res());
    TW_CHECK_EQ(hires.screen_width(), timewarp::lang::kChip8HiResWidth);
    TW_CHECK_EQ(hires.screen_height(), timewarp::lang::kChip8HiResHeight);
    TW_CHECK_EQ(hires.framebuffer().size(),
                static_cast<std::size_t>(timewarp::lang::kChip8HiResWidth) *
                    timewarp::lang::kChip8HiResHeight);

    const Chip8Machine lores = run_to_halt(
        "start: LD I, dot\n"
        "       DRW V0, V1, 1\n"
        "       DW 0x00FF\n"
        "       DW 0x00FE\n"
        "       HALT\n"
        "dot:   DB 0xFF");
    TW_CHECK(!lores.hi_res());
    TW_CHECK_EQ(lores.framebuffer().size(),
                static_cast<std::size_t>(timewarp::lang::kChip8ScreenWidth) *
                    timewarp::lang::kChip8ScreenHeight);
    // A mode switch does not keep the old drawing: the grid changed under it.
    TW_CHECK_EQ(lit_pixels(lores), std::size_t{0});
}

void test_chip8_cls_in_hires_keeps_the_hi_res_grid() {
    // CLS is 00E0. In hi-res the grid is 128x64, and clearing has to keep it:
    // a 64x32 buffer here would let the next DRAW write outside the frame and
    // paint nothing below the sixteenth row.
    const Chip8Machine cleared = run_to_halt(
        "start: DW 0x00FF\n"
        "       CLS\n"
        "       HALT");
    TW_CHECK(cleared.hi_res());
    TW_CHECK_EQ(cleared.framebuffer().size(),
                static_cast<std::size_t>(timewarp::lang::kChip8HiResWidth) *
                    timewarp::lang::kChip8HiResHeight);

    const Chip8Machine bottom = run_to_halt(
        "start: DW 0x00FF\n"
        "       CLS\n"
        "       LD V0, 0\n"
        "       LD V1, 63\n"
        "       LD I, dot\n"
        "       DRW V0, V1, 1\n"
        "       HALT\n"
        "dot:   DB 0x80");
    TW_CHECK_EQ(bottom.framebuffer().size(),
                static_cast<std::size_t>(timewarp::lang::kChip8HiResWidth) *
                    timewarp::lang::kChip8HiResHeight);
    TW_CHECK_EQ(static_cast<int>(bottom.framebuffer()[63 * timewarp::lang::kChip8HiResWidth]),
                1);
}

void test_chip8_large_sprite_is_sixteen_by_sixteen_in_hires() {
    std::string rows;
    for (int index = 0; index < 16; ++index) {
        rows += "       DW 0xFFFF\n";
    }
    const Chip8Machine machine = run_to_halt(
        "start: DW 0x00FF\n"
        "       LD I, sprite\n"
        "       DRW V0, V1, 0\n"
        "       HALT\n"
        "sprite:\n" +
        rows);
    TW_CHECK_EQ(lit_pixels(machine), std::size_t{16 * 16});
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[0]), 1);
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[15]), 1);
    TW_CHECK_EQ(
        static_cast<int>(machine.framebuffer()[15 * timewarp::lang::kChip8HiResWidth + 15]), 1);
}

void test_chip8_large_sprite_doubles_columns_in_lores() {
    std::string rows;
    for (int index = 0; index < 16; ++index) {
        rows += "       DB 0x80\n";
    }
    const Chip8Machine machine = run_to_halt(
        "start: LD I, sprite\n"
        "       DRW V0, V1, 0\n"
        "       HALT\n"
        "sprite:\n" +
        rows);
    // Each row's top bit becomes two pixels: 16 rows of two.
    TW_CHECK_EQ(lit_pixels(machine), std::size_t{32});
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[0]), 1);
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[1]), 1);
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[2]), 0);
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[timewarp::lang::kChip8ScreenWidth]), 1);
}

void test_chip8_scroll_down_moves_the_drawing_and_clears_the_top() {
    const Chip8Machine machine = run_to_halt(
        "start: LD I, dot\n"
        "       DRW V0, V1, 1\n"
        "       DW 0x00C2\n"
        "       HALT\n"
        "dot:   DB 0x80");
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[2 * timewarp::lang::kChip8ScreenWidth]), 1);
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[0]), 0);
    TW_CHECK_EQ(static_cast<int>(machine.framebuffer()[timewarp::lang::kChip8ScreenWidth]), 0);
}

void test_chip8_scroll_left_and_right_move_four_pixels() {
    const std::string base =
        "start: LD V0, 8\n"
        "       LD I, dot\n"
        "       DRW V0, V1, 1\n"
        "       DW 0x00FC\n"
        "       HALT\n"
        "dot:   DB 0x80";
    const Chip8Machine left = run_to_halt(base);
    TW_CHECK_EQ(static_cast<int>(left.framebuffer()[4]), 1);
    TW_CHECK_EQ(static_cast<int>(left.framebuffer()[8]), 0);

    std::string right_source = base;
    right_source.replace(right_source.find("0x00FC"), 6, "0x00FB");
    const Chip8Machine right = run_to_halt(right_source);
    TW_CHECK_EQ(static_cast<int>(right.framebuffer()[12]), 1);
    TW_CHECK_EQ(static_cast<int>(right.framebuffer()[8]), 0);
}

void test_chip8_exit_opcode_halts() {
    const Chip8Machine machine = run_to_halt("start: DW 0x00FD\n       LD V0, 9\n       HALT");
    TW_CHECK(machine.halted());
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 0);
}

void test_chip8_fx30_points_at_the_large_font() {
    const Chip8Machine machine = run_to_halt("start: LD V0, 3\n       DW 0xF030\n       HALT");
    TW_CHECK_EQ(machine.i, timewarp::lang::kChip8LargeFontAddress + 30);
}

void test_chip8_large_font_is_in_memory() {
    const Chip8Program program = assemble("start: HALT");
    // The small glyph for 0 is 0xF0, so the doubled first row is 0xFF twice.
    TW_CHECK_EQ(static_cast<int>(program.memory[timewarp::lang::kChip8LargeFontAddress]), 0xFF);
    TW_CHECK_EQ(static_cast<int>(program.memory[timewarp::lang::kChip8LargeFontAddress + 1]),
                0xFF);
}

void test_chip8_rpl_flags_round_trip() {
    const Chip8Machine machine = run_to_halt(
        "start: LD V0, 1\n"
        "       LD V1, 2\n"
        "       DW 0xF175\n"
        "       LD V0, 0\n"
        "       LD V1, 0\n"
        "       DW 0xF185\n"
        "       HALT");
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 1);
    TW_CHECK_EQ(static_cast<int>(machine.v[1]), 2);
}

void test_chip8_hires_display_halves_the_pixel_size() {
    RecordingHost host;
    Interpreter interpreter{Language::Chip8, host};
    interpreter.load_program(
        "start: DW 0x00FF\n       LD I, dot\n       DRW V0, V1, 1\n"
        "       HALT\ndot: DB 0xFF");
    interpreter.run();
    std::string lit;
    for (const std::string& op : host.ops) {
        if (op.rfind("rect ", 0) == 0 && op.find("#8CFF8C") != std::string::npos) {
            lit = op;
            break;
        }
    }
    TW_CHECK(!lit.empty());
    if (!lit.empty()) {
        std::istringstream stream{lit.substr(5)};
        double x0 = 0.0;
        double y0 = 0.0;
        double x1 = 0.0;
        double y1 = 0.0;
        std::string colour;
        stream >> x0 >> y0 >> x1 >> y1 >> colour;
        TW_CHECK(std::abs((x1 - x0) - 4.0) < 1e-9);
    }
}

void test_chip8_keys() {
    const std::string skip = "start: LD V0, 3\n       SKP V0\n       LD V1, 1\n       HALT";
    TW_CHECK_EQ(static_cast<int>(run_to_halt(skip, 1U << 3).v[1]), 0);
    TW_CHECK_EQ(static_cast<int>(run_to_halt(skip).v[1]), 1);

    const std::string skip_not =
        "start: LD V0, 3\n       SKNP V0\n       LD V1, 1\n       HALT";
    TW_CHECK_EQ(static_cast<int>(run_to_halt(skip_not).v[1]), 0);
    TW_CHECK_EQ(static_cast<int>(run_to_halt(skip_not, 1U << 3).v[1]), 1);

    TW_CHECK_EQ(static_cast<int>(run_to_halt("start: LD V0, K\n       HALT", 1U << 5).v[0]), 5);
    TW_CHECK_EQ(static_cast<int>(run_to_halt("start: LD V0, K\n       HALT").v[0]), 0);
}

void test_chip8_jump_with_offset_uses_v0() {
    const auto program = [](int offset) {
        return "start: LD V0, " + std::to_string(offset) +
               "\n       JP V0, target\n"
               "       LD V1, 1\n"
               "target: LD V2, 9\n"
               "       HALT";
    };
    TW_CHECK_EQ(static_cast<int>(run_to_halt(program(0)).v[2]), 9);
    // Two bytes past the label skips `LD V2, 9`, so V2 keeps its zero.
    const Chip8Machine machine = run_to_halt(program(2));
    TW_CHECK_EQ(static_cast<int>(machine.v[2]), 0);
    TW_CHECK_EQ(static_cast<int>(machine.v[1]), 0);
}

void test_chip8_timers() {
    const Chip8Program program = assemble(
        "start: LD V0, 2\n       LD DT, V0\n       LD ST, V0\n       LD V1, DT\n       HALT");
    Chip8Machine machine{program.memory, program.entry};
    while (!machine.halted()) {
        machine.step();
    }
    TW_CHECK_EQ(static_cast<int>(machine.v[1]), 2);
    TW_CHECK_EQ(machine.delay_timer, 2);
    TW_CHECK_EQ(machine.sound_timer, 2);
    // One tick per frame, never below zero.
    machine.tick_timers();
    TW_CHECK_EQ(machine.delay_timer, 1);
    TW_CHECK_EQ(machine.sound_timer, 1);
    machine.tick_timers();
    machine.tick_timers();
    TW_CHECK_EQ(machine.delay_timer, 0);
    TW_CHECK_EQ(machine.sound_timer, 0);
}

void test_chip8_frame_limit_is_information() {
    TW_CHECK(contains(run_chip8("start: JP start"), "600 frames"));
}

void test_chip8_unknown_opcode_is_reported() {
    // 0NNN is SYS and is ignored; Fx0FF is not an instruction.
    const std::vector<std::string> output = run_chip8("start: DW 0xF0FF");
    TW_CHECK(contains(output, "unknown opcode"));
    TW_CHECK(contains(output, "FxFF"));
}

void test_chip8_binary_rom_loads_at_0x200() {
    // LD V0, 5; LD V1, 7; HALT. The NUL in the HALT opcode is what marks this
    // as a ROM rather than a listing.
    const std::string rom{"\x60\x05\x61\x07\x00\x00", 6};
    const Chip8Machine machine = run_to_halt(rom);
    TW_CHECK_EQ(static_cast<int>(machine.v[0]), 5);
    TW_CHECK_EQ(static_cast<int>(machine.v[1]), 7);
    // The font is still where Fx29 points.
    TW_CHECK_EQ(static_cast<int>(machine.memory[0x50]), 0xF0);
}

void test_chip8_binary_rom_respects_memory() {
    std::string rom(static_cast<std::size_t>(timewarp::lang::kChip8RomLimit) + 1, '\0');
    rom[0] = '\x60';
    const std::vector<std::string> output = run_chip8(rom);
    TW_CHECK(contains(output, "memory from 0x200 holds 3584"));
}

// ---------------------------------------------------------------------------
// CHIP-8: drawing through the interpreter
// ---------------------------------------------------------------------------

void test_chip8_display_uses_the_palette() {
    RecordingHost host;
    Interpreter interpreter{Language::Chip8, host};
    interpreter.load_program("start: LD I, dot\n       DRW V0, V1, 1\n       HALT\ndot: DB 0xFF");
    interpreter.run();
    const std::size_t lit = static_cast<std::size_t>(
        std::count_if(host.ops.begin(), host.ops.end(), [](const std::string& op) {
            return op.rfind("rect ", 0) == 0 && op.find("#8CFF8C") != std::string::npos;
        }));
    TW_CHECK_EQ(lit, std::size_t{8});
}

void test_chip8_display_is_cleared_before_the_first_frame() {
    RecordingHost host;
    Interpreter interpreter{Language::Chip8, host};
    interpreter.load_program("start: CLS\n       HALT");
    interpreter.run();
    // svga, two palette entries, then the clear rectangle.
    TW_CHECK_EQ(host.ops.size(), std::size_t{4});
    TW_CHECK_EQ(host.ops.at(3), std::string{"rect -256 128 256 -128 #101014"});
}

}  // namespace tw::lang_tests
