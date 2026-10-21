"""CHIP-8 assembler and machine tests.

The assembler and the machine are tested separately: an assembled wrong opcode and
a wrongly executed right opcode are different bugs, and a test that only ran a
program end to end would let the first hide inside the second.
"""

# Tests say what they check in their names, which is the convention across this
# suite; a docstring per test would only restate the name.
# pylint: disable=missing-function-docstring

from __future__ import annotations

from time_warp.core.interpreter import Language
from time_warp.graphics.turtle_state import TurtleState
from time_warp.languages.chip8 import (
    _FONT_ADDRESS,
    _LARGE_FONT,
    _LARGE_FONT_ADDRESS,
    HIRES_HEIGHT,
    HIRES_WIDTH,
    SCREEN_HEIGHT,
    SCREEN_WIDTH,
    Chip8Error,
    Machine,
    assemble,
    execute_chip8,
)

from .conftest_lang import has, no_errors, run

CHIP8 = Language.CHIP8


def _assemble(source: str) -> tuple[bytearray, dict[str, int]]:
    memory, labels, _entry = assemble(source)
    return memory, labels


def _run_to_halt(source: str, keys: int = 0) -> Machine:
    memory, _labels, entry = assemble(source)
    machine = Machine(memory, entry, keys=keys)
    guard = 0
    while not machine.halted:
        machine.step()
        guard += 1
        assert guard < 100_000, "the program did not halt"
    return machine


def _draw(source: str) -> int:
    """Lit pixels in the final framebuffer."""
    return sum(_run_to_halt(source).framebuffer)


# -- assembler ---------------------------------------------------------------


def test_program_starts_at_0x200() -> None:
    memory, labels = _assemble("start: CLS")
    assert labels["start"] == 0x200
    assert memory[0x200] == 0x00 and memory[0x201] == 0xE0


def test_labels_resolve_forward_references() -> None:
    memory, labels = _assemble("start: JP later\nlater: CLS")
    assert labels["later"] == 0x202
    assert memory[0x200] == 0x12 and memory[0x201] == 0x02


def test_both_hex_spellings_agree() -> None:
    with_prefix, _ = _assemble("start: LD V0, 0x41")
    with_suffix, _ = _assemble("start: LD V0, 41h")
    bare, _ = _assemble("start: LD V0, 41")
    assert with_prefix[0x200:0x202] == with_suffix[0x200:0x202]
    # A bare number is decimal, which is why the marked forms exist.
    assert bare[0x200:0x202] != with_prefix[0x200:0x202]


def test_font_is_present_in_memory() -> None:
    memory, _labels = _assemble("start: HALT")
    assert memory[_FONT_ADDRESS] == 0xF0  # the glyph for 0


def test_db_and_dw_emit_data() -> None:
    memory, labels = _assemble("start: HALT\ndata: DB 0xAB\n      DW 0x1234")
    assert memory[labels["data"]] == 0xAB
    assert memory[labels["data"] + 1] == 0x12
    assert memory[labels["data"] + 2] == 0x34


def test_assembler_rejects_unknown_mnemonic() -> None:
    output = run("start: FROB V0", CHIP8)
    assert has(output, "❌", "not an instruction form")


def test_assembler_rejects_an_out_of_range_byte() -> None:
    output = run("start: LD V0, 999", CHIP8)
    assert has(output, "❌", "not a byte")


def test_assembler_rejects_a_duplicate_label() -> None:
    output = run("dup: CLS\ndup: CLS", CHIP8)
    assert has(output, "❌", "defined twice")


def test_assembler_rejects_a_non_v0_jump_offset() -> None:
    """Bnnn always adds V0; another register would assemble to a lie."""
    output = run("start: JP V5, target\ntarget: HALT", CHIP8)
    assert has(output, "❌", "V0 only")


def test_assembler_rejects_a_row_count_over_fifteen() -> None:
    output = run("start: DRW V0, V1, 20", CHIP8)
    assert has(output, "❌", "at most 15 rows")


def test_two_digit_register_names_are_not_accepted() -> None:
    output = run("start: LD VG, 1", CHIP8)
    assert has(output, "❌", "not a register")


# -- machine -----------------------------------------------------------------


def test_sys_is_ignored() -> None:
    """0NNN is an RCA 1802 call, which every interpreter since ignores."""
    machine = _run_to_halt("start: SYS 0x123\n       LD V0, 7\n       HALT")
    assert machine.v[0] == 7
    raw = _run_to_halt("start: DW 0x0123\n       LD V0, 9\n       HALT")
    assert raw.v[0] == 9
    memory, _labels = _assemble("start: SYS 0x123")
    assert memory[0x200:0x202] == bytearray([0x01, 0x23])


def test_ld_and_add_with_a_byte() -> None:
    machine = _run_to_halt("start: LD V0, 5\n       ADD V0, 3\n       HALT")
    assert machine.v[0] == 8


def test_add_byte_wraps_without_carry() -> None:
    machine = _run_to_halt("start: LD V0, 250\n       ADD V0, 10\n       HALT")
    assert machine.v[0] == 4
    assert machine.v[0xF] == 0


def test_logic_and_shift_left() -> None:
    machine = _run_to_halt(
        "start: LD V0, 0x0C\n       LD V1, 0x0A\n"
        "       LD V2, 0x0C\n       OR V2, V1\n"
        "       LD V3, 0x0C\n       AND V3, V1\n"
        "       LD V4, 0x0C\n       XOR V4, V1\n"
        "       LD V5, 3\n       LD V6, 10\n       SUBN V5, V6\n"
        "       LD V7, 0x81\n       SHL V7\n"
        "       HALT"
    )
    assert (machine.v[2], machine.v[3], machine.v[4]) == (0x0E, 0x08, 0x06)
    assert machine.v[5] == 7
    assert machine.v[7] == 2
    assert machine.v[0xF] == 1


def test_index_arithmetic() -> None:
    machine = _run_to_halt(
        "start: LD I, 0x300\n       LD V0, 5\n       ADD I, V0\n       HALT"
    )
    assert machine.i == 0x305


def test_add_sets_the_carry_flag() -> None:
    machine = _run_to_halt(
        "start: LD V0, 200\n       LD V1, 100\n       ADD V0, V1\n       HALT"
    )
    assert machine.v[0] == 44
    assert machine.v[0xF] == 1


def test_subtract_sets_the_borrow_flag() -> None:
    machine = _run_to_halt(
        "start: LD V0, 5\n       LD V1, 9\n       SUB V0, V1\n       HALT"
    )
    assert machine.v[0] == 252
    assert machine.v[0xF] == 0


def test_shr_shifts_the_named_register_in_place() -> None:
    """The 1977 machine ignores the y operand; a later emulator would not."""
    machine = _run_to_halt(
        "start: LD V0, 8\n       LD V1, 1\n       SHR V0\n       HALT"
    )
    assert machine.v[0] == 4
    assert machine.v[1] == 1


def test_bcd_conversion_writes_three_digits() -> None:
    memory, labels, _entry = assemble(
        "start: LD V0, 205\n"
        "       LD I, data\n"
        "       LD B, V0\n"
        "       HALT\n"
        "data: DB 0\n"
        "      DB 0\n"
        "      DB 0"
    )
    machine = Machine(memory, 0x200)
    while not machine.halted:
        machine.step()
    start = labels["data"]
    assert list(memory[start : start + 3]) == [2, 0, 5]


def test_store_and_load_through_i() -> None:
    machine = _run_to_halt(
        "start: LD V0, 1\n       LD V1, 2\n       LD I, buf\n       LD [I], V1\n"
        "       LD I, buf\n       LD V3, [I]\n       HALT\n"
        "buf:   DB 0\n       DB 0"
    )
    assert machine.v[0] == 1
    assert machine.v[1] == 2
    assert machine.v[3] == 0


def test_block_transfer_advances_i() -> None:
    """The 1977 interpreter leaves I one past the block."""
    memory, labels, entry = assemble(
        "start: LD V0, 1\n       LD V1, 2\n       LD I, buf\n"
        "       LD [I], V1\n       HALT\nbuf: DB 0\n     DB 0"
    )
    machine = Machine(memory, entry)
    while not machine.halted:
        machine.step()
    assert machine.i == labels["buf"] + 2

    memory, labels, entry = assemble(
        "start: LD I, buf\n       LD V1, [I]\n       HALT\nbuf: DB 7\n     DB 9"
    )
    loaded = Machine(memory, entry)
    while not loaded.halted:
        loaded.step()
    assert (loaded.v[0], loaded.v[1]) == (7, 9)
    assert loaded.i == labels["buf"] + 2


def test_skip_when_equal_skips_one_instruction() -> None:
    machine = _run_to_halt(
        "start: LD V0, 5\n       SE V0, 5\n       LD V1, 1\n       HALT"
    )
    assert machine.v[1] == 0


def test_skip_when_not_equal_does_not_skip_when_equal() -> None:
    machine = _run_to_halt(
        "start: LD V0, 5\n       SNE V0, 5\n       LD V1, 1\n       HALT"
    )
    assert machine.v[1] == 1


def test_call_and_return() -> None:
    machine = _run_to_halt(
        "start: LD V0, 7\n       CALL sub\n       HALT\nsub:   ADD V0, 1\n       RET"
    )
    assert machine.v[0] == 8
    assert not machine.stack


def test_ret_without_call_is_reported() -> None:
    output = run("start: RET", CHIP8)
    assert has(output, "❌", "empty call stack")


def test_draw_lights_the_expected_number_of_pixels() -> None:
    # The glyph for 3 is 0xF0,0x10,0xF0,0x10,0xF0: four, one, four, one, four.
    machine = _run_to_halt(
        "start: LD V0, 3\n       LD F, V0\n       LD V0, 0\n       LD V1, 0\n"
        "       DRW V0, V1, 5\n       HALT"
    )
    assert sum(machine.framebuffer) == 14


def test_drawing_twice_erases_and_reports_the_collision() -> None:
    machine = _run_to_halt(
        "start: LD I, dot\n       LD V0, 0\n       LD V1, 0\n"
        "       DRW V0, V1, 1\n       DRW V0, V1, 1\n       HALT\n"
        "dot:   DB 0x80"
    )
    assert machine.v[0xF] == 1
    assert sum(machine.framebuffer) == 0


def test_cls_empties_the_framebuffer() -> None:
    machine = _run_to_halt(
        "start: LD I, dot\n       DRW V0, V1, 1\n       CLS\n       HALT\ndot: DB 0x80"
    )
    assert sum(machine.framebuffer) == 0


def test_pixels_wrap_around_the_screen() -> None:
    """A sprite drawn at x=63 puts its second column back at x=0."""
    machine = _run_to_halt(
        "start: LD I, pair\n       LD V0, 63\n       LD V1, 0\n"
        "       DRW V0, V1, 1\n       HALT\npair:  DB 0xC0"
    )
    assert machine.framebuffer[0] == 1
    assert machine.framebuffer[63] == 1


def test_register_skips() -> None:
    source = "start: LD V0, 5\n       LD V1, 5\n       SE V0, V1\n       LD V2, 1\n       HALT"
    assert _run_to_halt(source).v[2] == 0
    source = "start: LD V0, 5\n       LD V1, 5\n       SNE V0, V1\n       LD V2, 1\n       HALT"
    assert _run_to_halt(source).v[2] == 1


def test_skp_skips_when_the_key_is_held() -> None:
    source = "start: LD V0, 3\n       SKP V0\n       LD V1, 1\n       HALT"
    assert _run_to_halt(source, keys=1 << 3).v[1] == 0
    assert _run_to_halt(source).v[1] == 1


def test_sknp_skips_when_the_key_is_up() -> None:
    source = "start: LD V0, 3\n       SKNP V0\n       LD V1, 1\n       HALT"
    assert _run_to_halt(source).v[1] == 0
    assert _run_to_halt(source, keys=1 << 3).v[1] == 1


def test_key_read_returns_the_held_key() -> None:
    machine = _run_to_halt("start: LD V0, K\n       HALT", keys=1 << 5)
    assert machine.v[0] == 5


def test_key_read_returns_zero_when_nothing_is_held() -> None:
    machine = _run_to_halt("start: LD V0, K\n       HALT")
    assert machine.v[0] == 0


def test_jump_with_offset_uses_v0() -> None:
    """`JP V0, addr` lands at addr + V0, so V0 chooses where inside the target."""
    program = (
        "start: LD V0, {offset}\n"
        "       JP V0, target\n"
        "       LD V1, 1\n"
        "target: LD V2, 9\n"
        "       HALT"
    )
    assert _run_to_halt(program.format(offset=0)).v[2] == 9
    # Two bytes past the label skips `LD V2, 9`, so V2 keeps its zero.
    machine = _run_to_halt(program.format(offset=2))
    assert machine.v[2] == 0
    assert machine.v[1] == 0


def test_timers_are_written_and_read() -> None:
    memory, _labels, entry = assemble(
        "start: LD V0, 2\n       LD DT, V0\n       LD ST, V0\n       LD V1, DT\n       HALT"
    )
    machine = Machine(memory, entry)
    while not machine.halted:
        machine.step()
    assert machine.v[1] == 2
    assert machine.delay_timer == 2
    assert machine.sound_timer == 2
    machine.tick_timers()
    assert (machine.delay_timer, machine.sound_timer) == (1, 1)
    machine.tick_timers()
    machine.tick_timers()
    assert (machine.delay_timer, machine.sound_timer) == (0, 0)


def test_a_binary_rom_loads_at_0x200() -> None:
    rom = "\x60\x05\x61\x07\x00\x00"
    machine = _run_to_halt(rom)
    assert (machine.v[0], machine.v[1]) == (5, 7)
    assert machine.memory[_FONT_ADDRESS] == 0xF0


def test_a_binary_rom_must_fit_memory() -> None:
    output = run("\x60" + "\x00" * 3584, CHIP8)
    assert has(output, "❌", "holds 3584")


def test_cycle_limit_is_reported() -> None:
    """A jump-to-self is a legitimate CHIP-8 program, so this is information."""
    output = run("start: JP start", CHIP8)
    assert has(output, "ℹ️", "600 frames")


# -- SuperChip hi-res --------------------------------------------------------


def test_hires_mode_changes_the_grid_and_starts_blank() -> None:
    machine = _run_to_halt("start: DW 0x00FF\n       HALT")
    assert (machine.width, machine.height) == (HIRES_WIDTH, HIRES_HEIGHT)
    assert len(machine.framebuffer) == HIRES_WIDTH * HIRES_HEIGHT

    machine = _run_to_halt(
        "start: LD I, dot\n"
        "       DRW V0, V1, 1\n"
        "       DW 0x00FF\n"
        "       DW 0x00FE\n"
        "       HALT\n"
        "dot:   DB 0xFF"
    )
    assert (machine.width, machine.height) == (SCREEN_WIDTH, SCREEN_HEIGHT)
    assert len(machine.framebuffer) == SCREEN_WIDTH * SCREEN_HEIGHT
    # A mode switch does not keep the old drawing: the grid changed under it.
    assert sum(machine.framebuffer) == 0


def test_the_large_sprite_is_sixteen_by_sixteen_in_hires() -> None:
    rows = "".join("       DW 0xFFFF\n" for _ in range(16))
    machine = _run_to_halt(
        "start: DW 0x00FF\n"
        "       LD I, sprite\n"
        "       DRW V0, V1, 0\n"
        "       HALT\n"
        "sprite:\n" + rows
    )
    assert sum(machine.framebuffer) == 16 * 16
    assert machine.framebuffer[0] == 1
    assert machine.framebuffer[15] == 1
    assert machine.framebuffer[15 * HIRES_WIDTH + 15] == 1


def test_the_large_sprite_doubles_columns_in_lores() -> None:
    rows = "".join("       DB 0x80\n" for _ in range(16))
    machine = _run_to_halt(
        "start: LD I, sprite\n"
        "       DRW V0, V1, 0\n"
        "       HALT\n"
        "sprite:\n" + rows
    )
    # Each row's top bit becomes two pixels, so 16 rows of 2.
    assert sum(machine.framebuffer) == 32
    assert machine.framebuffer[0] == 1
    assert machine.framebuffer[1] == 1
    assert machine.framebuffer[2] == 0
    assert machine.framebuffer[SCREEN_WIDTH] == 1


def test_scroll_down_moves_the_drawing_and_clears_the_top() -> None:
    machine = _run_to_halt(
        "start: LD I, dot\n"
        "       DRW V0, V1, 1\n"
        "       DW 0x00C2\n"
        "       HALT\n"
        "dot:   DB 0x80"
    )
    assert machine.framebuffer[2 * SCREEN_WIDTH] == 1
    assert machine.framebuffer[0] == 0
    assert machine.framebuffer[SCREEN_WIDTH] == 0


def test_scroll_left_moves_the_drawing_four_pixels() -> None:
    machine = _run_to_halt(
        "start: LD V0, 8\n"
        "       LD I, dot\n"
        "       DRW V0, V1, 1\n"
        "       DW 0x00FC\n"
        "       HALT\n"
        "dot:   DB 0x80"
    )
    assert machine.framebuffer[4] == 1
    assert machine.framebuffer[8] == 0


def test_scroll_right_moves_the_drawing_four_pixels() -> None:
    machine = _run_to_halt(
        "start: LD V0, 8\n"
        "       LD I, dot\n"
        "       DRW V0, V1, 1\n"
        "       DW 0x00FB\n"
        "       HALT\n"
        "dot:   DB 0x80"
    )
    assert machine.framebuffer[12] == 1
    assert machine.framebuffer[8] == 0


def test_exit_opcode_halts_without_running_the_next_instruction() -> None:
    machine = _run_to_halt("start: DW 0x00FD\n       LD V0, 9\n       HALT")
    assert machine.halted
    assert machine.v[0] == 0


def test_fx30_points_at_the_large_font() -> None:
    machine = _run_to_halt("start: LD V0, 3\n       DW 0xF030\n       HALT")
    assert machine.i == _LARGE_FONT_ADDRESS + 30


def test_the_large_font_is_in_memory() -> None:
    memory, _labels = _assemble("start: HALT")
    # The small glyph for 0 is 0xF0, so the doubled first row is 0xFF twice.
    assert memory[_LARGE_FONT_ADDRESS] == 0xFF
    assert (
        bytes(memory[_LARGE_FONT_ADDRESS : _LARGE_FONT_ADDRESS + 10])
        == _LARGE_FONT[:10]
    )


def test_rpl_flags_round_trip() -> None:
    machine = _run_to_halt(
        "start: LD V0, 1\n"
        "       LD V1, 2\n"
        "       DW 0xF175\n"
        "       LD V0, 0\n"
        "       LD V1, 0\n"
        "       DW 0xF185\n"
        "       HALT"
    )
    assert (machine.v[0], machine.v[1]) == (1, 2)


# -- drawing -----------------------------------------------------------------


def test_the_display_uses_the_palette() -> None:
    turtle = TurtleState()
    execute_chip8(
        None,
        "start: LD I, dot\n       DRW V0, V1, 1\n       HALT\ndot: DB 0xFF",
        turtle,
    )

    lit = [
        shape
        for shape in turtle.shapes
        if shape.shape_type == "rect" and shape.fill_color == (140, 255, 140)
    ]
    assert len(lit) == 8, "0xFF is eight pixels"
    assert no_errors([])


def test_the_display_halves_the_pixel_size_in_hires() -> None:
    turtle = TurtleState()
    execute_chip8(
        None,
        "start: DW 0x00FF\n       LD I, dot\n       DRW V0, V1, 1\n"
        "       HALT\ndot: DB 0xFF",
        turtle,
    )
    lit = [
        shape
        for shape in turtle.shapes
        if shape.shape_type == "rect" and shape.fill_color == (140, 255, 140)
    ]
    assert len(lit) == 8
    corner, right = lit[0].points[0], lit[0].points[1]
    assert abs((right[0] - corner[0]) - 4.0) < 1e-9, "a hi-res pixel is four units"


def test_the_display_is_cleared_before_the_first_frame() -> None:
    turtle = TurtleState()
    execute_chip8(None, "start: CLS\n       HALT", turtle)
    assert turtle.shapes, "the screen is painted even by a program that draws nothing"


def test_an_unknown_opcode_is_reported() -> None:
    """0NNN is SYS and is ignored; Fx0FF is not an instruction."""
    output = run("start: DW 0xF0FF", CHIP8)
    assert has(output, "❌", "unknown opcode")
    assert has(output, "FxFF")


def test_screen_constants_are_the_original_geometry() -> None:
    assert (SCREEN_WIDTH, SCREEN_HEIGHT) == (64, 32)


def test_the_assembler_reports_the_line_number() -> None:
    try:
        assemble("start: CLS\n       NOPE V0")
    except Chip8Error as exc:
        assert "line 2" in str(exc)
    else:  # pragma: no cover
        raise AssertionError("expected an assembler error")
