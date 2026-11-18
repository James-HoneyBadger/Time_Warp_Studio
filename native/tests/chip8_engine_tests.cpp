// SPDX-License-Identifier: AGPL-3.0-only
//
// The CHIP-8 engine's suite: the assembler, and the machine the listing runs on.
//
// CHIP-8 is its own application rather than one of the Studio's languages, so its
// cases have a binary of their own and the engine is called directly instead of
// through the registry.

#include <cstdio>

#include "check.hpp"

namespace tw::lang_tests {

void test_chip8_program_starts_at_0x200();
void test_chip8_labels_resolve_forward_references();
void test_chip8_both_hex_spellings_agree();
void test_chip8_font_is_present_in_memory();
void test_chip8_db_and_dw_emit_data();
void test_chip8_stops_before_an_instruction();
void test_chip8_assembler_errors();
void test_chip8_sys_is_ignored();

void test_chip8_ld_and_add_with_a_byte();
void test_chip8_add_sets_the_carry_flag();
void test_chip8_subtract_sets_the_borrow_flag();
void test_chip8_shr_shifts_the_named_register_in_place();
void test_chip8_logic_and_shl();
void test_chip8_add_byte_wraps_without_carry();
void test_chip8_index_arithmetic();
void test_chip8_keypad_is_live();
void test_chip8_ld_key_waits_for_a_press();
void test_chip8_bcd_conversion_writes_three_digits();
void test_chip8_store_and_load_through_i();
void test_chip8_block_transfer_advances_i();
void test_chip8_skips();
void test_chip8_call_and_return();
void test_chip8_ret_without_call_is_reported();
void test_chip8_draw_lights_the_expected_pixels();
void test_chip8_drawing_twice_erases_and_reports_the_collision();
void test_chip8_pixels_wrap_around_the_screen();
void test_chip8_hires_mode_changes_the_grid_and_starts_blank();
void test_chip8_cls_in_hires_keeps_the_hi_res_grid();
void test_chip8_large_sprite_is_sixteen_by_sixteen_in_hires();
void test_chip8_large_sprite_doubles_columns_in_lores();
void test_chip8_scroll_down_moves_the_drawing_and_clears_the_top();
void test_chip8_scroll_left_and_right_move_four_pixels();
void test_chip8_exit_opcode_halts();
void test_chip8_fx30_points_at_the_large_font();
void test_chip8_large_font_is_in_memory();
void test_chip8_rpl_flags_round_trip();
void test_chip8_hires_display_halves_the_pixel_size();
void test_chip8_keys();
void test_chip8_jump_with_offset_uses_v0();
void test_chip8_timers();
void test_chip8_frame_limit_is_information();
void test_chip8_unknown_opcode_is_reported();
void test_chip8_binary_rom_loads_at_0x200();
void test_chip8_binary_rom_respects_memory();
void test_chip8_display_uses_the_palette();
void test_chip8_display_is_cleared_before_the_first_frame();
void test_chip8_run_can_be_walked_back();
void test_chip8_rewind_restores_frame_phase();

}  // namespace tw::lang_tests

int main() {
    std::printf("CHIP-8 engine tests\n");
    tw::lang_tests::test_chip8_program_starts_at_0x200();
    tw::lang_tests::test_chip8_labels_resolve_forward_references();
    tw::lang_tests::test_chip8_both_hex_spellings_agree();
    tw::lang_tests::test_chip8_font_is_present_in_memory();
    tw::lang_tests::test_chip8_db_and_dw_emit_data();
    tw::lang_tests::test_chip8_stops_before_an_instruction();
    tw::lang_tests::test_chip8_assembler_errors();
    tw::lang_tests::test_chip8_sys_is_ignored();

    tw::lang_tests::test_chip8_ld_and_add_with_a_byte();
    tw::lang_tests::test_chip8_add_sets_the_carry_flag();
    tw::lang_tests::test_chip8_subtract_sets_the_borrow_flag();
    tw::lang_tests::test_chip8_shr_shifts_the_named_register_in_place();
    tw::lang_tests::test_chip8_logic_and_shl();
    tw::lang_tests::test_chip8_add_byte_wraps_without_carry();
    tw::lang_tests::test_chip8_index_arithmetic();
    tw::lang_tests::test_chip8_keypad_is_live();
    tw::lang_tests::test_chip8_ld_key_waits_for_a_press();
    tw::lang_tests::test_chip8_bcd_conversion_writes_three_digits();
    tw::lang_tests::test_chip8_store_and_load_through_i();
    tw::lang_tests::test_chip8_block_transfer_advances_i();
    tw::lang_tests::test_chip8_skips();
    tw::lang_tests::test_chip8_call_and_return();
    tw::lang_tests::test_chip8_ret_without_call_is_reported();
    tw::lang_tests::test_chip8_draw_lights_the_expected_pixels();
    tw::lang_tests::test_chip8_drawing_twice_erases_and_reports_the_collision();
    tw::lang_tests::test_chip8_pixels_wrap_around_the_screen();
    tw::lang_tests::test_chip8_hires_mode_changes_the_grid_and_starts_blank();
    tw::lang_tests::test_chip8_cls_in_hires_keeps_the_hi_res_grid();
    tw::lang_tests::test_chip8_large_sprite_is_sixteen_by_sixteen_in_hires();
    tw::lang_tests::test_chip8_large_sprite_doubles_columns_in_lores();
    tw::lang_tests::test_chip8_scroll_down_moves_the_drawing_and_clears_the_top();
    tw::lang_tests::test_chip8_scroll_left_and_right_move_four_pixels();
    tw::lang_tests::test_chip8_exit_opcode_halts();
    tw::lang_tests::test_chip8_fx30_points_at_the_large_font();
    tw::lang_tests::test_chip8_large_font_is_in_memory();
    tw::lang_tests::test_chip8_rpl_flags_round_trip();
    tw::lang_tests::test_chip8_hires_display_halves_the_pixel_size();
    tw::lang_tests::test_chip8_keys();
    tw::lang_tests::test_chip8_jump_with_offset_uses_v0();
    tw::lang_tests::test_chip8_timers();
    tw::lang_tests::test_chip8_frame_limit_is_information();
    tw::lang_tests::test_chip8_unknown_opcode_is_reported();
    tw::lang_tests::test_chip8_binary_rom_loads_at_0x200();
    tw::lang_tests::test_chip8_binary_rom_respects_memory();
    tw::lang_tests::test_chip8_display_uses_the_palette();
    tw::lang_tests::test_chip8_display_is_cleared_before_the_first_frame();
    tw::lang_tests::test_chip8_run_can_be_walked_back();
    tw::lang_tests::test_chip8_rewind_restores_frame_phase();
    return tw::test::summary("chip8_engine_tests");
}
