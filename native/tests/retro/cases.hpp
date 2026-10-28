// SPDX-License-Identifier: MIT
//
// The suite's cases, in the order `retro_computer_tests.cpp` runs them.
//
// Written down rather than discovered: every case wipes the machine's QSettings and
// its disk image first, so the order is part of what it means. Keep this list and
// `main` in step.

#pragma once

namespace tw::retro {

void test_boots_into_basic();
void test_rom_switch_reboots_and_keeps_memory();
void test_every_rom_boots();
void test_basic_run_prints_and_supports_question_mark();
void test_forth_poke_reaches_the_tube();
void test_forth_poke_refuses_rom();
void test_screen_modes_show_text_and_graphics();
void test_basic_direct_mode_edits_the_program();
void test_prompt_keeps_repl_bindings();
void test_run_prints_draws_and_switches_text_to_split();
void test_prompt_history_recalls_the_last_line();
void test_rom_and_program_survive_a_restart();
void test_main_window_owns_the_machine_action();
void test_config_button_opens_the_settings_screen();
void test_config_prompt_command_opens_the_settings_screen();
void test_settings_preview_apply_and_persist();
void test_boot_settings_decide_the_next_power_on();
void test_remember_programs_can_be_switched_off();
void test_prompt_history_depth_is_honoured();
void test_switch_to_split_can_be_turned_off();
void test_restore_defaults_returns_a_stock_machine();
void test_memory_map_is_the_machine();
void test_scrolling_moves_memory_and_peek_agrees();
void test_writing_text_wraps_and_scrolls();
void test_memory_checksum_changes_when_a_byte_does();
void test_a_poked_character_is_on_the_screen();
void test_locate_positions_the_print_that_follows();
void test_cls_clears_screen_memory();
void test_a_poke_into_rom_is_refused_out_loud();
void test_printing_past_the_bottom_scrolls_the_screen();
void test_a_graphics_character_paints_its_cell();
void test_the_machine_sounds_like_a_machine();
void test_a_muted_machine_is_silent_but_still_counts();
void test_poking_the_sound_latch_reaches_the_voice();
void test_the_self_test_counts_the_memory_it_has();
void test_the_power_switch_turns_the_machine_off_and_on();
void test_the_frame_register_counts_the_clock();
void test_auto_numbers_the_lines_that_are_typed();
void test_renum_renumbers_the_listing_and_every_jump();
void test_renum_leaves_a_target_it_cannot_resolve_alone();
void test_delete_and_list_take_ranges();
void test_new_can_be_undone_once();
void test_tron_prints_each_line_as_it_runs();
void test_hb_dos_saves_catalogues_loads_and_runs();
void test_hb_dos_lock_protects_a_file();
void test_hb_dos_renames_and_types();
void test_hb_dos_verify_compares_memory_with_the_disk();
void test_hb_dos_init_has_to_be_said_twice();
void test_hb_dos_survives_a_restart();
void test_hb_dos_a_file_belongs_to_its_rom();
void test_a_machine_survives_a_round_trip_through_a_file();
void test_a_machine_image_refuses_what_it_cannot_read();
void test_the_machine_words_save_and_load_an_image();
void test_hb_dos_refuses_bad_names_and_a_full_disk();

void test_llist_prints_the_program_to_the_paper_tray();
void test_lprint_prints_a_line_and_needs_one();

}  // namespace tw::retro
