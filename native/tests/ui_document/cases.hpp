// SPDX-License-Identifier: MIT
//
// The suite's cases, in the order `ui_document_tests.cpp` runs them.
//
// Written down rather than discovered: the cases share a QSettings store, a temporary
// directory and an application instance, and an order that depended on link order
// would be an order nobody chose. Keep this list and `main` in step.

#pragma once

namespace tw::ui_document {

void test_language_mapping();
void test_editor_widget();
void test_a_fresh_window_has_one_real_document();
void test_opening_a_file();
void test_opening_the_same_file_twice_raises_the_first_tab();
void test_moving_the_cursor_updates_the_status_bar();
void test_editing_marks_the_document_and_the_title();
void test_choosing_a_language_sticks();
void test_closing_tabs();
void test_saving_a_document();
void test_a_file_that_cannot_be_read_is_refused();
void test_a_byte_order_mark_is_not_part_of_the_program();
void test_running_scheme_draws_on_the_canvas();
void test_pen_up_moves_without_drawing();
void test_the_native_host_blits_a_sprite();
void test_running_basic_prints_natively();
void test_running_logo_draws_on_the_canvas();
void test_a_scheme_run_fills_the_variables_and_telemetry_panels();
void test_a_native_run_fills_the_variables_and_telemetry_panels();
void test_a_second_run_replaces_the_panels_rather_than_appending();
void test_a_native_run_fills_the_panels();
void test_the_console_can_show_only_failures();
void test_the_terminal_is_off_by_default();
void test_the_terminal_runs_commands_when_enabled();
void test_stopping_when_nothing_runs_is_harmless();
void test_a_light_theme_draws_dark_ink();
void test_stop_interrupts_a_running_program();
void test_find_next_selects_and_wraps();
void test_replace_all_is_one_step();
void test_the_find_bar_drives_the_editor();
void test_recent_files_are_remembered();
void test_the_session_comes_back();
void test_user_data_lives_where_the_environment_says();
void test_a_snippet_is_inserted_at_the_cursor();
void test_run_selection_and_history();
void test_autosave_keeps_unsaved_work();
void test_a_binary_chip8_rom_opens_read_only();
void test_word_at_cursor();
void test_context_help_follows_the_document_language();
void test_restoring_a_copy_opens_a_new_tab();
void test_the_split_survives_a_restart();
void test_a_run_pauses_at_a_breakpoint();
void test_a_whole_program_run_pauses_at_a_breakpoint();
void test_breakpoints_toggle_paint_and_persist();
void test_a_stale_breakpoint_path_is_dropped();

}  // namespace tw::ui_document
