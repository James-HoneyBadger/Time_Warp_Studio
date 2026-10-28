// SPDX-License-Identifier: MIT
//
// The suite's cases, in the order `ui_layout_tests.cpp` runs them.
//
// The order is written down rather than discovered: the cases share a QSettings
// store and a snippet catalogue, and an order that depended on link order would
// be an order nobody chose. Keep this list and `main` in step.

#pragma once

namespace tw::ui_layout {

void test_editor_tabs_refuse_to_be_sized_by_their_largest_page();
void test_enhanced_tab_bar_is_idempotent();
void test_window_frame_properties();
void test_central_column_nesting();
void test_main_splitter_geometry_and_behaviour();
void test_editor_column_is_editor_over_repl();
void test_editor_pane_splitter_holds_two_tab_groups();
void test_lesson_panel_contract();
void test_the_lesson_panel_loads_the_curriculum_without_a_bridge();
void test_lesson_panel_records_the_session();
void test_classroom_panel_contract();
void test_variables_panel_contract();
void test_telemetry_panel_contract();
void test_debug_panel_controls();
void test_right_column_tabs_and_order();
void test_output_and_graphics_share_a_persistent_split();
void test_run_both_panes_compares_them();
void test_the_split_editor_moves_documents_between_panes();
void test_the_outline_lists_and_jumps();
void test_the_scrubber_appears_only_when_there_is_ink();
void test_turtle_command_log_spells_each_command();
void test_turtle_log_and_inspector_follow_the_canvas();
void test_output_sub_tabs();
void test_function_key_bar_is_fixed_height();
void test_function_key_layers();
void test_function_keys_are_wired();
void test_themes();
void test_high_contrast_clears_a_seven_to_one_floor();
void test_high_contrast_toggles_and_restores_the_palette();
void test_font_scale_ladder();
void test_focus_mode_hides_and_restores_the_chrome();
void test_presentation_mode_is_focus_mode_for_a_room();
void test_gif_recording_toggles_and_samples_the_canvas();
void test_export_audio_records_a_runs_notes();
void test_examples_gallery_renders_a_program();
void test_notebook_round_trips_and_runs_a_cell();
void test_command_palette();
void test_standalone_player_layout();
void test_standalone_payload();
void test_tool_bar_configuration();
void test_status_bar_badge_placement();
void test_outline_dock_starts_hidden();
void test_menu_structure();
void test_print_entries_follow_the_component();
void test_help_menu_opens_the_documentation();
void test_editor_minimap_and_whitespace();
void test_a_suggested_line_can_be_taken_or_refused();
void test_typing_at_several_cursors();
void test_the_typing_pause_asks_for_the_text_before_the_caret();
void test_find_in_files_offers_replace();
void test_version_history_lists_snapshots();
void test_version_history_compares_with_the_open_document();
void test_snippet_picker_filters_by_language();
void test_edit_menu_has_find_in_files();
void test_every_right_column_page_has_a_key();
void test_file_menu_new_opens_a_document();
void test_file_menu_close_tab_closes_one();
void test_tools_menu_opens_one_emulator();
void test_the_guided_tour_points_at_real_widgets();
void test_the_guided_tour_is_seen_once_and_rearmable();
void test_the_guided_tour_walks_its_steps();
void test_coach_marks_appear_once();
void test_help_menu_opens_the_tour();
void test_settings_rearms_the_tour();
void test_settings_toggle_the_terminal();
void test_tab_close_button_closes_the_tab();
void test_file_menu_lists_the_installed_examples();
void test_view_menu_grid_drives_the_canvas();
void test_view_menu_zoom_scales_the_canvas();
void test_profile_run_is_opt_in();
void test_input_round_trip_reaches_the_runner();
void test_a_program_screen_reaches_the_canvas();
void test_profile_report_lists_the_measurements();
void test_edit_menu_acts_on_whatever_has_focus();
void test_run_menu_clear_output_empties_the_console();
void test_run_button_popup_is_not_empty();
void test_no_enabled_control_is_inert();
void test_unported_controls_say_so_instead_of_doing_nothing();
void test_crt_overlay_tracks_its_target();
void test_crt_settings_reach_the_overlay();
void test_screen_presets_pair_a_theme_with_its_crt_values();
void test_immediate_mode_panel_contract();
void test_windows_sound_command_line();
void test_splitter_positions_survive_a_restart();
void test_left_column_gets_the_larger_share_on_growth();
void test_project_panel_shows_groups_and_files();
void test_run_project_runs_the_entry_point_in_the_project_folder();
void test_a_run_writes_beside_its_document_without_a_project();
void test_watch_and_data_breakpoint_controls();
void test_sql_panel_runs_and_shows_results();
void test_the_new_pages_name_what_they_show();
void test_the_command_reference_lists_the_table();
void test_assistant_panel_contract();
void test_repl_bar_runs_and_keeps_bindings();
void test_repl_basic_direct_mode();
void test_repl_bar_scheme_errors_and_language_switch();

}  // namespace tw::ui_layout
