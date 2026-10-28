// SPDX-License-Identifier: MIT
//
// Structural tests for the ported main-window layout.
//
// A layout translation is easy to get "almost right" and impossible to notice:
// swap two children of a splitter, drop a `setStretchFactor`, forget
// `setChildrenCollapsible(false)`, and the window still looks correct until
// somebody resizes it and a pane they cannot recover. These tests assert the
// structure and the *sizing behaviour* rather than the pixels, so a regression
// fails here instead of in front of a user.
//
// Runs offscreen, so it needs no display and belongs in CI alongside the rest.
//
// The cases themselves are in `ui_layout/*.cpp`, one file per part of the
// window, and the order they run in is `ui_layout/cases.hpp`.

#include "timewarp/ui/command_reference.hpp"
#include "ui_layout/cases.hpp"
#include "ui_layout/support.hpp"

using namespace tw::ui_layout;

int main(int argc, char** argv) {
    // Set before QApplication exists, or Qt tries to open a real display.
    qputenv("QT_QPA_PLATFORM", "offscreen");
    QApplication app{argc, argv};
    // Isolate QSettings from the developer's real configuration.
    QCoreApplication::setOrganizationName(QStringLiteral("TimeWarpTests"));
    QCoreApplication::setApplicationName(QStringLiteral("UiLayoutTests"));
    QSettings::setDefaultFormat(QSettings::IniFormat);

    // The snippet catalogue is read through the resolver, which walks up from
    // the executable: that reaches the checkout from `native/build` and from
    // nowhere else. Where the walk finds nothing, the path CMake recorded is
    // supplied through the resolver's own override.
    if (timewarp::ui::snippetRoot().isEmpty()) {
        qputenv("TIME_WARP_SNIPPETS", QStringLiteral(TIME_WARP_TEST_SNIPPETS).toUtf8());
    }
    // The command table is read by the Reference page. Same arrangement, same
    // reason: the resolver walks up from the executable, which reaches the checkout
    // from `native/build` and from nowhere else.
    if (timewarp::ui::commandRowsPath().isEmpty()) {
        qputenv("TIME_WARP_COMMANDS", QStringLiteral(TIME_WARP_TEST_COMMANDS).toUtf8());
    }

    test_editor_tabs_refuse_to_be_sized_by_their_largest_page();
    test_enhanced_tab_bar_is_idempotent();

    test_window_frame_properties();
    test_central_column_nesting();
    test_main_splitter_geometry_and_behaviour();
    test_editor_column_is_editor_over_repl();
    test_editor_pane_splitter_holds_two_tab_groups();
    test_right_column_tabs_and_order();
    test_lesson_panel_contract();
    test_the_lesson_panel_loads_the_curriculum_without_a_bridge();
    test_lesson_panel_records_the_session();
    test_classroom_panel_contract();
    test_variables_panel_contract();
    test_telemetry_panel_contract();
    test_debug_panel_controls();
    test_output_and_graphics_share_a_persistent_split();
    test_run_both_panes_compares_them();
    test_the_split_editor_moves_documents_between_panes();
    test_project_panel_shows_groups_and_files();
    test_run_project_runs_the_entry_point_in_the_project_folder();
    test_a_run_writes_beside_its_document_without_a_project();
    test_watch_and_data_breakpoint_controls();
    test_sql_panel_runs_and_shows_results();
    test_the_command_reference_lists_the_table();
    test_assistant_panel_contract();
    test_repl_bar_runs_and_keeps_bindings();
    test_repl_bar_scheme_errors_and_language_switch();
    test_repl_basic_direct_mode();
    test_the_outline_lists_and_jumps();
    test_the_scrubber_appears_only_when_there_is_ink();
    test_turtle_command_log_spells_each_command();
    test_turtle_log_and_inspector_follow_the_canvas();
    test_output_sub_tabs();
    test_function_key_bar_is_fixed_height();
    test_function_key_layers();
    test_function_keys_are_wired();
    test_themes();
    test_high_contrast_clears_a_seven_to_one_floor();
    test_high_contrast_toggles_and_restores_the_palette();
    test_font_scale_ladder();
    test_focus_mode_hides_and_restores_the_chrome();
    test_presentation_mode_is_focus_mode_for_a_room();
    test_gif_recording_toggles_and_samples_the_canvas();
    test_command_palette();
    test_standalone_payload();
    test_standalone_player_layout();
    test_tool_bar_configuration();
    test_status_bar_badge_placement();
    test_outline_dock_starts_hidden();
    test_menu_structure();
    test_print_entries_follow_the_component();
    test_help_menu_opens_the_documentation();
    test_edit_menu_has_find_in_files();
    test_snippet_picker_filters_by_language();
    test_find_in_files_offers_replace();
    test_version_history_lists_snapshots();
    test_version_history_compares_with_the_open_document();
    test_editor_minimap_and_whitespace();
    test_every_right_column_page_has_a_key();
    test_file_menu_new_opens_a_document();
    test_tools_menu_opens_one_emulator();
    test_the_guided_tour_points_at_real_widgets();
    test_the_guided_tour_is_seen_once_and_rearmable();
    test_the_guided_tour_walks_its_steps();
    test_coach_marks_appear_once();
    test_help_menu_opens_the_tour();
    test_settings_rearms_the_tour();
    test_settings_toggle_the_terminal();
    test_file_menu_close_tab_closes_one();
    test_file_menu_lists_the_installed_examples();
    test_tab_close_button_closes_the_tab();
    test_view_menu_grid_drives_the_canvas();
    test_view_menu_zoom_scales_the_canvas();
    test_profile_run_is_opt_in();
    test_input_round_trip_reaches_the_runner();
    test_a_program_screen_reaches_the_canvas();
    test_profile_report_lists_the_measurements();
    test_run_menu_clear_output_empties_the_console();
    test_edit_menu_acts_on_whatever_has_focus();
    test_run_button_popup_is_not_empty();
    test_no_enabled_control_is_inert();
    test_unported_controls_say_so_instead_of_doing_nothing();
    test_crt_overlay_tracks_its_target();
    test_crt_settings_reach_the_overlay();
    test_screen_presets_pair_a_theme_with_its_crt_values();
    test_immediate_mode_panel_contract();

    test_windows_sound_command_line();
    test_splitter_positions_survive_a_restart();
    test_left_column_gets_the_larger_share_on_growth();
    return tw::test::summary("ui_layout");
}
