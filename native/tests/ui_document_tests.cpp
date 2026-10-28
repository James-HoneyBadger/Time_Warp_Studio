// SPDX-License-Identifier: MIT
//
// Tests for the editor and the document lifecycle.
//
// These cover the parts of the IDE that touch the user's files, so the
// interesting cases are the ones where getting it wrong loses work: opening a
// file that is already open, a byte-order mark left in the first line, saving
// over a file that is being written, and the unsaved-changes prompt that decides
// whether a close destroys anything.
//
// Runs offscreen, so it needs no display and belongs in CI alongside the rest.
//
// #### What is deliberately not tested here
//
// Any path that ends in a modal dialog. `QFileDialog` and `QMessageBox` block
// until a person answers them, so a test that triggers one does not fail, it
// hangs -- and a hang in CI is worse than a missing assertion. The unreadable
// file and cancelled-save paths are therefore covered by hand and described in
// the README rather than asserted here. Everything else in the lifecycle is
// exercised through the same entry points the dialogs call.
//
// The cases themselves are in `ui_document/*.cpp`, one file per part of the
// lifecycle, and the order they run in is `ui_document/cases.hpp`.

#include "ui_document/cases.hpp"
#include "ui_document/support.hpp"

using namespace tw::ui_document;

int main(int argc, char** argv) {
    // Set before QApplication exists, or Qt tries to open a real display.
    qputenv("QT_QPA_PLATFORM", "offscreen");
    QApplication app{argc, argv};
    // A settings identity of its own, so a test run cannot touch the real one.
    QCoreApplication::setOrganizationName(QStringLiteral("TimeWarpTests"));
    QCoreApplication::setApplicationName(QStringLiteral("UiDocumentTests"));
    QSettings::setDefaultFormat(QSettings::IniFormat);

    test_language_mapping();
    test_editor_widget();
    test_find_next_selects_and_wraps();
    test_replace_all_is_one_step();
    test_the_find_bar_drives_the_editor();
    test_recent_files_are_remembered();
    test_the_session_comes_back();

    test_a_fresh_window_has_one_real_document();
    test_opening_a_file();
    test_opening_the_same_file_twice_raises_the_first_tab();
    test_moving_the_cursor_updates_the_status_bar();
    test_editing_marks_the_document_and_the_title();
    test_choosing_a_language_sticks();
    test_closing_tabs();
    test_saving_a_document();
    test_a_file_that_cannot_be_read_is_refused();
    test_a_byte_order_mark_is_not_part_of_the_program();
    test_user_data_lives_where_the_environment_says();
    test_a_snippet_is_inserted_at_the_cursor();
    test_run_selection_and_history();
    test_autosave_keeps_unsaved_work();
    test_restoring_a_copy_opens_a_new_tab();
    test_a_binary_chip8_rom_opens_read_only();
    test_word_at_cursor();
    test_context_help_follows_the_document_language();
    test_breakpoints_toggle_paint_and_persist();
    test_a_stale_breakpoint_path_is_dropped();
    test_the_split_survives_a_restart();
    test_a_run_pauses_at_a_breakpoint();
    test_a_whole_program_run_pauses_at_a_breakpoint();

    test_running_scheme_draws_on_the_canvas();
    test_pen_up_moves_without_drawing();
    test_running_basic_prints_natively();
    test_the_native_host_blits_a_sprite();
    test_running_logo_draws_on_the_canvas();
    test_a_scheme_run_fills_the_variables_and_telemetry_panels();
    test_a_native_run_fills_the_variables_and_telemetry_panels();
    test_a_second_run_replaces_the_panels_rather_than_appending();
    test_a_native_run_fills_the_panels();
    test_stopping_when_nothing_runs_is_harmless();
    test_a_light_theme_draws_dark_ink();
    test_stop_interrupts_a_running_program();
    test_the_console_can_show_only_failures();
    test_the_terminal_is_off_by_default();
    test_the_terminal_runs_commands_when_enabled();

    return tw::test::summary("ui_documents");
}
