// SPDX-License-Identifier: MIT
//
// The HB-1: the ROM socket, the boot screen, the combined text and
// graphics display, the prompt, the two execution paths behind it, and HB DOS.
//
// These tests are deliberately about behaviour a user can see: the banner names
// the selected ROM, the prompt changes with it, a numbered line lands in the
// program in numeric order, `RUN` prints and draws, and each ROM remembers the
// program it was left holding. A screenshot can show the machine is green; only
// these can show it is a computer.

#include <cstdio>
//
// The cases themselves are in `retro/*.cpp`, one file per part of the machine, and
// the order they run in is `retro/cases.hpp`.

#include "retro/cases.hpp"
#include "retro/support.hpp"

using namespace tw::retro;

int main(int argc, char** argv) {
    qputenv("QT_QPA_PLATFORM", "offscreen");
    // HB DOS keeps its image under the application's data directory, so point
    // that at a scratch folder rather than a learner's real one.
    const QString data_dir = QDir::tempPath() + QStringLiteral("/timewarp-retro-tests");
    QDir{data_dir}.removeRecursively();
    qputenv("TIME_WARP_DATA_DIR", data_dir.toUtf8());
    QApplication app{argc, argv};
    QCoreApplication::setOrganizationName(QStringLiteral("TimeWarpTests"));
    QCoreApplication::setApplicationName(QStringLiteral("RetroComputerTests"));
    QSettings::setDefaultFormat(QSettings::IniFormat);

    test_boots_into_basic();
    test_rom_switch_reboots_and_keeps_memory();
    test_every_rom_boots();
    test_screen_modes_show_text_and_graphics();
    test_basic_direct_mode_edits_the_program();
    test_prompt_keeps_repl_bindings();
    test_basic_run_prints_and_supports_question_mark();
    test_run_prints_draws_and_switches_text_to_split();
    test_prompt_history_recalls_the_last_line();
    test_rom_and_program_survive_a_restart();
    test_main_window_owns_the_machine_action();
    test_config_button_opens_the_settings_screen();
    test_config_prompt_command_opens_the_settings_screen();
    test_settings_preview_apply_and_persist();
    test_boot_settings_decide_the_next_power_on();
    test_remember_programs_can_be_switched_off();
    test_prompt_history_depth_is_honoured();
    test_switch_to_split_can_be_turned_off();
    test_restore_defaults_returns_a_stock_machine();
    test_memory_map_is_the_machine();
    test_the_machine_sounds_like_a_machine();
    test_a_muted_machine_is_silent_but_still_counts();
    test_poking_the_sound_latch_reaches_the_voice();
    test_the_self_test_counts_the_memory_it_has();
    test_the_power_switch_turns_the_machine_off_and_on();
    test_the_frame_register_counts_the_clock();
    test_a_poked_character_is_on_the_screen();
    test_a_graphics_character_paints_its_cell();
    test_locate_positions_the_print_that_follows();
    test_cls_clears_screen_memory();
    test_a_poke_into_rom_is_refused_out_loud();
    test_forth_poke_reaches_the_tube();
    test_forth_poke_refuses_rom();
    test_printing_past_the_bottom_scrolls_the_screen();
    test_scrolling_moves_memory_and_peek_agrees();
    test_writing_text_wraps_and_scrolls();
    test_memory_checksum_changes_when_a_byte_does();
    test_auto_numbers_the_lines_that_are_typed();
    test_renum_renumbers_the_listing_and_every_jump();
    test_renum_leaves_a_target_it_cannot_resolve_alone();
    test_delete_and_list_take_ranges();
    test_new_can_be_undone_once();
    test_tron_prints_each_line_as_it_runs();
    test_hb_dos_saves_catalogues_loads_and_runs();
    test_hb_dos_lock_protects_a_file();
    test_hb_dos_renames_and_types();
    test_hb_dos_verify_compares_memory_with_the_disk();
    test_hb_dos_init_has_to_be_said_twice();
    test_hb_dos_survives_a_restart();
    test_hb_dos_a_file_belongs_to_its_rom();
    test_hb_dos_refuses_bad_names_and_a_full_disk();
    test_llist_prints_the_program_to_the_paper_tray();
    test_lprint_prints_a_line_and_needs_one();
    test_a_machine_survives_a_round_trip_through_a_file();
    test_a_machine_image_refuses_what_it_cannot_read();
    test_the_machine_words_save_and_load_an_image();

    return tw::test::summary("retro_computer_tests");
}
