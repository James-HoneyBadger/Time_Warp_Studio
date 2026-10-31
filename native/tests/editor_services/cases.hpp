// SPDX-License-Identifier: MIT
//
// The text transforms that arrived with the terminal, the notification and the
// homebrew catalogue.
//
// `editor_services_tests.cpp` is one binary with two files rather than one long
// file: it is the same suite, and the split is because a file that grows past its
// budget is a file doing more than one thing (`time_warp_gates structure` is what
// says so).

#pragma once

namespace tw::editor_services {

/// The pty's byte stream: escape sequences, carriage returns, split characters.
void test_terminal_text_strips_escapes();
void test_terminal_text_applies_carriage_returns();
void test_terminal_text_survives_split_characters();

/// What a finished run's notification says.
void test_the_finish_notification_says_one_thing();

/// The homebrew catalogue and the rules that keep a download inside its folder.
void test_the_archive_catalogue_is_a_cleared_list();

}  // namespace tw::editor_services
