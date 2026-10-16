// SPDX-License-Identifier: MIT
//
// The main window's stylesheet, in one place.
//
// Translated from the six scattered `setStyleSheet()` calls in the removed
// PySide6 implementation's `main_window.py`.
//
// Two deliberate changes from that implementation:
//
//  1. **One sheet, applied once, to the window.** Qt copies a widget's
//     stylesheet to its children and re-polishes all of them on every
//     `setStyleSheet()` call; six calls means six full re-polish passes over a
//     window with hundreds of widgets. Composing the sections and setting the
//     result once removes that, and gives a theme manager a single string to
//     replace.
//
//  2. **Scoped by object name.** The Python writes bare `QTabWidget { ... }`,
//     which cascades to *every* descendant tab widget -- including
//     `output_sub_tabs`, nested inside a page of the widget being styled. The
//     author had to notice this and add a compensating padding override.
//     Naming the widgets instead makes the cascade intentional rather than
//     accidental, so a tab widget added later cannot silently inherit editor
//     chrome.
//
// Colour roles are written with Qt's `palette(...)` function, so the sheet
// follows the active palette and the existing theme system keeps working.

#pragma once

#include <QString>

namespace timewarp::ui::style {

/// Every section, joined. What `MainWindow` installs.
[[nodiscard]] QString window();

// -- sections, exposed so a theme manager can compose or replace them -------

[[nodiscard]] QString windowBackground();
[[nodiscard]] QString tabBar();
[[nodiscard]] QString tabPane();
[[nodiscard]] QString splitter();
[[nodiscard]] QString pushButtons();
[[nodiscard]] QString toolbar();
[[nodiscard]] QString statusBar();
[[nodiscard]] QString statusBadges();
[[nodiscard]] QString outputSubTabs();
[[nodiscard]] QString functionKeyBar();

}  // namespace timewarp::ui::style
