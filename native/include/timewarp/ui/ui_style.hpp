// SPDX-License-Identifier: MIT
//
// The window's stylesheet.
//
// One sheet, composed from `design::Tokens`, applied once per theme change. It
// reads no `palette(...)` roles: a palette can say what colour body text is, but
// it cannot say that a toolbar is the window lifted six percent, that a hover is
// a wash of the accent at 14% alpha, or that a dock title is the muted rank at
// eleven pixels. Those relationships are the interface, so they are derived in
// `design::tokens()` and written here as literal values.
//
// #### What is deliberately *not* styled
//
// Icons are not drawn from a stylesheet: Qt has no way to recolour an image in
// QSS, and an `image: url(...)` per theme per state is thirty-five themes times
// three states of a PNG set. They come from `icons::glyph()`, whose icon engine
// asks the palette at paint time -- see `icons.cpp`. Check marks, radio dots and
// spin arrows stay native for the same reason: a custom `::indicator` without an
// image is an invisible control, and an image per theme is a set of files to keep
// in step. What those controls get instead is a frame and spacing that match the
// rest, so the native marks sit inside the design rather than beside it.
//
// #### Scoping
//
// Sections that apply to the whole window are unscoped on purpose -- every tab
// bar should match, including the output sub-tabs. Where one widget needs to
// differ, it is named: `#editorTabs`, `#outputSubTabs`, `#runButton`.

#pragma once

#include "timewarp/ui/design.hpp"

#include <QString>

namespace timewarp::ui::style {

/// The whole sheet for a set of tokens. What `MainWindow` installs.
[[nodiscard]] QString sheet(const design::Tokens& tokens);

// -- sections, exposed so a caller can compose or replace one ----------------

[[nodiscard]] QString base(const design::Tokens& tokens);
[[nodiscard]] QString menus(const design::Tokens& tokens);
[[nodiscard]] QString toolbars(const design::Tokens& tokens);
[[nodiscard]] QString tabs(const design::Tokens& tokens);
[[nodiscard]] QString panes(const design::Tokens& tokens);
[[nodiscard]] QString buttons(const design::Tokens& tokens);
[[nodiscard]] QString fields(const design::Tokens& tokens);
[[nodiscard]] QString views(const design::Tokens& tokens);
[[nodiscard]] QString panels(const design::Tokens& tokens);
[[nodiscard]] QString scrollBars(const design::Tokens& tokens);
[[nodiscard]] QString statusBar(const design::Tokens& tokens);
[[nodiscard]] QString functionKeyBar(const design::Tokens& tokens);

}  // namespace timewarp::ui::style
