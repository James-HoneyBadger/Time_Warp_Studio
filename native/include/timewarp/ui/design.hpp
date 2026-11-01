// SPDX-License-Identifier: MIT
//
// The interface's design tokens.
//
// A theme names about thirty colours: a background, a foreground, a border, a
// selection, a handful of syntax accents and a few panel chrome values. That is
// the right vocabulary for colouring syntax and painting a window, and the wrong
// one for building an interface out of, because a modern surface is not made of
// named colours. It is made of *relationships*: a toolbar is the window lifted
// 5%, a panel is the window pulled toward the editor's surface, a hover is the
// raised surface again, a muted label is the text pulled 40% toward its
// background, a wash is the accent at 14% alpha. None of those exist in a theme,
// and all of them have to be derived the same way for all thirty-five of them.
//
// So this file holds the vocabulary the stylesheet actually speaks -- surfaces,
// lines, text ranks, one accent and its states, and the numbers the layout is
// built on -- and `tokens()` derives it from whatever the theme supplied. The
// stylesheet then reads tokens only, which is what lets the chrome be restyled
// once rather than per theme, and is why a theme added to the table tomorrow
// arrives with a coherent toolbar, dock, tab strip and menu without its record
// gaining a single field.
//
// Two rules make a derived palette safe to apply to a retro set:
//
//  1. **The direction is the theme's.** A dark theme's raised surfaces are
//     lighter; a light theme's are darker. Nothing here tries to make a theme
//     something it is not.
//  2. **The theme's own colours win where they exist.** The accent is the
//     theme's syntax keyword (its most characteristic colour), the editor and
//     canvas surfaces are the theme's, and a high-contrast theme's accents are
//     already pushed to a 7:1 floor by `themes::resolve()` -- the derivations
//     here only fade between colours, never toward illegibility.

#pragma once

#include "timewarp/ui/themes.hpp"

#include <QColor>
#include <QFont>

namespace timewarp::ui::design {

/// The numbers. Every length the stylesheet writes comes from here rather than
/// from a literal in a rule, because a horizontal rhythm that is 6px in the
/// toolbar, 7px in a menu and 5px in a tab is the difference between an interface
/// that looks drawn and one that looks assembled.
struct Metrics {
    /// Corner radii. `radius` is the default for anything a user can click.
    int radius = 6;
    int radius_small = 4;
    int radius_large = 10;

    /// The fixed heights, in device-independent pixels.
    int bar_height = 40;
    int control_height = 26;
    int key_height = 22;

    /// Spacing steps. Everything horizontal is one of these three.
    int gap_small = 4;
    int gap = 8;
    int gap_large = 12;

    /// Type. Sizes are in CSS pixels because that is what a stylesheet takes;
    /// `font_small` is the rank used for dock titles, badges and keycaps, and
    /// `font_body` is left to the application font.
    int font_small = 11;
};

/// One resolved set of interface colours, plus the metrics above.
///
/// The alpha-carrying entries (`accent_wash`) are meant to be painted over a
/// surface, not compared against one.
struct Tokens {
    // Surfaces, from the back of the window forward.
    QColor window;  ///< Behind everything: the gap between docks.
    QColor chrome;  ///< Menus, toolbars, status bar, dock titles.
    QColor panel;   ///< Docks, the right-hand pages, list surfaces.
    QColor editor;       ///< The code and text surfaces.
    QColor editor_text;  ///< Text on `editor`, which a theme sets separately.
    QColor raised;  ///< A hover, or a control that wants to sit above its bar.
    QColor sunken;  ///< A field: text entry, a combo, a keycap.

    // Lines.
    QColor border;         ///< The hairline between two surfaces.
    QColor border_strong;  ///< A border that has to be seen: hover, focus ring.

    // Text, in three ranks.
    QColor text;
    QColor text_muted;
    QColor text_disabled;

    // The one accent, and its states.
    QColor accent;
    QColor accent_hover;
    QColor accent_pressed;
    QColor accent_text;  ///< Text or glyph drawn *on* the accent.
    QColor accent_wash;  ///< The accent at low alpha, for selected rows and tabs.

    /// The theme's own selection colours. Kept separate from the accent because
    /// they answer a different question: the accent says "this is the thing to
    /// press", the selection says "this text is chosen", and a theme that picked
    /// an orange block for the second did not mean it for the first.
    QColor selection;
    QColor selection_text;

    // States, taken from the theme's own.
    QColor danger;
    QColor warning;
    QColor success;
    QColor info;

    // Scrollbars.
    QColor scroll;
    QColor scroll_hover;

    /// The turtle canvas's default paper, before a program names its own.
    QColor canvas;

    Metrics metrics;
};

/// The tokens for a theme. With *high_contrast*, the surfaces are flattened onto
/// the theme's extreme and the text ranks stop being dimmed, so the accessibility
/// variant keeps the contrast it promises instead of having it faded back out by
/// a hover state.
[[nodiscard]] Tokens tokens(const themes::Colours& colours, bool high_contrast = false);

/// The interface font: a ranked fallback list, so a machine with a modern UI face
/// gets it and a machine without one lands on its own sans rather than on
/// whatever Qt picks first.
///
/// *point_size* of zero keeps whatever size the application font has, which is
/// the right default: the size is the user's setting, not ours.
[[nodiscard]] QFont uiFont(int point_size = 0);

/// The same ranked-fallback treatment for fixed-width text: the console, the
/// editor, the turtle log. `Courier New` was the removed PySide6 build's choice
/// and is not installed on a stock Linux machine, so it fell through to whatever
/// Qt resolved -- a different font per distribution, for the one surface where
/// column alignment is the point.
[[nodiscard]] QFont monoFont(int point_size = 0);

/// `from` moved `t` of the way to `to`, alpha excluded. The one mixing function,
/// so every derived shade in the interface is derived the same way.
[[nodiscard]] QColor blend(const QColor& from, const QColor& to, double t);

/// `colour` at `alpha` (0-255).
[[nodiscard]] QColor faded(const QColor& colour, int alpha);

}  // namespace timewarp::ui::design
