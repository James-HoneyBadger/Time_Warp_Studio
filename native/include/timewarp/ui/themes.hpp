// SPDX-License-Identifier: MIT
//
// The 33 themes.
//
// The removed PySide6 implementation shipped a theme manager with 33 named
// colour schemes -- the retro sets (Amber, Green, C64, Apple II, CGA, ZX
// Spectrum) as well as the modern editor palettes (Dracula, Nord, Solarized,
// Catppuccin) and the accessibility sets. `docs/ROADMAP.md` has carried
// "themes are not ported yet" since the Qt port landed. This module is that
// work: one record per theme, transcribed from the Python's table rather than
// re-invented, so a learner who set "Amiga Workbench 2.0" before the port gets
// the same colours afterwards.
//
// #### Why a struct of `const char*`
//
// The table is data. Keeping it as plain string literals (rather than QColor
// objects) means it can live in a generated `.inc` file with no construction
// order question, and the one place that turns a theme into a palette is
// `paletteFor()`, so a colour cannot be interpreted two ways.
//
// #### What a theme does *not* own
//
// The turtle canvas surface is the program's, not the theme's: a program that
// names a background colour has asked for it. The theme supplies the *default*
// surface -- what a run draws on before it names anything -- and the canvas
// recolours to it while idle (see `MainWindow::applyTheme`).

#pragma once

#include <QColor>
#include <QPalette>
#include <QString>

#include <vector>

namespace timewarp::ui {

/// One colour scheme. Field names match the Python dataclass's, which is what
/// makes the generated table diffable against the original.
struct Theme {
    const char* name = "";
    const char* background = "";
    const char* foreground = "";
    const char* editor_bg = "";
    const char* editor_fg = "";
    const char* line_number_bg = "";
    const char* line_number_fg = "";
    const char* keyword = "";
    const char* comment = "";
    const char* string = "";
    const char* number = "";
    const char* selection_bg = "";
    const char* selection_fg = "";
    const char* canvas_bg = "";
    const char* operator_colour = "";
    const char* function = "";
    const char* variable = "";
    /// Optional in the Python; `nullptr` means "fall back", as there.
    const char* menu_bg = nullptr;
    const char* menu_fg = nullptr;
    const char* border_color = nullptr;
    const char* error_color = "";
    const char* warning_color = "";
    const char* success_color = "";
    const char* info_color = "";
    const char* bevel_light = "";
    const char* bevel_dark = "";
    const char* panel_header_bg = "";
    const char* panel_header_fg = "";
    const char* fkey_bg = "";
    const char* fkey_badge = "";
    const char* hud_accent = "";

    // The three fallbacks the Python performs at apply time. Expressed here so
    // no caller has to remember them, and so an unset field cannot produce an
    // empty colour string in a stylesheet.
    [[nodiscard]] const char* menuBackground() const noexcept {
        return menu_bg != nullptr ? menu_bg : background;
    }
    [[nodiscard]] const char* menuForeground() const noexcept {
        return menu_fg != nullptr ? menu_fg : foreground;
    }
    [[nodiscard]] const char* border() const noexcept {
        return border_color != nullptr ? border_color : line_number_bg;
    }
};

namespace themes {

/// A theme's colours, resolved rather than raw: `resolve` is the one place that
/// decides what a field means, including the accessibility adjustment. Every
/// consumer below takes this, so "what colour is the keyword" has one answer.
struct Colours {
    QColor background;
    QColor foreground;
    QColor editor_bg;
    QColor editor_fg;
    QColor line_number_bg;
    QColor line_number_fg;
    QColor menu_bg;
    QColor menu_fg;
    QColor border;
    QColor selection_bg;
    QColor selection_fg;
    QColor bevel_light;
    QColor bevel_dark;
    QColor keyword;
    QColor comment;
    QColor string;
    QColor number;
    QColor function;
    QColor error_color;
    QColor warning_color;
    QColor success_color;
    QColor info_color;
    QColor canvas_bg;
    QColor fkey_bg;
    QColor fkey_badge;
    QColor panel_header_bg;
    QColor panel_header_fg;
    QColor hud_accent;
};

/// Every theme, in the order the View menu lists them.
[[nodiscard]] const std::vector<Theme>& all();

/// The theme a fresh install gets. The flagship retro set, which is what the
/// Python defaulted to as well.
[[nodiscard]] QString defaultName();

/// `name`'s theme, or the default when the name is not one of the 33. An unknown
/// name is a stale settings value rather than a programming error, so it must
/// not be fatal -- and it must not leave the window half-themed either.
[[nodiscard]] const Theme& byName(const QString& name);

/// The theme after `name`, wrapping. What the Shift+F10 key needs.
[[nodiscard]] const Theme& next(const QString& name);

/// The colours of `theme`.
///
/// With *high_contrast*, the surfaces become black and white -- in the direction
/// the theme already goes, so a dark theme stays dark -- and every accent is
/// pushed along its own hue until it clears 7:1 against that surface. The
/// accents keep their identity (red still reads as red); what they lose is the
/// freedom to be too dim to read, which is the point.
[[nodiscard]] Colours resolve(const Theme& theme, bool high_contrast = false);

/// The application palette for a theme. Every role the base stylesheet reads is
/// set, including the `dark`/`mid`/`shadow` shades -- a palette that leaves them
/// at the default style's values paints light-theme chrome on a dark-theme
/// window.
[[nodiscard]] QPalette paletteFor(const Theme& theme, bool high_contrast = false);

/// The theme's own rules, appended after `style::window()`: the tokens a
/// palette cannot express (a menu bar that is a different colour from the
/// window, the function-key bar's badge, the editor surface).
[[nodiscard]] QString stylesheetFor(const Theme& theme, bool high_contrast = false);

/// `#rrggbb` to a QColor. Returns an invalid QColor for anything else rather
/// than silently producing black, so a typo in the table shows up in a test.
[[nodiscard]] QColor colour(const char* hex);

/// The WCAG 2 contrast ratio of two colours, 1.0 (identical) to 21.0
/// (black against white). Used by the tests to hold high contrast to its
/// promise rather than to an opinion.
[[nodiscard]] double contrastRatio(const QColor& a, const QColor& b);

}  // namespace themes
}  // namespace timewarp::ui
