// SPDX-License-Identifier: MIT

#include "timewarp/ui/design.hpp"

#include <QApplication>
#include <QFontDatabase>

#include <algorithm>
#include <cmath>

namespace timewarp::ui::design {

namespace {

/// WCAG relative luminance: the arithmetic behind "black or white on this
/// colour", as opposed to HSL lightness, which calls a saturated blue light.
[[nodiscard]] double relativeLuminance(const QColor& colour) {
    const auto channel = [](double value) {
        const double srgb = value / 255.0;
        return srgb <= 0.03928 ? srgb / 12.92 : std::pow((srgb + 0.055) / 1.055, 2.4);
    };
    return 0.2126 * channel(colour.red()) + 0.7152 * channel(colour.green()) +
           0.0722 * channel(colour.blue());
}

/// `colour` stepped away from `surface` until it clears `target` contrast.
///
/// The derivations above fade between colours, which is right for surfaces and
/// wrong for anything a person has to read: a mid-grey theme such as Amiga
/// Workbench takes its accent from a keyword chosen to sit on a *navy* editor,
/// and that orange is 1.6:1 on the grey window it ends up on here. Stepping toward
/// black or white -- whichever the surface is further from -- keeps the hue while
/// making the colour legible, which is the part a theme cannot know about.
[[nodiscard]] QColor reachContrast(QColor colour, const QColor& surface, double target) {
    const QColor away = themes::contrastRatio(surface, Qt::black) >
                                themes::contrastRatio(surface, Qt::white)
                            ? QColor{0, 0, 0}
                            : QColor{255, 255, 255};
    for (int step = 0; step < 12 && themes::contrastRatio(colour, surface) < target; ++step) {
        colour = blend(colour, away, 0.16);
    }
    return colour;
}

/// Whether the theme is a dark one. Lightness is what the rest of the codebase
/// asks (`ui_layout_appearance_tests` included), so it is what decides the
/// direction of every derived shade.
[[nodiscard]] bool isDark(const QColor& background) { return background.lightness() < 128; }

}  // namespace

QColor blend(const QColor& from, const QColor& to, double t) {
    const double amount = std::clamp(t, 0.0, 1.0);
    const auto mix = [amount](int a, int b) {
        return static_cast<int>(std::lround(a + (b - a) * amount));
    };
    return QColor{mix(from.red(), to.red()), mix(from.green(), to.green()),
                  mix(from.blue(), to.blue())};
}

QColor faded(const QColor& colour, int alpha) {
    QColor result = colour;
    result.setAlpha(std::clamp(alpha, 0, 255));
    return result;
}

Tokens tokens(const themes::Colours& colours, bool high_contrast) {
    Tokens t;
    const bool dark = isDark(colours.background);
    // What "lighter" means for this theme. Everything raised is lifted toward it.
    const QColor lift = dark ? QColor{255, 255, 255} : QColor{0, 0, 0};

    t.window = colours.background;
    t.editor = colours.editor_bg;
    t.editor_text = colours.editor_fg;
    t.panel = blend(colours.background, colours.editor_bg, 0.6);
    // A theme whose background and editor are the same colour would give panels
    // and the window one surface; the panel steps away from the window instead.
    if (themes::contrastRatio(t.panel, colours.background) < 1.06) {
        t.panel = blend(colours.background, lift, dark ? 0.05 : 0.04);
    }
    t.chrome = blend(colours.background, lift, dark ? 0.06 : 0.035);
    t.raised = blend(t.chrome, lift, dark ? 0.08 : 0.055);
    // Fields take the window's surface, not the editor's: a theme whose editor is
    // a terminal navy on a grey window (Amiga Workbench, the Borland blue) put a
    // navy combo box on a grey toolbar, with the window's black text on it. A field
    // is a hole in the surface it sits in.
    t.sunken = dark ? blend(colours.background, QColor{0, 0, 0}, 0.22)
                    : blend(colours.background, QColor{255, 255, 255}, 0.55);

    // A hairline nobody can see is not a separator. Several themes carry no
    // border colour of their own, so `Colours::border` falls back to the gutter
    // colour -- which on a white theme is a shade off the page, and leaves panels,
    // tabs and fields floating on the same white. Below a visible threshold the
    // border is derived from the text colour instead, which is the one colour a
    // theme is guaranteed to have chosen against its background.
    t.border = themes::contrastRatio(colours.border, colours.background) < 1.2
                   ? blend(colours.background, colours.foreground, 0.18)
                   : colours.border;
    t.border_strong = blend(t.border, colours.foreground, 0.45);

    t.text = colours.foreground;
    t.text_muted = reachContrast(blend(colours.foreground, t.window, dark ? 0.42 : 0.38),
                                 t.chrome, 2.6);
    t.text_disabled = blend(colours.foreground, t.window, dark ? 0.66 : 0.6);

    // The accent: the theme's syntax keyword, which is the one colour every theme
    // in the table chose to be recognisable with. It falls back to the theme's
    // info colour only where the keyword is too close to the background to read as
    // a control -- a white keyword on a white background is a theme that wants a
    // different accent, not a button nobody can see.
    QColor accent = colours.keyword;
    if (themes::contrastRatio(accent, colours.background) < 3.0) {
        accent = colours.info_color;
    }
    if (themes::contrastRatio(accent, colours.background) < 2.0) {
        accent = colours.hud_accent;
    }
    accent = reachContrast(accent, t.chrome, 3.0);
    t.accent = accent;
    t.accent_hover = blend(accent, lift, dark ? 0.18 : 0.15);
    t.accent_pressed = blend(accent, dark ? QColor{0, 0, 0} : QColor{0, 0, 0}, 0.22);
    // Black or white on the accent. The threshold is luminance rather than the
    // higher of the two contrast ratios: a saturated mid-blue takes black by the
    // ratio (7:1) and looks wrong by every other measure, while white on it is
    // what every design in the last decade does.
    t.accent_text = relativeLuminance(accent) < 0.4 ? QColor{255, 255, 255}
                                                    : QColor{0, 0, 0};
    t.accent_wash = faded(accent, 40);
    t.selection = colours.selection_bg;
    // A theme may name a selection foreground that does not clear the floor on
    // its own selection colour; the text rank wins over the pairing.
    t.selection_text = reachContrast(colours.selection_fg, t.selection, 4.5);

    t.danger = colours.error_color;
    t.warning = colours.warning_color;
    t.success = colours.success_color;
    t.info = colours.info_color;

    t.scroll = blend(t.border, colours.foreground, 0.18);
    t.scroll_hover = blend(t.border, colours.foreground, 0.42);

    t.canvas = colours.canvas_bg;

    if (high_contrast) {
        // The variant's promise is contrast, so nothing here may fade a surface
        // toward its own text: the bars keep the theme's extreme, and the three
        // text ranks collapse to the foreground.
        t.window = colours.background;
        t.panel = colours.background;
        t.editor = colours.editor_bg;
        t.editor_text = colours.editor_fg;
        t.chrome = colours.background;
        t.raised = blend(colours.background, colours.foreground, 0.12);
        t.sunken = colours.background;
        t.border = blend(colours.foreground, colours.background, 0.4);
        t.border_strong = colours.foreground;
        t.text_muted = colours.foreground;
        t.text_disabled = blend(colours.foreground, colours.background, 0.3);
        t.accent_wash = faded(accent, 60);
        t.scroll = blend(colours.foreground, colours.background, 0.35);
        t.scroll_hover = colours.foreground;
    }

    return t;
}

QFont uiFont(int point_size) {
    QFont font = QApplication::font();
    font.setFamilies({QStringLiteral("Inter"), QStringLiteral("SF Pro Text"),
                      QStringLiteral("Segoe UI Variable Text"),
                      QStringLiteral("Cantarell"), QStringLiteral("Noto Sans"),
                      QStringLiteral("Ubuntu"), QStringLiteral("DejaVu Sans")});
    if (point_size > 0) {
        font.setPointSize(point_size);
    }
    return font;
}

QFont monoFont(int point_size) {
    QFont font = QFontDatabase::systemFont(QFontDatabase::FixedFont);
    font.setFamilies({QStringLiteral("JetBrains Mono"), QStringLiteral("Cascadia Code"),
                      QStringLiteral("Fira Code"), QStringLiteral("Source Code Pro"),
                      QStringLiteral("Ubuntu Mono"), QStringLiteral("Noto Sans Mono"),
                      QStringLiteral("DejaVu Sans Mono"),
                      QStringLiteral("Liberation Mono")});
    font.setStyleHint(QFont::Monospace);
    if (point_size > 0) {
        font.setPointSize(point_size);
    }
    return font;
}

}  // namespace timewarp::ui::design
