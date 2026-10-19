// SPDX-License-Identifier: MIT

#include "timewarp/ui/themes.hpp"

#include <QStringList>

#include <algorithm>
#include <cmath>
#include <iterator>

namespace timewarp::ui::themes {

namespace {

// The table itself: 33 `Theme` records, generated from the removed PySide6
// implementation's `ThemeManager.themes` so the colours are the ones the
// application used to show rather than a fresh approximation of them.
#include "theme_table.inc"

// The whole point of a table like this is that it is complete; the asserts are
// compile-time so a truncated generation is a build error rather than a theme
// that silently resolves to the fallback.
static_assert(std::size(kThemes) == 33, "the theme table is 33 themes");
static_assert(std::size(kThemes) > 0, "the theme table is not empty");

constexpr std::size_t kThemeCount = std::size(kThemes);

const Theme& at(std::size_t index) { return kThemes[index % kThemeCount]; }

}  // namespace

const std::vector<Theme>& all() {
    // Built once. A vector rather than exposing the array keeps the header free
    // of the generated table's size, and the table is small enough that the
    // allocation is not worth avoiding.
    static const std::vector<Theme> table{std::begin(kThemes), std::end(kThemes)};
    return table;
}

QString defaultName() { return QString::fromUtf8(at(0).name); }

const Theme& byName(const QString& name) {
    for (const Theme& theme : all()) {
        if (name == QLatin1String(theme.name)) {
            return theme;
        }
    }
    // A name that is not one of the 33 is a stale settings value, not a bug:
    // launching with a theme this build no longer has must still produce a
    // usable window.
    return at(0);
}

const Theme& next(const QString& name) {
    for (std::size_t index = 0; index < kThemeCount; ++index) {
        if (name == QLatin1String(at(index).name)) {
            return at(index + 1);
        }
    }
    return at(0);
}

QColor colour(const char* hex) {
    // Only `#rrggbb`. `QColor` also accepts names and `#aarrggbb`, and a table
    // that used either would be a transcription mistake worth catching.
    if (hex == nullptr) {
        return QColor{};
    }
    const QString text = QString::fromUtf8(hex);
    if (text.size() != 7 || text.at(0) != QLatin1Char('#')) {
        return QColor{};
    }
    return QColor{text};
}

double contrastRatio(const QColor& a, const QColor& b) {
    // The WCAG relative luminance: sRGB channels linearised, weighted by the eye's
    // sensitivity, then the ratio of the lighter to the darker.
    const auto channel = [](double value) {
        return value <= 0.03928 ? value / 12.92 : std::pow((value + 0.055) / 1.055, 2.4);
    };
    const auto luminance = [&channel](const QColor& colour) {
        return 0.2126 * channel(colour.redF()) + 0.7152 * channel(colour.greenF()) +
               0.0722 * channel(colour.blueF());
    };
    const double first = luminance(a);
    const double second = luminance(b);
    const double lighter = std::max(first, second);
    const double darker = std::min(first, second);
    return (lighter + 0.05) / (darker + 0.05);
}

namespace {

/// `colour`, pushed along its own hue until it clears `target` against
/// `background`. The hue is what makes a keyword look like a keyword, so it is
/// the one thing that survives; brightness is what accessibility owns.
[[nodiscard]] QColor liftToContrast(const QColor& colour, const QColor& background,
                                    double target = 7.0) {
    if (!colour.isValid()) {
        // A missing colour is not something to guess at: the foreground is the one
        // value guaranteed to be legible on the background.
        return background.lightness() < 128 ? QColor{Qt::white} : QColor{Qt::black};
    }
    if (contrastRatio(colour, background) >= target) {
        return colour;
    }
    const bool towards_white = background.lightness() < 128;
    for (int step = 1; step <= 20; ++step) {
        const double t = step / 20.0;
        const double red = towards_white ? colour.redF() + (1.0 - colour.redF()) * t
                                         : colour.redF() * (1.0 - t);
        const double green = towards_white ? colour.greenF() + (1.0 - colour.greenF()) * t
                                           : colour.greenF() * (1.0 - t);
        const double blue = towards_white ? colour.blueF() + (1.0 - colour.blueF()) * t
                                          : colour.blueF() * (1.0 - t);
        QColor mixed;
        mixed.setRgbF(red, green, blue);
        if (contrastRatio(mixed, background) >= target) {
            return mixed;
        }
    }
    return towards_white ? QColor{Qt::white} : QColor{Qt::black};
}

}  // namespace

Colours resolve(const Theme& theme, bool high_contrast) {
    Colours colours;
    colours.background = colour(theme.background);
    colours.foreground = colour(theme.foreground);
    colours.editor_bg = colour(theme.editor_bg);
    colours.editor_fg = colour(theme.editor_fg);
    colours.line_number_bg = colour(theme.line_number_bg);
    colours.line_number_fg = colour(theme.line_number_fg);
    colours.menu_bg = colour(theme.menuBackground());
    colours.menu_fg = colour(theme.menuForeground());
    colours.border = colour(theme.border());
    colours.selection_bg = colour(theme.selection_bg);
    colours.selection_fg = colour(theme.selection_fg);
    colours.bevel_light = colour(theme.bevel_light);
    colours.bevel_dark = colour(theme.bevel_dark);
    colours.keyword = colour(theme.keyword);
    colours.comment = colour(theme.comment);
    colours.string = colour(theme.string);
    colours.number = colour(theme.number);
    colours.function = colour(theme.function);
    colours.error_color = colour(theme.error_color);
    colours.warning_color = colour(theme.warning_color);
    colours.success_color = colour(theme.success_color);
    colours.info_color = colour(theme.info_color);
    colours.canvas_bg = colour(theme.canvas_bg);
    colours.fkey_bg = colour(theme.fkey_bg);
    colours.fkey_badge = colour(theme.fkey_badge);
    colours.panel_header_bg = colour(theme.panel_header_bg);
    colours.panel_header_fg = colour(theme.panel_header_fg);
    colours.hud_accent = colour(theme.hud_accent);

    if (!high_contrast) {
        return colours;
    }

    // Black on white or white on black, in the direction the theme already goes:
    // a user who reads light-on-dark is not asking to be flipped to black-on-white
    // when they ask for more contrast.
    const bool dark = colours.background.lightness() < 128;
    const QColor surface = dark ? QColor{Qt::black} : QColor{Qt::white};
    const QColor ink = dark ? QColor{Qt::white} : QColor{Qt::black};

    colours.background = surface;
    colours.foreground = ink;
    colours.editor_bg = surface;
    colours.editor_fg = ink;
    colours.canvas_bg = surface;
    colours.line_number_bg = surface;
    colours.line_number_fg = ink;
    colours.menu_bg = surface;
    colours.menu_fg = ink;
    colours.border = ink;
    colours.bevel_light = ink;
    colours.bevel_dark = ink;
    // Inverted, so a selection is still visibly a selection when the background is
    // already pure white or pure black.
    colours.selection_bg = ink;
    colours.selection_fg = surface;
    // The header bar and the badges carry their text *on* an accent, so both roles
    // become the pair rather than a lifted hue: an accent lightened for a black
    // surface would leave the surface-coloured text on it unreadable.
    colours.panel_header_bg = ink;
    colours.panel_header_fg = surface;
    colours.hud_accent = ink;
    colours.info_color = surface;
    colours.fkey_bg = surface;
    colours.fkey_badge = ink;

    colours.keyword = liftToContrast(colours.keyword, surface);
    colours.comment = liftToContrast(colours.comment, surface);
    colours.string = liftToContrast(colours.string, surface);
    colours.number = liftToContrast(colours.number, surface);
    colours.function = liftToContrast(colours.function, surface);
    colours.error_color = liftToContrast(colours.error_color, surface);
    colours.warning_color = liftToContrast(colours.warning_color, surface);
    colours.success_color = liftToContrast(colours.success_color, surface);
    return colours;
}

QPalette paletteFor(const Theme& theme, bool high_contrast) {
    const Colours colours = resolve(theme, high_contrast);
    QPalette palette;
    const QColor background = colours.background;
    const QColor foreground = colours.foreground;
    const QColor base = colours.editor_bg;
    const QColor text = colours.editor_fg;
    const QColor menu_bg = colours.menu_bg;
    const QColor menu_fg = colours.menu_fg;
    const QColor border = colours.border;
    const QColor selection = colours.selection_bg;
    const QColor selected_text = colours.selection_fg;

    // The roles the Python set, in the same order.
    palette.setColor(QPalette::Window, background);
    palette.setColor(QPalette::WindowText, foreground);
    palette.setColor(QPalette::Base, base);
    palette.setColor(QPalette::AlternateBase, colours.line_number_bg);
    palette.setColor(QPalette::Text, text);
    palette.setColor(QPalette::Button, menu_bg);
    palette.setColor(QPalette::ButtonText, menu_fg);
    palette.setColor(QPalette::Highlight, selection);
    palette.setColor(QPalette::HighlightedText, selected_text);
    palette.setColor(QPalette::ToolTipBase, menu_bg);
    palette.setColor(QPalette::ToolTipText, menu_fg);
    palette.setColor(QPalette::Link, colours.keyword);
    palette.setColor(QPalette::LinkVisited, colours.function);

    // The roles the Python left to the platform, and could not have left alone
    // here: `style::window()` reads `palette(dark)` for every border and
    // `palette(shadow)` for the breadcrumb. A default-constructed QPalette keeps
    // the *system* style's values for roles it is not given, which on a light
    // desktop paints pale grey separators across a near-black theme.
    palette.setColor(QPalette::Dark, border);
    palette.setColor(QPalette::Mid, colours.line_number_fg);
    palette.setColor(QPalette::Midlight, colours.bevel_light);
    palette.setColor(QPalette::Light, colours.bevel_light);
    palette.setColor(QPalette::Shadow, colours.bevel_dark);
    palette.setColor(QPalette::BrightText, foreground);
    palette.setColor(QPalette::PlaceholderText, colours.line_number_fg);
#if QT_VERSION >= QT_VERSION_CHECK(6, 6, 0)
    // `Accent` arrived in Qt 6.6. Nothing in the window reads it -- the base
    // stylesheet is built on `highlight` -- so a build against an older Qt loses
    // one role and no behaviour. Guarded rather than dropped, because a desktop
    // running 6.6 or later uses it for focus rings and selection chrome.
    palette.setColor(QPalette::Accent, colours.hud_accent);
#endif

    // Disabled widgets: the Python let the style dim them, which on a dark theme
    // produced black-on-black labels. `line_number_fg` is the theme's own "quiet"
    // colour, so it is the one that is legible on that theme's background.
    const QColor quiet = colours.line_number_fg;
    palette.setColor(QPalette::Disabled, QPalette::WindowText, quiet);
    palette.setColor(QPalette::Disabled, QPalette::Text, quiet);
    palette.setColor(QPalette::Disabled, QPalette::ButtonText, quiet);
    palette.setColor(QPalette::Disabled, QPalette::Base, background);
    palette.setColor(QPalette::Disabled, QPalette::Highlight, border);
    palette.setColor(QPalette::Disabled, QPalette::HighlightedText, quiet);

    return palette;
}

QString stylesheetFor(const Theme& theme, bool high_contrast) {
    // Everything that is *not* a palette role. The base stylesheet reads
    // `palette(...)` for tab bars, panes, buttons and the status bar, so those
    // follow the palette above with no rules here. What cannot be a role is a
    // colour that differs by widget: a menu bar that is not the window colour, an
    // editor surface that is not `Base`, a function-key bar with its own chrome.
    const Colours colours = resolve(theme, high_contrast);
    const auto hex = [](const QColor& colour) { return colour.name(QColor::HexRgb); };
    const QString menu_bg = hex(colours.menu_bg);
    const QString menu_fg = hex(colours.menu_fg);
    const QString border = hex(colours.border);
    const QString editor_bg = hex(colours.editor_bg);
    const QString editor_fg = hex(colours.editor_fg);
    const QString selection_bg = hex(colours.selection_bg);
    const QString selection_fg = hex(colours.selection_fg);

    return QStringLiteral(R"(
/* -- menus and toolbars: the bar is its own colour, not the window's -- */
QMenuBar {
    background-color: %1;
    color: %2;
    border-bottom: 1px solid %3;
}
QMenuBar::item { background-color: transparent; padding: 4px 8px; }
QMenuBar::item:selected { background-color: %4; color: %5; }
QMenu {
    background-color: %1;
    color: %2;
    border: 1px solid %3;
}
QMenu::item:selected { background-color: %4; color: %5; }
QMenu::separator { height: 1px; background-color: %3; margin: 4px 8px; }
QToolBar#mainToolBar, QToolBar#secondaryToolBar {
    background-color: %1;
    border-bottom: 1px solid %3;
}
/* -- the editor surface, which is a theme colour rather than `Base` -- */
QPlainTextEdit#consoleOutput, QPlainTextEdit#errorsLog, QPlainTextEdit {
    background-color: %6;
    color: %7;
    selection-background-color: %4;
    selection-color: %5;
}
QPlainTextEdit#errorsLog { color: %8; }
QLineEdit, QComboBox, QSpinBox, QDoubleSpinBox {
    background-color: %6;
    color: %7;
    border: 1px solid %3;
    border-radius: 3px;
    padding: 2px 4px;
}
QComboBox QAbstractItemView {
    background-color: %6;
    color: %7;
    selection-background-color: %4;
    selection-color: %5;
}
QTableWidget, QTableView, QListWidget, QTreeWidget, QTreeView {
    background-color: %6;
    color: %7;
    gridline-color: %3;
    selection-background-color: %4;
    selection-color: %5;
}
/* -- tabs: selected is the editor surface, with the theme's accent on top -- */
QTabBar::tab { background-color: %1; color: %2; border-color: %3; }
QTabBar::tab:selected {
    background-color: %6;
    color: %7;
    border-top: 2px solid %9;
}
QTabBar::tab:hover:!selected { background-color: %4; color: %5; }
/* -- the function-key bar: its own chrome tokens -- */
QFrame#functionKeyBar { background-color: %10; border-top: 1px solid %3; }
QFrame#functionKeyBar QPushButton {
    background-color: %10;
    color: %11;
    border: 1px solid %3;
}
QFrame#functionKeyBar QPushButton:hover {
    background-color: %4;
    color: %5;
}
QFrame#functionKeyBar QPushButton:checked {
    background-color: %4;
    color: %5;
    font-weight: bold;
}
QFrame#functionKeyBar QPushButton#layerBadge {
    background-color: %11;
    color: %10;
    font-weight: bold;
    min-width: 54px;
}
QFrame#functionKeyBar QLabel, QFrame#functionKeyBar QComboBox { color: %2; }
/* -- the HUD: language badge and the execution state -- */
QLabel#languageBadge { background-color: %12; color: %13; }
QLabel#executionBadge { color: %14; font-weight: bold; }
QLabel#breadcrumb { color: %15; }
QDockWidget::title { background-color: %12; color: %13; padding: 5px 8px; }
QToolTip { background-color: %1; color: %2; border: 1px solid %3; }
)")
        .arg(menu_bg, menu_fg, border, selection_bg, selection_fg, editor_bg, editor_fg)
        .arg(hex(colours.error_color))
        .arg(hex(colours.keyword))
        .arg(hex(colours.fkey_bg))
        .arg(hex(colours.fkey_badge))
        .arg(hex(colours.panel_header_bg))
        .arg(hex(colours.panel_header_fg))
        .arg(hex(colours.hud_accent))
        .arg(hex(colours.info_color));
}

}  // namespace timewarp::ui::themes
