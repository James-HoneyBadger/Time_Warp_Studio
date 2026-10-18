// SPDX-License-Identifier: MIT

#include "timewarp/ui/themes.hpp"

#include <QStringList>

#include <algorithm>
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

QPalette paletteFor(const Theme& theme) {
    QPalette palette;
    const QColor background = colour(theme.background);
    const QColor foreground = colour(theme.foreground);
    const QColor base = colour(theme.editor_bg);
    const QColor text = colour(theme.editor_fg);
    const QColor menu_bg = colour(theme.menuBackground());
    const QColor menu_fg = colour(theme.menuForeground());
    const QColor border = colour(theme.border());
    const QColor selection = colour(theme.selection_bg);
    const QColor selected_text = colour(theme.selection_fg);

    // The roles the Python set, in the same order.
    palette.setColor(QPalette::Window, background);
    palette.setColor(QPalette::WindowText, foreground);
    palette.setColor(QPalette::Base, base);
    palette.setColor(QPalette::AlternateBase, colour(theme.line_number_bg));
    palette.setColor(QPalette::Text, text);
    palette.setColor(QPalette::Button, menu_bg);
    palette.setColor(QPalette::ButtonText, menu_fg);
    palette.setColor(QPalette::Highlight, selection);
    palette.setColor(QPalette::HighlightedText, selected_text);
    palette.setColor(QPalette::ToolTipBase, menu_bg);
    palette.setColor(QPalette::ToolTipText, menu_fg);
    palette.setColor(QPalette::Link, colour(theme.keyword));
    palette.setColor(QPalette::LinkVisited, colour(theme.function));

    // The roles the Python left to the platform, and could not have left alone
    // here: `style::window()` reads `palette(dark)` for every border and
    // `palette(shadow)` for the breadcrumb. A default-constructed QPalette keeps
    // the *system* style's values for roles it is not given, which on a light
    // desktop paints pale grey separators across a near-black theme.
    palette.setColor(QPalette::Dark, border);
    palette.setColor(QPalette::Mid, colour(theme.line_number_fg));
    palette.setColor(QPalette::Midlight, colour(theme.bevel_light));
    palette.setColor(QPalette::Light, colour(theme.bevel_light));
    palette.setColor(QPalette::Shadow, colour(theme.bevel_dark));
    palette.setColor(QPalette::BrightText, foreground);
    palette.setColor(QPalette::PlaceholderText, colour(theme.line_number_fg));
#if QT_VERSION >= QT_VERSION_CHECK(6, 6, 0)
    // `Accent` arrived in Qt 6.6. Nothing in the window reads it -- the base
    // stylesheet is built on `highlight` -- so a build against an older Qt loses
    // one role and no behaviour. Guarded rather than dropped, because a desktop
    // running 6.6 or later uses it for focus rings and selection chrome.
    palette.setColor(QPalette::Accent, colour(theme.hud_accent));
#endif

    // Disabled widgets: the Python let the style dim them, which on a dark theme
    // produced black-on-black labels. `line_number_fg` is the theme's own "quiet"
    // colour, so it is the one that is legible on that theme's background.
    const QColor quiet = colour(theme.line_number_fg);
    palette.setColor(QPalette::Disabled, QPalette::WindowText, quiet);
    palette.setColor(QPalette::Disabled, QPalette::Text, quiet);
    palette.setColor(QPalette::Disabled, QPalette::ButtonText, quiet);
    palette.setColor(QPalette::Disabled, QPalette::Base, background);
    palette.setColor(QPalette::Disabled, QPalette::Highlight, border);
    palette.setColor(QPalette::Disabled, QPalette::HighlightedText, quiet);

    return palette;
}

QString stylesheetFor(const Theme& theme) {
    // Everything that is *not* a palette role. The base stylesheet reads
    // `palette(...)` for tab bars, panes, buttons and the status bar, so those
    // follow the palette above with no rules here. What cannot be a role is a
    // colour that differs by widget: a menu bar that is not the window colour, an
    // editor surface that is not `Base`, a function-key bar with its own chrome.
    const QString menu_bg = QString::fromUtf8(theme.menuBackground());
    const QString menu_fg = QString::fromUtf8(theme.menuForeground());
    const QString border = QString::fromUtf8(theme.border());
    const QString editor_bg = QString::fromUtf8(theme.editor_bg);
    const QString editor_fg = QString::fromUtf8(theme.editor_fg);
    const QString selection_bg = QString::fromUtf8(theme.selection_bg);
    const QString selection_fg = QString::fromUtf8(theme.selection_fg);

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
        .arg(QString::fromUtf8(theme.error_color))
        .arg(QString::fromUtf8(theme.keyword))
        .arg(QString::fromUtf8(theme.fkey_bg))
        .arg(QString::fromUtf8(theme.fkey_badge))
        .arg(QString::fromUtf8(theme.panel_header_bg))
        .arg(QString::fromUtf8(theme.panel_header_fg))
        .arg(QString::fromUtf8(theme.hud_accent))
        .arg(QString::fromUtf8(theme.info_color));
}

}  // namespace timewarp::ui::themes
