// SPDX-License-Identifier: MIT

#include "timewarp/ui/ui_style.hpp"

#include <QStringList>

namespace timewarp::ui::style {

namespace {

constexpr auto kWindowBackground = R"(
QMainWindow {
    background-color: palette(window);
}
)";

constexpr auto kTabBar = R"(
/* Shared tab look: every tab bar in the window, deliberately unscoped so the
   editor, the right-hand panes and the output sub-tabs match. */
QTabBar::tab {
    background-color: palette(window);
    color: palette(window-text);
    padding: 8px 16px;
    border: 1px solid palette(dark);
    border-bottom: none;
    border-radius: 4px 4px 0 0;
    margin-right: 2px;
    min-height: 24px;
}
QTabBar::tab:selected {
    background-color: palette(highlight);
    color: palette(highlighted-text);
    font-weight: bold;
}
QTabBar::tab:hover:!selected {
    background-color: rgba(0, 0, 0, 0.05);
}
)";

constexpr auto kTabPane = R"(
QTabWidget#editorTabs::pane,
QTabWidget#editorTabsSecondary::pane,
QTabWidget#rightTabs::pane {
    border: 1px solid palette(dark);
    border-radius: 4px;
    background-color: palette(base);
}
QTabWidget#editorTabs,
QTabWidget#editorTabsSecondary,
QTabWidget#rightTabs {
    background-color: palette(base);
    border: 1px solid palette(dark);
    border-radius: 4px;
}
)";

// The handle's *size* is set in C++ -- `QSplitter::setHandleWidth()` -- because
// a splitter's width is per-instance behaviour, not styling. Declaring it here
// as well would give two sources of truth for the same number, which is how a
// 6px handle becomes a 12px one after a theme change.
constexpr auto kSplitter = R"(
#mainSplitter {
    background-color: palette(window);
}
QSplitter::handle {
    background-color: palette(dark);
    border: none;
}
QSplitter::handle:hover {
    background-color: palette(highlight);
}
)";

constexpr auto kPushButtons = R"(
QPushButton {
    background-color: palette(button);
    color: palette(button-text);
    border: 2px solid palette(dark);
    border-radius: 6px;
    padding: 6px 14px;
    font-weight: bold;
    min-height: 28px;
    min-width: 80px;
}
QPushButton:hover {
    background-color: palette(highlight);
    color: palette(highlighted-text);
    border: 2px solid palette(highlight);
}
QPushButton:pressed {
    background-color: palette(dark);
    border: 2px solid palette(highlight);
}
QPushButton:disabled {
    color: palette(dark);
    background-color: palette(window);
    border: 2px solid palette(dark);
}
QDialogButtonBox QPushButton {
    min-width: 70px;
}
)";

constexpr auto kToolbar = R"(
QToolBar#mainToolBar {
    background-color: palette(window);
    border-bottom: 2px solid palette(highlight);
    padding: 4px;
    spacing: 8px;
}
QToolBar#mainToolBar QToolButton {
    background-color: transparent;
    border: 1px solid transparent;
    border-radius: 5px;
    padding: 6px 10px;
    margin: 2px;
    color: palette(window-text);
    font-weight: bold;
    min-width: 70px;
    min-height: 28px;
}
QToolBar#mainToolBar QToolButton:hover {
    background-color: palette(highlight);
    color: palette(highlighted-text);
    border: 1px solid palette(highlight);
    border-radius: 5px;
}
QToolBar#mainToolBar QToolButton:pressed {
    background-color: palette(dark);
    border: 1px solid palette(highlight);
}
QToolBar#mainToolBar QToolButton:disabled {
    color: palette(dark);
    background-color: transparent;
}
/* The Run button is a menu button: its menu arrow needs its own rule or it
   loses the toolbar's rounding on one side. */
QToolBar#mainToolBar QToolButton::menu-button {
    border-left: 1px solid palette(dark);
    width: 18px;
}
)";

constexpr auto kStatusBar = R"(
QStatusBar#mainStatusBar {
    background-color: palette(window);
    border-top: 1px solid palette(dark);
    padding: 2px;
}
QStatusBar#mainStatusBar QLabel {
    color: palette(window-text);
    padding: 2px 8px;
    border-radius: 3px;
}
)";

constexpr auto kStatusBadges = R"(
QLabel#languageBadge {
    background-color: palette(highlight);
    color: palette(highlighted-text);
    font-weight: bold;
    padding: 2px 6px;
    border-radius: 3px;
}
QLabel#positionBadge {
    background-color: palette(base);
    color: palette(text);
    padding: 2px 6px;
    border-radius: 3px;
    border: 1px solid palette(dark);
}
QLabel#breadcrumb {
    color: palette(shadow);
    padding: 2px 6px;
    font-size: 11px;
    font-style: italic;
}
)";

constexpr auto kOutputSubTabs = R"(
/* South-positioned sub-tabs hold secondary content, so they get a tighter
   tab strip than the panes above them. */
QTabWidget#outputSubTabs QTabBar::tab {
    padding: 4px 10px;
    min-height: 20px;
}
)";

constexpr auto kFunctionKeyBar = R"(
QFrame#functionKeyBar {
    background-color: palette(window);
    border-top: 1px solid palette(dark);
}
QFrame#functionKeyBar QPushButton {
    border: 1px solid palette(dark);
    border-radius: 3px;
    padding: 1px 6px;
    min-width: 48px;
    min-height: 18px;
    font-size: 11px;
    font-weight: normal;
}
QFrame#functionKeyBar QPushButton:hover {
    background-color: palette(highlight);
    color: palette(highlighted-text);
}
QFrame#functionKeyBar QLabel {
    color: palette(window-text);
    font-size: 11px;
}
)";

}  // namespace

QString windowBackground() { return QString::fromUtf8(kWindowBackground); }
QString tabBar() { return QString::fromUtf8(kTabBar); }
QString tabPane() { return QString::fromUtf8(kTabPane); }
QString splitter() { return QString::fromUtf8(kSplitter); }
QString pushButtons() { return QString::fromUtf8(kPushButtons); }
QString toolbar() { return QString::fromUtf8(kToolbar); }
QString statusBar() { return QString::fromUtf8(kStatusBar); }
QString statusBadges() { return QString::fromUtf8(kStatusBadges); }
QString outputSubTabs() { return QString::fromUtf8(kOutputSubTabs); }
QString functionKeyBar() { return QString::fromUtf8(kFunctionKeyBar); }

QString window() {
    const QStringList sections{
        windowBackground(), tabBar(),  tabPane(),        splitter(),
        pushButtons(),      toolbar(), statusBar(),     statusBadges(),
        outputSubTabs(),    functionKeyBar(),
    };
    return sections.join(QStringLiteral("\n"));
}

}  // namespace timewarp::ui::style
