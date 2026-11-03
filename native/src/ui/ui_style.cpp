// SPDX-License-Identifier: MIT

#include "timewarp/ui/ui_style.hpp"

#include <QDir>
#include <QHash>
#include <QImage>
#include <QFile>
#include <QPainter>
#include <QPainterPath>
#include <QStandardPaths>
#include <QStringList>

namespace timewarp::ui::style {

namespace {

using design::Tokens;

/// A colour as a stylesheet takes it. Alpha is written as `rgba(...)`: the washes
/// are the only way a sheet can say "this surface, tinted" without a colour per
/// theme per state.
[[nodiscard]] QString css(const QColor& colour) {
    if (colour.alpha() == 255) {
        return colour.name(QColor::HexRgb);
    }
    return QStringLiteral("rgba(%1, %2, %3, %4)")
        .arg(colour.red())
        .arg(colour.green())
        .arg(colour.blue())
        .arg(QString::number(colour.alphaF(), 'f', 3));
}

/// A downward chevron, rendered to a PNG and answered as a path.
///
/// A combo box is the one control Qt will not draw a native arrow for once its
/// drop-down subcontrol has been styled -- and a dropdown with no arrow is a text
/// field. QSS has no way to draw a shape, so the shape is a file: one small PNG
/// per theme, written into the cache directory on first use and reused after
/// that. The colour is the theme's, so the arrow follows the theme like the icons
/// do.
[[nodiscard]] QString chevronImage(const QColor& tint) {
    const QString directory =
        QStandardPaths::writableLocation(QStandardPaths::CacheLocation);
    if (directory.isEmpty()) {
        return QString{};
    }
    const QString path =
        QDir{directory}.filePath(QStringLiteral("chevron-%1.png").arg(tint.name()));
    if (QFile::exists(path)) {
        return path;
    }
    QDir{}.mkpath(directory);
    QImage image{12, 12, QImage::Format_ARGB32_Premultiplied};
    image.fill(Qt::transparent);
    QPainter painter{&image};
    painter.setRenderHint(QPainter::Antialiasing, true);
    QPen pen{tint, 1.6};
    pen.setCapStyle(Qt::RoundCap);
    pen.setJoinStyle(Qt::RoundJoin);
    painter.setPen(pen);
    QPainterPath arrow;
    arrow.moveTo(2.6, 4.4);
    arrow.lineTo(6.0, 7.8);
    arrow.lineTo(9.4, 4.4);
    painter.drawPath(arrow);
    painter.end();
    return image.save(path) ? path : QString{};
}

/// The tokens, named.
///
/// Templates are filled by name (`%%accent%%`) rather than by position: a
/// fifty-line rule block interpolated positionally is a block nobody can
/// reorder, and `arg()` runs out of arguments long before the sheet is written.
[[nodiscard]] QHash<QString, QString> names(const Tokens& t) {
    const auto px = [](int value) { return QString::number(value) + QStringLiteral("px"); };
    return {
        {QStringLiteral("window"), css(t.window)},
        {QStringLiteral("chrome"), css(t.chrome)},
        {QStringLiteral("panel"), css(t.panel)},
        {QStringLiteral("editor"), css(t.editor)},
        {QStringLiteral("editorFg"), css(t.editor_text)},
        {QStringLiteral("raised"), css(t.raised)},
        {QStringLiteral("sunken"), css(t.sunken)},
        {QStringLiteral("border"), css(t.border)},
        {QStringLiteral("borderStrong"), css(t.border_strong)},
        {QStringLiteral("text"), css(t.text)},
        {QStringLiteral("muted"), css(t.text_muted)},
        {QStringLiteral("disabled"), css(t.text_disabled)},
        {QStringLiteral("accent"), css(t.accent)},
        {QStringLiteral("accentHover"), css(t.accent_hover)},
        {QStringLiteral("accentPress"), css(t.accent_pressed)},
        {QStringLiteral("accentText"), css(t.accent_text)},
        {QStringLiteral("wash"), css(t.accent_wash)},
        {QStringLiteral("washStrong"), css(design::faded(t.accent, 70))},
        {QStringLiteral("arrowImage"),
         QStringLiteral("url(\"%1\")").arg(chevronImage(t.text_muted))},
        {QStringLiteral("selection"), css(t.selection)},
        {QStringLiteral("selectionText"), css(t.selection_text)},
        {QStringLiteral("danger"), css(t.danger)},
        {QStringLiteral("warning"), css(t.warning)},
        {QStringLiteral("success"), css(t.success)},
        {QStringLiteral("info"), css(t.info)},
        {QStringLiteral("scroll"), css(t.scroll)},
        {QStringLiteral("scrollHover"), css(t.scroll_hover)},
        {QStringLiteral("canvas"), css(t.canvas)},
        {QStringLiteral("radius"), px(t.metrics.radius)},
        {QStringLiteral("radiusSmall"), px(t.metrics.radius_small)},
        {QStringLiteral("radiusLarge"), px(t.metrics.radius_large)},
        {QStringLiteral("fontSmall"), px(t.metrics.font_small)},
        {QStringLiteral("keyHeight"), px(t.metrics.key_height)},
        {QStringLiteral("controlHeight"), px(t.metrics.control_height)},
        {QStringLiteral("gap"), px(t.metrics.gap)},
        {QStringLiteral("gapSmall"), px(t.metrics.gap_small)},
    };
}

/// Every `%%name%%` in `templ` replaced. Unknown names are left in place, which
/// makes a typo visible in the widget rather than silently blank.
[[nodiscard]] QString fill(QString templ, const Tokens& t) {
    const QHash<QString, QString> values = names(t);
    for (auto it = values.cbegin(); it != values.cend(); ++it) {
        templ.replace(QLatin1String("%%") + it.key() + QLatin1String("%%"), it.value());
    }
    return templ;
}

}  // namespace

QString base(const Tokens& t) {
    return fill(QStringLiteral(R"(
/* An interface with no default text colour inherits grey-on-grey from the style,
   so every widget starts from the theme's foreground. */
QWidget { color: %%text%%; }
QMainWindow, QDialog, QWizard { background-color: %%window%%; }
QLabel { background: transparent; }
QLabel:disabled { color: %%disabled%%; }
QToolTip {
    background-color: %%panel%%;
    color: %%text%%;
    border: 1px solid %%border%%;
    border-radius: %%radiusLarge%%;
    padding: 5px 8px;
}
QSplitter { background-color: %%window%%; }
QSplitter::handle { background-color: %%border%%; }
QSplitter::handle:hover { background-color: %%accent%%; }
QSplitter::handle:horizontal { width: 1px; }
QSplitter::handle:vertical { height: 1px; }
)"),
                        t);
}

QString menus(const Tokens& t) {
    return fill(QStringLiteral(R"(
QMenuBar {
    background-color: %%chrome%%;
    color: %%text%%;
    border-bottom: 1px solid %%border%%;
    padding: 2px 6px;
}
QMenuBar::item {
    background: transparent;
    padding: 5px 10px;
    border-radius: %%radius%%;
}
QMenuBar::item:selected { background-color: %%raised%%; color: %%text%%; }
QMenuBar::item:pressed { background-color: %%accent%%; color: %%accentText%%; }
QMenu {
    background-color: %%panel%%;
    color: %%text%%;
    border: 1px solid %%border%%;
    border-radius: %%radiusLarge%%;
    padding: 5px;
}
QMenu::item {
    padding: 6px 28px 6px 12px;
    border-radius: %%radiusSmall%%;
    background: transparent;
}
QMenu::item:selected { background-color: %%accent%%; color: %%accentText%%; }
QMenu::item:disabled { color: %%disabled%%; }
QMenu::separator { height: 1px; background-color: %%border%%; margin: 5px 8px; }
QMenu::indicator { width: 14px; height: 14px; padding-left: 8px; }
)"),
                        t);
}

QString toolbars(const Tokens& t) {
    return fill(QStringLiteral(R"(
QToolBar {
    background-color: %%chrome%%;
    border: none;
    spacing: 2px;
    padding: 4px 6px;
}
QToolBar#mainToolBar { border-bottom: 1px solid %%border%%; }
QWidget#toolBarSpacer { background: transparent; }
QToolBar::separator {
    width: 1px;
    background-color: %%border%%;
    margin: 8px 5px;
}
QToolBar QToolButton {
    background: transparent;
    border: 1px solid transparent;
    border-radius: %%radius%%;
    padding: 4px 6px;
    color: %%text%%;
    margin: 0px;
}
QToolBar QToolButton[compact="true"] { padding: 4px; }
QToolBar QToolButton:hover { background-color: %%raised%%; }
QToolBar QToolButton:pressed { background-color: %%sunken%%; }
QToolBar QToolButton:checked { background-color: %%wash%%; color: %%accent%%; }
QToolBar QToolButton:disabled { color: %%disabled%%; }
/* The split arrow of the Run button gets its own quiet region, so the accent face
   of the button is not cut by a painted divider. */
QToolBar QToolButton::menu-button {
    border: none;
    border-top-right-radius: %%radius%%;
    border-bottom-right-radius: %%radius%%;
    width: 16px;
    background: transparent;
}
QToolBar QToolButton::menu-button:hover { background-color: %%washStrong%%; }
/* Run is the primary action, so it is *tinted* rather than filled: the glyph on
   it is tinted from the palette like every other icon, and a filled button would
   need a white glyph on a theme-dependent colour -- which is a second icon set,
   or an invisible icon on the themes where the accent is pale. */
QToolButton#runButton {
    background-color: %%wash%%;
    color: %%accent%%;
    border: 1px solid %%accent%%;
    border-radius: %%radius%%;
    padding: 5px 10px 5px 12px;
    font-weight: 600;
}
QToolButton#runButton:hover { background-color: %%washStrong%%; }
QToolButton#runButton:pressed { background-color: %%washStrong%%; border-color: %%accentHover%%; }
QToolBar QLabel#fieldLabel { color: %%muted%%; font-size: %%fontSmall%%; padding-right: 2px; }
)"),
                        t);
}

QString tabs(const Tokens& t) {
    return fill(QStringLiteral(R"(
/* Underline tabs: a tab is a label with a rule under it, and the selected one's
   rule is the accent. Boxed tabs across three different tab bars is what made the
   window look like three applications. */
QTabBar { background: transparent; }
QTabBar::tab {
    background: transparent;
    color: %%muted%%;
    border: none;
    border-bottom: 2px solid transparent;
    padding: 7px 13px;
    margin: 0px;
    min-height: 20px;
}
QTabBar::tab:hover:!selected { color: %%text%%; background-color: %%raised%%; }
QTabBar::tab:selected {
    color: %%text%%;
    background: transparent;
    border-bottom: 2px solid %%accent%%;
    font-weight: 600;
}
QTabBar::tab:disabled { color: %%disabled%%; }
QTabBar::tab:selected:disabled { color: %%muted%%; border-bottom-color: %%border%%; }
/* The overflow arrows, which the right-hand strip reaches at 1400px. */
QTabBar QToolButton {
    background-color: %%chrome%%;
    border: 1px solid %%border%%;
    border-radius: %%radiusSmall%%;
}
QTabBar QToolButton:hover { background-color: %%raised%%; }
QTabBar::close-button { background: transparent; border-radius: %%radiusSmall%%; margin: 2px; }
QTabBar::close-button:hover { background-color: %%wash%%; }
)"),
                        t);
}

QString panes(const Tokens& t) {
    return fill(QStringLiteral(R"(
QTabWidget::pane {
    border: 1px solid %%border%%;
    border-top: none;
    background-color: %%panel%%;
}
QTabWidget#editorTabs::pane, QTabWidget#editorTabsSecondary::pane,
QTabWidget#rightTabs::pane {
    border: 1px solid %%border%%;
    border-top: none;
    background-color: %%editor%%;
}
QTabWidget#editorTabs, QTabWidget#editorTabsSecondary, QTabWidget#rightTabs,
QTabWidget#outputSubTabs { background-color: transparent; border: none; }
/* The output strip sits below its page, so the reading edge is the top one. */
QTabWidget#outputSubTabs QTabBar::tab {
    border-bottom: none;
    border-top: 2px solid transparent;
    padding: 5px 11px;
    min-height: 18px;
}
QTabWidget#outputSubTabs QTabBar::tab:selected {
    border-top: 2px solid %%accent%%;
    border-bottom: none;
}
QToolButton#consoleErrorFilter {
    background-color: %%chrome%%;
    color: %%muted%%;
    border: 1px solid %%border%%;
    border-radius: %%radiusSmall%%;
    padding: 2px 9px;
    margin: 3px 6px;
    font-size: %%fontSmall%%;
}
QToolButton#consoleErrorFilter:hover { color: %%text%%; border-color: %%borderStrong%%; }
QToolButton#consoleErrorFilter:checked {
    background-color: %%wash%%;
    color: %%accent%%;
    border-color: %%accent%%;
}
/* A dock title is a label rather than a handle: small, muted, with the bar's
   hairline under it. */
QDockWidget { color: %%text%%; border: none; }
QDockWidget::title {
    background-color: %%chrome%%;
    color: %%muted%%;
    border-bottom: 1px solid %%border%%;
    padding: 7px 10px;
    font-size: %%fontSmall%%;
    font-weight: 600;
    text-align: left;
}
QDockWidget::close-button, QDockWidget::float-button {
    background: transparent;
    border: none;
    border-radius: %%radiusSmall%%;
    padding: 0px;
}
QDockWidget::close-button:hover, QDockWidget::float-button:hover {
    background-color: %%raised%%;
}
)"),
                        t);
}

QString buttons(const Tokens& t) {
    return fill(QStringLiteral(R"(
QPushButton {
    background-color: %%raised%%;
    color: %%text%%;
    border: 1px solid %%border%%;
    border-radius: %%radius%%;
    padding: 5px 14px;
    min-height: 20px;
    font-weight: 500;
}
QPushButton:hover { border-color: %%borderStrong%%; background-color: %%wash%%; }
QPushButton:pressed { background-color: %%sunken%%; }
QPushButton:focus { border-color: %%accent%%; }
QPushButton:default {
    background-color: %%accent%%;
    color: %%accentText%%;
    border-color: %%accent%%;
}
QPushButton:default:hover { background-color: %%accentHover%%; border-color: %%accentHover%%; }
QPushButton:default:pressed { background-color: %%accentPress%%; }
QPushButton:disabled {
    background-color: transparent;
    color: %%disabled%%;
    border-color: %%border%%;
}
QPushButton:checked { background-color: %%wash%%; color: %%accent%%; border-color: %%accent%%; }
/* The one button in the lessons page that is the point of the page. */
QPushButton#lessonCheckButton {
    background-color: %%accent%%;
    color: %%accentText%%;
    border-color: %%accent%%;
    font-weight: 600;
}
QPushButton#lessonCheckButton:hover { background-color: %%accentHover%%; border-color: %%accentHover%%; }
QLabel#lessonVerdict { color: %%muted%%; font-size: %%fontSmall%%; }
QListWidget#lessonCheckpoints { font-size: %%fontSmall%%; }
QDialogButtonBox QPushButton { min-width: 76px; }
/* A tool button outside a toolbar is a small icon control: the panel headers use
   them. */
QToolButton {
    background: transparent;
    border: 1px solid transparent;
    border-radius: %%radiusSmall%%;
    padding: 3px;
}
QToolButton:hover { background-color: %%raised%%; border-color: %%border%%; }
QToolButton:pressed { background-color: %%sunken%%; }
QToolButton:checked { background-color: %%wash%%; }
QToolButton:disabled { color: %%disabled%%; }
)"),
                        t);
}

QString fields(const Tokens& t) {
    return fill(QStringLiteral(R"(
QLineEdit, QComboBox, QSpinBox, QDoubleSpinBox, QDateTimeEdit, QFontComboBox {
    background-color: %%sunken%%;
    color: %%text%%;
    border: 1px solid %%border%%;
    border-radius: %%radius%%;
    padding: 4px 8px;
    min-height: 18px;
    selection-background-color: %%selection%%;
    selection-color: %%selectionText%%;
}
QLineEdit:hover, QComboBox:hover, QSpinBox:hover, QDoubleSpinBox:hover {
    border-color: %%borderStrong%%;
}
QLineEdit:focus, QComboBox:focus, QSpinBox:focus, QDoubleSpinBox:focus,
QDateTimeEdit:focus { border-color: %%accent%%; }
QLineEdit:disabled, QComboBox:disabled, QSpinBox:disabled {
    color: %%disabled%%;
    border-color: %%border%%;
}

/* The drop-down keeps its native arrow -- Qt draws no arrow for a subcontrol
   that has been given a stylesheet image and we have none to give -- but loses
   the style's bevelled corner, which is what made the box look like Windows 95. */
QComboBox::drop-down {
    background: transparent;
    width: 22px;
    subcontrol-origin: padding;
    subcontrol-position: center right;
    border: none;
}
QComboBox::down-arrow { image: %%arrowImage%%; width: 12px; height: 12px; }
QComboBox::down-arrow:disabled { image: none; }
QComboBox QAbstractItemView {
    background-color: %%panel%%;
    color: %%text%%;
    border: 1px solid %%border%%;
    border-radius: %%radiusSmall%%;
    padding: 3px;
    selection-background-color: %%selection%%;
    selection-color: %%selectionText%%;
    outline: none;
}
QComboBox QAbstractItemView::item { min-height: 22px; padding: 2px 6px; }
/* Every text surface is the editor's, because the console, the errors log and the
   turtle log are the same kind of thing as the code. */
QPlainTextEdit, QTextEdit, QTextBrowser {
    background-color: %%editor%%;
    color: %%editorFg%%;
    border: none;
    selection-background-color: %%selection%%;
    selection-color: %%selectionText%%;
}
QPlainTextEdit#errorsLog { color: %%danger%%; }
)"),
                        t);
}

QString views(const Tokens& t) {
    return fill(QStringLiteral(R"(
QTreeView, QListView, QTableView, QTableWidget, QListWidget, QTreeWidget {
    background-color: %%panel%%;
    alternate-background-color: %%raised%%;
    color: %%text%%;
    border: 1px solid %%border%%;
    border-radius: %%radius%%;
    gridline-color: %%border%%;
    selection-background-color: %%selection%%;
    selection-color: %%selectionText%%;
    outline: none;
}
QTreeView::item, QListView::item, QTableWidget::item, QListWidget::item {
    padding: 3px 4px;
    border-radius: %%radiusSmall%%;
}
QTreeView::item:hover, QListView::item:hover, QTableWidget::item:hover {
    background-color: %%wash%%;
}
QHeaderView { background-color: transparent; }
QHeaderView::section {
    background-color: %%chrome%%;
    color: %%muted%%;
    border: none;
    border-bottom: 1px solid %%border%%;
    border-right: 1px solid %%border%%;
    padding: 5px 8px;
    font-size: %%fontSmall%%;
    font-weight: 600;
}
QTableCornerButton::section { background-color: %%chrome%%; border: none; }
QGroupBox {
    border: 1px solid %%border%%;
    border-radius: %%radius%%;
    margin-top: 14px;
    padding-top: 6px;
    font-weight: 600;
}
QGroupBox::title {
    subcontrol-origin: margin;
    subcontrol-position: top left;
    left: 10px;
    padding: 0px 5px;
    color: %%muted%%;
    font-size: %%fontSmall%%;
}
QProgressBar {
    background-color: %%sunken%%;
    border: 1px solid %%border%%;
    border-radius: %%radiusSmall%%;
    text-align: center;
    color: %%text%%;
    min-height: 8px;
}
QProgressBar::chunk { background-color: %%accent%%; border-radius: %%radiusSmall%%; }
/* Sliders: one accent-coloured groove and a small handle. The native handle is
   drawn by the style and reads as a 1990s scrollbar thumb next to everything
   else here. */
QSlider { min-height: 18px; }
QSlider::groove:horizontal {
    background-color: %%sunken%%;
    border: 1px solid %%border%%;
    border-radius: 2px;
    height: 4px;
    margin: 0px 6px;
}
QSlider::sub-page:horizontal { background-color: %%accent%%; border-radius: 2px; margin: 0px 6px; height: 4px; }
QSlider::handle:horizontal {
    background-color: %%panel%%;
    border: 1px solid %%accent%%;
    width: 11px;
    margin: -6px 0px;
    border-radius: 6px;
}
QSlider::handle:horizontal:hover { background-color: %%washStrong%%; }
QSlider::groove:vertical { width: 4px; background-color: %%sunken%%; border-radius: 2px; }
QSlider::handle:vertical {
    background-color: %%panel%%;
    border: 1px solid %%accent%%;
    height: 11px;
    margin: 0px -6px;
    border-radius: 6px;
}
QLabel#scrubberLabel { color: %%muted%%; font-size: %%fontSmall%%; }
/* The turtle strip is a readout, not a sentence: the same small muted rank as the
   status bar, so it does not compete with the canvas above it. */
QFrame#turtleInspector { background-color: %%chrome%%; border-top: 1px solid %%border%%; }
QFrame#turtleInspector QLabel { color: %%muted%%; font-size: %%fontSmall%%; }
QLabel#turtleCommands { color: %%muted%%; font-size: %%fontSmall%%; }
QLabel#scrubber { background-color: %%chrome%%; }
QLabel#variablesStatus, QLabel#debugStatus, QLabel#debugReason, QLabel#debugPauseReason {
    color: %%muted%%;
    font-size: %%fontSmall%%;
    padding: 4px 6px;
}
QToolButton#scrubberPlay {
    background-color: %%wash%%;
    color: %%accent%%;
    border: 1px solid %%accent%%;
    border-radius: %%radiusSmall%%;
    padding: 2px 9px;
    font-size: %%fontSmall%%;
}
)"),
                        t);
}

QString scrollBars(const Tokens& t) {
    return fill(QStringLiteral(R"(
/* A scrollbar is a hint, not furniture: no trough, no arrows, a handle that
   appears when the surface it belongs to is scrollable. */
QScrollBar:vertical { background: transparent; width: 10px; margin: 2px; }
QScrollBar::handle:vertical {
    background-color: %%scroll%%;
    border-radius: 4px;
    min-height: 30px;
}
QScrollBar::handle:vertical:hover { background-color: %%scrollHover%%; }
QScrollBar:horizontal { background: transparent; height: 10px; margin: 2px; }
QScrollBar::handle:horizontal {
    background-color: %%scroll%%;
    border-radius: 4px;
    min-width: 30px;
}
QScrollBar::handle:horizontal:hover { background-color: %%scrollHover%%; }
QScrollBar::add-line, QScrollBar::sub-line {
    width: 0px;
    height: 0px;
    border: none;
    background: none;
}
QScrollBar::add-page, QScrollBar::sub-page { background: none; }
)"),
                        t);
}

QString statusBar(const Tokens& t) {
    return fill(QStringLiteral(R"(
QStatusBar {
    background-color: %%chrome%%;
    color: %%text%%;
    border-top: 1px solid %%border%%;
    padding: 1px 4px;
}
QStatusBar::item { border: none; }
QStatusBar QLabel { background: transparent; color: %%muted%%; padding: 2px 8px; }
/* The three chips are one control in three weights: the language is the only one
   that is a *value* rather than a state, so it is the only one tinted. */
QLabel#languageBadge {
    background-color: %%wash%%;
    color: %%accent%%;
    border-radius: %%radiusSmall%%;
    padding: 2px 9px;
    font-size: %%fontSmall%%;
    font-weight: 600;
}
QLabel#positionBadge { font-size: %%fontSmall%%; }
QLabel#executionBadge { color: %%accent%%; font-size: %%fontSmall%%; font-weight: 600; }
QLabel#breadcrumb { color: %%muted%%; font-size: %%fontSmall%%; }
QSizeGrip { background: transparent; width: 12px; height: 12px; }
)"),
                        t);
}

QString functionKeyBar(const Tokens& t) {
    return fill(QStringLiteral(R"(
/* The key row: ten keycaps, drawn as keys rather than as buttons. A row of
   full-weight buttons across the bottom of the window competes with the toolbar
   for the eye, and it is not a toolbar: it is a reminder of the keyboard. */
QFrame#functionKeyBar {
    background-color: %%chrome%%;
    border-top: 1px solid %%border%%;
}
QFrame#functionKeyBar QPushButton {
    background-color: %%raised%%;
    color: %%muted%%;
    border: 1px solid %%border%%;
    border-radius: %%radiusSmall%%;
    padding: 1px 8px;
    min-height: %%keyHeight%%;
    min-width: 0px;
    font-size: %%fontSmall%%;
    font-weight: 500;
}
QFrame#functionKeyBar QPushButton:hover {
    background-color: %%wash%%;
    color: %%accent%%;
    border-color: %%accent%%;
}
QFrame#functionKeyBar QPushButton:pressed { background-color: %%sunken%%; }
QFrame#functionKeyBar QPushButton#layerBadge {
    background-color: %%accent%%;
    color: %%accentText%%;
    border-color: %%accent%%;
    font-weight: 700;
    min-width: 46px;
}
QFrame#functionKeyBar QComboBox {
    background-color: transparent;
    color: %%muted%%;
    border: 1px solid transparent;
    border-radius: %%radiusSmall%%;
    padding: 1px 6px;
    font-size: %%fontSmall%%;
    min-height: 18px;
}
QFrame#functionKeyBar QComboBox:hover { border-color: %%border%%; }
QFrame#functionKeyBar QLabel { color: %%muted%%; font-size: %%fontSmall%%; }
)"),
                        t);
}

QString panels(const Tokens& t) {
    return fill(QStringLiteral(R"(
/* -- the project dock ---------------------------------------------------- */
QLabel#projectTitle { font-weight: 600; font-size: %%fontSmall%%; color: %%text%%; }
QLabel#projectRoot { color: %%muted%%; font-size: %%fontSmall%%; }
QLabel#projectEmpty { color: %%muted%%; font-size: %%fontSmall%%; }
QLabel#sqlEmptyState { color: %%muted%%; }
QLabel#sqlDatabasePath { color: %%muted%%; font-size: %%fontSmall%%; }
QLabel#commandReferenceEffect { color: %%text%%; }
QLabel#commandReferenceStatus { color: %%muted%%; font-size: %%fontSmall%%; }
/* The database header is three buttons and a path in the width of a dock, so its
   buttons take the smaller rank. */
QPushButton#sqlOpenDatabase, QPushButton#sqlNewDatabase,
QToolButton#sqlRecentDatabases {
    padding: 3px 8px;
    min-height: 16px;
    font-size: %%fontSmall%%;
}
QToolButton#sqlRecentDatabases { border: 1px solid %%border%%; background-color: %%raised%%; }
QWidget#projectTools { background: transparent; }
QWidget#projectTools QToolButton {
    border-radius: %%radiusSmall%%;
    padding: 3px;
}
QWidget#projectTools QToolButton:hover { background-color: %%raised%%; }
QTreeView#projectTree {
    background-color: transparent;
    border: none;
    border-radius: 0px;
}
QTreeView#projectTree::item { padding: 2px 3px; border-radius: %%radiusSmall%%; }
QTreeView#projectTree::item:selected {
    background-color: %%wash%%;
    color: %%accent%%;
}
/* -- the guided tour ----------------------------------------------------- */
QFrame#tourCard {
    background-color: %%panel%%;
    border: 1px solid %%border%%;
    border-radius: %%radiusLarge%%;
}
QLabel#tourTitle { color: %%text%%; font-weight: 600; }
QLabel#tourBody { color: %%muted%%; }
QLabel#tourProgress { color: %%muted%%; font-size: %%fontSmall%%; }
QFrame#tourCard QPushButton { padding: 3px 10px; min-height: 16px; }
QPushButton#tourNext {
    background-color: %%accent%%;
    color: %%accentText%%;
    border-color: %%accent%%;
}
QPushButton#tourNext:hover { background-color: %%accentHover%%; border-color: %%accentHover%%; }
QPushButton#tourSkip, QPushButton#tourBack {
    background: transparent;
    border-color: transparent;
    color: %%muted%%;
}
QPushButton#tourSkip:hover, QPushButton#tourBack:hover { color: %%text%%; background-color: %%raised%%; }
)"),
                        t);
}

QString sheet(const Tokens& t) {
    const QStringList sections{
        base(t),   menus(t),      toolbars(t),   tabs(t),       panes(t),
        buttons(t), fields(t),    views(t),      panels(t),     scrollBars(t),
        statusBar(t), functionKeyBar(t),
    };
    // Each section ends in a newline, so joining on nothing keeps a comment with
    // the rules it describes.
    return sections.join(QStringLiteral("\n"));
}

}  // namespace timewarp::ui::style
