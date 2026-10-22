// SPDX-License-Identifier: MIT
//
// Appearance: the themes, high contrast, the font ladder, focus and presentation.

#include "ui_layout/support.hpp"

namespace tw::ui_layout {

void test_themes() {
    announce("themes");
    using timewarp::ui::themes::all;
    using timewarp::ui::themes::byName;
    using timewarp::ui::themes::defaultName;
    using timewarp::ui::themes::next;

    // The table ships 35 named schemes -- the Python's 33 plus the two Windows
    // schemes, 2000 and 7 -- so the count is a fact worth pinning: a truncated
    // table would silently drop the retro sets.
    TW_CHECK_EQ(all().size(), std::size_t{35});
    QSet<QString> names;
    for (const auto& theme : all()) {
        const QString name = QString::fromUtf8(theme.name);
        TW_CHECK(!name.isEmpty());
        names.insert(name);
        // Every colour is a valid #rrggbb, so a typo in the table cannot reach a
        // stylesheet as an empty string.
        for (const char* hex : {theme.background, theme.foreground, theme.editor_bg,
                                theme.editor_fg, theme.keyword, theme.comment,
                                theme.canvas_bg, theme.selection_bg}) {
            TW_CHECK(timewarp::ui::themes::colour(hex).isValid());
        }
    }
    TW_CHECK_EQ(names.size(), 35);

    // An unknown name is a stale setting, not a crash.
    TW_CHECK_EQ(QString::fromUtf8(byName(QStringLiteral("no such theme")).name),
                defaultName());
    // Cycling wraps.
    TW_CHECK_EQ(QString::fromUtf8(next(QString::fromUtf8(all().back().name)).name),
                defaultName());

    // These tests share a settings identity between runs, and the theme is a
    // persisted setting: start from "nothing chosen" so "a fresh window is the
    // default" is a statement about the window rather than about the last run.
    QSettings{}.remove(QStringLiteral("appearance/theme"));
    MainWindow window;
    const QString original = window.currentThemeName();
    // A fresh window starts on the default -- or on whatever the settings say,
    // which is the next assertion's job.
    TW_CHECK_EQ(original, defaultName());

    // Every theme applies: the window palette follows it, the theme's own rules
    // reach the stylesheet, and the canvas paper changes with it.
    for (const auto& theme : all()) {
        const QString name = QString::fromUtf8(theme.name);
        window.applyTheme(name);
        TW_CHECK_EQ(window.currentThemeName(), name);
        // Bound to named locals: `palette().color()` and `surfaceColor()` return
        // QColor by value, and the check macro binds a reference into the
        // temporary (GCC 16's `-Wdangling-reference` catches exactly this).
        const QColor window_colour = QApplication::palette().color(QPalette::Window);
        const QColor expected_window{QString::fromUtf8(theme.background)};
        TW_CHECK_EQ(window_colour, expected_window);
        TW_CHECK(window.styleSheet().contains(QString::fromUtf8(theme.selection_bg)));
        const QColor paper = window.turtleCanvas()->surfaceColor();
        const QColor expected_paper{QString::fromUtf8(theme.canvas_bg)};
        TW_CHECK_EQ(paper, expected_paper);
    }

    // The choice is remembered, which is what makes it a setting.
    window.applyTheme(QStringLiteral("Dracula"));
    {
        MainWindow second;
        TW_CHECK_EQ(second.currentThemeName(), QStringLiteral("Dracula"));
    }
    // Shift+F10's action cycles rather than repeating.
    window.cycleTheme();
    TW_CHECK(window.currentThemeName() != QStringLiteral("Dracula"));

    // Leave the developer's settings as they were for the tests after this one.
    window.applyTheme(original);
    QSettings{}.remove(QStringLiteral("appearance/theme"));
}


void test_high_contrast_clears_a_seven_to_one_floor() {
    announce("high contrast: the contrast floor");
    using timewarp::ui::themes::contrastRatio;
    using timewarp::ui::themes::resolve;

    // A dark theme, a light one and a monochrome retro set: the three cases where
    // an accent that happens to suit one surface would fail on another.
    for (const char* name : {"Dracula", "GitHub Light", "VGA Amber Lab"}) {
        // Copied, not bound by reference: `byName` takes a temporary and returns a
        // reference to the table, which GCC's dangling-reference analysis cannot
        // tell apart from a reference into the argument.
        const timewarp::ui::Theme theme =
            timewarp::ui::themes::byName(QString::fromLatin1(name));
        const timewarp::ui::themes::Colours colours = resolve(theme, true);
        const QColor surface = colours.editor_bg;

        // 7:1 is WCAG AAA for body text; the plan uses it as the floor.
        TW_CHECK(contrastRatio(colours.foreground, colours.background) >= 7.0);
        TW_CHECK(contrastRatio(colours.editor_fg, surface) >= 7.0);
        TW_CHECK(contrastRatio(colours.line_number_fg, colours.line_number_bg) >= 7.0);
        for (const QColor& accent : {colours.keyword, colours.comment, colours.string,
                                     colours.number, colours.function,
                                     colours.error_color, colours.warning_color,
                                     colours.success_color}) {
            TW_CHECK(contrastRatio(accent, surface) >= 7.0);
        }

        // The theme's direction survives: asking for more contrast on a dark theme
        // must not hand back a light one.
        const bool theme_is_dark = timewarp::ui::themes::colour(theme.background).lightness() < 128;
        TW_CHECK_EQ(colours.background.lightness() < 128, theme_is_dark);
        const QColor expected_surface = theme_is_dark ? QColor{Qt::black} : QColor{Qt::white};
        TW_CHECK_EQ(colours.background, expected_surface);

        // A selection has to stay visible against a pure surface.
        TW_CHECK(contrastRatio(colours.selection_fg, colours.selection_bg) >= 7.0);
    }

    // Without the flag the theme's own colours come through untouched, which is
    // what makes this a variant rather than a replacement.
    const timewarp::ui::Theme dracula = timewarp::ui::themes::byName(QStringLiteral("Dracula"));
    const timewarp::ui::themes::Colours plain = resolve(dracula, false);
    const QColor dracula_background = timewarp::ui::themes::colour(dracula.background);
    const QColor dracula_keyword = timewarp::ui::themes::colour(dracula.keyword);
    TW_CHECK_EQ(plain.background, dracula_background);
    TW_CHECK_EQ(plain.keyword, dracula_keyword);
}


void test_high_contrast_toggles_and_restores_the_palette() {
    announce("high contrast: toggle and restore");
    const std::unique_ptr<MainWindow> window = shownWindow();
    auto* action = window->findChild<QAction*>(QStringLiteral("highContrastAction"));
    TW_CHECK(action != nullptr);
    if (action == nullptr) {
        return;
    }
    TW_CHECK(action->isCheckable());
    TW_CHECK_EQ(action->shortcut(), timewarp::core::keySequence(Qt::CTRL | Qt::ALT, Qt::Key_H));
    TW_CHECK(!window->highContrast());

    // This test changes the theme, and the theme is a persisted setting.
    const QString original = window->currentThemeName();

    // A dark theme first.
    window->applyTheme(QStringLiteral("Dracula"));
    const QPalette dark_before = QApplication::palette();
    action->trigger();
    TW_CHECK(window->highContrast());
    const QColor dark_window = QApplication::palette().color(QPalette::Window);
    TW_CHECK_EQ(dark_window, QColor{Qt::black});
    TW_CHECK(QApplication::palette() != dark_before);
    action->trigger();
    TW_CHECK(!window->highContrast());
    // Exactly: a toggle that leaves a role behind is a toggle a user has to fix by
    // changing theme and back.
    TW_CHECK(QApplication::palette() == dark_before);

    // And a light one, where the variant is white rather than black.
    window->applyTheme(QStringLiteral("GitHub Light"));
    const QPalette light_before = QApplication::palette();
    window->setHighContrast(true);
    const QColor light_window = QApplication::palette().color(QPalette::Window);
    TW_CHECK_EQ(light_window, QColor{Qt::white});
    // The menu's tick follows a change made through the API rather than the menu.
    if (auto* checked = window->findChild<QAction*>(QStringLiteral("highContrastAction"));
        checked != nullptr) {
        TW_CHECK(checked->isChecked());
    }
    window->setHighContrast(false);
    TW_CHECK(QApplication::palette() == light_before);
    window->applyTheme(original);
    QSettings{}.remove(QStringLiteral("appearance/theme"));
}


void test_font_scale_ladder() {
    announce("text scale");
    const std::unique_ptr<MainWindow> window = shownWindow();
    auto* action = window->findChild<QAction*>(QStringLiteral("fontScaleAction"));
    TW_CHECK(action != nullptr);
    if (action == nullptr) {
        return;
    }
    // Start from the bottom step whatever an earlier test left behind.
    window->setFontScale(100);
    TW_CHECK_EQ(window->fontScale(), 100);
    TW_CHECK_EQ(action->shortcut(), timewarp::core::keySequence(Qt::CTRL | Qt::ALT, Qt::Key_A));

    QWidget* console = window->outputSubTabs()->findChild<QPlainTextEdit*>(
        QStringLiteral("consoleOutput"));
    TW_CHECK(console != nullptr);
    auto* console_view = qobject_cast<QPlainTextEdit*>(console);
    if (console_view == nullptr) {
        return;
    }
    const double base_editor = window->currentEditor()->font().pointSizeF();
    const int base_console = console_view->font().pointSize();
    const double base_panel = QApplication::font().pointSizeF();

    action->trigger();  // 125%
    TW_CHECK_EQ(window->fontScale(), 125);
    // The editor scales from the size the user chose, not by replacing it.
    TW_CHECK(std::abs(window->currentEditor()->font().pointSizeF() - base_editor * 1.25) < 0.01);
    // Ten points of console becomes 12.5, which rounds to 13: a log font smaller
    // than the editor's at every step would defeat the purpose.
    TW_CHECK_EQ(console_view->font().pointSize(), 13);
    TW_CHECK(QApplication::font().pointSizeF() > base_panel);

    // The ladder wraps rather than stopping at the top.
    window->cycleFontScale();
    TW_CHECK_EQ(window->fontScale(), 150);
    window->cycleFontScale();
    TW_CHECK_EQ(window->fontScale(), 100);
    TW_CHECK(std::abs(window->currentEditor()->font().pointSizeF() - base_editor) < 0.01);
    TW_CHECK_EQ(console_view->font().pointSize(), base_console);
    TW_CHECK(std::abs(QApplication::font().pointSizeF() - base_panel) < 0.01);

    // Off-ladder values snap, so a hand-edited settings file cannot produce a size
    // nothing was laid out for.
    window->setFontScale(140);
    TW_CHECK_EQ(window->fontScale(), 125);
    window->setFontScale(1000);
    TW_CHECK_EQ(window->fontScale(), 150);

    // Persisted: a second window opens at the scale the first one chose.
    {
        MainWindow second;
        TW_CHECK_EQ(second.fontScale(), 150);
    }
    window->setFontScale(100);
}


void test_focus_mode_hides_and_restores_the_chrome() {
    announce("focus mode");
    const std::unique_ptr<MainWindow> window = shownWindow();
    auto* action = window->findChild<QAction*>(QStringLiteral("focusModeAction"));
    TW_CHECK(action != nullptr);
    if (action == nullptr) {
        return;
    }
    TW_CHECK(action->isCheckable());
    TW_CHECK_EQ(action->shortcut(), QKeySequence{Qt::Key_F11});
    TW_CHECK(!window->focusMode());

    auto* escape = window->findChild<QShortcut*>(QStringLiteral("focusModeEscape"));
    TW_CHECK(escape != nullptr);
    if (escape != nullptr) {
        // Escape has no business being bound while the mode is off.
        TW_CHECK(!escape->isEnabled());
    }

    // What is visible before entering is what has to come back.
    const bool bar_before = window->mainToolBar()->isVisible();
    const bool second_before = window->secondaryToolBar()->isVisible();
    const bool keys_before = window->functionKeyBar()->isVisible();
    const bool status_before = window->statusBar()->isVisible();
    TW_CHECK(bar_before);
    TW_CHECK(keys_before);

    action->trigger();
    TW_CHECK(window->focusMode());
    TW_CHECK(!window->mainToolBar()->isVisible());
    TW_CHECK(!window->secondaryToolBar()->isVisible());
    TW_CHECK(!window->functionKeyBar()->isVisible());
    TW_CHECK(!window->statusBar()->isVisible());
    if (escape != nullptr) {
        TW_CHECK(escape->isEnabled());
    }

    action->trigger();
    TW_CHECK(!window->focusMode());
    TW_CHECK_EQ(window->mainToolBar()->isVisible(), bar_before);
    TW_CHECK_EQ(window->secondaryToolBar()->isVisible(), second_before);
    TW_CHECK_EQ(window->functionKeyBar()->isVisible(), keys_before);
    TW_CHECK_EQ(window->statusBar()->isVisible(), status_before);
    if (escape != nullptr) {
        TW_CHECK(!escape->isEnabled());
    }

    // Esc is the way out, and it un-binds itself on the way out.
    window->setFocusMode(true);
    TW_CHECK(window->focusMode());
    if (escape != nullptr) {
        QMetaObject::invokeMethod(escape, "activated");
    }
    TW_CHECK(!window->focusMode());
    TW_CHECK(window->mainToolBar()->isVisible());
    if (escape != nullptr) {
        TW_CHECK(!escape->isEnabled());
    }
}


void test_presentation_mode_is_focus_mode_for_a_room() {
    announce("presentation mode");
    const std::unique_ptr<MainWindow> window = shownWindow();
    auto* action = window->findChild<QAction*>(QStringLiteral("presentationModeAction"));
    TW_CHECK(action != nullptr);
    if (action == nullptr) {
        return;
    }
    TW_CHECK(action->isCheckable());
    // Next to F11, which already means "give the editor the window": this one is
    // the same idea with an audience.
    TW_CHECK_EQ(action->shortcut(), timewarp::core::keySequence(Qt::SHIFT, Qt::Key_F11));
    TW_CHECK(!window->presentationMode());

    auto* escape = window->findChild<QShortcut*>(QStringLiteral("presentationModeEscape"));
    TW_CHECK(escape != nullptr);
    if (escape != nullptr) {
        TW_CHECK(!escape->isEnabled());
    }

    // A windowed class, so this test says nothing about the machine's screen.
    window->setPresentationSettings(28, false);

    const bool bar_before = window->mainToolBar()->isVisible();
    const bool second_before = window->secondaryToolBar()->isVisible();
    const bool keys_before = window->functionKeyBar()->isVisible();
    const bool status_before = window->statusBar()->isVisible();
    const bool menu_before = window->menuBar()->isVisible();
    TW_CHECK(bar_before);
    TW_CHECK(keys_before);

    auto* console = window->findChild<QPlainTextEdit*>(QStringLiteral("consoleOutput"));
    auto* description = window->findChild<QLabel*>(QStringLiteral("lessonDescription"));
    TW_CHECK(console != nullptr);
    TW_CHECK(description != nullptr);
    const int console_before = console == nullptr ? 0 : console->font().pointSize();

    action->trigger();
    TW_CHECK(window->presentationMode());
    // The menu bar goes too: focus mode keeps it for the person at the keyboard,
    // but a room does not need File and Edit above the lesson.
    TW_CHECK(!window->menuBar()->isVisible());
    TW_CHECK(!window->mainToolBar()->isVisible());
    TW_CHECK(!window->secondaryToolBar()->isVisible());
    TW_CHECK(!window->functionKeyBar()->isVisible());
    TW_CHECK(!window->statusBar()->isVisible());
    TW_CHECK(!window->isFullScreen());
    if (escape != nullptr) {
        TW_CHECK(escape->isEnabled());
    }
    // The two surfaces that have to be readable from the back of the room.
    if (console != nullptr) {
        TW_CHECK_EQ(console->font().pointSize(), 28);
    }
    if (description != nullptr) {
        TW_CHECK_EQ(description->font().pointSize(), 28);
    }

    // A size set while presenting lands without leaving the mode, which is what
    // Refresh after `teach.py present` does.
    window->setPresentationSettings(32, false);
    if (console != nullptr) {
        TW_CHECK_EQ(console->font().pointSize(), 32);
    }

    action->trigger();
    TW_CHECK(!window->presentationMode());
    TW_CHECK_EQ(window->menuBar()->isVisible(), menu_before);
    TW_CHECK_EQ(window->mainToolBar()->isVisible(), bar_before);
    TW_CHECK_EQ(window->secondaryToolBar()->isVisible(), second_before);
    TW_CHECK_EQ(window->functionKeyBar()->isVisible(), keys_before);
    TW_CHECK_EQ(window->statusBar()->isVisible(), status_before);
    if (escape != nullptr) {
        TW_CHECK(!escape->isEnabled());
    }
    // The console goes back through the settings that own it, not a guessed size.
    if (console != nullptr) {
        TW_CHECK_EQ(console->font().pointSize(), console_before);
    }

    // Esc is the way out, and it un-binds itself on the way out.
    window->setPresentationMode(true);
    TW_CHECK(window->presentationMode());
    if (escape != nullptr) {
        QMetaObject::invokeMethod(escape, "activated");
    }
    TW_CHECK(!window->presentationMode());
    TW_CHECK(window->mainToolBar()->isVisible());

    // Both modes own Escape, so asking for one while the other is on hands over
    // rather than leaving two Escape shortcuts competing for the key.
    window->setPresentationMode(true);
    window->setFocusMode(true);
    TW_CHECK(!window->presentationMode());
    TW_CHECK(window->focusMode());
    window->setFocusMode(false);

    // A class that asks for the whole screen gets it, and gets its window back.
    window->setPresentationSettings(24, true);
    window->setPresentationMode(true);
    TW_CHECK(window->presentationMode());
    TW_CHECK(window->isFullScreen());
    window->setPresentationMode(false);
    TW_CHECK(!window->presentationMode());
    TW_CHECK(!window->isFullScreen());
    TW_CHECK(window->mainToolBar()->isVisible());
}

// Recording is a state, not a dialog: the menu entry toggles it and the timer
// samples the canvas. An empty canvas records nothing, so stopping asks for no
// path -- which is why this test can stop it without a modal dialog.

}  // namespace tw::ui_layout
