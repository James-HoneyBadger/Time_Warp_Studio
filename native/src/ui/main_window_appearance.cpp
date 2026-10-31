// SPDX-License-Identifier: MIT

#include "main_window_internal.hpp"

#include "timewarp/core/qt_names.hpp"
#include "timewarp/ui/main_window.hpp"

#include "timewarp/lang/executor.hpp"
#include "timewarp/lang/watch.hpp"
#include "timewarp/ui/native_runner.hpp"

#include <QAction>
#include <QActionGroup>
#include <QApplication>
#include <QDir>
#include <QDockWidget>
#include <QFileInfo>
#include <QFont>
#include <QMenu>
#include <QMenuBar>
#include <QPlainTextEdit>
#ifdef TIME_WARP_HAVE_PRINT
#endif
#include <QSettings>
#include <QShortcut>
#include <QSignalBlocker>
#include <QStatusBar>
#include <QTabWidget>
#include <QToolBar>

#include <algorithm>
#include <memory>
#include <string>
#include <vector>

#include "timewarp/canvas/host_adapter.hpp"
#include "timewarp/canvas/qt_bridge.hpp"
#include "timewarp/ui/app.hpp"
#include "timewarp/ui/app_paths.hpp"
#include "timewarp/ui/benchmark.hpp"
#include "timewarp/ui/breakpoints.hpp"
#include "timewarp/ui/chip8_window.hpp"
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/classroom_marker.hpp"
#include "timewarp/ui/command_palette.hpp"
#include "timewarp/ui/export.hpp"
#include "timewarp/ui/find_bar.hpp"
#include "timewarp/ui/find_in_files.hpp"
#include "timewarp/ui/formatter.hpp"
#include "timewarp/ui/help_browser.hpp"
#include "timewarp/ui/panels.hpp"
#include "timewarp/ui/preferences.hpp"
#include "timewarp/ui/profile_report.hpp"
#include "timewarp/ui/project_panel.hpp"
#include "timewarp/ui/retro_computer.hpp"
#include "timewarp/ui/repl_runner.hpp"
#include "timewarp/ui/sql_panel.hpp"
#include "timewarp/sql/database.hpp"
#include "timewarp/ui/resizable_tab_widget.hpp"
#include "timewarp/ui/scrubber.hpp"
#include "timewarp/ui/snippet_dialog.hpp"
#include "timewarp/ui/snippets.hpp"
#include "timewarp/ui/sound_player.hpp"
#include "timewarp/ui/tour.hpp"
#include "timewarp/ui/syntax_highlighter.hpp"
#include "timewarp/ui/terminal_widget.hpp"
#include "timewarp/ui/themes.hpp"
#include "timewarp/ui/turtle_canvas.hpp"
#include "timewarp/ui/version_history.hpp"
#include "timewarp/ui/ui_style.hpp"

// Supplied by CMake from the repository's VERSION file, so the window title and
// the packaged version cannot drift apart. The fallback keeps a standalone
// compile of this file working.
#ifndef TIME_WARP_VERSION
#define TIME_WARP_VERSION "0.0.0"
#endif

namespace timewarp::ui {

// The settings keys and the small helpers the sections share; they were an
// anonymous namespace in the one-file version, and a `using` is what keeps the call
// sites reading the same.
using namespace detail;

// ---------------------------------------------------------------------------
// Appearance
// ---------------------------------------------------------------------------

void MainWindow::buildThemeMenu(QMenu* view) {
    theme_menu_ = view->addMenu(tr("&Theme"));
    theme_menu_->setObjectName(QStringLiteral("themeMenu"));

    auto* group = new QActionGroup{theme_menu_};
    group->setExclusive(true);
    for (const Theme& theme : themes::all()) {
        const QString name = QString::fromUtf8(theme.name);
        QAction* action = theme_menu_->addAction(name);
        action->setCheckable(true);
        // The name travels as data rather than being parsed back out of the text:
        // a translation would break a lookup keyed on the visible string.
        action->setData(name);
        action->setChecked(name == theme_name_);
        group->addAction(action);
        connect(action, &QAction::triggered, this, [this, name] { applyTheme(name); });
    }
    theme_menu_->addSeparator();
    QAction* next = theme_menu_->addAction(tr("&Next Theme"));
    next->setObjectName(QStringLiteral("nextThemeAction"));
    next->setShortcut(timewarp::core::keySequence(Qt::SHIFT, Qt::Key_F10));
    connect(next, &QAction::triggered, this, [this] { cycleTheme(); });
}

void MainWindow::applyTheme(const QString& name) {
    const Theme& theme = themes::byName(name);
    theme_name_ = QString::fromUtf8(theme.name);

    // The application palette, not this window's: dialogs and message boxes are
    // separate top-level widgets and would otherwise stay on the default system
    // colours, which reads as a theme that half applied.
    if (auto* app = qobject_cast<QApplication*>(QCoreApplication::instance())) {
        app->setPalette(themes::paletteFor(theme, high_contrast_));
    }
    // The base sheet reads `palette(...)`, so it follows the palette above; the
    // theme's own block adds what a role cannot express.
    setStyleSheet(style::window() + themes::stylesheetFor(theme, high_contrast_));

    if (theme_menu_ != nullptr) {
        for (QAction* action : theme_menu_->actions()) {
            if (action->isCheckable()) {
                QSignalBlocker blocker{action};
                action->setChecked(action->data().toString() == theme_name_);
            }
        }
    }

    QSettings{}.setValue(kSettingTheme, theme_name_);
    applyThemeToWidgets();
}

void MainWindow::cycleTheme() {
    applyTheme(QString::fromUtf8(themes::next(theme_name_).name));
}

void MainWindow::setFontScale(int percent) {
    // Snap to the ladder rather than trusting the caller: the three steps are the
    // ones that were laid out and looked at, and a fourth size is a settings value
    // nobody designed.
    int snapped = kFontScales.front();
    for (const int step : kFontScales) {
        if (percent >= step) {
            snapped = step;
        }
    }
    if (editor_settings_.font_scale_percent == snapped) {
        return;
    }
    editor_settings_.font_scale_percent = snapped;
    saveEditorSettings();
    // One call applies all three surfaces: the editors through their own settings,
    // the console and logs through the same number, and the panels through the
    // application font that everything without an explicit font inherits.
    applyEditorSettings(editor_settings_);
    setExecutionState(tr("ready"), tr("Text size %1%").arg(snapped));
}

void MainWindow::cycleFontScale() {
    const int current = editor_settings_.font_scale_percent;
    const auto at = std::find(kFontScales.begin(), kFontScales.end(), current);
    if (at == kFontScales.end() || std::next(at) == kFontScales.end()) {
        setFontScale(kFontScales.front());
        return;
    }
    setFontScale(*std::next(at));
}

CrtOverlay::Settings MainWindow::loadCrtSettings() const {
    QSettings settings;
    // Off unless the user turned an effect on: the overlay used to start hidden and
    // a fresh install must keep looking like a modern IDE rather than a 1983
    // terminal -- the retro look is a choice, which is what the settings are for.
    CrtOverlay::Settings loaded;
    loaded.scanlines = settings.value(kSettingCrtScanlines, 0).toInt();
    loaded.vignette = settings.value(kSettingCrtVignette, 0).toInt();
    loaded.curvature = settings.value(kSettingCrtCurvature, 0).toInt();
    loaded.flicker = settings.value(kSettingCrtFlicker, 0).toInt();
    loaded.phosphor = settings.value(kSettingCrtPhosphor, 0).toInt();
    return loaded;
}

void MainWindow::saveCrtSettings() const {
    QSettings settings;
    settings.setValue(kSettingCrtScanlines, crt_settings_.scanlines);
    settings.setValue(kSettingCrtVignette, crt_settings_.vignette);
    settings.setValue(kSettingCrtCurvature, crt_settings_.curvature);
    settings.setValue(kSettingCrtFlicker, crt_settings_.flicker);
    settings.setValue(kSettingCrtPhosphor, crt_settings_.phosphor);
}

void MainWindow::setCrtSettings(const CrtOverlay::Settings& settings) {
    crt_settings_ = settings;
    if (crt_overlay_ != nullptr) {
        crt_overlay_->setSettings(crt_settings_);
    }
    // Showing the overlay is part of applying settings: a preview the user cannot
    // see is not a preview, and the menu's tick has to agree with the screen.
    const bool on = crt_settings_.scanlines > 0 || crt_settings_.vignette > 0 ||
                    crt_settings_.curvature > 0 || crt_settings_.flicker > 0 ||
                    crt_settings_.phosphor > 0;
    if (crt_overlay_ != nullptr) {
        crt_overlay_->setVisible(on);
    }
    if (crt_action_ != nullptr && crt_action_->isChecked() != on) {
        QSignalBlocker blocker{crt_action_};
        crt_action_->setChecked(on);
    }
}

void MainWindow::showCrtSettings() {
    if (crt_dialog_ != nullptr) {
        crt_dialog_->raise();
        crt_dialog_->activateWindow();
        return;
    }
    auto* dialog = new CrtSettingsDialog{crt_settings_, this};
    dialog->setAttribute(Qt::WA_DeleteOnClose);
    crt_dialog_ = dialog;
    crt_before_dialog_ = crt_settings_;
    // The preview is the real overlay, not a swatch in the dialog: a settings
    // window that shows its own mock-up can be right while the window is wrong.
    connect(dialog, &CrtSettingsDialog::previewRequested, this,
            [this](const CrtOverlay::Settings& preview) { setCrtSettings(preview); });
    connect(dialog, &QDialog::accepted, this, [this, dialog] {
        setCrtSettings(dialog->settings());
        saveCrtSettings();
        setExecutionState(tr("ready"), tr("CRT settings saved"));
    });
    connect(dialog, &QDialog::rejected, this, [this] {
        // Back to what was in force before the dialog opened, including the
        // overlay's visibility.
        setCrtSettings(crt_before_dialog_);
    });
    connect(dialog, &QObject::destroyed, this, [this] { crt_dialog_ = nullptr; });
    dialog->open();
}

void MainWindow::applyScreenPreset(ScreenPreset preset) {
    // One table, so a preset cannot half-apply and a new one is a row rather than
    // a code path. The themes are the ones the monochrome tube sets were built
    // from, and the CRT values are what make them read as a screen rather than as
    // a colour scheme.
    CrtOverlay::Settings crt;
    crt.scanlines = 0;
    crt.vignette = 0;
    crt.curvature = 0;
    crt.flicker = 0;
    crt.phosphor = 0;
    QString theme = QStringLiteral("VS Code Dark");
    QString label = tr("Normal display");

    switch (preset) {
        case ScreenPreset::Normal:
            break;
        case ScreenPreset::Amber:
            theme = QStringLiteral("Amber Monochrome");
            crt.scanlines = 25;
            crt.vignette = 30;
            crt.phosphor = 30;
            crt.phosphor_colour = QColor{255, 176, 0};
            label = tr("Amber monitor");
            break;
        case ScreenPreset::Green:
            theme = QStringLiteral("Green Monochrome");
            crt.scanlines = 25;
            crt.vignette = 30;
            crt.phosphor = 30;
            crt.phosphor_colour = QColor{0, 255, 0};
            label = tr("Green monitor");
            break;
        case ScreenPreset::White:
            theme = QStringLiteral("High Contrast Dark");
            crt.scanlines = 20;
            crt.curvature = 35;
            crt.phosphor = 20;
            crt.phosphor_colour = QColor{235, 235, 235};
            label = tr("White phosphor monitor");
            break;
    }

    applyTheme(theme);
    setCrtSettings(crt);
    saveCrtSettings();
    setExecutionState(tr("ready"), label);
}

void MainWindow::setFocusMode(bool on) {
    if (focus_mode_ == on) {
        return;
    }
    // Both modes own Escape, so the last one asked for wins rather than both
    // being on and the key being ambiguous. This is the other half of the same
    // rule in `setPresentationMode`.
    if (on && presentation_mode_) {
        setPresentationMode(false);
    }
    focus_mode_ = on;

    if (on) {
        // Only what was actually visible is recorded, so leaving does not pop up a
        // panel the user had closed before entering the mode.
        QList<QWidget*> chrome{tool_bar_, tool_bar_secondary_, function_key_bar_,
                               statusBar()};
        // Docks are the panels, and their titles are part of the chrome.
        for (QDockWidget* dock : findChildren<QDockWidget*>()) {
            chrome.append(dock);
        }
        focus_hidden_.clear();
        for (QWidget* widget : chrome) {
            if (widget != nullptr && widget->isVisible()) {
                focus_hidden_.append(QPointer<QWidget>{widget});
                widget->setVisible(false);
            }
        }
    } else {
        for (const QPointer<QWidget>& guarded : focus_hidden_) {
            // Through a raw pointer: GCC's `-Wnull-dereference` reads
            // `QPointer::operator->` as possibly null whatever the guard.
            QWidget* widget = guarded.data();
            if (widget != nullptr) {
                widget->setVisible(true);
            }
        }
        focus_hidden_.clear();
    }

    if (focus_shortcut_ != nullptr) {
        focus_shortcut_->setEnabled(on);
    }
    if (focus_action_ != nullptr && focus_action_->isChecked() != on) {
        QSignalBlocker blocker{focus_action_};
        focus_action_->setChecked(on);
    }
    setExecutionState(tr("ready"), on ? tr("Focus mode \u2014 press Esc to leave")
                                      : tr("Focus mode off"));
}

void MainWindow::setPresentationSettings(int fontPoints, bool fullscreen) {
    presentation_font_ = fontPoints;
    presentation_fullscreen_ = fullscreen;
    applyPresentationSettings();
}

void MainWindow::applyPresentationSettings() {
    if (!presentation_mode_) {
        return;
    }
    if (presentation_font_ > 0) {
        if (lesson_panel_ != nullptr) {
            lesson_panel_->setPresentationFont(presentation_font_);
        }
        // The console and the two logs, at the projection size rather than the
        // learner's ladder. Leaving the mode puts them back through
        // `applyEditorSettings`, which is what owns them.
        for (const QString& name : {QStringLiteral("consoleOutput"),
                                    QStringLiteral("errorsLog"),
                                    QStringLiteral("turtleLog")}) {
            const QList<QPlainTextEdit*> logs = findChildren<QPlainTextEdit*>(name);
            for (QPlainTextEdit* log : logs) {
                QFont font = log->font();
                font.setPointSize(presentation_font_);
                log->setFont(font);
            }
        }
    }
    if (presentation_fullscreen_) {
        if (!isFullScreen()) {
            showFullScreen();
            // Only a change this mode made is undone by leaving it; a window the
            // user had already made full screen is their choice.
            presentation_restore_geometry_ = true;
        }
    } else if (presentation_restore_geometry_ && isFullScreen()) {
        // The class says windowed and this mode made it full screen: put it back
        // but stay in the mode, which is what `teach.py present --windowed` means.
        presentation_restore_geometry_ = false;
        if (presentation_was_maximized_) {
            showMaximized();
        } else {
            showNormal();
        }
    }
}

void MainWindow::setPresentationMode(bool on) {
    if (presentation_mode_ == on) {
        return;
    }
    // Both modes own Escape, and two owners make it ambiguous. Presentation is the
    // stronger of the two -- it is the one with an audience -- so entering it ends
    // focus mode; leaving it returns to the plain window.
    if (on && focus_mode_) {
        setFocusMode(false);
    }
    presentation_mode_ = on;
    if (on) {
        // The menu bar goes too: focus mode keeps it for the person at the
        // keyboard, but a room does not need File and Edit above the lesson.
        QList<QWidget*> chrome{menuBar(),         tool_bar_, tool_bar_secondary_,
                               function_key_bar_, statusBar()};
        for (QDockWidget* dock : findChildren<QDockWidget*>()) {
            chrome.append(dock);
        }
        presentation_hidden_.clear();
        for (QWidget* widget : chrome) {
            if (widget != nullptr && widget->isVisible()) {
                presentation_hidden_.append(QPointer<QWidget>{widget});
                widget->setVisible(false);
            }
        }
        presentation_was_maximized_ = isMaximized();
        presentation_restore_geometry_ = false;
        // The class's size may not have been read yet, and presentation mode is
        // reachable without opening a panel first. One request settles it; the
        // answer applies to the mode already on screen.
        if (presentation_font_ <= 0 && ensureClassroom()) {
            setPresentationSettings(classroom_->presentation().fontSize,
                                    classroom_->presentation().fullscreen);
        }
        applyPresentationSettings();
    } else {
        for (const QPointer<QWidget>& guarded : presentation_hidden_) {
            QWidget* widget = guarded.data();
            if (widget != nullptr) {
                widget->setVisible(true);
            }
        }
        presentation_hidden_.clear();
        if (presentation_restore_geometry_) {
            presentation_restore_geometry_ = false;
            if (presentation_was_maximized_) {
                showMaximized();
            } else {
                showNormal();
            }
        }
        // The console goes back through the settings that own it, so the learner's
        // scale is what returns rather than a guessed default.
        applyEditorSettings(editor_settings_);
    }
    if (presentation_shortcut_ != nullptr) {
        presentation_shortcut_->setEnabled(on);
    }
    if (presentation_action_ != nullptr && presentation_action_->isChecked() != on) {
        QSignalBlocker blocker{presentation_action_};
        presentation_action_->setChecked(on);
    }
    // No shell in front of a room: the tab leaves the window, and a shell that
    // was running is stopped rather than left as an invisible process.
    refreshTerminalTab();
    setExecutionState(tr("ready"), on ? tr("Presentation mode \u2014 press Esc to leave")
                                      : tr("Presentation mode off"));
}

void MainWindow::setHighContrast(bool on) {
    if (high_contrast_ == on) {
        // Still re-applied: a caller that asks for the state the window is already
        // in wants the state, not a no-op, and a second `applyTheme` is cheap.
        applyTheme(theme_name_);
        return;
    }
    high_contrast_ = on;
    // Re-applying the same theme with the flag set is what makes this a *variant*
    // rather than a theme: the colours were chosen by the user and are not
    // replaced, only pushed until they can be read.
    applyTheme(theme_name_);
    if (high_contrast_action_ != nullptr && high_contrast_action_->isChecked() != on) {
        QSignalBlocker blocker{high_contrast_action_};
        high_contrast_action_->setChecked(on);
    }
    setExecutionState(tr("ready"), on ? tr("High contrast on") : tr("High contrast off"));
}

void MainWindow::applyThemeToWidgets() {
    const Theme& theme = themes::byName(theme_name_);
    const themes::Colours resolved = themes::resolve(theme, high_contrast_);

    // Syntax colours. The highlighter's five roles against the theme's six: the
    // theme's `function` is the closest thing it has to a "directive" (PILOT's
    // unit letters, Scheme's labels), which is what that slot means here.
    SyntaxColors colours;
    colours.keyword = resolved.keyword;
    colours.comment = resolved.comment;
    colours.string = resolved.string;
    colours.number = resolved.number;
    colours.directive = resolved.function;

    const auto recolour = [&colours](QTabWidget* tabs) {
        if (tabs == nullptr) {
            return;
        }
        for (int index = 0; index < tabs->count(); ++index) {
            auto* editor = qobject_cast<CodeEditor*>(tabs->widget(index));
            if (editor != nullptr && editor->highlighter() != nullptr) {
                editor->highlighter()->setColors(colours);
            }
        }
    };
    recolour(editor_tabs_);
    recolour(editor_tabs_secondary_);

    // The canvas surface, and the surface the next run starts on. A program that
    // names its own background still wins for that run; this is the default.
    const QColor surface = resolved.canvas_bg;
    if (turtle_canvas_ != nullptr) {
        turtle_canvas_->setSurfaceColor(surface);
    }

    // The two verdict colours, which the theme carries for exactly this reason:
    // the panel's shipped green and red are unreadable on the phosphor themes.
    if (lesson_panel_ != nullptr) {
        lesson_panel_->setVerdictColors(resolved.success_color, resolved.error_color);
    }

    // The problem markers. A red squiggle on the red-phosphor or amber themes is
    // exactly the colour of everything else, so the marker colours come from the
    // theme too -- the error colour it carries for output, and its warning colour.
    const auto recolour_markers = [&resolved](QTabWidget* tabs) {
        if (tabs == nullptr) {
            return;
        }
        for (int index = 0; index < tabs->count(); ++index) {
            if (auto* editor = qobject_cast<CodeEditor*>(tabs->widget(index));
                editor != nullptr) {
                editor->setDiagnosticColors(resolved.error_color, resolved.warning_color);
            }
        }
    };
    recolour_markers(editor_tabs_);
    recolour_markers(editor_tabs_secondary_);
}

// ---------------------------------------------------------------------------
// Editor settings
// ---------------------------------------------------------------------------

void MainWindow::loadEditorSettings() {
    QSettings settings;
    EditorSettings loaded;
    loaded.font_family = settings.value(kSettingEditorFont, QString{}).toString();
    loaded.font_point_size = settings.value(kSettingEditorFontSize, 11).toInt();
    loaded.font_scale_percent = settings.value(kSettingFontScale, 100).toInt();
    loaded.tab_width = settings.value(kSettingEditorTabWidth, 4).toInt();
    loaded.show_line_numbers = settings.value(kSettingEditorLineNumbers, true).toBool();
    loaded.highlight_current_line =
        settings.value(kSettingEditorCurrentLine, true).toBool();
    loaded.word_wrap = settings.value(kSettingEditorWordWrap, false).toBool();
    loaded.auto_indent = settings.value(kSettingEditorAutoIndent, true).toBool();
    loaded.completion = settings.value(kSettingEditorCompletion, false).toBool();
    loaded.lint = settings.value(kSettingEditorLint, true).toBool();
    loaded.show_whitespace = settings.value(kSettingEditorWhitespace, false).toBool();
    loaded.minimap = settings.value(kSettingEditorMinimap, true).toBool();
    loaded.autosave = settings.value(kSettingEditorAutosave, true).toBool();
    loaded.autosave_seconds = settings.value(kSettingEditorAutosaveSeconds, 120).toInt();
    editor_settings_ = loaded;
}

void MainWindow::saveEditorSettings() const {
    QSettings settings;
    settings.setValue(kSettingEditorFont, editor_settings_.font_family);
    settings.setValue(kSettingEditorFontSize, editor_settings_.font_point_size);
    settings.setValue(kSettingFontScale, editor_settings_.font_scale_percent);
    settings.setValue(kSettingEditorTabWidth, editor_settings_.tab_width);
    settings.setValue(kSettingEditorLineNumbers, editor_settings_.show_line_numbers);
    settings.setValue(kSettingEditorCurrentLine, editor_settings_.highlight_current_line);
    settings.setValue(kSettingEditorWordWrap, editor_settings_.word_wrap);
    settings.setValue(kSettingEditorAutoIndent, editor_settings_.auto_indent);
    settings.setValue(kSettingEditorCompletion, editor_settings_.completion);
    settings.setValue(kSettingEditorLint, editor_settings_.lint);
    settings.setValue(kSettingEditorWhitespace, editor_settings_.show_whitespace);
    settings.setValue(kSettingEditorMinimap, editor_settings_.minimap);
    settings.setValue(kSettingEditorAutosave, editor_settings_.autosave);
    settings.setValue(kSettingEditorAutosaveSeconds, editor_settings_.autosave_seconds);
}

void MainWindow::applyEditorSettings(const EditorSettings& settings) {
    editor_settings_ = settings;
    const themes::Colours resolved =
        themes::resolve(themes::byName(theme_name_), high_contrast_);
    const auto apply_to = [this, &resolved](QTabWidget* tabs) {
        if (tabs == nullptr) {
            return;
        }
        for (int index = 0; index < tabs->count(); ++index) {
            if (auto* editor = qobject_cast<CodeEditor*>(tabs->widget(index));
                editor != nullptr) {
                editor->setDiagnosticColors(resolved.error_color, resolved.warning_color);
                editor->applySettings(editor_settings_);
            }
        }
    };
    apply_to(editor_tabs_);
    apply_to(editor_tabs_secondary_);

    // The console and the two logs are separate text views with a fixed font, so
    // they scale with the same ladder rather than staying at ten points while the
    // editor grows around them.
    const int log_points =
        std::max(6, static_cast<int>(std::lround(10.0 * editor_settings_.font_scale_percent / 100.0)));
    for (const QString& name : {QStringLiteral("consoleOutput"), QStringLiteral("errorsLog"),
                                QStringLiteral("turtleLog")}) {
        const QList<QPlainTextEdit*> logs = findChildren<QPlainTextEdit*>(name);
        for (QPlainTextEdit* log : logs) {
            QFont log_font = log->font();
            log_font.setPointSize(log_points);
            log->setFont(log_font);
        }
    }

    // Everything without a font of its own -- panels, docks, dialogs, menus --
    // inherits the application font. Scaling from the font captured at first use
    // rather than from the current one keeps 150% from becoming 225% the next time
    // Preferences is applied.
    static const QFont base_ui_font = QApplication::font();
    QFont ui_font = base_ui_font;
    ui_font.setPointSizeF(std::max(6.0, base_ui_font.pointSizeF() *
                                            (std::max(1, editor_settings_.font_scale_percent) /
                                             100.0)));
    if (auto* app = qobject_cast<QApplication*>(QCoreApplication::instance())) {
        app->setFont(ui_font);
    }

    // The interval is a setting, so the timer follows it. Ten seconds is the
    // floor: anything shorter is a disk-write loop wearing a feature's clothes.
    if (autosave_timer_ != nullptr) {
        autosave_timer_->setInterval(std::max(10, editor_settings_.autosave_seconds) * 1000);
        if (editor_settings_.autosave) {
            autosave_timer_->start();
        } else {
            autosave_timer_->stop();
        }
    }
}

void MainWindow::setTerminalEnabled(bool on) {
    terminal_enabled_ = on;
    QSettings{}.setValue(kSettingTerminalEnabled, on);
    refreshTerminalTab();
}

void MainWindow::refreshTerminalTab() {
    if (output_sub_tabs_ == nullptr) {
        return;
    }
    // Presentation mode is a room watching a program, and a player window is a
    // program its reader did not start: neither is a place for a prompt that
    // runs arbitrary commands. Both remove the tab rather than merely hiding it.
    const bool allowed = terminal_enabled_ && !presentation_mode_ && !player_mode_;
    if (!allowed) {
        if (terminal_widget_ != nullptr) {
            const int index = output_sub_tabs_->indexOf(terminal_widget_);
            if (index >= 0) {
                output_sub_tabs_->removeTab(index);
            }
            // A shell nobody can see is a process nobody can stop.
            terminal_widget_->stop();
        }
        return;
    }
    if (terminal_widget_ == nullptr) {
        terminal_widget_ = new TerminalWidget{terminalDirectory(), output_sub_tabs_};
    }
    if (output_sub_tabs_->indexOf(terminal_widget_) < 0) {
        const int index =
            output_sub_tabs_->addTab(terminal_widget_, tr("\U0001F4BB Terminal"));
        output_sub_tabs_->setTabToolTip(
            index, tr("Commands run through your shell in this document's folder. "
                      "Nothing here is sandboxed."));
    }
    terminal_widget_->setWorkingDirectory(terminalDirectory());
}

QString MainWindow::terminalDirectory() const {
    const CodeEditor* editor = currentEditor();
    const QString path = editor != nullptr ? pathOf(editor) : QString{};
    if (!path.isEmpty()) {
        return QFileInfo{path}.absolutePath();
    }
    return QDir::homePath();
}

void MainWindow::showPreferences() {
    // A stack copy: the dialog edits its own record, and Cancel simply means the
    // window never hears about it.
    auto* dialog = new PreferencesDialog{editor_settings_, terminal_enabled_, this};
    dialog->setAttribute(Qt::WA_DeleteOnClose);
    connect(dialog, &PreferencesDialog::applied, this,
            [this](const EditorSettings& settings) { applyEditorSettings(settings); });
    connect(dialog, &PreferencesDialog::accepted, this,
            [this](const EditorSettings& settings) {
                applyEditorSettings(settings);
                saveEditorSettings();
                setExecutionState(tr("ready"), tr("Editor settings saved"));
            });
    connect(dialog, &PreferencesDialog::tourRequested, this, [this] {
        rearmGuidedTour();
        setExecutionState(tr("ready"),
                          tr("The guided tour will show the next time the window opens."));
    });
    // Immediate, like the tour: the tab is either on screen or not, and Apply is
    // not required for a control whose whole effect is visible behind the dialog.
    connect(dialog, &PreferencesDialog::terminalToggled, this, [this](bool on) {
        setTerminalEnabled(on);
        setExecutionState(tr("ready"),
                          on ? tr("The terminal is on")
                             : tr("The terminal is off"));
    });
    dialog->open();
}

}  // namespace timewarp::ui
