// SPDX-License-Identifier: MIT
//
// The canvas, the output panes and the CRT: what a program's ink arrives at.

#include "ui_layout/support.hpp"

namespace tw::ui_layout {

// Read by this file only: it arrived here with the cases that need it.

/// Renders the overlay into an image and counts the pixels it touched, which is
/// the only way to say "paints nothing" without looking at a screen.
[[nodiscard]] int overlayInk(CrtOverlay& overlay) {
    overlay.resize(80, 60);
    QImage image{80, 60, QImage::Format_ARGB32_Premultiplied};
    image.fill(Qt::transparent);
    QPainter painter{&image};
    overlay.render(&painter);
    painter.end();
    int inked = 0;
    for (int y = 0; y < image.height(); ++y) {
        for (int x = 0; x < image.width(); ++x) {
            if (image.pixelColor(x, y).alpha() > 0) {
                ++inked;
            }
        }
    }
    return inked;
}

void test_run_both_panes_compares_them() {
    announce("run both panes");
    const std::unique_ptr<MainWindow> window = shownWindow();
    // Without a split there is nothing to compare, and the window says so instead
    // of running the same program twice.
    window->runBothPanes();
    TW_CHECK(!window->findChild<QPlainTextEdit*>(QStringLiteral("consoleOutput"))
                  ->toPlainText()
                  .contains(QStringLiteral("Both panes")));

    window->splitEditor();
    auto* left = qobject_cast<CodeEditor*>(window->editorTabs()->currentWidget());
    auto* right = qobject_cast<CodeEditor*>(
        window->secondaryEditorTabs()->currentWidget());
    TW_CHECK(left != nullptr);
    TW_CHECK(right != nullptr);
    if (left == nullptr || right == nullptr) {
        return;
    }
    // The left program fails on its second statement; the right one must still run,
    // which is the property a comparison table is worthless without.
    left->setPlainText(QStringLiteral("10 PRINT \"LEFT\"\n20 GOTO 999\n"));
    right->setPlainText(QStringLiteral("10 PRINT \"RIGHT\"\n20 PRINT 2 + 2\n"));

    window->runBothPanes();
    auto* console = window->findChild<QPlainTextEdit*>(QStringLiteral("consoleOutput"));
    TW_CHECK(console != nullptr);
    if (console == nullptr) {
        return;
    }
    const QString text = console->toPlainText();
    TW_CHECK(text.contains(QStringLiteral("Both panes")));
    // Both programs are named, so the numbers can be attributed.
    TW_CHECK(text.contains(QStringLiteral("LEFT")));
    TW_CHECK(text.contains(QStringLiteral("RIGHT")));
    // One finished and one did not, and the table says which is which.
    TW_CHECK(text.contains(QStringLiteral("finished")));
    TW_CHECK(text.contains(QStringLiteral("stopped")));
    // The right program's own output arrived even though the left one failed.
    TW_CHECK(text.contains(QStringLiteral("4")));
    // And real timings, not a placeholder: every row of the table has "ms" in it.
    TW_CHECK(text.count(QStringLiteral(" ms")) >= 2);
}


void test_the_scrubber_appears_only_when_there_is_ink() {
    announce("canvas scrubber");
    const std::unique_ptr<MainWindow> window = shownWindow();
    auto* scrubber = window->findChild<timewarp::ui::Scrubber*>(QStringLiteral("scrubber"));
    TW_CHECK(scrubber != nullptr);
    if (scrubber == nullptr) {
        return;
    }
    // An empty canvas has nothing to replay, so the strip keeps out of the way
    // and gives the drawing its height.
    TW_CHECK(!scrubber->isVisible());
    TW_CHECK_EQ(scrubber->frames(), 0);

    auto forward = timewarp::canvas::make_motion(
        timewarp::canvas::CanvasCommandKind::Forward, 20.0);
    window->turtleCanvas()->apply(timewarp::canvas::CanvasCommandView{forward, {}, {}});
    TW_CHECK_EQ(scrubber->frames(), 1);
    TW_CHECK(scrubber->isVisible());
    TW_CHECK_EQ(scrubber->visibleFrame(), 1);

    // The slider is what a user drags, and it drives the canvas's view without
    // deleting the ink behind it.
    auto* slider = scrubber->findChild<QSlider*>(QStringLiteral("scrubberSlider"));
    TW_CHECK(slider != nullptr);
    if (slider != nullptr) {
        slider->setValue(0);
        TW_CHECK_EQ(window->turtleCanvas()->visibleFrameCount(), 0);
        TW_CHECK_EQ(window->turtleCanvas()->strokeCount(), 1);
        slider->setValue(1);
        TW_CHECK_EQ(window->turtleCanvas()->visibleFrameCount(), 1);
    }

    // `clear` empties the history, which hides the strip again.
    auto clear = timewarp::canvas::make_command(timewarp::canvas::CanvasCommandKind::Clear);
    window->turtleCanvas()->apply(timewarp::canvas::CanvasCommandView{clear, {}, {}});
    TW_CHECK_EQ(scrubber->frames(), 0);
    TW_CHECK(!scrubber->isVisible());
}

// The command log is how a picture can be explained after the fact: a program
// emits hundreds of commands, and "why does it look like that" is answered by the
// sequence. The spelling of each kind is what a person reads, so it is asserted
// directly rather than through the widget.

void test_turtle_command_log_spells_each_command() {
    announce("turtle command log");
    using timewarp::canvas::CanvasCommand;
    using timewarp::canvas::CanvasCommandKind;
    using timewarp::canvas::CanvasCommandView;
    using timewarp::ui::TurtleCommandLog;

    const auto line = [](CanvasCommand command, std::vector<double> points = {},
                         std::string text = {}) {
        return TurtleCommandLog::describe(CanvasCommandView{command, points, text});
    };

    TW_CHECK_EQ(line(timewarp::canvas::make_motion(CanvasCommandKind::Forward, 100.0)),
                QStringLiteral("FORWARD 100"));
    // A half degree survives: a heading rounded to a whole number is a drawing
    // that quietly disagrees with the program.
    TW_CHECK_EQ(line(timewarp::canvas::make_motion(CanvasCommandKind::Right, 90.5)),
                QStringLiteral("RIGHT 90.5"));

    CanvasCommand colour = timewarp::canvas::make_command(CanvasCommandKind::SetPenColor);
    colour.color = timewarp::canvas::make_rgb(255, 136, 0);
    TW_CHECK_EQ(line(colour), QStringLiteral("SETPENCOLOR #FF8800"));

    // A full circle has no meaningful extent, so it is not printed.
    CanvasCommand circle = timewarp::canvas::make_command(CanvasCommandKind::Circle);
    circle.x = 30.0;
    circle.y = 360.0;
    TW_CHECK_EQ(line(circle), QStringLiteral("CIRCLE 30"));

    // A point list is summarised: the coordinates of a 200-point polygon would
    // bury every command around it.
    CanvasCommand polygon = timewarp::canvas::make_command(CanvasCommandKind::DrawPolyline);
    polygon.color = timewarp::canvas::make_rgb(0, 255, 0);
    TW_CHECK_EQ(line(polygon, {0.0, 0.0, 10.0, 10.0, 20.0, 0.0}),
                QStringLiteral("DRAWPOLYLINE 3 points #00FF00"));

    CanvasCommand text = timewarp::canvas::make_command(CanvasCommandKind::DrawText);
    text.x = 5.0;
    text.y = 6.0;
    text.w = 12.0;
    TW_CHECK_EQ(line(text, {}, "hi"), QStringLiteral("DRAWTEXT \"hi\" at 5 6 (size 12)"));

    // A name is a payload too, and it arrives as trailing bytes.
    CanvasCommand sprite = timewarp::canvas::make_command(CanvasCommandKind::ShowSprite);
    TW_CHECK_EQ(line(sprite, {}, "ship"), QStringLiteral("SHOWSPRITE \"ship\""));

    TW_CHECK_EQ(line(timewarp::canvas::make_command(CanvasCommandKind::PenUp)),
                QStringLiteral("PENUP"));
}

// The strip and the log are views over the canvas, so they are asserted through
// the window: the command that reaches the drawing has to reach the record of it
// in the same call, or the two disagree about what was drawn.

void test_turtle_log_and_inspector_follow_the_canvas() {
    announce("turtle inspector");
    using timewarp::canvas::CanvasCommandKind;
    using timewarp::canvas::CanvasCommandView;

    const std::unique_ptr<MainWindow> window = shownWindow();
    auto* inspector =
        window->findChild<timewarp::ui::TurtleInspector*>(QStringLiteral("turtleInspector"));
    auto* log = window->findChild<timewarp::ui::TurtleCommandLog*>(
        QStringLiteral("turtleLog"));
    TW_CHECK(inspector != nullptr);
    TW_CHECK(log != nullptr);
    if (inspector == nullptr || log == nullptr) {
        return;
    }

    // An untouched window has drawn nothing and says so, rather than showing a
    // pose from whatever was drawn last.
    TW_CHECK_EQ(log->commandCount(), 0);
    TW_CHECK_EQ(inspector->commandCount(), 0);

    // Through the window's own apply, which is the sink the bridge drains into.
    auto forward = timewarp::canvas::make_motion(CanvasCommandKind::Forward, 40.0);
    window->applyCanvasCommand(CanvasCommandView{forward, {}, {}});
    TW_CHECK_EQ(log->commandCount(), 1);
    TW_CHECK_EQ(inspector->commandCount(), 1);
    TW_CHECK(log->toPlainText().endsWith(QStringLiteral("FORWARD 40")));

    // Heading 0 is north, so a forward move grows Y: the strip reads the canvas's
    // own pose rather than keeping a copy that could drift from it.
    TW_CHECK(inspector->poseText().contains(QStringLiteral("Y 40")));
    TW_CHECK(inspector->penText().contains(QStringLiteral("DOWN")));

    auto pen_up = timewarp::canvas::make_command(CanvasCommandKind::PenUp);
    window->applyCanvasCommand(CanvasCommandView{pen_up, {}, {}});
    TW_CHECK(inspector->penText().contains(QStringLiteral("UP")));
    TW_CHECK_EQ(log->commandCount(), 2);

    auto right = timewarp::canvas::make_motion(CanvasCommandKind::Right, 90.0);
    window->applyCanvasCommand(CanvasCommandView{right, {}, {}});
    TW_CHECK(inspector->poseText().contains(QStringLiteral("Heading 90")));

    // Clearing the canvas clears the record of how it was made.
    window->clearCanvas();
    TW_CHECK_EQ(log->commandCount(), 0);
    TW_CHECK(log->toPlainText().isEmpty());
    TW_CHECK_EQ(inspector->commandCount(), 0);
    TW_CHECK(inspector->penText().contains(QStringLiteral("DOWN")));
}


void test_view_menu_grid_drives_the_canvas() {
    announce("View > Show Grid");
    MainWindow window;
    timewarp::ui::TurtleCanvas* canvas = window.turtleCanvas();
    TW_CHECK(canvas != nullptr);
    if (canvas == nullptr) {
        return;
    }

    QAction* item = entryContaining(menuNamed(window, QStringLiteral("&View")),
                                    QStringLiteral("Grid"));
    TW_CHECK(item != nullptr);
    if (item == nullptr) {
        return;
    }

    // The tick and the canvas have to agree both ways round, which is why the
    // entry is checkable and reports the canvas's state rather than keeping one
    // of its own.
    TW_CHECK_EQ(item->isChecked(), canvas->gridVisible());
    const bool before = canvas->gridVisible();
    item->trigger();
    TW_CHECK_EQ(canvas->gridVisible(), !before);
    TW_CHECK_EQ(item->isChecked(), !before);
}


void test_view_menu_zoom_scales_the_canvas() {
    announce("View > Zoom In");
    MainWindow window;
    timewarp::ui::TurtleCanvas* canvas = window.turtleCanvas();
    TW_CHECK(canvas != nullptr);
    if (canvas == nullptr) {
        return;
    }

    QAction* in = entryContaining(menuNamed(window, QStringLiteral("&View")),
                                  QStringLiteral("Zoom In"));
    QAction* out = entryContaining(menuNamed(window, QStringLiteral("&View")),
                                   QStringLiteral("Zoom Out"));
    QAction* fit = entryContaining(menuNamed(window, QStringLiteral("&View")),
                                   QStringLiteral("Fit"));
    TW_CHECK(in != nullptr && out != nullptr && fit != nullptr);
    if (in == nullptr || out == nullptr || fit == nullptr) {
        return;
    }

    const double start = canvas->zoom();
    in->trigger();
    TW_CHECK(canvas->zoom() > start);
    out->trigger();
    out->trigger();
    TW_CHECK(canvas->zoom() < start);
    // Fit is the way back to 1:1, so it has to reset the zoom as well as the pan.
    fit->trigger();
    TW_CHECK_EQ(canvas->zoom(), 1.0);
}

/// View > Profile Run is per-session and off at launch: timing every statement
/// is work, and a remembered "on" would make every later run pay for a report
/// nobody asked to see.

void test_a_program_screen_reaches_the_canvas() {
    announce("a screen reaches the canvas");

    timewarp::canvas::CanvasCommandQueue queue;
    timewarp::ui::NativeRunner runner{timewarp::lang::Language::Basic, queue};

    std::atomic<bool> finished{false};
    QObject::connect(&runner, &timewarp::ui::NativeRunner::run_finished, &runner,
                     [&](bool, const QString&) { finished = true; });

    runner.load_program(QStringLiteral("10 SCREEN 13\n20 PSET (5, 7), #FF0000\n"));
    runner.start();
    TW_CHECK(spin_until([&finished] { return finished.load(); }));
    runner.wait(5000);

    timewarp::ui::TurtleCanvas canvas;
    canvas.resize(800, 600);
    while (queue.drain([&canvas](const timewarp::canvas::CanvasCommandView& view) {
        canvas.apply(view);
    }) > 0) {
    }

    TW_CHECK(canvas.framebufferActive());
    TW_CHECK_EQ(canvas.framebufferSize().width(), 320);
    TW_CHECK_EQ(canvas.framebufferSize().height(), 200);
    TW_CHECK_EQ(canvas.framebufferPixel(5, 7), QColor(255, 0, 0));

    // And the window says which mode is on: on a screen X and Y are pixels from
    // the top-left, not the turtle's world, and a learner reading "X 156 Y 180"
    // while looking at a 320x200 game has the wrong explanation for it.
    const std::unique_ptr<MainWindow> window = shownWindow();
    timewarp::ui::TurtleCanvas* shown = window->turtleCanvas();
    TW_CHECK(shown != nullptr);
    if (shown != nullptr) {
        shown->apply(timewarp::canvas::CanvasCommandView{
            timewarp::canvas::make_framebuffer(320.0, 200.0), {}, {}});
        TW_CHECK_EQ(window->turtleInspector()->screenText(), QStringLiteral("SCREEN 320x200"));
        shown->apply(timewarp::canvas::CanvasCommandView{
            timewarp::canvas::make_clear_framebuffer(), {}, {}});
        TW_CHECK(window->turtleInspector()->screenText().isEmpty());
    }
}

/// The report shows what the engine measured, in the order the engine sorted it:
/// the dialog must not re-sort or drop rows.

void test_crt_overlay_tracks_its_target() {
    announce("CRT overlay tracking");
    MainWindow window;
    CrtOverlay* overlay = window.crtOverlay();

    TW_CHECK(overlay != nullptr);
    if (overlay == nullptr) {
        return;
    }
    // A full-window overlay that accepted clicks would make every control
    // underneath dead.
    TW_CHECK(overlay->testAttribute(Qt::WA_TransparentForMouseEvents));
    TW_CHECK_EQ(overlay->target(), window.centralWidget());
    TW_CHECK(overlay->isHidden());  // the Python starts with it disabled

    window.resize(1100, 700);
    window.show();
    QApplication::processEvents();

    // The whole reason the overlay installs an event filter instead of being
    // resized by the window: it keeps tracking after a resize.
    const QRect before = overlay->geometry();
    window.resize(1400, 900);
    QApplication::processEvents();
    TW_CHECK(overlay->geometry() != before);
    TW_CHECK_EQ(overlay->geometry(), window.centralWidget()->rect());

    overlay->setScanlineIntensity(40);
    TW_CHECK_EQ(overlay->scanlineIntensity(), 40);
    // Clamped, not wrapped: a slider or a theme cannot drive it out of range.
    overlay->setScanlineIntensity(1000);
    TW_CHECK_EQ(overlay->scanlineIntensity(), 100);
    overlay->setScanlineIntensity(-5);
    TW_CHECK_EQ(overlay->scanlineIntensity(), 0);
}



void test_crt_settings_reach_the_overlay() {
    announce("CRT settings");
    const std::unique_ptr<MainWindow> window = shownWindow();
    CrtOverlay* overlay = window->crtOverlay();
    TW_CHECK(overlay != nullptr);
    if (overlay == nullptr) {
        return;
    }

    CrtOverlay::Settings off;
    off.scanlines = 0;
    off.vignette = 0;
    off.curvature = 0;
    off.flicker = 0;
    off.phosphor = 0;
    window->setCrtSettings(off);
    // Zero everywhere is exactly zero: not a nearly-invisible layer that dulls the
    // text and leaves a user wondering what changed.
    TW_CHECK_EQ(overlayInk(*overlay), 0);
    TW_CHECK(!overlay->isVisible());

    // Each effect on its own reaches the overlay and paints something.
    for (int CrtOverlay::Settings::*field :
         {&CrtOverlay::Settings::scanlines, &CrtOverlay::Settings::vignette,
          &CrtOverlay::Settings::curvature, &CrtOverlay::Settings::phosphor}) {
        CrtOverlay::Settings one = off;
        one.*(field) = 100;
        window->setCrtSettings(one);
        TW_CHECK(overlayInk(*overlay) > 0);
        TW_CHECK(overlay->isVisible());
    }

    // The dialog drives the real overlay: a preview in a swatch can be right while
    // the window is wrong.
    window->setCrtSettings(off);
    window->showCrtSettings();
    auto* dialog = window->findChild<timewarp::ui::CrtSettingsDialog*>(
        QStringLiteral("crtSettingsDialog"));
    TW_CHECK(dialog != nullptr);
    if (dialog == nullptr) {
        return;
    }
    auto* scanlines = dialog->findChild<QSlider*>(QStringLiteral("crtScanlines"));
    auto* phosphor = dialog->findChild<QSlider*>(QStringLiteral("crtPhosphor"));
    TW_CHECK(scanlines != nullptr);
    TW_CHECK(phosphor != nullptr);
    if (scanlines != nullptr) {
        scanlines->setValue(45);
        TW_CHECK_EQ(window->crtSettings().scanlines, 45);
        TW_CHECK_EQ(overlay->settings().scanlines, 45);
        TW_CHECK(overlay->isVisible());
    }
    if (phosphor != nullptr) {
        phosphor->setValue(15);
        TW_CHECK_EQ(overlay->settings().phosphor, 15);
    }

    // Cancel puts back what was in force before the dialog opened, including the
    // overlay's visibility.
    dialog->reject();
    QApplication::processEvents();
    TW_CHECK_EQ(window->crtSettings().scanlines, 0);
    TW_CHECK_EQ(window->crtSettings().phosphor, 0);
    TW_CHECK(!overlay->isVisible());

    // OK persists, and a second window opens on the same values.
    window->showCrtSettings();
    dialog = window->findChild<timewarp::ui::CrtSettingsDialog*>(
        QStringLiteral("crtSettingsDialog"));
    TW_CHECK(dialog != nullptr);
    if (dialog != nullptr) {
        if (auto* value = dialog->findChild<QSlider*>(QStringLiteral("crtScanlines"));
            value != nullptr) {
            value->setValue(35);
        }
        if (auto* value = dialog->findChild<QSlider*>(QStringLiteral("crtPhosphor"));
            value != nullptr) {
            value->setValue(25);
        }
        dialog->accept();
        QApplication::processEvents();
    }
    TW_CHECK_EQ(window->crtSettings().scanlines, 35);
    {
        MainWindow second;
        TW_CHECK_EQ(second.crtSettings().scanlines, 35);
        TW_CHECK_EQ(second.crtSettings().phosphor, 25);
    }

    // Leave the developer's settings as they were: a saved CRT effect would make
    // the next test's fresh window start with the overlay showing.
    window->setCrtSettings(off);
    QSettings settings;
    for (const char* key : {"display/crt/scanlines", "display/crt/vignette",
                            "display/crt/curvature", "display/crt/flicker",
                            "display/crt/phosphor"}) {
        settings.remove(QString::fromLatin1(key));
    }
}


void test_screen_presets_pair_a_theme_with_its_crt_values() {
    announce("screen presets");
    const std::unique_ptr<MainWindow> window = shownWindow();
    CrtOverlay* overlay = window->crtOverlay();
    TW_CHECK(overlay != nullptr);
    if (overlay == nullptr) {
        return;
    }
    const QString original = window->currentThemeName();

    auto* amber = window->findChild<QAction*>(QStringLiteral("screenPresetAmber"));
    auto* green = window->findChild<QAction*>(QStringLiteral("screenPresetGreen"));
    auto* white = window->findChild<QAction*>(QStringLiteral("screenPresetWhite"));
    auto* normal = window->findChild<QAction*>(QStringLiteral("screenPresetNormal"));
    TW_CHECK(amber != nullptr);
    TW_CHECK(green != nullptr);
    TW_CHECK(white != nullptr);
    TW_CHECK(normal != nullptr);
    if (amber == nullptr || green == nullptr || white == nullptr || normal == nullptr) {
        return;
    }

    // A preset is a theme *and* the effect values that suit it: the theme alone
    // would be a colour scheme, and the CRT values alone would be a wash over
    // whatever was on screen.
    amber->trigger();
    TW_CHECK_EQ(window->currentThemeName(), QStringLiteral("Amber Monochrome"));
    TW_CHECK_EQ(window->crtSettings().phosphor_colour, QColor(255, 176, 0));
    TW_CHECK(window->crtSettings().scanlines > 0);
    TW_CHECK_EQ(overlay->settings().scanlines, window->crtSettings().scanlines);
    TW_CHECK(overlay->isVisible());

    green->trigger();
    TW_CHECK_EQ(window->currentThemeName(), QStringLiteral("Green Monochrome"));
    TW_CHECK_EQ(window->crtSettings().phosphor_colour, QColor(0, 255, 0));

    white->trigger();
    TW_CHECK_EQ(window->currentThemeName(), QStringLiteral("High Contrast Dark"));
    TW_CHECK(window->crtSettings().phosphor_colour.red() > 200);
    TW_CHECK(window->crtSettings().curvature > 0);

    // Normal is the way back: a normal theme and no effects at all.
    normal->trigger();
    TW_CHECK_EQ(window->currentThemeName(), QStringLiteral("VS Code Dark"));
    TW_CHECK_EQ(window->crtSettings().scanlines, 0);
    TW_CHECK_EQ(window->crtSettings().vignette, 0);
    TW_CHECK_EQ(window->crtSettings().curvature, 0);
    TW_CHECK_EQ(window->crtSettings().phosphor, 0);
    TW_CHECK(!overlay->isVisible());

    // Leave no saved effect behind for the tests that build a window next.
    window->applyTheme(original);
    QSettings{}.remove(QStringLiteral("appearance/theme"));
    QSettings settings;
    for (const char* key : {"display/crt/scanlines", "display/crt/vignette",
                            "display/crt/curvature", "display/crt/flicker",
                            "display/crt/phosphor"}) {
        settings.remove(QString::fromLatin1(key));
    }
}

}  // namespace tw::ui_layout
