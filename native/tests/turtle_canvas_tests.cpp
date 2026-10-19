// SPDX-License-Identifier: MIT
//
// Tests for the turtle canvas: the state mirror, the ink it records, and the
// projection.
//
// The projection gets the most attention because it is the one part that cannot
// be checked by looking at a picture. A canvas that draws a mirrored or
// quarter-turned version of the same program produces something that looks like
// a perfectly good drawing, so a visual check passes and the bug survives. The
// reference implementation in `graphics/turtle_state.py` is the specification
// here, and its conventions are unusual enough to be worth stating as
// assertions:
//
//   * heading 0 is **north** (+y), not east
//   * heading increases **clockwise**, so `right` adds
//   * `forward(d)` moves by `(d*sin h, d*cos h)`
//   * world +y is up, so the screen projection flips it
//
// Every one of those is a place where choosing the conventional alternative
// would have been wrong.
//
// Runs offscreen, so it needs no display and belongs in the normal CTest set.

#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <memory>

#include <QApplication>
#include <QCoreApplication>
#include <QFile>
#include <QImage>
#include <QPointF>
#include <QSettings>
#include <QSlider>
#include <QTemporaryDir>
#include <QWidget>

#include "check.hpp"
#include "timewarp/canvas/command.hpp"
#include "timewarp/ui/scrubber.hpp"
#include "timewarp/ui/turtle_canvas.hpp"

namespace {

using timewarp::canvas::CanvasCommand;
using timewarp::canvas::CanvasCommandKind;
using timewarp::canvas::CanvasCommandView;
using timewarp::ui::Scrubber;
using timewarp::ui::TurtleCanvas;

void announce(const char* name) {
    std::printf("-- %s\n", name);
    std::fflush(stdout);
}

/// Applies a bare command, which is what the bridge hands over for every kind
/// that carries no trailing point list or text.
void push(TurtleCanvas& canvas, const CanvasCommand& command) {
    canvas.apply(CanvasCommandView{command, {}, {}});
}

void push_forward(TurtleCanvas& canvas, double distance) {
    auto command = timewarp::canvas::make_motion(CanvasCommandKind::Forward, distance);
    push(canvas, command);
}

void push_right(TurtleCanvas& canvas, double degrees) {
    auto command = timewarp::canvas::make_motion(CanvasCommandKind::Right, degrees);
    push(canvas, command);
}

/// A canvas with a known size, because the projection is derived from it.
[[nodiscard]] TurtleCanvas* sizedCanvas(int width = 400, int height = 400) {
    auto* canvas = new TurtleCanvas;
    canvas->resize(width, height);
    return canvas;
}

[[nodiscard]] bool nearly(double actual, double expected, double tolerance = 1e-9) {
    return std::abs(actual - expected) <= tolerance;
}

// ---------------------------------------------------------------------------
// The conventions
// ---------------------------------------------------------------------------

void test_heading_zero_is_north_and_right_turns_clockwise() {
    announce("heading convention");
    const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};

    // 0 is north, so the first move is straight up in world coordinates.
    push_forward(*canvas, 100.0);
    TW_CHECK(nearly(canvas->turtlePosition().x(), 0.0));
    TW_CHECK(nearly(canvas->turtlePosition().y(), 100.0));

    // Right adds to the heading, so 90 is east and the next move goes +x. The
    // standard mathematical convention would put 90 at north-west and produce a
    // drawing rotated by a quarter turn.
    push_right(*canvas, 90.0);
    TW_CHECK(nearly(canvas->turtleHeading(), 90.0));
    push_forward(*canvas, 100.0);
    TW_CHECK(nearly(canvas->turtlePosition().x(), 100.0));
    TW_CHECK(nearly(canvas->turtlePosition().y(), 100.0));

    // A classic square: forward, right, forward, right. With a mirrored `right`
    // it would close into the shape on the other side of the start line.
    TW_CHECK_EQ(canvas->strokeCount(), 2);
    TW_CHECK(nearly(canvas->totalStrokeLength(), 200.0));

    // Left is the negative direction, and the heading is kept in [0, 360) the
    // way Python's `%` does -- so one left turn from 90 lands on 270, not -90.
    auto left = timewarp::canvas::make_motion(CanvasCommandKind::Left, 180.0);
    push(*canvas, left);
    TW_CHECK(nearly(canvas->turtleHeading(), 270.0));
}

void test_world_y_is_up_on_screen() {
    announce("projection");
    const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas(400, 400)};

    // The default window is 500 world units across, so 400 pixels is 0.8
    // pixels per unit and the origin sits at the centre.
    const QPointF origin = canvas->toScreen(QPointF{0.0, 0.0});
    TW_CHECK(nearly(origin.x(), 200.0, 0.5));
    TW_CHECK(nearly(origin.y(), 200.0, 0.5));

    // The flip: +y in the world is *up* the screen, which means a smaller screen
    // y. Without it every drawing is upside down, and for a symmetric shape that
    // is invisible.
    const QPointF north = canvas->toScreen(QPointF{0.0, 100.0});
    TW_CHECK(north.y() < origin.y());
    TW_CHECK(nearly(origin.y() - north.y(), 80.0, 1.0));

    const QPointF east = canvas->toScreen(QPointF{100.0, 0.0});
    TW_CHECK(east.x() > origin.x());

    // Round trip, so a click maps back to the coordinate it came from.
    const QPointF world{37.5, -12.25};
    const QPointF back = canvas->toWorld(canvas->toScreen(world));
    TW_CHECK(nearly(back.x(), world.x(), 1e-6));
    TW_CHECK(nearly(back.y(), world.y(), 1e-6));
}

void test_absolute_moves_and_home() {
    announce("moves and home");
    const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};

    auto goto_xy = timewarp::canvas::make_position(30.0, 40.0);
    push(*canvas, goto_xy);
    TW_CHECK(nearly(canvas->turtlePosition().x(), 30.0));
    TW_CHECK(nearly(canvas->turtlePosition().y(), 40.0));
    // `goto` draws when the pen is down, which is what makes it a move rather
    // than a jump.
    TW_CHECK_EQ(canvas->strokeCount(), 1);

    push_right(*canvas, 45.0);
    auto home = timewarp::canvas::make_command(CanvasCommandKind::Home);
    push(*canvas, home);
    TW_CHECK(nearly(canvas->turtlePosition().x(), 0.0));
    TW_CHECK(nearly(canvas->turtlePosition().y(), 0.0));
    // `home` faces north again as well as returning: the reference sets the
    // heading, and a version that only moved would leave the turtle at the
    // origin pointing the wrong way.
    TW_CHECK(nearly(canvas->turtleHeading(), 0.0));

    // SetX and SetY move one axis, drawing the way goto does.
    const int before = canvas->strokeCount();
    auto set_x = timewarp::canvas::make_position(10.0, 0.0);
    set_x.kind = CanvasCommandKind::SetX;
    push(*canvas, set_x);
    TW_CHECK(nearly(canvas->turtlePosition().x(), 10.0));
    TW_CHECK_EQ(canvas->strokeCount(), before + 1);
}

void test_pen_state_controls_whether_a_move_leaves_ink() {
    announce("pen up and down");
    const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};

    auto pen_up = timewarp::canvas::make_command(CanvasCommandKind::PenUp);
    push(*canvas, pen_up);
    push_forward(*canvas, 50.0);
    TW_CHECK_EQ(canvas->strokeCount(), 0);
    TW_CHECK(!canvas->penIsDown());
    TW_CHECK(nearly(canvas->turtlePosition().y(), 50.0));

    auto pen_down = timewarp::canvas::make_command(CanvasCommandKind::PenDown);
    push(*canvas, pen_down);
    push_forward(*canvas, 50.0);
    TW_CHECK_EQ(canvas->strokeCount(), 1);
    TW_CHECK(nearly(canvas->totalStrokeLength(), 50.0));
    TW_CHECK(nearly(canvas->turtlePosition().y(), 100.0));
}

void test_pen_colour_and_width_are_recorded() {
    announce("pen style");
    const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};
    canvas->resize(400, 400);

    auto colour = timewarp::canvas::make_color(CanvasCommandKind::SetPenColor,
                                               timewarp::canvas::make_rgb(200, 30, 90));
    push(*canvas, colour);
    TW_CHECK_EQ(canvas->penColor().red(), 200);
    TW_CHECK_EQ(canvas->penColor().green(), 30);
    TW_CHECK_EQ(canvas->penColor().blue(), 90);

    auto width = timewarp::canvas::make_motion(CanvasCommandKind::SetPenWidth, 4.0);
    push(*canvas, width);
    TW_CHECK(nearly(canvas->penWidth(), 4.0));

    // A zero or negative width is refused rather than stored: an invisible line
    // is a bug the user cannot diagnose.
    auto bad_width = timewarp::canvas::make_motion(CanvasCommandKind::SetPenWidth, 0.0);
    push(*canvas, bad_width);
    TW_CHECK(nearly(canvas->penWidth(), 4.0));
}

// ---------------------------------------------------------------------------
// Composite commands
// ---------------------------------------------------------------------------

void test_a_circle_is_walked_as_a_polygon() {
    announce("circle");
    const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};

    // The reference draws a circle as `max(12, |extent|/10)` chords, each a
    // `forward` of `r * radians(step)` followed by a `right` of one step. For a
    // full turn that is 36 chords, plus the closing `goto` the reference emits
    // when the extent reaches 360.
    auto circle = timewarp::canvas::make_circle(50.0);
    push(*canvas, circle);
    TW_CHECK(canvas->strokeCount() >= 36);
    TW_CHECK(canvas->strokeCount() <= 37);

    // A 50-unit radius has a circumference of about 314, so the walk is a
    // polygon inscribed in that path rather than the path itself.
    TW_CHECK(std::abs(canvas->totalStrokeLength() - 314.0) < 8.0);

    // A full turn ends where it began, facing the way it began: the whole point
    // of `circle` being usable in a loop without drifting.
    TW_CHECK(std::abs(canvas->turtlePosition().x()) < 1e-6);
    TW_CHECK(std::abs(canvas->turtlePosition().y()) < 1e-6);
    TW_CHECK(nearly(canvas->turtleHeading(), 0.0, 1e-6));

    // An extent of zero draws nothing at all, matching the reference's early
    // return rather than drawing a degenerate point.
    const std::unique_ptr<TurtleCanvas> other{sizedCanvas()};
    auto none = timewarp::canvas::make_circle(50.0, 0.0);
    push(*other, none);
    TW_CHECK_EQ(other->strokeCount(), 0);
}

void test_clear_wipes_the_paper_and_reset_starts_over() {
    announce("clear and reset");
    const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};

    push_forward(*canvas, 100.0);
    push_right(*canvas, 90.0);
    auto width = timewarp::canvas::make_motion(CanvasCommandKind::SetPenWidth, 5.0);
    push(*canvas, width);
    TW_CHECK_EQ(canvas->strokeCount(), 1);

    auto clear = timewarp::canvas::make_command(CanvasCommandKind::Clear);
    push(*canvas, clear);
    TW_CHECK_EQ(canvas->strokeCount(), 0);
    TW_CHECK(canvas->contentBounds().isEmpty());
    // `clear` is "wipe the paper", not "start the program over": where the
    // turtle is and what it holds are untouched.
    TW_CHECK(nearly(canvas->turtlePosition().y(), 100.0));
    TW_CHECK(nearly(canvas->turtleHeading(), 90.0));
    TW_CHECK(nearly(canvas->penWidth(), 5.0));

    auto reset = timewarp::canvas::make_command(CanvasCommandKind::Reset);
    push(*canvas, reset);
    TW_CHECK_EQ(canvas->strokeCount(), 0);
    TW_CHECK(canvas->penIsDown());
    TW_CHECK(canvas->turtleIsVisible());
    TW_CHECK(nearly(canvas->turtlePosition().x(), 0.0));
    TW_CHECK(nearly(canvas->turtlePosition().y(), 0.0));
    TW_CHECK(nearly(canvas->turtleHeading(), 0.0));
    TW_CHECK(nearly(canvas->penWidth(), 1.0));
    TW_CHECK_EQ(canvas->penColor().red(), 0);
    // A reset also clears the counters, so a fresh run's diagnostics are its own.
    TW_CHECK(canvas->commandsApplied() <= 1);
}

void test_show_and_hide_the_turtle() {
    announce("turtle visibility");
    const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};
    TW_CHECK(canvas->turtleIsVisible());
    auto hide = timewarp::canvas::make_command(CanvasCommandKind::HideTurtle);
    push(*canvas, hide);
    TW_CHECK(!canvas->turtleIsVisible());
    auto show = timewarp::canvas::make_command(CanvasCommandKind::ShowTurtle);
    push(*canvas, show);
    TW_CHECK(canvas->turtleIsVisible());
}

void test_shape_primitives_are_recorded() {
    announce("shape primitives");
    const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};

    auto rect = timewarp::canvas::make_draw_rect(0.0, 0.0, 40.0, 20.0);
    rect.color = timewarp::canvas::make_rgb(10, 20, 30);
    push(*canvas, rect);
    TW_CHECK_EQ(canvas->shapeCount(), 1);
    TW_CHECK_EQ(canvas->strokeCount(), 0);

    auto point = timewarp::canvas::make_draw_point(5.0, 5.0,
                                                   timewarp::canvas::make_rgb(1, 2, 3));
    push(*canvas, point);
    TW_CHECK_EQ(canvas->shapeCount(), 2);

    // A polyline arrives as trailing point data, which is the one path where the
    // view's `points` span matters. Three points are two segments, and the closed
    // flag adds the one that joins the ends.
    const double points[] = {0.0, 0.0, 10.0, 0.0, 10.0, 10.0};
    CanvasCommand polyline =
        timewarp::canvas::make_command(CanvasCommandKind::DrawPolyline);
    polyline.x = 2.0;
    canvas->apply(CanvasCommandView{polyline, points, {}});
    TW_CHECK_EQ(canvas->strokeCount(), 2);

    CanvasCommand closed = polyline;
    closed.flags = timewarp::canvas::kCanvasFlagClosed;
    canvas->apply(CanvasCommandView{closed, points, {}});
    TW_CHECK_EQ(canvas->strokeCount(), 5);
}

void test_unhandled_commands_are_counted_rather_than_lost() {
    announce("unhandled commands");
    const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};

    // Gradients, sprites and palette cycling are in the command set but not in
    // this widget. They must be counted: a shape that silently never appears is
    // indistinguishable from a program that never drew it.
    auto gradient = timewarp::canvas::make_command(CanvasCommandKind::DrawRectGradient);
    push(*canvas, gradient);
    auto sprite = timewarp::canvas::make_command(CanvasCommandKind::ShowSprite);
    push(*canvas, sprite);
    // Compared against unsigned literals: the counters are unsigned, and mixing
    // them with `int` is a sign comparison the project builds with -Werror on.
    TW_CHECK_EQ(canvas->commandsIgnored(), std::uint64_t{2});
    TW_CHECK_EQ(canvas->commandsApplied(), std::uint64_t{0});

    // An ignored command still counts as processed for the caller's purposes:
    // nothing threw, and the next real command applies normally.
    push_forward(*canvas, 10.0);
    TW_CHECK_EQ(canvas->commandsApplied(), std::uint64_t{1});
    TW_CHECK_EQ(canvas->strokeCount(), 1);
}

void test_zoom_is_clamped_and_fit_recentres() {
    announce("zoom and fit");
    const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};

    canvas->setZoom(1e9);
    TW_CHECK(canvas->zoom() <= 50.0);
    canvas->setZoom(-5.0);
    TW_CHECK(canvas->zoom() >= 0.05);

    push_forward(*canvas, 100.0);
    const QPointF before = canvas->toScreen(QPointF{0.0, 100.0});
    canvas->fitToContent();
    TW_CHECK(nearly(canvas->zoom(), 1.0));
    const QPointF after = canvas->toScreen(QPointF{0.0, 100.0});
    // The fit recentres, so the same world point projects somewhere else; what
    // matters is that it is still on the widget rather than off the edge.
    TW_CHECK(after.x() >= 0.0 && after.x() <= 400.0);
    TW_CHECK(after.y() >= 0.0 && after.y() <= 400.0);
    TW_CHECK(before != after);

    canvas->setGridVisible(false);
    TW_CHECK(!canvas->gridVisible());
    canvas->setGridVisible(false);
    TW_CHECK(!canvas->gridVisible());
}

void test_content_bounds_include_every_drawn_axis() {
    announce("content bounds");
    const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};

    // A purely horizontal stroke has zero height, and a purely vertical one has
    // zero width. Bounds that discarded either -- because a degenerate rectangle
    // "is not valid" -- would leave the fit unable to see half a drawing.
    push_right(*canvas, 90.0);
    push_forward(*canvas, 100.0);
    const QRectF horizontal = canvas->contentBounds();
    TW_CHECK(nearly(horizontal.width(), 100.0));
    TW_CHECK(nearly(horizontal.height(), 0.0));

    auto home = timewarp::canvas::make_command(CanvasCommandKind::Home);
    push(*canvas, home);
    push_forward(*canvas, 100.0);
    const QRectF both = canvas->contentBounds();
    TW_CHECK(nearly(both.width(), 100.0));
    TW_CHECK(nearly(both.height(), 100.0));
}

// ---------------------------------------------------------------------------
// Export
// ---------------------------------------------------------------------------

void test_the_drawing_can_be_exported() {
    announce("export a drawing");
    TurtleCanvas canvas;
    canvas.resize(200, 200);
    push_forward(canvas, 60.0);
    push_right(canvas, 90.0);
    push_forward(canvas, 60.0);
    // The projection is derived from the content, and a widget that has never been
    // shown fits itself when a program finishes -- which is the state an export
    // happens in. Without this the scale is whatever an unlaid-out canvas has.
    canvas.fitToContent();

    const QImage image = canvas.renderedDrawing(1.0);
    TW_CHECK_EQ(image.size(), QSize(200, 200));
    // The grid is chrome and not ink: a corner of the picture is background, so an
    // exported drawing is a drawing rather than a screenshot of the window.
    TW_CHECK_EQ(image.pixelColor(0, 0), canvas.background());

    int ink = 0;
    for (int y = 0; y < image.height(); ++y) {
        for (int x = 0; x < image.width(); ++x) {
            if (image.pixelColor(x, y) != canvas.background()) {
                ++ink;
            }
        }
    }
    TW_CHECK(ink > 100);

    // Twice the on-screen size by default, and the drawing scales with it.
    TW_CHECK_EQ(canvas.renderedDrawing().size(), QSize(400, 400));

    const QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    const QString path = dir.filePath(QStringLiteral("drawing.png"));
    TW_CHECK(canvas.exportDrawing(path, nullptr, 1.0));
    TW_CHECK(QFile::exists(path));
    const QImage reloaded{path};
    TW_CHECK_EQ(reloaded.size(), QSize(200, 200));

    // The file holds the same drawing rather than a blank of the right size. A
    // count of inked pixels says that without guessing where a one-pixel line
    // landed, which is the kind of assertion that fails on a different Qt.
    int reloaded_ink = 0;
    for (int y = 0; y < reloaded.height(); ++y) {
        for (int x = 0; x < reloaded.width(); ++x) {
            if (reloaded.pixelColor(x, y) != canvas.background()) {
                ++reloaded_ink;
            }
        }
    }
    TW_CHECK_EQ(reloaded_ink, ink);

    // An extension nothing recognises is a failure the caller hears about rather
    // than a file written in some default format.
    QString error;
    TW_CHECK(!canvas.exportDrawing(dir.filePath(QStringLiteral("drawing.what")), &error));
    TW_CHECK(!error.isEmpty());
}

// ---------------------------------------------------------------------------
// Scrubbing
// ---------------------------------------------------------------------------

void test_scrubbing_shows_a_prefix_without_losing_the_drawing() {
    announce("scrub the drawing");
    TurtleCanvas canvas;
    canvas.resize(400, 400);
    for (int segment = 0; segment < 100; ++segment) {
        push_forward(canvas, 2.0);
        push_right(canvas, 3.6);
    }
    canvas.fitToContent();
    TW_CHECK_EQ(canvas.frameCount(), 100);
    TW_CHECK_EQ(canvas.visibleFrameCount(), 100);
    TW_CHECK_EQ(canvas.visibleStrokeCount(), 100);

    const QImage before = canvas.renderedDrawing(1.0);

    canvas.setVisibleFrame(40);
    TW_CHECK_EQ(canvas.visibleFrameCount(), 40);
    TW_CHECK_EQ(canvas.visibleStrokeCount(), 40);
    // Hidden, not deleted: the slider must be able to come back.
    TW_CHECK_EQ(canvas.strokeCount(), 100);
    TW_CHECK_EQ(canvas.frameCount(), 100);

    // Scrubbing is a view. An export of a half-drawn picture would be a
    // surprise, so the whole drawing is always written.
    TW_CHECK(canvas.renderedDrawing(1.0) == before);

    canvas.setVisibleFrame(-5);
    TW_CHECK_EQ(canvas.visibleFrameCount(), 0);
    canvas.setVisibleFrame(1000);
    TW_CHECK_EQ(canvas.visibleFrameCount(), 100);

    // New ink rejoins the live view rather than leaving the user in the past.
    canvas.setVisibleFrame(40);
    push_forward(canvas, 2.0);
    TW_CHECK_EQ(canvas.frameCount(), 101);
    TW_CHECK_EQ(canvas.visibleFrameCount(), 101);

    auto clear = timewarp::canvas::make_command(CanvasCommandKind::Clear);
    push(canvas, clear);
    TW_CHECK_EQ(canvas.frameCount(), 0);
    TW_CHECK_EQ(canvas.visibleFrameCount(), 0);
}

void test_history_keeps_strokes_and_shapes_in_draw_order() {
    announce("history order");
    const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};

    push_forward(*canvas, 10.0);
    auto rect = timewarp::canvas::make_draw_rect(0.0, 0.0, 20.0, 10.0);
    push(*canvas, rect);
    push_forward(*canvas, 10.0);

    TW_CHECK_EQ(canvas->frameCount(), 3);
    TW_CHECK_EQ(canvas->history()[0].kind, TurtleCanvas::HistoryEntry::Kind::Stroke);
    TW_CHECK_EQ(canvas->history()[1].kind, TurtleCanvas::HistoryEntry::Kind::Shape);
    TW_CHECK_EQ(canvas->history()[2].kind, TurtleCanvas::HistoryEntry::Kind::Stroke);

    // The counts are of what is shown, so a rectangle inside the prefix counts
    // there and not in the strokes it sits between.
    canvas->setVisibleFrame(1);
    TW_CHECK_EQ(canvas->visibleStrokeCount(), 1);
    TW_CHECK_EQ(canvas->visibleShapeCount(), 0);
    canvas->setVisibleFrame(2);
    TW_CHECK_EQ(canvas->visibleStrokeCount(), 1);
    TW_CHECK_EQ(canvas->visibleShapeCount(), 1);
}

void test_the_scrubber_plays_a_drawing_back() {
    announce("scrubber");
    // One frame per tick for a short drawing, ten for a long one, and never zero
    // -- a step of zero is a play button that does nothing.
    TW_CHECK_EQ(Scrubber::stepForFrames(600), 1);
    TW_CHECK_EQ(Scrubber::stepForFrames(6000), 10);
    TW_CHECK_EQ(Scrubber::stepForFrames(0), 1);

    Scrubber scrubber;
    int last = -1;
    int calls = 0;
    QObject::connect(&scrubber, &Scrubber::frameRequested, &scrubber, [&](int frame) {
        last = frame;
        ++calls;
    });

    scrubber.setFrames(6000);
    TW_CHECK_EQ(scrubber.frames(), 6000);
    TW_CHECK_EQ(scrubber.visibleFrame(), 6000);

    // Playing from the end restarts, which is the only useful meaning of pressing
    // play on a finished drawing.
    scrubber.togglePlay();
    TW_CHECK(scrubber.playing());
    TW_CHECK_EQ(last, 0);
    scrubber.advance();
    TW_CHECK_EQ(last, 10);

    for (int tick = 0; tick < 1000 && scrubber.playing(); ++tick) {
        scrubber.advance();
    }
    TW_CHECK(!scrubber.playing());
    TW_CHECK_EQ(last, 6000);

    // A programmatic move is not a user request: the window would otherwise echo
    // every frame back to the canvas.
    const int seen = calls;
    scrubber.setVisibleFrame(30);
    TW_CHECK_EQ(calls, seen);
    TW_CHECK_EQ(scrubber.visibleFrame(), 30);

    // The slider is the user's control, so its motion does ask for a frame.
    auto* slider = scrubber.findChild<QSlider*>(QStringLiteral("scrubberSlider"));
    TW_CHECK(slider != nullptr);
    slider->setValue(60);
    TW_CHECK_EQ(last, 60);
    TW_CHECK_EQ(scrubber.visibleFrame(), 60);

    scrubber.setFrames(0);
    TW_CHECK(!scrubber.isEnabled());
    TW_CHECK_EQ(scrubber.visibleFrame(), 0);
}

}  // namespace

int main(int argc, char** argv) {    // Set before QApplication exists, or Qt tries to open a real display.
    qputenv("QT_QPA_PLATFORM", "offscreen");
    QApplication app{argc, argv};
    QCoreApplication::setOrganizationName(QStringLiteral("TimeWarpTests"));
    QCoreApplication::setApplicationName(QStringLiteral("TurtleCanvasTests"));
    QSettings::setDefaultFormat(QSettings::IniFormat);

    test_heading_zero_is_north_and_right_turns_clockwise();
    test_world_y_is_up_on_screen();
    test_absolute_moves_and_home();
    test_pen_state_controls_whether_a_move_leaves_ink();
    test_pen_colour_and_width_are_recorded();

    test_a_circle_is_walked_as_a_polygon();
    test_clear_wipes_the_paper_and_reset_starts_over();
    test_show_and_hide_the_turtle();
    test_shape_primitives_are_recorded();
    test_unhandled_commands_are_counted_rather_than_lost();
    test_zoom_is_clamped_and_fit_recentres();
    test_content_bounds_include_every_drawn_axis();
    test_the_drawing_can_be_exported();
    test_scrubbing_shows_a_prefix_without_losing_the_drawing();
    test_history_keeps_strokes_and_shapes_in_draw_order();
    test_the_scrubber_plays_a_drawing_back();

    return tw::test::summary("turtle_canvas");
}
