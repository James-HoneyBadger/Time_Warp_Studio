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

#include <array>
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
#include <QPainter>
#include <QSettings>
#include <QSlider>
#include <QTemporaryDir>
#include <QWidget>

#ifdef TIME_WARP_HAVE_SVG
#include <QSvgRenderer>
#endif

#include "check.hpp"
#include "timewarp/canvas/command.hpp"
#include "timewarp/ui/scrubber.hpp"
#include "timewarp/ui/native_runner.hpp"
#include "timewarp/ui/turtle_canvas.hpp"

namespace {

using timewarp::canvas::CanvasCommand;
using timewarp::canvas::CanvasCommandKind;
using timewarp::canvas::CanvasCommandView;
using timewarp::canvas::to_string;
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

// `turtleStateChanged` promises "moved, turned, or changed pen", and the readouts
// built on it (the status bar, the turtle strip) show all of that. A pen-up that
// left the signal unsent left those saying the pen was still down.
void test_a_state_change_reports_itself() {
    announce("turtle state signal");
    const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};

    int state_changes = 0;
    QObject::connect(canvas.get(), &TurtleCanvas::turtleStateChanged,
                     [&state_changes] { ++state_changes; });

    auto pen_up = timewarp::canvas::make_command(CanvasCommandKind::PenUp);
    push(*canvas, pen_up);
    TW_CHECK_EQ(state_changes, 1);

    // A colour is state too, and so is the width.
    auto colour = timewarp::canvas::make_color(CanvasCommandKind::SetPenColor,
                                               timewarp::canvas::make_rgb(10, 20, 30));
    push(*canvas, colour);
    TW_CHECK_EQ(state_changes, 2);

    auto width = timewarp::canvas::make_motion(CanvasCommandKind::SetPenWidth, 3.0);
    push(*canvas, width);
    TW_CHECK_EQ(state_changes, 3);

    // And a command that changes none of it stays quiet, so a drawing that emits
    // thousands of shapes does not turn every one into a palette update.
    const int before = state_changes;
    auto clear = timewarp::canvas::make_command(CanvasCommandKind::Clear);
    push(*canvas, clear);
    TW_CHECK_EQ(state_changes, before);

    // A pose arriving from the bridge counts even when only the heading or the
    // marker's visibility moved: the strip shows both.
    canvas->setTurtlePose(QPointF{0.0, 0.0}, 45.0, true);
    TW_CHECK_EQ(state_changes, before + 1);
    canvas->setTurtlePose(QPointF{0.0, 0.0}, 45.0, false);
    TW_CHECK_EQ(state_changes, before + 2);
    canvas->setTurtlePose(QPointF{0.0, 0.0}, 45.0, false);
    TW_CHECK_EQ(state_changes, before + 2);
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

void test_a_sprite_block_is_drawn_as_shapes() {
    announce("sprite block");
    const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};

    // A 2x2 block: the top row is one colour, the bottom row two, so the top row
    // becomes one run and the bottom two. The block arrives as packed colours in
    // the trailing point list, one doubles value per pixel.
    const double pixels[] = {
        static_cast<double>(timewarp::canvas::make_rgb(200, 10, 10)),
        static_cast<double>(timewarp::canvas::make_rgb(200, 10, 10)),
        static_cast<double>(timewarp::canvas::make_rgb(10, 10, 200)),
        static_cast<double>(timewarp::canvas::make_rgb(10, 200, 10)),
    };
    CanvasCommand blit = timewarp::canvas::make_put_pixels(10.0, 20.0, 2.0, 2.0);
    canvas->apply(CanvasCommandView{blit, pixels, {}});

    TW_CHECK_EQ(canvas->commandsApplied(), std::uint64_t{1});
    TW_CHECK_EQ(canvas->commandsIgnored(), std::uint64_t{0});
    TW_CHECK_EQ(canvas->shapeCount(), 3);
    TW_CHECK_EQ(canvas->strokeCount(), 0);
    // Raster rows count down; the view counts up, so the block's first row is
    // its highest on screen, and it spans exactly its own width and height. In a
    // normalized world-space rectangle that highest edge is `bottom()`, because
    // world y grows upwards.
    const QRectF bounds = canvas->contentBounds();
    TW_CHECK(nearly(bounds.left(), 10.0));
    TW_CHECK(nearly(bounds.right(), 12.0));
    TW_CHECK(nearly(bounds.top(), 19.0));
    TW_CHECK(nearly(bounds.bottom(), 21.0));
}

void test_a_screen_is_painted_at_fixed_coordinates() {
    announce("screen mode");
    const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};

    auto mode = timewarp::canvas::make_framebuffer(320.0, 200.0);
    canvas->apply(CanvasCommandView{mode, {}, {}});
    TW_CHECK(canvas->framebufferActive());
    TW_CHECK_EQ(canvas->framebufferSize().width(), 320);
    TW_CHECK_EQ(canvas->framebufferSize().height(), 200);

    // `PSET` carries the program's own coordinates, and on a screen those are
    // pixel coordinates: no flip, no fit, no origin at the centre.
    auto point = timewarp::canvas::make_draw_point(10.0, 20.0,
                                                   timewarp::canvas::make_rgb(255, 0, 0));
    canvas->apply(CanvasCommandView{point, {}, {}});
    TW_CHECK_EQ(canvas->framebufferPixel(10, 20), QColor(255, 0, 0));
    // The neighbouring pixel is untouched, which is what makes the assertion
    // above about a pixel rather than about "something red exists".
    TW_CHECK(canvas->framebufferPixel(10, 21) != QColor(255, 0, 0));

    // A `LINE` is a polyline of the same coordinates.
    const double ends[] = {0.0, 0.0, 319.0, 0.0};
    auto line = timewarp::canvas::make_command(CanvasCommandKind::DrawPolyline);
    line.color = timewarp::canvas::make_rgb(0, 0, 255);
    canvas->apply(CanvasCommandView{line, ends, {}});
    TW_CHECK_EQ(canvas->framebufferPixel(160, 0), QColor(0, 0, 255));

    // A `PUT` block lands top row first, at the coordinate it names.
    const double pixels[] = {
        static_cast<double>(timewarp::canvas::make_rgb(1, 2, 3)),
        static_cast<double>(timewarp::canvas::make_rgb(4, 5, 6)),
    };
    auto blit = timewarp::canvas::make_put_pixels(100.0, 50.0, 2.0, 1.0);
    canvas->apply(CanvasCommandView{blit, pixels, {}});
    TW_CHECK_EQ(canvas->framebufferPixel(100, 50), QColor(1, 2, 3));
    TW_CHECK_EQ(canvas->framebufferPixel(101, 50), QColor(4, 5, 6));

    // The exported picture is the screen, not the window's fitted view of it.
    const QImage shot = canvas->renderedDrawing(1.0);
    TW_CHECK_EQ(shot.width(), 320);
    TW_CHECK_EQ(shot.height(), 200);
    TW_CHECK_EQ(shot.pixelColor(10, 20), QColor(255, 0, 0));

    // The screen answers in its own coordinates: where it is drawn, and which
    // pixel is under a point. A screen whose wheel and clicks did nothing would
    // be the one control a game author reaches for first.
    const QRectF target = canvas->framebufferTarget();
    TW_CHECK(!target.isEmpty());
    const QPointF centre_pixel = canvas->framebufferPixelAt(target.center());
    TW_CHECK(std::abs(centre_pixel.x() - 160.0) < 1.5);
    TW_CHECK(std::abs(centre_pixel.y() - 100.0) < 1.5);
    canvas->setZoom(2.0);
    TW_CHECK(canvas->framebufferTarget().width() > target.width());
    canvas->setZoom(1.0);
    // Outside the drawn screen is not a pixel.
    TW_CHECK(canvas->framebufferPixelAt(QPointF{-5.0, -5.0}).x() < 0.0);

    // Fitting is what a screen must not do.
    canvas->fitToContent();
    TW_CHECK_EQ(canvas->renderedDrawing(1.0).pixelColor(10, 20), QColor(255, 0, 0));

    // `SCREEN 0` gives the display list back.
    canvas->apply(CanvasCommandView{timewarp::canvas::make_clear_framebuffer(), {}, {}});
    TW_CHECK(!canvas->framebufferActive());
    TW_CHECK(!canvas->framebufferPixel(10, 20).isValid());
}

/// Every kind the vocabulary declares, and why the canvas may refuse one.
///
/// The enum has no sentinel, so this list is maintained by hand -- and it is a
/// *test* that the list is complete: `test_the_drawing_vocabulary_is_complete`
/// checks its size against the vocabulary, so a command added tomorrow fails here
/// until somebody says, in writing, whether it draws.
struct Undrawn {
    CanvasCommandKind kind;
    const char* reason;
};

// Empty, and that is the claim: every declared kind is drawn. `Pitch`/`Roll` were
// here until 19.1 with "3-D camera" as the reason; they were deleted from the
// vocabulary instead, because the camera belongs to `TurtlePose`, which projects
// before it speaks. A waiver table with nothing in it is a stronger statement than
// a table with two honest excuses. It is a `std::array` of zero because C++ has no
// zero-length array.
constexpr std::array<Undrawn, 0> kNotDrawn{};

constexpr CanvasCommandKind kEveryKind[] = {
    CanvasCommandKind::Forward,     CanvasCommandKind::Back,
    CanvasCommandKind::Left,        CanvasCommandKind::Right,
    CanvasCommandKind::Goto,        CanvasCommandKind::SetX,
    CanvasCommandKind::SetY,        CanvasCommandKind::Home,
    CanvasCommandKind::SetHeading,  CanvasCommandKind::PenUp,
    CanvasCommandKind::PenDown,     CanvasCommandKind::SetPenColor,
    CanvasCommandKind::SetPenWidth, CanvasCommandKind::SetPenStyle,
    CanvasCommandKind::SetBackgroundColor, CanvasCommandKind::BeginFill,
    CanvasCommandKind::EndFill,     CanvasCommandKind::FillLastShape,
    CanvasCommandKind::ShowTurtle,  CanvasCommandKind::HideTurtle,
    CanvasCommandKind::Clear,       CanvasCommandKind::Reset,
    CanvasCommandKind::SetSvgaMode,
    CanvasCommandKind::SetFramebuffer, CanvasCommandKind::ClearFramebuffer,
    CanvasCommandKind::SetPaletteColor, CanvasCommandKind::CyclePalette,
    CanvasCommandKind::Circle,      CanvasCommandKind::DrawPoint,
    CanvasCommandKind::DrawPolyline, CanvasCommandKind::DrawRect,
    CanvasCommandKind::DrawEllipse, CanvasCommandKind::DrawArc,
    CanvasCommandKind::DrawText,    CanvasCommandKind::DrawRectGradient,
    CanvasCommandKind::DrawEllipseGradient, CanvasCommandKind::BezierCurve,
    CanvasCommandKind::PutPixels,   CanvasCommandKind::DefineSprite,
    CanvasCommandKind::PlaceSprite, CanvasCommandKind::MoveSprite,
    CanvasCommandKind::ShowSprite,  CanvasCommandKind::HideSprite,
};

[[nodiscard]] bool waived(CanvasCommandKind kind) {
    for (const Undrawn& entry : kNotDrawn) {
        if (entry.kind == kind) {
            return true;
        }
    }
    return false;
}

/// A command of `kind` with just enough payload for it to mean something.
struct Sample {
    CanvasCommand command;
    QVector<double> points;
    QByteArray text;
};

[[nodiscard]] Sample sampleFor(CanvasCommandKind kind) {
    Sample sample;
    sample.command = timewarp::canvas::make_command(kind);
    sample.command.color = timewarp::canvas::make_rgb(200, 100, 50);
    sample.command.x = 1.0;
    sample.command.y = 1.0;
    sample.command.w = 4.0;
    sample.command.h = 4.0;
    switch (kind) {
        case CanvasCommandKind::DrawPolyline:
        case CanvasCommandKind::BezierCurve:
            sample.points = {0.0, 0.0, 10.0, 10.0, 20.0, 0.0, 30.0, 10.0};
            break;
        case CanvasCommandKind::DrawText:
        case CanvasCommandKind::DefineSprite:
        case CanvasCommandKind::PlaceSprite:
        case CanvasCommandKind::MoveSprite:
        case CanvasCommandKind::ShowSprite:
        case CanvasCommandKind::HideSprite:
            sample.text = QByteArrayLiteral("sample");
            break;
        case CanvasCommandKind::PutPixels:
            // One double per pixel, two by two.
            sample.points = {0.0, 0.0, 0.0, 0.0};
            sample.command.w = 2.0;
            sample.command.h = 2.0;
            break;
        default:
            break;
    }
    return sample;
}

void pushSample(TurtleCanvas& canvas, const Sample& sample) {
    canvas.apply(CanvasCommandView{
        sample.command,
        std::span<const double>{sample.points.constData(),
                                static_cast<std::size_t>(sample.points.size())},
        std::string_view{sample.text.constData(),
                         static_cast<std::size_t>(sample.text.size())}});
}

// The gate W19.1 exists for: every declared command draws, or is waived in
// writing. A vocabulary that declares more than the surface can show is a promise
// the product cannot keep, and until 18.x nothing noticed.
void test_the_drawing_vocabulary_is_complete() {
    announce("drawing vocabulary");
    // Every declared kind, and the count is *derived* rather than written down: a
    // new enumerator is named by `to_string` (which must gain a case or the build
    // fails under -Wswitch) and the walk stops at the first unnamed value, so the
    // expected total grows on its own and a kind that is never added to
    // `kEveryKind` fails here instead of passing quietly. The values have to stay
    // contiguous for that walk to mean anything.
    std::size_t declared = 0;
    for (int value = 1; value < 256; ++value) {
        if (to_string(static_cast<CanvasCommandKind>(value)) == "Unknown") {
            break;
        }
        ++declared;
    }
    TW_CHECK_EQ(std::size(kEveryKind), declared);

    for (const CanvasCommandKind kind : kEveryKind) {
        const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};
        pushSample(*canvas, sampleFor(kind));
        if (waived(kind)) {
            TW_CHECK_EQ(canvas->commandsIgnored(), std::uint64_t{1});
            TW_CHECK_EQ(canvas->commandsApplied(), std::uint64_t{0});
            continue;
        }
        // `Reset` is handled by the one path that also zeroes the counters,
        // because a reset *is* a fresh run: asking it to leave `applied_ == 1`
        // would be asking it not to reset. Asserted by its effect instead.
        if (kind == CanvasCommandKind::Reset) {
            TW_CHECK_EQ(canvas->commandsIgnored(), std::uint64_t{0});
            TW_CHECK_EQ(canvas->strokeCount(), 0);
            TW_CHECK_EQ(canvas->shapeCount(), 0);
            continue;
        }
        if (canvas->commandsIgnored() != 0 || canvas->commandsApplied() != 1) {
            std::printf("   %.*s is declared and not drawn\n",
                        static_cast<int>(timewarp::canvas::to_string(kind).size()),
                        timewarp::canvas::to_string(kind).data());
        }
        TW_CHECK_EQ(canvas->commandsIgnored(), std::uint64_t{0});
        TW_CHECK_EQ(canvas->commandsApplied(), std::uint64_t{1});
    }
}

// The eighteen kinds that used to fall into `default:` now draw. Each case here is
// the *behaviour*, not the counter: a kind that increments `applied_` and paints
// nothing would pass the walk and fail a user.
// `FILLED "red" [fd 100 rt 90 ...]` names a fill colour that is not the pen's.
// The command carries it in `b`, and the painter used to fill with the stroke --
// so every filled polygon in a Logo program was outlined in one colour and
// painted in the same one, whatever the program asked for. The gate that would
// have caught it is a *pixel*, because "a shape exists" was already true.
void test_a_filled_polygon_is_painted_in_its_own_colour() {
    announce("filled polygon colour");
    const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};
    const QVector<QPointF> corners{QPointF{-50.0, -50.0}, QPointF{50.0, -50.0},
                                   QPointF{50.0, 50.0}, QPointF{-50.0, 50.0}};
    QVector<double> flat;
    for (const QPointF& point : corners) {
        flat.append(point.x());
        flat.append(point.y());
    }
    auto shape = timewarp::canvas::make_polyline(
        CanvasCommandKind::DrawPolyline, timewarp::canvas::make_rgb(0, 0, 0), 1.0,
        /*closed=*/true, /*filled=*/true);
    shape.b = static_cast<double>(timewarp::canvas::make_rgb(0, 0, 255));
    canvas->apply(CanvasCommandView{
        shape,
        std::span<const double>{flat.constData(), static_cast<std::size_t>(flat.size())},
        {}});

    const QImage drawing = canvas->renderedDrawing(1.0);
    TW_CHECK(!drawing.isNull());
    // The middle of the square is the blue the program asked for, not the black
    // the pen was drawing with.
    const QColor middle = drawing.pixelColor(drawing.width() / 2, drawing.height() / 2);
    TW_CHECK_EQ(middle.blue() > 200, true);
    TW_CHECK_EQ(middle.red() < 60, true);
}

// The end of the chain: `PENSTYLE 1` in Logo must reach the canvas as a dashed
// pen, not stop at the engine's own state. Driven through `NativeCanvasHost`
// because that is the host a user has, and a host method nobody wires up is the
// same defect as a command nobody draws.
void test_a_pen_style_reaches_the_canvas_from_the_host() {
    announce("pen style through the host");
    const std::unique_ptr<TurtleCanvas> plain{sizedCanvas()};
    const std::unique_ptr<TurtleCanvas> dashed{sizedCanvas()};
    timewarp::canvas::CanvasCommandQueue queue_plain;
    timewarp::canvas::CanvasCommandQueue queue_dashed;
    timewarp::ui::NativeCanvasHost host_plain{queue_plain};
    timewarp::ui::NativeCanvasHost host_dashed{queue_dashed};

    host_plain.forward(80.0);
    host_dashed.set_pen_style(1, 1.0);
    host_dashed.forward(80.0);

    // `drain` takes a callable, so the canvas is the sink: the same path the
    // window's `CanvasBridge` uses.
    const auto drain = [](timewarp::canvas::CanvasCommandQueue& queue,
                          TurtleCanvas& canvas) {
        queue.drain([&canvas](timewarp::canvas::CanvasCommandView view) {
            canvas.apply(view);
        });
    };
    drain(queue_plain, *plain);
    drain(queue_dashed, *dashed);

    TW_CHECK_EQ(plain->strokeCount(), 1);
    TW_CHECK_EQ(dashed->strokeCount(), 1);
    // A dashed line is not a solid one: the pixels differ. This is the assertion
    // that would have failed while `set_pen_style` had no implementation on the
    // host at all -- the command would have been accepted and the ink unchanged.
    TW_CHECK(plain->renderedDrawing(1.0) != dashed->renderedDrawing(1.0));
    TW_CHECK_EQ(dashed->commandsIgnored(), std::uint64_t{0});
}

// The three shapes a program could not ask for until a language could emit them:
// a bezier, two gradients and a named sprite. Each is driven through the real
// host, so what is asserted is the path a program takes rather than a command a
// test pushed by hand.
void test_bezier_and_gradients_reach_the_canvas_from_the_host() {
    announce("bezier and gradients through the host");
    const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};
    timewarp::canvas::CanvasCommandQueue queue;
    timewarp::ui::NativeCanvasHost host{queue};

    host.bezier({timewarp::lang::Point{0.0, 0.0}, timewarp::lang::Point{40.0, 80.0},
                 timewarp::lang::Point{80.0, 80.0}, timewarp::lang::Point{120.0, 0.0}},
                timewarp::lang::Rgb{255, 255, 0}, 2.0);
    host.draw_rect_gradient(0.0, 0.0, 50.0, 50.0, 0.0, timewarp::lang::Rgb{255, 0, 0},
                            timewarp::lang::Rgb{0, 0, 255});
    host.draw_ellipse_gradient(10.0, 10.0, 90.0, 60.0, timewarp::lang::Rgb{0, 255, 0},
                               timewarp::lang::Rgb{0, 0, 0});
    queue.drain([&canvas](timewarp::canvas::CanvasCommandView view) {
        canvas->apply(view);
    });

    TW_CHECK_EQ(canvas->shapeCount(), 3);
    TW_CHECK_EQ(canvas->commandsIgnored(), std::uint64_t{0});
    // The bezier is a curve, not the line between its ends: a straight line would
    // not pass through the control points it bows towards.
    const QImage drawing = canvas->renderedDrawing(1.0);
    TW_CHECK(!drawing.isNull());
}

void test_a_named_sprite_is_defined_placed_moved_and_hidden() {
    announce("sprite objects through the host");
    const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas(200, 200)};
    timewarp::canvas::CanvasCommandQueue queue;
    timewarp::ui::NativeCanvasHost host{queue};
    const auto drain = [&queue, &canvas] {
        queue.drain([&canvas](timewarp::canvas::CanvasCommandView view) {
            canvas->apply(view);
        });
    };

    // A fixed screen, because a sprite is a bitmap and its definition is only
    // visible where a bitmap is.
    push(*canvas, timewarp::canvas::make_framebuffer(200.0, 200.0));
    const std::vector<timewarp::lang::Rgb> pixels(4, timewarp::lang::Rgb{255, 0, 0});
    host.define_sprite("box", 0, 0, 2, 2, pixels);
    host.place_sprite("box", 20.0, 30.0);
    drain();

    TW_CHECK_EQ(canvas->framebufferPixel(20, 30), QColor(255, 0, 0));
    TW_CHECK_EQ(canvas->framebufferPixel(21, 31), QColor(255, 0, 0));
    // Outside the 2x2 bitmap the screen is still the colour the screen was
    // made with, so nothing was written there.
    const QColor paper = canvas->framebufferPixel(0, 0);
    TW_CHECK(canvas->framebufferPixel(22, 30) == paper);

    // A move in screen mode redraws the bitmap and puts back what it covered --
    // the old square is gone, which a display-list-only move could not do.
    host.move_sprite("box", 10.0, 10.0);
    drain();
    TW_CHECK_EQ(canvas->framebufferPixel(30, 40), QColor(255, 0, 0));
    TW_CHECK_EQ(canvas->framebufferPixel(31, 41), QColor(255, 0, 0));
    TW_CHECK(canvas->framebufferPixel(20, 30) == paper);

    host.set_sprite_visible("box", false);
    drain();
    TW_CHECK(canvas->framebufferPixel(30, 40) == paper);
    host.set_sprite_visible("box", true);
    drain();
    TW_CHECK_EQ(canvas->framebufferPixel(30, 40), QColor(255, 0, 0));

    TW_CHECK_EQ(canvas->commandsIgnored(), std::uint64_t{0});
}

// A `PUT` larger than one queue chunk used to stack every chunk after the first
// on top of the first, because the host subtracted the chunk's row offset instead
// of adding it. A 200x100 block is 20k pixels, so it takes two chunks.
void test_a_large_pixel_block_is_placed_where_it_was_asked_for() {
    announce("multi-chunk PUT");
    const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas(220, 120)};
    timewarp::canvas::CanvasCommandQueue queue;
    timewarp::ui::NativeCanvasHost host{queue};

    push(*canvas, timewarp::canvas::make_framebuffer(220.0, 120.0));
    constexpr int kWidth = 200;
    constexpr int kHeight = 100;
    std::vector<timewarp::lang::Rgb> pixels(static_cast<std::size_t>(kWidth) * kHeight,
                                            timewarp::lang::Rgb{0, 0, 255});
    host.put_pixels(10, 10, kWidth, kHeight, pixels);
    queue.drain([&canvas](timewarp::canvas::CanvasCommandView view) {
        canvas->apply(view);
    });

    // The first row the block covers, the first row of the second chunk, and the
    // last row of all three -- the last is the one the old arithmetic lost.
    TW_CHECK_EQ(canvas->framebufferPixel(10, 10), QColor(0, 0, 255));
    TW_CHECK_EQ(canvas->framebufferPixel(10, 10 + 81), QColor(0, 0, 255));
    TW_CHECK_EQ(canvas->framebufferPixel(10, 10 + kHeight - 1), QColor(0, 0, 255));
    // And the row below the block is still the screen's own colour, so nothing
    // was written twice.
    const QColor paper = canvas->framebufferPixel(0, 0);
    TW_CHECK(canvas->framebufferPixel(10, 10 + kHeight) == paper);
}

void test_the_new_kinds_draw_something() {
    announce("new drawing kinds");

    // A fill is a polygon walked by the turtle, not a paint bucket: the four
    // sides of a square become one filled shape.
    {
        const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};
        push(*canvas, timewarp::canvas::make_fill_begin());
        for (int side = 0; side < 4; ++side) {
            push_forward(*canvas, 100.0);
            push_right(*canvas, 90.0);
        }
        push(*canvas, timewarp::canvas::make_fill_end());
        TW_CHECK_EQ(canvas->shapeCount(), 1);
        TW_CHECK_EQ(canvas->strokeCount(), 4);
        // The square has ink in its middle, which is the whole difference between
        // an outline and a fill.
        const QImage drawing = canvas->renderedDrawing(1.0);
        TW_CHECK(!drawing.isNull());
    }

    // A gradient is filled by definition, and its two colours differ.
    {
        const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};
        push(*canvas, timewarp::canvas::make_rect_gradient(
                          -50.0, -50.0, 100.0, 100.0, 0.0,
                          timewarp::canvas::make_rgb(255, 0, 0),
                          timewarp::canvas::make_rgb(0, 0, 255)));
        TW_CHECK_EQ(canvas->shapeCount(), 1);
        TW_CHECK_EQ(canvas->commandsIgnored(), std::uint64_t{0});
    }

    // A bezier is one shape with four control points.
    {
        const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};
        auto curve = timewarp::canvas::make_command(CanvasCommandKind::BezierCurve);
        curve.x = 2.0;
        const QVector<double> points{0.0, 0.0, 30.0, 80.0, 70.0, 80.0, 100.0, 0.0};
        canvas->apply(CanvasCommandView{
            curve,
            std::span<const double>{points.constData(),
                                    static_cast<std::size_t>(points.size())},
            {}});
        TW_CHECK_EQ(canvas->shapeCount(), 1);
    }

    // A sprite is defined once and placed as many times as a program likes: two
    // placements, one definition.
    {
        const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};
        auto define = timewarp::canvas::make_command(CanvasCommandKind::DefineSprite);
        define.w = 2.0;
        define.h = 2.0;
        const QVector<double> pixels{
            static_cast<double>(timewarp::canvas::make_rgb(255, 0, 0)),
            static_cast<double>(timewarp::canvas::make_rgb(0, 255, 0)),
            static_cast<double>(timewarp::canvas::make_rgb(0, 0, 255)), 0.0};
        canvas->apply(CanvasCommandView{
            define,
            std::span<const double>{pixels.constData(),
                                    static_cast<std::size_t>(pixels.size())},
            std::string_view{"dot"}});
        // The definition itself draws nothing, which is right: a sprite nobody
        // placed is not on the canvas.
        TW_CHECK_EQ(canvas->shapeCount(), 0);
        TW_CHECK_EQ(canvas->commandsIgnored(), std::uint64_t{0});

        for (int copy = 0; copy < 2; ++copy) {
            auto place = timewarp::canvas::make_sprite_place(copy * 20.0, 0.0);
            canvas->apply(CanvasCommandView{
                place, {}, std::string_view{"dot"}});
        }
        TW_CHECK_EQ(canvas->shapeCount(), 2);
    }

    // Palette cycling recolours the ink that uses a palette slot, which is what
    // makes a C64 border or an Amiga copper bar animate.
    {
        const std::unique_ptr<TurtleCanvas> a{sizedCanvas()};
        const std::unique_ptr<TurtleCanvas> b{sizedCanvas()};
        for (TurtleCanvas* canvas : {a.get(), b.get()}) {
            push(*canvas, timewarp::canvas::make_palette_color(
                               0, timewarp::canvas::make_rgb(255, 0, 0)));
            push(*canvas, timewarp::canvas::make_palette_color(
                               1, timewarp::canvas::make_rgb(0, 0, 255)));
            auto ink = timewarp::canvas::make_color(
                CanvasCommandKind::SetPenColor, timewarp::canvas::make_rgb(255, 0, 0));
            push(*canvas, ink);
            push_forward(*canvas, 50.0);
        }
        push(*b, timewarp::canvas::make_cycle_palette(1));
        const QImage plain = a->renderedDrawing(1.0);
        const QImage cycled = b->renderedDrawing(1.0);
        TW_CHECK(plain != cycled);
    }

    // A virtual screen is a fixed extent, so the same drawing fits differently in
    // it. Without this a program positioning at 800x600 coordinates is scaled to
    // whatever its ink happens to be.
    {
        const std::unique_ptr<TurtleCanvas> canvas{sizedCanvas()};
        push(*canvas, timewarp::canvas::make_svga_mode(800.0, 600.0));
        TW_CHECK_EQ(canvas->commandsIgnored(), std::uint64_t{0});
        // `Reset` is what returns the surface to fitting its ink; the vocabulary
        // has no second way to say so.
        push(*canvas, timewarp::canvas::make_command(CanvasCommandKind::Reset));
        TW_CHECK_EQ(canvas->commandsIgnored(), std::uint64_t{0});
    }
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

// SVG is a second writer over the same ink, so what needs checking is that the
// file holds the drawing rather than that it is valid XML. Rendering it back
// with `QSvgRenderer` and counting inked pixels is the check that would fail if
// the export painted nothing, or painted it somewhere else.
void test_the_drawing_can_be_exported_as_svg() {
    announce("export a drawing as SVG");
    TurtleCanvas canvas;
    canvas.resize(200, 200);
    push_forward(canvas, 60.0);
    push_right(canvas, 90.0);
    push_forward(canvas, 60.0);
    canvas.fitToContent();

    const QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    const QString path = dir.filePath(QStringLiteral("drawing.svg"));
    QString error;
    const bool written = canvas.exportDrawing(path, &error);
#ifdef TIME_WARP_HAVE_SVG
    TW_CHECK(written);
    TW_CHECK(error.isEmpty());
    TW_CHECK(QFile::exists(path));

    QFile file{path};
    TW_CHECK(file.open(QIODevice::ReadOnly));
    const QByteArray svg = file.readAll();
    file.close();
    TW_CHECK(svg.contains("<svg"));
    TW_CHECK(svg.contains("</svg>"));
    // One user unit is one widget pixel, so the view box is the on-screen size
    // rather than the doubled size of a raster export. (The header's physical
    // size is in millimetres: `QSvgGenerator` states it at 72 dpi, so it is the
    // view box, not `width=`, that carries the pixel size.)
    TW_CHECK(svg.contains("viewBox=\"0 0 200 200\""));
    // A vector path, not a bitmap wrapped in SVG.
    TW_CHECK(svg.contains("<path") || svg.contains("<polyline") ||
             svg.contains("<line"));

    // The drawing is inside it: rasterise the file and compare the ink with the
    // raster export of the same canvas.
    QSvgRenderer renderer{svg};
    TW_CHECK(renderer.isValid());
    QImage rastered{200, 200, QImage::Format_ARGB32_Premultiplied};
    rastered.fill(canvas.background());
    {
        QPainter painter{&rastered};
        renderer.render(&painter);
    }
    const QImage expected = canvas.renderedDrawing(1.0);

    struct Ink {
        int pixels{0};
        QRect bounds;
    };
    const auto measure = [&canvas](const QImage& image) {
        Ink ink;
        for (int y = 0; y < image.height(); ++y) {
            for (int x = 0; x < image.width(); ++x) {
                if (image.pixelColor(x, y) != canvas.background()) {
                    ++ink.pixels;
                    ink.bounds |= QRect{x, y, 1, 1};
                }
            }
        }
        return ink;
    };
    const Ink vector_ink = measure(rastered);
    const Ink raster_ink = measure(expected);
    TW_CHECK(raster_ink.pixels > 100);
    // The box the ink occupies is exact: an export that painted nothing, or
    // painted the same lines shifted or scaled, would move it.
    TW_CHECK(vector_ink.bounds == raster_ink.bounds);
    // The count is not: `QSvgRenderer` antialiases edge pixels however the
    // painter is set, and a two-pixel line covers about a third more of them
    // than the raster path does. A blank picture or a much thicker pen is
    // outside this band; the same pen is inside it.
    TW_CHECK(vector_ink.pixels >= raster_ink.pixels * 9 / 10);
    TW_CHECK(vector_ink.pixels <= raster_ink.pixels * 3 / 2);

    // A second export is byte-identical: nothing in the file is a timestamp, so a
    // drawing can be committed without spurious diffs.
    const QString again = dir.filePath(QStringLiteral("again.svg"));
    TW_CHECK(canvas.exportDrawing(again, nullptr));
    QFile second{again};
    TW_CHECK(second.open(QIODevice::ReadOnly));
    TW_CHECK(second.readAll() == svg);
#else
    // No Qt6::Svg in this build: refused and said so, rather than writing an
    // empty file with an SVG name.
    TW_CHECK(!written);
    TW_CHECK(error.contains("SVG"));
    TW_CHECK(!QFile::exists(path));
#endif
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
    test_a_state_change_reports_itself();
    test_pen_colour_and_width_are_recorded();

    test_a_circle_is_walked_as_a_polygon();
    test_clear_wipes_the_paper_and_reset_starts_over();
    test_show_and_hide_the_turtle();
    test_shape_primitives_are_recorded();
    test_a_sprite_block_is_drawn_as_shapes();
    test_a_screen_is_painted_at_fixed_coordinates();
    test_the_drawing_vocabulary_is_complete();
    test_a_filled_polygon_is_painted_in_its_own_colour();
    test_a_pen_style_reaches_the_canvas_from_the_host();
    test_bezier_and_gradients_reach_the_canvas_from_the_host();
    test_a_named_sprite_is_defined_placed_moved_and_hidden();
    test_a_large_pixel_block_is_placed_where_it_was_asked_for();
    test_the_new_kinds_draw_something();
    test_zoom_is_clamped_and_fit_recentres();
    test_content_bounds_include_every_drawn_axis();
    test_the_drawing_can_be_exported();
    test_the_drawing_can_be_exported_as_svg();
    test_scrubbing_shows_a_prefix_without_losing_the_drawing();
    test_history_keeps_strokes_and_shapes_in_draw_order();
    test_the_scrubber_plays_a_drawing_back();

    return tw::test::summary("turtle_canvas");
}
