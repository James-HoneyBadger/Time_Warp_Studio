// SPDX-License-Identifier: MIT
//
// The graphics surface: what a turtle program actually draws on.
//
// The interpreter never touches this widget. It pushes `CanvasCommand`s into a
// `CanvasCommandQueue` (one lock-free push per turtle call) and a `CanvasBridge`
// drains that queue on the GUI thread and hands each command to `apply()` here.
// So this class is a *consumer*: it keeps the mirror of the turtle state, the
// list of everything drawn, and the painter.
//
// #### Why a display list rather than a pixmap
//
// Repainting on resize, on zoom and on pan needs the drawing again, so the ink
// is kept as geometry in world coordinates rather than as pixels. A pixmap would
// go blurry when zoomed and cannot be re-projected at all.
//
// #### Why the turtle state is mirrored here
//
// `QueuedTurtleHost` translates turtle calls one-to-one and statelessly: a
// `Forward` command carries only a distance, not the heading to move along or
// the pen colour to draw with. Something has to replay the sequence and hold the
// state, and the comment on that adapter is explicit that the GUI is where it
// belongs -- the alternative would be copying the state into every command.

#pragma once

#include <cstdint>

#include <QColor>
#include <QImage>
#include <QLineF>
#include <QPointF>
#include <QRectF>
#include <QString>
#include <QVector>
#include <QWidget>

#include "timewarp/canvas/command.hpp"

class QPainter;
class QPaintEvent;
class QResizeEvent;
class QWheelEvent;
class QMouseEvent;

namespace timewarp::ui {

/// The turtle graphics canvas.
///
/// #### Coordinates
///
/// The convention is the reference implementation's, transcribed from
/// `graphics/turtle_state.py` rather than chosen, because a canvas that draws a
/// mirrored or quarter-turned version of the same program is worse than no
/// canvas at all:
///
///   * The origin is where the turtle starts; world +y is up.
///   * `heading` is in degrees, **0 is north (+y)** and it increases
///     **clockwise**, so 90 is east (+x). This is a compass, not the
///     mathematical convention: `right(a)` adds to the heading and `left(a)`
///     subtracts from it.
///   * `forward(d)` moves by `(d*sin(h), d*cos(h))` -- north at 0, east at 90.
///
/// Screen space has +y *downwards*, so the projection flips it. That flip lives
/// in `toScreen`/`toWorld` and nowhere else.
class TurtleCanvas final : public QWidget {
    Q_OBJECT

public:
    explicit TurtleCanvas(QWidget* parent = nullptr);

    /// Applies one command from the queue. This is the bridge's sink.
    void apply(const timewarp::canvas::CanvasCommandView& view);

    /// Moves the turtle marker without drawing anything.
    ///
    /// The Python bridge needs this and the command stream cannot express it. The
    /// bridge's commands carry *absolute geometry* -- a segment to draw -- because
    /// diffing the reference's line list is the only way to capture everything it
    /// draws without overriding every turtle method. Replaying `Forward` on top of
    /// that geometry would draw every line a second time; leaving the marker at the
    /// origin would leave it nowhere near the drawing. So the pose arrives
    /// separately, and the marker moves without leaving a trail.
    void setTurtlePose(const QPointF& position, double heading, bool visible);

    /// Forgets the drawing and puts the turtle back where it started with the
    /// default pen. What a fresh run needs.
    void reset();

    // -- view -------------------------------------------------------------

    /// Scales and centres the view so everything drawn is comfortably visible.
    /// Called when a run ends rather than on every command: a view that refits
    /// as the drawing grows shrinks continuously while you watch it.
    void fitToContent();

    /// Zoom multiplier over the fitted scale. Clamped to the range in the .cpp.
    void setZoom(double zoom);
    [[nodiscard]] double zoom() const noexcept { return zoom_; }

    void setGridVisible(bool visible);
    [[nodiscard]] bool gridVisible() const noexcept { return grid_visible_; }

    /// The colour a *run* starts on when the program does not name one: the
    /// theme's canvas surface. `reset()` returns to this rather than to white, so
    /// applying a theme and starting a run does not flash a white canvas -- and so
    /// there is one answer to "what colour is the paper" rather than two.
    ///
    /// A program that sets a background colour still wins: this is the default,
    /// not an override.
    void setSurfaceColor(const QColor& colour);
    [[nodiscard]] QColor surfaceColor() const noexcept { return surface_color_; }

    // -- coordinate conversion --------------------------------------------
    //
    // Public because they are the only way to ask "where is this on screen" and
    // "what did the user just click on", which the status bar, a future turtle
    // inspector and a click-to-read-coordinate feature all need. Keeping them
    // private would mean reimplementing the y flip at every call site.

    [[nodiscard]] QPointF toScreen(const QPointF& world) const;
    [[nodiscard]] QPointF toWorld(const QPointF& screen) const;

    // -- what was drawn ---------------------------------------------------
    //
    // Read by the tests, which is the only way to check a painter without
    // comparing pixels: a screenshot can only say "something is here".

    /// Straight pen strokes, which is what every turtle motion produces.
    [[nodiscard]] int strokeCount() const noexcept {
        return static_cast<int>(strokes_.size());
    }
    /// Shape primitives (points, rectangles, ellipses, arcs, text, circles).
    [[nodiscard]] int shapeCount() const noexcept { return static_cast<int>(shapes_.size()); }

    /// The colour of the `index`th stroke, or an invalid colour when there is no
    /// such stroke. What a theme's default ink is checked with: a light theme
    /// that drew white on white has the right number of strokes and nothing to
    /// look at.
    [[nodiscard]] QColor strokeColor(int index) const {
        if (index < 0 || index >= static_cast<int>(strokes_.size())) {
            return QColor{};
        }
        return strokes_.at(index).color;
    }

    // -- scrubbing --------------------------------------------------------
    //
    // The ink in draw order, not the two lists separately: a rectangle painted
    // between two lines has to come back between them. A frame is one primitive,
    // which is the unit the old scrubber counted and what a drawing of 100
    // segments makes mean "show the first 40".

    struct HistoryEntry {
        enum class Kind { Stroke, Shape };
        Kind kind = Kind::Stroke;
        int index = 0;  ///< Index into `strokes_` or `shapes_`.
    };

    [[nodiscard]] const QVector<HistoryEntry>& history() const noexcept {
        return history_;
    }
    /// Total primitives, which is the scrubber's maximum.
    [[nodiscard]] int frameCount() const noexcept { return static_cast<int>(history_.size()); }
    /// How many of them the view is showing. Equal to `frameCount` until a
    /// scrub moves it back.
    [[nodiscard]] int visibleFrameCount() const noexcept { return visible_frames_; }
    /// Shows the first *frame* primitives. A view only: export, fit and the ink
    /// counts still use the whole list.
    void setVisibleFrame(int frame);
    [[nodiscard]] int visibleStrokeCount() const noexcept;
    [[nodiscard]] int visibleShapeCount() const noexcept;
    /// Total world-space length of every stroke. A drawing that "appeared" but
    /// came out the wrong size shows up here and nowhere in a pixel comparison.
    [[nodiscard]] double totalStrokeLength() const;
    /// World-space bounding box of all the ink, empty when nothing was drawn.
    [[nodiscard]] QRectF contentBounds() const noexcept { return content_bounds_; }

    // -- turtle state (the mirror) ----------------------------------------
    [[nodiscard]] QPointF turtlePosition() const noexcept { return position_; }
    /// Degrees, counter-clockwise from the initial facing.
    [[nodiscard]] double turtleHeading() const noexcept { return heading_; }
    [[nodiscard]] bool penIsDown() const noexcept { return pen_down_; }
    [[nodiscard]] bool turtleIsVisible() const noexcept { return turtle_visible_; }
    [[nodiscard]] QColor penColor() const noexcept { return pen_color_; }
    [[nodiscard]] double penWidth() const noexcept { return pen_width_; }
    /// The surface colour. Public because it is a fact about what is on screen:
    /// the reference's pen is white, so a client that lost the background -- across
    /// a process boundary, say -- would draw white ink on its own white canvas and
    /// appear to draw nothing at all.
    [[nodiscard]] QColor background() const noexcept { return background_; }

    // -- export -----------------------------------------------------------
    //
    // The drawing by itself: no grid, no turtle marker, because those are chrome
    // rather than ink. Both of these go through the same `paintInk` the window
    // paints with, so an exported picture cannot drift from what is on screen.

    /// The drawing at `factor` times the on-screen size, on the canvas
    /// background. Two by default: a picture worth saving is worth the pixels.
    [[nodiscard]] QImage renderedDrawing(double factor = 2.0) const;

    /// Writes `renderedDrawing(factor)` to `path`, the format taken from the
    /// extension. Returns false and fills `error` when the format is unknown or
    /// the file cannot be written.
    ///
    /// `.svg` writes vectors instead of pixels through `QSvgGenerator`, so
    /// `factor` does not apply to it. That needs Qt6::Svg, which is optional: a
    /// build without it still exports the raster formats and fails `.svg` with
    /// an error that says so.
    bool exportDrawing(const QString& path, QString* error = nullptr,
                       double factor = 2.0) const;

    // -- diagnostics ------------------------------------------------------

    /// Commands applied since the last `reset()`.
    [[nodiscard]] std::uint64_t commandsApplied() const noexcept { return applied_; }
    /// Commands this build does not render. Non-zero means the canvas is
    /// dropping something the interpreters can emit, which is worth knowing
    /// rather than discovering as a missing shape.
    [[nodiscard]] std::uint64_t commandsIgnored() const noexcept { return ignored_; }

protected:
    void paintEvent(QPaintEvent* event) override;
    void resizeEvent(QResizeEvent* event) override;
    void wheelEvent(QWheelEvent* event) override;
    void mousePressEvent(QMouseEvent* event) override;
    void mouseMoveEvent(QMouseEvent* event) override;
    void mouseReleaseEvent(QMouseEvent* event) override;
    /// Forwards printable keys to `keyTyped`, so a program watching `INKEY$` can
    /// read them.
    void keyPressEvent(QKeyEvent* event) override;

signals:
    /// The turtle moved, turned, or changed pen. The zoom/status readouts and,
    /// later, the turtle inspector connect to this.
    void turtleStateChanged();
    /// Something was drawn. Emitted once per applied command rather than per
    /// paint, so a listener can throttle its own repaints.
    void contentChanged();
    /// A key was pressed while the canvas had focus. `INKEY$` is fed from this:
    /// a game polls the canvas, so the canvas is where the keys are typed.
    void keyTyped(QChar key);

private:
    /// One straight pen stroke, in world coordinates.
    struct Stroke {
        QLineF line;
        QColor color;
        double width = 1.0;
    };

    /// A shape primitive. One struct for all of them: they differ in which
    /// fields they use, not in how they are stored or painted, and a variant
    /// would cost more in ceremony than it saves in memory.
    struct Shape {
        timewarp::canvas::CanvasCommandKind kind =
            timewarp::canvas::CanvasCommandKind::DrawPoint;
        QRectF rect;          ///< bounds, or the arc's ellipse
        QPointF at;           ///< the point, or the text origin
        QPointF centre;       ///< arcs: the ellipse centre
        double a = 0.0;       ///< arcs: start angle (degrees)
        double b = 0.0;       ///< arcs: extent (degrees)
        QColor color;
        double width = 1.0;
        bool filled = false;
        QString text;
    };

    // -- command handlers -------------------------------------------------
    void moveAlong(double distance, bool forwards);
    void turn(double degrees, bool to_the_right);
    void lineTo(const QPointF& target);
    /// `circle` by the reference's own definition: a polygon walk of
    /// `max(12, |extent|/10)` chords, not an arc. Matching the decomposition
    /// matters because a smooth arc and a 12-sided polygon look different, and
    /// the Python build draws the polygon.
    void drawCircle(double radius, double extent);
    void appendStroke(const QPointF& from, const QPointF& to);
    void appendStyledStroke(const QPointF& from, const QPointF& to,
                            const QColor& color, double width);
    /// Appends a shape and its history entry, so no draw path can add one
    /// without the other.
    void appendShape(const Shape& shape);

    /// Unit vector the turtle is facing, in world coordinates.
    [[nodiscard]] QPointF headingVector() const;

    // -- projection -------------------------------------------------------
    [[nodiscard]] double scale() const noexcept;
    [[nodiscard]] double fittedScale() const;

    void recomputeFit();
    void growBounds(const QRectF& box);

    // -- painting ---------------------------------------------------------
    void paintGrid(QPainter& painter);
    /// Paints the first *frames* history entries. `paintEvent` passes the
    /// visible count; export passes the whole list, which is what makes
    /// scrubbing a view rather than a state change.
    void paintInk(QPainter& painter, int frames) const;
    void paintTurtle(QPainter& painter);

    // -- state ------------------------------------------------------------
    QPointF position_{0.0, 0.0};
    double heading_ = 0.0;
    bool pen_down_ = true;
    bool turtle_visible_ = true;
    QColor pen_color_{0, 0, 0};
    double pen_width_ = 1.0;
    QColor background_{255, 255, 255};
    /// What `reset()` puts in `background_`. Set from the theme; white by
    /// default, which is what the canvas shipped with.
    QColor surface_color_{255, 255, 255};

    QVector<Stroke> strokes_;
    QVector<Shape> shapes_;
    QVector<HistoryEntry> history_;
    int visible_frames_ = 0;
    QRectF content_bounds_;
    /// Whether `content_bounds_` means anything. A separate flag rather than
    /// `content_bounds_.isNull()`, because a single point and a horizontal line
    /// both produce a zero-height rectangle: testing the rectangle would throw
    /// away exactly the strokes a fit needs to know about.
    bool has_bounds_ = false;

    /// The world rectangle the fitted view shows, derived from the content. Kept
    /// so `fittedScale()` is a lookup rather than a recalculation during paint.
    QRectF fitted_world_{QPointF{-250.0, -250.0}, QPointF{250.0, 250.0}};

    double zoom_ = 1.0;
    QPointF pan_pixels_{0.0, 0.0};
    bool grid_visible_ = true;
    bool dragging_ = false;
    QPointF drag_from_;

    std::uint64_t applied_ = 0;
    std::uint64_t ignored_ = 0;
};

}  // namespace timewarp::ui
