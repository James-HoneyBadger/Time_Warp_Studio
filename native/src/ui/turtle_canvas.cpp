// SPDX-License-Identifier: MIT

#include "timewarp/ui/turtle_canvas.hpp"

#include <algorithm>
#include <cmath>
#include <iterator>
#include <numbers>

#include <QFont>
#include <QKeyEvent>
#include <QMouseEvent>
#include <QImage>
#include <QImageWriter>
#include <QPainter>
#include <QPainterPath>
#include <QPaintEvent>
#include <QResizeEvent>
#include <QWheelEvent>

#include "timewarp/canvas/command.hpp"

namespace timewarp::ui {

using timewarp::canvas::CanvasCommandKind;

namespace {

/// The world window shown when nothing has been drawn yet. Chosen to match the
/// Python canvas's default extent closely enough that a program's first
/// `forward 100` looks the same size in both.
constexpr double kDefaultWorldHalfExtent = 250.0;

/// Breathing room left around the ink when fitting, as a fraction of its size.
constexpr double kFitMargin = 0.08;

/// Smallest world window the fit will produce, so a program that draws a single
/// point does not get magnified to a meaningless scale.
constexpr double kMinimumFitExtent = 10.0;

constexpr double kMinimumZoom = 0.05;
constexpr double kMaximumZoom = 50.0;

/// Target on-screen spacing between grid lines. Below this the grid stops being
/// a reference and becomes noise, so the step moves up a decade instead.
constexpr double kMinimumGridSpacingPixels = 14.0;

/// Grid steps a person reads without counting: 1, 2, 5, 10, 20, 50, ...
constexpr double kGridSteps[] = {0.1,  0.2,  0.5,  1.0,   2.0,   5.0,
                                 10.0, 20.0, 50.0, 100.0, 200.0, 500.0,
                                 1000.0, 2000.0, 5000.0, 10000.0};

/// The turtle marker's size in *screen* pixels. Deliberately not world units: a
/// turtle that grows when you zoom out stops being a position cue.
constexpr double kTurtlePixels = 9.0;

[[nodiscard]] double degreesToRadians(double degrees) noexcept {
    return degrees * std::numbers::pi / 180.0;
}

/// Brings an angle into `[0, 360)`, the way Python's `%` does.
///
/// `std::fmod` alone is not enough: it keeps the sign of its argument, so
/// `right(720)` and `left(360)` would leave the heading negative here while the
/// reference leaves it positive, and an identical program would then disagree
/// about the heading from its first left turn onwards.
[[nodiscard]] double normaliseHeading(double degrees) noexcept {
    double result = std::fmod(degrees, 360.0);
    if (result < 0.0) {
        result += 360.0;
    }
    return result;
}

}  // namespace

TurtleCanvas::TurtleCanvas(QWidget* parent) : QWidget(parent) {
    // The name the layout, the stylesheet and the tests all refer to. It is
    // deliberately the same string the placeholder used, so the tests that
    // assert *where* the canvas sits kept working when the contents became real.
    setObjectName(QStringLiteral("turtleCanvas"));
    setAccessibleName(tr("Turtle Graphics Canvas"));
    setAccessibleDescription(
        tr("Draws the output of turtle graphics programs. Scroll to zoom, drag to pan."));
    setMinimumSize(160, 120);
    setFocusPolicy(Qt::ClickFocus);
    // The widget paints every pixel of itself, so Qt must not pre-fill it from
    // the palette. What that looks like when it does is the window background
    // flashing through on every resize.
    setAttribute(Qt::WA_OpaquePaintEvent, true);
    recomputeFit();
}

// ---------------------------------------------------------------------------
// Commands
// ---------------------------------------------------------------------------

void TurtleCanvas::apply(const timewarp::canvas::CanvasCommandView& view) {
    const timewarp::canvas::CanvasCommand& command = view.command;
    const QPointF before = position_;
    const double before_heading = heading_;

    switch (command.kind) {
        case CanvasCommandKind::Forward:
            moveAlong(command.x, true);
            break;
        case CanvasCommandKind::Back:
            moveAlong(command.x, false);
            break;
        case CanvasCommandKind::Right:
            turn(command.x, true);
            break;
        case CanvasCommandKind::Left:
            turn(command.x, false);
            break;
        case CanvasCommandKind::SetHeading:
            heading_ = normaliseHeading(command.x);
            break;
        case CanvasCommandKind::Goto:
            lineTo(QPointF{command.x, command.y});
            break;
        case CanvasCommandKind::SetX:
            lineTo(QPointF{command.x, position_.y()});
            break;
        case CanvasCommandKind::SetY:
            lineTo(QPointF{position_.x(), command.y});
            break;
        case CanvasCommandKind::Home:
            // `home()` is a goto *and* a setheading in the reference: back to
            // where it started, facing the way it started. That combination is
            // what makes it useful as "start again" inside a loop.
            lineTo(QPointF{0.0, 0.0});
            heading_ = 0.0;
            break;
        case CanvasCommandKind::PenUp:
            pen_down_ = false;
            break;
        case CanvasCommandKind::PenDown:
            pen_down_ = true;
            break;
        case CanvasCommandKind::SetPenColor:
            pen_color_ = QColor{command.red(), command.green(), command.blue()};
            break;
        case CanvasCommandKind::SetPenWidth:
            // A zero or negative width is not a stroke. Refused here rather than
            // producing an invisible line the user cannot explain.
            if (command.x > 0.0) {
                pen_width_ = command.x;
            }
            break;
        case CanvasCommandKind::SetBackgroundColor:
            background_ = QColor{command.red(), command.green(), command.blue()};
            break;
        case CanvasCommandKind::ShowTurtle:
            turtle_visible_ = true;
            break;
        case CanvasCommandKind::HideTurtle:
            turtle_visible_ = false;
            break;
        case CanvasCommandKind::Circle:
            drawCircle(command.x, command.y);
            break;
        case CanvasCommandKind::Clear:
            // Everything drawn, but not where the turtle is or what it holds:
            // `clear` means "wipe the paper", not "start the program over".
            strokes_.clear();
            shapes_.clear();
            history_.clear();
            visible_frames_ = 0;
            content_bounds_ = QRectF{};
            has_bounds_ = false;
            recomputeFit();
            break;
        case CanvasCommandKind::Reset:
            reset();
            return;

        // -- generic drawing primitives -----------------------------------
        //
        // Nothing the current interpreters emit uses these yet -- they arrive
        // with the BASIC and Logo ports -- but they cost little to draw, and
        // leaving them unhandled would mean a ported language silently losing
        // its shapes.

        case CanvasCommandKind::DrawPoint: {
            Shape shape;
            shape.kind = command.kind;
            shape.at = QPointF{command.x, command.y};
            shape.color = QColor{command.red(), command.green(), command.blue()};
            shape.width = command.w > 0.0 ? command.w : pen_width_;
            appendShape(shape);
            growBounds(QRectF{shape.at, shape.at});
            break;
        }
        case CanvasCommandKind::DrawPolyline: {
            // The reference stores a polyline as one shape with a point list and
            // paints it as a single stroked path. Expanded into segments here,
            // which is what a stroked path is -- but the pen is drawn at every
            // junction, so at large widths the two are not pixel-identical.
            const std::size_t points = view.point_count();
            const QColor color{command.red(), command.green(), command.blue()};
            const double width = command.x > 0.0 ? command.x : pen_width_;
            for (std::size_t index = 0; index + 1 < points; ++index) {
                appendStyledStroke(
                    QPointF{view.points[index * 2], view.points[index * 2 + 1]},
                    QPointF{view.points[index * 2 + 2], view.points[index * 2 + 3]},
                    color, width);
            }
            if (command.is_closed() && points >= 3) {
                appendStyledStroke(
                    QPointF{view.points[(points - 1) * 2],
                            view.points[(points - 1) * 2 + 1]},
                    QPointF{view.points[0], view.points[1]}, color, width);
            }
            break;
        }
        case CanvasCommandKind::DrawRect:
        case CanvasCommandKind::DrawEllipse: {
            Shape shape;
            shape.kind = command.kind;
            shape.rect = QRectF{command.x, command.y, command.w, command.h}.normalized();
            shape.centre = shape.rect.center();
            shape.color = QColor{command.red(), command.green(), command.blue()};
            shape.width = pen_width_;
            shape.filled = command.is_filled();
            appendShape(shape);
            growBounds(shape.rect);
            break;
        }
        case CanvasCommandKind::DrawArc: {
            Shape shape;
            shape.kind = command.kind;
            shape.rect = QRectF{command.x, command.y, command.w, command.h}.normalized();
            shape.centre = shape.rect.center();
            shape.a = command.a;
            shape.b = command.b;
            shape.color = QColor{command.red(), command.green(), command.blue()};
            shape.width = pen_width_;
            appendShape(shape);
            growBounds(shape.rect);
            break;
        }
        case CanvasCommandKind::DrawText: {
            Shape shape;
            shape.kind = command.kind;
            shape.at = QPointF{command.x, command.y};
            shape.width = command.w > 0.0 ? command.w : 12.0;
            shape.color = QColor{command.red(), command.green(), command.blue()};
            shape.text = QString::fromUtf8(view.text.data(),
                                           static_cast<qsizetype>(view.text.size()));
            appendShape(shape);
            // A text box is not known until it is laid out, so the bounds take a
            // nominal extent. Over-tight bounds only change how much margin the
            // fit leaves, never whether the ink is drawn.
            growBounds(QRectF{shape.at, QSizeF{shape.width * 8.0, shape.width}});
            break;
        }

        default:
            // Counted, not silently dropped. Gradients, sprites, palette cycling,
            // pen styles and the 3-D pitch/roll commands need machinery this
            // widget does not have; `commandsIgnored()` reports the gap instead
            // of leaving it to be discovered as a shape that never appears.
            ++ignored_;
            return;
    }

    ++applied_;
    if (position_ != before || heading_ != before_heading) {
        emit turtleStateChanged();
    }
    emit contentChanged();
    update();
}

void TurtleCanvas::setTurtlePose(const QPointF& position, double heading, bool visible) {
    heading_ = normaliseHeading(heading);
    turtle_visible_ = visible;
    if (position_ != position) {
        position_ = position;
        emit turtleStateChanged();
    }
    growBounds(QRectF{position, position});
    update();
}

QPointF TurtleCanvas::headingVector() const {
    // North at 0 and clockwise, so x takes the sine: this is the reference's
    // `(sin(h), cos(h))`, not the mathematical `(cos(h), sin(h))`.
    const double radians = degreesToRadians(heading_);
    return QPointF{std::sin(radians), std::cos(radians)};
}

void TurtleCanvas::moveAlong(double distance, bool forwards) {
    const double sign = forwards ? 1.0 : -1.0;
    lineTo(position_ + headingVector() * distance * sign);
}

void TurtleCanvas::turn(double degrees, bool to_the_right) {
    // Clockwise is positive here, so a right turn *adds*. The reference's
    // `right` does the same, and getting it backwards produces a mirror-image
    // drawing -- the kind of wrong that looks entirely plausible.
    heading_ = normaliseHeading(heading_ + (to_the_right ? degrees : -degrees));
}

void TurtleCanvas::lineTo(const QPointF& target) {
    if (pen_down_) {
        appendStroke(position_, target);
    }
    position_ = target;
    growBounds(QRectF{position_, position_});
}

void TurtleCanvas::drawCircle(double radius, double extent) {
    // The reference draws a circle by walking a polygon: `max(12, |extent|/10)`
    // chords of `forward(r * radians(step))` each followed by `right(step)`. So
    // does this, rather than asking the painter for an arc, because a smooth arc
    // and a 12-sided polygon are visibly different and the Python build draws
    // the polygon. Matching the decomposition also means the two agree on how
    // many strokes a circle costs.
    if (extent == 0.0) {
        return;
    }
    const QPointF start = position_;
    const int segments = std::max(12, static_cast<int>(std::abs(extent) / 10.0));
    const double angle_step = extent / static_cast<double>(segments);
    const double chord = radius * degreesToRadians(angle_step);

    for (int step = 0; step < segments; ++step) {
        moveAlong(chord, true);
        turn(angle_step, true);
    }
    if (extent >= 360.0) {
        // Closes the path. A full turn already ends where it started, so in
        // exact arithmetic this is a zero-length join -- but not after a polygon
        // walk in floating point, and the reference emits it unconditionally.
        lineTo(start);
    }
}

void TurtleCanvas::appendStroke(const QPointF& from, const QPointF& to) {
    appendStyledStroke(from, to, pen_color_, pen_width_);
}

void TurtleCanvas::appendStyledStroke(const QPointF& from, const QPointF& to,
                                      const QColor& color, double width) {
    if (from == to) {
        // A zero-length stroke is not worth storing, and a program that draws a
        // million of them in a loop would otherwise fill the list with nothing.
        return;
    }
    strokes_.append(Stroke{QLineF{from, to}, color, width});
    history_.append(HistoryEntry{HistoryEntry::Kind::Stroke,
                                 static_cast<int>(strokes_.size()) - 1});
    // New ink follows the live view: a program streaming while the user has
    // scrubbed back should not leave them looking at the past.
    visible_frames_ = static_cast<int>(history_.size());
    growBounds(QRectF{from, to}.normalized());
}

void TurtleCanvas::appendShape(const Shape& shape) {
    shapes_.append(shape);
    history_.append(HistoryEntry{HistoryEntry::Kind::Shape,
                                 static_cast<int>(shapes_.size()) - 1});
    visible_frames_ = static_cast<int>(history_.size());
}

void TurtleCanvas::setVisibleFrame(int frame) {
    const int clamped = std::clamp(frame, 0, frameCount());
    if (clamped == visible_frames_) {
        return;
    }
    visible_frames_ = clamped;
    update();
}

int TurtleCanvas::visibleStrokeCount() const noexcept {
    int count = 0;
    for (int index = 0; index < visible_frames_ && index < history_.size(); ++index) {
        if (history_[index].kind == HistoryEntry::Kind::Stroke) {
            ++count;
        }
    }
    return count;
}

int TurtleCanvas::visibleShapeCount() const noexcept {
    int count = 0;
    for (int index = 0; index < visible_frames_ && index < history_.size(); ++index) {
        if (history_[index].kind == HistoryEntry::Kind::Shape) {
            ++count;
        }
    }
    return count;
}

void TurtleCanvas::reset() {
    strokes_.clear();
    shapes_.clear();
    history_.clear();
    visible_frames_ = 0;
    content_bounds_ = QRectF{};
    has_bounds_ = false;
    position_ = QPointF{0.0, 0.0};
    heading_ = 0.0;
    pen_down_ = true;
    turtle_visible_ = true;
    pen_color_ = QColor{0, 0, 0};
    pen_width_ = 1.0;
    background_ = surface_color_;
    applied_ = 0;
    ignored_ = 0;
    zoom_ = 1.0;
    pan_pixels_ = QPointF{0.0, 0.0};
    recomputeFit();
    emit turtleStateChanged();
    emit contentChanged();
    update();
}

// ---------------------------------------------------------------------------
// Bounds and projection
// ---------------------------------------------------------------------------

void TurtleCanvas::growBounds(const QRectF& box) {
    if (!has_bounds_) {
        content_bounds_ = box;
        has_bounds_ = true;
        return;
    }
    content_bounds_ = content_bounds_.united(box);
}

void TurtleCanvas::recomputeFit() {
    if (!has_bounds_) {
        // Nothing drawn yet: the standard window, extended to include wherever
        // the turtle has walked with the pen up.
        fitted_world_ =
            QRectF{QPointF{-kDefaultWorldHalfExtent, -kDefaultWorldHalfExtent},
                   QPointF{kDefaultWorldHalfExtent, kDefaultWorldHalfExtent}}
                .united(QRectF{position_, position_});
        return;
    }

    // The ink, plus the turtle so it cannot walk off the edge unseen. Not the
    // origin: a drawing far from home is the thing worth looking at, and pinning
    // the origin as well would shrink it for no benefit.
    QRectF window = content_bounds_.united(QRectF{position_, position_});
    if (window.width() < kMinimumFitExtent) {
        const double grow = (kMinimumFitExtent - window.width()) / 2.0;
        window.adjust(-grow, 0.0, grow, 0.0);
    }
    if (window.height() < kMinimumFitExtent) {
        const double grow = (kMinimumFitExtent - window.height()) / 2.0;
        window.adjust(0.0, -grow, 0.0, grow);
    }
    const double margin_x = window.width() * kFitMargin;
    const double margin_y = window.height() * kFitMargin;
    window.adjust(-margin_x, -margin_y, margin_x, margin_y);
    fitted_world_ = window;
}

double TurtleCanvas::fittedScale() const {
    const double width = std::max(1, this->width());
    const double height = std::max(1, this->height());
    const double world_width = std::max(1e-9, fitted_world_.width());
    const double world_height = std::max(1e-9, fitted_world_.height());
    // One scale for both axes. A drawing stretched to fill a non-square pane
    // would be wrong in a way that is hard to see and impossible to explain.
    return std::min(width / world_width, height / world_height);
}

double TurtleCanvas::scale() const noexcept { return fittedScale() * zoom_; }

QPointF TurtleCanvas::toScreen(const QPointF& world) const {
    const double s = scale();
    const QPointF centre = fitted_world_.center();
    return QPointF{width() / 2.0 + (world.x() - centre.x()) * s + pan_pixels_.x(),
                   height() / 2.0 - (world.y() - centre.y()) * s + pan_pixels_.y()};
}

QPointF TurtleCanvas::toWorld(const QPointF& screen) const {
    const double s = scale();
    const QPointF centre = fitted_world_.center();
    return QPointF{centre.x() + (screen.x() - width() / 2.0 - pan_pixels_.x()) / s,
                   centre.y() - (screen.y() - height() / 2.0 - pan_pixels_.y()) / s};
}

double TurtleCanvas::totalStrokeLength() const {
    double total = 0.0;
    for (const Stroke& stroke : strokes_) {
        total += stroke.line.length();
    }
    return total;
}

void TurtleCanvas::fitToContent() {
    recomputeFit();
    zoom_ = 1.0;
    pan_pixels_ = QPointF{0.0, 0.0};
    update();
}

void TurtleCanvas::setZoom(double zoom) {
    zoom_ = std::clamp(zoom, kMinimumZoom, kMaximumZoom);
    update();
}

void TurtleCanvas::setGridVisible(bool visible) {
    if (grid_visible_ == visible) {
        return;
    }
    grid_visible_ = visible;
    update();
}

void TurtleCanvas::setSurfaceColor(const QColor& colour) {
    if (!colour.isValid()) {
        return;
    }
    surface_color_ = colour;
    // Repainted immediately as well as remembered: a theme is applied while the
    // canvas is idle, and an idle canvas showing the previous theme's paper until
    // the next run is the whole visible half of the feature missing.
    background_ = colour;
    update();
}

// ---------------------------------------------------------------------------
// Events
// ---------------------------------------------------------------------------

void TurtleCanvas::resizeEvent(QResizeEvent* event) {
    QWidget::resizeEvent(event);
    // The fitted scale is derived from the widget size, so a resize changes it.
    // Nothing else does.
    update();
}

void TurtleCanvas::wheelEvent(QWheelEvent* event) {
    const int steps = event->angleDelta().y();
    if (steps == 0) {
        return;
    }
    const QPointF cursor = event->position();
    // Keep the world point under the cursor under the cursor. Zooming towards
    // the middle instead makes whatever you were looking at slide away.
    const QPointF anchor = toWorld(cursor);

    zoom_ = std::clamp(zoom_ * std::pow(1.0015, static_cast<double>(steps)),
                       kMinimumZoom, kMaximumZoom);

    const double s = scale();
    const QPointF centre = fitted_world_.center();
    pan_pixels_ = QPointF{cursor.x() - width() / 2.0 - (anchor.x() - centre.x()) * s,
                          cursor.y() - height() / 2.0 + (anchor.y() - centre.y()) * s};
    update();
    event->accept();
}

void TurtleCanvas::keyPressEvent(QKeyEvent* event) {
    // Printable characters only: `INKEY$` in these dialects answers text, and a
    // cursor key would arrive as an empty string, which a program reads as
    // "nothing was pressed".
    if (!event->text().isEmpty()) {
        emit keyTyped(event->text().at(0));
    }
    QWidget::keyPressEvent(event);
}

void TurtleCanvas::mousePressEvent(QMouseEvent* event) {
    if (event->button() != Qt::LeftButton) {
        QWidget::mousePressEvent(event);
        return;
    }
    dragging_ = true;
    drag_from_ = event->position();
    setCursor(Qt::ClosedHandCursor);
    event->accept();
}

void TurtleCanvas::mouseMoveEvent(QMouseEvent* event) {
    if (!dragging_) {
        QWidget::mouseMoveEvent(event);
        return;
    }
    pan_pixels_ += event->position() - drag_from_;
    drag_from_ = event->position();
    update();
    event->accept();
}

void TurtleCanvas::mouseReleaseEvent(QMouseEvent* event) {
    if (event->button() != Qt::LeftButton || !dragging_) {
        QWidget::mouseReleaseEvent(event);
        return;
    }
    dragging_ = false;
    unsetCursor();
    event->accept();
}

// ---------------------------------------------------------------------------
// Painting
// ---------------------------------------------------------------------------

void TurtleCanvas::paintEvent(QPaintEvent* /*event*/) {
    QPainter painter{this};
    painter.fillRect(rect(), background_);
    if (grid_visible_) {
        paintGrid(painter);
    }
    // Antialiasing off for the ink and on for the marker: a drawn line wants to
    // be crisp, and a nine-pixel arrow wants to look like an arrow.
    painter.setRenderHint(QPainter::Antialiasing, false);
    paintInk(painter, visible_frames_);
    painter.setRenderHint(QPainter::Antialiasing, true);
    paintTurtle(painter);
}

QImage TurtleCanvas::renderedDrawing(double factor) const {
    const double safe = factor > 0.0 ? factor : 1.0;
    const QSize target{std::max(1, qRound(static_cast<double>(width()) * safe)),
                       std::max(1, qRound(static_cast<double>(height()) * safe))};
    QImage image{target, QImage::Format_ARGB32_Premultiplied};
    image.fill(background_);

    QPainter painter{&image};
    // Antialiasing off for the ink, the way `paintEvent` has it. The scale goes
    // on the painter rather than into a projection: `paintInk` maps to widget
    // coordinates, so scaling here scales the pen widths with it and a 4-unit
    // line stays 4 units wide in the exported picture.
    painter.setRenderHint(QPainter::Antialiasing, false);
    painter.scale(safe, safe);
    // The whole list, never the visible count: scrubbing is a view, and an
    // export after a scrub has to be the same picture as an export without one.
    paintInk(painter, frameCount());
    return image;
}

bool TurtleCanvas::exportDrawing(const QString& path, QString* error,
                                 double factor) const {
    // `QImageWriter` takes the format from the suffix, which is what makes
    // `drawing.png` and `drawing.jpg` do the obvious thing. An unknown suffix is
    // an error rather than a guess at PNG.
    QImageWriter writer{path};
    if (!writer.write(renderedDrawing(factor))) {
        if (error != nullptr) {
            *error = writer.errorString();
        }
        return false;
    }
    return true;
}

void TurtleCanvas::paintGrid(QPainter& painter) {
    const double s = scale();
    // The coarsest step that still leaves the lines readable, so a zoomed-out
    // view does not become a solid block of grid.
    double step = kGridSteps[std::size(kGridSteps) - 1];
    for (const double candidate : kGridSteps) {
        if (candidate * s >= kMinimumGridSpacingPixels) {
            step = candidate;
            break;
        }
    }

    const QPointF top_left = toWorld(QPointF{0.0, 0.0});
    const QPointF bottom_right =
        toWorld(QPointF{static_cast<double>(width()), static_cast<double>(height())});

    QPen minor{QColor{228, 228, 232}};
    minor.setCosmetic(true);
    minor.setWidthF(1.0);
    painter.setPen(minor);

    const auto at_or_below = [](double value, double increment) {
        return std::floor(value / increment) * increment;
    };

    for (double x = at_or_below(top_left.x(), step); x <= bottom_right.x(); x += step) {
        const QPointF top = toScreen(QPointF{x, 0.0});
        painter.drawLine(QPointF{top.x(), 0.0},
                         QPointF{top.x(), static_cast<double>(height())});
    }
    for (double y = at_or_below(bottom_right.y(), step); y <= top_left.y(); y += step) {
        const QPointF left = toScreen(QPointF{0.0, y});
        painter.drawLine(QPointF{0.0, left.y()},
                         QPointF{static_cast<double>(width()), left.y()});
    }

    QPen axes{QColor{148, 148, 156}};
    axes.setCosmetic(true);
    axes.setWidthF(1.0);
    painter.setPen(axes);
    const QPointF origin = toScreen(QPointF{0.0, 0.0});
    if (origin.y() >= 0.0 && origin.y() <= height()) {
        painter.drawLine(QPointF{0.0, origin.y()},
                         QPointF{static_cast<double>(width()), origin.y()});
    }
    if (origin.x() >= 0.0 && origin.x() <= width()) {
        painter.drawLine(QPointF{origin.x(), 0.0},
                         QPointF{origin.x(), static_cast<double>(height())});
    }
}

void TurtleCanvas::paintInk(QPainter& painter, int frames) const {
    // The pen is set on a style change rather than per segment. A turtle program
    // emits long runs of one colour and width, and that grouping is what keeps a
    // drawing of a hundred thousand segments repainting instantly.
    QColor current_color;
    double current_width = -1.0;
    const int count = std::min(frames, static_cast<int>(history_.size()));
    for (int entry_index = 0; entry_index < count; ++entry_index) {
        const HistoryEntry& entry = history_[entry_index];
        if (entry.kind == HistoryEntry::Kind::Stroke) {
            const Stroke& stroke = strokes_[entry.index];
            if (stroke.color != current_color || stroke.width != current_width) {
                current_color = stroke.color;
                current_width = stroke.width;
                QPen pen{current_color};
                // Scaled by the view: a pen width is a world-space figure, so
                // otherwise a 4-unit line and a 400-unit line would look identical,
                // which defeats the point of setting a width at all.
                pen.setWidthF(std::max(0.5, stroke.width * scale()));
                pen.setCapStyle(Qt::RoundCap);
                pen.setJoinStyle(Qt::RoundJoin);
                painter.setPen(pen);
            }
            painter.drawLine(toScreen(stroke.line.p1()), toScreen(stroke.line.p2()));
            continue;
        }

        const Shape& shape = shapes_[entry.index];
        // A shape sets its own pen and brush; the next stroke must not inherit
        // either, so the cached style is invalidated.
        current_width = -1.0;
        QPen pen{shape.color};
        pen.setWidthF(std::max(0.5, shape.width * scale()));
        pen.setCapStyle(Qt::RoundCap);
        painter.setPen(pen);
        painter.setBrush(shape.filled ? QBrush{shape.color} : QBrush{Qt::NoBrush});

        switch (shape.kind) {
            case CanvasCommandKind::DrawPoint: {
                painter.setBrush(QBrush{shape.color});
                painter.drawEllipse(toScreen(shape.at), 2.0, 2.0);
                break;
            }
            case CanvasCommandKind::DrawRect: {
                painter.drawRect(
                    QRectF{toScreen(QPointF{shape.rect.left(), shape.rect.top()}),
                           toScreen(QPointF{shape.rect.right(), shape.rect.bottom()})}
                        .normalized());
                break;
            }
            case CanvasCommandKind::DrawEllipse: {
                const QPointF centre = toScreen(shape.centre);
                const double radius_x = shape.rect.width() / 2.0 * scale();
                const double radius_y = shape.rect.height() / 2.0 * scale();
                painter.drawEllipse(QRectF{centre.x() - radius_x, centre.y() - radius_y,
                                           radius_x * 2.0, radius_y * 2.0});
                break;
            }
            case CanvasCommandKind::DrawArc: {
                // Qt measures arc angles counter-clockwise from 3 o'clock in a
                // y-down space, which -- after this projection's y flip -- is the
                // same angle as the world angle. So no sign juggling is needed.
                painter.drawArc(
                    QRectF{toScreen(QPointF{shape.rect.left(), shape.rect.top()}),
                           toScreen(QPointF{shape.rect.right(), shape.rect.bottom()})}
                        .normalized(),
                    static_cast<int>(shape.a * 16.0), static_cast<int>(shape.b * 16.0));
                break;
            }
            case CanvasCommandKind::DrawText: {
                QFont font = painter.font();
                font.setPointSizeF(std::max(4.0, shape.width * scale()));
                painter.setFont(font);
                painter.drawText(toScreen(shape.at), shape.text);
                break;
            }
            default:
                break;
        }
    }
}

void TurtleCanvas::paintTurtle(QPainter& painter) {
    if (!turtle_visible_) {
        return;
    }
    // Built in *world* coordinates around the turtle and then projected, so the
    // y flip is applied once, by `toScreen`, instead of by hand here -- and the
    // marker keeps a constant size on screen because its world size is divided
    // by the view scale.
    const double reach = kTurtlePixels / scale();
    const QPointF forward = headingVector();
    const QPointF left{-forward.y(), forward.x()};

    const QPointF nose = position_ + forward * reach;
    const QPointF left_barb = position_ - forward * reach * 0.75 + left * reach * 0.6;
    const QPointF right_barb = position_ - forward * reach * 0.75 - left * reach * 0.6;

    QPainterPath glyph;
    glyph.moveTo(toScreen(nose));
    glyph.lineTo(toScreen(left_barb));
    glyph.lineTo(toScreen(right_barb));
    glyph.closeSubpath();

    painter.setPen(QPen{QColor{120, 30, 30}, 1.0});
    painter.setBrush(QBrush{QColor{220, 70, 70}});
    painter.drawPath(glyph);
}

}  // namespace timewarp::ui
