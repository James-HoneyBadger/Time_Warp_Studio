// SPDX-License-Identifier: MIT
//
// The view: the world window, the fit, zoom, the grid, and the events that change
// them.

#include "timewarp/ui/turtle_canvas.hpp"

#include "turtle_canvas_internal.hpp"

#include <algorithm>
#include <cmath>

#include <QKeyEvent>
#include <QMouseEvent>
#include <QResizeEvent>
#include <QWheelEvent>

#ifdef TIME_WARP_HAVE_SVG
#endif

namespace timewarp::ui {

using namespace canvas_detail;
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

}  // namespace


void TurtleCanvas::growBounds(const QRectF& box) {
    if (!has_bounds_) {
        content_bounds_ = box;
        has_bounds_ = true;
        return;
    }
    content_bounds_ = content_bounds_.united(box);
}

void TurtleCanvas::recomputeFit() {
    if (svga_) {
        // A virtual screen is a fixed extent, not a fit: a program that positions
        // things at 800x600 coordinates has said what its coordinate system is.
        fitted_world_ = QRectF{QPointF{-svga_size_.width() / 2.0, -svga_size_.height() / 2.0},
                               svga_size_};
        return;
    }
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
    if (framebufferActive()) {
        // A screen is already fitted: its coordinates are the program's. The
        // call is not an error -- a run ends by fitting -- it just has nothing
        // to do, and moving the view here would be the refit a screen must not
        // have.
        return;
    }
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

    if (framebufferActive()) {
        // A screen zooms in its own coordinates. The anchor is the screen pixel
        // under the cursor, which is found by working back through the same
        // rectangle the painter used, then put back under the cursor afterwards.
        const QPointF pixel = framebufferPixelAt(cursor);
        zoom_ = std::clamp(zoom_ * std::pow(1.0015, static_cast<double>(steps)),
                           kMinimumZoom, kMaximumZoom);
        const QRectF after = framebufferTarget();
        if (after.isEmpty()) {
            update();
            event->accept();
            return;
        }
        const QPointF placed{after.left() + pixel.x() * after.width() / framebuffer_.width(),
                             after.top() + pixel.y() * after.height() / framebuffer_.height()};
        pan_pixels_ += cursor - placed;
        update();
        event->accept();
        return;
    }

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
    if (framebufferActive()) {
        // A click on a screen is a question -- "what colour is this pixel?" --
        // and the answer is cheap enough to give before the drag begins.
        const QPointF pixel = framebufferPixelAt(drag_from_);
        const int x = static_cast<int>(std::floor(pixel.x()));
        const int y = static_cast<int>(std::floor(pixel.y()));
        if (x >= 0 && y >= 0 && x < framebuffer_.width() && y < framebuffer_.height()) {
            emit pixelPicked(x, y, framebuffer_.pixelColor(x, y));
        }
    }
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

}  // namespace timewarp::ui
