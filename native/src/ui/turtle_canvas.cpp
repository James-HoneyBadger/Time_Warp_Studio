// SPDX-License-Identifier: MIT

#include "timewarp/core/qt_gui_compat.hpp"
#include "timewarp/core/qt_compat.hpp"
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
#include <QSaveFile>
#include <QWheelEvent>

#ifdef TIME_WARP_HAVE_SVG
#include <QSvgGenerator>
#endif

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
    // The pen is part of the state this widget reports, and `turtleStateChanged`
    // promises "moved, turned, or changed pen". A pen-up that left the signal
    // unsent left every readout showing a pen that was no longer down.
    const bool before_pen_down = pen_down_;
    const QColor before_pen_colour = pen_color_;
    const double before_pen_width = pen_width_;
    const bool before_visible = turtle_visible_;

    // The screen, drawn a second time in its own coordinates. The display list
    // below still runs, because it is the record the log, the scrubber and a
    // later export read; this is what the surface *shows* when a program asked
    // for a screen.
    if (framebufferActive()) {
        rasterize(view);
    }

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
            sprites_.clear();
            history_.clear();
            visible_frames_ = 0;
            content_bounds_ = QRectF{};
            has_bounds_ = false;
            if (framebufferActive()) {
                framebuffer_.fill(background_);
            }
            recomputeFit();
            break;
        case CanvasCommandKind::Reset:
            reset();
            return;
        case CanvasCommandKind::SetFramebuffer:
            setFramebufferSize(static_cast<int>(command.x), static_cast<int>(command.y));
            break;
        case CanvasCommandKind::ClearFramebuffer:
            clearFramebuffer();
            break;

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
            // A *filled* polygon carries its fill in `b`, next to the flag that
            // says there is one -- the convention `FILLED` uses, where the outline
            // and the fill are different colours. Recording it only as the stroke
            // colour made `FILLED "red" [...]` fill with the pen's ink.
            if (command.is_filled()) {
                Shape shape;
                shape.kind = CanvasCommandKind::DrawPolyline;
                shape.closed = command.is_closed();
                shape.filled = true;
                const auto fill = static_cast<std::uint32_t>(command.b);
                shape.color = color;
                shape.color2 = fill != 0u
                                   ? QColor{static_cast<int>((fill >> 16) & 0xFFu),
                                            static_cast<int>((fill >> 8) & 0xFFu),
                                            static_cast<int>(fill & 0xFFu)}
                                   : color;
                shape.width = width;
                shape.points.reserve(static_cast<qsizetype>(points));
                for (std::size_t index = 0; index < points; ++index) {
                    shape.points.append(QPointF{view.points[index * 2],
                                                view.points[index * 2 + 1]});
                }
                QRectF box = shape.points.isEmpty()
                                 ? QRectF{}
                                 : QRectF{shape.points.first(), shape.points.first()};
                for (const QPointF& point : shape.points) {
                    box.setLeft(std::min(box.left(), point.x()));
                    box.setRight(std::max(box.right(), point.x()));
                    box.setTop(std::min(box.top(), point.y()));
                    box.setBottom(std::max(box.bottom(), point.y()));
                }
                shape.rect = box;
                shape.centre = box.center();
                appendShape(shape);
                growBounds(box);
                break;
            }
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
        case CanvasCommandKind::PutPixels: {
            // A sprite placed by `PUT`. The raster counts its rows down from the
            // block's top edge; this view counts y up, so each row is flipped.
            // Equal colours in a row are one filled rect, which keeps a tile from
            // becoming hundreds of shape entries in the scrubber.
            const int width = static_cast<int>(command.w);
            const int height = static_cast<int>(command.h);
            if (width <= 0 || height <= 0) {
                break;
            }
            const auto columns = static_cast<std::size_t>(width);
            // One double per pixel, not one (x, y) pair per pixel, so this is the
            // one command whose payload is counted in doubles and not in points.
            const std::size_t pixels =
                std::min(columns * static_cast<std::size_t>(height), view.points.size());
            const auto packed_at = [&view](std::size_t index) {
                return static_cast<std::uint32_t>(view.points[index]);
            };
            for (int row = 0; row < height; ++row) {
                const std::size_t row_start = static_cast<std::size_t>(row) * columns;
                if (row_start >= pixels) {
                    break;
                }
                // A short payload loses whole pixels off the end of the last row,
                // which is what a truncated block should mean.
                const int row_columns = static_cast<int>(
                    std::min(columns, pixels - row_start));
                int column = 0;
                while (column < row_columns) {
                    const std::size_t start = row_start + static_cast<std::size_t>(column);
                    const std::uint32_t colour = packed_at(start);
                    int run = 1;
                    while (column + run < row_columns &&
                           packed_at(start + static_cast<std::size_t>(run)) == colour) {
                        ++run;
                    }
                    Shape shape;
                    shape.kind = CanvasCommandKind::DrawRect;
                    shape.rect = QRectF{command.x + column, command.y - row,
                                        static_cast<double>(run), 1.0};
                    shape.centre = shape.rect.center();
                    shape.color = QColor{static_cast<int>((colour >> 16) & 0xFFu),
                                         static_cast<int>((colour >> 8) & 0xFFu),
                                         static_cast<int>(colour & 0xFFu)};
                    shape.width = 0.0;
                    shape.filled = true;
                    appendShape(shape);
                    growBounds(shape.rect);
                    column += run;
                }
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

        // -- fills ---------------------------------------------------------
        //
        // The reference's `begin_fill`/`end_fill` is a polygon walk, not a paint
        // bucket: the lines drawn between the two commands *are* the outline.
        case CanvasCommandKind::BeginFill:
            filling_ = true;
            fill_start_ = static_cast<std::size_t>(strokes_.size());
            break;
        case CanvasCommandKind::EndFill:
            if (filling_) {
                closeFill();
            }
            break;
        case CanvasCommandKind::FillLastShape: {
            // Recolours the shape drawn last, which is how `fill_last_shape`
            // works in the reference: the shape exists, its fill did not.
            if (shapes_.isEmpty()) {
                break;
            }
            Shape& last = shapes_.last();
            last.filled = true;
            last.color2 = QColor{command.red(), command.green(), command.blue()};
            break;
        }

        // -- pen style ------------------------------------------------------
        case CanvasCommandKind::SetPenStyle:
            pen_style_ = static_cast<int>(command.index);
            if (command.x > 0.0) {
                pen_style_parameter_ = command.x;
            }
            break;

        // -- gradients ------------------------------------------------------
        case CanvasCommandKind::DrawRectGradient:
        case CanvasCommandKind::DrawEllipseGradient: {
            Shape shape;
            shape.kind = command.kind;
            shape.rect = QRectF{command.x, command.y, command.w, command.h}.normalized();
            shape.centre = shape.rect.center();
            shape.color = QColor{command.red(), command.green(), command.blue()};
            const auto second = timewarp::canvas::gradient_second(command);
            shape.color2 = QColor{static_cast<int>((second >> 16) & 0xFFu),
                                  static_cast<int>((second >> 8) & 0xFFu),
                                  static_cast<int>(second & 0xFFu)};
            // The angle rides in `a` because a rect has no other use for it; a
            // rect drawn at 0 is the common case and shades left to right.
            shape.a = command.a;
            shape.filled = true;
            appendShape(shape);
            growBounds(shape.rect);
            break;
        }

        // -- bezier ---------------------------------------------------------
        case CanvasCommandKind::BezierCurve: {
            const std::size_t count = view.point_count();
            if (count < 2) {
                break;
            }
            Shape shape;
            shape.kind = CanvasCommandKind::BezierCurve;
            shape.points.reserve(static_cast<qsizetype>(count));
            QRectF box{QPointF{view.points[0], view.points[1]},
                       QPointF{view.points[0], view.points[1]}};
            for (std::size_t index = 0; index < count; ++index) {
                const QPointF point{view.points[index * 2], view.points[index * 2 + 1]};
                shape.points.append(point);
                box.setLeft(std::min(box.left(), point.x()));
                box.setRight(std::max(box.right(), point.x()));
                box.setTop(std::min(box.top(), point.y()));
                box.setBottom(std::max(box.bottom(), point.y()));
            }
            shape.rect = box;
            shape.centre = box.center();
            shape.color = QColor{command.red(), command.green(), command.blue()};
            shape.width = command.x > 0.0 ? command.x : pen_width_;
            appendShape(shape);
            growBounds(box);
            break;
        }

        // -- sprites --------------------------------------------------------
        case CanvasCommandKind::DefineSprite: {
            const QString name = QString::fromUtf8(view.text.data(),
                                                   static_cast<qsizetype>(view.text.size()));
            const int width = static_cast<int>(command.w);
            const int height = static_cast<int>(command.h);
            if (name.isEmpty() || width <= 0 || height <= 0) {
                break;
            }
            const auto columns = static_cast<std::size_t>(width);
            // One double per pixel, like `PutPixels`: a bitmap, not a polygon.
            const std::size_t available = view.points.size();
            const std::size_t expected = columns * static_cast<std::size_t>(height);
            QImage image{QSize{width, height}, QImage::Format_ARGB32_Premultiplied};
            image.fill(Qt::transparent);
            for (int row = 0; row < height; ++row) {
                for (int column = 0; column < width; ++column) {
                    const std::size_t index =
                        static_cast<std::size_t>(row) * columns +
                        static_cast<std::size_t>(column);
                    if (index >= available) {
                        break;
                    }
                    const auto packed = static_cast<std::uint32_t>(view.points[index]);
                    // A zero pixel is transparent rather than black: an
                    // unset cell of a sprite is a hole, which is what a sprite
                    // needs and what the reference's `None` meant.
                    if (packed == 0u) {
                        continue;
                    }
                    image.setPixelColor(column, row,
                                        QColor{static_cast<int>((packed >> 16) & 0xFFu),
                                               static_cast<int>((packed >> 8) & 0xFFu),
                                               static_cast<int>(packed & 0xFFu)});
                }
            }
            Q_UNUSED(expected);
            sprites_.insert(name, Sprite{image, QPointF{command.x, command.y}});
            break;
        }
        case CanvasCommandKind::PlaceSprite: {
            const QString name = QString::fromUtf8(view.text.data(),
                                                   static_cast<qsizetype>(view.text.size()));
            const auto found = sprites_.constFind(name);
            if (found == sprites_.constEnd()) {
                // Placing a sprite nobody defined is a program error, but the
                // canvas is not the place to report it: the shape is recorded with
                // an empty bitmap and draws nothing, which is what the engine's own
                // message already said.
                break;
            }
            Shape shape;
            shape.kind = CanvasCommandKind::PlaceSprite;
            shape.text = name;
            shape.at = QPointF{command.x - found->hotspot.x(),
                               command.y + found->hotspot.y()};
            shape.rect = QRectF{shape.at, QSizeF{static_cast<double>(found->image.width()),
                                                 static_cast<double>(found->image.height())}};
            shape.filled = true;
            appendShape(shape);
            growBounds(shape.rect);
            break;
        }
        case CanvasCommandKind::MoveSprite: {
            // Moves the newest placement of this sprite. A sprite that has not
            // been placed cannot move, and saying so by doing nothing is what the
            // reference did too.
            const QString name = QString::fromUtf8(view.text.data(),
                                                   static_cast<qsizetype>(view.text.size()));
            for (qsizetype index = shapes_.size(); index-- > 0;) {
                Shape& shape = shapes_[index];
                if (shape.kind != CanvasCommandKind::PlaceSprite || shape.text != name) {
                    continue;
                }
                // A sprite is a bitmap, so its coordinates are the raster's: y
                // grows downwards and the delta is added as given, which is also
                // what the framebuffer path does. Negating it here moved the log
                // the opposite way from the picture.
                shape.at += QPointF{command.x, command.y};
                shape.rect.moveTopLeft(shape.at);
                growBounds(shape.rect);
                break;
            }
            break;
        }
        case CanvasCommandKind::ShowSprite:
        case CanvasCommandKind::HideSprite: {
            const bool visible = command.kind == CanvasCommandKind::ShowSprite;
            const QString name = QString::fromUtf8(view.text.data(),
                                                   static_cast<qsizetype>(view.text.size()));
            for (qsizetype index = shapes_.size(); index-- > 0;) {
                Shape& shape = shapes_[index];
                if (shape.kind != CanvasCommandKind::PlaceSprite || shape.text != name) {
                    continue;
                }
                shape.closed = visible;
                break;
            }
            break;
        }

        // -- palette --------------------------------------------------------
        case CanvasCommandKind::SetPaletteColor:
            palette_.insert(static_cast<int>(command.index),
                            QColor{command.red(), command.green(), command.blue()});
            break;
        case CanvasCommandKind::CyclePalette:
            // One step per command, in the direction the sign gives. Cycling is a
            // *repaint*: every stroke that uses a palette colour is redrawn in the
            // next slot's, which is the effect a C64 border or an Amiga copper bar
            // is made of.
            palette_offset_ += static_cast<int>(command.a >= 0.0 ? command.index
                                                                 : -command.index);
            break;

        // -- virtual screen -------------------------------------------------
        case CanvasCommandKind::SetSvgaMode:
            if (command.x > 0.0 && command.y > 0.0) {
                svga_ = true;
                svga_size_ = QSizeF{command.x, command.y};
                recomputeFit();
            }
            break;

        default:
            // Counted, never silently dropped. Nothing reaches here: every kind in
            // the vocabulary has a case above, which `turtle_canvas_tests` walks to
            // prove. The counter stays because the next kind added without a case
            // must be visible rather than invisible.
            ++ignored_;
            return;
    }

    ++applied_;
    if (position_ != before || heading_ != before_heading || pen_down_ != before_pen_down ||
        pen_color_ != before_pen_colour || pen_width_ != before_pen_width ||
        turtle_visible_ != before_visible) {
        emit turtleStateChanged();
    }
    emit contentChanged();
    update();
}

void TurtleCanvas::setTurtlePose(const QPointF& position, double heading, bool visible) {
    const QPointF before_position = position_;
    const double before_heading = heading_;
    const bool before_visible = turtle_visible_;
    heading_ = normaliseHeading(heading);
    turtle_visible_ = visible;
    if (position_ != position) {
        position_ = position;
    }
    // An arrival from the bridge that only turned the turtle is still a state
    // change: the readouts show the heading, and the marker is tilted by it.
    if (position_ != before_position || heading_ != before_heading ||
        turtle_visible_ != before_visible) {
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

QPen TurtleCanvas::penFor(const QColor& color, double width, int style) const {
    QPen pen{color};
    pen.setWidthF(std::max(0.5, width * scale()));
    pen.setCapStyle(style == 0 ? Qt::RoundCap : Qt::FlatCap);
    pen.setJoinStyle(Qt::RoundJoin);
    // The dash is measured in multiples of the line width, and scaled with the
    // view: a dashed ink that turned solid when you zoomed out would be the
    // control doing nothing at the one moment you use it.
    const double unit = std::max(1.0, pen.widthF()) * std::max(0.25, pen_style_parameter_);
    switch (style) {
        case 1:  // dashed
            pen.setStyle(Qt::CustomDashLine);
            pen.setDashPattern({3.0, 2.0});
            pen.setDashOffset(0.0);
            pen.setWidthF(std::max(0.5, pen.widthF()));
            break;
        case 2:  // dotted
            pen.setStyle(Qt::CustomDashLine);
            pen.setDashPattern({1.0, 2.0});
            break;
        case 3:  // dash-dot
            pen.setStyle(Qt::CustomDashLine);
            pen.setDashPattern({3.0, 2.0, 1.0, 2.0});
            break;
        case 4:  // invisible ink: the turtle moves and nothing is drawn
            pen.setStyle(Qt::NoPen);
            break;
        default:
            pen.setStyle(Qt::SolidLine);
            break;
    }
    Q_UNUSED(unit);
    return pen;
}

QColor TurtleCanvas::paletteMapped(const QColor& colour) const {
    if (palette_.size() < 2 || palette_offset_ == 0) {
        return colour;
    }
    // The colours a program has declared, in slot order. A colour that is not one
    // of them passes through untouched, so cycling never recolours ink the program
    // did not put in the palette.
    // `slots` is a Qt keyword, so the local is `declared`: the spelling that
    // compiles is the one that says what it holds anyway.
    const QList<int> declared = palette_.keys();
    const int count = static_cast<int>(declared.size());
    for (int index = 0; index < count; ++index) {
        if (palette_.value(declared.at(index)) != colour) {
            continue;
        }
        const int rotated = ((index + palette_offset_) % count + count) % count;
        return palette_.value(declared.at(rotated));
    }
    return colour;
}

void TurtleCanvas::closeFill() {
    filling_ = false;
    if (fill_start_ >= static_cast<std::size_t>(strokes_.size())) {
        return;
    }
    // The reference's `end_fill`: the first stroke's start, then every stroke's
    // end. That is a closed path even when the turtle did not return to where it
    // began, which is what makes `repeat 4 [fd 10 rt 90]` fill a square.
    Shape shape;
    shape.kind = CanvasCommandKind::DrawPolyline;
    shape.points.reserve(static_cast<qsizetype>(strokes_.size() - fill_start_) + 1);
    shape.points.append(strokes_[static_cast<qsizetype>(fill_start_)].line.p1());
    for (std::size_t index = fill_start_; index < static_cast<std::size_t>(strokes_.size());
         ++index) {
        shape.points.append(strokes_[index].line.p2());
    }
    if (shape.points.size() < 3) {
        return;
    }
    QRectF box{shape.points.first(), shape.points.first()};
    for (const QPointF& point : shape.points) {
        box.setLeft(std::min(box.left(), point.x()));
        box.setRight(std::max(box.right(), point.x()));
        box.setTop(std::min(box.top(), point.y()));
        box.setBottom(std::max(box.bottom(), point.y()));
    }
    shape.rect = box;
    shape.centre = box.center();
    shape.color = pen_color_;
    shape.width = pen_width_;
    shape.filled = true;
    shape.closed = true;
    appendShape(shape);
    growBounds(box);
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
    // An invisible pen leaves no ink and no history: recording an invisible
    // stroke would make the scrubber stop on a frame that draws nothing.
    if (pen_style_ == 4) {
        growBounds(QRectF{from, to}.normalized());
        return;
    }
    strokes_.append(Stroke{QLineF{from, to}, color, width, pen_style_});
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
    // A fresh run starts with a plain pen and an empty palette: a program that
    // cycles the palette must not inherit the previous program's slots, and one
    // that sets a dash must not inherit a dash.
    pen_style_ = 0;
    pen_style_parameter_ = 1.0;
    filling_ = false;
    fill_start_ = 0;
    sprites_.clear();
    palette_.clear();
    palette_offset_ = 0;
    svga_ = false;
    svga_size_ = QSizeF{800.0, 600.0};
    background_ = surface_color_;
    applied_ = 0;
    ignored_ = 0;
    zoom_ = 1.0;
    pan_pixels_ = QPointF{0.0, 0.0};
    framebuffer_ = QImage{};
    placements_.clear();
    raster_position_ = QPointF{0.0, 0.0};
    recomputeFit();
    emit turtleStateChanged();
    emit contentChanged();
    emit framebufferChanged();
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
    const QPointF cursor = timewarp::core::eventPosition(event);

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
    drag_from_ = timewarp::core::eventPosition(event);
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
    pan_pixels_ += timewarp::core::eventPosition(event) - drag_from_;
    drag_from_ = timewarp::core::eventPosition(event);
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

// ---------------------------------------------------------------------------
// Screen mode
// ---------------------------------------------------------------------------

void TurtleCanvas::setFramebufferSize(int width, int height) {
    if (width <= 0 || height <= 0) {
        return;
    }
    framebuffer_ = QImage{QSize{width, height}, QImage::Format_ARGB32_Premultiplied};
    framebuffer_.fill(background_);
    raster_position_ = QPointF{0.0, 0.0};
    raster_colour_ = pen_color_;
    raster_width_ = pen_width_;
    raster_pen_down_ = true;
    emit framebufferChanged();
    update();
}

void TurtleCanvas::clearFramebuffer() {
    if (framebuffer_.isNull()) {
        return;
    }
    framebuffer_ = QImage{};
    placements_.clear();
    emit framebufferChanged();
    update();
}

QColor TurtleCanvas::framebufferPixel(int x, int y) const {
    if (framebuffer_.isNull() || x < 0 || y < 0 || x >= framebuffer_.width() ||
        y >= framebuffer_.height()) {
        return QColor{};
    }
    return framebuffer_.pixelColor(x, y);
}

void TurtleCanvas::rasterLine(const QPointF& from, const QPointF& to) {
    QPainter painter{&framebuffer_};
    painter.setRenderHint(QPainter::Antialiasing, false);
    QPen pen{raster_colour_};
    pen.setWidthF(std::max(1.0, raster_width_));
    pen.setCapStyle(Qt::RoundCap);
    painter.setPen(pen);
    painter.drawLine(from, to);
}

void TurtleCanvas::stampBitmap(BitmapPlacement& placement) {
    const auto found = sprites_.constFind(placement.name);
    if (framebuffer_.isNull() || found == sprites_.constEnd() ||
        found->image.isNull()) {
        placement.visible = false;
        return;
    }
    const QRect area{QPoint{qRound(placement.at.x()), qRound(placement.at.y())},
                     found->image.size()};
    // Clipped, so a sprite half off the screen puts back only the part that was
    // ever covered.
    const QRect clipped = area.intersected(framebuffer_.rect());
    placement.under = clipped.isEmpty() ? QImage{} : framebuffer_.copy(clipped);
    QPainter painter{&framebuffer_};
    painter.drawImage(placement.at, found->image);
    placement.visible = true;
}

void TurtleCanvas::eraseBitmap(BitmapPlacement& placement) {
    if (!placement.visible || placement.under.isNull()) {
        placement.visible = false;
        return;
    }
    QPainter painter{&framebuffer_};
    painter.drawImage(QPoint{qRound(placement.at.x()), qRound(placement.at.y())},
                      placement.under);
    placement.visible = false;
}

void TurtleCanvas::rasterize(const timewarp::canvas::CanvasCommandView& view) {
    const timewarp::canvas::CanvasCommand& command = view.command;
    switch (command.kind) {
        case CanvasCommandKind::Goto:
        case CanvasCommandKind::SetX:
        case CanvasCommandKind::SetY: {
            // `CIRCLE` walks the pen around its arc with gotos, so these are the
            // segments that make a circle visible on a screen.
            const QPointF target{
                command.kind == CanvasCommandKind::SetY ? raster_position_.x() : command.x,
                command.kind == CanvasCommandKind::SetX ? raster_position_.y() : command.y};
            if (raster_pen_down_) {
                rasterLine(raster_position_, target);
            }
            raster_position_ = target;
            break;
        }
        case CanvasCommandKind::PenUp:
            raster_pen_down_ = false;
            break;
        case CanvasCommandKind::PenDown:
            raster_pen_down_ = true;
            break;
        case CanvasCommandKind::SetPenColor:
            raster_colour_ = QColor{command.red(), command.green(), command.blue()};
            break;
        case CanvasCommandKind::SetPenWidth:
            if (command.x > 0.0) {
                raster_width_ = command.x;
            }
            break;
        case CanvasCommandKind::DrawPoint:
            framebuffer_.setPixelColor(qRound(command.x), qRound(command.y),
                                       QColor{command.red(), command.green(), command.blue()});
            break;
        case CanvasCommandKind::DrawPolyline: {
            // A polyline carries its own pen: `x` is the width and `color` the
            // ink, so the raster pen is borrowed for the duration of the shape.
            const QColor saved_colour = raster_colour_;
            const double saved_width = raster_width_;
            raster_colour_ = QColor{command.red(), command.green(), command.blue()};
            if (command.x > 0.0) {
                raster_width_ = command.x;
            }
            const std::size_t points = view.point_count();
            for (std::size_t index = 0; index + 1 < points; ++index) {
                rasterLine(QPointF{view.points[index * 2], view.points[index * 2 + 1]},
                           QPointF{view.points[index * 2 + 2], view.points[index * 2 + 3]});
            }
            if (command.is_closed() && points >= 3) {
                rasterLine(QPointF{view.points[(points - 1) * 2],
                                   view.points[(points - 1) * 2 + 1]},
                           QPointF{view.points[0], view.points[1]});
            }
            if (command.is_filled() && points >= 3) {
                // The same convention as the display list: the fill colour is in
                // `b` and the outline is the stroke's.
                const auto fill_bits = static_cast<std::uint32_t>(command.b);
                QPolygonF polygon;
                polygon.reserve(static_cast<qsizetype>(points));
                for (std::size_t index = 0; index < points; ++index) {
                    polygon.append(QPointF{view.points[index * 2],
                                           view.points[index * 2 + 1]});
                }
                QPainter painter{&framebuffer_};
                painter.setRenderHint(QPainter::Antialiasing, false);
                painter.setPen(Qt::NoPen);
                painter.setBrush(QBrush{fill_bits != 0u
                                            ? QColor{static_cast<int>((fill_bits >> 16) & 0xFFu),
                                                     static_cast<int>((fill_bits >> 8) & 0xFFu),
                                                     static_cast<int>(fill_bits & 0xFFu)}
                                            : raster_colour_});
                painter.drawPolygon(polygon);
            }
            raster_colour_ = saved_colour;
            raster_width_ = saved_width;
            break;
        }
        case CanvasCommandKind::DrawRect: {
            const QColor colour{command.red(), command.green(), command.blue()};
            QPainter painter{&framebuffer_};
            painter.setRenderHint(QPainter::Antialiasing, false);
            painter.setPen(QPen{colour});
            painter.setBrush(command.is_filled() ? QBrush{colour} : QBrush{Qt::NoBrush});
            painter.drawRect(
                QRectF{QPointF{command.x, command.y}, QSizeF{command.w, command.h}}
                    .normalized());
            break;
        }
        // A screen program is entitled to the same vocabulary as a drawing one:
        // the shapes below are drawn into the framebuffer rather than left to the
        // display list, because on a screen the framebuffer *is* the picture.
        case CanvasCommandKind::DrawEllipse: {
            const QColor colour{command.red(), command.green(), command.blue()};
            QPainter painter{&framebuffer_};
            painter.setRenderHint(QPainter::Antialiasing, false);
            painter.setPen(QPen{colour});
            painter.setBrush(command.is_filled() ? QBrush{colour} : QBrush{Qt::NoBrush});
            painter.drawEllipse(
                QRectF{QPointF{command.x, command.y}, QSizeF{command.w, command.h}}
                    .normalized());
            break;
        }
        case CanvasCommandKind::DrawRectGradient:
        case CanvasCommandKind::DrawEllipseGradient: {
            const QRectF box{QPointF{command.x, command.y}, QSizeF{command.w, command.h}};
            const auto second = timewarp::canvas::gradient_second(command);
            const double radians = degreesToRadians(command.a);
            const QPointF centre = box.center();
            const double reach = std::hypot(box.width(), box.height()) / 2.0;
            QLinearGradient gradient{
                centre - QPointF{std::cos(radians), -std::sin(radians)} * reach,
                centre + QPointF{std::cos(radians), -std::sin(radians)} * reach};
            gradient.setColorAt(0.0, QColor{command.red(), command.green(), command.blue()});
            gradient.setColorAt(1.0,
                                QColor{static_cast<int>((second >> 16) & 0xFFu),
                                       static_cast<int>((second >> 8) & 0xFFu),
                                       static_cast<int>(second & 0xFFu)});
            QPainter painter{&framebuffer_};
            painter.setPen(Qt::NoPen);
            painter.setBrush(QBrush{gradient});
            if (command.kind == CanvasCommandKind::DrawRectGradient) {
                painter.drawRect(box.normalized());
            } else {
                painter.drawEllipse(box.normalized());
            }
            break;
        }
        case CanvasCommandKind::DefineSprite: {
            // The definition itself draws nothing; the placement does. It is
            // handled here so that a screen program's sprite is defined for the
            // framebuffer path as well as for the display list.
            const QString name = QString::fromUtf8(view.text.data(),
                                                   static_cast<qsizetype>(view.text.size()));
            const int width = static_cast<int>(command.w);
            const int height = static_cast<int>(command.h);
            if (name.isEmpty() || width <= 0 || height <= 0) {
                break;
            }
            const auto columns = static_cast<std::size_t>(width);
            QImage image{QSize{width, height}, QImage::Format_ARGB32_Premultiplied};
            image.fill(Qt::transparent);
            for (int row = 0; row < height; ++row) {
                for (int column = 0; column < width; ++column) {
                    const std::size_t index = static_cast<std::size_t>(row) * columns +
                                              static_cast<std::size_t>(column);
                    if (index >= view.points.size()) {
                        break;
                    }
                    const auto packed = static_cast<std::uint32_t>(view.points[index]);
                    if (packed == 0u) {
                        continue;
                    }
                    image.setPixelColor(column, row,
                                        QColor{static_cast<int>((packed >> 16) & 0xFFu),
                                               static_cast<int>((packed >> 8) & 0xFFu),
                                               static_cast<int>(packed & 0xFFu)});
                }
            }
            sprites_.insert(name, Sprite{image, QPointF{command.x, command.y}});
            break;
        }
        case CanvasCommandKind::PlaceSprite: {
            const QString name = QString::fromUtf8(view.text.data(),
                                                   static_cast<qsizetype>(view.text.size()));
            const auto found = sprites_.constFind(name);
            if (found == sprites_.constEnd()) {
                break;
            }
            BitmapPlacement placement;
            placement.name = name;
            placement.at = QPointF{command.x - found->hotspot.x(),
                                   command.y - found->hotspot.y()};
            placements_.push_back(placement);
            stampBitmap(placements_.back());
            break;
        }
        case CanvasCommandKind::MoveSprite: {
            const QString name = QString::fromUtf8(view.text.data(),
                                                   static_cast<qsizetype>(view.text.size()));
            for (std::size_t index = placements_.size(); index-- > 0;) {
                BitmapPlacement& placement = placements_[index];
                if (placement.name != name) {
                    continue;
                }
                // Raster coordinates, so y grows downwards and the delta is added
                // as given -- the display list's world y grows upwards, which is
                // why the two paths do not share this arithmetic.
                eraseBitmap(placement);
                placement.at += QPointF{command.x, command.y};
                stampBitmap(placement);
                break;
            }
            break;
        }
        case CanvasCommandKind::ShowSprite:
        case CanvasCommandKind::HideSprite: {
            const bool visible = command.kind == CanvasCommandKind::ShowSprite;
            const QString name = QString::fromUtf8(view.text.data(),
                                                   static_cast<qsizetype>(view.text.size()));
            for (std::size_t index = placements_.size(); index-- > 0;) {
                BitmapPlacement& placement = placements_[index];
                if (placement.name != name) {
                    continue;
                }
                if (visible) {
                    stampBitmap(placement);
                } else {
                    eraseBitmap(placement);
                }
                break;
            }
            break;
        }
        case CanvasCommandKind::PutPixels: {
            // One double per pixel, top row first, which is this command's own
            // convention rather than the point-list every other payload uses.
            const int width = static_cast<int>(command.w);
            const int height = static_cast<int>(command.h);
            if (width <= 0 || height <= 0) {
                break;
            }
            const auto columns = static_cast<std::size_t>(width);
            const std::size_t pixels =
                std::min(columns * static_cast<std::size_t>(height), view.points.size());
            for (std::size_t index = 0; index < pixels; ++index) {
                const auto packed = static_cast<std::uint32_t>(view.points[index]);
                framebuffer_.setPixelColor(
                    static_cast<int>(command.x) + static_cast<int>(index % columns),
                    static_cast<int>(command.y) + static_cast<int>(index / columns),
                    QColor{static_cast<int>((packed >> 16) & 0xFFu),
                           static_cast<int>((packed >> 8) & 0xFFu),
                           static_cast<int>(packed & 0xFFu)});
            }
            break;
        }
        default:
            break;
    }
}

QRectF TurtleCanvas::framebufferTarget() const {
    const int screen_width = framebuffer_.width();
    const int screen_height = framebuffer_.height();
    if (screen_width <= 0 || screen_height <= 0) {
        return {};
    }
    // Whole-number scaling while there is room: a 320x200 screen at 3x is three
    // screen pixels per drawn pixel, and a smoothed one is a lie about what the
    // program drew. Only a window smaller than the screen scales fractionally.
    double factor = std::min(static_cast<double>(width()) / screen_width,
                             static_cast<double>(height()) / screen_height);
    if (factor >= 1.0) {
        factor = std::floor(factor);
    }
    if (factor <= 0.0) {
        return {};
    }
    // The zoom and the drag are the same ones the drawing uses: a screen with a
    // dead wheel is the one control a game author reaches for first.
    factor *= zoom_;
    // A screen is made of pixels, so the drawn scale snaps to a whole number and
    // a one-pixel line stays one screen pixel wide. Zooming in therefore steps,
    // which is what a pixel grid should do; below 2x there is no whole number
    // left to snap to, so it scales smoothly and can still be shrunk.
    if (factor >= 2.0) {
        factor = std::round(factor);
    }
    const double drawn_width = screen_width * factor;
    const double drawn_height = screen_height * factor;
    return QRectF{(width() - drawn_width) / 2.0 + pan_pixels_.x(),
                  (height() - drawn_height) / 2.0 + pan_pixels_.y(), drawn_width,
                  drawn_height};
}

void TurtleCanvas::paintFramebuffer(QPainter& painter) const {
    const QRectF target = framebufferTarget();
    if (target.isEmpty()) {
        return;
    }
    painter.setRenderHint(QPainter::SmoothPixmapTransform, false);
    painter.drawImage(target, framebuffer_, QRectF{framebuffer_.rect()});
}

QPointF TurtleCanvas::framebufferPixelAt(const QPointF& widget_point) const {
    const QRectF target = framebufferTarget();
    if (target.isEmpty() || target.width() <= 0.0 || target.height() <= 0.0) {
        return {-1.0, -1.0};
    }
    const double scale_x = target.width() / framebuffer_.width();
    const double scale_y = target.height() / framebuffer_.height();
    return {(widget_point.x() - target.left()) / scale_x,
            (widget_point.y() - target.top()) / scale_y};
}

void TurtleCanvas::paintEvent(QPaintEvent* /*event*/) {
    QPainter painter{this};
    painter.fillRect(rect(), background_);
    if (framebufferActive()) {
        // A screen has no grid and no turtle marker: both are statements about
        // a coordinate system the program is not using.
        paintFramebuffer(painter);
        return;
    }
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
    if (framebufferActive()) {
        // The screen is already pixels: scaling it is a nearest-neighbour
        // multiplication, not a re-render, so the exported picture is the one
        // the program drew and not an antialiased impression of it.
        const QSize target{std::max(1, qRound(framebuffer_.width() * safe)),
                           std::max(1, qRound(framebuffer_.height() * safe))};
        return framebuffer_
            .scaled(target, Qt::IgnoreAspectRatio, Qt::FastTransformation)
            .convertToFormat(QImage::Format_ARGB32_Premultiplied);
    }
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
    // SVG is a different writer, not a different picture: `QSvgGenerator` is a
    // `QPaintDevice`, so `paintInk` paints into it exactly as it paints into the
    // widget or the image and the export cannot drift from the screen. The size
    // is the on-screen one (a vector file has no pixels to multiply), and the
    // view box makes one user unit one widget pixel.
    if (path.endsWith(QStringLiteral(".svg"), Qt::CaseInsensitive)) {
#ifdef TIME_WARP_HAVE_SVG
        const QSize canvas{std::max(1, width()), std::max(1, height())};
        // Through `QSaveFile` rather than a bare path: a failed write must not
        // leave a half-written file where a good drawing used to be.
        QSaveFile file{path};
        if (!file.open(QIODevice::WriteOnly)) {
            if (error != nullptr) {
                *error = file.errorString();
            }
            return false;
        }
        {
            QSvgGenerator generator;
            generator.setOutputDevice(&file);
            generator.setSize(canvas);
            generator.setViewBox(QRect{QPoint{0, 0}, canvas});
            generator.setTitle(tr("Time Warp Studio drawing"));
            generator.setDescription(tr("Turtle graphics, exported as vectors"));

            QPainter painter{&generator};
            painter.setRenderHint(QPainter::Antialiasing, false);
            painter.fillRect(QRect{QPoint{0, 0}, canvas}, background_);
            if (framebufferActive()) {
                // A screen exports as an image: there are no vectors to write,
                // and an empty SVG would be the silent failure the export
                // exists to prevent.
                painter.drawImage(QRect{QPoint{0, 0}, canvas}, framebuffer_);
            } else {
                paintInk(painter, frameCount());
            }
        }
        // The generator flushes on destruction, which is why it has its own
        // scope above and why the commit comes after it.
        if (!file.commit()) {
            if (error != nullptr) {
                *error = file.errorString();
            }
            return false;
        }
        return true;
#else
        if (error != nullptr) {
            *error = tr("this build has no SVG support (Qt6::Svg was not found)");
        }
        return false;
#endif
    }

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
    int current_style = -1;
    const int count = std::min(frames, static_cast<int>(history_.size()));
    for (int entry_index = 0; entry_index < count; ++entry_index) {
        const HistoryEntry& entry = history_[entry_index];
        if (entry.kind == HistoryEntry::Kind::Stroke) {
            const Stroke& stroke = strokes_[entry.index];
            const QColor inked = paletteMapped(stroke.color);
            if (inked != current_color || stroke.width != current_width ||
                stroke.style != current_style) {
                current_color = inked;
                current_width = stroke.width;
                current_style = stroke.style;
                // Scaled by the view: a pen width is a world-space figure, so
                // otherwise a 4-unit line and a 400-unit line would look identical,
                // which defeats the point of setting a width at all.
                painter.setPen(penFor(inked, stroke.width, stroke.style));
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
        // A gradient and a filled polygon both carry a *second* colour; a fill
        // with no second colour is the stroke's own, which is what a plain
        // `end_fill` means.
        const QColor brush_colour = shape.color2.isValid() ? shape.color2 : shape.color;
        painter.setBrush(shape.filled ? QBrush{paletteMapped(brush_colour)}
                                      : QBrush{Qt::NoBrush});

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
            case CanvasCommandKind::DrawPolyline: {
                // A polygon: the fill of a `BeginFill`/`EndFill` pair, or a
                // closed polyline. Drawn as one path so the fill has no seams.
                QPainterPath path;
                if (shape.points.isEmpty()) {
                    break;
                }
                path.moveTo(toScreen(shape.points.first()));
                for (qsizetype index = 1; index < shape.points.size(); ++index) {
                    path.lineTo(toScreen(shape.points.at(index)));
                }
                if (shape.closed) {
                    path.closeSubpath();
                }
                painter.drawPath(path);
                break;
            }
            case CanvasCommandKind::BezierCurve: {
                if (shape.points.size() < 2) {
                    break;
                }
                QPainterPath path;
                path.moveTo(toScreen(shape.points.at(0)));
                if (shape.points.size() >= 4) {
                    // Cubic: start, two controls, end -- the shape a turtle's
                    // `bezier` emits, and the one the reference's
                    // `control_points` held.
                    path.cubicTo(toScreen(shape.points.at(1)),
                                 toScreen(shape.points.at(2)),
                                 toScreen(shape.points.at(3)));
                    for (qsizetype index = 4; index + 2 < shape.points.size(); index += 3) {
                        path.cubicTo(toScreen(shape.points.at(index)),
                                     toScreen(shape.points.at(index + 1)),
                                     toScreen(shape.points.at(index + 2)));
                    }
                } else if (shape.points.size() == 3) {
                    path.quadTo(toScreen(shape.points.at(1)),
                                toScreen(shape.points.at(2)));
                } else {
                    path.lineTo(toScreen(shape.points.at(1)));
                }
                painter.setBrush(Qt::NoBrush);
                painter.drawPath(path);
                break;
            }
            case CanvasCommandKind::DrawRectGradient:
            case CanvasCommandKind::DrawEllipseGradient: {
                const QRectF box{
                    toScreen(QPointF{shape.rect.left(), shape.rect.top()}),
                    toScreen(QPointF{shape.rect.right(), shape.rect.bottom()})};
                // The angle is degrees from the left edge, in world terms; the
                // screen flips y, so the y component of the direction is negated
                // -- the same one flip the projection does everywhere.
                const double radians = degreesToRadians(shape.a);
                const QPointF centre = box.center();
                const double reach = std::hypot(box.width(), box.height()) / 2.0;
                QLinearGradient gradient{
                    centre - QPointF{std::cos(radians), -std::sin(radians)} * reach,
                    centre + QPointF{std::cos(radians), -std::sin(radians)} * reach};
                gradient.setColorAt(0.0, paletteMapped(shape.color));
                gradient.setColorAt(1.0, paletteMapped(shape.color2));
                painter.setPen(Qt::NoPen);
                painter.setBrush(QBrush{gradient});
                if (shape.kind == CanvasCommandKind::DrawRectGradient) {
                    painter.drawRect(box.normalized());
                } else {
                    painter.drawEllipse(box.normalized());
                }
                break;
            }
            case CanvasCommandKind::PlaceSprite: {
                const auto found = sprites_.constFind(shape.text);
                if (found == sprites_.constEnd()) {
                    break;
                }
                // `closed` carries visibility: a hidden placement stays in the
                // display list, because hiding a sprite is not forgetting it.
                if (!shape.closed) {
                    break;
                }
                painter.setPen(Qt::NoPen);
                painter.drawImage(
                    QRectF{toScreen(shape.at), toScreen(shape.at) +
                                                   QPointF{found->image.width() * scale(),
                                                           found->image.height() * scale()}},
                    found->image);
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
