// SPDX-License-Identifier: MIT
//
// The commands a program's drawing arrives as, and the geometry each one appends to
// the display list. The display list, not the pixels, is what the log, the scrubber
// and an export read.

#include "timewarp/ui/turtle_canvas.hpp"

#include "turtle_canvas_internal.hpp"

#include <algorithm>
#include <cmath>

#include <QImage>
#include <QPainter>

#ifdef TIME_WARP_HAVE_SVG
#endif

#include "timewarp/canvas/command.hpp"

namespace timewarp::ui {

using namespace canvas_detail;
using timewarp::canvas::CanvasCommandKind;

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

namespace {

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

}  // namespace timewarp::ui
