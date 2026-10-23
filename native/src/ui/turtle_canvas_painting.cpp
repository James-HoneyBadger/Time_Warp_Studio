// SPDX-License-Identifier: MIT
//
// The painter: what the widget draws for the display list, the grid, the turtle marker,
// and the two exports.

#include "timewarp/core/qt_gui_compat.hpp"
#include "timewarp/core/qt_compat.hpp"
#include "timewarp/ui/turtle_canvas.hpp"

#include "turtle_canvas_internal.hpp"

#include <algorithm>
#include <cmath>
#include <iterator>
#include <numbers>

#include <QFont>
#include <QImage>
#include <QImageWriter>
#include <QKeyEvent>
#include <QMouseEvent>
#include <QPaintEvent>
#include <QPainter>
#include <QPainterPath>
#include <QResizeEvent>
#include <QSaveFile>
#include <QWheelEvent>

#ifdef TIME_WARP_HAVE_SVG
#include <QSvgGenerator>
#endif

#include "timewarp/canvas/command.hpp"

namespace timewarp::ui {

using namespace canvas_detail;
using timewarp::canvas::CanvasCommandKind;

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
