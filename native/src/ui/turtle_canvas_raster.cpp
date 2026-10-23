// SPDX-License-Identifier: MIT
//
// Screen mode: the framebuffer a `SCREEN` statement allocates, and the commands that
// write pixels into it.

#include "timewarp/core/qt_compat.hpp"
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

}  // namespace timewarp::ui
