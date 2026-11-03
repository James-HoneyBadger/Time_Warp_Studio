// SPDX-License-Identifier: MIT

#include "timewarp/ui/icons.hpp"

#include <QApplication>
#include <QGuiApplication>
#include <QHash>
#include <QScreen>
#include <QIconEngine>
#include <QPainter>
#include <QPainterPath>
#include <QPixmap>

#include <algorithm>
#include <cmath>

namespace timewarp::ui::icons {
namespace {

/// The grid every glyph is drawn on. A glyph is authored in these units and scaled to
/// whatever size is asked for, so the stroke weight stays proportional and a 16px icon
/// and a 32px one are the same drawing.
constexpr qreal kGrid = 16.0;
constexpr qreal kStroke = 1.5;

void stroke(QPainter& painter, const QPainterPath& path) {
    QPen pen{painter.pen()};
    pen.setWidthF(kStroke);
    pen.setCapStyle(Qt::RoundCap);
    pen.setJoinStyle(Qt::RoundJoin);
    painter.setPen(pen);
    painter.setBrush(Qt::NoBrush);
    painter.drawPath(path);
}

QPainterPath line(qreal x1, qreal y1, qreal x2, qreal y2) {
    QPainterPath path;
    path.moveTo(x1, y1);
    path.lineTo(x2, y2);
    return path;
}

QPainterPath polyline(std::initializer_list<QPointF> points) {
    QPainterPath path;
    bool first = true;
    for (const QPointF& point : points) {
        if (first) {
            path.moveTo(point);
            first = false;
        } else {
            path.lineTo(point);
        }
    }
    return path;
}

QPainterPath rounded_rect(qreal x, qreal y, qreal w, qreal h, qreal r) {
    QPainterPath path;
    path.addRoundedRect(QRectF{x, y, w, h}, r, r);
    return path;
}

QPainterPath folder_body() {
    QPainterPath path;
    path.moveTo(2.5, 12.5);
    path.lineTo(2.5, 4.5);
    path.lineTo(6.5, 4.5);
    path.lineTo(8.0, 6.5);
    path.lineTo(13.5, 6.5);
    path.lineTo(13.5, 12.5);
    path.closeSubpath();
    return path;
}

QPainterPath page_body() {
    QPainterPath path;
    path.moveTo(4.5, 2.5);
    path.lineTo(9.5, 2.5);
    path.lineTo(11.5, 4.5);
    path.lineTo(11.5, 13.5);
    path.lineTo(4.5, 13.5);
    path.closeSubpath();
    return path;
}

void draw(Glyph id, QPainter& painter, const QColor& tint) {
    const auto fill = [&painter, &tint] {
        painter.setPen(Qt::NoPen);
        painter.setBrush(tint);
    };
    const auto outline = [&painter, &tint] {
        painter.setPen(QPen{tint, kStroke});
        painter.setBrush(Qt::NoBrush);
    };
    outline();
    switch (id) {
        case Glyph::None:
            break;
        case Glyph::New: {
            stroke(painter, page_body());
            stroke(painter, polyline({{6.5, 9.0}, {9.5, 9.0}}));
            stroke(painter, polyline({{8.0, 7.5}, {8.0, 10.5}}));
            break;
        }
        case Glyph::Open:
            stroke(painter, folder_body());
            break;
        case Glyph::Save: {
            stroke(painter, polyline({{8.0, 2.5}, {8.0, 9.0}}));
            stroke(painter, polyline({{5.5, 6.5}, {8.0, 9.5}, {10.5, 6.5}}));
            stroke(painter, polyline({{3.0, 10.5}, {3.0, 13.0}, {13.0, 13.0}, {13.0, 10.5}}));
            break;
        }
        case Glyph::Run: {
            QPainterPath path;
            path.moveTo(5.0, 3.5);
            path.lineTo(12.0, 8.0);
            path.lineTo(5.0, 12.5);
            path.closeSubpath();
            fill();
            painter.drawPath(path);
            outline();
            break;
        }
        // Continue is Run resumed at a breakpoint, so it carries the bar it resumes
        // from; two identical triangles in one toolbar say nothing.
        case Glyph::Continue: {
            QPainterPath path;
            path.moveTo(6.5, 3.5);
            path.lineTo(12.5, 8.0);
            path.lineTo(6.5, 12.5);
            path.closeSubpath();
            fill();
            painter.drawPath(path);
            painter.drawRoundedRect(QRectF{3.5, 3.5, 1.8, 9.0}, 0.7, 0.7);
            outline();
            break;
        }
        case Glyph::Pause: {
            fill();
            painter.drawRoundedRect(QRectF{4.5, 3.5, 2.5, 9.0}, 0.8, 0.8);
            painter.drawRoundedRect(QRectF{9.0, 3.5, 2.5, 9.0}, 0.8, 0.8);
            outline();
            break;
        }
        case Glyph::Stop: {
            fill();
            painter.drawRoundedRect(QRectF{4.5, 4.5, 7.0, 7.0}, 1.6, 1.6);
            outline();
            break;
        }
        case Glyph::Step: {
            stroke(painter, polyline({{8.0, 3.0}, {8.0, 9.5}}));
            stroke(painter, polyline({{5.5, 7.0}, {8.0, 10.0}, {10.5, 7.0}}));
            stroke(painter, line(4.0, 12.5, 12.0, 12.5));
            break;
        }
        case Glyph::Trace: {
            stroke(painter, polyline({{3.0, 4.5}, {12.5, 4.5}}));
            stroke(painter, polyline({{10.0, 2.0}, {12.5, 4.5}, {10.0, 7.0}}));
            stroke(painter, line(3.0, 9.0, 8.0, 9.0));
            stroke(painter, line(3.0, 12.5, 8.0, 12.5));
            break;
        }
        case Glyph::Debug: {
            QPainterPath body;
            body.addEllipse(QPointF{8.0, 9.0}, 3.4, 4.0);
            stroke(painter, body);
            QPainterPath head;
            head.addEllipse(QPointF{8.0, 4.0}, 1.8, 1.8);
            stroke(painter, head);
            stroke(painter, line(8.0, 2.2, 8.0, 2.6));
            stroke(painter, line(3.4, 5.6, 5.6, 6.8));
            stroke(painter, line(12.6, 5.6, 10.4, 6.8));
            stroke(painter, line(3.0, 9.2, 4.7, 9.2));
            stroke(painter, line(13.0, 9.2, 11.3, 9.2));
            stroke(painter, line(3.4, 12.8, 5.8, 11.4));
            stroke(painter, line(12.6, 12.8, 10.2, 11.4));
            break;
        }
        case Glyph::Snippets: {
            stroke(painter, polyline({{6.0, 4.0}, {2.8, 8.0}, {6.0, 12.0}}));
            stroke(painter, polyline({{10.0, 4.0}, {13.2, 8.0}, {10.0, 12.0}}));
            break;
        }
        case Glyph::ClearOutput: {
            stroke(painter, line(3.0, 5.0, 13.0, 5.0));
            stroke(painter, rounded_rect(4.8, 5.8, 6.4, 7.4, 1.2));
            stroke(painter, line(6.8, 3.4, 9.2, 3.4));
            break;
        }
        case Glyph::ClearCanvas: {
            stroke(painter, rounded_rect(2.8, 2.8, 10.4, 10.4, 1.6));
            stroke(painter, line(4.6, 11.4, 11.4, 4.6));
            break;
        }
        case Glyph::TextScale: {
            stroke(painter, polyline({{8.0, 3.0}, {8.0, 12.0}}));
            stroke(painter, polyline({{5.0, 6.0}, {8.0, 2.8}, {11.0, 6.0}}));
            stroke(painter, line(4.0, 13.4, 12.0, 13.4));
            break;
        }
        case Glyph::Settings: {
            stroke(painter, line(2.6, 4.6, 13.4, 4.6));
            stroke(painter, line(2.6, 8.0, 13.4, 8.0));
            stroke(painter, line(2.6, 11.4, 13.4, 11.4));
            fill();
            painter.drawEllipse(QPointF{5.6, 4.6}, 1.6, 1.6);
            painter.drawEllipse(QPointF{10.4, 8.0}, 1.6, 1.6);
            painter.drawEllipse(QPointF{6.8, 11.4}, 1.6, 1.6);
            outline();
            break;
        }
        case Glyph::Console: {
            stroke(painter, rounded_rect(2.4, 3.4, 11.2, 9.2, 1.6));
            stroke(painter, polyline({{5.2, 6.6}, {7.2, 8.0}, {5.2, 9.4}}));
            stroke(painter, line(8.8, 9.4, 11.0, 9.4));
            break;
        }
        case Glyph::Graphics: {
            stroke(painter, rounded_rect(2.4, 3.4, 11.2, 9.2, 1.6));
            QPainterPath curve;
            curve.moveTo(4.6, 10.4);
            curve.cubicTo(6.4, 5.2, 9.6, 11.6, 11.4, 6.2);
            stroke(painter, curve);
            break;
        }
        case Glyph::Variables: {
            stroke(painter, polyline({{6.0, 3.2}, {4.2, 3.2}, {4.2, 8.0}, {2.8, 8.0},
                                      {4.2, 8.0}, {4.2, 12.8}, {6.0, 12.8}}));
            stroke(painter, polyline({{10.0, 3.2}, {11.8, 3.2}, {11.8, 8.0}, {13.2, 8.0},
                                      {11.8, 8.0}, {11.8, 12.8}, {10.0, 12.8}}));
            fill();
            painter.drawEllipse(QPointF{8.0, 8.0}, 1.1, 1.1);
            outline();
            break;
        }
        case Glyph::Telemetry: {
            stroke(painter, polyline({{2.6, 9.0}, {5.6, 9.0}, {7.2, 4.8}, {9.4, 11.8},
                                      {11.0, 9.0}, {13.4, 9.0}}));
            break;
        }
        case Glyph::Lessons: {
            QPainterPath book;
            book.moveTo(8.0, 4.6);
            book.cubicTo(6.4, 3.4, 4.2, 3.2, 2.8, 4.2);
            book.lineTo(2.8, 11.8);
            book.cubicTo(4.2, 11.0, 6.4, 11.2, 8.0, 12.2);
            book.cubicTo(9.6, 11.2, 11.8, 11.0, 13.2, 11.8);
            book.lineTo(13.2, 4.2);
            book.cubicTo(11.8, 3.2, 9.6, 3.4, 8.0, 4.6);
            stroke(painter, book);
            stroke(painter, line(8.0, 4.6, 8.0, 12.2));
            break;
        }
        case Glyph::Classroom: {
            QPainterPath head;
            head.addEllipse(QPointF{6.0, 5.2}, 1.9, 1.9);
            stroke(painter, head);
            QPainterPath shoulders;
            shoulders.moveTo(2.8, 12.4);
            shoulders.cubicTo(2.8, 9.6, 9.2, 9.6, 9.2, 12.4);
            stroke(painter, shoulders);
            QPainterPath second;
            second.addEllipse(QPointF{11.4, 6.6}, 1.4, 1.4);
            stroke(painter, second);
            QPainterPath other;
            other.moveTo(9.8, 12.4);
            other.cubicTo(10.0, 10.4, 13.6, 10.4, 13.6, 12.4);
            stroke(painter, other);
            break;
        }
        case Glyph::Projects: {
            stroke(painter, folder_body());
            fill();
            painter.drawEllipse(QPointF{10.4, 9.6}, 1.5, 1.5);
            outline();
            break;
        }
        case Glyph::Terminal: {
            stroke(painter, rounded_rect(2.4, 3.4, 11.2, 9.2, 1.6));
            stroke(painter, polyline({{5.4, 6.4}, {7.6, 8.0}, {5.4, 9.6}}));
            stroke(painter, line(9.2, 9.6, 11.2, 9.6));
            break;
        }
        case Glyph::Database: {
            QPainterPath top;
            top.addEllipse(QPointF{8.0, 4.8}, 4.6, 1.9);
            stroke(painter, top);
            stroke(painter, polyline({{3.4, 4.8}, {3.4, 11.4}}));
            stroke(painter, polyline({{12.6, 4.8}, {12.6, 11.4}}));
            QPainterPath bottom;
            bottom.moveTo(3.4, 11.4);
            bottom.cubicTo(4.6, 13.0, 11.4, 13.0, 12.6, 11.4);
            stroke(painter, bottom);
            QPainterPath middle;
            middle.moveTo(3.4, 8.1);
            middle.cubicTo(4.6, 9.7, 11.4, 9.7, 12.6, 8.1);
            stroke(painter, middle);
            break;
        }
        case Glyph::Reference: {
            stroke(painter, rounded_rect(3.4, 2.8, 9.2, 10.4, 1.4));
            stroke(painter, line(6.2, 2.8, 6.2, 13.2));
            stroke(painter, line(8.2, 5.6, 10.6, 5.6));
            stroke(painter, line(8.2, 8.0, 10.6, 8.0));
            break;
        }
        case Glyph::Notebook: {
            stroke(painter, rounded_rect(3.0, 2.8, 10.0, 10.4, 1.4));
            stroke(painter, line(6.0, 2.8, 6.0, 13.2));
            stroke(painter, line(7.8, 5.8, 11.0, 5.8));
            stroke(painter, line(7.8, 8.4, 11.0, 8.4));
            break;
        }
        case Glyph::Gallery: {
            stroke(painter, rounded_rect(2.8, 2.8, 4.6, 4.6, 1.0));
            stroke(painter, rounded_rect(8.6, 2.8, 4.6, 4.6, 1.0));
            stroke(painter, rounded_rect(2.8, 8.6, 4.6, 4.6, 1.0));
            stroke(painter, rounded_rect(8.6, 8.6, 4.6, 4.6, 1.0));
            break;
        }
        case Glyph::Folder:
            stroke(painter, folder_body());
            break;
        case Glyph::File:
            stroke(painter, page_body());
            break;
        case Glyph::Close:
            stroke(painter, line(4.6, 4.6, 11.4, 11.4));
            stroke(painter, line(11.4, 4.6, 4.6, 11.4));
            break;
    }
}

/// Pixmaps by glyph, size and tint. A toolbar repaints its icons on every hover, and
/// drawing twenty paths each time is work the interface should not repeat.
QHash<QString, QPixmap>& pixmapCache() {
    static QHash<QString, QPixmap> cache;
    return cache;
}

QPixmap render(Glyph id, int size, const QColor& tint) {
    const QScreen* screen = QGuiApplication::primaryScreen();
    const qreal ratio = screen != nullptr ? screen->devicePixelRatio() : 1.0;
    const int pixels = std::max(1, static_cast<int>(std::lround(size * ratio)));
    const QString key = QStringLiteral("%1|%2|%3|%4")
                            .arg(static_cast<int>(id))
                            .arg(pixels)
                            .arg(tint.rgba())
                            .arg(ratio);

    const auto found = pixmapCache().constFind(key);
    if (found != pixmapCache().constEnd()) {
        return found.value();
    }

    QPixmap pixmap{pixels, pixels};
    pixmap.fill(Qt::transparent);
    if (id != Glyph::None && tint.alpha() > 0) {
        QPainter painter{&pixmap};
        painter.setRenderHint(QPainter::Antialiasing, true);
        painter.scale(pixels / kGrid, pixels / kGrid);
        draw(id, painter, tint);
        painter.end();
    }
    pixmap.setDevicePixelRatio(ratio);

    // Bounded: the interface has a handful of glyphs and a handful of colours, but a
    // theme cycle could otherwise mint an entry per shade it passes through.
    if (pixmapCache().size() > 512) {
        pixmapCache().clear();
    }
    pixmapCache().insert(key, pixmap);
    return pixmap;
}

/// An icon that asks the palette for its colour each time Qt needs a pixmap, so a
/// theme change needs no rebuild anywhere.
class PaletteIconEngine final : public QIconEngine {
public:
    PaletteIconEngine(Glyph id, QPalette::ColorRole role) : id_{id}, role_{role} {}

    void paint(QPainter* painter, const QRect& rect, QIcon::Mode mode,
               QIcon::State state) override {
        const QColor tint = colour_for(mode, state);
        painter->save();
        painter->setRenderHint(QPainter::Antialiasing, true);
        const int side = std::min(rect.width(), rect.height());
        painter->translate(rect.x() + (rect.width() - side) / 2,
                           rect.y() + (rect.height() - side) / 2);
        painter->scale(side / kGrid, side / kGrid);
        draw(id_, *painter, tint);
        painter->restore();
    }

    [[nodiscard]] QPixmap pixmap(const QSize& size, QIcon::Mode mode,
                                 QIcon::State state) override {
        const int side = std::min(size.width(), size.height());
        return render(id_, side > 0 ? side : 16, colour_for(mode, state));
    }

    [[nodiscard]] QIconEngine* clone() const override {
        return new PaletteIconEngine{id_, role_};
    }

private:
    [[nodiscard]] QColor colour_for(QIcon::Mode mode, QIcon::State state) const {
        const QPalette palette = QApplication::palette();
        QColor colour = palette.color(role_);
        if (mode == QIcon::Selected) {
            colour = palette.color(QPalette::HighlightedText);
        } else if (mode == QIcon::Disabled) {
            colour.setAlphaF(0.38);
        }
        if (state == QIcon::On) {
            colour = palette.color(QPalette::Highlight);
        }
        return colour;
    }

    Glyph id_;
    QPalette::ColorRole role_;
};

class FixedIconEngine final : public QIconEngine {
public:
    FixedIconEngine(Glyph id, const QColor& tint) : id_{id}, tint_{tint} {}

    void paint(QPainter* painter, const QRect& rect, QIcon::Mode, QIcon::State) override {
        painter->save();
        painter->setRenderHint(QPainter::Antialiasing, true);
        timewarp::ui::icons::paint(*painter, id_, QRectF{rect}, tint_);
        painter->restore();
    }

    [[nodiscard]] QPixmap pixmap(const QSize& size, QIcon::Mode,
                                 QIcon::State) override {
        return render(id_, std::min(size.width(), size.height()), tint_);
    }

    [[nodiscard]] QIconEngine* clone() const override {
        return new FixedIconEngine{id_, tint_};
    }

private:
    Glyph id_;
    QColor tint_;
};

}  // namespace

QIcon glyph(Glyph id, QPalette::ColorRole role) {
    return QIcon{new PaletteIconEngine{id, role}};
}

QIcon glyph(Glyph id, const QColor& tint) {
    return QIcon{new FixedIconEngine{id, tint}};
}

void paint(QPainter& painter, Glyph id, const QRectF& bounds, const QColor& tint) {
    painter.save();
    painter.setRenderHint(QPainter::Antialiasing, true);
    const qreal side = std::min(bounds.width(), bounds.height());
    painter.translate(bounds.x() + (bounds.width() - side) / 2,
                      bounds.y() + (bounds.height() - side) / 2);
    painter.scale(side / kGrid, side / kGrid);
    draw(id, painter, tint);
    painter.restore();
}

}  // namespace timewarp::ui::icons
