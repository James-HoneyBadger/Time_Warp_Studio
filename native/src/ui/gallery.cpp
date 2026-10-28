// SPDX-License-Identifier: MIT

#include "timewarp/ui/gallery.hpp"

#include <QApplication>
#include <QDir>
#include <QDirIterator>
#include <QFile>
#include <QFileInfo>
#include <QHBoxLayout>
#include <QIcon>
#include <QLabel>
#include <QListWidget>
#include <QPainter>
#include <QPalette>
#include <QPushButton>
#include <QTimer>
#include <QVBoxLayout>

#include <algorithm>
#include <optional>
#include <string>

#include "timewarp/graphics/recording_host.hpp"
#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/language.hpp"
#include "timewarp/scheme/machine.hpp"
#include "timewarp/ui/app_paths.hpp"

namespace timewarp::ui {

namespace {

constexpr int kThumbWidth = 168;
constexpr int kThumbHeight = 112;

[[nodiscard]] std::optional<lang::Language> languageForFile(const QString& path) {
    const QString suffix = QFileInfo{path}.suffix().toLower();
    for (const lang::Language language : lang::all_languages()) {
        for (const std::string_view extension : lang::extensions(language)) {
            const QString candidate = QString::fromLatin1(
                                          extension.data(),
                                          static_cast<int>(extension.size()))
                                          .toLower();
            if (suffix == candidate) {
                return language;
            }
        }
    }
    return std::nullopt;
}

[[nodiscard]] QColor colourOf(const lang::Rgb& rgb) {
    return QColor{rgb.red, rgb.green, rgb.blue};
}

/// Everything the recording touched, plus the origin, so a drawing that only goes
/// one way is still framed the way the SVG writer frames it.
struct Bounds {
    double min_x = 0.0;
    double max_x = 0.0;
    double min_y = 0.0;
    double max_y = 0.0;
    bool seen = false;

    void add(double x, double y) {
        if (!seen) {
            min_x = max_x = x;
            min_y = max_y = y;
            seen = true;
            return;
        }
        min_x = std::min(min_x, x);
        max_x = std::max(max_x, x);
        min_y = std::min(min_y, y);
        max_y = std::max(max_y, y);
    }
};

void paintRecording(QPainter& painter, const QRect& area, const graphics::RecordingHost& host) {
    Bounds bounds;
    bounds.add(0.0, 0.0);
    for (const graphics::RecordingHost::Segment& segment : host.segments()) {
        bounds.add(segment.x1, segment.y1);
        bounds.add(segment.x2, segment.y2);
    }
    for (const graphics::RecordingHost::Shape& shape : host.shapes()) {
        if (shape.kind == graphics::RecordingHost::Shape::Kind::Polygon) {
            for (const lang::Point& point : shape.points) {
                bounds.add(point.x, point.y);
            }
        } else if (shape.kind == graphics::RecordingHost::Shape::Kind::Circle) {
            bounds.add(shape.x - shape.w, shape.y - shape.w);
            bounds.add(shape.x + shape.w, shape.y + shape.w);
        } else if (shape.kind != graphics::RecordingHost::Shape::Kind::Text) {
            bounds.add(shape.x, shape.y);
            bounds.add(shape.x + shape.w, shape.y + shape.h);
        } else {
            bounds.add(shape.x, shape.y);
        }
    }

    const double margin = 8.0;
    const double span_x = std::max(1.0, bounds.max_x - bounds.min_x + 2 * margin);
    const double span_y = std::max(1.0, bounds.max_y - bounds.min_y + 2 * margin);
    const double scale = std::min(area.width() / span_x, area.height() / span_y);
    const double offset_x = area.x() + (area.width() - span_x * scale) / 2.0;
    const double offset_y = area.y() + (area.height() - span_y * scale) / 2.0;
    // The turtle's y grows upwards and a painter's grows downwards, which is the one
    // coordinate fact a thumbnail has to get right.
    const auto map_x = [&](double x) {
        return offset_x + (x - (bounds.min_x - margin)) * scale;
    };
    const auto map_y = [&](double y) {
        return offset_y + ((bounds.max_y + margin) - y) * scale;
    };

    for (const graphics::RecordingHost::Segment& segment : host.segments()) {
        QPen pen{colourOf(segment.colour)};
        pen.setWidthF(std::max(1.0, segment.width * scale));
        pen.setCapStyle(Qt::RoundCap);
        painter.setPen(pen);
        painter.drawLine(QPointF{map_x(segment.x1), map_y(segment.y1)},
                         QPointF{map_x(segment.x2), map_y(segment.y2)});
    }
    for (const graphics::RecordingHost::Shape& shape : host.shapes()) {
        QPen pen{colourOf(shape.stroke)};
        pen.setWidthF(std::max(1.0, shape.width * scale));
        painter.setPen(pen);
        painter.setBrush(shape.filled ? QBrush{colourOf(shape.fill)} : Qt::NoBrush);
        switch (shape.kind) {
            case graphics::RecordingHost::Shape::Kind::Circle:
                painter.drawEllipse(QPointF{map_x(shape.x), map_y(shape.y)},
                                    std::fabs(shape.w) * scale, std::fabs(shape.w) * scale);
                break;
            case graphics::RecordingHost::Shape::Kind::Rect:
                painter.drawRect(QRectF{QPointF{map_x(shape.x), map_y(shape.y + shape.h)},
                                        QPointF{map_x(shape.x + shape.w), map_y(shape.y)}});
                break;
            case graphics::RecordingHost::Shape::Kind::Point:
                painter.setBrush(QBrush{colourOf(shape.fill)});
                painter.drawEllipse(QPointF{map_x(shape.x), map_y(shape.y)},
                                    1.5 * scale, 1.5 * scale);
                break;
            case graphics::RecordingHost::Shape::Kind::Text: {
                QFont font = painter.font();
                font.setPixelSize(std::max(6, static_cast<int>(shape.font_size * scale)));
                painter.setFont(font);
                painter.setPen(colourOf(shape.fill));
                painter.drawText(QPointF{map_x(shape.x), map_y(shape.y)},
                                 QString::fromStdString(shape.text));
                break;
            }
            case graphics::RecordingHost::Shape::Kind::Polygon: {
                QPolygonF polygon;
                for (const lang::Point& point : shape.points) {
                    polygon << QPointF{map_x(point.x), map_y(point.y)};
                }
                painter.drawPolygon(polygon);
                break;
            }
        }
    }
}

}  // namespace

QImage GalleryDialog::renderThumbnail(const QString& path, const QSize& size) {
    QImage image{size, QImage::Format_ARGB32_Premultiplied};
    image.fill(QApplication::palette().color(QPalette::Base));

    const std::optional<lang::Language> language = languageForFile(path);
    QFile file{path};
    if (!language.has_value() || !file.open(QIODevice::ReadOnly)) {
        return image;
    }
    const std::string source = file.readAll().toStdString();

    // The same host the API and the compiled player use, so a thumbnail is a picture
    // of what the program draws rather than of a second implementation of drawing.
    graphics::RecordingHost host;
    if (*language == lang::Language::Scheme) {
        scheme::Arena arena;
        scheme::Machine machine{arena, host};
        (void)machine.execute(source);
    } else {
        lang::Interpreter interpreter{*language, host};
        interpreter.set_output_callback([](const std::string&) {});
        // A program that asks a question gets no answer rather than a stall.
        interpreter.set_input_callback([](const std::string&) { return std::string{}; });
        interpreter.load_program(source);
        interpreter.run();
    }

    QPainter painter{&image};
    painter.setRenderHints(QPainter::Antialiasing | QPainter::TextAntialiasing);
    paintRecording(painter, image.rect().adjusted(4, 4, -4, -4), host);
    if (!host.drew()) {
        // A program with no ink gets a card rather than a blank square: the gallery
        // is a list of what is there, and "this one prints" is a true answer.
        painter.setPen(QApplication::palette().color(QPalette::Mid));
        QFont font = painter.font();
        font.setPixelSize(13);
        painter.setFont(font);
        painter.drawText(image.rect().adjusted(8, 8, -8, -8),
                         Qt::AlignCenter | Qt::TextWordWrap,
                         QString::fromStdString(std::string{lang::display_name(*language)}));
    }
    return image;
}

GalleryDialog::GalleryDialog(QWidget* parent) : QDialog(parent) {
    setObjectName(QStringLiteral("examplesGallery"));
    setWindowTitle(tr("Examples Gallery"));
    resize(760, 560);

    auto* layout = new QVBoxLayout{this};
    auto* hint = new QLabel{
        tr("Every program that ships with the Studio. Double-click one to open it."), this};
    hint->setWordWrap(true);
    layout->addWidget(hint);

    list_ = new QListWidget{this};
    list_->setObjectName(QStringLiteral("galleryList"));
    list_->setViewMode(QListView::IconMode);
    list_->setIconSize(QSize{kThumbWidth, kThumbHeight});
    list_->setGridSize(QSize{kThumbWidth + 24, kThumbHeight + 44});
    list_->setResizeMode(QListView::Adjust);
    list_->setMovement(QListView::Static);
    list_->setWordWrap(true);
    layout->addWidget(list_, 1);

    auto* buttons = new QHBoxLayout;
    buttons->addStretch(1);
    open_ = new QPushButton{tr("Open"), this};
    open_->setObjectName(QStringLiteral("galleryOpen"));
    open_->setDefault(true);
    auto* close = new QPushButton{tr("Close"), this};
    close->setObjectName(QStringLiteral("galleryClose"));
    buttons->addWidget(open_);
    buttons->addWidget(close);
    layout->addLayout(buttons);

    connect(close, &QPushButton::clicked, this, &QDialog::reject);
    connect(open_, &QPushButton::clicked, this, [this] { choose(list_->currentItem()); });
    connect(list_, &QListWidget::itemDoubleClicked, this,
            [this](QListWidgetItem* item) { choose(item); });

    const QString root = examplesDir();
    if (!root.isEmpty()) {
        QDirIterator it{root, QDir::Files, QDirIterator::Subdirectories};
        QStringList paths;
        while (it.hasNext()) {
            const QString path = it.next();
            if (languageForFile(path).has_value()) {
                paths.append(path);
            }
        }
        paths.sort();
        for (const QString& path : paths) {
            auto* item = new QListWidgetItem{list_};
            item->setText(QFileInfo{path}.fileName());
            item->setToolTip(QDir::toNativeSeparators(path));
            item->setData(Qt::UserRole, path);
            item->setTextAlignment(Qt::AlignHCenter);
            pending_.append(path);
        }
    }

    // One batch per event-loop turn: the window is on screen before the first
    // program runs, and a slow one cannot freeze the gallery.
    timer_ = new QTimer{this};
    timer_->setObjectName(QStringLiteral("galleryTimer"));
    timer_->setInterval(0);
    connect(timer_, &QTimer::timeout, this, [this] { renderNextBatch(); });
    timer_->start();

    open_->setEnabled(list_->currentItem() != nullptr);
    connect(list_, &QListWidget::itemSelectionChanged, this, [this] {
        open_->setEnabled(list_->currentItem() != nullptr);
    });
}

int GalleryDialog::count() const { return list_ == nullptr ? 0 : list_->count(); }

QStringList GalleryDialog::programs() const {
    QStringList paths;
    if (list_ == nullptr) {
        return paths;
    }
    for (int row = 0; row < list_->count(); ++row) {
        paths.append(list_->item(row)->data(Qt::UserRole).toString());
    }
    return paths;
}

void GalleryDialog::renderNextBatch() {
    int rendered = 0;
    while (rendered < kBatch && !pending_.isEmpty()) {
        const QString path = pending_.takeFirst();
        const QImage image = renderThumbnail(path, QSize{kThumbWidth, kThumbHeight});
        for (int row = 0; row < list_->count(); ++row) {
            QListWidgetItem* item = list_->item(row);
            if (item->data(Qt::UserRole).toString() == path) {
                item->setIcon(QIcon{QPixmap::fromImage(image)});
                break;
            }
        }
        ++rendered;
    }
    if (pending_.isEmpty()) {
        timer_->stop();
    }
}

void GalleryDialog::choose(QListWidgetItem* item) {
    if (item == nullptr) {
        return;
    }
    chosen_ = item->data(Qt::UserRole).toString();
    emit programChosen(chosen_);
    accept();
}

}  // namespace timewarp::ui
