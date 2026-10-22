// SPDX-License-Identifier: MIT

#include "timewarp/ui/panels.hpp"

#include "timewarp/core/qt_compat.hpp"

#include <algorithm>
#include <cmath>
#include <iterator>
#include <numbers>
#include <utility>

#include <QComboBox>
#include <QDialogButtonBox>
#include <QEvent>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QPainter>
#include <QPainterPath>
#include <QProgressBar>
#include <QPushButton>
#include <QRadialGradient>
#include <QResizeEvent>
#include <QSlider>
#include <QSpinBox>
#include <QTableWidget>
#include <QTextBrowser>
#include <QTimer>
#include <QVector>
#include <QVBoxLayout>

namespace timewarp::ui {

// ---------------------------------------------------------------------------
// CrtOverlay
// ---------------------------------------------------------------------------

CrtOverlay::CrtOverlay(QWidget* target) : QWidget(target), target_(target) {
    setObjectName(QStringLiteral("crtOverlay"));
    // An overlay that eats clicks is worse than no overlay: every control
    // underneath would become dead.
    setAttribute(Qt::WA_TransparentForMouseEvents, true);
    setAttribute(Qt::WA_NoSystemBackground, true);
    setFocusPolicy(Qt::NoFocus);

    if (target_ != nullptr) {
        target_->installEventFilter(this);
        syncToTarget();
    }
    hide();
}

void CrtOverlay::setSettings(const Settings& settings) {
    settings_.scanlines = std::clamp(settings.scanlines, 0, 100);
    settings_.vignette = std::clamp(settings.vignette, 0, 100);
    settings_.curvature = std::clamp(settings.curvature, 0, 100);
    settings_.flicker = std::clamp(settings.flicker, 0, 100);
    settings_.phosphor = std::clamp(settings.phosphor, 0, 100);
    settings_.phosphor_colour = settings.phosphor_colour;

    // The flicker clock runs only while there is flicker to paint: a timer ticking
    // over a still surface is battery spent on nothing.
    if (settings_.flicker > 0) {
        if (flicker_timer_ == nullptr) {
            flicker_timer_ = new QTimer{this};
            flicker_timer_->setObjectName(QStringLiteral("crtFlickerTimer"));
            flicker_timer_->setInterval(80);
            connect(flicker_timer_, &QTimer::timeout, this, [this] {
                flicker_phase_ = (flicker_phase_ + 1) % 12;
                update();
            });
        }
        if (!flicker_timer_->isActive()) {
            flicker_timer_->start();
        }
    } else if (flicker_timer_ != nullptr) {
        flicker_timer_->stop();
        flicker_phase_ = 0;
    }
    update();
}

void CrtOverlay::setScanlineIntensity(int percent) {
    Settings updated = settings_;
    updated.scanlines = percent;
    setSettings(updated);
}

int CrtOverlay::scanlineIntensity() const noexcept { return settings_.scanlines; }

QWidget* CrtOverlay::target() const noexcept { return target_; }

bool CrtOverlay::eventFilter(QObject* watched, QEvent* event) {
    if (watched == target_) {
        if (event->type() == QEvent::Resize) {
            syncToTarget();
        } else if (event->type() == QEvent::Destroy) {
            // The overlay is a child of its target, so this is a formality -- but
            // a dangling pointer here would outlive the run and be read by
            // `target()`.
            target_ = nullptr;
        }
    }
    return QWidget::eventFilter(watched, event);
}

void CrtOverlay::syncToTarget() {
    if (target_ == nullptr) {
        return;
    }
    setGeometry(target_->rect());
    // Children are painted in insertion order, and the overlay is created last,
    // so it is already on top -- but a widget created later would cover it.
    raise();
}

void CrtOverlay::paintEvent(QPaintEvent* /*event*/) {
    QPainter painter{this};
    painter.setRenderHint(QPainter::Antialiasing, true);
    paintPhosphor(painter);
    paintScanlines(painter);
    paintVignette(painter);
    paintCurvature(painter);
}

void CrtOverlay::paintScanlines(QPainter& painter) {
    if (settings_.scanlines <= 0) {
        return;
    }
    // Deliberately understated: a full-strength scanline layer over text is
    // unreadable at 100%. The divisor keeps the effect at the level the Python's
    // default 0.15 intensity produced.
    const int alpha = (settings_.scanlines * 255) / 100 / 3;
    if (alpha <= 0) {
        return;
    }
    painter.setPen(Qt::NoPen);
    painter.setBrush(QColor{0, 0, 0, alpha});
    for (int y = 0; y < height(); y += 2) {
        painter.drawRect(0, y, width(), 1);
    }
}

void CrtOverlay::paintVignette(QPainter& painter) {
    if (settings_.vignette <= 0 || width() <= 0 || height() <= 0) {
        return;
    }
    const int alpha = (settings_.vignette * 255) / 100 / 2;
    if (alpha <= 0) {
        return;
    }
    // A radial wash from a clear centre: the tube's face falls off at the corners.
    QRadialGradient gradient{QPointF{width() / 2.0, height() / 2.0},
                             std::hypot(width(), height()) / 2.0};
    gradient.setColorAt(0.55, QColor{0, 0, 0, 0});
    gradient.setColorAt(1.0, QColor{0, 0, 0, alpha});
    painter.setPen(Qt::NoPen);
    painter.setBrush(gradient);
    painter.drawRect(rect());
}

void CrtOverlay::paintCurvature(QPainter& painter) {
    if (settings_.curvature <= 0) {
        return;
    }
    const int alpha = (settings_.curvature * 255) / 100 / 2;
    if (alpha <= 0) {
        return;
    }
    // The bezel: a rounded border inset by a few pixels, with the outside of the
    // radius darkened. Anything more would be a claim to distort the window.
    const double inset = std::max(2.0, std::min(width(), height()) * 0.02);
    const QRectF face = QRectF{rect()}.adjusted(inset, inset, -inset, -inset);
    const double radius = std::min(face.width(), face.height()) * 0.12;
    QPainterPath outer;
    outer.addRect(rect());
    QPainterPath inner;
    inner.addRoundedRect(face, radius, radius);
    painter.setPen(Qt::NoPen);
    painter.setBrush(QColor{0, 0, 0, alpha});
    painter.drawPath(outer.subtracted(inner));
}

void CrtOverlay::paintPhosphor(QPainter& painter) {
    if (settings_.phosphor <= 0) {
        return;
    }
    const int alpha = (settings_.phosphor * 255) / 100 / 6;
    if (alpha <= 0) {
        return;
    }
    QColor tint = settings_.phosphor_colour;
    tint.setAlpha(alpha);
    // Flicker rides on the tint: the same wash, a little stronger on some frames.
    // Twelve steps of a shallow sine rather than a random level, so it breathes
    // instead of strobing.
    if (settings_.flicker > 0) {
        const double wave = std::sin(flicker_phase_ * std::numbers::pi / 6.0);
        const int extra = static_cast<int>(wave * (settings_.flicker * 255) / 100 / 12);
        tint.setAlpha(std::clamp(alpha + extra, 0, 255));
    }
    painter.setPen(Qt::NoPen);
    painter.setBrush(tint);
    painter.drawRect(rect());
}

// ---------------------------------------------------------------------------
// CrtSettingsDialog
// ---------------------------------------------------------------------------

CrtSettingsDialog::CrtSettingsDialog(const CrtOverlay::Settings& start, QWidget* parent)
    : QDialog(parent), settings_(start) {
    setObjectName(QStringLiteral("crtSettingsDialog"));
    setWindowTitle(tr("CRT Settings"));
    setModal(true);

    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(12, 12, 12, 12);
    layout->setSpacing(6);

    // One row per effect, each with a live number: a slider without its value is a
    // control a user cannot describe to anybody else.
    const auto addRow = [this, layout](const QString& object, const QString& label,
                                       int value, int CrtOverlay::Settings::*field) {
        auto* row = new QHBoxLayout;
        auto* name = new QLabel{label, this};
        name->setMinimumWidth(110);
        row->addWidget(name);
        auto* slider = new QSlider{Qt::Horizontal, this};
        slider->setObjectName(object);
        slider->setRange(0, 100);
        slider->setValue(value);
        row->addWidget(slider, 1);
        auto* number = new QLabel{QString::number(value), this};
        number->setMinimumWidth(32);
        number->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
        row->addWidget(number);
        layout->addLayout(row);
        connect(slider, &QSlider::valueChanged, this, [this, number, field](int next) {
            number->setText(QString::number(next));
            settings_.*(field) = next;
            emit previewRequested(settings_);
        });
    };

    addRow(QStringLiteral("crtScanlines"), tr("Scanlines"), settings_.scanlines,
           &CrtOverlay::Settings::scanlines);
    addRow(QStringLiteral("crtVignette"), tr("Vignette"), settings_.vignette,
           &CrtOverlay::Settings::vignette);
    addRow(QStringLiteral("crtCurvature"), tr("Curvature"), settings_.curvature,
           &CrtOverlay::Settings::curvature);
    addRow(QStringLiteral("crtFlicker"), tr("Flicker"), settings_.flicker,
           &CrtOverlay::Settings::flicker);
    addRow(QStringLiteral("crtPhosphor"), tr("Phosphor tint"), settings_.phosphor,
           &CrtOverlay::Settings::phosphor);

    auto* buttons = new QDialogButtonBox{QDialogButtonBox::Ok | QDialogButtonBox::Cancel, this};
    buttons->setObjectName(QStringLiteral("crtSettingsButtons"));
    connect(buttons, &QDialogButtonBox::accepted, this, &QDialog::accept);
    connect(buttons, &QDialogButtonBox::rejected, this, &QDialog::reject);
    layout->addWidget(buttons);
}

}  // namespace timewarp::ui
