// SPDX-License-Identifier: MIT

#include "timewarp/ui/tour.hpp"

#include <QEvent>
#include <QFrame>
#include <QHBoxLayout>
#include <QLabel>
#include <QMouseEvent>
#include <QPainter>
#include <QPushButton>
#include <QSettings>
#include <QShortcut>
#include <QVBoxLayout>

#include <algorithm>

namespace timewarp::ui {

namespace {

constexpr auto kSeenKey = "tour/seen";
constexpr auto kCoachPrefix = "tour/coach/";
constexpr int kCardWidth = 360;
constexpr int kMargin = 16;
constexpr int kGap = 10;

}  // namespace

TourOverlay::TourOverlay(QWidget* window) : QWidget(window), window_(window) {
    setObjectName(QStringLiteral("tourOverlay"));
    setAttribute(Qt::WA_NoSystemBackground);
    setGeometry(window->rect());

    card_ = new QFrame{this};
    card_->setObjectName(QStringLiteral("tourCard"));
    card_->setFrameShape(QFrame::StyledPanel);
    card_->setFrameShadow(QFrame::Raised);
    card_->setFixedWidth(kCardWidth);

    auto* layout = new QVBoxLayout{card_};
    layout->setContentsMargins(14, 12, 14, 12);
    layout->setSpacing(8);

    title_ = new QLabel{card_};
    title_->setObjectName(QStringLiteral("tourTitle"));
    QFont title_font = title_->font();
    title_font.setBold(true);
    title_font.setPointSize(title_font.pointSize() + 2);
    title_->setFont(title_font);

    body_ = new QLabel{card_};
    body_->setObjectName(QStringLiteral("tourBody"));
    body_->setWordWrap(true);
    body_->setMinimumWidth(kCardWidth - 40);

    progress_ = new QLabel{card_};
    progress_->setObjectName(QStringLiteral("tourProgress"));

    auto* buttons = new QHBoxLayout;
    buttons->setSpacing(8);
    skip_ = new QPushButton{tr("Skip"), card_};
    skip_->setObjectName(QStringLiteral("tourSkip"));
    back_ = new QPushButton{tr("Back"), card_};
    back_->setObjectName(QStringLiteral("tourBack"));
    next_ = new QPushButton{tr("Next"), card_};
    next_->setObjectName(QStringLiteral("tourNext"));
    buttons->addWidget(skip_);
    buttons->addStretch(1);
    buttons->addWidget(back_);
    buttons->addWidget(next_);

    layout->addWidget(title_);
    layout->addWidget(body_);
    layout->addWidget(progress_);
    layout->addLayout(buttons);

    connect(next_, &QPushButton::clicked, this, &TourOverlay::next);
    connect(back_, &QPushButton::clicked, this, &TourOverlay::back);
    connect(skip_, &QPushButton::clicked, this, &TourOverlay::dismiss);
    auto* escape = new QShortcut{QKeySequence{Qt::Key_Escape}, this};
    // Scoped to the overlay: Escape must keep its meaning in the window whenever
    // the card is not up.
    escape->setContext(Qt::WidgetWithChildrenShortcut);
    connect(escape, &QShortcut::activated, this, &TourOverlay::dismiss);

    hide();
    window_->installEventFilter(this);
}

void TourOverlay::startTour(const QList<TourStep>& steps) {
    if (steps.isEmpty()) {
        return;
    }
    steps_ = steps;
    index_ = 0;
    coach_mode_ = false;
    active_ = true;
    skip_->show();
    back_->show();
    progress_->show();
    next_->setText(tr("Next"));
    setGeometry(window_->rect());
    show();
    raise();
    showCurrentStep();
}

void TourOverlay::startCoachMark(const CoachMark& mark) {
    coach_mode_ = true;
    coach_text_ = mark.text;
    anchor_ = mark.objectName;
    active_ = true;
    skip_->hide();
    back_->hide();
    progress_->hide();
    next_->setText(tr("Got it"));
    setGeometry(window_->rect());
    show();
    raise();
    showCurrentStep();
}

void TourOverlay::next() {
    if (!active_) {
        return;
    }
    if (coach_mode_) {
        dismiss();
        return;
    }
    if (index_ + 1 >= steps_.size()) {
        dismiss();
        return;
    }
    ++index_;
    showCurrentStep();
}

void TourOverlay::back() {
    if (!active_ || coach_mode_ || index_ <= 0) {
        return;
    }
    --index_;
    showCurrentStep();
}

void TourOverlay::dismiss() {
    if (!active_) {
        return;
    }
    active_ = false;
    hide();
    if (coach_mode_) {
        coach_mode_ = false;
        emit coachDismissed();
    } else {
        emit tourFinished();
    }
}

void TourOverlay::showCurrentStep() {
    if (coach_mode_) {
        title_->setText(tr("Tip"));
        body_->setText(coach_text_);
        progress_->clear();
    } else {
        if (index_ < 0 || index_ >= steps_.size()) {
            dismiss();
            return;
        }
        const TourStep& step = steps_.at(index_);
        anchor_ = step.objectName;
        title_->setText(step.title);
        body_->setText(step.text);
        progress_->setText(tr("Step %1 of %2").arg(index_ + 1).arg(steps_.size()));
        back_->setEnabled(index_ > 0);
        next_->setText(index_ + 1 >= steps_.size() ? tr("Done") : tr("Next"));
        // Before measuring: the listener may raise the panel this step names.
        emit stepShown(index_);
    }
    layoutCard();
    update();
    next_->setFocus(Qt::OtherFocusReason);
}

void TourOverlay::layoutCard() {
    card_->setFixedWidth(std::min(kCardWidth, std::max(220, window_->width() - 2 * kMargin)));
    card_->adjustSize();
    const QSize size = card_->size();
    const QRect anchor = anchorRect();

    int x = 0;
    int y = 0;
    if (anchor.isEmpty()) {
        x = (window_->width() - size.width()) / 2;
        y = (window_->height() - size.height()) / 2;
    } else {
        x = anchor.center().x() - size.width() / 2;
        y = anchor.bottom() + kGap;
        if (y + size.height() > window_->height() - kMargin) {
            y = anchor.top() - size.height() - kGap;
        }
        if (y < kMargin) {
            y = std::max(kMargin, window_->height() - size.height() - kMargin);
        }
    }
    x = std::clamp(x, kMargin,
                   std::max(kMargin, window_->width() - size.width() - kMargin));
    card_->move(x, y);
}

QRect TourOverlay::anchorRect() const {
    if (window_ == nullptr || anchor_.isEmpty()) {
        return {};
    }
    QWidget* anchor = window_->findChild<QWidget*>(anchor_);
    if (anchor == nullptr || !anchor->isVisibleTo(window_)) {
        return {};
    }
    QRect rect{anchor->mapTo(window_, QPoint{0, 0}), anchor->size()};
    rect = rect.adjusted(-6, -6, 6, 6);
    return rect.intersected(window_->rect());
}

void TourOverlay::paintEvent(QPaintEvent* event) {
    Q_UNUSED(event);
    QPainter painter{this};
    const QColor dim{0, 0, 0, 120};
    const QRect hole = anchorRect();
    if (hole.isEmpty()) {
        painter.fillRect(rect(), dim);
        return;
    }
    // Four rectangles rather than a composition-mode hole: it works on every
    // backing store, including the offscreen platform the tests use.
    painter.fillRect(QRect{0, 0, width(), hole.top()}, dim);
    painter.fillRect(QRect{0, hole.bottom() + 1, width(), height() - hole.bottom() - 1}, dim);
    painter.fillRect(QRect{0, hole.top(), hole.left(), hole.height()}, dim);
    painter.fillRect(QRect{hole.right() + 1, hole.top(), width() - hole.right() - 1,
                           hole.height()},
                     dim);
    QPen pen{palette().color(QPalette::Highlight), 2};
    painter.setPen(pen);
    painter.drawRoundedRect(hole, 4, 4);
}

void TourOverlay::mousePressEvent(QMouseEvent* event) {
    // The point of the overlay is that the window below is not clickable until
    // the learner has seen the step.
    event->accept();
}

bool TourOverlay::eventFilter(QObject* watched, QEvent* event) {
    if (watched == window_) {
        if (event->type() == QEvent::Resize) {
            setGeometry(window_->rect());
            if (active_) {
                layoutCard();
            }
        } else if (event->type() == QEvent::Hide && active_) {
            // The window going away is not a completion: do not mark the tour
            // seen just because the application closed.
            active_ = false;
            hide();
        }
    }
    return QWidget::eventFilter(watched, event);
}

TourGuide::TourGuide(QWidget* window, QObject* parent)
    : QObject(parent), window_(window), overlay_(new TourOverlay{window}) {
    connect(overlay_, &TourOverlay::stepShown, this, &TourGuide::stepShown);
    connect(overlay_, &TourOverlay::tourFinished, this, [] {
        QSettings{}.setValue(kSeenKey, true);
    });
}

QList<TourStep> TourGuide::steps() {
    return {
        {QStringLiteral("runButton"), tr("Run the program"),
         tr("F9 or the Run button executes the document in the editor. The arrow "
            "beside it lists the last ten programs you ran.")},
        {QStringLiteral("turtleCanvas"), tr("Watch the canvas"),
         tr("Turtle programs draw here. Drag to pan, use the wheel to zoom, and "
            "Fit Drawing brings the whole picture back into view.")},
        {QStringLiteral("lessonPanel"), tr("Follow a lesson"),
         tr("Every language has two lessons in this tab. Each checkpoint runs "
            "your program and tells you exactly what is missing.")},
        {QStringLiteral("functionKeyBar"), tr("Find anything"),
         tr("F10 opens the command palette with every menu entry, filtered as "
            "you type. The function-key bar shows the rest of the shortcuts.")},
    };
}

QList<CoachMark> TourGuide::coachMarks() {
    return {
        {QStringLiteral("lessons"), QStringLiteral("lessonPanel"),
         tr("Every language has two lessons here; a checkpoint runs your program "
            "and says what is missing.")},
        {QStringLiteral("run-history"), QStringLiteral("runButton"),
         tr("The arrow beside Run remembers the last ten programs you ran, "
            "newest first.")},
    };
}

bool TourGuide::seen() const {
    return QSettings{}.value(kSeenKey, false).toBool();
}

bool TourGuide::coachSeen(const QString& key) const {
    return QSettings{}.value(QString::fromLatin1(kCoachPrefix) + key, false).toBool();
}

void TourGuide::start() {
    if (seen()) {
        return;
    }
    show();
}

void TourGuide::show() {
    overlay_->startTour(steps());
}

void TourGuide::rearm() {
    QSettings settings;
    settings.remove(kSeenKey);
    for (const QString& key : settings.allKeys()) {
        if (key.startsWith(QLatin1String(kCoachPrefix))) {
            settings.remove(key);
        }
    }
}

bool TourGuide::hint(const QString& key) {
    if (active() || hints_shown_ >= kMaximumHints || coachSeen(key)) {
        return false;
    }
    for (const CoachMark& mark : coachMarks()) {
        if (mark.key != key) {
            continue;
        }
        if (window_ == nullptr || window_->findChild<QWidget*>(mark.objectName) == nullptr) {
            return false;
        }
        QSettings{}.setValue(QString::fromLatin1(kCoachPrefix) + key, true);
        ++hints_shown_;
        overlay_->startCoachMark(mark);
        return true;
    }
    return false;
}

bool TourGuide::active() const {
    return overlay_ != nullptr && overlay_->active();
}

}  // namespace timewarp::ui
