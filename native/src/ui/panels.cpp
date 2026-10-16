// SPDX-License-Identifier: MIT

#include "timewarp/ui/panels.hpp"

#include <algorithm>
#include <utility>

#include <QComboBox>
#include <QEvent>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QPainter>
#include <QPushButton>
#include <QResizeEvent>
#include <QVector>
#include <QVBoxLayout>

namespace timewarp::ui {

// ---------------------------------------------------------------------------
// PlaceholderPanel
// ---------------------------------------------------------------------------

PlaceholderPanel::PlaceholderPanel(const QString& title, const QString& standsFor,
                                   const QSize& minimum, QWidget* parent)
    : QFrame(parent), minimum_(minimum) {
    setObjectName(QStringLiteral("placeholderPanel"));
    setFrameShape(QFrame::StyledPanel);

    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(12, 12, 12, 12);
    layout->setSpacing(4);

    auto* heading = new QLabel{title, this};
    QFont headingFont = heading->font();
    headingFont.setBold(true);
    heading->setFont(headingFont);
    heading->setAlignment(Qt::AlignCenter);
    layout->addWidget(heading);

    auto* detail = new QLabel{standsFor, this};
    QFont detailFont = detail->font();
    detailFont.setItalic(true);
    detailFont.setPointSizeF(detailFont.pointSizeF() * 0.9);
    detail->setFont(detailFont);
    detail->setAlignment(Qt::AlignCenter);
    detail->setWordWrap(true);
    layout->addWidget(detail);

    layout->addStretch(1);
}

QSize PlaceholderPanel::minimumSizeHint() const { return minimum_; }

// ---------------------------------------------------------------------------
// ImmediateModePanel
// ---------------------------------------------------------------------------

ImmediateModePanel::ImmediateModePanel(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("immediateMode"));
    setFrameShape(QFrame::StyledPanel);
    // The floor the Python sets, kept in sync with the layout test. Without it
    // the vertical splitter above can drag the REPL bar down to nothing and the
    // user has no way to type a command.
    setMinimumHeight(kMinimumHeight);

    auto* layout = new QHBoxLayout{this};
    layout->setContentsMargins(6, 4, 6, 4);
    layout->setSpacing(6);

    auto* prompt = new QLabel{QStringLiteral("REPL \u203a"), this};
    QFont promptFont = prompt->font();
    promptFont.setBold(true);
    prompt->setFont(promptFont);
    layout->addWidget(prompt);

    input_ = new QLineEdit{this};
    input_->setObjectName(QStringLiteral("immediateInput"));
    input_->setPlaceholderText(
        tr("Type a command and press Return \u2014 e.g. PRINT 2 + 2"));
    input_->setClearButtonEnabled(true);
    layout->addWidget(input_, 1);

    connect(input_, &QLineEdit::returnPressed, this, [this] {
        const QString text = input_->text().trimmed();
        if (text.isEmpty()) {
            return;
        }
        // The panel does not clear its own input: whether a command is accepted
        // is the interpreter's answer, and the owner decides what to do with it.
        emit commandSubmitted(text);
    });
}

QString ImmediateModePanel::command() const { return input_->text(); }

void ImmediateModePanel::setCommand(const QString& text) { input_->setText(text); }

void ImmediateModePanel::focusInput() {
    input_->setFocus(Qt::ShortcutFocusReason);
    input_->selectAll();
}

// ---------------------------------------------------------------------------
// FunctionKeyBar
// ---------------------------------------------------------------------------

namespace {

struct KeyBinding {
    const char* key;
    const char* action;
    const char* label;
};

// The "normal" layer of the Python widget's table.
constexpr KeyBinding kKeys[] = {
    {"F1", "help", "Help"},        {"F2", "save", "Save"},
    {"F3", "open", "Open"},        {"F4", "outline", "Outline"},
    {"F5", "zoom", "Zoom"},        {"F6", "next_pane", "Next"},
    {"F7", "step_into", "Trace"},  {"F8", "step_over", "Step"},
    {"F9", "run", "Run"},          {"F10", "palette", "Menu"},
};

// label -> stable id, from the Python widget's `_DECKS`.
constexpr std::pair<const char*, const char*> kDecks[] = {
    {"1:CODE", "code_focus"},
    {"2:SPLIT", "split_lab"},
    {"3:GFX", "graphics_studio"},
    {"4:DEBUG", "debugger_deck"},
};

}  // namespace

FunctionKeyBar::FunctionKeyBar(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("functionKeyBar"));
    setFrameShape(QFrame::StyledPanel);
    setFixedHeight(kFixedHeight);

    auto* layout = new QHBoxLayout{this};
    layout->setContentsMargins(4, 0, 4, 0);
    layout->setSpacing(3);

    decks_ = new QComboBox{this};
    decks_->setObjectName(QStringLiteral("deckSelector"));
    QStringList labels;
    for (const auto& [label, id] : kDecks) {
        const QString text = QString::fromUtf8(label);
        labels << text;
        deckIds_.append({text, QString::fromUtf8(id)});
    }
    decks_->addItems(labels);
    connect(decks_, &QComboBox::currentIndexChanged, this, [this](int index) {
        if (index >= 0 && index < deckIds_.size()) {
            emit deckSelected(deckIds_.at(index).second);
        }
    });
    layout->addWidget(decks_);

    for (const KeyBinding& binding : kKeys) {
        auto* button = new QPushButton{
            QStringLiteral("%1 %2").arg(QString::fromUtf8(binding.key),
                                        QString::fromUtf8(binding.label)),
            this};
        const QString action = QString::fromUtf8(binding.action);
        button->setToolTip(QStringLiteral("%1 \u2014 %2")
                               .arg(QString::fromUtf8(binding.key),
                                    QString::fromUtf8(binding.label)));
        // The action travels as its own argument rather than being captured from
        // the button text, so relabelling a key cannot silently change what it
        // does.
        connect(button, &QPushButton::clicked, this,
                [this, action] { emit actionTriggered(action); });
        layout->addWidget(button);
    }

    layout->addStretch(1);
}

void FunctionKeyBar::setDecks(const QStringList& labels, int current) {
    deckIds_.clear();
    decks_->blockSignals(true);
    decks_->clear();
    for (const QString& label : labels) {
        decks_->addItem(label);
        deckIds_.append({label, label});
    }
    decks_->setCurrentIndex(current);
    decks_->blockSignals(false);
}

int FunctionKeyBar::currentDeck() const { return decks_->currentIndex(); }

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

void CrtOverlay::setScanlineIntensity(int percent) {
    const int clamped = std::clamp(percent, 0, 100);
    if (clamped == intensity_) {
        return;
    }
    intensity_ = clamped;
    update();
}

int CrtOverlay::scanlineIntensity() const noexcept { return intensity_; }

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
    if (intensity_ <= 0) {
        return;
    }
    // Deliberately understated: a full-strength scanline layer over text is
    // unreadable at 100%. The divisor keeps the effect at the level the Python's
    // default 0.15 intensity produced.
    const int alpha = (intensity_ * 255) / 100 / 3;
    if (alpha <= 0) {
        return;
    }

    QPainter painter{this};
    painter.setPen(Qt::NoPen);
    painter.setBrush(QColor{0, 0, 0, alpha});
    for (int y = 0; y < height(); y += 2) {
        painter.drawRect(0, y, width(), 1);
    }
}

}  // namespace timewarp::ui
