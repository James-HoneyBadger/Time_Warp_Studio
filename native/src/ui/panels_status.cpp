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
// OutlinePanel
// ---------------------------------------------------------------------------

OutlinePanel::OutlinePanel(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("outlinePanel"));
    setFrameShape(QFrame::StyledPanel);

    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(2, 2, 2, 2);
    layout->setSpacing(0);

    list_ = new QListWidget{this};
    list_->setObjectName(QStringLiteral("outlineList"));
    list_->setAccessibleName(tr("Document outline"));
    list_->setFrameShape(QFrame::NoFrame);
    // One click jumps: an outline is a navigation control, and asking a user to
    // double-click a list that looks like a list is a wasted gesture.
    connect(list_, &QListWidget::itemClicked, this, [this](QListWidgetItem* item) {
        if (item != nullptr) {
            emit entryActivated(item->data(Qt::UserRole).toInt());
        }
    });
    connect(list_, &QListWidget::itemActivated, this, [this](QListWidgetItem* item) {
        if (item != nullptr) {
            emit entryActivated(item->data(Qt::UserRole).toInt());
        }
    });
    layout->addWidget(list_, 1);

    empty_ = new QLabel{tr("No procedures, functions or labels in this program."), this};
    empty_->setObjectName(QStringLiteral("outlineEmptyState"));
    empty_->setAlignment(Qt::AlignCenter);
    empty_->setWordWrap(true);
    empty_->setEnabled(false);
    layout->addWidget(empty_, 1);

    setEntries({});
}

void OutlinePanel::setEntries(const QList<timewarp::ui::OutlineEntry>& entries) {
    list_->clear();
    for (const timewarp::ui::OutlineEntry& entry : entries) {
        auto* item = new QListWidgetItem{entry.name, list_};
        item->setData(Qt::UserRole, entry.line);
        item->setToolTip(tr("%1 \u2014 line %2")
                             .arg(timewarp::ui::outlineKindName(entry.kind))
                             .arg(entry.line));
    }
    const bool empty = entries.isEmpty();
    list_->setVisible(!empty);
    empty_->setVisible(empty);
}

int OutlinePanel::entryCount() const { return list_->count(); }

int OutlinePanel::lineAt(int index) const {
    const QListWidgetItem* item = list_->item(index);
    return item == nullptr ? 0 : item->data(Qt::UserRole).toInt();
}

bool OutlinePanel::emptyStateVisible() const { return !empty_->isHidden(); }

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

}  // namespace timewarp::ui
