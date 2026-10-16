// SPDX-License-Identifier: MIT

#include "timewarp/ui/find_bar.hpp"

#include <QCheckBox>
#include <QEvent>
#include <QHBoxLayout>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QTextCursor>
#include <QToolButton>

#include "timewarp/ui/code_editor.hpp"

namespace timewarp::ui {

namespace {

/// Narrower than a text field: these hold one word or one arrow.
constexpr int kButtonWidth = 30;

[[nodiscard]] QToolButton* makeButton(const QString& text, const QString& tooltip) {
    auto* button = new QToolButton;
    button->setText(text);
    button->setToolTip(tooltip);
    button->setAutoRaise(true);
    return button;
}

}  // namespace

FindBar::FindBar(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("findBar"));
    setFrameShape(QFrame::StyledPanel);

    auto* row = new QHBoxLayout{this};
    row->setContentsMargins(6, 4, 6, 4);
    row->setSpacing(4);

    term_ = new QLineEdit;
    term_->setObjectName(QStringLiteral("findTerm"));
    term_->setPlaceholderText(tr("Find"));
    term_->setClearButtonEnabled(true);
    term_->installEventFilter(this);
    row->addWidget(term_, 2);

    previous_ = makeButton(QStringLiteral("\u25C0"), tr("Previous match (Shift+F3)"));
    next_ = makeButton(QStringLiteral("\u25B6"), tr("Next match (F3)"));
    for (QToolButton* button : {previous_, next_}) {
        button->setFixedWidth(kButtonWidth);
        row->addWidget(button);
    }

    case_ = new QCheckBox{tr("Match case")};
    case_->setObjectName(QStringLiteral("findCase"));
    row->addWidget(case_);

    replacement_ = new QLineEdit;
    replacement_->setObjectName(QStringLiteral("replaceTerm"));
    replacement_->setPlaceholderText(tr("Replace with"));
    replacement_->installEventFilter(this);
    row->addWidget(replacement_, 2);

    replace_ = new QToolButton;
    replace_->setText(tr("Replace"));
    replace_->setAutoRaise(true);
    row->addWidget(replace_);

    replace_all_ = new QToolButton;
    replace_all_->setText(tr("Replace all"));
    replace_all_->setAutoRaise(true);
    row->addWidget(replace_all_);

    status_ = new QLabel;
    status_->setObjectName(QStringLiteral("findStatus"));
    row->addWidget(status_);

    QToolButton* close = makeButton(QStringLiteral("\u2715"), tr("Close (Esc)"));
    close->setFixedWidth(kButtonWidth);
    row->addWidget(close);

    // Typing re-counts rather than jumping: a search that moves the cursor on
    // every keystroke makes it impossible to finish typing the term.
    connect(term_, &QLineEdit::textChanged, this, [this](const QString&) { updateStatus(); });
    connect(case_, &QCheckBox::toggled, this, [this](bool) { updateStatus(); });
    connect(next_, &QToolButton::clicked, this, [this] { findAgain(false); });
    connect(previous_, &QToolButton::clicked, this, [this] { findAgain(true); });
    connect(replace_, &QToolButton::clicked, this, [this] { replaceOne(); });
    connect(replace_all_, &QToolButton::clicked, this, [this] { replaceEverything(); });
    connect(close, &QToolButton::clicked, this, [this] { closeBar(); });

    hide();
    updateStatus();
}

void FindBar::setEditor(CodeEditor* editor) { editor_ = editor; }

QString FindBar::term() const { return term_->text(); }

QString FindBar::statusText() const { return status_->text(); }

bool FindBar::replaceRowVisible() const {
    // The explicit flag, not `isVisible()`: a bar inside a window that has not
    // been shown yet is not visible either way, and the contract a test can hold
    // this to is "the replacement row was asked for", not "the window is up".
    return !replacement_->isHidden();
}

void FindBar::setTerm(const QString& text) {
    term_->setText(text);
    updateStatus();
}

void FindBar::open(bool withReplacement) {
    if (editor_ != nullptr) {
        // A selection on one line is what the user means by "find this".
        QTextCursor cursor = editor_->textCursor();
        if (cursor.hasSelection()) {
            const QString selected = cursor.selectedText();
            if (!selected.contains(QChar::ParagraphSeparator) && !selected.isEmpty() &&
                !selected.contains(QLatin1Char('\n'))) {
                term_->setText(selected);
            }
        }
    }
    replacement_->setVisible(withReplacement);
    replace_->setVisible(withReplacement);
    replace_all_->setVisible(withReplacement);
    show();
    term_->setFocus();
    term_->selectAll();
    updateStatus();
}

void FindBar::refresh() { updateStatus(); }

void FindBar::findAgain(bool backwards) {
    if (editor_ == nullptr || term_->text().isEmpty()) {
        return;
    }
    const bool found = editor_->findNext(term_->text(), case_->isChecked(), backwards);
    status_->setText(found ? status_->text() : tr("no matches"));
    if (found) {
        updateStatus();
    }
}

void FindBar::replaceOne() {
    if (editor_ == nullptr || term_->text().isEmpty()) {
        return;
    }
    // Replace, then move on: the cursor is left on the match just replaced, so
    // without the second step the button would replace the same word forever.
    editor_->replaceCurrent(term_->text(), replacement_->text(), case_->isChecked());
    editor_->findNext(term_->text(), case_->isChecked(), false);
    updateStatus();
}

void FindBar::replaceEverything() {
    if (editor_ == nullptr || term_->text().isEmpty()) {
        return;
    }
    const int count =
        editor_->replaceAll(term_->text(), replacement_->text(), case_->isChecked());
    status_->setText(count == 1 ? tr("replaced 1") : tr("replaced %1").arg(count));
}

void FindBar::closeBar() {
    hide();
    if (editor_ != nullptr) {
        editor_->setFocus();
    }
    emit closed();
}

void FindBar::updateStatus() {
    if (editor_ == nullptr || term_->text().isEmpty()) {
        status_->clear();
        return;
    }
    const int count = editor_->matchCount(term_->text(), case_->isChecked());
    if (count == 0) {
        status_->setText(tr("no matches"));
    } else if (count == 1) {
        status_->setText(tr("1 match"));
    } else {
        status_->setText(tr("%1 matches").arg(count));
    }
}

bool FindBar::eventFilter(QObject* watched, QEvent* event) {
    if (event->type() != QEvent::KeyPress) {
        return QFrame::eventFilter(watched, event);
    }
    auto* key = static_cast<QKeyEvent*>(event);
    if (key->key() == Qt::Key_Escape) {
        closeBar();
        return true;
    }
    if (key->key() == Qt::Key_Return || key->key() == Qt::Key_Enter) {
        if (watched == replacement_) {
            replaceOne();
        } else {
            findAgain(key->modifiers().testFlag(Qt::ShiftModifier));
        }
        return true;
    }
    return QFrame::eventFilter(watched, event);
}

void FindBar::keyPressEvent(QKeyEvent* event) {
    if (event->key() == Qt::Key_Escape) {
        closeBar();
        return;
    }
    QFrame::keyPressEvent(event);
}

}  // namespace timewarp::ui
