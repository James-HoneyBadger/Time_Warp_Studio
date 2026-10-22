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
// LessonPanel
// ---------------------------------------------------------------------------

namespace {

/// The tick and dash the checkpoint list uses. Chosen rather than drawn, because a
/// learner reads the list as a checklist and an empty bullet reads as "nothing
/// here" instead of "not done yet".
constexpr auto kDoneMark = "\u2713";
constexpr auto kTodoMark = "\u00b7";

}  // namespace

LessonPanel::LessonPanel(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("lessonPanel"));
    setFrameShape(QFrame::StyledPanel);

    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(8, 8, 8, 8);
    layout->setSpacing(6);

    lessons_ = new QComboBox{this};
    lessons_->setObjectName(QStringLiteral("lessonChooser"));
    lessons_->setAccessibleName(tr("Lesson"));
    lessons_->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLengthWithIcon);
    layout->addWidget(lessons_);

    description_ = new QLabel{this};
    description_->setObjectName(QStringLiteral("lessonDescription"));
    description_->setWordWrap(true);
    layout->addWidget(description_);

    checkpoints_ = new QListWidget{this};
    checkpoints_->setObjectName(QStringLiteral("lessonCheckpoints"));
    checkpoints_->setAccessibleName(tr("Checkpoints"));
    // A checkpoint is one line: a title, and whether it is done. Four rows is
    // enough to see the shape of a lesson without the list taking the panel.
    checkpoints_->setMaximumHeight(96);
    layout->addWidget(checkpoints_);

    auto* actions = new QHBoxLayout{};
    actions->setSpacing(4);
    open_ = new QPushButton{tr("Open"), this};
    open_->setObjectName(QStringLiteral("lessonOpenButton"));
    open_->setToolTip(tr("Put this checkpoint's starting program in a document"));
    check_ = new QPushButton{tr("Check"), this};
    check_->setObjectName(QStringLiteral("lessonCheckButton"));
    check_->setToolTip(tr("Run your program and see whether it does what was asked"));
    hint_ = new QPushButton{tr("Hint"), this};
    hint_->setObjectName(QStringLiteral("lessonHintButton"));
    solution_ = new QPushButton{tr("Answer"), this};
    solution_->setObjectName(QStringLiteral("lessonSolutionButton"));
    solution_->setToolTip(tr("Show the worked answer. Try a hint first."));
    reset_ = new QPushButton{tr("Restart"), this};
    reset_->setObjectName(QStringLiteral("lessonResetButton"));
    for (QPushButton* button : {open_, check_, hint_, solution_, reset_}) {
        actions->addWidget(button);
    }
    layout->addLayout(actions);

    detail_ = new QTextBrowser{this};
    detail_->setObjectName(QStringLiteral("lessonDetail"));
    detail_->setOpenExternalLinks(false);
    // Read-only and framed off, because it is there to be read rather than to
    // look like somewhere to type: the editor is somewhere to type.
    detail_->setFrameShape(QFrame::NoFrame);
    layout->addWidget(detail_, 1);

    verdict_ = new QLabel{this};
    verdict_->setObjectName(QStringLiteral("lessonVerdict"));
    verdict_->setWordWrap(true);
    verdict_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    layout->addWidget(verdict_);

    progress_ = new QProgressBar{this};
    progress_->setObjectName(QStringLiteral("lessonProgress"));
    progress_->setTextVisible(true);
    progress_->setFixedHeight(14);
    layout->addWidget(progress_);

    connect(lessons_, QOverload<int>::of(&QComboBox::currentIndexChanged), this, &LessonPanel::onLessonChanged);
    connect(checkpoints_, &QListWidget::currentRowChanged, this,
            &LessonPanel::onCheckpointChanged);
    connect(open_, &QPushButton::clicked, this, [this] {
        emit openCodeRequested(currentLanguageKey(), currentStarter(), currentLesson());
    });
    connect(check_, &QPushButton::clicked, this, [this] {
        if (!currentLesson().isEmpty() && currentCheckpoint() >= 0) {
            emit checkRequested(currentLesson(), currentCheckpoint());
        }
    });
    connect(hint_, &QPushButton::clicked, this, &LessonPanel::showNextHint);
    connect(solution_, &QPushButton::clicked, this, [this] {
        if (!currentLesson().isEmpty() && currentCheckpoint() >= 0) {
            emit solutionRequested(currentLesson(), currentCheckpoint());
        }
    });
    connect(reset_, &QPushButton::clicked, this, [this] {
        if (!currentLesson().isEmpty()) {
            // A restart forgets the verdicts for that lesson, because they are
            // about programs the learner has just chosen to begin again.
            verdicts_.remove(currentLesson());
            emit resetRequested(currentLesson());
        }
    });

    // Nothing is requested here on purpose. A signal emitted during construction
    // is a signal nobody is connected to yet, and the decision about *when* to
    // pay for the curriculum -- when the tab is first looked at -- belongs to the
    // window, which is the only thing that knows when that happens.
}

void LessonPanel::setLessons(const QList<timewarp::ui::LessonSummary>& lessons) {
    const QString previous = currentLesson();
    curriculum_ = lessons;
    // A new curriculum is a new session: a verdict against an id that may no
    // longer exist is not worth carrying into a report.
    verdicts_.clear();

    const QSignalBlocker blocker{lessons_};
    lessons_->clear();
    for (const timewarp::ui::LessonSummary& lesson : lessons) {
        lessons_->addItem(tr("%1 \u00b7 %2").arg(lesson.language, lesson.title), lesson.id);
    }

    // The selection is restored by id rather than by index: progress can change
    // the order of nothing, but a refresh must not move a learner off the lesson
    // they were reading.
    int index = previous.isEmpty() ? 0 : lessons_->findData(previous);
    if (index < 0) {
        index = 0;
    }
    if (lessons_->count() > 0) {
        lessons_->setCurrentIndex(index);
    }
    onLessonChanged(lessons_->currentIndex());
}

void LessonPanel::setProgress(const QList<timewarp::ui::LessonProgress>& rows, int completed,
                              int total) {
    rows_ = rows;
    progress_->setRange(0, total > 0 ? total : 1);
    progress_->setValue(completed);
    progress_->setFormat(tr("%1 of %2 lessons").arg(completed).arg(total));
    refreshCheckpoint();
}

void LessonPanel::setHint(const QString& text, int number, int total) {
    detail_->setPlainText(text);
    if (total > 0) {
        hint_->setToolTip(tr("Hint %1 of %2").arg(number).arg(total));
    }
}

void LessonPanel::setSolution(const QString& text) {
    detail_->setPlainText(tr("Worked answer:\n\n%1").arg(text));
}

void LessonPanel::setChecked(const QString& lesson, int checkpoint, bool passed,
                             const QString& message, bool completed, int finished, int total) {
    // Recorded before the staleness check below: the display only shows the
    // checkpoint on screen, but a report lists every one that was tried, and a
    // verdict that arrives after the panel has moved on is still a verdict.
    verdicts_[lesson][checkpoint] = SessionVerdict{passed, message};

    // A verdict for a different checkpoint than the one on screen is stale, and
    // showing it would tell a learner their answer was wrong when they have
    // already moved on.
    if (lessons_->currentData().toString() != lesson || checkpoint != currentCheckpoint()) {
        return;
    }
    verdict_->setText(message);
    verdict_passed_ = passed;
    refreshVerdictColour();

    if (passed && checkpoint < checkpoints_->count()) {
        // The tick replaces the dash without touching the title, so a learner sees
        // the list they were reading with one line marked rather than a list that
        // has been rebuilt.
        checkpoints_->item(checkpoint)->setText(
            QStringLiteral("%1 %2").arg(QString::fromUtf8(kDoneMark),
                                        checkpoints_->item(checkpoint)->text().mid(2)));
    }
    progress_->setValue(finished);
    // Finishing a lesson is worth saying in the one place the learner is looking,
    // and it is the panel's own sentence: the engine reports the fact, and how to
    // put it in front of somebody is not the engine's business.
    progress_->setFormat(completed
                             ? tr("%1 of %2 lessons \u00b7 this one is finished")
                                   .arg(finished)
                                   .arg(total)
                             : tr("%1 of %2 lessons").arg(finished).arg(total));

    if (passed) {
        // Only move on when there is somewhere to move to, so finishing a lesson
        // does not leave the panel pointing at a checkpoint that does not exist.
        if (checkpoint + 1 < checkpoints_->count()) {
            checkpoints_->setCurrentRow(checkpoint + 1);
        }
        emit progressNeeded();
    }
}

void LessonPanel::setFailure(const QString& request, const QString& error) {
    verdict_->setText(tr("%1 could not be answered: %2").arg(request, error));
    verdict_passed_ = false;
    refreshVerdictColour();
}

void LessonPanel::setVerdictColors(const QColor& passed, const QColor& failed) {
    verdict_pass_ = passed;
    verdict_fail_ = failed;
    refreshVerdictColour();
}

void LessonPanel::refreshVerdictColour() {
    verdict_->setStyleSheet(
        QStringLiteral("color: %1;")
            .arg((verdict_passed_ ? verdict_pass_ : verdict_fail_).name()));
}

void LessonPanel::setPresentationFont(int pointSize) {
    presentation_font_ = pointSize;
    QFont font = description_->font();
    if (pointSize > 0) {
        font.setPointSize(pointSize);
    }
    description_->setFont(font);
    QFont detailFont = detail_->font();
    if (pointSize > 0) {
        detailFont.setPointSize(pointSize);
    }
    detail_->setFont(detailFont);
}

QString LessonPanel::currentLesson() const { return lessons_->currentData().toString(); }

int LessonPanel::currentCheckpoint() const { return checkpoints_->currentRow(); }

QString LessonPanel::currentStarter() const {
    const int index = currentCheckpoint();
    const QString id = currentLesson();
    for (const timewarp::ui::LessonSummary& lesson : curriculum_) {
        if (lesson.id == id && index >= 0 && index < lesson.checkpointCount()) {
            return lesson.checkpoints.at(index).starter;
        }
    }
    return {};
}

QString LessonPanel::currentLanguageKey() const {
    const QString id = currentLesson();
    for (const timewarp::ui::LessonSummary& lesson : curriculum_) {
        if (lesson.id == id) {
            return lesson.languageKey;
        }
    }
    return {};
}

QString LessonPanel::verdictText() const { return verdict_->text(); }

int LessonPanel::lessonCount() const { return static_cast<int>(curriculum_.size()); }

timewarp::ui::exporter::LessonSession LessonPanel::session() const {
    using timewarp::ui::exporter::LessonSession;
    using timewarp::ui::exporter::LessonSessionCheckpoint;

    LessonSession session;
    session.current = currentCheckpoint();
    const QString id = currentLesson();
    if (id.isEmpty()) {
        return session;
    }

    const QHash<int, SessionVerdict> no_verdicts;
    const auto found = verdicts_.constFind(id);
    const QHash<int, SessionVerdict>& lesson_verdicts =
        found == verdicts_.constEnd() ? no_verdicts : found.value();

    for (const timewarp::ui::LessonSummary& lesson : curriculum_) {
        if (lesson.id != id) {
            continue;
        }
        session.lessonTitle = lesson.title;
        session.lessonDescription = lesson.description;
        session.language = lesson.language;
        session.objectives = lesson.objectives;
        for (int index = 0; index < lesson.checkpointCount(); ++index) {
            LessonSessionCheckpoint checkpoint;
            checkpoint.title = lesson.checkpoints.at(index).title;
            checkpoint.description = lesson.checkpoints.at(index).description;
            const auto verdict = lesson_verdicts.constFind(index);
            if (verdict != lesson_verdicts.constEnd()) {
                checkpoint.checked = true;
                checkpoint.passed = verdict.value().passed;
                checkpoint.message = verdict.value().message;
            }
            session.checkpoints.append(checkpoint);
        }
        break;
    }
    for (const timewarp::ui::LessonProgress& row : rows_) {
        if (row.id == id) {
            session.status = row.status;
            break;
        }
    }
    return session;
}

void LessonPanel::onLessonChanged(int index) {
    if (index < 0 || index >= curriculum_.size()) {
        checkpoints_->clear();
        description_->clear();
        detail_->clear();
        return;
    }
    const timewarp::ui::LessonSummary& lesson = curriculum_.at(index);
    description_->setText(tr("%1 \u2014 %2").arg(lesson.title, lesson.description));

    const QSignalBlocker blocker{checkpoints_};
    checkpoints_->clear();
    for (int position = 0; position < lesson.checkpointCount(); ++position) {
        checkpoints_->addItem(
            QStringLiteral("%1 %2").arg(QString::fromUtf8(kTodoMark),
                                        lesson.checkpoints.at(position).title));
    }
    hint_number_ = 0;
    detail_->clear();
    verdict_->clear();
    refreshCheckpoint();
    if (checkpoints_->count() > 0) {
        checkpoints_->setCurrentRow(0);
    }
}

void LessonPanel::onCheckpointChanged(int row) {
    Q_UNUSED(row);
    hint_number_ = 0;
    verdict_->clear();
    refreshCheckpoint();
}

void LessonPanel::refreshCheckpoint() {
    const QString id = currentLesson();
    if (id.isEmpty()) {
        return;
    }
    // The learner's place decides which checkpoint the panel opens on, so a
    // learner who comes back to a lesson is put where they left off rather than
    // at the beginning of it.
    for (const timewarp::ui::LessonProgress& row : std::as_const(rows_)) {
        if (row.id != id) {
            continue;
        }
        for (int position = 0; position < checkpoints_->count(); ++position) {
            const bool done = position < row.checkpoint;
            checkpoints_->item(position)->setText(
                QStringLiteral("%1 %2").arg(QString::fromUtf8(done ? kDoneMark : kTodoMark),
                                            checkpoints_->item(position)->text().mid(2)));
        }
        if (row.checkpoint > 0 && row.checkpoint < checkpoints_->count() &&
            checkpoints_->currentRow() == 0) {
            const QSignalBlocker blocker{checkpoints_};
            checkpoints_->setCurrentRow(row.checkpoint);
        }
        break;
    }
    const int index = currentCheckpoint();
    const int position = lessons_->currentIndex();
    if (position < 0 || index < 0 || position >= curriculum_.size()) {
        return;
    }
    const timewarp::ui::LessonSummary& lesson = curriculum_.at(position);
    if (index < lesson.checkpointCount()) {
        detail_->setPlainText(lesson.checkpoints.at(index).description);
    }
}

void LessonPanel::showNextHint() {
    const QString id = currentLesson();
    const int index = currentCheckpoint();
    if (id.isEmpty() || index < 0) {
        return;
    }
    // The number asked for is the panel's own count of how many it has shown, so
    // pressing Hint twice gives two different hints rather than the same one.
    emit hintRequested(id, index);
    hint_number_ += 1;
}

}  // namespace timewarp::ui
