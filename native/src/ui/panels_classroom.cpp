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
// ClassroomPanel
// ---------------------------------------------------------------------------

ClassroomPanel::ClassroomPanel(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("classroomPanel"));
    setFrameShape(QFrame::StyledPanel);

    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(8, 8, 8, 8);
    layout->setSpacing(6);

    summary_ = new QLabel{this};
    summary_->setObjectName(QStringLiteral("classroomSummary"));
    summary_->setWordWrap(true);
    layout->addWidget(summary_);

    marks_ = new QTableWidget{this};
    marks_->setObjectName(QStringLiteral("classroomMarks"));
    marks_->setAccessibleName(tr("Class marks"));
    marks_->setColumnCount(4);
    marks_->setHorizontalHeaderLabels(
        {tr("Student"), tr("Lesson"), tr("Handed in"), tr("Passed")});
    marks_->horizontalHeader()->setSectionResizeMode(QHeaderView::Stretch);
    marks_->verticalHeader()->setVisible(false);
    marks_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    marks_->setSelectionBehavior(QAbstractItemView::SelectRows);
    marks_->setSelectionMode(QAbstractItemView::SingleSelection);
    layout->addWidget(marks_, 1);

    empty_ = new QLabel{tr("No class list yet. A teacher creates one with "
                           "`teach.py roster add`."),
                        this};
    empty_->setObjectName(QStringLiteral("classroomEmptyState"));
    empty_->setAlignment(Qt::AlignCenter);
    empty_->setWordWrap(true);
    empty_->setEnabled(false);
    layout->addWidget(empty_, 1);

    auto* buttons = new QHBoxLayout;
    open_ = new QPushButton{tr("Open Student Folder"), this};
    open_->setObjectName(QStringLiteral("classroomOpenFolder"));
    connect(open_, &QPushButton::clicked, this, [this] {
        int row = marks_->currentRow();
        if (row < 0 && marks_->rowCount() > 0) {
            row = 0;
        }
        const QString directory = directoryForRow(row);
        if (!directory.isEmpty()) {
            emit openFolderRequested(directory);
        }
    });
    buttons->addWidget(open_);
    refresh_ = new QPushButton{tr("Refresh"), this};
    refresh_->setObjectName(QStringLiteral("classroomRefresh"));
    connect(refresh_, &QPushButton::clicked, this, [this] { emit refreshRequested(); });
    buttons->addWidget(refresh_);
    layout->addLayout(buttons);

    // Carrying a class to another machine is two buttons rather than a menu: it is
    // the teacher's own page, and a class is what moves. The format is the
    // engine's, so what this writes `teach.py unbundle` can read.
    auto* archive = new QHBoxLayout;
    archive->setSpacing(4);
    bundle_ = new QPushButton{tr("Save Bundle..."), this};
    bundle_->setObjectName(QStringLiteral("classroomBundle"));
    bundle_->setToolTip(tr("Write the class list, its work and its settings to one archive"));
    connect(bundle_, &QPushButton::clicked, this, [this] { emit bundleRequested(); });
    archive->addWidget(bundle_);
    unbundle_ = new QPushButton{tr("Load Bundle..."), this};
    unbundle_->setObjectName(QStringLiteral("classroomUnbundle"));
    unbundle_->setToolTip(tr("Unpack a class archive, leaving existing work alone"));
    connect(unbundle_, &QPushButton::clicked, this, [this] { emit unbundleRequested(); });
    archive->addWidget(unbundle_);
    layout->addLayout(archive);

    setRoster({}, {}, {});
    setMarks({});
}

void ClassroomPanel::setRoster(const QList<timewarp::ui::ClassroomStudent>& students,
                               const QStringList& assignedLessons, const QString& note) {
    student_count_ = static_cast<int>(students.size());
    directories_.clear();
    names_.clear();
    student_order_.clear();
    for (const timewarp::ui::ClassroomStudent& student : students) {
        directories_.insert(student.id, student.directory);
        names_.insert(student.id, student.name.isEmpty() ? student.id : student.name);
        student_order_ << student.id;
    }
    // A refresh can bring a new name for a learner whose marks are already on
    // screen, so keep the first column in step rather than leaving the old one.
    for (int index = 0; index < marks_->rowCount(); ++index) {
        QTableWidgetItem* item = marks_->item(index, 0);
        if (item == nullptr) {
            continue;
        }
        const QString id = item->data(Qt::UserRole).toString();
        item->setText(names_.value(id, id));
    }

    // The summary says what the numbers below are about: a table of marks with no
    // statement of which lessons they are for is a table a teacher has to guess at.
    QStringList lines;
    if (student_count_ == 0) {
        lines << tr("No students on the class list.");
    } else if (student_count_ == 1) {
        lines << tr("1 student on the class list.");
    } else {
        lines << tr("%1 students on the class list.").arg(student_count_);
    }
    if (!assignedLessons.isEmpty()) {
        lines << tr("Assigned: %1").arg(assignedLessons.join(QStringLiteral(", ")));
    } else {
        lines << tr("Nothing has been assigned yet.");
    }
    if (!note.isEmpty()) {
        lines << note;
    }
    summary_->setText(lines.join(QStringLiteral(" \u00b7 ")));
    updateSummary();
}

void ClassroomPanel::setMarks(const QList<timewarp::ui::ClassroomMark>& rows) {
    marks_->setRowCount(static_cast<int>(rows.size()));
    for (int index = 0; index < rows.size(); ++index) {
        const timewarp::ui::ClassroomMark& row = rows.at(index);
        marks_->setItem(index, 0,
                        new QTableWidgetItem{names_.value(row.student, row.student)});
        marks_->setItem(index, 1, new QTableWidgetItem{row.title.isEmpty() ? row.lesson
                                                                          : row.title});
        marks_->setItem(index, 2,
                        new QTableWidgetItem{QStringLiteral("%1/%2").arg(row.handedIn).arg(
                            row.checkpoints)});
        marks_->setItem(index, 3,
                        new QTableWidgetItem{QStringLiteral("%1/%2").arg(row.passed).arg(
                            row.checkpoints)});
        // The student id travels with the row, so the folder button can find the
        // right directory whatever the visible column ends up showing.
        marks_->item(index, 0)->setData(Qt::UserRole, row.student);
    }
    updateSummary();
}

void ClassroomPanel::setFailure(const QString& request, const QString& error) {
    summary_->setText(tr("%1 could not be answered: %2").arg(request, error));
}

int ClassroomPanel::studentCount() const { return student_count_; }

int ClassroomPanel::markRowCount() const { return marks_->rowCount(); }

bool ClassroomPanel::emptyStateVisible() const { return !empty_->isHidden(); }

QString ClassroomPanel::directoryForRow(int row) const {
    if (row < 0 || row >= marks_->rowCount()) {
        // With no marks yet, the first student on the class list is the useful
        // default: a teacher looking at an unmarked class still wants the folder.
        // The list order, not the hash's: see `student_order_`.
        return student_order_.isEmpty() ? QString{} : directories_.value(student_order_.first());
    }
    const QTableWidgetItem* item = marks_->item(row, 0);
    const QString id = item == nullptr ? QString{} : item->data(Qt::UserRole).toString();
    return directories_.value(id);
}

void ClassroomPanel::updateSummary() {
    const bool empty = student_count_ == 0;
    empty_->setVisible(empty);
    marks_->setVisible(!empty);
    open_->setEnabled(!directories_.isEmpty());
    refresh_->setEnabled(true);
    // An empty class is nothing to carry; an archive to unpack is exactly how a
    // class arrives in the first place, so that button never goes away.
    bundle_->setEnabled(!directories_.isEmpty());
    unbundle_->setEnabled(true);
}

}  // namespace timewarp::ui
