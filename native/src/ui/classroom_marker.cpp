// SPDX-License-Identifier: MIT
//
// The marking thread. See `classroom_marker.hpp`.

#include "timewarp/ui/classroom_marker.hpp"

#include <utility>

#include <QMetaType>

namespace timewarp::ui {

ClassroomMarker::ClassroomMarker(QObject* parent) : QThread(parent) {
    // Queued connections copy the payload, which needs the type registered.
    qRegisterMetaType<QList<timewarp::ui::ClassroomMark>>(
        "QList<timewarp::ui::ClassroomMark>");
}

bool ClassroomMarker::begin(QList<timewarp::lessons::MarkJob> jobs) {
    if (isRunning()) {
        return false;
    }
    jobs_ = std::move(jobs);
    start();
    return true;
}

void ClassroomMarker::run() {
    const QList<timewarp::lessons::MarkJob> jobs = jobs_;
    jobs_.clear();
    const QList<timewarp::lessons::Submission> submissions =
        timewarp::lessons::Classroom::markAll(jobs);
    const QList<timewarp::lessons::MarkRow> rows =
        timewarp::lessons::Classroom::marksFor(jobs, submissions);
    QList<timewarp::ui::ClassroomMark> out;
    for (const timewarp::lessons::MarkRow& row : rows) {
        timewarp::ui::ClassroomMark mark;
        mark.student = row.student;
        mark.lesson = row.lesson;
        mark.title = row.title;
        mark.handedIn = row.handedIn;
        mark.checkpoints = row.checkpoints;
        mark.passed = row.passed;
        mark.complete = row.complete;
        out.append(mark);
    }
    emit marked(out);
}

}  // namespace timewarp::ui
