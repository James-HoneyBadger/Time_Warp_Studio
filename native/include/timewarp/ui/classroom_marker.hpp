// SPDX-License-Identifier: MIT
//
// Marking a class off the GUI thread.
//
// A mark runs the learner's program. A handout that never terminates therefore
// takes its whole budget (ten seconds), and doing that on the GUI thread -- which
// is what a direct call from the panel would do -- freezes the window for one
// file, and for as long as the class has files like it.
//
// The thread owns everything it needs: a job carries the source and the
// checkpoint, so marking never reaches back into the class while the window is
// free to change it.

#pragma once

#include <QList>
#include <QThread>

#include "timewarp/lessons/classroom.hpp"
#include "timewarp/ui/reports.hpp"

namespace timewarp::ui {

class ClassroomMarker final : public QThread {
    Q_OBJECT

public:
    explicit ClassroomMarker(QObject* parent = nullptr);

    /// Starts marking `jobs`. Refused when a mark is already running, which is
    /// what keeps two answers from arriving out of order.
    bool begin(QList<timewarp::lessons::MarkJob> jobs);

signals:
    /// The mark sheet, one row per learner per lesson. Emitted from the worker
    /// thread and delivered on the GUI thread by a queued connection.
    void marked(const QList<timewarp::ui::ClassroomMark>& rows);

protected:
    void run() override;

private:
    QList<timewarp::lessons::MarkJob> jobs_;
};

}  // namespace timewarp::ui
