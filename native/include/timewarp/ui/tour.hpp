// SPDX-License-Identifier: MIT
//
// The first-run tour and the one-time hints built on the same anchors.
//
// A tour is only useful if it points at the real widget: a step that names a
// menu entry and draws a box somewhere else teaches the wrong thing. Every step
// here names an objectName, and `ui_layout_tests` asserts that the name still
// resolves, so renaming a widget breaks the test rather than the tour.
//
// The overlay is deliberately not a dialog: it dims the window but leaves the
// anchored control visible, and it swallows clicks rather than letting a learner
// press a button they cannot yet see behind the card.

#pragma once

#include <QList>
#include <QObject>
#include <QString>
#include <QWidget>

class QFrame;
class QLabel;
class QPushButton;

namespace timewarp::ui {

/// One stop of the guided tour.
struct TourStep {
    QString objectName;  ///< The widget the card points at.
    QString title;
    QString text;
};

/// A one-time hint. The key is what "already seen" is stored under.
struct CoachMark {
    QString key;
    QString objectName;
    QString text;
};

/// The dimming overlay and its card. Owned by the window it points into.
class TourOverlay final : public QWidget {
    Q_OBJECT

public:
    explicit TourOverlay(QWidget* window);

    void startTour(const QList<TourStep>& steps);
    void startCoachMark(const CoachMark& mark);
    void next();
    void back();
    void dismiss();

    [[nodiscard]] bool active() const noexcept { return active_; }
    [[nodiscard]] int step() const noexcept { return index_; }
    [[nodiscard]] int stepCount() const noexcept { return steps_.size(); }

signals:
    /// The card is now pointing at a step; a listener can raise the panel the
    /// step names before the card measures its anchor.
    void stepShown(int index);
    void tourFinished();
    void coachDismissed();

protected:
    void paintEvent(QPaintEvent* event) override;
    void mousePressEvent(QMouseEvent* event) override;
    bool eventFilter(QObject* watched, QEvent* event) override;

private:
    void showCurrentStep();
    void layoutCard();
    [[nodiscard]] QRect anchorRect() const;

    QWidget* window_ = nullptr;
    QFrame* card_ = nullptr;
    QLabel* title_ = nullptr;
    QLabel* body_ = nullptr;
    QLabel* progress_ = nullptr;
    QPushButton* back_ = nullptr;
    QPushButton* next_ = nullptr;
    QPushButton* skip_ = nullptr;
    QList<TourStep> steps_;
    int index_ = 0;
    QString anchor_;
    QString coach_text_;
    bool coach_mode_ = false;
    bool active_ = false;
};

/// Persistence and the catalogue of steps and hints.
class TourGuide final : public QObject {
    Q_OBJECT

public:
    /// *window* is the window whose children the steps point at; it also owns the
    /// overlay.
    explicit TourGuide(QWidget* window, QObject* parent = nullptr);

    [[nodiscard]] static QList<TourStep> steps();
    [[nodiscard]] static QList<CoachMark> coachMarks();
    /// At most this many hints per session, so an unlucky build cannot paper the
    /// window with tips.
    static constexpr int kMaximumHints = 4;

    [[nodiscard]] bool seen() const;
    [[nodiscard]] bool coachSeen(const QString& key) const;

    /// Starts the tour only on a first run.
    void start();
    /// Starts it even when it has been seen: the Help menu's explicit request.
    void show();
    /// Forgets "seen", so the next start shows the tour again. Also re-arms the
    /// hints, which is what a teacher demonstrating the tour wants.
    void rearm();
    /// Shows the named hint once. False when it is already seen, the tour is
    /// running, the session's cap is reached, or the anchor is missing.
    bool hint(const QString& key);

    [[nodiscard]] bool active() const;
    [[nodiscard]] TourOverlay* overlay() const noexcept { return overlay_; }

signals:
    void stepShown(int index);

private:
    QWidget* window_ = nullptr;
    TourOverlay* overlay_ = nullptr;
    int hints_shown_ = 0;
};

}  // namespace timewarp::ui
