// SPDX-License-Identifier: MIT
//
// The main window's leaf widgets.
//
// Each of these stands in for a Python widget that is far too large to port as
// part of a layout translation -- `output.py` alone is 1,220 lines. What they do
// share is the part the *layout* depends on: the size hints, the minimums, the
// fixed heights and the signals. So the window's structure is real and testable
// today, and replacing one with the full implementation is a single construction
// site in `MainWindow`.
//
// Where a widget is a genuine simplification rather than a stub, the comment says
// so, so nobody assumes the feature is complete.

#pragma once

#include <QFrame>
#include <QList>
#include <QPair>
#include <QSize>
#include <QString>
#include <QStringList>
#include <QWidget>

#include "timewarp/bridge/python_worker.hpp"

class QComboBox;
class QLabel;
class QLineEdit;
class QListWidget;
class QProgressBar;
class QPushButton;
class QTextBrowser;
class QVBoxLayout;

namespace timewarp::ui {

/// Stands in for a panel not yet ported.
///
/// Honours a minimum size, because without one a `QSplitter` collapses the panel
/// to zero and the *layout* stops being verifiable -- which is the only thing
/// this class exists to protect.
class PlaceholderPanel final : public QFrame {
    Q_OBJECT

public:
    /// `standsFor` names the Python class this replaces, and is shown in the
    /// panel so a screenshot of the running window is self-documenting.
    PlaceholderPanel(const QString& title, const QString& standsFor,
                     const QSize& minimum = QSize{120, 80},
                     QWidget* parent = nullptr);

    [[nodiscard]] QSize minimumSizeHint() const override;

private:
    QSize minimum_;
};

/// Stands in for `ImmediateModePanel` (`ui/output.py`).
///
/// A REPL prompt and an entry line. The layout contract is the 44px floor the
/// Python sets so the bar cannot be dragged out of sight; the interpreter
/// plumbing behind it is out of scope here.
class ImmediateModePanel final : public QFrame {
    Q_OBJECT

public:
    /// The Python sets `setMinimumHeight(44)`; kept as a named constant because
    /// the layout test asserts it.
    static constexpr int kMinimumHeight = 44;

    explicit ImmediateModePanel(QWidget* parent = nullptr);

    /// Text the user has typed but not yet submitted, for history recall.
    [[nodiscard]] QString command() const;
    void setCommand(const QString& text);
    void focusInput();

signals:
    /// The user pressed Return on a non-empty command.
    void commandSubmitted(const QString& command);

private:
    QLineEdit* input_ = nullptr;
};

/// Stands in for `FunctionKeyBar` (`ui/function_key_bar.py`).
///
/// The Python widget has three modifier layers and a deck selector. Only the
/// normal layer and the deck selector are reproduced: the layout needs the
/// fixed 30px height and the two signals, and inventing the remaining layers
/// here would mean inventing actions that do not exist yet.
class FunctionKeyBar final : public QFrame {
    Q_OBJECT

public:
    /// Python: `setFixedHeight(30)`.
    static constexpr int kFixedHeight = 30;

    explicit FunctionKeyBar(QWidget* parent = nullptr);

    /// Replaces the deck choices. The Python ships four workspace decks.
    void setDecks(const QStringList& labels, int current = 0);

    [[nodiscard]] int currentDeck() const;

signals:
    /// Emitted with the action name of the pressed key, e.g. `"save"`.
    void actionTriggered(const QString& action);
    /// Emitted with the deck's stable id, e.g. `"debugger_deck"`.
    void deckSelected(const QString& deckId);

private:
    QComboBox* decks_ = nullptr;
    /// Displayed label paired with the stable id the deck is known by.
    QList<QPair<QString, QString>> deckIds_;
};

/// Stands in for `CRTEffectOverlay` (`ui/crt_effect.py`).
///
/// The Python overlay draws scanlines, curvature, vignette, glow and flicker and
/// exposes sliders for each. Only scanlines are implemented: the point of
/// including it at all is the *geometry tracking*, which is where the Python
/// approach is fragile.
///
/// The Python resizes the overlay from `MainWindow.resizeEvent` by reaching into
/// `overlay.parent().rect()`. That only holds while the overlay's parent happens
/// to be the widget being resized; reparenting it -- to the viewport of a
/// scroll area, say -- silently stops it from tracking. This version installs
/// itself as an event filter on the widget it covers, so the coupling is stated
/// instead of assumed.
class CrtOverlay final : public QWidget {
    Q_OBJECT

public:
    explicit CrtOverlay(QWidget* target);

    /// 0..100. Zero disables painting without hiding the widget.
    void setScanlineIntensity(int percent);
    [[nodiscard]] int scanlineIntensity() const noexcept;

    /// The widget this overlay tracks, or null if it has been destroyed.
    [[nodiscard]] QWidget* target() const noexcept;

protected:
    bool eventFilter(QObject* watched, QEvent* event) override;
    void paintEvent(QPaintEvent* event) override;

private:
    void syncToTarget();

    QWidget* target_ = nullptr;
    int intensity_ = 15;
};

/// The lesson panel: the curriculum, the current checkpoint, and a way to check.
///
/// Unlike everything else in this header, this one is **not** a placeholder. The
/// Python implementation shipped a working lesson panel, and the panel is the one
/// part of the teaching side that has to exist in the application rather than
/// only in a tool: a learner of eleven should not have to open a terminal.
///
/// The panel is a *view* and nothing more. It asks and it displays; it does not
/// read the curriculum, run a program, or decide whether an answer is right. All
/// of that is the engine's, reached through the bridge, so the verdict in this
/// panel and the verdict from `native/tools/learn.py` are the same function's
/// answer rather than two opinions that can drift apart.
///
/// It never edits a document either. "Open in editor" raises a request, and the
/// window decides where the code goes, because the panel has no business knowing
/// about tabs.
class LessonPanel final : public QFrame {
    Q_OBJECT

public:
    /// The narrowest the panel is useful at: a lesson title and a checkpoint.
    static constexpr int kMinimumWidth = 240;

    explicit LessonPanel(QWidget* parent = nullptr);

    /// Replaces the lesson list, keeping the current selection when it survives.
    void setLessons(const QList<timewarp::bridge::LessonSummary>& lessons);
    /// Applies the learner's place in every lesson.
    void setProgress(const QList<timewarp::bridge::LessonProgress>& rows, int completed,
                     int total);
    /// Shows a hint. `number` and `total` are 1-based, for "hint 2 of 3".
    void setHint(const QString& text, int number, int total);
    /// Shows the worked answer.
    void setSolution(const QString& text);
    /// Shows the verdict for a checkpoint.
    void setChecked(const QString& lesson, int checkpoint, bool passed,
                    const QString& message, bool completed, int finished, int total);
    /// Reports a request that could not be answered.
    void setFailure(const QString& request, const QString& error);
    /// Sets the size the lesson text is drawn at, for teaching from the front.
    void setPresentationFont(int pointSize);

    /// The lesson the learner is looking at, or an empty string.
    [[nodiscard]] QString currentLesson() const;
    /// The checkpoint the learner is looking at, or -1.
    [[nodiscard]] int currentCheckpoint() const;
    /// The starting program for the current checkpoint, for the window to open.
    [[nodiscard]] QString currentStarter() const;
    /// The language key of the current lesson, e.g. "CHIP8".
    [[nodiscard]] QString currentLanguageKey() const;
    /// The verdict text, so a test can assert what the learner is told.
    [[nodiscard]] QString verdictText() const;
    /// The number of lessons the panel is showing.
    [[nodiscard]] int lessonCount() const;

signals:
    /// The panel has just been built and wants the curriculum.
    void curriculumNeeded();
    /// The panel wants the learner's progress again.
    void progressNeeded();
    /// Put the current checkpoint's starting program in a document.
    void openCodeRequested(const QString& languageKey, const QString& source,
                           const QString& name);
    /// Check the current checkpoint. The window supplies the source, because it
    /// owns the editor and the panel does not.
    void checkRequested(const QString& lesson, int checkpoint);
    /// Ask for the next hint.
    void hintRequested(const QString& lesson, int checkpoint);
    /// Ask for the worked answer, on purpose.
    void solutionRequested(const QString& lesson, int checkpoint);
    /// Start the lesson again.
    void resetRequested(const QString& lesson);

private:
    void onLessonChanged(int index);
    void onCheckpointChanged(int row);
    void refreshCheckpoint();
    void showNextHint();

    QComboBox* lessons_ = nullptr;
    QListWidget* checkpoints_ = nullptr;
    QLabel* description_ = nullptr;
    QTextBrowser* detail_ = nullptr;
    QPushButton* open_ = nullptr;
    QPushButton* check_ = nullptr;
    QPushButton* hint_ = nullptr;
    QPushButton* solution_ = nullptr;
    QPushButton* reset_ = nullptr;
    QLabel* verdict_ = nullptr;
    QProgressBar* progress_ = nullptr;

    /// The curriculum as last received. Named apart from `lessons_`, which is the
    /// combo box that shows it: one is the data and one is the widget, and a
    /// single name for both is how a panel ends up assigning one to the other.
    QList<timewarp::bridge::LessonSummary> curriculum_;
    QList<timewarp::bridge::LessonProgress> rows_;
    /// Points asked for and answered, per lesson id, so hints come out in order.
    int hint_number_ = 0;
    int presentation_font_ = 0;
};

}  // namespace timewarp::ui
