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

#include <QColor>
#include <QDialog>
#include <QFrame>
#include <QHash>
#include <QList>
#include <QPair>
#include <QPlainTextEdit>
#include <QPointF>
#include <QSize>
#include <QString>
#include <QStringList>
#include <QWidget>

#include "timewarp/ui/reports.hpp"
#include "timewarp/canvas/command.hpp"
#include "timewarp/ui/export.hpp"
#include "timewarp/ui/outline.hpp"

class QComboBox;
class QHBoxLayout;
class QLabel;
class QLineEdit;
class QListWidget;
class QProgressBar;
class QPushButton;
class QSlider;
class QSpinBox;
class QTableWidget;
class QTextBrowser;
class QTimer;
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

/// The document outline: the named places in the current program, click to jump.
///
/// A view over `outlineFor`, which is where the parsing lives. It never edits a
/// document: jumping is a request the window answers with `goToLine`, for the
/// same reason the lesson panel does not open tabs.
class OutlinePanel final : public QFrame {
    Q_OBJECT

public:
    explicit OutlinePanel(QWidget* parent = nullptr);

    /// Replaces the list. An empty list shows the empty state rather than a blank
    /// box, because "this program has no procedures" and "this panel is broken"
    /// have to look different.
    void setEntries(const QList<timewarp::ui::OutlineEntry>& entries);
    [[nodiscard]] int entryCount() const;
    /// The line of entry `index`, or 0 when there is none. Public so a test can
    /// ask what a click would do without synthesising one.
    [[nodiscard]] int lineAt(int index) const;
    [[nodiscard]] bool emptyStateVisible() const;

signals:
    void entryActivated(int line);

private:
    QListWidget* list_ = nullptr;
    QLabel* empty_ = nullptr;
};

/// The classroom: the class list, what has been assigned, and the marks.
///
/// A view, like the lesson panel: the roster and the marking live in
/// `classroom_mode.py`, which `teach.py` writes too, and the panel reaches them
/// through the worker's `classroom-roster`/`classroom-marks` requests. Parsing
/// `roster.json` here would be a second reader of one file, and the one that
/// drifts is the one a teacher is looking at.
class ClassroomPanel final : public QFrame {
    Q_OBJECT

public:
    explicit ClassroomPanel(QWidget* parent = nullptr);

    /// Replaces the class list and what it has been set.
    void setRoster(const QList<timewarp::ui::ClassroomStudent>& students,
                   const QStringList& assignedLessons, const QString& note);
    /// Replaces the mark sheet.
    void setMarks(const QList<timewarp::ui::ClassroomMark>& rows);

    /// Reports a request the engine could not answer.
    ///
    /// The rows already on screen are left alone: the last answer the engine gave
    /// is still the best one available, and blanking the class because a refresh
    /// failed would show "nobody" as if that were the truth.
    void setFailure(const QString& request, const QString& error);

    [[nodiscard]] int studentCount() const;
    [[nodiscard]] int markRowCount() const;
    /// True when the empty state is the thing on screen: no students yet.
    [[nodiscard]] bool emptyStateVisible() const;
    /// The folder for the student on mark row `row`, or empty when there is none.
    [[nodiscard]] QString directoryForRow(int row) const;

signals:
    /// The panel wants fresh numbers. Asking rather than polling, because a
    /// classroom file changes when a teacher runs `teach.py`, not on a timer.
    void refreshRequested();
    /// Open one learner's folder in the file manager.
    void openFolderRequested(const QString& path);
    /// Write the whole class to one archive, and read one back. The dialogs are
    /// the window's; the panel only asks, as it does for a folder.
    void bundleRequested();
    void unbundleRequested();

private:
    void updateSummary();

    QLabel* summary_ = nullptr;
    QLabel* empty_ = nullptr;
    QTableWidget* marks_ = nullptr;
    QPushButton* open_ = nullptr;
    QPushButton* refresh_ = nullptr;
    QPushButton* bundle_ = nullptr;
    QPushButton* unbundle_ = nullptr;
    QHash<QString, QString> directories_;
    /// Student id to display name. The mark rows carry ids; a teacher reads names.
    QHash<QString, QString> names_;
    /// The students in the order the engine sent them. `directories_` is a hash
    /// and iterates in an arbitrary order, which would make "the first student"
    /// mean a different person on the next process start.
    QStringList student_order_;
    int student_count_ = 0;
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

/// The function-key bar: three modifier layers, a layer badge and a deck
/// selector. Translated from `ui/function_key_bar.py`.
///
/// The Python's bar was the discoverable half of its keyboard interface: ten
/// keys per layer, each labelled with the key and the thing it does, with a
/// badge that cycled normal -> shift -> control. The Qt port shipped only the
/// normal layer's labels and connected the signals to nothing, so every button
/// was inert. Both halves are fixed here: the bar emits the action's stable id,
/// and `MainWindow` has a dispatcher that performs it.
///
/// The key sequences are the ones this application *actually* answers to, which
/// is not quite the Python's table: F3 is Find Next here (the Python had Open on
/// F3 and no find-next key at all). A bar that advertises a key the window does
/// not answer is worse than no bar, so where the two differ the application's
/// binding wins and the caption says so.
class FunctionKeyBar final : public QFrame {
    Q_OBJECT

public:
    /// Python: `setFixedHeight(30)`.
    static constexpr int kFixedHeight = 30;

    /// The modifier layer the bar is showing.
    enum class Layer {
        Normal,
        Shift,
        Control,
    };

    /// One key as the bar draws it. `keys` is what the user presses, `action`
    /// is the stable id the window dispatches on, `caption` is the word on the
    /// key's face.
    struct Binding {
        QString keys;
        QString action;
        QString caption;
    };

    explicit FunctionKeyBar(QWidget* parent = nullptr);

    /// Replaces the deck choices. The Python ships four workspace decks.
    void setDecks(const QStringList& labels, int current = 0);

    [[nodiscard]] int currentDeck() const;

    /// The bindings on one layer, in key order. Static and public because the
    /// window binds the same table to real shortcuts: one table, so a button and
    /// the key that matches it cannot drift apart.
    [[nodiscard]] static QList<Binding> bindingsFor(Layer layer);

    /// The layer currently shown.
    void setLayer(Layer layer);
    [[nodiscard]] Layer layer() const noexcept { return layer_; }
    /// Normal -> Shift -> Control -> Normal, what the badge does when clicked.
    void cycleLayer();

    /// Every key button, in row order. For a test that walks the layer's slots.
    [[nodiscard]] QList<QPushButton*> keyButtons() const;
    /// The badge that cycles the layer.
    [[nodiscard]] QPushButton* layerBadge() const noexcept { return layer_badge_; }

signals:
    /// Emitted with the action name of the pressed key, e.g. `"save"`.
    void actionTriggered(const QString& action);
    /// Emitted with the deck's stable id, e.g. `"debugger_deck"`.
    void deckSelected(const QString& deckId);
    /// Emitted after the layer changes, carrying the new layer as an int.
    void layerChanged(int layer);

private:
    void rebuildKeys();

    QComboBox* decks_ = nullptr;
    QPushButton* layer_badge_ = nullptr;
    QWidget* keys_ = nullptr;
    QHBoxLayout* keys_layout_ = nullptr;
    Layer layer_ = Layer::Normal;
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

    /// The overlay's five effects, each 0..100. Zero everywhere paints nothing at
    /// all, which is what makes "off" exact rather than nearly-nothing.
    struct Settings {
        int scanlines = 15;
        int vignette = 0;
        /// A rounded, darkened border suggesting a tube's curved face. Not a real
        /// warp: distorting the pixels underneath would mean rendering the window
        /// through a shader, and the honest version of the effect is a bezel.
        int curvature = 0;
        /// A slow brightness breath. Subtle by design -- a flicker a learner notices
        /// while reading is a flicker that hurts.
        int flicker = 0;
        /// A tint over everything, the amber or green cast of a phosphor monitor.
        int phosphor = 0;
        QColor phosphor_colour{51, 255, 102};
    };

    void setSettings(const Settings& settings);
    [[nodiscard]] const Settings& settings() const noexcept { return settings_; }

    /// 0..100. Zero disables scanlines without hiding the widget. Kept as its own
    /// accessor because the View menu toggles the overlay's visibility, not this.
    void setScanlineIntensity(int percent);
    [[nodiscard]] int scanlineIntensity() const noexcept;

    /// The widget this overlay tracks, or null if it has been destroyed.
    [[nodiscard]] QWidget* target() const noexcept;

protected:
    bool eventFilter(QObject* watched, QEvent* event) override;
    void paintEvent(QPaintEvent* event) override;

private:
    void syncToTarget();
    void paintScanlines(QPainter& painter);
    void paintVignette(QPainter& painter);
    void paintCurvature(QPainter& painter);
    void paintPhosphor(QPainter& painter);

    QWidget* target_ = nullptr;
    Settings settings_;
    /// Flicker needs a clock; the other four are still.
    QTimer* flicker_timer_ = nullptr;
    int flicker_phase_ = 0;
};

/// The CRT settings dialog: five sliders and a live preview.
///
/// A dialog rather than panel controls because the effect is set once and then
/// forgotten, and because the preview has to be the real overlay: a settings
/// window that shows its own mock-up can be right while the window is wrong.
class CrtSettingsDialog final : public QDialog {
    Q_OBJECT

public:
    explicit CrtSettingsDialog(const CrtOverlay::Settings& start, QWidget* parent = nullptr);

    [[nodiscard]] CrtOverlay::Settings settings() const noexcept { return settings_; }

signals:
    /// Emitted on every slider move, so the window can paint the change before the
    /// user decides. `rejected` is the window's cue to put the old values back.
    void previewRequested(const CrtOverlay::Settings& settings);

private:
    CrtOverlay::Settings settings_;
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
    void setLessons(const QList<timewarp::ui::LessonSummary>& lessons);
    /// Applies the learner's place in every lesson.
    void setProgress(const QList<timewarp::ui::LessonProgress>& rows, int completed,
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

    /// The current lesson as a session report sees it: the lesson, every
    /// checkpoint, and the verdict each was given in this session. The program and
    /// the console are the window's to add; they belong to the document and the
    /// run, not to the lesson.
    [[nodiscard]] timewarp::ui::exporter::LessonSession session() const;

    /// The colours a pass and a failure are written in. The window sets them
    /// from the theme: green-on-white is unreadable on the green-phosphor and
    /// amber themes, and a verdict nobody can read is the one message that has to
    /// be legible.
    void setVerdictColors(const QColor& passed, const QColor& failed);

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
    /// Theme colours for the verdict. Defaults match the light-theme pair this
    /// panel shipped with, so it is legible before a theme is applied.
    QColor verdict_pass_{0x2e, 0x7d, 0x32};
    QColor verdict_fail_{0xb3, 0x26, 0x1e};
    /// What the last verdict said, so a theme change mid-session repaints it in
    /// the new colours rather than leaving the old theme's green on screen.
    bool verdict_passed_ = true;
    void refreshVerdictColour();

    /// The curriculum as last received. Named apart from `lessons_`, which is the
    /// combo box that shows it: one is the data and one is the widget, and a
    /// single name for both is how a panel ends up assigning one to the other.
    QList<timewarp::ui::LessonSummary> curriculum_;
    QList<timewarp::ui::LessonProgress> rows_;
    /// What each checkpoint was given this session. The panel shows one verdict at
    /// a time, which is right while a learner is working; a session report is the
    /// one place where all of them matter, and only the panel sees them arrive.
    struct SessionVerdict {
        bool passed = false;
        QString message;
    };
    /// Lesson id to checkpoint index to verdict, so a report can list every
    /// checkpoint that was tried rather than only the one on screen.
    QHash<QString, QHash<int, SessionVerdict>> verdicts_;
    /// Points asked for and answered, per lesson id, so hints come out in order.
    int hint_number_ = 0;
    int presentation_font_ = 0;
};

/// The variables table.
///
/// Stands in for `ui/variable_inspector.py`. The rows come from whoever ran the
/// program -- the native runner's snapshot for every in-process language, or the
/// reference worker's report for a lesson check -- so the panel is a table and
/// nothing else. It does not ask, and it does not decide what a value is worth:
/// the engine does.
class VariablesPanel final : public QFrame {
    Q_OBJECT

public:
    explicit VariablesPanel(QWidget* parent = nullptr);

    /// Replaces the table. The filter is re-applied, because a filter that
    /// survived a new set of names would show an empty table with no explanation.
    void setRows(const QList<timewarp::ui::VariableRow>& rows);

    /// Rows given, and rows showing after the filter. Both, because a test that
    /// only knew one of them could not tell "filtered out" from "never arrived".
    [[nodiscard]] int rowCount() const;
    [[nodiscard]] int visibleRowCount() const;
    /// The filter, so a test can type into it.
    void setFilter(const QString& text);

private:
    void applyFilter();

    QLineEdit* filter_ = nullptr;
    QTableWidget* table_ = nullptr;
    QLabel* status_ = nullptr;
    QList<timewarp::ui::VariableRow> rows_;
};

/// The telemetry HUD: labelled counters for the run that just finished.
///
/// Stands in for `ui/telemetry_hud.py`. The window composes the rows from two
/// sources -- the engine's own counters and the ones only the IDE knows (wall
/// clock, how much ink reached the canvas) -- and the panel does not care which
/// is which.
class TelemetryPanel final : public QFrame {
    Q_OBJECT

public:
    explicit TelemetryPanel(QWidget* parent = nullptr);

    void setRows(const QList<timewarp::ui::TelemetryRow>& rows);

    [[nodiscard]] int rowCount() const;
    /// The value shown for `label`, or an empty string. For tests: a HUD is read
    /// by looking at it, so the assertion has to name the cell.
    [[nodiscard]] QString valueOf(const QString& label) const;

private:
    QTableWidget* table_ = nullptr;
};

/// The debug panel: breakpoints, stepping, the call stack and the frame's
/// variables.
///
/// Stands in for `ui/debug_panel.py`, and unlike the other two it is mostly
/// *controls*: the runner already knows how to pause, step and report, and this
/// is the surface that asks it to. It holds no debugger and no document -- every
/// button is a signal, and the window decides what the request means.
///
/// Only Scheme can be stepped: every other language runs to completion with no
/// pause protocol, so the panel disables itself and says why rather than
/// offering buttons that do nothing.
class DebugPanel final : public QFrame {
    Q_OBJECT

public:
    /// One frame of the call stack, for the stack list.
    struct Frame {
        QString procedure;
        int line = 0;
    };

    explicit DebugPanel(QWidget* parent = nullptr);

    /// Whether the running language can be stepped at all. `reason` is shown when
    /// it cannot, and is the sentence a user reads instead of a dead button.
    void setAvailable(bool available, const QString& reason);
    [[nodiscard]] bool isAvailable() const;

    void setPaused(bool paused, int line, const QString& procedure);
    void setStack(const QList<Frame>& frames);
    void setVariables(const QList<timewarp::ui::VariableRow>& rows);
    void setBreakpoints(const QList<int>& lines);
    [[nodiscard]] QList<int> breakpoints() const;

    /// One evaluated watch, as the window computed it. The panel does not
    /// evaluate: the engine that owns the bindings does, so `X * 2` means what it
    /// means in the program.
    struct WatchRow {
        QString expression;
        QString value;
        bool ok = true;
        QString error;
    };

    /// Replaces the watch list. Each row's value is left as "—" until the window
    /// has bindings to evaluate against.
    void setWatches(const QStringList& expressions);
    [[nodiscard]] QStringList watches() const;
    void setWatchValues(const QList<WatchRow>& values);

    /// Replaces the set of variables whose change pauses the run.
    void setDataBreakpoints(const QStringList& names);
    [[nodiscard]] QStringList dataBreakpoints() const;

    /// The line under the status that says *why* a pause happened, e.g. "X
    /// changed". Empty for an ordinary breakpoint.
    void setPauseReason(const QString& reason);

    /// The line the "add" spinner is on, and a way to set it from the editor's
    /// cursor without the panel knowing what an editor is.
    void setLineNumber(int line);
    [[nodiscard]] int lineNumber() const;

    /// Adds `line` if it is not in the set and removes it if it is. What Ctrl+F8
    /// does: the same edit the panel's own add/remove buttons make, applied to
    /// the cursor's line, so the key cannot produce a set the buttons could not.
    void toggleBreakpointAt(int line);

    /// Emit the matching request, exactly as the panel's own button does. The
    /// function-key bar and the command palette go through these so a key is not
    /// a second implementation of a control.
    void requestContinue();
    void requestPause();
    void requestStepInto();
    void requestStepOver();
    void requestStepOut();
    void requestStop();

signals:
    void continueRequested();
    void pauseRequested();
    void stepIntoRequested();
    void stepOverRequested();
    void stepOutRequested();
    void stopRequested();
    /// The breakpoint set changed. Carries the whole set, so a receiver cannot
    /// apply a delta to a set it thinks it has.
    void breakpointsChanged(const QList<int>& lines);
    /// The watch list changed. Carries the whole list, like the breakpoints.
    void watchesChanged(const QStringList& expressions);
    /// A watched variable was added or removed.
    void dataBreakpointsChanged(const QStringList& names);

private:
    void addBreakpoint();
    void removeBreakpoint();
    void emitBreakpoints();
    void addWatch();
    void removeWatch();
    void addDataBreakpoint(const QString& name = {});
    void removeDataBreakpoint();
    void emitWatches();
    void emitDataBreakpoints();

    QLabel* status_ = nullptr;
    QLabel* reason_ = nullptr;
    QPushButton* continue_ = nullptr;
    QPushButton* pause_ = nullptr;
    QPushButton* step_into_ = nullptr;
    QPushButton* step_over_ = nullptr;
    QPushButton* step_out_ = nullptr;
    QPushButton* stop_ = nullptr;
    QSpinBox* line_ = nullptr;
    QPushButton* add_ = nullptr;
    QPushButton* remove_ = nullptr;
    QListWidget* breakpoints_ = nullptr;
    QListWidget* stack_ = nullptr;
    QTableWidget* variables_ = nullptr;
    QLabel* pause_reason_ = nullptr;
    QLineEdit* watch_input_ = nullptr;
    QTableWidget* watches_ = nullptr;
    QPushButton* watch_add_ = nullptr;
    QPushButton* watch_remove_ = nullptr;
    QLineEdit* data_input_ = nullptr;
    QListWidget* data_breakpoints_ = nullptr;
    QPushButton* data_add_ = nullptr;
    QPushButton* data_remove_ = nullptr;
    bool available_ = false;
};

/// The canvas's own state, as a one-line readout under the drawing: where the
/// turtle is, which way it faces, what the pen is doing and how much the program
/// has drawn.
///
/// A view, not a second copy of the state: the canvas owns the pose, and this is
/// refreshed from `TurtleCanvas::turtleStateChanged`. It sits under the canvas
/// rather than in a dock for the reason an inspector exists at all -- a learner
/// watching the turtle turn cannot turn away to read it.
class TurtleInspector final : public QFrame {
    Q_OBJECT

public:
    explicit TurtleInspector(QWidget* parent = nullptr);

    void showPose(const QPointF& position, double heading, bool pen_down,
                  const QColor& pen_colour, double pen_width);
    void setCommandCount(int count);
    /// The state of a canvas that has not been drawn on.
    void clear();

    /// The two halves of the strip, so a test can read what a user reads without
    /// reaching into the labels.
    [[nodiscard]] QString poseText() const;
    [[nodiscard]] QString penText() const;
    [[nodiscard]] int commandCount() const noexcept { return commands_; }

private:
    QLabel* pose_ = nullptr;
    QLabel* pen_ = nullptr;
    QLabel* commands_label_ = nullptr;
    int commands_ = 0;
};

/// The drawing commands the canvas has received, in the order it received them.
///
/// The canvas is a display list, so "why does my picture look like that" is
/// answered by the sequence that built it -- and the last few hundred commands
/// are the part anybody actually reads. The log is capped for that reason: a
/// program may emit tens of thousands, and an uncapped log would cost more than
/// the drawing it describes.
class TurtleCommandLog final : public QPlainTextEdit {
    Q_OBJECT

public:
    explicit TurtleCommandLog(QWidget* parent = nullptr);

    /// One line per command, numbered from 1. Past `kMaxLines` the oldest go.
    void appendCommand(const timewarp::canvas::CanvasCommandView& view);
    void clearLog();
    [[nodiscard]] int commandCount() const noexcept { return count_; }

    /// How one command is spelled, without appending it. Public so a test can
    /// assert the spelling of every kind without filling a widget.
    [[nodiscard]] static QString describe(const timewarp::canvas::CanvasCommandView& view);

    /// Lines kept. Enough for a looping program's last spiral, small enough that
    /// the log stays a log.
    static constexpr int kMaxLines = 2000;

private:
    int count_ = 0;
};

}  // namespace timewarp::ui
