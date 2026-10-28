// SPDX-License-Identifier: MIT
//
// The panels: lessons, classroom, the inspectors, the outline, SQL.

#include "ui_layout/support.hpp"

#include <QHostAddress>
#include <QTcpServer>
#include <QTcpSocket>

namespace tw::ui_layout {

void test_lesson_panel_contract() {
    announce("lesson panel");
    const std::unique_ptr<MainWindow> window = shownWindow();
    LessonPanel* panel = window->lessonPanel();
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }

    TW_CHECK_EQ(panel->objectName(), QStringLiteral("lessonPanel"));

    // The parts a learner touches, by object name: these are the names the
    // stylesheet and any screenshot tool will use, so they are a contract rather
    // than an implementation detail.
    TW_CHECK(panel->findChild<QComboBox*>(QStringLiteral("lessonChooser")) != nullptr);
    TW_CHECK(panel->findChild<QListWidget*>(QStringLiteral("lessonCheckpoints")) != nullptr);
    TW_CHECK(panel->findChild<QLabel*>(QStringLiteral("lessonVerdict")) != nullptr);
    TW_CHECK(panel->findChild<QProgressBar*>(QStringLiteral("lessonProgress")) != nullptr);
    TW_CHECK(panel->findChild<QTextBrowser*>(QStringLiteral("lessonDetail")) != nullptr);
    for (const char* name : {"lessonOpenButton", "lessonCheckButton", "lessonHandInButton",
                             "lessonHintButton", "lessonSolutionButton", "lessonResetButton"}) {
        TW_CHECK(panel->findChild<QPushButton*>(QString::fromLatin1(name)) != nullptr);
    }

    // Empty until the curriculum arrives. The panel does *not* ask for it itself:
    // loading it at start-up would make every user pay for a Python process, and
    // the window asks when the tab is first shown.
    TW_CHECK_EQ(panel->lessonCount(), 0);
    TW_CHECK(panel->currentLesson().isEmpty());
    TW_CHECK_EQ(panel->currentCheckpoint(), -1);
    TW_CHECK(panel->currentStarter().isEmpty());
    TW_CHECK(panel->verdictText().isEmpty());

    // It must survive being squeezed, or a splitter can reduce it to nothing and
    // there is no way back.
    TW_CHECK(panel->minimumSizeHint().width() >= LessonPanel::kMinimumWidth);

    TW_CHECK(window->rightTabs()->widget(4) == panel);
    TW_CHECK_EQ(window->rightTabs()->tabText(4), QString::fromUtf8("\U0001F393 Lessons"));
}

// Opening the Lessons tab loads the curriculum *in process*. This is the test
// that proves the panel is no longer a client of the Python worker: no bridge is
// started here, and the twenty lessons still arrive.

void test_the_lesson_panel_loads_the_curriculum_without_a_bridge() {
    announce("lessons load without the bridge");
    tw::test::TempDir directory;
    TW_CHECK(directory.valid());
    if (!directory.valid()) {
        return;
    }
    directory.useAsDataDir();
    {
        const std::unique_ptr<MainWindow> window = shownWindow();
        LessonPanel* panel = window->lessonPanel();
        TW_CHECK(panel != nullptr);
        if (panel != nullptr) {
            // Raising the tab is what makes the window ask: the panel itself does
            // not load anything at start-up.
            window->rightTabs()->setCurrentIndex(4);
            TW_CHECK_EQ(panel->lessonCount(), 22);
            TW_CHECK(!panel->currentLesson().isEmpty());
            // The first lesson's starter is a document the learner can open.
            TW_CHECK(!panel->currentStarter().isEmpty());
        }
    }
    directory.restoreDataDir();
}

// The panel keeps every verdict this session, not only the one on screen:
// session report lists what was tried, and the panel is the one place a verdict
// arrives. It has no bridge here -- these are the setters the worker's answers
// call.

void test_lesson_panel_records_the_session() {
    announce("lesson session record");
    const std::unique_ptr<MainWindow> window = shownWindow();
    LessonPanel* panel = window->lessonPanel();
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }

    timewarp::ui::LessonSummary lesson;
    lesson.id = QStringLiteral("basic_first_steps");
    lesson.title = QStringLiteral("First Steps in BASIC");
    lesson.language = QStringLiteral("BASIC");
    lesson.description = QStringLiteral("Print something.");
    lesson.objectives = QStringList{QStringLiteral("Use PRINT")};
    for (int index = 0; index < 3; ++index) {
        timewarp::ui::LessonCheckpoint checkpoint;
        checkpoint.title = QStringLiteral("Checkpoint %1").arg(index + 1);
        checkpoint.description = QStringLiteral("Do the thing.");
        lesson.checkpoints.append(checkpoint);
    }
    timewarp::ui::LessonProgress row;
    row.id = lesson.id;
    row.status = QStringLiteral("in_progress");
    row.checkpoint = 0;
    row.checkpoints = 3;
    panel->setLessons({lesson});
    panel->setProgress({row}, 0, 1);

    const auto before = panel->session();
    TW_CHECK_EQ(before.lessonTitle, lesson.title);
    TW_CHECK_EQ(before.language, lesson.language);
    TW_CHECK_EQ(before.status, QStringLiteral("in_progress"));
    TW_CHECK_EQ(before.objectives, lesson.objectives);
    TW_CHECK_EQ(before.checkpoints.size(), 3);
    for (const auto& checkpoint : before.checkpoints) {
        TW_CHECK(!checkpoint.checked);
    }

    // A pass advances the panel to the next checkpoint. The record of that pass
    // survives the move, and a verdict for a checkpoint that is no longer on
    // screen is still recorded -- it is a verdict, not a display.
    panel->setChecked(lesson.id, 0, true, QStringLiteral("Well done."), false, 0, 1);
    panel->setChecked(lesson.id, 2, false, QStringLiteral("Try again."), false, 0, 1);
    const auto recorded = panel->session();
    TW_CHECK_EQ(recorded.current, 1);
    TW_CHECK(recorded.checkpoints.at(0).checked);
    TW_CHECK(recorded.checkpoints.at(0).passed);
    TW_CHECK_EQ(recorded.checkpoints.at(0).message, QStringLiteral("Well done."));
    TW_CHECK(!recorded.checkpoints.at(1).checked);
    TW_CHECK(recorded.checkpoints.at(2).checked);
    TW_CHECK(!recorded.checkpoints.at(2).passed);
    TW_CHECK_EQ(recorded.checkpoints.at(2).message, QStringLiteral("Try again."));
}

// The classroom page: a class list and a row of marks per learner. It has no
// bridge here -- these are the panel's own setters, which is what the worker's
// answers end up calling.

void test_classroom_panel_contract() {
    announce("classroom panel");
    const std::unique_ptr<MainWindow> window = shownWindow();
    timewarp::ui::ClassroomPanel* panel = window->classroomPanel();
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }

    TW_CHECK_EQ(panel->objectName(), QStringLiteral("classroomPanel"));
    TW_CHECK(window->rightTabs()->widget(5) == panel);
    TW_CHECK_EQ(window->rightTabs()->tabText(5), QString::fromUtf8("\U0001F3EB Classroom"));

    // No class list is the state a fresh installation is in, and it has to say so
    // rather than show an empty table that looks like a class of nobody.
    TW_CHECK_EQ(panel->studentCount(), 0);
    TW_CHECK_EQ(panel->markRowCount(), 0);
    TW_CHECK(panel->emptyStateVisible());
    TW_CHECK(panel->findChild<QTableWidget*>(QStringLiteral("classroomMarks")) != nullptr);
    TW_CHECK(panel->findChild<QLabel*>(QStringLiteral("classroomEmptyState")) != nullptr);
    for (const char* name : {"classroomOpenFolder", "classroomRefresh", "classroomSummary",
                             "classroomBundle", "classroomUnbundle"}) {
        TW_CHECK(panel->findChild<QWidget*>(QString::fromLatin1(name)) != nullptr);
    }
    // Nothing to carry until there is a class; an archive to unpack is exactly how
    // a class arrives, so that button is always there.
    auto* save_bundle = panel->findChild<QPushButton*>(QStringLiteral("classroomBundle"));
    auto* load_bundle = panel->findChild<QPushButton*>(QStringLiteral("classroomUnbundle"));
    TW_CHECK(save_bundle != nullptr);
    TW_CHECK(load_bundle != nullptr);
    if (save_bundle != nullptr) {
        TW_CHECK(!save_bundle->isEnabled());
    }
    if (load_bundle != nullptr) {
        TW_CHECK(load_bundle->isEnabled());
    }

    QList<timewarp::ui::ClassroomStudent> students;
    students.append(timewarp::ui::ClassroomStudent{
        QStringLiteral("ada-lovelace"), QStringLiteral("Ada Lovelace"),
        QStringLiteral("/tmp/class/students/ada-lovelace")});
    students.append(timewarp::ui::ClassroomStudent{
        QStringLiteral("alan-turing"), QStringLiteral("Alan Turing"),
        QStringLiteral("/tmp/class/students/alan-turing")});
    panel->setRoster(students, {QStringLiteral("basic_first_steps")},
                     QStringLiteral("Chapter one"));
    TW_CHECK_EQ(panel->studentCount(), 2);
    TW_CHECK(!panel->emptyStateVisible());
    if (save_bundle != nullptr) {
        TW_CHECK(save_bundle->isEnabled());
    }
    // With nothing marked yet, Open Folder still has somewhere to go: the first
    // learner's directory is the useful default for a teacher looking at an
    // unmarked class.
    TW_CHECK_EQ(panel->directoryForRow(-1),
                QStringLiteral("/tmp/class/students/ada-lovelace"));

    QList<timewarp::ui::ClassroomMark> rows;
    rows.append(timewarp::ui::ClassroomMark{
        QStringLiteral("alan-turing"), QStringLiteral("basic_first_steps"),
        QStringLiteral("First steps"), 3, 3, 2, false});
    panel->setMarks(rows);
    TW_CHECK_EQ(panel->markRowCount(), 1);
    // The folder follows the *row*, not the class list order: a teacher clicking
    // Alan's mark must not be shown Ada's work.
    TW_CHECK_EQ(panel->directoryForRow(0),
                QStringLiteral("/tmp/class/students/alan-turing"));
    // A row is four readable cells, and "2/3" is what a teacher reads rather than
    // two numbers in two columns to be added up mentally.
    auto* table = panel->findChild<QTableWidget*>(QStringLiteral("classroomMarks"));
    TW_CHECK(table != nullptr);
    if (table != nullptr) {
        // The name, not the id: the mark rows carry ids and the roster carries
        // names, and a teacher reads the mark sheet.
        TW_CHECK_EQ(table->item(0, 0)->text(), QStringLiteral("Alan Turing"));
        TW_CHECK_EQ(table->item(0, 1)->text(), QStringLiteral("First steps"));
        TW_CHECK_EQ(table->item(0, 2)->text(), QStringLiteral("3/3"));
        TW_CHECK_EQ(table->item(0, 3)->text(), QStringLiteral("2/3"));
    }

    // A refresh that brings a new name updates the row rather than leaving the
    // old one: the mark row and the roster are two answers to "who is this?".
    students[1].name = QStringLiteral("Alan M. Turing");
    panel->setRoster(students, {QStringLiteral("basic_first_steps")},
                     QStringLiteral("Chapter one"));
    if (table != nullptr) {
        TW_CHECK_EQ(table->item(0, 0)->text(), QStringLiteral("Alan M. Turing"));
    }

    // A failure is said in the summary, and the last answer stays on screen: a
    // refresh the engine could not answer must not look like an empty class.
    panel->setFailure(QStringLiteral("classroom-marks"),
                      QStringLiteral("something went wrong"));
    auto* summary = panel->findChild<QLabel*>(QStringLiteral("classroomSummary"));
    TW_CHECK(summary != nullptr);
    if (summary != nullptr) {
        TW_CHECK(summary->text().contains(QStringLiteral("classroom-marks")));
        TW_CHECK(summary->text().contains(QStringLiteral("something went wrong")));
    }
    TW_CHECK_EQ(panel->studentCount(), 2);
    TW_CHECK(!panel->emptyStateVisible());

    // And a roster with nobody on it goes back to the empty state, because that
    // is what the engine reports for an empty class.
    panel->setRoster({}, {}, {});
    TW_CHECK_EQ(panel->studentCount(), 0);
    TW_CHECK(panel->emptyStateVisible());
    if (save_bundle != nullptr) {
        TW_CHECK(!save_bundle->isEnabled());
    }
}


void test_variables_panel_contract() {
    announce("variables panel");
    const std::unique_ptr<MainWindow> window = shownWindow();
    VariablesPanel* panel = window->variablesPanel();
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }
    TW_CHECK_EQ(panel->objectName(), QStringLiteral("variablesPanel"));
    TW_CHECK(window->rightTabs()->widget(1) == panel);

    // The parts a user touches, by object name: the filter, the table and the
    // sentence that explains an empty panel.
    auto* filter = panel->findChild<QLineEdit*>(QStringLiteral("variablesFilter"));
    auto* table = panel->findChild<QTableWidget*>(QStringLiteral("variablesTable"));
    auto* status = panel->findChild<QLabel*>(QStringLiteral("variablesStatus"));
    TW_CHECK(filter != nullptr);
    TW_CHECK(table != nullptr);
    TW_CHECK(status != nullptr);

    // Before a run there is nothing to show, and the panel says so rather than
    // presenting an empty table that reads as "this program has no variables".
    TW_CHECK_EQ(panel->rowCount(), 0);
    TW_CHECK_EQ(panel->visibleRowCount(), 0);
    TW_CHECK(status != nullptr && !status->text().isEmpty());

    const QList<timewarp::ui::VariableRow> rows{
        {QStringLiteral("name"), QStringLiteral("text"), QStringLiteral("ada")},
        {QStringLiteral("repcount"), QStringLiteral("number"), QStringLiteral("4")},
        {QStringLiteral("total"), QStringLiteral("number"), QStringLiteral("12.5")}};
    panel->setRows(rows);
    TW_CHECK_EQ(panel->rowCount(), 3);
    TW_CHECK_EQ(panel->visibleRowCount(), 3);
    TW_CHECK(table != nullptr && table->rowCount() == 3);
    if (table != nullptr) {
        TW_CHECK_EQ(table->item(0, 0)->text(), QStringLiteral("name"));
        TW_CHECK_EQ(table->item(0, 2)->text(), QStringLiteral("ada"));
    }

    // A filter narrows what is *shown* and keeps the set, so clearing it puts the
    // rows back. Hiding rows for good would make the filter destructive.
    panel->setFilter(QStringLiteral("COUNT"));
    TW_CHECK_EQ(panel->visibleRowCount(), 1);
    TW_CHECK_EQ(panel->rowCount(), 3);
    panel->setFilter(QString{});
    TW_CHECK_EQ(panel->visibleRowCount(), 3);

    // A filter that survives a new set of names would show an empty table with no
    // explanation, so setting rows re-applies it rather than keeping a stale one.
    panel->setFilter(QStringLiteral("nothing matches this"));
    panel->setRows(rows);
    TW_CHECK_EQ(panel->visibleRowCount(), 0);
    TW_CHECK_EQ(panel->rowCount(), 3);
    TW_CHECK(status != nullptr && !status->text().isEmpty());
}


void test_telemetry_panel_contract() {
    announce("telemetry panel");
    const std::unique_ptr<MainWindow> window = shownWindow();
    TelemetryPanel* panel = window->telemetryPanel();
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }
    TW_CHECK_EQ(panel->objectName(), QStringLiteral("telemetryPanel"));
    TW_CHECK(window->rightTabs()->widget(2) == panel);
    TW_CHECK(panel->findChild<QTableWidget*>(QStringLiteral("telemetryTable")) != nullptr);

    // A HUD over a run that has not happened has no counters, and no invented
    // ones either.
    TW_CHECK_EQ(panel->rowCount(), 0);
    TW_CHECK(panel->valueOf(QStringLiteral("Steps")).isEmpty());

    panel->setRows({{QStringLiteral("Steps"), QStringLiteral("1,180 of 250,000")},
                    {QStringLiteral("Canvas ink"), QStringLiteral("4 segments, 0 shapes")}});
    TW_CHECK_EQ(panel->rowCount(), 2);
    TW_CHECK_EQ(panel->valueOf(QStringLiteral("Steps")),
                QStringLiteral("1,180 of 250,000"));
    TW_CHECK_EQ(panel->valueOf(QStringLiteral("Canvas ink")),
                QStringLiteral("4 segments, 0 shapes"));
    TW_CHECK(panel->valueOf(QStringLiteral("no such label")).isEmpty());
}


void test_debug_panel_controls() {
    announce("debug panel");
    // The depth is a persisted run setting, so an earlier test's choice would
    // make "the default is off" wrong here.
    QSettings{}.remove(QStringLiteral("debug/history"));
    const std::unique_ptr<MainWindow> window = shownWindow();
    DebugPanel* panel = window->debugPanel();
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }
    TW_CHECK_EQ(panel->objectName(), QStringLiteral("debugPanel"));
    TW_CHECK(window->rightTabs()->widget(3) == panel);

    // The controls a debugger needs, by object name.
    for (const char* name : {"debugContinue", "debugPause", "debugStepInto",
                             "debugStepOver", "debugStepOut", "debugStop",
                             "debugAddBreakpoint", "debugRemoveBreakpoint"}) {
        TW_CHECK(panel->findChild<QPushButton*>(QString::fromLatin1(name)) != nullptr);
    }
    for (const char* name : {"debugLine", "debugBreakpoints", "debugStack",
                             "debugVariables", "debugStatus", "debugReason"}) {
        TW_CHECK(panel->findChild<QWidget*>(QString::fromLatin1(name)) != nullptr);
    }

    // Breakpoints are a set of lines, and the panel reports the whole set rather
    // than a delta, so nothing downstream has to keep its own copy in step.
    auto* line = panel->findChild<QSpinBox*>(QStringLiteral("debugLine"));
    TW_CHECK(line != nullptr);
    if (line != nullptr) {
        panel->setLineNumber(12);
        TW_CHECK_EQ(panel->lineNumber(), 12);
        TW_CHECK_EQ(line->value(), 12);
    }

    QList<int> announced;
    QObject::connect(panel, &DebugPanel::breakpointsChanged,
                     [&announced](const QList<int>& lines) { announced = lines; });
    auto* add = panel->findChild<QPushButton*>(QStringLiteral("debugAddBreakpoint"));
    auto* remove = panel->findChild<QPushButton*>(QStringLiteral("debugRemoveBreakpoint"));
    TW_CHECK(add != nullptr && remove != nullptr);

    // Every language can stop now -- at a statement, a message send or an
    // instruction -- so the controls stay live whichever document is open. The
    // call stack remains the Scheme machine's own, which is a display difference
    // rather than a disabled button.
    auto* combo = window->findChild<QComboBox*>(QStringLiteral("languageSelector"));
    TW_CHECK(combo != nullptr);
    if (combo != nullptr) {
        const int scheme = combo->findData(QStringLiteral("scheme"));
        const int basic = combo->findData(QStringLiteral("basic"));
        TW_CHECK(scheme >= 0);
        TW_CHECK(basic >= 0);
        if (scheme >= 0 && basic >= 0) {
            combo->setCurrentIndex(basic);
            TW_CHECK(panel->isAvailable());
            TW_CHECK(add != nullptr && add->isEnabled());
            combo->setCurrentIndex(scheme);
            TW_CHECK(panel->isAvailable());
            TW_CHECK(add != nullptr && add->isEnabled());
        }
    }

    if (add != nullptr && remove != nullptr) {
        panel->setLineNumber(7);
        add->click();
        TW_CHECK_EQ(announced, QList<int>{7});
        TW_CHECK_EQ(panel->breakpoints(), QList<int>{7});
        // Adding the same line twice is a set, not two entries.
        add->click();
        TW_CHECK_EQ(panel->breakpoints().size(), 1);
        remove->click();
        TW_CHECK(panel->breakpoints().isEmpty());
        TW_CHECK(announced.isEmpty());
    }

    // A paused run shows where, and an idle one does not claim a line.
    auto* status = panel->findChild<QLabel*>(QStringLiteral("debugStatus"));
    TW_CHECK(status != nullptr);
    panel->setPaused(true, 9, QStringLiteral("area"));
    TW_CHECK(status != nullptr && status->text().contains(QStringLiteral("9")));
    TW_CHECK(status != nullptr && status->text().contains(QStringLiteral("area")));
    panel->setPaused(false, 0, QString{});
    // "not paused" rather than "running": the panel cannot tell a run in
    // progress from one that has finished, so it says only what it knows.
    TW_CHECK(status != nullptr && !status->text().contains(QStringLiteral("area")));
    TW_CHECK(status != nullptr && status->text() != QStringLiteral("running"));

    // Walking a run back is offered only when it can happen: the buttons are dead
    // with nothing paused, and the depth is a run setting rather than a button,
    // because a session started with no history has nothing behind it.
    auto* back = panel->findChild<QPushButton*>(QStringLiteral("debugBack"));
    auto* back_ten = panel->findChild<QPushButton*>(QStringLiteral("debugBackTen"));
    auto* history = panel->findChild<QComboBox*>(QStringLiteral("debugHistory"));
    TW_CHECK(back != nullptr && back_ten != nullptr && history != nullptr);
    TW_CHECK(!panel->isRewindAvailable());
    TW_CHECK(back != nullptr && !back->isEnabled());

    int announced_steps = 0;
    QObject::connect(panel, &DebugPanel::rewindRequested,
                     [&announced_steps](int steps) { announced_steps = steps; });
    int announced_depth = -1;
    QObject::connect(panel, &DebugPanel::historyChanged,
                     [&announced_depth](int depth) { announced_depth = depth; });

    if (history != nullptr) {
        // Off costs nothing and is the default; anything else is a run that can
        // be walked back.
        TW_CHECK_EQ(panel->historyDepth(), 0);
        panel->setHistoryDepth(500);
        TW_CHECK_EQ(panel->historyDepth(), 500);
        TW_CHECK_EQ(announced_depth, 500);
        // A depth the menu does not offer becomes the closest one it does, rather
        // than silently meaning "off".
        panel->setHistoryDepth(1500);
        TW_CHECK_EQ(panel->historyDepth(), 500);
    }

    panel->setPaused(true, 12, QString{});
    panel->setRewindAvailable(true, QString{});
    TW_CHECK(panel->isRewindAvailable());
    if (back != nullptr && back_ten != nullptr) {
        TW_CHECK(back->isEnabled());
        back->click();
        TW_CHECK_EQ(announced_steps, 1);
        back_ten->click();
        TW_CHECK_EQ(announced_steps, 10);
    }
    // A disabled button emits nothing, so the window never has to ask whether the
    // engine is in a state to answer.
    panel->setRewindAvailable(false, QStringLiteral("Pause the run first"));
    if (back != nullptr) {
        TW_CHECK(!back->isEnabled());
        announced_steps = 0;
        back->click();
        TW_CHECK_EQ(announced_steps, 0);
    }
    // Leaving the pause puts the buttons back to dead by itself.
    panel->setRewindAvailable(true, QString{});
    panel->setPaused(false, 0, QString{});
    TW_CHECK(!panel->isRewindAvailable());
    if (back != nullptr) {
        TW_CHECK(!back->isEnabled());
    }

    // The stack and the frame's variables.
    panel->setStack({DebugPanel::Frame{QStringLiteral("<program>"), 1},
                     DebugPanel::Frame{QStringLiteral("area"), 9}});
    auto* stack = panel->findChild<QListWidget*>(QStringLiteral("debugStack"));
    TW_CHECK(stack != nullptr && stack->count() == 2);
    if (stack != nullptr && stack->count() == 2) {
        TW_CHECK(stack->item(0)->text().contains(QStringLiteral("<program>")));
        TW_CHECK(stack->item(1)->text().contains(QStringLiteral("area")));
    }
    panel->setVariables(
        {{QStringLiteral("w"), QStringLiteral("number"), QStringLiteral("3")}});
    auto* variables = panel->findChild<QTableWidget*>(QStringLiteral("debugVariables"));
    TW_CHECK(variables != nullptr && variables->rowCount() == 1);
    if (variables != nullptr && variables->rowCount() == 1) {
        TW_CHECK_EQ(variables->item(0, 0)->text(), QStringLiteral("w"));
        TW_CHECK_EQ(variables->item(0, 1)->text(), QStringLiteral("3"));
    }
}


void test_the_outline_lists_and_jumps() {
    announce("outline panel");
    const std::unique_ptr<MainWindow> window = shownWindow();
    auto* panel = window->findChild<timewarp::ui::OutlinePanel*>(
        QStringLiteral("outlinePanel"));
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }
    // The dock holds the real panel now, not the placeholder it started as.
    TW_CHECK_EQ(window->outlineDock()->widget(), panel);

    CodeEditor* editor = window->currentEditor();
    TW_CHECK(editor != nullptr);
    if (editor == nullptr) {
        return;
    }
    // Text first, then the language: setting the text marks the document
    // modified, and the window re-derives the language from the path (a scratch
    // buffer is BASIC) when that happens.
    editor->setPlainText(QStringLiteral("TO SQUARE\n  REPEAT 4 [ FORWARD 50 RIGHT 90 ]\nEND\n"
                                        "TO TRI\n  REPEAT 3 [ FORWARD 50 RIGHT 120 ]\nEND\n"));
    editor->setLanguage(Language::Logo);
    // The edit restarted the debounce, which is the wiring an edit's own test
    // cannot see; `refreshOutline` is what the timer would have called.
    auto* timer = window->findChild<QTimer*>(QStringLiteral("outlineTimer"));
    TW_CHECK(timer != nullptr);
    if (timer != nullptr) {
        TW_CHECK(timer->isActive());
    }
    window->refreshOutline();
    // The ordering above is load-bearing: the edit re-derives the language, so the
    // switch comes after it.
    TW_CHECK_EQ(static_cast<int>(editor->language()), static_cast<int>(Language::Logo));
    TW_CHECK_EQ(panel->entryCount(), 2);
    TW_CHECK_EQ(panel->lineAt(0), 1);
    TW_CHECK_EQ(panel->lineAt(1), 4);
    TW_CHECK(!panel->emptyStateVisible());

    // Activating an entry is what a click does: it puts the cursor on that line.
    auto* list = panel->findChild<QListWidget*>(QStringLiteral("outlineList"));
    TW_CHECK(list != nullptr);
    if (list != nullptr) {
        QListWidgetItem* item = list->item(1);
        TW_CHECK(item != nullptr);
        if (item != nullptr) {
            TW_CHECK_EQ(item->text(), QStringLiteral("TRI"));
            QMetaObject::invokeMethod(list, "itemActivated", Q_ARG(QListWidgetItem*, item));
        }
    }
    TW_CHECK_EQ(editor->currentLine(), 4);

    // A language with no named places shows the empty state rather than a blank
    // box, and the empty state is a sentence rather than a disabled widget.
    editor->setLanguage(Language::Focal);
    editor->setPlainText(QStringLiteral("1.10 T \"HI\"\n"));
    window->refreshOutline();
    TW_CHECK_EQ(panel->entryCount(), 0);
    TW_CHECK(panel->emptyStateVisible());
}


void test_immediate_mode_panel_contract() {
    announce("immediate mode panel");
    MainWindow window;
    ImmediateModePanel* panel = window.immediateMode();

    TW_CHECK_EQ(panel->objectName(), QStringLiteral("immediateMode"));
    TW_CHECK(panel->minimumHeight() >= ImmediateModePanel::kMinimumHeight);

    // The panel reports the command rather than clearing its own input: only the
    // interpreter can say whether the command was accepted.
    QString received;
    QObject::connect(panel, &ImmediateModePanel::commandSubmitted,
                     [&received](const QString& text) { received = text; });

    auto* input = panel->findChild<QLineEdit*>();
    TW_CHECK(input != nullptr);
    if (input != nullptr) {
        input->setText(QStringLiteral("PRINT 2 + 2"));
        emit input->returnPressed();
        TW_CHECK_EQ(received, QStringLiteral("PRINT 2 + 2"));
        // The panel itself does not clear; the window does once it has handed the
        // command to the REPL. Both behaviours are asserted here because both are
        // part of who owns the input.
        TW_CHECK(panel->command().isEmpty());

        // An empty command is not a command.
        received.clear();
        input->setText(QStringLiteral("   "));
        emit input->returnPressed();
        TW_CHECK(received.isEmpty());
    }
}


void test_sql_panel_runs_and_shows_results() {
    announce("SQL panel");
    MainWindow window;
    SqlPanel* panel = window.sqlPanel();
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }
    TW_CHECK_EQ(panel->objectName(), QStringLiteral("sqlPanel"));
    TW_CHECK(window.showPanel(QStringLiteral("sql")));
    TW_CHECK(window.rightTabs()->widget(6) == panel);

    panel->setDatabase(QStringLiteral(":memory:"), {QStringLiteral("scores")});
    TW_CHECK_EQ(panel->tableCount(), 1);
    panel->setQuery(QStringLiteral("SELECT * FROM scores;"));
    TW_CHECK_EQ(panel->query(), QStringLiteral("SELECT * FROM scores;"));

    QString submitted;
    QObject::connect(panel, &SqlPanel::runRequested,
                     [&submitted](const QString& sql) { submitted = sql; });
    auto* run = panel->findChild<QPushButton*>(QStringLiteral("sqlRun"));
    TW_CHECK(run != nullptr);
    if (run != nullptr) {
        run->click();
        TW_CHECK_EQ(submitted, QStringLiteral("SELECT * FROM scores;"));
    }

    timewarp::sql::QueryResult result;
    result.columns = QStringList{QStringLiteral("name"), QStringLiteral("score")};
    result.rows = {{QStringLiteral("Ada"), QStringLiteral("42")}};
    panel->setResult(result);
    TW_CHECK_EQ(panel->resultRowCount(), 1);
    TW_CHECK_EQ(panel->resultColumnCount(), 2);
    TW_CHECK(panel->statusText().contains(QStringLiteral("1 row")));

    timewarp::sql::QueryResult failure;
    failure.error = QStringLiteral("no such table: nope");
    panel->setResult(failure);
    TW_CHECK(panel->statusText().contains(QStringLiteral("no such table")));
}

// The assistant is the one page of the right column that depends on something
// outside this machine, so what is asserted about it is what can be known without
// a network: that it refuses to pretend it is ready, that it says what is missing,
// that configuring it turns it on, and that the settings dialog's presets really
// do fill the fields. Nothing here sends a request.


// The Reference page is a view over `native/share/command_rows.json`, the same table
// `time_warp_gates commands` drives. Two things follow, and both are the point: the
// page can only show commands the gate has run, and a command the gate loses fails
// the build rather than quietly leaving the reference.


// Accessibility starts with a name. A screen reader announces what a control is
// called; a control with no accessible name and no text of its own is announced as
// "button" or "edit", which is the same as not being there.
//
// This case covers the three pages added at 23.1 and 23.2 -- the assistant, its
// settings and the reference -- rather than the whole window, because a gate over
// everything has to be written against what everything is, and that is a pass of its
// own. What it asserts is the invariant worth keeping: a field, list or slider must
// carry a name, and a button or tick must carry one or its own visible text.

namespace {

QStringList unnamedControls(QWidget* page) {
    QStringList anonymous;
    const auto note = [&anonymous](QWidget* widget) {
        if (widget == nullptr || widget->isHidden()) {
            return;
        }
        if (!widget->accessibleName().trimmed().isEmpty()) {
            return;
        }
        // A spin box carries its own line edit; that edit is the spin box to a screen
        // reader, so naming the box is what counts.
        if (qobject_cast<QAbstractSpinBox*>(widget->parentWidget()) != nullptr) {
            return;
        }
        const auto* button = qobject_cast<QAbstractButton*>(widget);
        if (button != nullptr && !button->text().trimmed().isEmpty()) {
            return;
        }
        anonymous << QStringLiteral("%1 (%2)")
                         .arg(widget->objectName().isEmpty()
                                  ? QString::fromLatin1(widget->metaObject()->className())
                                  : widget->objectName(),
                              QString::fromLatin1(widget->metaObject()->className()));
    };
    for (QLineEdit* field : page->findChildren<QLineEdit*>()) {
        note(field);
    }
    for (QComboBox* box : page->findChildren<QComboBox*>()) {
        note(box);
    }
    for (QAbstractSpinBox* spin : page->findChildren<QAbstractSpinBox*>()) {
        note(spin);
    }
    for (QPlainTextEdit* edit : page->findChildren<QPlainTextEdit*>()) {
        note(edit);
    }
    for (QSlider* slider : page->findChildren<QSlider*>()) {
        note(slider);
    }
    for (QAbstractButton* button : page->findChildren<QAbstractButton*>()) {
        note(button);
    }
    return anonymous;
}

}  // namespace

void test_the_new_pages_name_what_they_show() {
    announce("assistant and reference: every control has a name");
    MainWindow window;
    if (CommandReferencePanel* reference = window.commandReferencePanel()) {
        reference->ensureLoaded();
        const QStringList anonymous = unnamedControls(reference);
        if (!anonymous.isEmpty()) {
            std::printf("     reference: %s\n", anonymous.join(", ").toStdString().c_str());
        }
        TW_CHECK(anonymous.isEmpty());
    }
#ifdef TIME_WARP_HAVE_AI
    if (AiPanel* panel = window.aiPanel()) {
        const QStringList anonymous = unnamedControls(panel);
        if (!anonymous.isEmpty()) {
            std::printf("     assistant: %s\n", anonymous.join(", ").toStdString().c_str());
        }
        TW_CHECK(anonymous.isEmpty());
    }
    timewarp::ai::Config config;
    config.base_url = QStringLiteral("https://api.deepseek.com/v1");
    config.model = QStringLiteral("deepseek-chat");
    AiSettingsDialog dialog{config};
    const QStringList anonymous = unnamedControls(&dialog);
    if (!anonymous.isEmpty()) {
        std::printf("     settings: %s\n", anonymous.join(", ").toStdString().c_str());
    }
    TW_CHECK(anonymous.isEmpty());
#endif
}

void test_the_command_reference_lists_the_table() {
    announce("command reference page");
    MainWindow window;
    CommandReferencePanel* panel = window.commandReferencePanel();
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }
    TW_CHECK(window.showPanel(QStringLiteral("reference")));
    TW_CHECK(window.rightTabs()->widget(7) == panel);

    panel->ensureLoaded();
    // The table is the shipped one, and it has rows for every language.
    TW_CHECK(panel->statusText().contains(QStringLiteral("command(s) for")) ||
             panel->statusText().isEmpty());
    panel->setLanguage(timewarp::ui::Language::Logo);
    TW_CHECK(panel->language() == timewarp::ui::Language::Logo);
    const int logo_rows = panel->rowCount();
    TW_CHECK(logo_rows > 10);
    TW_CHECK_EQ(panel->visibleRowCount(), logo_rows);

    // A filter narrows what is shown and says so when it matches nothing.
    panel->setFilter(QStringLiteral("FORWARD"));
    TW_CHECK(panel->visibleRowCount() > 0);
    TW_CHECK(panel->visibleRowCount() < logo_rows);
    TW_CHECK(!panel->selectedSource().isEmpty());
    panel->setFilter(QStringLiteral("zzz-no-such-command"));
    TW_CHECK_EQ(panel->visibleRowCount(), 0);
    TW_CHECK(panel->selectedSource().isEmpty());

    // Try it hands the row's program to the window, which opens it in a document.
    panel->setFilter(QString{});
    panel->setLanguage(timewarp::ui::Language::Basic);
    TW_CHECK(panel->visibleRowCount() > 10);
    TW_CHECK(!panel->selectedSource().isEmpty());
    const int before = window.editorTabs()->count();
    auto* try_button = panel->findChild<QPushButton*>(QStringLiteral("commandReferenceTry"));
    TW_CHECK(try_button != nullptr);
    if (try_button != nullptr) {
        try_button->click();
        TW_CHECK_EQ(window.editorTabs()->count(), before + 1);
        const CodeEditor* opened = window.currentEditor();
        TW_CHECK(opened != nullptr);
        if (opened != nullptr) {
            TW_CHECK(!opened->toPlainText().isEmpty());
        }
    }
}

// A one-answer endpoint on loopback. It speaks just enough HTTP to be a real
// endpoint to Qt's network stack -- headers, then an OpenAI-compatible stream -- so
// the assistant's whole path (compose, send, read the pieces, tally the tokens) runs
// with no network and no key. A test that stopped at the button would not notice a
// panel that sends and then discards what comes back.
class StubEndpoint final : public QObject {
public:
    StubEndpoint() {
        QObject::connect(&server_, &QTcpServer::newConnection, this, [this] {
            QTcpSocket* socket = server_.nextPendingConnection();
            QObject::connect(socket, &QTcpSocket::readyRead, this, [this, socket] {
                request_ += socket->readAll();
                const int header_end = request_.indexOf("\r\n\r\n");
                if (header_end < 0) {
                    return;
                }
                int body = 0;
                for (const QByteArray& line : request_.left(header_end).split('\n')) {
                    const QByteArray trimmed = line.trimmed();
                    if (trimmed.toLower().startsWith("content-length:")) {
                        body = trimmed.mid(15).trimmed().toInt();
                    }
                }
                if (request_.size() - (header_end + 4) < body) {
                    return;  // the request is not all here yet
                }
                socket->write(
                    "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n"
                    "Cache-Control: no-cache\r\nConnection: close\r\n\r\n"
                    "data: {\"model\":\"stub-1\",\"choices\":[{\"delta\":{\"content\":\"It \"}}]}\n\n"
                    "data: {\"choices\":[{\"delta\":{\"content\":\"prints.\"}}]}\n\n"
                    "data: {\"choices\":[],\"usage\":{\"prompt_tokens\":5,"
                    "\"completion_tokens\":2}}\n\n"
                    "data: [DONE]\n\n");
                socket->flush();
                socket->disconnectFromHost();
            });
        });
    }

    [[nodiscard]] bool listen() { return server_.listen(QHostAddress::LocalHost, 0); }
    [[nodiscard]] QString baseUrl() const {
        return QStringLiteral("http://127.0.0.1:%1/v1").arg(server_.serverPort());
    }

private:
    QTcpServer server_;
    QByteArray request_;
};

void test_assistant_panel_contract() {
    announce("assistant panel");
    MainWindow window;
#ifdef TIME_WARP_HAVE_AI
    AiPanel* panel = window.aiPanel();
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }
    TW_CHECK_EQ(panel->objectName(), QStringLiteral("aiPanel"));
    TW_CHECK(window.showPanel(QStringLiteral("ai")));
    // The reference page sits between Database and the assistant, so the assistant
    // is the ninth page and the reference is the eighth.
    TW_CHECK(window.rightTabs()->widget(8) == panel);
    TW_CHECK_EQ(window.rightTabs()->tabText(8),
                QString::fromUtf8("\U0001F916 Assistant"));

    // A fresh profile has no endpoint: the panel says what to do about it, and
    // there is nothing to press that would send anything.
    TW_CHECK(!panel->configured());
    TW_CHECK(panel->emptyStateVisible());
    auto* send = panel->findChild<QPushButton*>(QStringLiteral("aiSendButton"));
    TW_CHECK(send != nullptr);
    if (send != nullptr) {
        TW_CHECK(!send->isEnabled());
    }
    TW_CHECK(panel->question() != nullptr);
    if (panel->question() != nullptr) {
        TW_CHECK(!panel->question()->isEnabled());
    }

    // Asking with no endpoint is a sentence rather than a request, and it is not
    // remembered as a turn of a conversation that never happened.
    panel->setConfig(timewarp::ai::Config{});
    panel->ask(timewarp::ai::Task::Explain);
    TW_CHECK(panel->status() != nullptr);
    if (panel->status() != nullptr) {
        TW_CHECK(panel->status()->text().contains(QString::fromUtf8("\u274C")));
    }
    TW_CHECK_EQ(panel->historySize(), 0);
    // The refusal is readable, not just recorded: the transcript holds the sentence
    // rather than the markup that carried it.
    TW_CHECK(panel->transcript() != nullptr);
    if (panel->transcript() != nullptr) {
        TW_CHECK(panel->transcript()->toPlainText().contains(QStringLiteral("switched off")));
        TW_CHECK(!panel->transcript()->toPlainText().contains(QStringLiteral("<p")));
    }

    // Configured: the banner goes, the controls come alive, and the privacy note
    // says which service would receive the program.
    timewarp::ai::Config config;
    config.enabled = true;
    config.base_url = QStringLiteral("https://api.deepseek.com/v1");
    config.model = QStringLiteral("deepseek-chat");
    panel->setConfig(config);
    TW_CHECK(panel->configured());
    TW_CHECK(!panel->emptyStateVisible());
    if (send != nullptr) {
        TW_CHECK(send->isEnabled());
    }
    if (panel->question() != nullptr) {
        TW_CHECK(panel->question()->isEnabled());
    }
    auto* note = panel->findChild<QLabel*>(QStringLiteral("aiPrivacyNote"));
    TW_CHECK(note != nullptr);
    if (note != nullptr) {
        TW_CHECK(note->text().contains(QStringLiteral("DeepSeek")));
        TW_CHECK(note->text().contains(QStringLiteral("Nothing is sent")));
    }

    // A request against a port with nothing listening on it: the whole path from
    // the button to the transcript, with no network and no waiting. Loopback refuses
    // immediately, so this asserts what a unit test of the client cannot see -- that
    // the panel composes, sends, shows the refusal and comes back to life.
    timewarp::ai::Config unreachable = config;
    unreachable.base_url = QStringLiteral("http://127.0.0.1:1/v1");
    unreachable.timeout_seconds = 5;
    panel->setConfig(unreachable);
    panel->ask(timewarp::ai::Task::Explain);
    TW_CHECK(spin_until([panel] { return !panel->busy(); }, 8000));
    if (panel->status() != nullptr) {
        TW_CHECK(panel->status()->text().contains(QString::fromUtf8("\u274C")));
    }
    // The question is remembered even though the answer did not arrive, so the next
    // request still has the document in its history -- and the panel is usable
    // again rather than stuck in "asking".
    TW_CHECK_EQ(panel->historySize(), 1);
    if (panel->transcript() != nullptr) {
        TW_CHECK(panel->transcript()->toPlainText().contains(QStringLiteral("Explain")));
    }
    if (send != nullptr) {
        TW_CHECK(send->isEnabled());
    }
    panel->setConfig(config);

    // Reachable from Tools, one key away, with the two one-click questions beside
    // it. A feature with no menu entry is a feature only its author knows about.
    auto* assistant = window.findChild<QAction*>(QStringLiteral("aiAssistantAction"));
    TW_CHECK(assistant != nullptr);
    if (assistant != nullptr) {
        TW_CHECK_EQ(assistant->shortcut(),
                    timewarp::core::keySequence(Qt::CTRL | Qt::SHIFT, Qt::Key_A));
    }
    for (const char* name :
         {"aiExplainAction", "aiFixAction", "aiDiagnoseAction", "aiSettingsAction"}) {
        TW_CHECK(window.findChild<QAction*>(QString::fromLatin1(name)) != nullptr);
    }

    // A server on this machine is the configuration to recommend to a classroom, so
    // the note beside the question box says that nothing leaves the computer rather
    // than naming a provider and leaving the reader to work out where it is.
    timewarp::ai::Config on_this_machine = config;
    on_this_machine.base_url = QStringLiteral("http://127.0.0.1:11434/v1");
    panel->setConfig(on_this_machine);
    if (note != nullptr) {
        TW_CHECK(note->text().contains(QStringLiteral("nothing leaves the computer")));
    }
    panel->setConfig(config);

    // "Ask about the last error" starts from the run rather than the program: the
    // newest line the Console holds becomes the question.
    auto* console = window.findChild<QPlainTextEdit*>(QStringLiteral("consoleOutput"));
    TW_CHECK(console != nullptr);
    auto* diagnose = window.findChild<QAction*>(QStringLiteral("aiDiagnoseAction"));
    TW_CHECK(diagnose != nullptr);
    if (console != nullptr && diagnose != nullptr) {
        console->setPlainText(QStringLiteral(
            "some output\n\u274C Error at line 20: expected a number"));
        panel->setConfig(unreachable);
        const int before = panel->historySize();
        diagnose->trigger();
        TW_CHECK_EQ(panel->historySize(), before + 1);
        TW_CHECK(spin_until([panel] { return !panel->busy(); }, 8000));
        if (panel->transcript() != nullptr) {
            TW_CHECK(panel->transcript()->toPlainText().contains(QStringLiteral("Diagnose")));
            TW_CHECK(panel->transcript()
                         ->toPlainText()
                         .contains(QStringLiteral("expected a number")));
        }
        console->clear();
        panel->setConfig(config);
    }

    // A conversation that survives a restart, and the tokens it cost: one endpoint
    // on loopback answers both questions with no network and no key.
    StubEndpoint endpoint;
    const bool listening = endpoint.listen();
    TW_CHECK(listening);
    QTemporaryDir conversation_dir;
    TW_CHECK(conversation_dir.isValid());
    if (listening && conversation_dir.isValid()) {
        timewarp::ai::Config live = config;
        live.base_url = endpoint.baseUrl();
        live.timeout_seconds = 10;
        live.remember_conversation = true;
        panel->setConversationDirectory(conversation_dir.path());
        panel->clearConversation();
        panel->setConfig(live);
        panel->ask(timewarp::ai::Task::Explain);
        TW_CHECK(spin_until([panel] { return !panel->busy(); }, 10000));

        // The answer came through the stream, and the counts it carried are the
        // session's tally rather than a number the panel invented.
        TW_CHECK_EQ(panel->sessionUsage().requests, 1);
        TW_CHECK_EQ(panel->sessionUsage().prompt_tokens, 5);
        TW_CHECK_EQ(panel->sessionUsage().completion_tokens, 2);
        auto* usage = panel->findChild<QLabel*>(QStringLiteral("aiUsage"));
        TW_CHECK(usage != nullptr);
        if (usage != nullptr) {
            TW_CHECK(!usage->isHidden());
            TW_CHECK(usage->text().contains(QStringLiteral("5")));
        }
        // The HUD shows the same tally beside the run's own counters.
        TW_CHECK(window.telemetryPanel() != nullptr);
        if (window.telemetryPanel() != nullptr) {
            static_cast<void>(window.showPanel(QStringLiteral("telemetry")));
            TW_CHECK_EQ(window.telemetryPanel()->valueOf(QStringLiteral("Assistant asks")),
                        QStringLiteral("1"));
        }

        // Two turns were kept, and a window opened later starts with them: the
        // question was worth asking again after a restart.
        AiPanel later;
        later.setConversationDirectory(conversation_dir.path());
        timewarp::ai::Config remembering = live;
        later.setConfig(remembering);
        TW_CHECK_EQ(later.historySize(), 2);

        // Turning the switch off deletes what was kept. A setting that says "do not
        // keep this" and leaves the file behind is worse than not having it.
        remembering.remember_conversation = false;
        later.setConfig(remembering);
        AiPanel third;
        third.setConversationDirectory(conversation_dir.path());
        third.setConfig(live);
        TW_CHECK_EQ(third.historySize(), 0);
    }

    // The presets are what makes DeepSeek -- and Ollama, and LM Studio -- an option
    // rather than a URL a user has to know by heart. Choosing one fills the three
    // fields, and the result is what the window would save.
    AiSettingsDialog dialog{config};
    TW_CHECK_EQ(dialog.presetChooser()->currentText(), QStringLiteral("DeepSeek"));
    const int anthropic = dialog.presetChooser()->findText(QStringLiteral("Anthropic"));
    TW_CHECK(anthropic >= 0);
    if (anthropic >= 0) {
        dialog.presetChooser()->setCurrentIndex(anthropic);
        TW_CHECK_EQ(dialog.baseUrlField()->text(),
                    QStringLiteral("https://api.anthropic.com/v1"));
        TW_CHECK_EQ(dialog.modelField()->text(),
                    QStringLiteral("claude-3-5-haiku-latest"));
        TW_CHECK(dialog.config().protocol == timewarp::ai::Protocol::Anthropic);
    }
    // The key is a password until the user says otherwise: a shoulder is cheaper
    // than a screenshot.
    TW_CHECK_EQ(dialog.keyField()->echoMode(), QLineEdit::Password);
    // The two switches that change what the assistant does without being asked: one
    // writes the conversation to a file, the other sends while you type. Both are in
    // the dialog and both are off unless ticked.
    auto* remember = dialog.findChild<QCheckBox*>(QStringLiteral("aiRememberBox"));
    auto* inline_box = dialog.findChild<QCheckBox*>(QStringLiteral("aiInlineBox"));
    TW_CHECK(remember != nullptr);
    TW_CHECK(inline_box != nullptr);
    if (remember != nullptr && inline_box != nullptr) {
        TW_CHECK(!timewarp::ai::Config{}.remember_conversation);
        TW_CHECK(!dialog.config().remember_conversation);
        TW_CHECK(!dialog.config().inline_suggestions);
        remember->setChecked(true);
        inline_box->setChecked(true);
        TW_CHECK(dialog.config().remember_conversation);
        TW_CHECK(dialog.config().inline_suggestions);
    }
    // Where a stored key would go is named rather than implied.
    TW_CHECK(!dialog.keyField()->placeholderText().isEmpty());
    TW_CHECK(dialog.testStatus() != nullptr);
#else
    // A Qt with no Network has no assistant at all, rather than a page that can
    // only report that it cannot work.
    TW_CHECK(window.aiPanel() == nullptr);
    TW_CHECK(!window.showPanel(QStringLiteral("ai")));
#endif
}

}  // namespace tw::ui_layout
