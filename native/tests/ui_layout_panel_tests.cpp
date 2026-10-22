// SPDX-License-Identifier: MIT
//
// The panels: lessons, classroom, the inspectors, the outline, SQL.

#include "ui_layout/support.hpp"

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
    for (const char* name : {"lessonOpenButton", "lessonCheckButton", "lessonHintButton",
                             "lessonSolutionButton", "lessonResetButton"}) {
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

}  // namespace tw::ui_layout
