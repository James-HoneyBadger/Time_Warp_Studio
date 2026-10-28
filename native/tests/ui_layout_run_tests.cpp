// SPDX-License-Identifier: MIT
//
// Running: the runners, projects, the REPL bar, profiling, and what persists.

#include "ui_layout/support.hpp"

namespace tw::ui_layout {

// Read by this file only: it arrived here with the cases that need it.

/// A window that has restored its saved layout, in the order the header
/// documents: construct, resize, restore, *then* show.
///
/// The order matters. `restoreGeometry()` applied to an already-visible window
/// lets Qt re-fit it to the screen instead of honouring the stored size -- under
/// the offscreen platform a 1200x800 window came back as 900x774, which is its
/// minimum width. Restoring first avoids the whole question.
[[nodiscard]] std::unique_ptr<MainWindow> restoredWindow(int width = 1200,
                                                        int height = 800) {
    auto window = std::make_unique<MainWindow>();
    window->resize(width, height);
    window->restoreLayout();
    window->show();
    QApplication::processEvents();
    QApplication::processEvents();
    return window;
}

void test_export_audio_records_a_runs_notes() {
    announce("export audio records a run's notes");
    // The run hands its sound to this object, and the recording happens whether or
    // not a player was found -- which is what makes a file possible on a machine
    // with no speaker, and what the assertion below relies on.
    timewarp::ui::SoundPlayer player;
    player.beginRecording();
    player.note(440.0, 100);
    player.play({timewarp::audio::Tone{timewarp::audio::Waveform::Triangle, 880.0, 50}});

    TW_CHECK_EQ(player.recorded().size(), std::size_t{2});
    TW_CHECK_EQ(player.recorded().at(0).frequency, 440.0);
    TW_CHECK(player.recorded().at(1).waveform == timewarp::audio::Waveform::Triangle);

    const QByteArray wav = player.recordedWav();
    TW_CHECK(wav.startsWith("RIFF"));
    TW_CHECK(wav.mid(8, 4) == "WAVE");
    // A run that starts clears it: the export is this run's audio, not every note
    // since the window opened.
    player.beginRecording();
    TW_CHECK(player.recorded().empty());
}

void test_gif_recording_toggles_and_samples_the_canvas() {
    announce("record canvas as GIF");
    const std::unique_ptr<MainWindow> window = shownWindow();
    auto* action = window->findChild<QAction*>(QStringLiteral("recordGifAction"));
    TW_CHECK(action != nullptr);
    if (action == nullptr) {
        return;
    }
    TW_CHECK(action->isCheckable());
    TW_CHECK(!action->isChecked());

    auto* timer = window->findChild<QTimer*>(QStringLiteral("gifRecordTimer"));
    TW_CHECK(timer != nullptr);
    if (timer != nullptr) {
        TW_CHECK(!timer->isActive());
    }

    action->trigger();
    TW_CHECK(action->isChecked());
    if (timer != nullptr) {
        TW_CHECK(timer->isActive());
        // Ten frames a second: the removed implementation's rate, and enough for
        // a turtle drawing without producing a file nobody can open.
        TW_CHECK_EQ(timer->interval(), 100);
    }

    action->trigger();
    TW_CHECK(!action->isChecked());
    if (timer != nullptr) {
        TW_CHECK(!timer->isActive());
    }
}


void test_command_palette() {
    announce("command palette");
    MainWindow window;
    timewarp::ui::CommandPalette palette{&window};
    palette.populate(&window);

    // The palette is the menu bar, so it cannot be shorter than the File menu.
    TW_CHECK(palette.commandCount() > 15);
    palette.setFilter(QStringLiteral("save"));
    QList<QString> rows = palette.visibleCommands();
    TW_CHECK(!rows.isEmpty());
    for (const QString& row : rows) {
        TW_CHECK(row.contains(QStringLiteral("ave"), Qt::CaseInsensitive));
    }

    // A filter that matches nothing offers nothing, rather than silently
    // offering everything.
    palette.setFilter(QStringLiteral("zzzz-no-such-command"));
    TW_CHECK_EQ(palette.commandCount(), 0);

    // Choosing a row really runs the command -- and closes the palette first, so
    // an action that opens a dialog is not left underneath it.
    window.clearOutput();
    auto* console = window.findChild<QPlainTextEdit*>(QStringLiteral("consoleOutput"));
    TW_CHECK(console != nullptr);
    if (console != nullptr) {
        console->setPlainText(QStringLiteral("something"));
    }
    palette.setFilter(QStringLiteral("Clear Output"));
    TW_CHECK(palette.commandCount() >= 1);
    TW_CHECK(palette.activate(0));
    if (console != nullptr) {
        TW_CHECK(console->toPlainText().isEmpty());
    }
}


void test_standalone_player_layout() {
    announce("standalone player layout");
    MainWindow window;
    TW_CHECK(window.openStandaloneProgram(QStringLiteral("basic"),
                                          QStringLiteral("10 PRINT 1\n"),
                                          QStringLiteral("Player")));
    TW_CHECK(window.isPlayerMode());
    const auto* tabs = window.findChild<QTabWidget*>(QStringLiteral("rightTabs"));
    TW_CHECK(tabs != nullptr);
    if (tabs != nullptr) {
        // The program's output and drawing remain; the inspector panels and their
        // tab strip do not.
        TW_CHECK_EQ(tabs->count(), 1);
        TW_CHECK(tabs->widget(0) != nullptr &&
                 tabs->widget(0)->objectName() == QStringLiteral("outputCanvasSplitter"));
        TW_CHECK(!tabs->isHidden());
        TW_CHECK(tabs->tabBar()->isHidden());
    }
    const auto* project = window.findChild<QDockWidget*>(QStringLiteral("projectDock"));
    TW_CHECK(project != nullptr && project->isHidden());
}


void test_standalone_payload() {
    announce("standalone payload");
    using timewarp::ui::standalone::Embedded;

    // The format round-trips through the encoder and the decoder, which is the
    // half of "compile" that can be tested without a second process.
    const QByteArray bytes =
        timewarp::ui::standalone::encode(QStringLiteral("basic"),
                                         QStringLiteral("PRINT \"HI\"\n"));
    Embedded decoded;
    QString error;
    TW_CHECK(timewarp::ui::standalone::decode(bytes, &decoded, &error));
    TW_CHECK(decoded.present);
    TW_CHECK_EQ(decoded.language, QStringLiteral("basic"));
    TW_CHECK_EQ(decoded.source, QStringLiteral("PRINT \"HI\"\n"));

    // Truncation and foreign files are refused with a reason, not decoded into a
    // plausible-looking empty program.
    TW_CHECK(!timewarp::ui::standalone::decode(bytes.left(bytes.size() - 3), &decoded,
                                               &error));
    TW_CHECK(!error.isEmpty());
    TW_CHECK(!timewarp::ui::standalone::decode(QByteArray{"not a program"}, &decoded,
                                               &error));

    // End to end: build an artifact from the *test* binary (which is an
    // executable like any other) and read the program back out of it.
    QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    const QString source_path = dir.filePath(QStringLiteral("hello.bas"));
    {
        QFile source{source_path};
        TW_CHECK(source.open(QIODevice::WriteOnly));
        source.write("PRINT \"HELLO\"\n");
    }
    const QString artifact = dir.filePath(QStringLiteral("hello"));
    TW_CHECK(timewarp::ui::standalone::compileFile(
        QCoreApplication::applicationFilePath(), source_path, artifact, false, &error));

    const Embedded carried = timewarp::ui::standalone::embeddedIn(artifact);
    TW_CHECK(carried.present);
    TW_CHECK_EQ(carried.language, QStringLiteral("basic"));
    TW_CHECK_EQ(carried.source, QStringLiteral("PRINT \"HELLO\"\n"));

    // The artifact is executable, or the thing a learner is told to double-click
    // cannot be. Windows has no execute bit -- a file runs because of its
    // extension, and setPermissions there can only toggle read-only -- so the
    // promise is checkable on the POSIX builds, which is where it is made.
#ifndef Q_OS_WIN
    QFile::Permissions permissions = QFile::permissions(artifact);
    TW_CHECK(permissions.testFlag(QFile::ExeOwner));
#endif

    // Overwriting needs to be asked for, and a file that is not a program is
    // refused before anything is written.
    TW_CHECK(!timewarp::ui::standalone::compileFile(
        QCoreApplication::applicationFilePath(), source_path, artifact, false, &error));
    const QString text_path = dir.filePath(QStringLiteral("notes.txt"));
    {
        QFile text{text_path};
        TW_CHECK(text.open(QIODevice::WriteOnly));
        text.write("hello");
    }
    TW_CHECK(!timewarp::ui::standalone::compileFile(
        QCoreApplication::applicationFilePath(), text_path,
        dir.filePath(QStringLiteral("notes")), true, &error));

    // A plain executable carries nothing, which is the IDE's own case.
    TW_CHECK(!timewarp::ui::standalone::embeddedIn(
                  QCoreApplication::applicationFilePath())
                  .present);

    // Format 2: the program arrives with the name and the author on it, and with
    // the size it was written for, so the window that opens is the window the
    // author saw.
    timewarp::ui::standalone::Metadata metadata;
    metadata.title = QStringLiteral("Star Patrol");
    metadata.author = QStringLiteral("A Learner");
    metadata.screen_width = 320;
    metadata.screen_height = 200;
    metadata.background = QStringLiteral("#101018");
    metadata.crt = 40;
    Embedded versioned;
    const QByteArray payload = timewarp::ui::standalone::encode(
        QStringLiteral("basic"), QStringLiteral("10 SCREEN 13\n"), metadata);
    TW_CHECK(timewarp::ui::standalone::decode(payload, &versioned, &error));
    TW_CHECK_EQ(versioned.format, 2);
    TW_CHECK_EQ(versioned.metadata.title, QStringLiteral("Star Patrol"));
    TW_CHECK_EQ(versioned.metadata.author, QStringLiteral("A Learner"));
    TW_CHECK_EQ(versioned.metadata.screen_width, 320);
    TW_CHECK_EQ(versioned.metadata.screen_height, 200);
    TW_CHECK_EQ(versioned.metadata.background, QStringLiteral("#101018"));
    TW_CHECK_EQ(versioned.metadata.crt, 40);
    TW_CHECK_EQ(versioned.source, QStringLiteral("10 SCREEN 13\n"));

    // A format-1 trailer -- what 17.x wrote, and what every artifact already in a
    // learner's folder is -- still reads, and reads as version 1. Built by hand
    // rather than by calling the encoder, because the point is to read what an
    // older *writer* produced: `source`, source length, key, key length, magic,
    // and nothing after the magic.
    const QByteArray body{"FORWARD 10\n"};
    const QByteArray key{"logo"};
    QByteArray legacy = body;
    QByteArray source_length(8, Qt::Uninitialized);
    qToLittleEndian<quint64>(static_cast<quint64>(body.size()), source_length.data());
    legacy += source_length;
    legacy += key;
    legacy += static_cast<char>(key.size());
    legacy.append(timewarp::ui::standalone::kMagic, timewarp::ui::standalone::kMagicLength);
    Embedded one;
    TW_CHECK(timewarp::ui::standalone::decode(legacy, &one, &error));
    TW_CHECK_EQ(one.format, 1);
    TW_CHECK_EQ(one.language, QStringLiteral("logo"));
    TW_CHECK_EQ(one.source, QStringLiteral("FORWARD 10\n"));
    TW_CHECK(one.metadata.title.isEmpty());

    // And the artifact written above carries its metadata through a real file,
    // not only through the encoder.
    const QString titled = dir.filePath(QStringLiteral("star_patrol"));
    TW_CHECK(timewarp::ui::standalone::compileFile(
        QCoreApplication::applicationFilePath(), source_path, titled, false, metadata,
        &error));
    const Embedded carried_metadata = timewarp::ui::standalone::embeddedIn(titled);
    TW_CHECK_EQ(carried_metadata.format, 2);
    TW_CHECK_EQ(carried_metadata.metadata.title, QStringLiteral("Star Patrol"));

    // A field this build does not know is ignored rather than refused, which is
    // what makes the format extensible without a third version number.
    const QByteArray forward = QByteArray{
        R"({"title":"Later","next_year":"a field this build has never heard of"})"};
    timewarp::ui::standalone::Metadata read;
    TW_CHECK(timewarp::ui::standalone::metadataFromJson(forward, &read));
    TW_CHECK_EQ(read.title, QStringLiteral("Later"));

    // Anything that is not a JSON object is refused: a garbled metadata block is
    // a truncated download, not a program with no title.
    timewarp::ui::standalone::Metadata ignored_metadata;
    TW_CHECK(!timewarp::ui::standalone::metadataFromJson(QByteArray{"{oops"}, nullptr));
    TW_CHECK(!timewarp::ui::standalone::metadataFromJson(QByteArray{"[1,2]"}, &ignored_metadata));
}


void test_profile_run_is_opt_in() {
    announce("View menu: Profile Run is opt-in");
    MainWindow window;
    QAction* profile = entryContaining(menuNamed(window, QStringLiteral("&View")),
                                       QStringLiteral("Profile Run"));
    TW_CHECK(profile != nullptr);
    if (profile == nullptr) {
        return;
    }
    TW_CHECK(profile->isCheckable());
    TW_CHECK(!profile->isChecked());
    profile->setChecked(true);
    TW_CHECK(profile->isChecked());
}

// The window's prompt path, without the dialog: the runner asks, the answer the
// dialog would have collected goes back, and the program reads it. The dialog
// itself is modal and cannot be clicked from a test -- which is why this seam,
// the one the dialog calls, is the half worth covering, and why the round trip
// went unverified for so long.

void test_input_round_trip_reaches_the_runner() {
    announce("input round trip");

    timewarp::canvas::CanvasCommandQueue queue;
    timewarp::ui::NativeRunner runner{timewarp::lang::Language::Basic, queue};

    QStringList prompts;
    QString output;
    std::atomic<bool> finished{false};
    bool completed = false;
    QObject::connect(&runner, &timewarp::ui::NativeRunner::inputRequested, &runner,
                     [&](const QString& prompt) {
                         prompts << prompt;
                         runner.answerInput(QStringLiteral("21"));
                     });
    QObject::connect(&runner, &timewarp::ui::NativeRunner::outputAvailable, &runner,
                     [&](const QString& text) { output += text; });
    QObject::connect(&runner, &timewarp::ui::NativeRunner::run_finished, &runner,
                     [&](bool ok, const QString&) {
                         completed = ok;
                         finished = true;
                     });

    runner.load_program(QStringLiteral("10 INPUT \"N\"; N\n20 PRINT N * 2\n"));
    runner.start();
    TW_CHECK(spin_until([&finished] { return finished.load(); }));
    runner.wait(5000);

    TW_CHECK(completed);
    TW_CHECK_EQ(prompts.size(), 1);
    if (!prompts.isEmpty()) {
        TW_CHECK_EQ(prompts.first(), QStringLiteral("N "));
    }
    TW_CHECK(output.contains(QStringLiteral("42")));

    // The answer landed in the engine, not just in the runner: the Variables panel
    // reads the same snapshot, so a value the program used has to be there.
    bool saw_n = false;
    for (const auto& row : runner.variables()) {
        if (row.name == QStringLiteral("N")) {
            saw_n = true;
        }
    }
    TW_CHECK(saw_n);
}

// `SCREEN` is a request that has to survive the whole chain: the language asks
// its host, the host pushes a command, and the canvas changes what it draws.
// Each half has a test of its own; this is the seam between them, which is the
// one that could silently do nothing.

void test_profile_report_lists_the_measurements() {
    announce("profile report rows");
    const QList<timewarp::ui::ProfileEntry> entries{
        {20, QStringLiteral("  PRINT I"), 10, 12.5},
        {10, QStringLiteral("FOR I = 1 TO 10"), 1, 0.5},
    };
    timewarp::ui::ProfileReportDialog dialog{QStringLiteral("loop.bas"), entries, 13.0};
    TW_CHECK_EQ(dialog.rowCount(), 2);
    TW_CHECK_EQ(dialog.lineAt(0), 20);
    TW_CHECK_EQ(dialog.lineAt(1), 10);
    TW_CHECK_EQ(dialog.hitsAt(0), std::uint64_t{10});
}


void test_windows_sound_command_line() {
    announce("windows sound command line");
    // Windows has no `aplay` on PATH, so the player is PowerShell and a .NET
    // class, and the file path goes inside a script rather than after an
    // argument. A path is not a safe thing to paste into a script -- a user
    // called O'Brien has an apostrophe in it -- so the quoting is the part
    // worth pinning, and it can be checked from Linux.
    const QString plain =
        timewarp::ui::SoundPlayer::powerShellCommand(QStringLiteral("C:/Temp/tone.wav"));
    TW_CHECK(plain.contains(QStringLiteral("'C:/Temp/tone.wav'")));
    TW_CHECK(plain.startsWith(QStringLiteral("(New-Object Media.SoundPlayer ")));
    TW_CHECK(plain.endsWith(QStringLiteral(".PlaySync()")));

    // The doubled quote is PowerShell's escape, and it must not close the string.
    const QString quoted = timewarp::ui::SoundPlayer::powerShellCommand(
        QStringLiteral("C:/Users/O'Brien/AppData/Local/Temp/tone.wav"));
    TW_CHECK(quoted.contains(QStringLiteral("'C:/Users/O''Brien/")));
    TW_CHECK_EQ(quoted.count(QLatin1Char{'\''}), 4);  // open, doubled pair, close
}

// ---------------------------------------------------------------------------
// Persistence: the part a user notices only when it is missing
// ---------------------------------------------------------------------------


void test_splitter_positions_survive_a_restart() {
    announce("splitter persistence");
    QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    QSettings::setPath(QSettings::IniFormat, QSettings::UserScope, dir.path());

    QList<int> savedMain;
    QList<int> savedColumn;
    QList<int> defaultMain;

    {
        const std::unique_ptr<MainWindow> first = shownWindow();
        defaultMain = first->mainSplitter()->sizes();

        // Move the dividers the way a user would. Both windows below are the same
        // size, so a saved pixel layout is comparable rather than merely similar.
        first->mainSplitter()->setSizes({400, 800});
        first->editorColumnSplitter()->setSizes({300, 120});
        QApplication::processEvents();

        savedMain = first->mainSplitter()->sizes();
        savedColumn = first->editorColumnSplitter()->sizes();
        // The move has to be a real change, or the round trip below would pass
        // with the defaults left in place.
        TW_CHECK(savedMain.at(0) != defaultMain.at(0));
        first->saveLayout();
    }

    {
        const std::unique_ptr<MainWindow> second = restoredWindow();
        const QList<int> main = second->mainSplitter()->sizes();
        const QList<int> column = second->editorColumnSplitter()->sizes();

        // The Python persists window geometry and dock state but not split
        // positions, so a dragged divider is forgotten on every restart. These
        // assertions are what fail if that block is removed from
        // `saveLayout()` / `restoreLayout()`.
        //
        // Compared as proportions, not pixels. Splitter state is *defined* as
        // proportions, and the window is not guaranteed to come back the same
        // size: `restoreGeometry()` fits a stored geometry to the available
        // screen, and the offscreen platform's virtual screen is 800x800, so a
        // 1200x800 window legitimately returns as 900x774. Asserting the pixels
        // would be asserting that the test environment has a large display.
        TW_CHECK(std::abs(fraction(second->mainSplitter(), 0) -
                          static_cast<double>(savedMain.at(0)) /
                              (savedMain.at(0) + savedMain.at(1))) < 0.01);
        TW_CHECK(std::abs(fraction(second->editorColumnSplitter(), 0) -
                          static_cast<double>(savedColumn.at(0)) /
                              (savedColumn.at(0) + savedColumn.at(1))) < 0.02);
        // And the REPL bar, being small and height-driven, survives verbatim.
        TW_CHECK(std::abs(column.at(1) - savedColumn.at(1)) <= 8);
    }

    // A settings file with no splitter state -- a first run, or one written by an
    // older build -- must leave the layout alone rather than wedge a pane to
    // zero, which is the failure mode a blind `restoreState()` would produce.
    QSettings().clear();
    {
        const std::unique_ptr<MainWindow> fresh = restoredWindow();
        // With nothing stored, `restoreLayout()` must leave the defaults exactly
        // as they are rather than wedge a pane to zero -- which is what a blind
        // `restoreState()` on an empty array would produce.
        TW_CHECK(std::abs(fraction(fresh->mainSplitter(), 0) - 0.6) < 0.05);
        TW_CHECK(fresh->immediateMode()->height() >= 20);
    }
}


void test_left_column_gets_the_larger_share_on_growth() {
    announce("resize behaviour");
    const std::unique_ptr<MainWindow> window = shownWindow(1000, 700);

    const QList<int> before = window->mainSplitter()->sizes();
    // The default split is exactly 3:2 -- 720:480 -- which is what the stretch
    // factors encode. Growth must preserve that ratio: with equal stretch the
    // canvas would gain as fast as the code and the ratio would drift to 1:1.
    TW_CHECK(std::abs(static_cast<double>(before.at(0)) / before.at(1) - 1.5) < 0.05);

    window->resize(1800, 700);
    QApplication::processEvents();
    QApplication::processEvents();
    const QList<int> after = window->mainSplitter()->sizes();

    // Whether the offscreen platform actually delivers the resize is not
    // something this test should depend on, so the growth itself is a soft
    // assertion and the ratio is the real one.
    TW_CHECK(after.at(0) >= before.at(0));
    if (after.at(0) > before.at(0)) {
        TW_CHECK(std::abs(static_cast<double>(after.at(0)) / after.at(1) - 1.5) < 0.2);
    }

    // And the REPL bar must not have crept open: it holds its height while the
    // editor takes the vertical slack.
    TW_CHECK(window->immediateMode()->height() <= 80);
}

void test_project_panel_shows_groups_and_files() {
    announce("project panel");
    QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    MainWindow window;
    ProjectPanel* panel = window.projectPanel();
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }
    TW_CHECK_EQ(panel->objectName(), QStringLiteral("projectPanel"));
    QDockWidget* dock = window.findChild<QDockWidget*>(QStringLiteral("projectDock"));
    TW_CHECK(dock != nullptr);
    if (dock != nullptr) {
        TW_CHECK(dock->widget() == panel);
        TW_CHECK_EQ(window.dockWidgetArea(dock), Qt::LeftDockWidgetArea);
    }

    // The model is the source of truth; the panel is its view.
    const QString root = dir.path();
    const QString first = QDir{root}.filePath(QStringLiteral("one.bas"));
    const QString second = QDir{root}.filePath(QStringLiteral("two.logo"));
    {
        QFile file{first};
        static_cast<void>(file.open(QIODevice::WriteOnly));
    }
    {
        QFile file{second};
        static_cast<void>(file.open(QIODevice::WriteOnly));
    }
    timewarp::project::Project project =
        timewarp::project::Project::create(QStringLiteral("Class"), root);
    project.addGroup(QStringLiteral("Art"));
    project.addFile(first, QStringLiteral("Art"));
    project.addFile(second);
    panel->setProject(project);

    TW_CHECK_EQ(panel->projectName(), QStringLiteral("Class"));
    TW_CHECK_EQ(panel->fileCount(), 2);
    TW_CHECK_EQ(panel->groupNames(), QStringList({QStringLiteral("Art")}));
    TW_CHECK(panel->hasFile(first));
    TW_CHECK_EQ(panel->groupOf(first), QStringLiteral("Art"));
    TW_CHECK_EQ(panel->groupOf(second), QString{});
    TW_CHECK(panel->tree()->topLevelItemCount() >= 2);
    panel->selectFile(first);
    TW_CHECK(panel->tree()->currentItem() != nullptr);

    panel->clearProject();
    TW_CHECK_EQ(panel->fileCount(), 0);
    TW_CHECK_EQ(panel->projectName(), QString{});
}

// **Run Project** is the claim: a project's entry point runs, and the program's
// relative file names land in the project's folder rather than in whatever
// directory the IDE was launched from. Both halves are asserted here because the
// second is invisible until a program tries to write a file.

void test_run_project_runs_the_entry_point_in_the_project_folder() {
    announce("run project");
    QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    const QString root = dir.path();
    const QString entry = QDir{root}.filePath(QStringLiteral("main.bas"));
    const QString other = QDir{root}.filePath(QStringLiteral("other.bas"));
    const auto write_file = [](const QString& path, const QByteArray& text) {
        QFile file{path};
        if (!file.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
            return false;
        }
        return file.write(text) == text.size();
    };
    TW_CHECK(write_file(entry,
                        "10 OPEN \"notes.txt\" FOR OUTPUT AS #1\n"
                        "20 PRINT #1, \"from the project\"\n"
                        "30 CLOSE #1\n"
                        "40 PRINT \"ENTRY RAN\"\n"));
    TW_CHECK(write_file(other, "10 PRINT \"NOT ME\"\n"));

    timewarp::project::Project project =
        timewarp::project::Project::create(QStringLiteral("Proj"), root);
    project.addFile(entry);
    project.addFile(other);
    TW_CHECK(project.setEntry(entry));
    QString error;
    TW_CHECK(project.save(&error));

    MainWindow window;
    TW_CHECK(window.openProject(project.filePath()));
    auto* console = window.findChild<QPlainTextEdit*>(QStringLiteral("consoleOutput"));
    TW_CHECK(console != nullptr);
    if (console == nullptr) {
        return;
    }

    window.runProject();
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("ENTRY RAN"));
    }));
    // The entry ran; the other program did not.
    TW_CHECK(!console->toPlainText().contains(QStringLiteral("NOT ME")));

    const QString written = QDir{root}.filePath(QStringLiteral("notes.txt"));
    TW_CHECK(QFileInfo::exists(written));
    QFile file{written};
    if (file.open(QIODevice::ReadOnly)) {
        TW_CHECK(QString::fromUtf8(file.readAll()).contains(QStringLiteral("from the project")));
    }
    // Nothing was written beside the test runner's working directory.
    TW_CHECK(!QFileInfo::exists(QDir::currentPath() + QStringLiteral("/notes.txt")));

    window.closeProject();
}

// With no project open the document's own folder is the run's directory, which is
// the behaviour a user opening a file from anywhere expects.

void test_a_run_writes_beside_its_document_without_a_project() {
    announce("a run writes beside its document");
    QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    const QString folder = dir.path();
    const QString program = QDir{folder}.filePath(QStringLiteral("writer.bas"));
    QFile file{program};
    TW_CHECK(file.open(QIODevice::WriteOnly));
    file.write("10 OPEN \"beside.txt\" FOR OUTPUT AS #1\n"
               "20 PRINT #1, \"here\"\n"
               "30 CLOSE #1\n"
               "40 PRINT \"WROTE\"\n");
    file.close();

    MainWindow window;
    window.closeProject();
    TW_CHECK(window.openFile(program));

    auto* console = window.findChild<QPlainTextEdit*>(QStringLiteral("consoleOutput"));
    TW_CHECK(console != nullptr);
    if (console == nullptr) {
        return;
    }
    window.runCurrentDocument();
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("WROTE"));
    }));
    TW_CHECK(QFileInfo::exists(QDir{folder}.filePath(QStringLiteral("beside.txt"))));
}


void test_watch_and_data_breakpoint_controls() {
    announce("watch and data breakpoint controls");
    // The panel restores the session-wide lists; a value written by an earlier
    // test would otherwise make "the list I just created" wrong.
    QSettings{}.remove(QStringLiteral("debug/watches"));
    QSettings{}.remove(QStringLiteral("debug/dataBreakpoints"));
    MainWindow window;
    DebugPanel* panel = window.debugPanel();
    TW_CHECK(panel != nullptr);
    if (panel == nullptr) {
        return;
    }
    for (const char* name : {"debugWatchInput", "debugAddWatch", "debugRemoveWatch",
                             "debugWatches", "debugDataBreakpointInput",
                             "debugAddDataBreakpoint", "debugRemoveDataBreakpoint",
                             "debugDataBreakpoints", "debugPauseReason"}) {
        TW_CHECK(panel->findChild<QWidget*>(QString::fromLatin1(name)) != nullptr);
    }

    QStringList announced;
    QObject::connect(panel, &DebugPanel::watchesChanged,
                     [&announced](const QStringList& expressions) {
                         announced = expressions;
                     });
    auto* watch_input = panel->findChild<QLineEdit*>(QStringLiteral("debugWatchInput"));
    auto* watch_add = panel->findChild<QPushButton*>(QStringLiteral("debugAddWatch"));
    TW_CHECK(watch_input != nullptr);
    TW_CHECK(watch_add != nullptr);
    if (watch_input != nullptr && watch_add != nullptr) {
        watch_input->setText(QStringLiteral("X * 2"));
        watch_add->click();
        TW_CHECK_EQ(panel->watches(), QStringList({QStringLiteral("X * 2")}));
        TW_CHECK_EQ(announced, QStringList({QStringLiteral("X * 2")}));
    }
    panel->setWatchValues({DebugPanel::WatchRow{QStringLiteral("X * 2"),
                                                QStringLiteral("10"), true, QString{}}});
    auto* table = panel->findChild<QTableWidget*>(QStringLiteral("debugWatches"));
    TW_CHECK(table != nullptr);
    if (table != nullptr) {
        TW_CHECK_EQ(table->rowCount(), 1);
        TW_CHECK_EQ(table->item(0, 1)->text(), QStringLiteral("10"));
    }

    QStringList watched;
    QObject::connect(panel, &DebugPanel::dataBreakpointsChanged,
                     [&watched](const QStringList& names) { watched = names; });
    auto* data_input =
        panel->findChild<QLineEdit*>(QStringLiteral("debugDataBreakpointInput"));
    auto* data_add =
        panel->findChild<QPushButton*>(QStringLiteral("debugAddDataBreakpoint"));
    TW_CHECK(data_input != nullptr);
    TW_CHECK(data_add != nullptr);
    if (data_input != nullptr && data_add != nullptr) {
        data_input->setText(QStringLiteral("TOTAL"));
        data_add->click();
        TW_CHECK_EQ(panel->dataBreakpoints(), QStringList({QStringLiteral("TOTAL")}));
        TW_CHECK_EQ(watched, QStringList({QStringLiteral("TOTAL")}));
    }
    panel->setDataBreakpoints({QStringLiteral("X")});
    TW_CHECK_EQ(panel->dataBreakpoints(), QStringList({QStringLiteral("X")}));
    panel->setPauseReason(QStringLiteral("X changed"));
    auto* reason = panel->findChild<QLabel*>(QStringLiteral("debugPauseReason"));
    TW_CHECK(reason != nullptr);
    if (reason != nullptr) {
        TW_CHECK_EQ(reason->text(), QStringLiteral("X changed"));
    }
}


void test_repl_bar_runs_and_keeps_bindings() {
    announce("REPL bar");
    const std::unique_ptr<MainWindow> window = shownWindow();
    ImmediateModePanel* panel = window->immediateMode();
    auto* console = window->findChild<QPlainTextEdit*>(QStringLiteral("consoleOutput"));
    TW_CHECK(panel != nullptr);
    TW_CHECK(console != nullptr);
    if (panel == nullptr || console == nullptr) {
        return;
    }

    emit panel->commandSubmitted(QStringLiteral("PRINT 2 + 2"));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("4"));
    }));
    TW_CHECK(console->toPlainText().contains(QStringLiteral("REPL")));
    // The bar is cleared as soon as the command is handed over, so the next
    // command starts from an empty field rather than appending to the last one.
    TW_CHECK(panel->command().isEmpty());

    emit panel->commandSubmitted(QStringLiteral("LET X = 7"));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("REPL \u203A LET X = 7"));
    }));
    emit panel->commandSubmitted(QStringLiteral("PRINT X"));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().count(QStringLiteral("7")) >= 2;
    }));

    // BASIC's NEW clears the workspace rather than being an unknown command.
    emit panel->commandSubmitted(QStringLiteral("new"));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("workspace cleared"));
    }));
    const QString before = console->toPlainText();
    emit panel->commandSubmitted(QStringLiteral("PRINT X"));
    TW_CHECK(spin_until([console, before] {
        return console->toPlainText().mid(before.size()).contains(QStringLiteral("0"));
    }));
}


void test_repl_basic_direct_mode() {
    announce("REPL bar: BASIC direct mode");
    const std::unique_ptr<MainWindow> window = shownWindow();
    ImmediateModePanel* panel = window->immediateMode();
    auto* console = window->findChild<QPlainTextEdit*>(QStringLiteral("consoleOutput"));
    CodeEditor* editor = window->currentEditor();
    TW_CHECK(panel != nullptr && console != nullptr && editor != nullptr);
    if (panel == nullptr || console == nullptr || editor == nullptr) {
        return;
    }
    editor->setPlainText(QString{});
    editor->setLanguage(Language::Basic);

    emit panel->commandSubmitted(QStringLiteral("20 PRINT \"TWO\""));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("Line 20 stored"));
    }));
    emit panel->commandSubmitted(QStringLiteral("10 PRINT \"ONE\""));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("Line 10 stored"));
    }));
    emit panel->commandSubmitted(QStringLiteral("15 ? 6 * 7"));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("Line 15 stored"));
    }));
    TW_CHECK_EQ(editor->toPlainText(),
                QStringLiteral("10 PRINT \"ONE\"\n15 PRINT 6 * 7\n20 PRINT \"TWO\"\n"));

    // The same number replaces the line instead of duplicating it.
    emit panel->commandSubmitted(QStringLiteral("10 PRINT \"REPLACED\""));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("Line 10 stored"));
    }));
    TW_CHECK_EQ(editor->toPlainText(),
                QStringLiteral("10 PRINT \"REPLACED\"\n15 PRINT 6 * 7\n20 PRINT \"TWO\"\n"));

    // `?` is PRINT in immediate mode too.
    emit panel->commandSubmitted(QStringLiteral("? 6 * 7"));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("42"));
    }));

    const QString before_list = console->toPlainText();
    emit panel->commandSubmitted(QStringLiteral("LIST"));
    TW_CHECK(spin_until([console, before_list] {
        return console->toPlainText().mid(before_list.size()).contains(
            QStringLiteral("10 PRINT \"REPLACED\""));
    }));

    // RUN executes the program that the numbered entries built.
    emit panel->commandSubmitted(QStringLiteral("RUN"));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("REPLACED")) &&
               console->toPlainText().contains(QStringLiteral("TWO"));
    }));

    // NEW clears both the document and the REPL bindings.
    emit panel->commandSubmitted(QStringLiteral("NEW"));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("workspace cleared"));
    }));
    TW_CHECK(editor->toPlainText().isEmpty());
}


void test_repl_bar_scheme_errors_and_language_switch() {
    announce("REPL bar: Scheme, errors and language switch");
    const std::unique_ptr<MainWindow> window = shownWindow();
    ImmediateModePanel* panel = window->immediateMode();
    auto* console = window->findChild<QPlainTextEdit*>(QStringLiteral("consoleOutput"));
    CodeEditor* editor = window->currentEditor();
    auto* selector = window->findChild<QComboBox*>(QStringLiteral("languageSelector"));
    TW_CHECK(panel != nullptr && console != nullptr && editor != nullptr &&
             selector != nullptr);
    if (panel == nullptr || console == nullptr || editor == nullptr ||
        selector == nullptr) {
        return;
    }
    const auto choose = [selector](const QString& key) {
        const int index = selector->findData(key);
        TW_CHECK(index >= 0);
        selector->setCurrentIndex(index);
    };

    choose(QStringLiteral("scheme"));
    emit panel->commandSubmitted(QStringLiteral("(define y 4)"));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("REPL \u203A (define y 4)"));
    }));
    emit panel->commandSubmitted(QStringLiteral("(* y 5)"));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("20"));
    }));

    // A reader error is shown, not swallowed.
    emit panel->commandSubmitted(QStringLiteral("("));
    TW_CHECK(spin_until([console] {
        return console->toPlainText().contains(QStringLiteral("\u274C"));
    }));

    // Switching the document's language swaps the REPL engine rather than
    // sending Logo a Scheme form.
    choose(QStringLiteral("logo"));
    const QString before = console->toPlainText();
    emit panel->commandSubmitted(QStringLiteral("PRINT 2 + 2"));
    TW_CHECK(spin_until([console, before] {
        return console->toPlainText().mid(before.size()).contains(QStringLiteral("4"));
    }));
    TW_CHECK(console->toPlainText().mid(before.size()).contains(
        QStringLiteral("REPL \u203A PRINT 2 + 2")));
}

}  // namespace tw::ui_layout
