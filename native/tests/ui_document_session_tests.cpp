// SPDX-License-Identifier: MIT
//
// The session: recent files, the layout that comes back, where user data lives, and
// the autosave that keeps unsaved work.

#include "ui_document/support.hpp"

namespace tw::ui_document {

void test_recent_files_are_remembered() {
    announce("recent files");
    const QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    MainWindow window;
    // The list lives in settings, so it starts out holding whatever earlier tests
    // opened -- and whatever the last real session left there.
    window.clearRecentFiles();
    TW_CHECK(window.recentFiles().isEmpty());

    const QString first = writeFile(dir, QStringLiteral("one.bas"), "PRINT 1\n");
    const QString second = writeFile(dir, QStringLiteral("two.logo"), "FORWARD 10\n");
    TW_CHECK(window.openFile(first));
    TW_CHECK(window.openFile(second));

    // Most recent first, and opening a file again moves it rather than adding a
    // second entry for the same file.
    QStringList recent = window.recentFiles();
    TW_CHECK_EQ(recent.size(), 2);
    TW_CHECK_EQ(recent.first(), second);
    TW_CHECK(window.openFile(first));
    recent = window.recentFiles();
    TW_CHECK_EQ(recent.size(), 2);
    TW_CHECK_EQ(recent.first(), first);

    // Capped: a menu of forty paths is a menu nobody reads.
    for (int index = 0; index < 14; ++index) {
        const QString path = writeFile(
            dir, QStringLiteral("f%1.bas").arg(index), "PRINT 1\n");
        TW_CHECK(window.openFile(path));
    }
    TW_CHECK_EQ(window.recentFiles().size(), 10);
}


void test_the_session_comes_back() {
    announce("session restore");
    const QTemporaryDir dir;
    const QString first = writeFile(dir, QStringLiteral("keep.bas"), "PRINT 1\n");
    const QString second = writeFile(dir, QStringLiteral("keep.logo"), "FORWARD 10\n");
    {
        MainWindow window;
        TW_CHECK(window.openFile(first));
        TW_CHECK(window.openFile(second));
        window.saveSession();
    }

    MainWindow restored;
    TW_CHECK_EQ(restored.restoreSession(), 2);
    // The list is bound to a name first: `openFilePaths().first()` is a reference
    // into a temporary, which GCC's -Wdangling-reference is right to refuse.
    const QStringList restored_paths = restored.openFilePaths();
    TW_CHECK_EQ(restored_paths.size(), 2);
    TW_CHECK_EQ(restored_paths.first(), first);

    // An untitled buffer has no name to reopen, so it is not part of a session:
    // a tab restored as "untitled 3" would be a document the user never had.
    MainWindow with_untitled;
    TW_CHECK(with_untitled.openFile(first));
    with_untitled.newFile();
    with_untitled.saveSession();
    MainWindow after;
    TW_CHECK_EQ(after.restoreSession(), 1);

    // A file that has been deleted since is skipped rather than reported. A
    // prompt at startup that cannot be dismissed is worse than a missing tab.
    QFile::remove(second);
    after.saveSession();
    MainWindow again;
    TW_CHECK_EQ(again.restoreSession(), 1);
    TW_CHECK_EQ(again.openFilePaths().size(), 1);
}

/// The data directory is where autosave, version history and classroom bundles
/// will write, so the two things that matter are that the environment variable
/// wins and that an unset one does not write into the working directory.

void test_user_data_lives_where_the_environment_says() {
    announce("user data directory");
    tw::test::TempDir scratch;
    TW_CHECK(scratch.valid());
    if (!scratch.valid()) {
        return;
    }

    scratch.useAsDataDir();
    TW_CHECK_EQ(timewarp::ui::dataDir().toStdString(), scratch.path().toStdString());

    // A named subdirectory is created rather than merely named: the caller is
    // about to write a snapshot into it.
    const QString autosave = timewarp::ui::dataDirFor(QStringLiteral("autosave"));
    TW_CHECK(!autosave.isEmpty());
    TW_CHECK(QFileInfo{autosave}.isDir());
    TW_CHECK_EQ(autosave.toStdString(),
                (scratch.path() + QStringLiteral("/autosave")).toStdString());

    // A leaf that escapes the data directory is refused, because the only
    // reason to pass one is a mistake.
    TW_CHECK(timewarp::ui::dataDirFor(QStringLiteral("../elsewhere")).isEmpty());
    TW_CHECK(timewarp::ui::dataDirFor(QStringLiteral("/etc")).isEmpty());

    // `~` expands the way the Python engine expands it.
    qputenv("TIME_WARP_DATA_DIR", "~/tw-probe");
    TW_CHECK_EQ(timewarp::ui::dataDir().toStdString(),
                (QDir::homePath() + QStringLiteral("/tw-probe")).toStdString());

    // And unset it is the reference engine's directory, not the current one.
    scratch.restoreDataDir();
    TW_CHECK_EQ(timewarp::ui::dataDir().toStdString(),
                (QDir::homePath() + QStringLiteral("/.time_warp")).toStdString());
}


/// Insertion is the half of the snippet feature that can be tested: the picker is
/// modal, so `applySnippet` is where the behaviour lives.

void test_autosave_keeps_unsaved_work() {
    announce("autosave: a snapshot of unsaved work");
    tw::test::TempDir scratch;
    TW_CHECK(scratch.valid());
    if (!scratch.valid()) {
        return;
    }
    // Before the window exists: the store resolves its directory when it is
    // constructed.
    scratch.useAsDataDir();

    MainWindow window;
    CodeEditor* editor = window.currentEditor();
    TW_CHECK(editor != nullptr);
    if (editor == nullptr) {
        scratch.restoreDataDir();
        return;
    }

    // An unmodified document is not snapshotted: it is already on disk or it is
    // an untouched buffer, and either way a copy would push the useful ones out.
    window.autosaveNow();
    timewarp::ui::AutosaveStore store;
    TW_CHECK(store.all().isEmpty());

    // Typed, not `setPlainText`: setting the text clears the modified flag, which
    // is the flag autosave asks about.
    editor->setPlainText(QString{});
    QTextCursor typed = editor->textCursor();
    typed.insertText(QStringLiteral("10 PRINT \"UNSAVED\"\n"));
    TW_CHECK(editor->document()->isModified());
    window.autosaveNow();

    const QList<timewarp::ui::Snapshot> snapshots = store.all();
    TW_CHECK_EQ(snapshots.size(), 1);
    if (!snapshots.isEmpty()) {
        TW_CHECK_EQ(store.read(snapshots.first()).toStdString(),
                    std::string{"10 PRINT \"UNSAVED\"\n"});
        // An untitled buffer is recorded as such: there is no path to name it by.
        TW_CHECK(snapshots.first().document.startsWith(QStringLiteral("untitled:")));
    }

    // The second snapshot of the same buffer supersedes rather than accumulates
    // inside one session's millisecond, and the store stays bounded.
    window.autosaveNow();
    TW_CHECK(store.all().size() <= 2);
    scratch.restoreDataDir();
}

/// A binary ROM and a text listing share `.ch8`. The ROM is decoded through the
/// byte-preserving codec, opened read-only so it cannot be edited into
/// corruption, and run without the assembler seeing it.

void test_the_split_survives_a_restart() {
    announce("session restore: the split and which side each document was on");
    const QTemporaryDir dir;
    const QString left = writeFile(dir, QStringLiteral("left.bas"), "PRINT 1\n");
    const QString right = writeFile(dir, QStringLiteral("right.bas"), "PRINT 2\n");
    {
        MainWindow window;
        TW_CHECK(window.openFile(left));
        TW_CHECK(window.openFile(right));
        // Splitting moves the current document -- `right` -- into the second pane
        // and leaves a fresh document behind, so both halves are usable.
        window.splitEditor();
        TW_CHECK(window.editorSplit());
        TW_CHECK_EQ(window.secondaryFilePaths().size(), 1);
        window.saveSession();
    }

    MainWindow restored;
    TW_CHECK_EQ(restored.restoreSession(), 2);
    TW_CHECK(restored.editorSplit());
    // `isHidden`, not `isVisible`: the window has not been shown, and a child of an
    // unshown window is never "visible" whatever its own flag says.
    TW_CHECK(!restored.secondaryEditorTabs()->isHidden());
    const QStringList secondary = restored.secondaryFilePaths();
    TW_CHECK_EQ(secondary.size(), 1);
    if (!secondary.isEmpty()) {
        TW_CHECK_EQ(secondary.first(), right);
    }
    // And the primary still has the document that was on that side.
    const QStringList primary = restored.openFilePaths();
    TW_CHECK_EQ(primary.size(), 1);
    if (!primary.isEmpty()) {
        TW_CHECK_EQ(primary.first(), left);
    }

    // A saved split whose other pane is empty is a blank rectangle, so it is not
    // restored as one.
    QSettings settings;
    settings.setValue(QStringLiteral("session/editorSplit"), true);
    settings.setValue(QStringLiteral("session/secondaryFiles"), QStringList{});
    MainWindow empty_split;
    TW_CHECK_EQ(empty_split.restoreSession(), 1);
    TW_CHECK(!empty_split.editorSplit());
    TW_CHECK(!empty_split.secondaryEditorTabs()->isVisible());
}

}  // namespace tw::ui_document
