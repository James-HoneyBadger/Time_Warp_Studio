// SPDX-License-Identifier: MIT
//
// The project file: what a project remembers, and what it refuses.
//
// The model is tested without a window because the parts that can lose a user's
// grouping are the load path and the path arithmetic, not the tree widget. The
// panel is a view over this object; a bug here is a project that opens with the
// wrong files in it.

#include <cstdio>

#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QString>

#include "check.hpp"
#include "temp_dir.hpp"
#include "timewarp/project/project.hpp"

namespace {

using timewarp::project::Project;

void announce(const char* name) {
    std::printf("-- %s\n", name);
    std::fflush(stdout);
}

void test_create_and_add() {
    announce("create, groups and files");
    tw::test::TempDir temp;
    if (!temp.valid()) {
        return;
    }
    const QString root = temp.path(QStringLiteral("course"));
    const QString inside = temp.write(QStringLiteral("course/turtle/flower.logo"), "FD 10\n");
    const QString outside = temp.write(QStringLiteral("outside.bas"), "10 PRINT 1\n");
    TW_CHECK(!inside.isEmpty());

    Project project = Project::create(QStringLiteral("Class One"), root);
    TW_CHECK(project.isValid());
    TW_CHECK_EQ(project.name(), QStringLiteral("Class One"));
    TW_CHECK_EQ(project.root(), QDir::cleanPath(root));
    TW_CHECK_EQ(project.filePath(),
                QDir{root}.filePath(QStringLiteral("Class One.twproject")));

    TW_CHECK(project.addGroup(QStringLiteral("Turtle art")));
    TW_CHECK(!project.addGroup(QStringLiteral("Turtle art")));
    TW_CHECK(!project.addGroup(QStringLiteral("   ")));

    TW_CHECK(project.addFile(inside, QStringLiteral("Turtle art")));
    TW_CHECK(!project.addFile(inside, QStringLiteral("Elsewhere")));
    TW_CHECK(project.addFile(outside));
    TW_CHECK_EQ(project.files().size(), 2);
    TW_CHECK_EQ(project.groupOf(inside), QStringLiteral("Turtle art"));
    TW_CHECK_EQ(project.filesIn(QStringLiteral("Turtle art")).size(), 1);
    TW_CHECK_EQ(project.displayPath(inside), QStringLiteral("turtle/flower.logo"));
    TW_CHECK_EQ(project.displayPath(outside), QDir::cleanPath(outside));

    // A group named while adding a file is a group, not a dangling label.
    TW_CHECK(project.addFile(temp.write(QStringLiteral("loops.bas"), "10 FOR I = 1 TO 3\n"),
                             QStringLiteral("Loops")));
    TW_CHECK(project.groups().contains(QStringLiteral("Loops")));
}

void test_round_trip() {
    announce("save and load");
    tw::test::TempDir temp;
    if (!temp.valid()) {
        return;
    }
    const QString root = temp.path(QStringLiteral("course"));
    TW_CHECK(!root.isEmpty());
    const QString first = temp.write(QStringLiteral("course/a.bas"), "10 PRINT 1\n");
    const QString second = temp.write(QStringLiteral("course/logo/b.logo"), "FD 5\n");
    const QString outside = temp.write(QStringLiteral("elsewhere.c.scm"), "(+ 1 2)\n");
    TW_CHECK(!first.isEmpty());

    Project original = Project::create(QStringLiteral("Course"), root);
    original.addGroup(QStringLiteral("BASIC"));
    original.addFile(first, QStringLiteral("BASIC"));
    original.addGroup(QStringLiteral("Logo"));
    original.addFile(second, QStringLiteral("Logo"));
    original.addFile(outside);

    // Inside-root paths are relative in the file, so the folder can move; a path
    // outside is absolute because a relative one would name the wrong file.
    const QJsonObject json = QJsonDocument::fromJson(original.toJson()).object();
    const QString stored_root = json.value(QStringLiteral("root")).toString();
    TW_CHECK(!QFileInfo{stored_root}.isAbsolute());
    const QJsonArray stored_files = json.value(QStringLiteral("files")).toArray();
    TW_CHECK_EQ(stored_files.size(), 3);
    TW_CHECK_EQ(stored_files.at(0).toObject().value(QStringLiteral("path")).toString(),
                QStringLiteral("a.bas"));
    TW_CHECK(QFileInfo{stored_files.at(2).toObject().value(QStringLiteral("path")).toString()}
                 .isAbsolute());

    QString error;
    TW_CHECK(original.save(&error));
    TW_CHECK(error.isEmpty());
    TW_CHECK(QFileInfo::exists(original.filePath()));

    Project loaded;
    TW_CHECK(loaded.load(original.filePath(), &error));
    TW_CHECK(error.isEmpty());
    TW_CHECK_EQ(loaded.name(), QStringLiteral("Course"));
    TW_CHECK_EQ(loaded.root(), original.root());
    TW_CHECK_EQ(loaded.groups(), original.groups());
    TW_CHECK_EQ(loaded.files().size(), 3);
    TW_CHECK_EQ(loaded.groupOf(first), QStringLiteral("BASIC"));
    TW_CHECK_EQ(loaded.groupOf(second), QStringLiteral("Logo"));
    TW_CHECK(loaded.contains(outside));

    // The same project saved again is byte-identical: a panel refresh must not
    // rewrite the file into a different shape every time.
    TW_CHECK_EQ(loaded.toJson(), original.toJson());
}

void test_group_edits() {
    announce("group edits do not delete files");
    tw::test::TempDir temp;
    if (!temp.valid()) {
        return;
    }
    const QString root = temp.path();
    const QString file = temp.write(QStringLiteral("one.bas"), "10 END\n");
    Project project = Project::create(QStringLiteral("P"), root);
    project.addFile(file, QStringLiteral("Old"));

    TW_CHECK(project.renameGroup(QStringLiteral("Old"), QStringLiteral("New")));
    TW_CHECK_EQ(project.groupOf(file), QStringLiteral("New"));
    TW_CHECK(!project.renameGroup(QStringLiteral("Missing"), QStringLiteral("New")));
    TW_CHECK(!project.renameGroup(QStringLiteral("New"), QStringLiteral("New")));

    TW_CHECK(project.removeGroup(QStringLiteral("New")));
    TW_CHECK_EQ(project.groupOf(file), QString{});
    TW_CHECK(project.contains(file));
    TW_CHECK(!project.removeGroup(QStringLiteral("New")));

    TW_CHECK(project.setGroup(file, QStringLiteral("Second")));
    TW_CHECK(project.groups().contains(QStringLiteral("Second")));
    TW_CHECK(project.setGroup(file, QString{}));
    TW_CHECK_EQ(project.groupOf(file), QString{});
    TW_CHECK(!project.setGroup(file, QString{}));  // no change
}

void test_missing_files() {
    announce("missing files are reported, not hidden");
    tw::test::TempDir temp;
    if (!temp.valid()) {
        return;
    }
    const QString root = temp.path();
    const QString present = temp.write(QStringLiteral("present.bas"), "10 END\n");
    const QString absent = temp.path(QStringLiteral("absent.bas"));
    Project project = Project::create(QStringLiteral("P"), root);
    project.addFile(present);
    project.addFile(absent);

    TW_CHECK_EQ(project.missingFiles().size(), 1);
    TW_CHECK_EQ(project.pruneMissing(), 1);
    TW_CHECK_EQ(project.files().size(), 1);
    TW_CHECK(project.contains(present));
    TW_CHECK(!project.contains(absent));
}

// The entry point is what **Run Project** runs, so it has to survive a save and a
// load, stay inside the project's own path arithmetic (relative when it can be),
// and never name something the dock does not show.
void test_the_entry_point_round_trips() {
    announce("the entry point round-trips");
    tw::test::TempDir temp;
    if (!temp.valid()) {
        return;
    }
    const QString root = temp.path(QStringLiteral("course"));
    const QString entry = temp.write(QStringLiteral("course/main.bas"), "10 PRINT 1\n");
    const QString other = temp.write(QStringLiteral("course/extra.bas"), "10 PRINT 2\n");

    Project project = Project::create(QStringLiteral("Course"), root);
    TW_CHECK(!project.hasEntry());
    TW_CHECK(project.setEntry(entry));
    // Setting an entry that was not a member adds it: the entry is always shown.
    TW_CHECK(project.contains(entry));
    TW_CHECK_EQ(project.entry(), QDir::cleanPath(entry));
    // Setting the same one twice is not a change, which is what lets the window
    // save only after a real edit.
    TW_CHECK(!project.setEntry(entry));
    TW_CHECK(project.setEntry(other));
    TW_CHECK_EQ(project.entry(), QDir::cleanPath(other));
    TW_CHECK_EQ(project.files().size(), 2);

    QString error;
    TW_CHECK(project.save(&error));
    // Stored relative to the root, like every other path in the file.
    QFile saved{project.filePath()};
    TW_CHECK(saved.open(QIODevice::ReadOnly));
    const QJsonObject written =
        QJsonDocument::fromJson(saved.readAll()).object();
    TW_CHECK_EQ(written.value(QStringLiteral("entry")).toString(),
                QStringLiteral("extra.bas"));

    Project reloaded;
    TW_CHECK(reloaded.load(project.filePath(), &error));
    TW_CHECK_EQ(reloaded.entry(), QDir::cleanPath(other));
    TW_CHECK(reloaded.hasEntry());

    // Removing the entry from the project clears it, because a project that runs a
    // program it no longer lists is a mystery rather than a feature.
    TW_CHECK(reloaded.removeFile(other));
    TW_CHECK(!reloaded.hasEntry());
    TW_CHECK(reloaded.clearEntry() == false);
}

void test_a_version_one_project_still_loads() {
    announce("a version 1 project still loads");
    tw::test::TempDir temp;
    if (!temp.valid()) {
        return;
    }
    const QString root = temp.path(QStringLiteral("old"));
    const QString program = temp.write(QStringLiteral("old/one.bas"), "10 PRINT 1\n");
    // Exactly what 20.x wrote: no `entry`, format 1.
    QJsonObject object;
    object.insert(QStringLiteral("format"), 1);
    object.insert(QStringLiteral("name"), QStringLiteral("Old"));
    object.insert(QStringLiteral("root"), QStringLiteral("."));
    QJsonArray files;
    QJsonObject record;
    record.insert(QStringLiteral("path"), QFileInfo{program}.fileName());
    files.append(record);
    object.insert(QStringLiteral("files"), files);
    const QString path = temp.write(QStringLiteral("old/Old.twproject"),
                                    QJsonDocument{object}.toJson());

    Project project;
    QString error;
    TW_CHECK(project.load(path, &error));
    TW_CHECK_EQ(project.files().size(), 1);
    TW_CHECK(!project.hasEntry());
}

// A hand-edited project that names an entry it does not list must still load, with
// the file added: the alternative is an entry point the dock cannot show.
void test_an_entry_outside_the_file_list_is_added() {
    announce("an unlisted entry is added");
    tw::test::TempDir temp;
    if (!temp.valid()) {
        return;
    }
    const QString root = temp.path(QStringLiteral("hand"));
    (void)temp.write(QStringLiteral("hand/start.logo"), "FD 10\n");
    QJsonObject object;
    object.insert(QStringLiteral("format"), 2);
    object.insert(QStringLiteral("name"), QStringLiteral("Hand"));
    object.insert(QStringLiteral("root"), QStringLiteral("."));
    object.insert(QStringLiteral("files"), QJsonArray{});
    object.insert(QStringLiteral("entry"), QStringLiteral("start.logo"));
    const QString path = temp.write(QStringLiteral("hand/Hand.twproject"),
                                    QJsonDocument{object}.toJson());

    Project project;
    QString error;
    TW_CHECK(project.load(path, &error));
    TW_CHECK(project.hasEntry());
    // Named, because `files()` returns a list by value: `.first()` on the temporary
    // would name a member of something already destroyed by the time it is read.
    const QList<timewarp::project::ProjectFile> listed = project.files();
    TW_CHECK_EQ(listed.size(), 1);
    const QString expected_entry =
        QDir::cleanPath(QDir{root}.filePath(QStringLiteral("start.logo")));
    TW_CHECK_EQ(listed.first().path, expected_entry);
}

void test_bad_files_are_refused() {
    announce("malformed projects are refused");
    tw::test::TempDir temp;
    if (!temp.valid()) {
        return;
    }
    const QString root = temp.path();
    Project project = Project::create(QStringLiteral("Keep"), root);
    const QString kept_path = project.filePath();
    TW_CHECK(project.addGroup(QStringLiteral("Kept")));

    QString error;
    TW_CHECK(!project.load(temp.write(QStringLiteral("broken.twproject"), "{not json"),
                           &error));
    TW_CHECK(!error.isEmpty());
    // A failed load leaves the open project alone.
    TW_CHECK_EQ(project.name(), QStringLiteral("Keep"));
    TW_CHECK(project.groups().contains(QStringLiteral("Kept")));
    TW_CHECK_EQ(project.filePath(), kept_path);

    error.clear();
    QJsonObject newer;
    newer.insert(QStringLiteral("format"), timewarp::project::kFormatVersion + 1);
    newer.insert(QStringLiteral("name"), QStringLiteral("Future"));
    const QString future = temp.write(
        QStringLiteral("future.twproject"), QJsonDocument{newer}.toJson());
    TW_CHECK(!project.load(future, &error));
    TW_CHECK(error.contains(QStringLiteral("format")));

    error.clear();
    QJsonObject malformed;
    malformed.insert(QStringLiteral("format"), 1);
    malformed.insert(QStringLiteral("name"), QStringLiteral("Bad"));
    malformed.insert(QStringLiteral("files"), QStringLiteral("not a list"));
    const QString bad = temp.write(
        QStringLiteral("bad.twproject"), QJsonDocument{malformed}.toJson());
    TW_CHECK(!project.load(bad, &error));
    TW_CHECK(!error.isEmpty());
}

}  // namespace

int main() {
    test_create_and_add();
    test_round_trip();
    test_group_edits();
    test_missing_files();
    test_the_entry_point_round_trips();
    test_a_version_one_project_still_loads();
    test_an_entry_outside_the_file_list_is_added();
    test_bad_files_are_refused();
    return tw::test::summary("project_tests");
}
