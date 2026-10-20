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
    test_bad_files_are_refused();
    return tw::test::summary("project_tests");
}
