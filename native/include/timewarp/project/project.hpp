// SPDX-License-Identifier: MIT
//
// A project: a named group of programs, grouped into named sections.
//
// A project is a *view over files that already exist*, not a copy of them. The
// programs keep their own paths, so a file opened from the project panel is the
// same file the editor would open, autosave would snapshot and Run would execute.
// What the project adds is the grouping the Explorer cannot express -- "Turtle
// art", "Loops", "Friday's class" -- and one place to reach all of it.
//
// The format is `.twproject`, a small JSON document saved beside the programs
// it names. Paths inside the project root are stored relative, so copying a
// folder (or handing one to a learner) keeps the project working; a file outside
// the root is stored absolutely, because that is the only path that still names
// it.

#pragma once

#include <QList>
#include <QString>
#include <QStringList>

namespace timewarp::project {

/// The format version written by this build. A newer file is refused rather than
/// silently half-read, because a project the user cannot open is a recoverable
/// problem while a project that lost a group is not.
///
/// Version 2 added `entry`: the program **Run Project** runs. A version 1 file
/// still loads -- it simply has no entry point -- so a project written by an
/// older build is not a project this build refuses.
inline constexpr int kFormatVersion = 2;

/// The extension a project file carries, without the dot.
inline constexpr auto kExtension = "twproject";

/// The default file name filter for Open Project dialogs.
[[nodiscard]] QString openFileFilter();

/// One program in a project.
struct ProjectFile {
    /// Absolute, clean path. This is what the editor is asked to open.
    QString path;
    /// The group it belongs to, or empty for the ungrouped files. A group is a
    /// name, not a directory: the file's own path is unchanged.
    QString group;

    [[nodiscard]] bool operator==(const ProjectFile&) const = default;
};

/// A project on disk: name, root folder, groups and files.
///
/// Every mutating method returns whether it changed anything, which is what
/// lets the window save only after a real edit and lets a test tell "already
/// there" from "added".
class Project {
public:
    Project() = default;

    /// A new, unsaved project rooted at `root`. `name` is trimmed; an empty name
    /// becomes the root folder's name. `filePath` defaults to
    /// `<root>/<name>.twproject`.
    static Project create(const QString& name, const QString& root,
                          const QString& filePath = {});

    /// Reads a `.twproject`. On failure returns false and fills `error`; the
    /// object is left in the state it had, so a failed open does not destroy an
    /// already-open project.
    [[nodiscard]] bool load(const QString& filePath, QString* error = nullptr);

    /// Writes the project, creating the parent folder when it does not exist.
    /// Uses `QSaveFile`, so a full disk leaves the previous file intact.
    [[nodiscard]] bool save(QString* error = nullptr) const;

    // -- identity ---------------------------------------------------------

    [[nodiscard]] bool isValid() const noexcept { return !file_path_.isEmpty(); }
    [[nodiscard]] QString name() const noexcept { return name_; }
    [[nodiscard]] QString root() const noexcept { return root_; }
    [[nodiscard]] QString filePath() const noexcept { return file_path_; }

    void setName(const QString& name);

    // -- groups -----------------------------------------------------------

    /// Group names in display order. The ungrouped section is not a group and is
    /// not listed: it is the absence of one.
    [[nodiscard]] QStringList groups() const noexcept { return groups_; }

    /// Adds a group. False when the trimmed name is empty or already exists.
    bool addGroup(const QString& name);
    /// Renames a group and every file in it. False when `from` does not exist.
    bool renameGroup(const QString& from, const QString& to);
    /// Removes a group, returning its files to the ungrouped section. The files
    /// are not deleted: removing a project section is not deleting a program.
    bool removeGroup(const QString& name);

    // -- files ------------------------------------------------------------

    [[nodiscard]] QList<ProjectFile> files() const noexcept { return files_; }
    [[nodiscard]] bool contains(const QString& path) const;
    [[nodiscard]] QString groupOf(const QString& path) const;
    [[nodiscard]] QStringList filesIn(const QString& group) const;

    /// Adds one file, idempotently. A non-empty `group` is created when it does
    /// not exist, because a caller adding to a group is asking for both.
    bool addFile(const QString& path, const QString& group = {});
    /// Adds every file that is not already present; returns how many were added.
    int addFiles(const QStringList& paths, const QString& group = {});
    /// Removes one file from the project. False when it was not a member.
    bool removeFile(const QString& path);
    /// Moves a member to `group`. An empty group ungroups it. False when the
    /// file is not a member or the group does not exist and is not empty.
    bool setGroup(const QString& path, const QString& group);

    /// Entries whose file no longer exists, without changing anything. The panel
    /// shows them rather than hiding a problem: a moved program is something the
    /// user can fix, and a project that silently forgot it is not.
    [[nodiscard]] QStringList missingFiles() const;
    /// Drops those entries. Returns how many were removed.
    int pruneMissing();

    /// `<root>/<relative>`, or the absolute entry itself when it lies outside.
    /// Exposed so the panel can show a short path and a tooltip can show the full
    /// one from the same rule.
    [[nodiscard]] QString displayPath(const QString& path) const;

    // -- the entry point ---------------------------------------------------

    /// The program **Run Project** runs, as an absolute path, or empty when the
    /// project has none. Always a member of the project, which is what keeps the
    /// command from naming a file the dock does not show.
    [[nodiscard]] QString entry() const noexcept { return entry_; }
    [[nodiscard]] bool hasEntry() const noexcept { return !entry_.isEmpty(); }
    /// Makes `path` the entry point, adding it to the project when it is not a
    /// member already. False when the path is empty or cannot be resolved.
    bool setEntry(const QString& path);
    /// Forgets the entry point. False when there was none.
    bool clearEntry();

    // -- serialisation ----------------------------------------------------

    /// The JSON this project writes. Exposed for tests and for a future
    /// export-as-text; `save` is the normal caller.
    [[nodiscard]] QByteArray toJson() const;

private:
    /// Stores `path` relative to `base` when it is inside, else absolute. The
    /// inverse of `resolve`.
    [[nodiscard]] static QString relativeTo(const QString& path, const QString& base);
    [[nodiscard]] static QString resolve(const QString& stored, const QString& base);
    [[nodiscard]] int indexOf(const QString& path) const;
    [[nodiscard]] static bool samePath(const QString& lhs, const QString& rhs);

    QString name_;
    QString root_;
    QString file_path_;
    QStringList groups_;
    QList<ProjectFile> files_;
    QString entry_;
};

}  // namespace timewarp::project
