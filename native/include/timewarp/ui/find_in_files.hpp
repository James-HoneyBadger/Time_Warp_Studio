// SPDX-License-Identifier: MIT
//
// Find in Files: a workspace-wide search, and the dialog that drives it.
//
// Ported from `ui/find_in_files.py` (610 lines), whose shape was a worker thread
// feeding a results tree. The port keeps that split and adds one thing the
// original did not have: the search itself is a **free function over a directory
// tree**, so the interesting behaviour (what counts as a hit, which files are
// skipped, what an invalid pattern does) is testable without a window, a thread
// or a click.
//
// What it deliberately does not do: follow symlinks out of the tree (a link to
// `/` would make "search the folder" mean "search the machine"), read files that
// are not UTF-8 text, or return an unbounded number of hits.
//
// #### Replacing
//
// The dialog can also replace, and that half is a free function too -- a list of
// paths in, what changed out -- because a bulk edit that writes to somebody's files
// is exactly the code that must be testable without a window. Three rules are
// deliberate:
//
//  * A file is rewritten **once**, through `QSaveFile`, and only when something in
//    it actually matched: a replace that touches forty files to change nothing is a
//    replace that updates forty modification times.
//  * A file the caller names as **off limits** is skipped and reported. The window
//    names the documents with unsaved changes, because writing to disk behind an
//    editor's buffer is how an afternoon of work disappears.
//  * A pattern that cannot be compiled is an **error** and nothing is written.

#pragma once

#include <atomic>
#include <functional>
#include <memory>

#include <QDialog>
#include <QList>
#include <QSet>
#include <QString>
#include <QStringList>
#include <QThread>

class QCheckBox;
class QLabel;
class QLineEdit;
class QPushButton;
class QTreeWidget;

namespace timewarp::ui {

/// What to look for.
struct FindQuery {
    QString term;
    bool case_sensitive = false;
    /// `term` is a regular expression. An invalid one is a reported error, not an
    /// empty result: "no matches" and "your pattern is broken" are different
    /// answers and a learner needs the second.
    bool regex = false;
    /// Stop after this many hits. A single-letter search over a corpus finds
    /// thousands, and neither the tree nor the user can use them.
    int max_hits = 500;
};

/// One matching line.
struct FindHit {
    QString path;
    /// 1-based, the way every editor and compiler counts.
    int line = 0;
    QString text;
};

/// Searches `root` recursively for `query`.
///
/// Files larger than 2 MB are skipped, as are files whose first block contains a
/// NUL byte (binary). A directory that cannot be read is skipped rather than
/// aborting the search. `error` is set when the query itself is unusable.
[[nodiscard]] QList<FindHit> searchInFiles(const QString& root, const FindQuery& query,
                                           QString* error = nullptr);

/// The same search, streaming: `on_hit` sees each hit as it is found and returns
/// `false` to stop.
///
/// This is the implementation; the overload above calls it with no callback. One
/// walk, so the tests exercise exactly the code the worker runs -- the
/// alternative, a second copy of the skipping rules for the streaming case, is
/// how the two would drift apart.
[[nodiscard]] QList<FindHit> searchInFiles(
    const QString& root, const FindQuery& query, QString* error,
    const std::function<bool(const FindHit&)>& on_hit);

/// What a replacement would produce, and how often it fired.
///
/// `error` is set when the query itself is unusable -- an invalid regular
/// expression, an empty term -- and nothing is written in that case.
struct ReplaceResult {
    int files_changed = 0;
    int replacements = 0;
    QStringList changed_paths;
    /// Files that matched but were not written: named off limits by the caller, or
    /// unreadable when the replacement got there.
    QStringList skipped_paths;
    QString error;

    [[nodiscard]] bool failed() const { return !error.isEmpty(); }
};

/// `text` with every match of `query` replaced. `count` receives how many times the
/// pattern fired; `error` is set for a pattern that cannot be compiled.
///
/// A literal replacement is inserted as typed -- a backslash in `C:\temp` stays a
/// backslash -- while a regular expression's replacement may use `\1` and friends,
/// which is what somebody using a regular expression expects.
[[nodiscard]] QString replacedText(const QString& text, const FindQuery& query,
                                   const QString& replacement, int* count = nullptr,
                                   QString* error = nullptr);

/// Replaces across `paths` -- typically the files a search hit, not the whole tree:
/// a user who searched for one thing and presses Replace means those matches.
///
/// `off_limits` is a set of absolute paths not to write. Each file is read again
/// rather than trusting the hit text, because the tree may be a minute old.
[[nodiscard]] ReplaceResult replaceInFiles(const QStringList& paths, const FindQuery& query,
                                           const QString& replacement,
                                           const QSet<QString>& off_limits = {});

/// The worker. A thread of its own because a search over a home directory takes
/// long enough that doing it on the GUI thread would freeze the window, and a
/// frozen window looks like a crash.
class FindInFilesThread final : public QThread {
    Q_OBJECT

public:
    FindInFilesThread(FindQuery query, QString root, QObject* parent = nullptr);

    /// Asks the search to stop at the next line. The flag is the only state the
    /// two threads share, and it is atomic because that is what it is.
    void cancel();
    [[nodiscard]] bool wasCancelled() const;

signals:
    /// Primitives only: a queued connection copies them, so no metatype has to be
    /// registered and a hit cannot arrive after its dialog has gone.
    void hit(const QString& path, int line, const QString& text);
    void finishedWith(const QString& error, int hits);

protected:
    void run() override;

private:
    FindQuery query_;
    QString root_;
    std::shared_ptr<std::atomic<bool>> cancelled_;
};

/// The dialog: term, options, a results tree grouped by file, and a Stop button
/// that exists because a search can take long enough to want out of.
class FindInFilesDialog final : public QDialog {
    Q_OBJECT

public:
    explicit FindInFilesDialog(QWidget* parent = nullptr);
    ~FindInFilesDialog() override;

    /// The directory searched, shown so the user can see what "this folder"
    /// meant.
    void setRoot(const QString& path);
    [[nodiscard]] QString root() const { return root_; }
    /// Pre-fills the term (the editor passes its selection).
    void setTerm(const QString& term);
    /// Runs the search as if the button had been pressed, so the action can
    /// search a selection without a second Enter.
    void startSearch();
    /// How many rows the last search produced. For the tests.
    [[nodiscard]] int hitCount() const;
    /// Documents the replacement must not touch, because their buffer holds work
    /// that is not on disk. The window supplies them before the dialog is used.
    void setOffLimits(const QSet<QString>& paths);
    /// Replaces the search's matches, asking first. Public so a test can drive it
    /// without a mouse; the confirmation is the only part a person adds.
    void replaceAll();
    /// The replacement field, so an action can pre-fill it.
    [[nodiscard]] QLineEdit* replacementField() const noexcept { return replacement_; }

signals:
    /// A result row was activated. `line` is 1-based.
    void hitActivated(const QString& path, int line);
    /// Files were rewritten. The window reloads the ones it has open, because a
    /// buffer that still holds the old text would write it back on the next save.
    void filesReplaced(const QStringList& paths);

private:
    void stopSearch();
    void addHit(const QString& path, int line, const QString& text);
    void showMessage(const QString& text);

    QLineEdit* term_ = nullptr;
    QLineEdit* replacement_ = nullptr;
    QCheckBox* case_ = nullptr;
    QCheckBox* regex_ = nullptr;
    QLabel* root_label_ = nullptr;
    QLabel* status_ = nullptr;
    QTreeWidget* results_ = nullptr;
    QPushButton* search_ = nullptr;
    QPushButton* stop_ = nullptr;
    QPushButton* replace_ = nullptr;
    QString root_;
    QSet<QString> off_limits_;
    FindInFilesThread* worker_ = nullptr;
    int hits_ = 0;
};

}  // namespace timewarp::ui
