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

#pragma once

#include <atomic>
#include <functional>
#include <memory>

#include <QDialog>
#include <QList>
#include <QString>
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

signals:
    /// A result row was activated. `line` is 1-based.
    void hitActivated(const QString& path, int line);

private:
    void stopSearch();
    void addHit(const QString& path, int line, const QString& text);
    void showMessage(const QString& text);

    QLineEdit* term_ = nullptr;
    QCheckBox* case_ = nullptr;
    QCheckBox* regex_ = nullptr;
    QLabel* root_label_ = nullptr;
    QLabel* status_ = nullptr;
    QTreeWidget* results_ = nullptr;
    QPushButton* search_ = nullptr;
    QPushButton* stop_ = nullptr;
    QString root_;
    FindInFilesThread* worker_ = nullptr;
    int hits_ = 0;
};

}  // namespace timewarp::ui
