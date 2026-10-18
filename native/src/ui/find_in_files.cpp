// SPDX-License-Identifier: MIT

#include "timewarp/ui/find_in_files.hpp"

#include <QCheckBox>
#include <QDir>
#include <QDirIterator>
#include <QFile>
#include <QFileInfo>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QRegularExpression>
#include <QStringList>
#include <QTreeWidget>
#include <QVBoxLayout>

namespace timewarp::ui {
namespace {

/// Anything larger is a log or a build artifact, and reading it would stall the
/// search for a hit nobody meant.
constexpr qint64 kMaxFileBytes = 2 * 1024 * 1024;
/// How much of a file to look at when deciding whether it is text.
constexpr qint64 kSniffBytes = 8 * 1024;

/// True when the file looks like text: no NUL byte in its first block.
///
/// The cheap test every search tool uses. A UTF-16 file is misread as binary by
/// it, which is the accepted cost of not scanning a JPEG line by line.
[[nodiscard]] bool looksLikeText(const QString& path) {
    QFile file{path};
    if (!file.open(QIODevice::ReadOnly)) {
        return false;
    }
    const QByteArray head = file.read(kSniffBytes);
    return !head.contains('\0');
}

/// Directories a search should not descend into, by name: a `.git` holds
/// thousands of files a learner will never open, and a build tree holds copies of
/// the source being edited.
[[nodiscard]] bool isSkippedDirectory(const QString& name) {
    static const QStringList skipped{
        QStringLiteral(".git"),        QStringLiteral(".hg"),         QStringLiteral(".svn"),
        QStringLiteral("node_modules"), QStringLiteral("__pycache__"), QStringLiteral(".venv"),
        QStringLiteral("build"),       QStringLiteral("build-debug"), QStringLiteral(".ruff_cache"),
        QStringLiteral(".mypy_cache"), QStringLiteral(".pytest_cache")};
    return skipped.contains(name);
}

/// True when any component of `relative` is a directory this search skips.
[[nodiscard]] bool isInSkippedDirectory(const QString& relative) {
    for (const QString& part : relative.split(QLatin1Char('/'))) {
        if (isSkippedDirectory(part)) {
            return true;
        }
    }
    return false;
}

/// The pattern for `query`, compiled once per search rather than once per line.
/// An invalid pattern is reported by the caller before this is used.
[[nodiscard]] QRegularExpression compiledPattern(const FindQuery& query) {
    if (!query.regex) {
        return {};
    }
    return QRegularExpression{query.term, query.case_sensitive
                                               ? QRegularExpression::NoPatternOption
                                               : QRegularExpression::CaseInsensitiveOption};
}

[[nodiscard]] bool lineMatches(const QString& line, const FindQuery& query,
                               const QRegularExpression& pattern) {
    if (query.regex) {
        return pattern.match(line).hasMatch();
    }
    return line.contains(query.term, query.case_sensitive ? Qt::CaseSensitive
                                                          : Qt::CaseInsensitive);
}

}  // namespace

QList<FindHit> searchInFiles(const QString& root, const FindQuery& query, QString* error) {
    return searchInFiles(root, query, error, {});
}

QList<FindHit> searchInFiles(const QString& root, const FindQuery& query, QString* error,
                             const std::function<bool(const FindHit&)>& on_hit) {
    QList<FindHit> hits;
    const auto fail = [error](const QString& message) {
        if (error != nullptr) {
            *error = message;
        }
        return QList<FindHit>{};
    };

    const QString base = QFileInfo{root}.absoluteFilePath();
    if (root.isEmpty() || !QFileInfo{base}.isDir()) {
        return fail(QStringLiteral("%1 is not a folder.").arg(root));
    }
    if (query.term.isEmpty()) {
        return fail(QStringLiteral("Type something to search for."));
    }
    if (query.regex) {
        const QRegularExpression probe{query.term};
        if (!probe.isValid()) {
            return fail(QStringLiteral("That is not a valid regular expression: %1")
                            .arg(probe.errorString()));
        }
    }

    const QRegularExpression pattern = compiledPattern(query);
    const QDir base_dir{base};
    QDirIterator it{base, QDir::Files | QDir::NoSymLinks, QDirIterator::Subdirectories};
    bool stop = false;
    while (it.hasNext() && !stop) {
        const QString path = it.next();
        if (hits.size() >= query.max_hits) {
            break;
        }
        if (isInSkippedDirectory(base_dir.relativeFilePath(path))) {
            continue;
        }
        if (QFileInfo{path}.size() > kMaxFileBytes || !looksLikeText(path)) {
            continue;
        }
        QFile file{path};
        if (!file.open(QIODevice::ReadOnly)) {
            continue;  // unreadable is a file to skip, not a search failure
        }
        const QString text = QString::fromUtf8(file.readAll());
        file.close();

        const QStringList lines = text.split(QLatin1Char('\n'));
        for (int index = 0; index < lines.size(); ++index) {
            if (!lineMatches(lines.at(index), query, pattern)) {
                continue;
            }
            const FindHit hit{path, index + 1, lines.at(index).trimmed()};
            hits.append(hit);
            if (on_hit && !on_hit(hit)) {
                stop = true;
                break;
            }
            if (hits.size() >= query.max_hits) {
                stop = true;
                break;
            }
        }
    }
    if (error != nullptr) {
        error->clear();
    }
    return hits;
}

// ---------------------------------------------------------------------------
// The worker
// ---------------------------------------------------------------------------

FindInFilesThread::FindInFilesThread(FindQuery query, QString root, QObject* parent)
    : QThread{parent},
      query_(std::move(query)),
      root_(std::move(root)),
      cancelled_(std::make_shared<std::atomic<bool>>(false)) {}

void FindInFilesThread::cancel() { cancelled_->store(true, std::memory_order_relaxed); }

bool FindInFilesThread::wasCancelled() const {
    return cancelled_->load(std::memory_order_relaxed);
}

void FindInFilesThread::run() {
    // The same function the tests call. The callback is where cancelling and
    // reporting live, so there is one set of skipping rules and one place they
    // can be wrong.
    int found = 0;
    QString error;
    // The return value is the list the dialog does not need: the hits have
    // already gone out as signals, and collecting them again would double the
    // memory for a search whose results are on screen.
    static_cast<void>(searchInFiles(root_, query_, &error, [this, &found](const FindHit& hit) {
        if (wasCancelled()) {
            return false;
        }
        ++found;
        emit this->hit(hit.path, hit.line, hit.text);
        return true;
    }));
    emit finishedWith(error, found);
}

// ---------------------------------------------------------------------------
// The dialog
// ---------------------------------------------------------------------------

FindInFilesDialog::FindInFilesDialog(QWidget* parent) : QDialog{parent} {
    setObjectName(QStringLiteral("findInFilesDialog"));
    setWindowTitle(tr("Find in Files"));
    resize(760, 480);

    term_ = new QLineEdit{this};
    term_->setObjectName(QStringLiteral("findInFilesTerm"));
    term_->setPlaceholderText(tr("Find in files\u2026"));
    term_->setClearButtonEnabled(true);

    case_ = new QCheckBox{tr("Match case"), this};
    case_->setObjectName(QStringLiteral("findInFilesCase"));
    regex_ = new QCheckBox{tr("Regular expression"), this};
    regex_->setObjectName(QStringLiteral("findInFilesRegex"));

    root_label_ = new QLabel{this};
    root_label_->setObjectName(QStringLiteral("findInFilesRoot"));
    root_label_->setTextInteractionFlags(Qt::TextSelectableByMouse);

    search_ = new QPushButton{tr("Search"), this};
    search_->setObjectName(QStringLiteral("findInFilesSearch"));
    search_->setDefault(true);
    stop_ = new QPushButton{tr("Stop"), this};
    stop_->setObjectName(QStringLiteral("findInFilesStop"));
    stop_->setEnabled(false);

    results_ = new QTreeWidget{this};
    results_->setObjectName(QStringLiteral("findInFilesResults"));
    results_->setColumnCount(2);
    results_->setHeaderLabels({tr("File"), tr("Line")});
    results_->header()->setSectionResizeMode(0, QHeaderView::Stretch);
    results_->header()->setSectionResizeMode(1, QHeaderView::ResizeToContents);

    status_ = new QLabel{this};
    status_->setObjectName(QStringLiteral("findInFilesStatus"));
    status_->setText(tr("Searches the folder of the current document."));

    auto* options = new QHBoxLayout;
    options->addWidget(case_);
    options->addWidget(regex_);
    options->addStretch(1);
    options->addWidget(search_);
    options->addWidget(stop_);

    auto* layout = new QVBoxLayout{this};
    layout->addWidget(term_);
    layout->addWidget(root_label_);
    layout->addLayout(options);
    layout->addWidget(results_, 1);
    layout->addWidget(status_);

    connect(search_, &QPushButton::clicked, this, [this] { startSearch(); });
    connect(stop_, &QPushButton::clicked, this, [this] { stopSearch(); });
    connect(term_, &QLineEdit::returnPressed, this, [this] { startSearch(); });
    connect(results_, &QTreeWidget::itemActivated, this, [this](QTreeWidgetItem* item, int) {
        if (item == nullptr || item->parent() == nullptr) {
            return;
        }
        emit hitActivated(item->parent()->data(0, Qt::UserRole).toString(),
                          item->data(1, Qt::UserRole).toInt());
    });
}

FindInFilesDialog::~FindInFilesDialog() {
    if (worker_ != nullptr) {
        worker_->cancel();
        // Bounded: the flag is checked per hit, so this waits for one file, not
        // for the tree.
        worker_->wait(2000);
    }
}

void FindInFilesDialog::setRoot(const QString& path) {
    root_ = path;
    root_label_->setText(tr("In: %1").arg(QDir::toNativeSeparators(path)));
}

void FindInFilesDialog::setTerm(const QString& term) {
    term_->setText(term);
    term_->selectAll();
}

void FindInFilesDialog::showMessage(const QString& text) { status_->setText(text); }

int FindInFilesDialog::hitCount() const { return hits_; }

void FindInFilesDialog::startSearch() {
    if (worker_ != nullptr && worker_->isRunning()) {
        return;
    }
    results_->clear();
    hits_ = 0;

    FindQuery query;
    query.term = term_->text();
    query.case_sensitive = case_->isChecked();
    query.regex = regex_->isChecked();
    if (query.term.isEmpty()) {
        showMessage(tr("Type something to search for."));
        return;
    }

    worker_ = new FindInFilesThread{query, root_, this};
    connect(worker_, &FindInFilesThread::hit, this,
            [this](const QString& path, int line, const QString& text) { addHit(path, line, text); });
    connect(worker_, &FindInFilesThread::finishedWith, this,
            [this](const QString& error, int found) {
                search_->setEnabled(true);
                stop_->setEnabled(false);
                if (!error.isEmpty()) {
                    showMessage(error);
                } else if (worker_ != nullptr && worker_->wasCancelled()) {
                    showMessage(tr("Stopped after %1 match(es).").arg(found));
                } else if (found == 0) {
                    showMessage(tr("No matches."));
                } else {
                    showMessage(tr("%1 match(es).").arg(found));
                }
            });
    // The dialog owns the thread and `run()` returns on its own; deleting it
    // after its `finished` signal is what keeps a second search from leaking the
    // first.
    connect(worker_, &QThread::finished, worker_, &QObject::deleteLater);

    search_->setEnabled(false);
    stop_->setEnabled(true);
    showMessage(tr("Searching\u2026"));
    worker_->start();
}

void FindInFilesDialog::stopSearch() {
    if (worker_ != nullptr) {
        worker_->cancel();
    }
}

void FindInFilesDialog::addHit(const QString& path, int line, const QString& text) {
    ++hits_;
    QTreeWidgetItem* file_item = nullptr;
    for (int index = 0; index < results_->topLevelItemCount(); ++index) {
        if (results_->topLevelItem(index)->data(0, Qt::UserRole).toString() == path) {
            file_item = results_->topLevelItem(index);
            break;
        }
    }
    if (file_item == nullptr) {
        file_item = new QTreeWidgetItem{results_};
        file_item->setText(0, QDir::toNativeSeparators(QFileInfo{path}.fileName()));
        file_item->setToolTip(0, QDir::toNativeSeparators(path));
        file_item->setData(0, Qt::UserRole, path);
        file_item->setExpanded(true);
    }
    auto* row = new QTreeWidgetItem{file_item};
    row->setText(0, text);
    row->setText(1, QString::number(line));
    row->setData(0, Qt::UserRole, path);
    row->setData(1, Qt::UserRole, line);
}

}  // namespace timewarp::ui
