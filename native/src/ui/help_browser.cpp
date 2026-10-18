// SPDX-License-Identifier: MIT

#include "timewarp/ui/help_browser.hpp"

#include <algorithm>

#include "timewarp/ui/syntax_highlighter.hpp"

#include <QAbstractItemView>
#include <QCoreApplication>
#include <QDesktopServices>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QHBoxLayout>
#include <QHash>
#include <QHeaderView>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QRegularExpression>
#include <QTextBrowser>
#include <QTextCursor>
#include <QTextDocument>
#include <QTreeWidget>
#include <QTreeWidgetItem>
#include <QUrl>
#include <QVBoxLayout>

namespace timewarp::ui {
namespace {

/// `[Title](target)` and nothing else. A row that also carries prose in its
/// first cell is not a link, and the index's header and separator rows are
/// skipped by not matching rather than by counting rows.
const QRegularExpression& tableLink() {
    static const QRegularExpression expression{
        QStringLiteral(R"(^\s*\[([^\]]+)\]\(([^)\s]+)\)\s*$)")};
    return expression;
}

[[nodiscard]] QString expandHome(QString path) {
    if (path == QLatin1String("~")) {
        return QDir::homePath();
    }
    if (path.startsWith(QLatin1String("~/"))) {
        path = QDir::homePath() + path.mid(1);
    }
    return path;
}

}  // namespace

QString docsRoot() {
    const QString override = qEnvironmentVariable("TIME_WARP_DOCS").trimmed();
    if (!override.isEmpty()) {
        return QDir::cleanPath(expandHome(override));
    }
    const QString app_dir = QCoreApplication::applicationDirPath();
    // Installed (`share/doc/time-warp-studio` beside bin/), then a build tree
    // (`native/build/time_warp_ide` two levels below the checkout's `docs/`).
    // `INDEX.md` is the marker: a directory without one is not a documentation
    // tree, and an empty pane would not say which path was wrong.
    for (const QString& candidate :
         {app_dir + QStringLiteral("/../share/doc/time-warp-studio"),
          app_dir + QStringLiteral("/../../docs"),
          app_dir + QStringLiteral("/../../../docs")}) {
        if (QFileInfo{candidate + QStringLiteral("/INDEX.md")}.isFile()) {
            return QDir::cleanPath(QFileInfo{candidate}.absoluteFilePath());
        }
    }
    return {};
}

QList<HelpSection> helpContents(const QString& index, const QString& root) {
    QList<HelpSection> sections;
    if (root.isEmpty()) {
        return sections;
    }
    const QDir root_dir{root};

    HelpSection current;
    const auto flush = [&sections, &current] {
        if (!current.topics.isEmpty()) {
            sections.append(current);
        }
        current = HelpSection{};
    };

    const QStringList lines = index.split(QLatin1Char('\n'));
    for (const QString& line : lines) {
        const QString trimmed = line.trimmed();
        if (trimmed.startsWith(QLatin1String("## "))) {
            flush();
            current.title = trimmed.mid(3).trimmed();
            continue;
        }
        if (!trimmed.startsWith(QLatin1Char('|'))) {
            continue;
        }
        const QStringList cells = trimmed.split(QLatin1Char('|'));
        if (cells.size() < 3) {
            continue;
        }
        const QRegularExpressionMatch match = tableLink().match(cells.at(1).trimmed());
        if (!match.hasMatch()) {
            continue;
        }

        QString target = match.captured(2).trimmed();
        if (target.contains(QLatin1String("://")) || QFileInfo{target}.isAbsolute()) {
            continue;
        }
        target = QUrl::fromPercentEncoding(target.toUtf8());
        if (const qsizetype fragment = target.indexOf(QLatin1Char('#')); fragment >= 0) {
            target.truncate(fragment);
        }
        if (target.isEmpty()) {
            continue;
        }

        const QString path = QDir::cleanPath(root_dir.absoluteFilePath(target));
        const QString relative = root_dir.relativeFilePath(path);
        // The index links to the repository's README and issue forms, which are
        // not installed with the documentation. A package would show those rows
        // as dead links; leaving them out of the tree is what keeps the checkout
        // and the package honest about the same set of pages.
        if (relative == QLatin1String("..") || relative.startsWith(QLatin1String("../")) ||
            QFileInfo{relative}.isAbsolute()) {
            continue;
        }
        current.topics.append(HelpTopic{match.captured(1).trimmed(), path});
    }
    flush();
    return sections;
}

QString guideTopic(Language language) {
    if (language == Language::Unknown) {
        return {};
    }
    // One tutorial per language, named by the same settings key the highlighter
    // and the bridge use.
    return QStringLiteral("tutorials/%1.md").arg(settingsKey(language));
}

namespace {

/// The guide's text, read once per documentation root. The inspector asks for a
/// description per keyword, and re-reading a 60 KB file a hundred times to open
/// one dialog is work nobody sees but everybody waits for.
[[nodiscard]] QString guideText() {
    const QString root = docsRoot();
    if (root.isEmpty()) {
        return {};
    }
    static QHash<QString, QString> cache;
    const QString path = QDir{root}.filePath(QStringLiteral("LANGUAGE_GUIDE.md"));
    if (const auto found = cache.constFind(path); found != cache.cend()) {
        return *found;
    }
    QFile file{path};
    const QString text = file.open(QIODevice::ReadOnly) ? QString::fromUtf8(file.readAll())
                                                        : QString{};
    cache.insert(path, text);
    return text;
}

/// A keyword match that works for `eq?` and `set!` as well as `PRINT`: `\b`
/// needs a word character on one side, which a keyword ending in punctuation
/// never has at its end.
[[nodiscard]] QRegularExpression wholeWord(const QString& keyword) {
    return QRegularExpression{
        QStringLiteral("(?<![A-Za-z0-9_])%1(?![A-Za-z0-9_])")
            .arg(QRegularExpression::escape(keyword)),
        QRegularExpression::CaseInsensitiveOption};
}

/// Drops the guide's markup and a leading repetition of the keyword.
[[nodiscard]] QString cleanDescription(QString text, const QString& keyword) {
    text = text.trimmed();
    const QRegularExpression prefix{
        QStringLiteral("^`?\\*{0,2}%1\\*{0,2}\\s*[-:]\\s*")
            .arg(QRegularExpression::escape(keyword)),
        QRegularExpression::CaseInsensitiveOption};
    text.remove(prefix);
    text.remove(QLatin1Char('`'));
    text.remove(QStringLiteral("**"));
    return text.trimmed();
}

}  // namespace

QString guideDescription(Language language, const QString& keyword) {
    if (language == Language::Unknown || keyword.trimmed().isEmpty()) {
        return {};
    }
    const QString guide = guideText();
    if (guide.isEmpty()) {
        return {};
    }

    const QStringList lines = guide.split(QLatin1Char('\n'));
    const QString heading = QStringLiteral("## ") + displayName(language);
    qsizetype first = -1;
    for (qsizetype index = 0; index < lines.size(); ++index) {
        if (lines.at(index).trimmed() == heading) {
            first = index + 1;
            break;
        }
    }
    if (first < 0) {
        return {};
    }

    const QRegularExpression word = wholeWord(keyword);
    // A line that is only a comment, in any of the ten dialects' spellings.
    static const QRegularExpression comment{
        QStringLiteral(R"(^\s*(?:;|'|"|--|//|#|REM(?:ARK)?\b)\s*(.*)$)"),
        QRegularExpression::CaseInsensitiveOption};
    // REXX and COMAL attach their comments to the end of a code line.
    static const QRegularExpression block_comment{QStringLiteral(R"(/\*(.*?)\*/)")};
    static const QRegularExpression trailing_rem{
        QStringLiteral(R"(\bREM(?:ARK)?\b(.*)$)"),
        QRegularExpression::CaseInsensitiveOption};

    bool in_fence = false;
    QString prose;
    for (qsizetype index = first; index < lines.size(); ++index) {
        const QString line = lines.at(index);
        const QString trimmed = line.trimmed();
        if (trimmed.startsWith(QLatin1String("```"))) {
            in_fence = !in_fence;
            continue;
        }
        // The next language starts at the next `## ` heading.
        if (trimmed.startsWith(QLatin1String("## "))) {
            break;
        }

        // A table row: any cell naming the word, with the last cell as meaning.
        if (trimmed.startsWith(QLatin1Char('|'))) {
            const QStringList cells = trimmed.split(QLatin1Char('|'));
            const bool names_keyword = std::any_of(cells.cbegin(), cells.cend(),
                                                   [&word](const QString& cell) {
                                                       return word.match(cell).hasMatch();
                                                   });
            if (names_keyword) {
                for (qsizetype cell = cells.size() - 2; cell >= 1; --cell) {
                    QString meaning = cells.at(cell).trimmed();
                    meaning.remove(QLatin1Char('`'));
                    meaning.remove(QLatin1Char('*'));
                    if (!meaning.isEmpty() && !word.match(meaning).hasMatch()) {
                        return meaning;
                    }
                }
            }
            continue;
        }

        // A whole-line comment: where most of the guides state a meaning.
        const QRegularExpressionMatch comment_match = comment.match(line);
        if (comment_match.hasMatch() && word.match(comment_match.captured(1)).hasMatch()) {
            return cleanDescription(comment_match.captured(1), keyword);
        }
        // A trailing block or REM comment, e.g. COMAL and REXX.
        for (const QRegularExpression& attached : {block_comment, trailing_rem}) {
            const QRegularExpressionMatch match = attached.match(line);
            if (match.hasMatch() && word.match(match.captured(1)).hasMatch()) {
                return cleanDescription(match.captured(1), keyword);
            }
        }

        // Prose outside a fence, kept as a fallback for a command the guide
        // describes in a sentence rather than in a comment.
        if (!in_fence && prose.isEmpty() && !trimmed.startsWith(QLatin1Char('#')) &&
            word.match(trimmed).hasMatch()) {
            prose = cleanDescription(trimmed, keyword);
        }
    }
    return prose;
}

QList<KeywordRow> keywordRows(Language language) {
    QList<KeywordRow> rows;
    const QStringList keywords = keywordsOf(language);
    rows.reserve(keywords.size());
    for (const QString& keyword : keywords) {
        rows.append(KeywordRow{keyword, guideDescription(language, keyword)});
    }
    return rows;
}

HelpBrowserDialog::HelpBrowserDialog(QWidget* parent)
    : QDialog{parent}, root_(docsRoot()) {
    setObjectName(QStringLiteral("helpBrowserDialog"));
    setWindowTitle(tr("Documentation"));
    resize(960, 640);

    contents_ = new QTreeWidget{this};
    contents_->setObjectName(QStringLiteral("helpBrowserContents"));
    contents_->setHeaderHidden(true);
    contents_->setSelectionMode(QAbstractItemView::SingleSelection);

    page_ = new QTextBrowser{this};
    page_->setObjectName(QStringLiteral("helpBrowserText"));
    // Every link is handled here. A local one opens in the pane, an external one
    // in the user's browser; QTextBrowser's default would try to render an
    // `https:` page inside the help window.
    page_->setOpenLinks(false);
    page_->setOpenExternalLinks(false);

    status_ = new QLabel{this};
    status_->setObjectName(QStringLiteral("helpBrowserStatus"));
    status_->setWordWrap(true);

    auto* home = new QPushButton{tr("Home"), this};
    home->setObjectName(QStringLiteral("helpBrowserHome"));
    auto* close = new QPushButton{tr("Close"), this};
    close->setObjectName(QStringLiteral("helpBrowserClose"));
    close->setDefault(true);

    auto* body = new QHBoxLayout;
    body->addWidget(contents_, 1);
    body->addWidget(page_, 3);

    auto* buttons = new QHBoxLayout;
    buttons->addWidget(status_, 1);
    buttons->addWidget(home);
    buttons->addWidget(close);

    auto* layout = new QVBoxLayout{this};
    layout->addLayout(body, 1);
    layout->addLayout(buttons);

    connect(close, &QPushButton::clicked, this, [this] { accept(); });
    connect(home, &QPushButton::clicked, this, [this] { showIndex(); });
    connect(contents_, &QTreeWidget::itemClicked, this, [this](QTreeWidgetItem* item, int) {
        if (item != nullptr) {
            static_cast<void>(openTopic(item->data(0, Qt::UserRole).toString()));
        }
    });
    connect(contents_, &QTreeWidget::itemActivated, this, [this](QTreeWidgetItem* item, int) {
        if (item != nullptr) {
            static_cast<void>(openTopic(item->data(0, Qt::UserRole).toString()));
        }
    });
    connect(page_, &QTextBrowser::sourceChanged, this,
            [this](const QUrl& url) { selectTopic(url.toLocalFile()); });
    connect(page_, &QTextBrowser::anchorClicked, this, [this](const QUrl& url) {
        if (url.scheme().isEmpty() || url.scheme() == QLatin1String("file")) {
            if (url.path().isEmpty() && !url.fragment().isEmpty()) {
                page_->scrollToAnchor(url.fragment());
                return;
            }
            const QString base = QFileInfo{page_->source().toLocalFile()}.absolutePath();
            const QString target = url.isRelative()
                                       ? QDir{base}.filePath(url.path())
                                       : url.toLocalFile();
            if (!openTopic(target)) {
                status_->setText(tr("That page is not part of the documentation: %1")
                                     .arg(url.toString()));
            }
            return;
        }
        QDesktopServices::openUrl(url);
    });

    buildContents(root_);
    if (!showIndex()) {
        page_->setHtml(tr("<h1>Documentation is not installed</h1>"
                          "<p>No <code>INDEX.md</code> was found at <code>%1</code>.</p>"
                          "<p>Install the documentation with the application, or set "
                          "<code>TIME_WARP_DOCS</code> to a checkout's <code>docs</code> "
                          "directory.</p>")
                           .arg(root_.isEmpty() ? tr("share/doc/time-warp-studio")
                                                : root_.toHtmlEscaped()));
        status_->setText(tr("No documentation was found."));
    }
}

int HelpBrowserDialog::topicCount() const {
    int count = 0;
    for (int group = 0; group < contents_->topLevelItemCount(); ++group) {
        count += contents_->topLevelItem(group)->childCount();
    }
    return count;
}

bool HelpBrowserDialog::openTopic(const QString& path) {
    const QFileInfo info{QFileInfo{path}.absoluteFilePath()};
    if (!info.isFile()) {
        return false;
    }
    page_->setSource(QUrl::fromLocalFile(info.absoluteFilePath()));
    if (!root_.isEmpty()) {
        status_->setText(QDir{root_}.relativeFilePath(info.absoluteFilePath()));
    }
    return true;
}

bool HelpBrowserDialog::showIndex() {
    return root_.isEmpty() ? false : openTopic(QDir{root_}.filePath(QStringLiteral("INDEX.md")));
}

bool HelpBrowserDialog::findInPage(const QString& text) {
    if (text.trimmed().isEmpty()) {
        return false;
    }
    QTextCursor from = page_->textCursor();
    from.movePosition(QTextCursor::Start);
    const QTextCursor found = page_->document()->find(wholeWord(text.trimmed()), from);
    if (found.isNull()) {
        return false;
    }
    page_->setTextCursor(found);
    page_->ensureCursorVisible();
    return true;
}

void HelpBrowserDialog::buildContents(const QString& root) {
    contents_->clear();
    QFile file{root.isEmpty() ? QString{} : QDir{root}.filePath(QStringLiteral("INDEX.md"))};
    if (!file.open(QIODevice::ReadOnly)) {
        return;
    }
    const QString index = QString::fromUtf8(file.readAll());
    file.close();

    for (const HelpSection& section : helpContents(index, root)) {
        auto* group = new QTreeWidgetItem{contents_, {section.title}};
        group->setFlags(group->flags() & ~Qt::ItemIsSelectable);
        group->setExpanded(true);
        for (const HelpTopic& topic : section.topics) {
            auto* item = new QTreeWidgetItem{group, {topic.title}};
            item->setData(0, Qt::UserRole, topic.path);
            item->setToolTip(0, QDir{root}.relativeFilePath(topic.path));
        }
    }
    contents_->expandAll();
}

KeywordInspectorDialog::KeywordInspectorDialog(Language language, QWidget* parent)
    : QDialog{parent}, language_(language) {
    setObjectName(QStringLiteral("keywordInspector"));
    setWindowTitle(tr("%1 Keywords").arg(displayName(language)));
    resize(680, 560);

    auto* heading = new QLabel{
        tr("%1 words, as the editor colours them. Type to filter; the second "
           "column is what the language guide says about each.")
            .arg(displayName(language)),
        this};
    heading->setObjectName(QStringLiteral("keywordInspectorHeading"));
    heading->setWordWrap(true);

    filter_ = new QLineEdit{this};
    filter_->setObjectName(QStringLiteral("keywordInspectorFilter"));
    filter_->setPlaceholderText(tr("Filter keywords"));
    filter_->setClearButtonEnabled(true);

    rows_ = new QTreeWidget{this};
    rows_->setObjectName(QStringLiteral("keywordInspectorList"));
    rows_->setHeaderLabels({tr("Keyword"), tr("Guide")});
    rows_->setRootIsDecorated(false);
    rows_->setAlternatingRowColors(true);
    rows_->header()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    rows_->header()->setStretchLastSection(true);

    empty_ = new QLabel{this};
    empty_->setObjectName(QStringLiteral("keywordInspectorEmpty"));
    empty_->setVisible(false);

    auto* close = new QPushButton{tr("Close"), this};
    close->setObjectName(QStringLiteral("keywordInspectorClose"));
    close->setDefault(true);

    for (const KeywordRow& row : keywordRows(language)) {
        auto* item = new QTreeWidgetItem{rows_, {row.keyword, row.description}};
        // The guide line can be long; the tooltip is where the whole of it stays
        // readable when the column is narrow.
        item->setToolTip(1, row.description);
    }

    auto* layout = new QVBoxLayout{this};
    layout->addWidget(heading);
    layout->addWidget(filter_);
    layout->addWidget(rows_, 1);
    layout->addWidget(empty_);
    auto* buttons = new QHBoxLayout;
    buttons->addStretch(1);
    buttons->addWidget(close);
    layout->addLayout(buttons);

    connect(filter_, &QLineEdit::textChanged, this, [this] { applyFilter(); });
    connect(close, &QPushButton::clicked, this, [this] { accept(); });
    applyFilter();
}

int KeywordInspectorDialog::rowCount() const { return rows_->topLevelItemCount(); }

int KeywordInspectorDialog::visibleRowCount() const {
    int visible = 0;
    for (int index = 0; index < rows_->topLevelItemCount(); ++index) {
        if (!rows_->topLevelItem(index)->isHidden()) {
            ++visible;
        }
    }
    return visible;
}

void KeywordInspectorDialog::setFilter(const QString& text) {
    filter_->setText(text);
    applyFilter();
}

void KeywordInspectorDialog::applyFilter() {
    const QString term = filter_->text().trimmed();
    int visible = 0;
    for (int index = 0; index < rows_->topLevelItemCount(); ++index) {
        QTreeWidgetItem* item = rows_->topLevelItem(index);
        const bool matches = term.isEmpty() ||
                             item->text(0).contains(term, Qt::CaseInsensitive) ||
                             item->text(1).contains(term, Qt::CaseInsensitive);
        item->setHidden(!matches);
        if (matches) {
            ++visible;
        }
    }
    if (rows_->topLevelItemCount() == 0) {
        empty_->setText(tr("This language has no keywords."));
        empty_->setVisible(true);
    } else if (visible == 0) {
        empty_->setText(tr("No keyword matches that filter."));
        empty_->setVisible(true);
    } else {
        empty_->setVisible(false);
    }
}

void HelpBrowserDialog::selectTopic(const QString& path) {
    if (path.isEmpty()) {
        return;
    }
    const QString wanted = QFileInfo{path}.absoluteFilePath();
    for (int group = 0; group < contents_->topLevelItemCount(); ++group) {
        QTreeWidgetItem* parent = contents_->topLevelItem(group);
        for (int child = 0; child < parent->childCount(); ++child) {
            QTreeWidgetItem* item = parent->child(child);
            if (QFileInfo{item->data(0, Qt::UserRole).toString()}.absoluteFilePath() == wanted) {
                contents_->setCurrentItem(item);
                return;
            }
        }
    }
}

}  // namespace timewarp::ui
