// SPDX-License-Identifier: MIT

#include "timewarp/ui/help_browser.hpp"

#include <QAbstractItemView>
#include <QCoreApplication>
#include <QDesktopServices>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QRegularExpression>
#include <QTextBrowser>
#include <QTreeWidget>
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
