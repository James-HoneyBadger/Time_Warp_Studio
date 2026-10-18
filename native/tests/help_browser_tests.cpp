// SPDX-License-Identifier: MIT
//
// The documentation browser's two halves, tested without opening a window: the
// resolver that finds the docs and the parser that turns `INDEX.md` into a
// contents tree. The dead-link check is the point of the suite -- a table row
// pointing at a file that is not there is invisible until a learner clicks it.
//
// Runs offscreen, so it belongs in CI beside the other suites.

#include <cstdio>
#include <string>

#include <QApplication>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QTextBrowser>
#include <QTreeWidget>
#include <QTreeWidgetItem>

#include "check.hpp"
#include "temp_dir.hpp"
#include "timewarp/ui/help_browser.hpp"

namespace {

void announce(const char* what) { std::printf("-- %s\n", what); }

[[nodiscard]] QString readFile(const QString& path) {
    QFile file{path};
    if (!file.open(QIODevice::ReadOnly)) {
        return {};
    }
    return QString::fromUtf8(file.readAll());
}

/// `TIME_WARP_DOCS` is an explicit override, not a hint: the test can point the
/// browser at a tree of its own, and an override that names nothing is reported
/// rather than silently replaced by the checkout.
void test_docs_root_follows_the_override() {
    announce("docs root: TIME_WARP_DOCS, then the checkout");
    tw::test::TempDir scratch;
    TW_CHECK(scratch.valid());
    if (!scratch.valid()) {
        return;
    }
    const QString root = scratch.path(QStringLiteral("docs"));
    scratch.write(QStringLiteral("docs/INDEX.md"), "# Index\n");
    qputenv("TIME_WARP_DOCS", root.toUtf8());
    TW_CHECK_EQ(timewarp::ui::docsRoot().toStdString(), root.toStdString());
    qunsetenv("TIME_WARP_DOCS");

    // With no override the test binary is in `native/build`, so the resolver
    // walks up to the checkout's `docs/`.
    const QString checkout = timewarp::ui::docsRoot();
    TW_CHECK(!checkout.isEmpty());
    TW_CHECK(QFileInfo{checkout + QStringLiteral("/INDEX.md")}.isFile());
}

/// Every in-root row of the real index becomes a topic, its path exists, and a
/// row that points outside the docs tree is left out: those files are not
/// installed with the application, so a package would show them as dead links.
void test_contents_come_from_the_index() {
    announce("contents: the index's own tables");
    const QString root = timewarp::ui::docsRoot();
    TW_CHECK(!root.isEmpty());
    if (root.isEmpty()) {
        return;
    }
    const QString index = readFile(root + QStringLiteral("/INDEX.md"));
    TW_CHECK(!index.isEmpty());
    if (index.isEmpty()) {
        return;
    }

    const QList<timewarp::ui::HelpSection> sections =
        timewarp::ui::helpContents(index, root);
    TW_CHECK(!sections.isEmpty());

    int topics = 0;
    bool saw_tutorials = false;
    const QDir root_dir{root};
    for (const timewarp::ui::HelpSection& section : sections) {
        TW_CHECK(!section.title.isEmpty());
        TW_CHECK(!section.topics.isEmpty());
        if (section.title == QLatin1String("Language tutorials")) {
            saw_tutorials = true;
            TW_CHECK_EQ(section.topics.size(), 10);
        }
        for (const timewarp::ui::HelpTopic& topic : section.topics) {
            ++topics;
            TW_CHECK(!topic.title.isEmpty());
            // The dead-link check: every promise the tree makes is a file.
            TW_CHECK(QFileInfo{topic.path}.isFile());
            // And none of them leaves the documentation root.
            const QString relative = root_dir.relativeFilePath(topic.path);
            TW_CHECK(relative != QLatin1String(".."));
            TW_CHECK(!relative.startsWith(QLatin1String("../")));
        }
    }
    TW_CHECK(saw_tutorials);
    TW_CHECK(topics > 15);

    // The repository-only links (`../README.md`, `.github/SUPPORT.md`, the
    // examples catalogue) are not in `share/doc`, so they are not in the tree.
    for (const timewarp::ui::HelpSection& section : sections) {
        for (const timewarp::ui::HelpTopic& topic : section.topics) {
            TW_CHECK(!topic.path.endsWith(QLatin1String("/native/README.md")));
            TW_CHECK(topic.path != root_dir.absoluteFilePath(QStringLiteral("../README.md")));
        }
    }
}

/// A row pointing at a file that is not there is still returned: the dialog is
/// not allowed to hide a broken promise, and this suite is what notices it.
void test_a_broken_link_survives_parsing() {
    announce("contents: a broken row is kept for the check to see");
    const QString index = QStringLiteral(
        "## Things\n\n"
        "| Document | What it covers |\n"
        "| --- | --- |\n"
        "| [Missing](missing.md) | not there |\n");
    const QList<timewarp::ui::HelpSection> sections =
        timewarp::ui::helpContents(index, QStringLiteral("/tmp/docs"));
    TW_CHECK_EQ(sections.size(), 1);
    if (sections.size() == 1 && sections.first().topics.size() == 1) {
        TW_CHECK_EQ(sections.first().topics.first().path.toStdString(),
                    std::string{"/tmp/docs/missing.md"});
        TW_CHECK(!QFileInfo{sections.first().topics.first().path}.isFile());
    }
}

/// The dialog lists the tree and renders a chosen page as Markdown, not as the
/// literal file. Opening something that is not there leaves the page alone.
void test_the_dialog_lists_and_opens_pages() {
    announce("dialog: tree, page and selection");
    const QString root = timewarp::ui::docsRoot();
    TW_CHECK(!root.isEmpty());
    if (root.isEmpty()) {
        return;
    }
    timewarp::ui::HelpBrowserDialog dialog;
    TW_CHECK(dialog.topicCount() > 15);

    auto* tree = dialog.findChild<QTreeWidget*>(QStringLiteral("helpBrowserContents"));
    TW_CHECK(tree != nullptr);
    if (tree != nullptr) {
        TW_CHECK(tree->topLevelItemCount() > 0);
    }

    const QString faq = QDir{root}.filePath(QStringLiteral("FAQ.md"));
    TW_CHECK(dialog.openTopic(faq));
    TW_CHECK(dialog.page()->toPlainText().contains(
        QStringLiteral("Frequently Asked Questions")));
    // Rendered, not dumped: `setSource` selects the Markdown resource.
    TW_CHECK(dialog.page()->toHtml().contains(QStringLiteral("<h1")));
    if (tree != nullptr && tree->currentItem() != nullptr) {
        TW_CHECK(tree->currentItem()->data(0, Qt::UserRole)
                     .toString()
                     .endsWith(QStringLiteral("FAQ.md")));
    }

    // A page that does not exist is refused, and the FAQ stays on screen.
    TW_CHECK(!dialog.openTopic(QDir{root}.filePath(QStringLiteral("missing.md"))));
    TW_CHECK(dialog.page()->toPlainText().contains(
        QStringLiteral("Frequently Asked Questions")));
    TW_CHECK(!dialog.openTopic(QString{}));
}

/// With nothing to show, the pane explains what is missing and where it looked;
/// an empty white box is the failure the plan names.
void test_a_missing_root_is_explained() {
    announce("dialog: a missing docs root");
    tw::test::TempDir scratch;
    TW_CHECK(scratch.valid());
    if (!scratch.valid()) {
        return;
    }
    // The directory exists but has no INDEX.md: the override is authoritative.
    qputenv("TIME_WARP_DOCS", scratch.path().toUtf8());
    timewarp::ui::HelpBrowserDialog dialog;
    TW_CHECK_EQ(dialog.topicCount(), 0);
    TW_CHECK(dialog.page()->toPlainText().contains(
        QStringLiteral("Documentation is not installed")));
    qunsetenv("TIME_WARP_DOCS");
}

}  // namespace

int main(int argc, char** argv) {
    // Set before QApplication exists, or Qt tries to open a real display.
    qputenv("QT_QPA_PLATFORM", "offscreen");
    QApplication app{argc, argv};

    test_docs_root_follows_the_override();
    test_contents_come_from_the_index();
    test_a_broken_link_survives_parsing();
    test_the_dialog_lists_and_opens_pages();
    test_a_missing_root_is_explained();
    return tw::test::summary("help_browser");
}
