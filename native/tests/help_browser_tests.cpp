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
#include <QSet>
#include <QTextBrowser>
#include <QTextCursor>
#include <QTreeWidget>
#include <QTreeWidgetItem>

#include "check.hpp"
#include "temp_dir.hpp"
#include "timewarp/ui/help_browser.hpp"
#include "timewarp/ui/syntax_highlighter.hpp"

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
    const QByteArray inherited = qgetenv("TIME_WARP_DOCS");
    qputenv("TIME_WARP_DOCS", root.toUtf8());
    TW_CHECK_EQ(timewarp::ui::docsRoot().toStdString(), root.toStdString());
    // Restored rather than cleared. Without an override the resolver walks up
    // from the executable to the checkout's `docs/`, which works from
    // `native/build`; from any other build directory `main()` has pointed the
    // override at that same checkout, and clearing it would take the tree away
    // from every test that follows.
    if (inherited.isEmpty()) {
        qunsetenv("TIME_WARP_DOCS");
    } else {
        qputenv("TIME_WARP_DOCS", inherited);
    }

    // Either the walk-up or the inherited path must name a browsable tree.
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

/// F1's target: one tutorial per language, and the file exists.
void test_guide_topics_follow_the_languages() {
    announce("context help: a guide per language");
    const QString root = timewarp::ui::docsRoot();
    TW_CHECK(!root.isEmpty());
    if (root.isEmpty()) {
        return;
    }
    for (const timewarp::ui::Language language : timewarp::ui::allLanguages()) {
        const QString topic = timewarp::ui::guideTopic(language);
        TW_CHECK_EQ(topic.toStdString(),
                    QStringLiteral("tutorials/%1.md")
                        .arg(timewarp::ui::settingsKey(language))
                        .toStdString());
        TW_CHECK(QFileInfo{QDir{root}.filePath(topic)}.isFile());
    }
    TW_CHECK(timewarp::ui::guideTopic(timewarp::ui::Language::Unknown).isEmpty());
}

/// The inspector is the highlighter's vocabulary, not a second list that can
/// drift from it, and the guide supplies a description where it states one.
void test_every_highlighted_keyword_has_a_row() {
    announce("keyword inspector: the highlighter's words");
    for (const timewarp::ui::Language language : timewarp::ui::allLanguages()) {
        const QStringList highlighted = timewarp::ui::keywordsOf(language);
        const QList<timewarp::ui::KeywordRow> rows = timewarp::ui::keywordRows(language);
        TW_CHECK(!highlighted.isEmpty());
        TW_CHECK_EQ(rows.size(), highlighted.size());

        QSet<QString> from_highlighter;
        for (const QString& keyword : highlighted) {
            from_highlighter.insert(keyword);
        }
        QSet<QString> from_inspector;
        for (const timewarp::ui::KeywordRow& row : rows) {
            from_inspector.insert(row.keyword);
        }
        TW_CHECK(from_inspector == from_highlighter);
    }

    // Logo's guide describes FORWARD in a comment beside its example; that is
    // the line the inspector should show.
    const QList<timewarp::ui::KeywordRow> logo = timewarp::ui::keywordRows(timewarp::ui::Language::Logo);
    bool saw_forward = false;
    for (const timewarp::ui::KeywordRow& row : logo) {
        if (row.keyword == QLatin1String("FORWARD")) {
            saw_forward = true;
            TW_CHECK(!row.description.isEmpty());
            TW_CHECK(row.description.contains(QStringLiteral("forward"), Qt::CaseInsensitive));
        }
    }
    TW_CHECK(saw_forward);
}

/// Ctrl+F1's dialog: every row, and the filter narrows by either column.
void test_the_inspector_filters() {
    announce("keyword inspector: filtering");
    timewarp::ui::KeywordInspectorDialog dialog{timewarp::ui::Language::Logo};
    TW_CHECK(dialog.rowCount() > 20);
    TW_CHECK_EQ(dialog.visibleRowCount(), dialog.rowCount());

    dialog.setFilter(QStringLiteral("PEN"));
    TW_CHECK(dialog.visibleRowCount() > 0);
    TW_CHECK(dialog.visibleRowCount() < dialog.rowCount());

    dialog.setFilter(QStringLiteral("no-such-keyword"));
    TW_CHECK_EQ(dialog.visibleRowCount(), 0);

    dialog.setFilter(QString{});
    TW_CHECK_EQ(dialog.visibleRowCount(), dialog.rowCount());
}

/// F1 scrolls the guide to the command under the cursor, by whole word: `TO`
/// must not land in the middle of "Turtle".
void test_find_in_page_selects_a_whole_word() {
    announce("context help: finding the command");
    const QString root = timewarp::ui::docsRoot();
    TW_CHECK(!root.isEmpty());
    if (root.isEmpty()) {
        return;
    }
    timewarp::ui::HelpBrowserDialog dialog;
    TW_CHECK(dialog.openTopic(QDir{root}.filePath(QStringLiteral("tutorials/logo.md"))));
    TW_CHECK(dialog.findInPage(QStringLiteral("FORWARD")));
    // Case-insensitively, because the guide's first mention is "Move forward".
    TW_CHECK_EQ(dialog.page()->textCursor().selectedText().toUpper().toStdString(),
                std::string{"FORWARD"});
    TW_CHECK(!dialog.findInPage(QStringLiteral("a phrase the guide does not contain")));
    TW_CHECK(!dialog.findInPage(QString{}));
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
    const QByteArray inherited = qgetenv("TIME_WARP_DOCS");
    qputenv("TIME_WARP_DOCS", scratch.path().toUtf8());
    timewarp::ui::HelpBrowserDialog dialog;
    TW_CHECK_EQ(dialog.topicCount(), 0);
    TW_CHECK(dialog.page()->toPlainText().contains(
        QStringLiteral("Documentation is not installed")));
    if (inherited.isEmpty()) {
        qunsetenv("TIME_WARP_DOCS");
    } else {
        qputenv("TIME_WARP_DOCS", inherited);
    }
}

}  // namespace

int main(int argc, char** argv) {
    // Set before QApplication exists, or Qt tries to open a real display.
    qputenv("QT_QPA_PLATFORM", "offscreen");
    QApplication app{argc, argv};

    // The guides are read through the resolver's walk-up, which finds the
    // checkout from `native/build` only. From any other build directory the
    // path CMake recorded is supplied through the resolver's own override, so
    // the dead-link check still runs against a real docs tree.
    if (timewarp::ui::docsRoot().isEmpty()) {
        qputenv("TIME_WARP_DOCS", QStringLiteral(TIME_WARP_TEST_DOCS).toUtf8());
    }

    test_docs_root_follows_the_override();
    test_contents_come_from_the_index();
    test_a_broken_link_survives_parsing();
    test_the_dialog_lists_and_opens_pages();
    test_guide_topics_follow_the_languages();
    test_every_highlighted_keyword_has_a_row();
    test_the_inspector_filters();
    test_find_in_page_selects_a_whole_word();
    test_a_missing_root_is_explained();
    return tw::test::summary("help_browser");
}
