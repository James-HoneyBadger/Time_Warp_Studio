// SPDX-License-Identifier: MIT
//
// The documentation browser: the pages under `docs/`, with a contents tree
// built from `docs/INDEX.md` itself.
//
// Why the index and not a hand-written tree: a tree beside the index is a second
// list of the same documents, and the day one is added and the other is not, the
// browser either promises a page that is not there or hides one that is. Reading
// the index's own tables means the browser cannot get ahead of the documentation.
//
// Ported from `mixins/help_mixin.py`, minus the five hand-coded entry points the
// old implementation had. The resolver follows `snippetRoot()`: an environment
// override for a test, then the installed `share/doc/time-warp-studio`, then the
// checkout's `docs/`, so a build tree and a package show the same pages.

#pragma once

#include <QDialog>
#include <QList>
#include <QString>

#include "timewarp/ui/language.hpp"

class QLabel;
class QLineEdit;
class QTextBrowser;
class QTreeWidget;

namespace timewarp::ui {

/// One document the index points at.
struct HelpTopic {
    /// The link text, without the brackets.
    QString title;
    /// Absolute path, whether or not the file exists: the dead-link check in
    /// `help_browser_tests` needs to see the path an index row *promised*.
    QString path;
};

/// One heading of the index, and the documents under it.
struct HelpSection {
    QString title;
    QList<HelpTopic> topics;
};

/// Where the documentation lives.
///
/// `TIME_WARP_DOCS` when it is set, even if it turns out not to contain an
/// index: an explicit override that silently falls back would make "the docs are
/// missing" impossible to reproduce or to test. Otherwise the installed
/// `share/doc/time-warp-studio` beside the binary, then the checkout's `docs/`.
/// Empty when none of those has an `INDEX.md`.
[[nodiscard]] QString docsRoot();

/// The sections and topics of `index` (the text of `INDEX.md`), with paths
/// resolved against `root`.
///
/// Only rows that stay inside `root` are returned. The index also links to the
/// repository's README, `CONTRIBUTING.md` and the issue forms, which are not
/// installed with the documentation; a package would otherwise show dead links
/// the checkout does not have. Row order is preserved, because it is the order a
/// reader of the index expects.
[[nodiscard]] QList<HelpSection> helpContents(const QString& index, const QString& root);

/// The guide page for a language, relative to the docs root: `tutorials/logo.md`.
/// Empty for `Language::Unknown`.
[[nodiscard]] QString guideTopic(Language language);

/// One row of the keyword inspector.
struct KeywordRow {
    /// The word as the highlighter spells it, which may contain a space
    /// (`END FUNCTION`).
    QString keyword;
    /// The guide's line for the word, empty when the guide does not name it.
    QString description;
};

/// The inspector's rows: the highlighter's own vocabulary (`keywordsOf`), each
/// with the description the language guide gives it. There is no second keyword
/// list here on purpose -- a word the editor colours and the inspector omits
/// would be invisible in the one place a learner goes to ask about it.
[[nodiscard]] QList<KeywordRow> keywordRows(Language language);

/// The guide's one-line description of `keyword`, or empty when the guide does
/// not mention it. Read from `LANGUAGE_GUIDE.md` under the language's heading;
/// comment lines and table rows are preferred over prose, because that is where
/// the guide states a command's meaning.
[[nodiscard]] QString guideDescription(Language language, const QString& keyword);

/// A contents tree and a page, side by side.
class HelpBrowserDialog final : public QDialog {
    Q_OBJECT

public:
    explicit HelpBrowserDialog(QWidget* parent = nullptr);

    /// How many topics the tree holds. Zero when the documentation is missing.
    [[nodiscard]] int topicCount() const;
    /// The tree and the page, so a test can drive them without a modal dialog.
    [[nodiscard]] QTreeWidget* contents() const noexcept { return contents_; }
    [[nodiscard]] QTextBrowser* page() const noexcept { return page_; }

    /// Shows `path`. Returns false when it is not a readable file, having left
    /// the current page alone.
    bool openTopic(const QString& path);
    /// Shows the index, which is what the dialog opens on.
    bool showIndex();
    /// Selects the first whole-word occurrence of `text` and scrolls it into
    /// view. False when the page does not contain it, having left the page
    /// where it was. F1 uses this to land on the command under the cursor.
    bool findInPage(const QString& text);

private:
    void buildContents(const QString& root);
    void showFile(const QString& path);
    void selectTopic(const QString& path);

    QString root_;
    QTreeWidget* contents_ = nullptr;
    QTextBrowser* page_ = nullptr;
    QLabel* status_ = nullptr;
};

/// Every word the editor colours for one language, filterable, with the guide's
/// description beside it. Ctrl+F1.
class KeywordInspectorDialog final : public QDialog {
    Q_OBJECT

public:
    explicit KeywordInspectorDialog(Language language, QWidget* parent = nullptr);

    /// Rows in total, and rows the filter leaves visible.
    [[nodiscard]] int rowCount() const;
    [[nodiscard]] int visibleRowCount() const;
    /// The table, so a test can read the rows.
    [[nodiscard]] QTreeWidget* rows() const noexcept { return rows_; }
    /// Types into the filter, as a learner would.
    void setFilter(const QString& text);

private:
    void applyFilter();

    Language language_ = Language::Unknown;
    QLineEdit* filter_ = nullptr;
    QTreeWidget* rows_ = nullptr;
    QLabel* empty_ = nullptr;
};

}  // namespace timewarp::ui
