// SPDX-License-Identifier: MIT
//
// The snippet picker: filter by language and name, preview the body, insert it.
//
// Ported from `ui/snippet_dialog.py` (230 lines). The dialog is a *view* over the
// library in `snippets.hpp` and holds no catalogue of its own, so the resolver,
// the validation and the placeholder rules have one implementation and one test.

#pragma once

#include <QDialog>
#include <QList>

#include "timewarp/ui/language.hpp"
#include "timewarp/ui/snippets.hpp"

class QComboBox;
class QLabel;
class QLineEdit;
class QListWidget;
class QPlainTextEdit;
class QPushButton;

namespace timewarp::ui {

class SnippetDialog final : public QDialog {
    Q_OBJECT

public:
    explicit SnippetDialog(QWidget* parent = nullptr);

    /// The catalogue to show. The dialog does not load it, so a test can hand it
    /// three records instead of depending on the shipped file.
    void setSnippets(const QList<Snippet>& snippets);
    /// Selects the language filter. `Unknown` means "every language", which is
    /// what a document with no extension gets.
    void setLanguage(Language language);
    /// What the user chose. Only meaningful after `exec()` returned Accepted.
    [[nodiscard]] Snippet chosen() const;

private:
    void fillList();
    void showSelected();

    QList<Snippet> snippets_;
    QList<Snippet> shown_;
    QComboBox* language_ = nullptr;
    QLineEdit* filter_ = nullptr;
    QListWidget* list_ = nullptr;
    QPlainTextEdit* preview_ = nullptr;
    QLabel* description_ = nullptr;
    QPushButton* insert_ = nullptr;
};

}  // namespace timewarp::ui
