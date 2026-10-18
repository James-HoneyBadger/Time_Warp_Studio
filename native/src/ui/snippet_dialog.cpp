// SPDX-License-Identifier: MIT

#include "timewarp/ui/snippet_dialog.hpp"

#include <QComboBox>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QVBoxLayout>

namespace timewarp::ui {
namespace {

/// The filter's entry for "show everything". `Language::Unknown` is the value the
/// rest of the build uses for "no language", so it doubles as the sentinel here
/// rather than adding a second one.
constexpr int kAllLanguages = -1;

}  // namespace

SnippetDialog::SnippetDialog(QWidget* parent) : QDialog{parent} {
    setObjectName(QStringLiteral("snippetDialog"));
    setWindowTitle(tr("Insert Snippet"));
    resize(720, 520);

    language_ = new QComboBox{this};
    language_->setObjectName(QStringLiteral("snippetLanguage"));
    language_->addItem(tr("All languages"), kAllLanguages);
    for (const Language language : allLanguages()) {
        language_->addItem(displayName(language), static_cast<int>(language));
    }

    filter_ = new QLineEdit{this};
    filter_->setObjectName(QStringLiteral("snippetFilter"));
    filter_->setPlaceholderText(tr("Filter by name\u2026"));
    filter_->setClearButtonEnabled(true);

    list_ = new QListWidget{this};
    list_->setObjectName(QStringLiteral("snippetList"));

    description_ = new QLabel{this};
    description_->setObjectName(QStringLiteral("snippetDescription"));
    description_->setWordWrap(true);

    preview_ = new QPlainTextEdit{this};
    preview_->setObjectName(QStringLiteral("snippetPreview"));
    preview_->setReadOnly(true);
    preview_->setLineWrapMode(QPlainTextEdit::NoWrap);

    auto* buttons = new QHBoxLayout;
    buttons->addStretch(1);
    auto* cancel = new QPushButton{tr("Cancel"), this};
    cancel->setObjectName(QStringLiteral("snippetCancel"));
    insert_ = new QPushButton{tr("Insert"), this};
    insert_->setObjectName(QStringLiteral("snippetInsert"));
    insert_->setDefault(true);
    insert_->setEnabled(false);
    buttons->addWidget(cancel);
    buttons->addWidget(insert_);

    auto* top = new QHBoxLayout;
    top->addWidget(language_);
    top->addWidget(filter_, 1);

    auto* columns = new QHBoxLayout;
    columns->addWidget(list_, 1);
    auto* right = new QVBoxLayout;
    right->addWidget(description_);
    right->addWidget(preview_, 1);
    columns->addLayout(right, 2);

    auto* layout = new QVBoxLayout{this};
    layout->addLayout(top);
    layout->addLayout(columns, 1);
    layout->addLayout(buttons);

    connect(language_, &QComboBox::currentIndexChanged, this, [this] { fillList(); });
    connect(filter_, &QLineEdit::textChanged, this, [this] { fillList(); });
    connect(list_, &QListWidget::currentRowChanged, this, [this] { showSelected(); });
    connect(list_, &QListWidget::itemActivated, this, [this] { accept(); });
    connect(insert_, &QPushButton::clicked, this, [this] { accept(); });
    connect(cancel, &QPushButton::clicked, this, [this] { reject(); });
}

void SnippetDialog::setSnippets(const QList<Snippet>& snippets) {
    snippets_ = snippets;
    fillList();
}

void SnippetDialog::setLanguage(Language language) {
    // A document whose language has no snippets shows everything rather than an
    // empty list: "there is nothing for Logo" is a worse answer than "here is
    // what there is".
    int index = language_->findData(static_cast<int>(language));
    if (index < 0) {
        index = 0;
    }
    language_->setCurrentIndex(index);
    fillList();
}

Snippet SnippetDialog::chosen() const {
    const int row = list_->currentRow();
    if (row < 0 || row >= shown_.size()) {
        return {};
    }
    return shown_.at(row);
}

void SnippetDialog::fillList() {
    const int wanted = language_->currentData().toInt();
    const QString needle = filter_->text().trimmed();
    shown_.clear();
    list_->clear();
    for (const Snippet& snippet : snippets_) {
        if (wanted != kAllLanguages && static_cast<int>(snippet.language) != wanted) {
            continue;
        }
        if (!needle.isEmpty() && !snippet.name.contains(needle, Qt::CaseInsensitive)) {
            continue;
        }
        shown_.append(snippet);
        list_->addItem(tr("%1  \u2014  %2").arg(snippet.name, displayName(snippet.language)));
    }
    if (!shown_.isEmpty()) {
        list_->setCurrentRow(0);
    }
    showSelected();
}

void SnippetDialog::showSelected() {
    const Snippet snippet = chosen();
    const bool any = !snippet.name.isEmpty();
    insert_->setEnabled(any);
    description_->setText(any ? snippet.description : QString{});
    preview_->setPlainText(any ? snippet.body : QString{});
}

}  // namespace timewarp::ui
