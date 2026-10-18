// SPDX-License-Identifier: MIT

#include "timewarp/ui/version_history.hpp"

#include <QDateTime>
#include <QHBoxLayout>
#include <QLabel>
#include <QListWidget>
#include <QLocale>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QVBoxLayout>

namespace timewarp::ui {

VersionHistoryDialog::VersionHistoryDialog(const AutosaveStore& store, QString document,
                                           QWidget* parent)
    : QDialog{parent}, store_(store), document_(std::move(document)) {
    setObjectName(QStringLiteral("versionHistoryDialog"));
    setWindowTitle(tr("Version History"));
    resize(720, 520);
    snapshots_ = store_.versions(document_);

    auto* heading = new QLabel{this};
    heading->setObjectName(QStringLiteral("versionHistoryHeading"));
    heading->setText(tr("Saved copies of %1, newest first. Restoring opens a new tab; "
                        "the file is not touched.")
                         .arg(document_.isEmpty() ? tr("this document") : document_));
    heading->setWordWrap(true);

    list_ = new QListWidget{this};
    list_->setObjectName(QStringLiteral("versionHistoryList"));
    for (const Snapshot& snapshot : snapshots_) {
        const QDateTime when =
            QDateTime::fromSecsSinceEpoch(snapshot.timestamp / 1000).toLocalTime();
        list_->addItem(tr("%1  \u00b7  %2")
                           .arg(when.toString(QStringLiteral("yyyy-MM-dd HH:mm:ss")))
                           .arg(QLocale{}.formattedDataSize(snapshot.bytes)));
    }

    preview_ = new QPlainTextEdit{this};
    preview_->setObjectName(QStringLiteral("versionHistoryPreview"));
    preview_->setReadOnly(true);
    preview_->setLineWrapMode(QPlainTextEdit::NoWrap);

    status_ = new QLabel{this};
    status_->setObjectName(QStringLiteral("versionHistoryStatus"));

    restore_ = new QPushButton{tr("Open in a New Tab"), this};
    restore_->setObjectName(QStringLiteral("versionHistoryRestore"));
    restore_->setDefault(true);
    restore_->setEnabled(!snapshots_.isEmpty());
    auto* close = new QPushButton{tr("Close"), this};
    close->setObjectName(QStringLiteral("versionHistoryClose"));

    auto* buttons = new QHBoxLayout;
    buttons->addWidget(status_, 1);
    buttons->addWidget(close);
    buttons->addWidget(restore_);

    auto* body = new QHBoxLayout;
    body->addWidget(list_, 1);
    body->addWidget(preview_, 2);

    auto* layout = new QVBoxLayout{this};
    layout->addWidget(heading);
    layout->addLayout(body, 1);
    layout->addLayout(buttons);

    connect(list_, &QListWidget::currentRowChanged, this, [this] { showSelected(); });
    // Choosing is two things: hold the text, then close with Accepted. The text
    // is taken here rather than read from the widget afterwards, so the caller
    // gets what was on screen when the button was pressed.
    const auto choose = [this] {
        const int row = list_->currentRow();
        if (row >= 0 && row < snapshots_.size()) {
            chosen_text_ = store_.read(snapshots_.at(row));
        }
        if (!chosen_text_.isEmpty()) {
            accept();
        }
    };
    connect(list_, &QListWidget::itemActivated, this,
            [choose](QListWidgetItem*) { choose(); });
    connect(restore_, &QPushButton::clicked, this, choose);
    connect(close, &QPushButton::clicked, this, [this] { reject(); });

    if (snapshots_.isEmpty()) {
        status_->setText(tr("No saved copies yet. Autosave keeps them while a document "
                            "has unsaved changes."));
    } else {
        list_->setCurrentRow(0);
    }
    showSelected();
}

int VersionHistoryDialog::versionCount() const { return int(snapshots_.size()); }

void VersionHistoryDialog::showSelected() {
    const int row = list_->currentRow();
    if (row < 0 || row >= snapshots_.size()) {
        preview_->clear();
        restore_->setEnabled(false);
        return;
    }
    preview_->setPlainText(store_.read(snapshots_.at(row)));
    restore_->setEnabled(true);
    if (snapshots_.size() > 1) {
        status_->setText(tr("%1 copies kept").arg(snapshots_.size()));
    }
}

}  // namespace timewarp::ui
