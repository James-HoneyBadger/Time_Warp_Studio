// SPDX-License-Identifier: MIT

#include "timewarp/ui/version_history.hpp"

#include "timewarp/ui/diff.hpp"

#include <QCheckBox>
#include <QDateTime>
#include <QHBoxLayout>
#include <QLabel>
#include <QListWidget>
#include <QLocale>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QStackedWidget>
#include <QTextBlock>
#include <QTextEdit>
#include <QVBoxLayout>

namespace timewarp::ui {

VersionHistoryDialog::VersionHistoryDialog(const AutosaveStore& store, QString document,
                                           QString current_text, QWidget* parent)
    : QDialog{parent},
      store_(store),
      document_(std::move(document)),
      current_text_(std::move(current_text)) {
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
    preview_->setAccessibleName(tr("Saved copy"));

    diff_ = new QPlainTextEdit{this};
    diff_->setObjectName(QStringLiteral("versionHistoryDiff"));
    diff_->setReadOnly(true);
    diff_->setLineWrapMode(QPlainTextEdit::NoWrap);
    diff_->setAccessibleName(tr("What changed"));
    diff_->setPlaceholderText(tr("Nothing differs."));

    panes_ = new QStackedWidget{this};
    panes_->setObjectName(QStringLiteral("versionHistoryPanes"));
    panes_->addWidget(preview_);
    panes_->addWidget(diff_);

    compare_ = new QCheckBox{tr("Compare with what is open"), this};
    compare_->setObjectName(QStringLiteral("versionHistoryCompare"));
    compare_->setToolTip(tr("Show this copy and the editor's text as one diff, "
                            "instead of the copy on its own"));
    // Offered only when there is something to compare with: a tick that always
    // produced an empty diff would be a tick that teaches people not to press it.
    if (current_text_.isEmpty()) {
        compare_->setEnabled(false);
        compare_->setToolTip(tr("The document is empty, so there is nothing to compare with"));
    }

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
    body->addWidget(panes_, 2);

    auto* layout = new QVBoxLayout{this};
    layout->addWidget(heading);
    layout->addLayout(body, 1);
    layout->addLayout(buttons);

    connect(list_, &QListWidget::currentRowChanged, this, [this] { showSelected(); });
    connect(compare_, &QCheckBox::toggled, this, [this] {
        refreshComparison();
        showPage();
    });
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

QString VersionHistoryDialog::diffText() const {
    return diff_ != nullptr ? diff_->toPlainText() : QString{};
}

void VersionHistoryDialog::showPage() {
    if (panes_ == nullptr) {
        return;
    }
    panes_->setCurrentWidget(compare_ != nullptr && compare_->isChecked() ? diff_
                                                                          : preview_);
}

void VersionHistoryDialog::showSelected() {
    const int row = list_->currentRow();
    if (row < 0 || row >= snapshots_.size()) {
        preview_->clear();
        diff_->clear();
        restore_->setEnabled(false);
        return;
    }
    preview_->setPlainText(store_.read(snapshots_.at(row)));
    restore_->setEnabled(true);
    refreshComparison();
    showPage();
    if (snapshots_.size() > 1) {
        status_->setText(compare_->isChecked()
                             ? tr("%1 copies kept \u00b7 %2")
                                   .arg(snapshots_.size())
                                   .arg(diffSummary(compareDocuments(
                                       current_text_, preview_->toPlainText())))
                             : tr("%1 copies kept").arg(snapshots_.size()));
    }
}

void VersionHistoryDialog::refreshComparison() {
    if (diff_ == nullptr || compare_ == nullptr) {
        return;
    }
    if (!compare_->isChecked()) {
        // Empty rather than stale: a hidden diff nobody can see is still a document
        // somebody could read back, and "not comparing" should answer nothing.
        diff_->clear();
        return;
    }
    const int row = list_->currentRow();
    if (row < 0 || row >= snapshots_.size()) {
        diff_->clear();
        return;
    }
    const QString snapshot = store_.read(snapshots_.at(row));
    // From the editor's text *to* the snapshot, which is the direction of the
    // decision: "restore this" means these lines go and those lines come back.
    diff_->setPlainText(unifiedDiff(current_text_, snapshot));

    QList<QTextEdit::ExtraSelection> selections;
    const QColor added{0, 140, 70, 46};
    const QColor removed{190, 40, 40, 46};
    const QTextDocument* document = diff_->document();
    const QStringList lines = diff_->toPlainText().split(QLatin1Char('\n'));
    for (int index = 0; index < lines.size(); ++index) {
        const QString& line = lines.at(index);
        if (line.isEmpty() || (line.front() != QLatin1Char('+') &&
                               line.front() != QLatin1Char('-'))) {
            continue;
        }
        const QTextBlock block = document->findBlockByNumber(index);
        if (!block.isValid()) {
            continue;
        }
        QTextEdit::ExtraSelection selection;
        selection.cursor = QTextCursor{block};
        selection.cursor.select(QTextCursor::LineUnderCursor);
        selection.format.setBackground(line.front() == QLatin1Char('+') ? added
                                                                       : removed);
        selections.append(selection);
    }
    diff_->setExtraSelections(selections);
}

}  // namespace timewarp::ui
