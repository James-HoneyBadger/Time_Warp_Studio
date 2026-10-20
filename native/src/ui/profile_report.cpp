// SPDX-License-Identifier: MIT

#include "timewarp/ui/profile_report.hpp"

#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QPushButton>
#include <QTableWidget>
#include <QTableWidgetItem>
#include <QVBoxLayout>

namespace timewarp::ui {

namespace {

[[nodiscard]] QTableWidgetItem* cell(const QString& text, Qt::Alignment alignment) {
    auto* item = new QTableWidgetItem{text};
    item->setFlags(item->flags() & ~Qt::ItemIsEditable);
    item->setTextAlignment(alignment | Qt::AlignVCenter);
    return item;
}

}  // namespace

ProfileReportDialog::ProfileReportDialog(QString document, QList<ProfileEntry> entries,
                                         double total_ms, QWidget* parent)
    : QDialog{parent}, entries_(std::move(entries)) {
    setObjectName(QStringLiteral("profileReportDialog"));
    setWindowTitle(tr("Profile of %1")
                       .arg(document.isEmpty() ? tr("this program") : document));
    resize(820, 520);

    auto* heading = new QLabel{this};
    heading->setObjectName(QStringLiteral("profileReportHeading"));
    heading->setWordWrap(true);
    // Every line that ran is in the list, not only the slowest: a learner reading
    // "why did only three of my ten lines run" needs the whole picture.
    heading->setText(tr("%1 lines, %2 in total. Sorted by time, longest first; the time is "
                        "wall clock around the statement, so a prompt counts against the "
                        "line that asked.")
                         .arg(entries_.size())
                         .arg(tr("%1 ms").arg(QString::number(total_ms, 'f', 1))));

    table_ = new QTableWidget{static_cast<int>(entries_.size()), 5, this};
    table_->setObjectName(QStringLiteral("profileReportTable"));
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    table_->setSelectionBehavior(QAbstractItemView::SelectRows);
    table_->setWordWrap(false);
    table_->setHorizontalHeaderLabels(
        {tr("Line"), tr("Hits"), tr("Time (ms)"), tr("Share"), tr("Source")});
    table_->verticalHeader()->setVisible(false);
    table_->horizontalHeader()->setStretchLastSection(true);

    for (int row = 0; row < entries_.size(); ++row) {
        const ProfileEntry& entry = entries_.at(row);
        table_->setItem(row, 0, cell(QString::number(entry.line), Qt::AlignRight));
        table_->setItem(row, 1, cell(QString::number(entry.hits), Qt::AlignRight));
        table_->setItem(row, 2, cell(QString::number(entry.elapsed_ms, 'f', 1), Qt::AlignRight));
        const double share = total_ms > 0.0 ? entry.elapsed_ms / total_ms * 100.0 : 0.0;
        table_->setItem(row, 3, cell(tr("%1 %").arg(QString::number(share, 'f', 1)),
                                     Qt::AlignRight));
        table_->setItem(row, 4, cell(entry.source.simplified(), Qt::AlignLeft));
    }
    table_->resizeColumnsToContents();
    table_->horizontalHeader()->setStretchLastSection(true);

    auto* close = new QPushButton{tr("Close"), this};
    close->setObjectName(QStringLiteral("profileReportClose"));
    close->setDefault(true);
    connect(close, &QPushButton::clicked, this, &QDialog::accept);
    auto* buttons = new QHBoxLayout;
    buttons->addStretch(1);
    buttons->addWidget(close);

    auto* layout = new QVBoxLayout{this};
    layout->addWidget(heading);
    layout->addWidget(table_, 1);
    layout->addLayout(buttons);
}

int ProfileReportDialog::rowCount() const {
    return static_cast<int>(entries_.size());
}

int ProfileReportDialog::lineAt(int row) const {
    if (row < 0 || row >= entries_.size()) {
        return 0;
    }
    return entries_.at(row).line;
}

std::uint64_t ProfileReportDialog::hitsAt(int row) const {
    if (row < 0 || row >= entries_.size()) {
        return 0;
    }
    return entries_.at(row).hits;
}

}  // namespace timewarp::ui
