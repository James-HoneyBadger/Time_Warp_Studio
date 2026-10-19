// SPDX-License-Identifier: MIT

#include "timewarp/ui/chip8_library.hpp"

#include <QDialogButtonBox>
#include <QDir>
#include <QDirIterator>
#include <QFileDialog>
#include <QFileInfo>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QPushButton>
#include <QSettings>
#include <QVBoxLayout>

#include <algorithm>

namespace timewarp::ui {

namespace {

constexpr auto kFolderKey = "chip8/romFolder";

[[nodiscard]] bool isRomSuffix(const QString& suffix) {
    const QString lower = suffix.toLower();
    return lower == QLatin1String("ch8") || lower == QLatin1String("c8") ||
           lower == QLatin1String("rom") || lower == QLatin1String("bin");
}

}  // namespace

QList<Chip8Rom> chip8RomsIn(const QString& folder) {
    QList<Chip8Rom> roms;
    if (folder.isEmpty()) {
        return roms;
    }
    const QDir root{folder};
    if (!root.exists()) {
        return roms;
    }

    // Filtering by suffix rather than by name filters: a name filter's case
    // sensitivity follows the filesystem, so `.CH8` would appear on one machine
    // and vanish on another.
    QDirIterator iterator{folder, QDir::Files | QDir::NoDotAndDotDot,
                          QDirIterator::Subdirectories};
    while (iterator.hasNext()) {
        const QString path = iterator.next();
        if (!isRomSuffix(QFileInfo{path}.suffix())) {
            continue;
        }
        roms.append(Chip8Rom{path, root.relativeFilePath(path)});
    }
    std::sort(roms.begin(), roms.end(), [](const Chip8Rom& left, const Chip8Rom& right) {
        return left.relative.compare(right.relative, Qt::CaseInsensitive) < 0;
    });
    return roms;
}

QString storedChip8RomFolder() {
    return QSettings{}.value(QLatin1String(kFolderKey)).toString();
}

void storeChip8RomFolder(const QString& folder) {
    QSettings settings;
    if (folder.isEmpty()) {
        settings.remove(QLatin1String(kFolderKey));
    } else {
        settings.setValue(QLatin1String(kFolderKey), folder);
    }
}

Chip8LibraryDialog::Chip8LibraryDialog(const QString& folder, QWidget* parent)
    : QDialog(parent), folder_(folder) {
    setObjectName(QStringLiteral("chip8LibraryDialog"));
    setWindowTitle(tr("CHIP-8 ROM Library"));
    resize(560, 420);

    auto* layout = new QVBoxLayout{this};

    auto* folder_row = new QHBoxLayout;
    folder_label_ = new QLabel{this};
    folder_label_->setObjectName(QStringLiteral("chip8LibraryFolder"));
    folder_label_->setWordWrap(true);
    folder_row->addWidget(folder_label_, 1);
    auto* choose = new QPushButton{tr("Choose Folder..."), this};
    choose->setObjectName(QStringLiteral("chip8LibraryChoose"));
    connect(choose, &QPushButton::clicked, this, &Chip8LibraryDialog::chooseFolder);
    folder_row->addWidget(choose);
    layout->addLayout(folder_row);

    // Said in the dialog rather than only in the documentation: the moment a
    // teacher goes looking for ROMs is the moment the licensing question comes up.
    auto* note = new QLabel{
        tr("The studio ships no ROMs. Point this at a folder you have the right to use."),
        this};
    note->setObjectName(QStringLiteral("chip8LibraryNote"));
    note->setWordWrap(true);
    note->setEnabled(false);
    layout->addWidget(note);

    filter_ = new QLineEdit{this};
    filter_->setObjectName(QStringLiteral("chip8LibraryFilter"));
    filter_->setPlaceholderText(tr("Filter by name"));
    connect(filter_, &QLineEdit::textChanged, this, [this](const QString& text) {
        for (int row = 0; row < list_->count(); ++row) {
            QListWidgetItem* item = list_->item(row);
            item->setHidden(!item->text().contains(text, Qt::CaseInsensitive));
        }
    });
    layout->addWidget(filter_);

    list_ = new QListWidget{this};
    list_->setObjectName(QStringLiteral("chip8LibraryList"));
    list_->setAccessibleName(tr("CHIP-8 programs"));
    connect(list_, &QListWidget::itemActivated, this,
            [this](QListWidgetItem*) { acceptSelection(); });
    layout->addWidget(list_, 1);

    empty_label_ = new QLabel{
        tr("No programs in this folder yet. Choose the folder you downloaded them into."),
        this};
    empty_label_->setObjectName(QStringLiteral("chip8LibraryEmpty"));
    empty_label_->setWordWrap(true);
    empty_label_->setEnabled(false);
    layout->addWidget(empty_label_);

    auto* buttons = new QDialogButtonBox{QDialogButtonBox::Open | QDialogButtonBox::Cancel, this};
    buttons->setObjectName(QStringLiteral("chip8LibraryButtons"));
    open_ = buttons->button(QDialogButtonBox::Open);
    if (open_ != nullptr) {
        open_->setText(tr("Open"));
        connect(open_, &QPushButton::clicked, this, &Chip8LibraryDialog::acceptSelection);
    }
    connect(buttons, &QDialogButtonBox::rejected, this, &QDialog::reject);
    layout->addWidget(buttons);

    reload();
}

QString Chip8LibraryDialog::selectedPath() const {
    const QListWidgetItem* item = list_->currentItem();
    return item == nullptr ? QString{} : item->data(Qt::UserRole).toString();
}

int Chip8LibraryDialog::romCount() const { return list_->count(); }

void Chip8LibraryDialog::chooseFolder() {
    const QString chosen = QFileDialog::getExistingDirectory(
        this, tr("Choose the CHIP-8 program folder"),
        folder_.isEmpty() ? QDir::homePath() : folder_);
    if (chosen.isEmpty()) {
        return;
    }
    folder_ = chosen;
    reload();
}

void Chip8LibraryDialog::reload() {
    list_->clear();
    const QList<Chip8Rom> roms = chip8RomsIn(folder_);
    for (const Chip8Rom& rom : roms) {
        auto* item = new QListWidgetItem{rom.relative, list_};
        item->setData(Qt::UserRole, rom.path);
    }
    folder_label_->setText(folder_.isEmpty() ? tr("No folder chosen yet")
                                             : QDir::toNativeSeparators(folder_));
    const bool empty = roms.isEmpty();
    empty_label_->setVisible(empty);
    list_->setVisible(!empty);
    if (open_ != nullptr) {
        open_->setEnabled(!empty);
    }
    if (!roms.isEmpty()) {
        list_->setCurrentRow(0);
    }
}

void Chip8LibraryDialog::acceptSelection() {
    if (selectedPath().isEmpty()) {
        return;
    }
    accept();
}

}  // namespace timewarp::ui
