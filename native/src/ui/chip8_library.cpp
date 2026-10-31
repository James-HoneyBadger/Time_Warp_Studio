// SPDX-License-Identifier: MIT

#include "timewarp/ui/chip8_library.hpp"

#include "timewarp/lang/registry.hpp"

#include "timewarp/ui/chip8_archive.hpp"

#include <QDialogButtonBox>
#include <QDir>
#include <QDirIterator>
#include <QFileDialog>
#include <QFileInfo>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QMessageBox>
#include <QPushButton>
#include <QSettings>
#include <QVBoxLayout>

#include <algorithm>

namespace timewarp::ui {

namespace {

constexpr auto kFolderKey = "chip8/romFolder";

/// A ROM is a file the registry calls CHIP-8, or a raw `.rom`/`.bin` image. The two
/// extensions that name the language come from `timewarp/lang/registry.hpp`: they
/// used to be written out here, in `chip8_window.cpp`, in the Qt language table and
/// in the smoke gate, and four copies of an extension is four chances to disagree.
[[nodiscard]] bool isRomSuffix(const QString& suffix) {
    const std::string lower = suffix.toLower().toStdString();
    if (lower == "rom" || lower == "bin") {
        return true;
    }
    const timewarp::lang::LanguageRow* row =
        timewarp::lang::language_row_for_extension(lower);
    return row != nullptr && row->language == timewarp::lang::Language::Chip8;
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
        tr("The studio ships no ROMs. Point this at a folder you have the right to use, "
           "or fetch the public-domain homebrew archive below."),
        this};
    note->setObjectName(QStringLiteral("chip8LibraryNote"));
    note->setWordWrap(true);
    note->setEnabled(false);
    layout->addWidget(note);

    if (archiveFetchingAvailable()) {
        auto* archive_row = new QHBoxLayout;
        fetch_ = new QPushButton{tr("Fetch Homebrew Archive..."), this};
        fetch_->setObjectName(QStringLiteral("chip8LibraryFetch"));
        fetch_->setToolTip(
            tr("Downloads a dozen public-domain CHIP-8 programs into this "
               "application's own folder. Nothing is sent except the request for "
               "each file."));
        connect(fetch_, &QPushButton::clicked, this, &Chip8LibraryDialog::fetchArchive);
        archive_row->addWidget(fetch_);
        archive_status_ = new QLabel{this};
        archive_status_->setObjectName(QStringLiteral("chip8LibraryFetchStatus"));
        archive_status_->setWordWrap(true);
        archive_row->addWidget(archive_status_, 1);
        layout->addLayout(archive_row);
    }

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

void Chip8LibraryDialog::fetchArchive() {
    QString error;
    const QString path = archiveCataloguePath();
    const std::optional<ArchiveCatalogue> catalogue = loadArchiveCatalogue(path, &error);
    if (!catalogue) {
        setArchiveStatus(error);
        return;
    }
    // What is about to happen, in the dialog, before it happens: the repository, the
    // licence, how many files and where they go. A download a person did not ask
    // about in these terms is one they did not agree to.
    const QString question =
        tr("Download %1 program(s) from %2, under the %3 licence, into %4?\n\n"
           "This is the only place the application fetches anything, and it only "
           "does it when you ask.")
            .arg(catalogue->programs.size())
            .arg(catalogue->repository, catalogue->licence,
                 QDir::toNativeSeparators(archiveFolder()));
    if (QMessageBox::question(this, tr("Fetch the homebrew archive"), question,
                              QMessageBox::Yes | QMessageBox::No, QMessageBox::No) !=
        QMessageBox::Yes) {
        return;
    }

    auto* fetcher = new ArchiveFetcher{this};
    fetch_->setEnabled(false);
    setArchiveStatus(tr("Fetching 0 of %1...").arg(catalogue->programs.size()));
    connect(fetcher, &ArchiveFetcher::progressed, this, [this](int done, int total,
                                                               const QString& current) {
        setArchiveStatus(tr("Fetching %1 of %2: %3").arg(done).arg(total).arg(current));
    });
    connect(fetcher, &ArchiveFetcher::failed, this, [this, fetcher](const QString& message) {
        setArchiveStatus(message);
        if (fetch_ != nullptr) {
            fetch_->setEnabled(true);
        }
        fetcher->deleteLater();
    });
    connect(fetcher, &ArchiveFetcher::finished, this,
            [this, fetcher](int written, int failures, const QStringList& messages) {
                fetcher->deleteLater();
                if (fetch_ != nullptr) {
                    fetch_->setEnabled(true);
                }
                if (written == 0) {
                    setArchiveStatus(failures == 0 ? tr("Nothing to fetch.")
                                                   : messages.join(QStringLiteral("; ")));
                    return;
                }
                // The library becomes the archive: the files are there, and a person
                // who just fetched them wants to see them rather than to go looking.
                folder_ = archiveFolder();
                reload();
                if (failures == 0) {
                    setArchiveStatus(tr("\u2705 %1 program(s) fetched.").arg(written));
                } else {
                    setArchiveStatus(tr("\u2705 %1 fetched, %2 failed: %3")
                                         .arg(written)
                                         .arg(failures)
                                         .arg(messages.join(QStringLiteral("; "))));
                }
            });
    fetcher->start(*catalogue, archiveFolder());
}

void Chip8LibraryDialog::setArchiveStatus(const QString& text) {
    if (archive_status_ != nullptr) {
        archive_status_->setText(text);
    }
}

void Chip8LibraryDialog::acceptSelection() {
    if (selectedPath().isEmpty()) {
        return;
    }
    accept();
}

}  // namespace timewarp::ui
