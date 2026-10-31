// SPDX-License-Identifier: MIT
//
// The one download this application offers: a homebrew CHIP-8 archive.
//
// The Studio still ships no third-party ROMs, and this does not change that. What it
// changes is who does the fetching, and when: a person presses a button, reads what
// is about to be downloaded and from where, and gets those files in their own data
// directory. The repository stays legally as it was.
//
// #### Why a catalogue rather than a search
//
// Most CHIP-8 software that circulates online is somebody's copyright with no
// licence attached, and several are unlicensed ports of games that still are. A
// browser that pointed at "a repo of ROMs" would be a browser that eventually
// downloaded one of those. So the list is written down, in
// `share/chip8_archive.json`, one entry per program with its author and its
// licence; the source repository dedicates its contents to the public domain, and
// the commit is pinned so the bytes cannot change under a running application.
//
// Adding a program is adding an entry. An entry without a licence that permits
// redistribution does not belong in the file.

#pragma once

#include <QList>
#include <QObject>
#include <QString>
#include <QStringList>
#include <QUrl>

#include <optional>

class QNetworkAccessManager;

namespace timewarp::ui {

/// One program the catalogue offers.
struct ArchiveProgram {
    /// The file name inside the catalogue's `roms/` directory.
    QString file;
    QString title;
    QStringList authors;
    QString description;
    QString platform;
    QString licence;
};

/// The catalogue: where the programs come from, and which ones.
struct ArchiveCatalogue {
    QString repository;
    /// The commit every URL is built from, so a catalogue is reproducible.
    QString commit;
    QString base_url;
    QString licence;
    QString licence_url;
    QString note;
    QList<ArchiveProgram> programs;

    /// The URL of one program: `base_url` + the file name.
    [[nodiscard]] QUrl urlFor(const ArchiveProgram& program) const;
};

/// Reads a catalogue from *path*. Empty and `error` set when it is missing or is
/// not the document described above.
[[nodiscard]] std::optional<ArchiveCatalogue> loadArchiveCatalogue(const QString& path,
                                                                   QString* error);
/// The catalogue bundled with this build.
[[nodiscard]] QString archiveCataloguePath();

/// Where fetched programs are written: `<data directory>/chip8/archive`. Created on
/// first use; empty when it cannot be created.
[[nodiscard]] QString archiveFolder();

/// Whether this build can fetch at all. False in a build with no Qt Network, where
/// the button is absent rather than broken.
[[nodiscard]] bool archiveFetchingAvailable();

/// Fetches a catalogue's programs into a folder, one request at a time.
///
/// Sequential rather than parallel: this is somebody else's server hosting
/// somebody else's hobby projects, and a dozen simultaneous connections to be a few
/// seconds faster is not a trade worth making. Each file is written to `.part` and
/// renamed, so a cancelled or failed download never leaves a half ROM in the
/// library.
class ArchiveFetcher final : public QObject {
    Q_OBJECT

public:
    explicit ArchiveFetcher(QObject* parent = nullptr);
    ~ArchiveFetcher() override;

    /// Begins fetching. Does nothing in progress already, or in a build with no
    /// network; `finished` is emitted either way so a caller does not hang.
    void start(const ArchiveCatalogue& catalogue, const QString& folder);

    /// Stops after the request in flight. The files already written stay: they are
    /// whole files, and a partial catalogue is still a library.
    void cancel();

    [[nodiscard]] bool running() const { return running_; }

signals:
    /// `done` files of `total`, the last one being `current`.
    void progressed(int done, int total, const QString& current);
    /// `written` files landed, `failed` did not, and `failures` says which and why.
    void finished(int written, int failures, const QStringList& failureMessages);
    /// Nothing could be started at all -- no network module, or no folder.
    void failed(const QString& message);

private:
    void requestNext();
    void store(const QByteArray& bytes);

    QNetworkAccessManager* network_ = nullptr;
    QList<ArchiveProgram> queue_;
    /// The catalogue's base URL, carried into each request so a file name can only
    /// ever name something inside it.
    QString base_url_;
    QString folder_;
    QString current_file_;
    int total_ = 0;
    int done_ = 0;
    int written_ = 0;
    QStringList failures_;
    bool running_ = false;
    bool cancelled_ = false;
};

}  // namespace timewarp::ui
