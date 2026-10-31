// SPDX-License-Identifier: MIT

#include "timewarp/ui/chip8_archive.hpp"

#include "timewarp/core/share_files.hpp"
#include "timewarp/ui/app_paths.hpp"

#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>

#if defined(TIME_WARP_HAVE_NETWORK)
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QNetworkRequest>
#endif

// Supplied by CMake from the repository's VERSION file, so the agent string cannot
// go stale in a header that nothing checks. The fallback keeps a standalone compile
// of this file working.
#ifndef TIME_WARP_VERSION
#define TIME_WARP_VERSION "0.0.0"
#endif

namespace timewarp::ui {

namespace {

/// The largest a CHIP-8 program can be. 4 KB of memory, 0x200 of it reserved, and a
/// few hundred bytes of headroom for a machine with the extra font. A file larger
/// than this is not a ROM and is not written.
constexpr qint64 kMaxProgramBytes = 8192;

[[nodiscard]] QString text(const QJsonObject& object, const char* key) {
    return object.value(QLatin1String(key)).toString().trimmed();
}

}  // namespace

QUrl ArchiveCatalogue::urlFor(const ArchiveProgram& program) const {
    return QUrl{base_url + program.file};
}

std::optional<ArchiveCatalogue> loadArchiveCatalogue(const QString& path, QString* error) {
    QFile file{path};
    if (path.isEmpty() || !file.open(QIODevice::ReadOnly)) {
        if (error != nullptr) {
            *error = QStringLiteral("No homebrew catalogue was found.");
        }
        return std::nullopt;
    }
    QJsonParseError parse_error{};
    const QJsonDocument document = QJsonDocument::fromJson(file.readAll(), &parse_error);
    if (parse_error.error != QJsonParseError::NoError || !document.isObject()) {
        if (error != nullptr) {
            *error = QStringLiteral("%1 is not a catalogue: %2")
                         .arg(QFileInfo{path}.fileName(), parse_error.errorString());
        }
        return std::nullopt;
    }
    const QJsonObject root = document.object();
    const QJsonObject source = root.value(QLatin1String("source")).toObject();

    ArchiveCatalogue catalogue;
    catalogue.repository = text(source, "repository");
    catalogue.commit = text(source, "commit");
    catalogue.base_url = text(source, "base_url");
    catalogue.licence = text(source, "licence");
    catalogue.licence_url = text(source, "licence_url");
    catalogue.note = text(source, "note");
    if (catalogue.base_url.isEmpty() || !catalogue.base_url.endsWith(QLatin1Char('/'))) {
        if (error != nullptr) {
            *error = QStringLiteral("%1 has no usable source").arg(QFileInfo{path}.fileName());
        }
        return std::nullopt;
    }

    for (const QJsonValue& value : root.value(QLatin1String("programs")).toArray()) {
        const QJsonObject entry = value.toObject();
        ArchiveProgram program;
        program.file = text(entry, "file");
        program.title = text(entry, "title");
        program.platform = text(entry, "platform");
        program.licence = text(entry, "licence");
        program.description = text(entry, "description");
        for (const QJsonValue& author : entry.value(QLatin1String("authors")).toArray()) {
            program.authors.append(author.toString());
        }
        // An entry without a file or without a licence is a catalogue mistake, and
        // the one thing this file exists to prevent is downloading something whose
        // terms nobody wrote down.
        if (program.file.isEmpty() || program.title.isEmpty() || program.licence.isEmpty()) {
            continue;
        }
        if (program.file.contains(QLatin1Char('/')) ||
            program.file.contains(QLatin1String(".."))) {
            continue;
        }
        catalogue.programs.append(program);
    }
    if (catalogue.programs.isEmpty()) {
        if (error != nullptr) {
            *error = QStringLiteral("%1 lists no programs").arg(QFileInfo{path}.fileName());
        }
        return std::nullopt;
    }
    return catalogue;
}

QString archiveCataloguePath() {
    return timewarp::core::shareFile("TIME_WARP_CHIP8_ARCHIVE",
                                     QStringLiteral("chip8_archive.json"));
}

QString archiveFolder() {
    return dataDirFor(QStringLiteral("chip8/archive"));
}

bool archiveFetchingAvailable() {
#if defined(TIME_WARP_HAVE_NETWORK)
    return true;
#else
    return false;
#endif
}

ArchiveFetcher::ArchiveFetcher(QObject* parent) : QObject(parent) {
#if defined(TIME_WARP_HAVE_NETWORK)
    network_ = new QNetworkAccessManager{this};
#endif
}

ArchiveFetcher::~ArchiveFetcher() = default;

void ArchiveFetcher::start(const ArchiveCatalogue& catalogue, const QString& folder) {
    if (running_) {
        return;
    }
    if (network_ == nullptr) {
        emit failed(tr("This build cannot reach the network, so it cannot fetch the "
                       "archive. Every program the application itself ships is in "
                       "Examples/chip8."));
        return;
    }
    const QString target = folder.isEmpty() ? archiveFolder() : folder;
    if (target.isEmpty() || !QDir{}.mkpath(target)) {
        emit failed(tr("There is nowhere to put the programs: %1 could not be created.")
                        .arg(target));
        return;
    }
    queue_ = catalogue.programs;
    base_url_ = catalogue.base_url;
    folder_ = target;
    total_ = static_cast<int>(queue_.size());
    done_ = 0;
    written_ = 0;
    failures_.clear();
    cancelled_ = false;
    running_ = true;
    requestNext();
}

void ArchiveFetcher::cancel() {
    cancelled_ = true;
#if defined(TIME_WARP_HAVE_NETWORK)
    if (!running_) {
        return;
    }
    // The request in flight is aborted, and `requestNext` sees `cancelled_` before it
    // starts the next one.
    for (QNetworkReply* reply : findChildren<QNetworkReply*>()) {
        reply->abort();
    }
#else
    running_ = false;
#endif
}

void ArchiveFetcher::requestNext() {
#if defined(TIME_WARP_HAVE_NETWORK)
    if (queue_.isEmpty() || cancelled_) {
        running_ = false;
        emit finished(written_, static_cast<int>(failures_.size()), failures_);
        return;
    }
    const ArchiveProgram program = queue_.takeFirst();
    current_file_ = program.file;

    // The catalogue's own base URL is the only source, and a program's file name
    // cannot reach outside it: the loader has already refused a name with a slash
    // or `..` in it.
    QNetworkRequest request{QUrl{base_url_ + program.file}};
    request.setAttribute(QNetworkRequest::RedirectPolicyAttribute,
                         QNetworkRequest::NoLessSafeRedirectPolicy);
    request.setHeader(QNetworkRequest::UserAgentHeader,
                      QStringLiteral("TimeWarpStudio/%1 (+CHIP-8 archive)")
                          .arg(QLatin1String{TIME_WARP_VERSION}));
    request.setTransferTimeout(30000);
    if (request.url().isEmpty()) {
        failures_.append(tr("%1: no URL").arg(program.file));
        ++done_;
        emit progressed(done_, total_, program.file);
        requestNext();
        return;
    }
    QNetworkReply* reply = network_->get(request);
    // A hung server must not hang the dialog: this is a footer-sized download. The
    // timeout is on the request because that is where Qt 5.15 and 6 both have it.
    //
    // The size is stopped as the bytes arrive rather than checked afterwards. A cap
    // applied to `readAll()` is a cap on what was already in memory, and this one
    // exists so that a catalogue pointing somewhere wrong costs a refused file
    // rather than an allocation.
    connect(reply, &QNetworkReply::downloadProgress, this,
            [reply](qint64 received, qint64 total) {
                if (received > kMaxProgramBytes || total > kMaxProgramBytes) {
                    reply->setProperty("tooLarge", true);
                    reply->abort();
                }
            });

    connect(reply, &QNetworkReply::finished, this, [this, reply, program] {
        const bool too_large = reply->property("tooLarge").toBool();
        reply->deleteLater();
        ++done_;
        if (too_large) {
            // Named as what it is: an abort is otherwise reported as "canceled",
            // which reads like something the user did.
            failures_.append(tr("%1: larger than %2 bytes, so it is not a CHIP-8 program")
                                 .arg(program.file)
                                 .arg(kMaxProgramBytes));
        } else if (reply->error() != QNetworkReply::NoError) {
            failures_.append(tr("%1: %2").arg(program.file, reply->errorString()));
        } else {
            const QByteArray bytes = reply->readAll();
            if (bytes.isEmpty()) {
                failures_.append(tr("%1: the server sent nothing").arg(program.file));
            } else if (bytes.size() > kMaxProgramBytes) {
                failures_.append(tr("%1: %2 bytes is not a CHIP-8 program")
                                     .arg(program.file)
                                     .arg(bytes.size()));
            } else {
                const QString path = QDir{folder_}.filePath(program.file);
                QFile file{path + QStringLiteral(".part")};
                if (!file.open(QIODevice::WriteOnly)) {
                    failures_.append(tr("%1: %2").arg(program.file, file.errorString()));
                } else {
                    const qint64 written_now = file.write(bytes);
                    const QString write_error = file.errorString();
                    file.close();
                    if (written_now != bytes.size()) {
                        // A short write is a full disk, and renaming it would put a
                        // truncated program in the library as though it were whole.
                        failures_.append(tr("%1: only %2 of %3 bytes could be written%4")
                                             .arg(program.file)
                                             .arg(written_now)
                                             .arg(bytes.size())
                                             .arg(write_error.isEmpty()
                                                      ? QString{}
                                                      : QStringLiteral(" (%1)")
                                                            .arg(write_error)));
                        QFile::remove(file.fileName());
                    } else {
                        // Renamed only once it is whole: a library that lists a file
                        // it cannot run is worse than one that lists nothing.
                        QFile::remove(path);
                        if (file.rename(path)) {
                            ++written_;
                        } else {
                            failures_.append(
                                tr("%1: could not be written").arg(program.file));
                        }
                    }
                }
            }
        }
        emit progressed(done_, total_, program.file);
        requestNext();
    });
#endif
}

}  // namespace timewarp::ui
