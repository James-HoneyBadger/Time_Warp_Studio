// SPDX-License-Identifier: MIT

#include "timewarp/ui/standalone.hpp"

#include <QFile>
#include <QFileInfo>
#include <QJsonDocument>
#include <QJsonObject>
#include <QSaveFile>
#include <QtEndian>

#include "timewarp/ui/language.hpp"

namespace timewarp::ui::standalone {

namespace {

/// Where the language key length sits, counted back from the end. The version
/// byte comes after the magic, so the offsets of everything before it are the
/// same in both formats.
constexpr int kLanguageLengthFromEnd = kMagicLength + 1;
/// And the eight-byte source length, before that.
constexpr int kSourceLengthFromEnd = kLanguageLengthFromEnd + 8;

void setError(QString* error, const QString& text) {
    if (error != nullptr) {
        *error = text;
    }
}

}  // namespace

QByteArray metadataJson(const Metadata& metadata) {
    QJsonObject object;
    // Omitted rather than empty: an artifact with nothing to say carries `{}`,
    // and a reader cannot then mistake "the author left it blank" for a title
    // that happens to be "".
    if (!metadata.title.isEmpty()) {
        object.insert(QStringLiteral("title"), metadata.title);
    }
    if (!metadata.author.isEmpty()) {
        object.insert(QStringLiteral("author"), metadata.author);
    }
    if (metadata.screen_width > 0 && metadata.screen_height > 0) {
        object.insert(QStringLiteral("screen"),
                      QStringLiteral("%1x%2")
                          .arg(metadata.screen_width)
                          .arg(metadata.screen_height));
    }
    if (!metadata.background.isEmpty()) {
        object.insert(QStringLiteral("background"), metadata.background);
    }
    if (metadata.crt > 0) {
        object.insert(QStringLiteral("crt"), metadata.crt);
    }
    return QJsonDocument{object}.toJson(QJsonDocument::Compact);
}

bool metadataFromJson(const QByteArray& json, Metadata* out) {
    if (json.isEmpty()) {
        return true;
    }
    QJsonParseError error{};
    const QJsonDocument document = QJsonDocument::fromJson(json, &error);
    if (error.error != QJsonParseError::NoError || !document.isObject()) {
        return false;
    }
    const QJsonObject object = document.object();
    if (out == nullptr) {
        return true;
    }
    // A field this build does not know is ignored rather than refused: that is
    // what makes the format extensible without a third version.
    out->title = object.value(QStringLiteral("title")).toString();
    out->author = object.value(QStringLiteral("author")).toString();
    out->background = object.value(QStringLiteral("background")).toString();
    out->crt = object.value(QStringLiteral("crt")).toInt();
    const QString screen = object.value(QStringLiteral("screen")).toString();
    const qsizetype cross = screen.indexOf(QLatin1Char('x'));
    if (cross > 0) {
        out->screen_width = screen.left(cross).toInt();
        out->screen_height = screen.mid(cross + 1).toInt();
    }
    return true;
}

QByteArray encode(const QString& language, const QString& source) {
    return encode(language, source, Metadata{});
}

QByteArray encode(const QString& language, const QString& source,
                  const Metadata& metadata) {
    const QByteArray key = language.toUtf8();
    const QByteArray body = source.toUtf8();
    const QByteArray extra = metadataJson(metadata);

    QByteArray payload;
    payload.reserve(body.size() + extra.size() + kSourceLengthFromEnd + key.size() + 4);
    payload.append(body);
    payload.append(extra);

    // The three lengths and the key, in the order a reader walking backwards
    // meets them: metadata, source, key, key length, magic, version.
    QByteArray extra_length(4, Qt::Uninitialized);
    qToLittleEndian<quint32>(static_cast<quint32>(extra.size()), extra_length.data());
    payload.append(extra_length);

    QByteArray length(8, Qt::Uninitialized);
    qToLittleEndian<quint64>(static_cast<quint64>(body.size()), length.data());
    payload.append(length);

    payload.append(key);
    payload.append(static_cast<char>(key.size()));
    payload.append(kMagic, kMagicLength);
    // After the magic, so a format-1 artifact -- which stops at the magic -- reads
    // as version 1 by having nothing there.
    payload.append(static_cast<char>(kFormat));
    return payload;
}

bool decode(const QByteArray& bytes, Embedded* out, QString* error) {
    if (bytes.size() < kSourceLengthFromEnd) {
        setError(error, QStringLiteral("the file is too short to be a program"));
        return false;
    }
    if (bytes.right(kMagicLength) != QByteArray{kMagic, kMagicLength} &&
        bytes.mid(bytes.size() - kMagicLength - 1, kMagicLength) !=
            QByteArray{kMagic, kMagicLength}) {
        setError(error, QStringLiteral("this executable does not carry a program"));
        return false;
    }

    // The magic is either the last 16 bytes (format 1) or one byte from the end,
    // because format 2 appends its version after it.
    const bool versioned =
        bytes.right(kMagicLength) != QByteArray{kMagic, kMagicLength};
    const int format =
        versioned ? static_cast<unsigned char>(bytes.back()) : 1;
    const int end = versioned ? bytes.size() - 1 : bytes.size();
    if (format != 1 && format != kFormat) {
        setError(error, QStringLiteral("this program was written by a newer version "
                                       "(format %1)").arg(format));
        return false;
    }

    const int size = end;
    const int key_length = static_cast<unsigned char>(bytes.at(size - kLanguageLengthFromEnd));
    const int trailer = kSourceLengthFromEnd + key_length;
    if (key_length == 0 || size < trailer) {
        setError(error, QStringLiteral("the program's language key is missing"));
        return false;
    }

    const int key_offset = size - kLanguageLengthFromEnd - key_length;
    const int source_length = static_cast<int>(
        qFromLittleEndian<quint64>(bytes.constData() + size - trailer));
    // A format-2 payload has a metadata block between the source and the source
    // length. Its own length is what says how much, so the source's offset is
    // computed from the two rather than guessed.
    int metadata_length = 0;
    if (format >= 2) {
        // The four-byte metadata length sits immediately before the eight-byte
        // source length, so it is `trailer + 4` from the end -- the key's length
        // is part of `trailer`, and leaving it out reads four bytes of the wrong
        // field.
        if (size < trailer + 4) {
            setError(error, QStringLiteral("the program is truncated"));
            return false;
        }
        metadata_length = static_cast<int>(
            qFromLittleEndian<quint32>(bytes.constData() + size - trailer - 4));
        if (metadata_length < 0) {
            setError(error, QStringLiteral("the program's metadata length is invalid"));
            return false;
        }
    }
    // Walking backwards from the source length: the four-byte metadata length,
    // then the metadata, then the source. Leaving the length field out of this
    // sum is a four-byte lie about where the program starts.
    const int metadata_field = format >= 2 ? 4 : 0;
    const int source_offset =
        size - trailer - metadata_field - metadata_length - source_length;
    if (source_length < 0 || source_offset < 0) {
        setError(error, QStringLiteral("the program is truncated"));
        return false;
    }

    if (out != nullptr) {
        out->present = true;
        out->format = format;
        out->language = QString::fromUtf8(bytes.constData() + key_offset, key_length);
        out->source = QString::fromUtf8(bytes.constData() + source_offset, source_length);
        if (metadata_length > 0) {
            const QByteArray extra{bytes.constData() + size - trailer - metadata_field -
                                       metadata_length,
                                   metadata_length};
            if (!metadataFromJson(extra, &out->metadata)) {
                setError(error, QStringLiteral("the program's metadata is not valid "
                                               "JSON"));
                return false;
            }
        }
    }
    return true;
}

Embedded embeddedIn(const QString& executablePath) {
    Embedded embedded;
    QFile file{executablePath};
    if (!file.open(QIODevice::ReadOnly)) {
        return embedded;
    }
    // The whole file as a QByteArray is a few megabytes for the IDE and a few
    // tens for a program: small enough that seeking around it is not worth the
    // complexity, and the decoder needs random access anyway.
    const QByteArray bytes = file.readAll();
    file.close();
    // A failure here is the *ordinary* case -- this is the IDE binary -- so it is
    // deliberately not reported. Only a payload that is present but damaged
    // matters, and that shows up as `present == false` with a reason the caller
    // can choose to ignore.
    QString ignored;
    if (!decode(bytes, &embedded, &ignored)) {
        return Embedded{};
    }
    return embedded;
}

bool compileFile(const QString& hostExecutable, const QString& sourcePath,
                 const QString& outputPath, bool overwrite, QString* error) {
    return compileFile(hostExecutable, sourcePath, outputPath, overwrite, Metadata{},
                       error);
}

bool compileFile(const QString& hostExecutable, const QString& sourcePath,
                 const QString& outputPath, bool overwrite, const Metadata& metadata,
                 QString* error) {
    const QFileInfo source_info{sourcePath};
    if (!source_info.isFile() || !source_info.isReadable()) {
        setError(error, QStringLiteral("%1 cannot be read").arg(sourcePath));
        return false;
    }
    const Language language = languageForFile(sourcePath);
    if (language == Language::Unknown) {
        setError(error, QStringLiteral("%1 is not a program this IDE recognises")
                            .arg(source_info.fileName()));
        return false;
    }

    const QFileInfo host_info{hostExecutable};
    if (!host_info.isFile() || !host_info.isReadable()) {
        setError(error, QStringLiteral("the IDE's own executable (%1) cannot be read")
                            .arg(hostExecutable));
        return false;
    }

    const QFileInfo output_info{outputPath};
    if (output_info.absoluteFilePath() == host_info.absoluteFilePath()) {
        // Appending a program to the running IDE would produce a binary that
        // runs that program forever, with no way to open another one. Said
        // rather than done.
        setError(error, QStringLiteral("refusing to overwrite the IDE itself"));
        return false;
    }
    if (output_info.exists() && !overwrite) {
        setError(error, QStringLiteral("%1 already exists").arg(outputPath));
        return false;
    }

    QFile source{sourcePath};
    if (!source.open(QIODevice::ReadOnly)) {
        setError(error, QStringLiteral("%1 cannot be read: %2")
                            .arg(sourcePath, source.errorString()));
        return false;
    }
    const QString text = QString::fromUtf8(source.readAll());
    source.close();

    // The title defaults to the artifact's own name when the author gave none:
    // "the file I ran" is a better title than an empty one, and it is what the
    // window would have shown anyway.
    Metadata filled = metadata;
    if (filled.title.isEmpty()) {
        filled.title = artifactName(outputPath);
    }
    const QByteArray payload = encode(settingsKey(language), text, filled);

    QFile host{hostExecutable};
    if (!host.open(QIODevice::ReadOnly)) {
        setError(error, QStringLiteral("the IDE's own executable cannot be read: %1")
                            .arg(host.errorString()));
        return false;
    }

    // `QSaveFile`: the output is written beside its destination and renamed into
    // place, so an interrupted compile leaves the previous artifact intact rather
    // than a half-written executable that fails at load time.
    QSaveFile out{outputPath};
    if (!out.open(QIODevice::WriteOnly)) {
        setError(error, QStringLiteral("%1 cannot be written: %2")
                            .arg(outputPath, out.errorString()));
        return false;
    }
    // Copying in chunks so a 40MB IDE does not need a second 40MB buffer.
    constexpr qint64 kChunk = 1 << 20;
    qint64 remaining = host.size();
    while (remaining > 0) {
        const QByteArray chunk = host.read(qMin(kChunk, remaining));
        if (chunk.isEmpty()) {
            setError(error, QStringLiteral("the IDE's own executable changed while "
                                           "being copied"));
            out.cancelWriting();
            return false;
        }
        if (out.write(chunk) != chunk.size()) {
            setError(error, QStringLiteral("%1 cannot be written: %2")
                                .arg(outputPath, out.errorString()));
            out.cancelWriting();
            return false;
        }
        remaining -= chunk.size();
    }
    host.close();

    if (out.write(payload) != payload.size()) {
        setError(error, QStringLiteral("%1 cannot be written: %2")
                            .arg(outputPath, out.errorString()));
        out.cancelWriting();
        return false;
    }
    if (!out.commit()) {
        setError(error, QStringLiteral("%1 cannot be written: %2")
                            .arg(outputPath, out.errorString()));
        return false;
    }

    // `QSaveFile` preserves the *new* file's permissions from the template it
    // creates, which is user-read-write only. The artifact has to be executable,
    // or the thing a learner is told to double-click cannot be.
    QFile::setPermissions(outputPath, QFile::ReadOwner | QFile::WriteOwner |
                                         QFile::ExeOwner | QFile::ReadGroup |
                                         QFile::ExeGroup | QFile::ReadOther |
                                         QFile::ExeOther);
    return true;
}

QString artifactName(const QString& outputPath) {
    return QFileInfo{outputPath}.completeBaseName();
}

}  // namespace timewarp::ui::standalone
