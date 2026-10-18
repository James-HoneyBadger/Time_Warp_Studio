// SPDX-License-Identifier: MIT

#include "timewarp/ui/standalone.hpp"

#include <QFile>
#include <QFileInfo>
#include <QSaveFile>
#include <QtEndian>

#include "timewarp/ui/language.hpp"

namespace timewarp::ui::standalone {

namespace {

/// Where the language key length sits, counted back from the end.
constexpr int kLanguageLengthFromEnd = kMagicLength + 1;
/// And the eight-byte source length, before that.
constexpr int kSourceLengthFromEnd = kLanguageLengthFromEnd + 8;

void setError(QString* error, const QString& text) {
    if (error != nullptr) {
        *error = text;
    }
}

}  // namespace

QByteArray encode(const QString& language, const QString& source) {
    const QByteArray key = language.toUtf8();
    const QByteArray body = source.toUtf8();

    QByteArray payload;
    payload.reserve(body.size() + key.size() + kSourceLengthFromEnd);
    payload.append(body);

    QByteArray length(8, Qt::Uninitialized);
    qToLittleEndian<quint64>(static_cast<quint64>(body.size()), length.data());
    payload.append(length);

    // Key first, then its length, then the magic: the trailer has to be
    // readable from the end of the file, and a length that follows what it
    // measures cannot be found without already knowing the length.
    payload.append(key);
    // A single byte, so a language key is capped rather than silently truncated:
    // the longest key in the table is "smalltalk", and 255 is a bound nobody can
    // reach by accident.
    payload.append(static_cast<char>(key.size()));
    payload.append(kMagic, kMagicLength);
    return payload;
}

bool decode(const QByteArray& bytes, Embedded* out, QString* error) {
    if (bytes.size() < kSourceLengthFromEnd) {
        setError(error, QStringLiteral("the file is too short to be a program"));
        return false;
    }
    if (bytes.right(kMagicLength) != QByteArray{kMagic, kMagicLength}) {
        setError(error, QStringLiteral("this executable does not carry a program"));
        return false;
    }

    const int size = bytes.size();
    const int key_length =
        static_cast<unsigned char>(bytes.at(size - kLanguageLengthFromEnd));
    const int trailer = kSourceLengthFromEnd + key_length;
    if (key_length == 0 || size < trailer) {
        setError(error, QStringLiteral("the program's language key is missing"));
        return false;
    }

    const int key_offset = size - kLanguageLengthFromEnd - key_length;
    const int source_length = static_cast<int>(
        qFromLittleEndian<quint64>(bytes.constData() + size - trailer));
    const int source_offset = size - trailer - source_length;
    if (source_length < 0 || source_offset < 0) {
        setError(error, QStringLiteral("the program is truncated"));
        return false;
    }

    if (out != nullptr) {
        out->present = true;
        out->language = QString::fromUtf8(bytes.constData() + key_offset, key_length);
        out->source = QString::fromUtf8(bytes.constData() + source_offset, source_length);
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

    const QByteArray payload = encode(settingsKey(language), text);

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
