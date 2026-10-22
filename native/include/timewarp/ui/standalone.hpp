// SPDX-License-Identifier: MIT
//
// Standalone executables: turning one program into a file that runs itself.
//
// A learner who finishes a program wants to send it to somebody -- a parent, a
// teacher, a classmate -- and "install a 40MB IDE, then open this .bas" is not
// that. This module builds the thing they actually want: a copy of the IDE with
// the program *inside it*, which runs the program as soon as it starts and shows
// nothing of the authoring UI. `docs/ARCHITECTURE.md` calls it "the compiler"
// because that is what a learner will call it; it compiles nothing, it appends.
//
// #### The format
//
// The payload is appended to a byte-for-byte copy of the running executable, so
// the result is a normal ELF that happens to have some bytes after its segments
// (which is legal -- a loader reads the headers, not the tail).
//
// **Format 1** (17.x, still read):
//
//     [ source, UTF-8 ]
//     [ source length, 8 bytes little-endian ]
//     [ language key, e.g. "basic" ]
//     [ language key length, 1 byte ]
//     [ magic, 16 bytes ]
//
// **Format 2** adds a metadata block *in front of* the source, so a program
// arrives with the name and the author on it and the window it opens in can be
// the size the author meant:
//
//     [ source, UTF-8 ]
//     [ metadata, UTF-8 JSON ]     {"title":…, "author":…, "screen":[w,h], …}
//     [ metadata length, 4 bytes little-endian ]
//     [ source length, 8 bytes little-endian ]
//     [ language key, e.g. "basic" ]
//     [ language key length, 1 byte ]
//     [ magic, 16 bytes ]          version 1 magic
//     [ format version, 1 byte ]   absent means 1
//
// The magic is last and fixed-width so a reader can start at the end of the
// file; the key's length sits immediately before it so the key can be found
// without a table of offsets, and the source length before that. A truncated
// file fails the magic check rather than producing a plausible-looking half
// program. The version byte is *after* the magic, so a format-1 artifact -- which
// has nothing there -- reads as version 1 by its absence, and the 17.x reader
// still finds the magic of a format-2 artifact (it would then misparse, which is
// why the version byte went first in the reader).
//
// Metadata is JSON rather than a binary struct so a field can be added without a
// third format: a reader ignores what it does not know, and a title is text in
// whatever language the author wrote it in.
//
// #### What this is not
//
// Not a static binary: the artifact links against Qt like the IDE does, so it
// runs on a machine with the same Qt. Not a license bypass either -- the payload
// is the user's own source. Both limits are stated in `native/README.md` rather
// than discovered by a teacher on the wrong laptop.

#pragma once

#include <QByteArray>
#include <QString>

namespace timewarp::ui::standalone {

/// How long the trailer's magic is.
inline constexpr int kMagicLength = 16;

/// The 16 bytes that mark an executable as carrying a program. Fixed width and
/// compared whole, so a file that merely *ends* with something similar is not
/// mistaken for one of these. Written as a byte array rather than a string
/// literal because it is a trailer, not text.
inline constexpr char kMagic[kMagicLength] = {'T', 'I', 'M', 'E', '-', 'W', 'A',
                                              'R', 'P', '-', 'S', 'P', 'L', '1',
                                              '\0', '\0'};

/// What an author can say about a program besides its text.
///
/// Every field is optional and an empty one is omitted from the encoded JSON, so
/// an artifact written by 17.x and one written by 18.0 with nothing filled in
/// produce the same bytes apart from the version byte.
struct Metadata {
    QString title;
    QString author;
    /// The size the program asked for, when it named a screen: zero means "let
    /// the window decide".
    int screen_width = 0;
    int screen_height = 0;
    /// The background the author saw, as `#RRGGBB`. Empty keeps the default.
    QString background;
    /// CRT strength 0-100 for the player's glass, 0 for none.
    int crt = 0;
};

/// A program read back out of an executable.
struct Embedded {
    /// Whether an executable at the path had a payload at all. False is the
    /// normal case: the IDE itself is built without one.
    bool present = false;
    /// 1 or 2; the version the artifact was written with.
    int format = 0;
    /// The language's settings key, e.g. `"basic"`.
    QString language;
    QString source;
    Metadata metadata;
};

/// The current format. Writing produces this; reading accepts 1 and 2.
inline constexpr int kFormat = 2;

/// Encodes a payload. Separate from writing it to a file so the format can be
/// tested without an executable to append to. No metadata, format 2.
[[nodiscard]] QByteArray encode(const QString& language, const QString& source);

/// Encodes a payload with metadata. An empty `metadata` still writes a format-2
/// trailer with an empty JSON object, because the format is a decision the
/// writer makes rather than a consequence of having filled a field in.
[[nodiscard]] QByteArray encode(const QString& language, const QString& source,
                                const Metadata& metadata);

/// Decodes bytes produced by `encode()`. Returns false and explains itself when
/// the magic is absent or the lengths do not fit the buffer -- which is what a
/// truncated download or a file that is simply not a program looks like.
[[nodiscard]] bool decode(const QByteArray& bytes, Embedded* out, QString* error);

/// Reads the payload from `executablePath`, if it has one. Never fails loudly:
/// an absent payload is the ordinary case for the IDE binary.
[[nodiscard]] Embedded embeddedIn(const QString& executablePath);

/// Writes `sourcePath`'s program into a copy of `hostExecutable`'s binary at
/// `outputPath`.
///
/// `overwrite` must be true to replace an existing file: a compile that silently
/// clobbers the program next to it is how a learner loses work. Returns false and
/// fills `error` on any failure, having left no half-written file behind.
[[nodiscard]] bool compileFile(const QString& hostExecutable, const QString& sourcePath,
                               const QString& outputPath, bool overwrite, QString* error);

/// The same, with metadata and an explicit host binary to copy.
[[nodiscard]] bool compileFile(const QString& hostExecutable, const QString& sourcePath,
                               const QString& outputPath, bool overwrite,
                               const Metadata& metadata, QString* error);

/// The JSON a format-2 payload carries, and its inverse. Public because a test
/// can then check that an unknown key survives a round trip neither side wrote.
[[nodiscard]] QByteArray metadataJson(const Metadata& metadata);
[[nodiscard]] bool metadataFromJson(const QByteArray& json, Metadata* out);

/// The program name shown in a standalone window's title: the artifact's own
/// base name without its extension, because that is what the user named it.
[[nodiscard]] QString artifactName(const QString& outputPath);

}  // namespace timewarp::ui::standalone
