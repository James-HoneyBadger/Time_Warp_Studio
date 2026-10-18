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
//     [ source, UTF-8 ]
//     [ source length, 8 bytes little-endian ]
//     [ language key, e.g. "basic" ]
//     [ language key length, 1 byte ]
//     [ magic, 16 bytes ]
//
// The magic is last and fixed-width so a reader can start at the end of the
// file; the key's length sits immediately before it so the key can be found
// without a table of offsets, and the source length before that. A truncated
// file fails the magic check rather than producing a plausible-looking half
// program.
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

/// A program read back out of an executable.
struct Embedded {
    /// Whether an executable at the path had a payload at all. False is the
    /// normal case: the IDE itself is built without one.
    bool present = false;
    /// The language's settings key, e.g. `"basic"`.
    QString language;
    QString source;
};

/// Encodes a payload. Separate from writing it to a file so the format can be
/// tested without an executable to append to.
[[nodiscard]] QByteArray encode(const QString& language, const QString& source);

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

/// The program name shown in a standalone window's title: the artifact's own
/// base name without its extension, because that is what the user named it.
[[nodiscard]] QString artifactName(const QString& outputPath);

}  // namespace timewarp::ui::standalone
