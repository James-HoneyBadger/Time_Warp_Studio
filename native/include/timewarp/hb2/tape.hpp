// SPDX-License-Identifier: MIT
//
// The cassette: HB DOS's older sibling, kept as a second device.
//
// A disk is a tree you can see; a tape is a position. That is the whole reason the
// machine keeps one. A tape holds an ordered list of files, each with a header -- a
// name, a length and a checksum -- and a head that only goes forward unless somebody
// rewinds it. `CLOAD` reads from where the head is, so loading a name that has already
// gone by answers with the counter value instead of pretending the tape is a disk.
//
// It is `HbDos`'s shape on purpose: a value type with no widgets, one image, plain
// calls, and a test that drives the same methods a program reaches. The image is text
// so a cassette is something a person can read, copy and put in a repository, which is
// the same promise the disk images make.

#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace timewarp::hb2 {

/// One file on the tape, with the header a 1982 loader checked.
struct TapeFile {
    std::string name;
    std::vector<std::uint8_t> data;
    std::uint16_t checksum = 0;

    [[nodiscard]] std::uint16_t computeChecksum() const;
};

class Tape {
public:
    static constexpr std::size_t kNameLimit = 30;

    Tape();

    /// Records a file at the end of the tape. A tape is append-only: recording over a
    /// position is what a physical tape cannot do, and refusing it is what teaches the
    /// difference between a tape and a disk.
    bool record(const std::string& name, const std::vector<std::uint8_t>& bytes);

    /// Plays from the head forward until the named file arrives. A name that has
    /// already gone by is refused with "?PASSED" and the head does not move.
    bool load(const std::string& name, std::vector<std::uint8_t>* bytes);
    /// The file at the head, whatever it is called, which is what `CLOAD ""` reads.
    bool loadNext(std::vector<std::uint8_t>* bytes, std::string* name);

    void rewind();
    /// Plays past the file at the head without loading it.
    bool forward();
    /// The head's position, 0 before the first file and `size()` past the last.
    [[nodiscard]] std::size_t position() const noexcept { return head_; }
    [[nodiscard]] std::size_t size() const noexcept { return files_.size(); }

    /// One line a file, with its counter: `01  HELLO    42 BYTES  $1A2B`.
    [[nodiscard]] std::vector<std::string> catalog() const;
    /// What the device says, in the machine's voice: `?CHECKSUM`, `?PASSED`, `?TAPE
    /// FULL`. Empty when nothing is wrong.
    [[nodiscard]] const std::string& error() const noexcept { return error_; }
    void clearError();

    /// A blank tape: the cassette in the box, before anything is recorded.
    void erase();

    [[nodiscard]] std::string serialise() const;
    bool deserialise(const std::string& image);

    /// The header word a 1982 loader checked: the sum of the bytes, sixteen bits.
    /// Public because a file keeps one and a test checks it.
    [[nodiscard]] static std::uint16_t checksumOf(const std::vector<std::uint8_t>& bytes);

private:

    std::vector<TapeFile> files_;
    std::size_t head_ = 0;
    std::string error_;
};

}  // namespace timewarp::hb2
