// SPDX-License-Identifier: MIT
//
// HB DOS 2.0: the HB-2's disk operating system.
//
// ---------------------------------------------------------------------------
// Why it is a value type and not an object on the host
// ---------------------------------------------------------------------------
//
// The HB-1's HB DOS was one flat volume of named programs, because that is what a
// 1982 home computer shipped with. The HB-2 is a 1992 machine with two drives, so
// its DOS is the DOS of 1992: A: is a 1.44 MB floppy, B: is a 40 MB hard disk, and
// a disk holds a *tree* -- directories, subdirectories, and the full paths that
// makes possible, with the timestamps and the read-only, hidden and locked
// attributes a catalogue of that era printed in its DIR listing.
//
// The whole thing is a plain value type: a tree per drive, and an image that
// serialises to text and back. It holds no clock of its own and reads none from the
// host -- `setClock` is how a caller (or a test) hands it the machine's RTC reading,
// which is what makes a listing reproducible on any machine at any moment.
//
// Two rules are worth stating here because everything else follows from them:
//
//   * Every byte is charged to 512-byte sectors, and every directory costs one
//     sector. A write that does not fit is refused and the old file survives; the
//     DOS never truncates to make room, because a truncated file is a file the user
//     did not ask for.
//   * Every failure sets `error()` to one short sentence beginning with '?', and
//     every success clears it. That is exactly what a program printed to the screen,
//     and it means a caller can run a command, look at `error()`, and print it.
//   * A name longer than `kNameLimit` is refused with `?NAME TOO LONG`, never cut
//     down to fit: a catalogue that silently renames a file is one the user cannot
//     trust to hold what they saved.

#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

namespace timewarp::hb2 {

/// A point in the machine's own calendar (the RTC's), never the host's.
struct Timestamp {
    std::uint16_t year = 92;
    std::uint8_t month = 1;
    std::uint8_t day = 1;
    std::uint8_t hour = 0;
    std::uint8_t minute = 0;
    /// The stamp as a catalogue prints it: "92-10-31 12:05". The year is the only
    /// field that may grow past two digits, so a stamp after 1999 still reads as
    /// itself.
    [[nodiscard]] std::string text() const;
};

/// One file or directory.
struct DosEntry {
    /// As stored and as listed: upper case, at most `Dos::kNameLimit` characters.
    std::string name;
    bool directory = false;
    bool read_only = false;
    bool hidden = false;
    /// A locked file cannot be deleted or written over, which is a disk's
    /// write-protect tab made per file.
    bool locked = false;
    /// Files: the length of `data`. Directories: zero.
    std::uint32_t bytes = 0;
    Timestamp stamp;
    /// Files only.
    std::vector<std::uint8_t> data;
    /// Directories only. A file's list is empty.
    std::vector<DosEntry> children;
};

/// One catalogue row, as DIR prints it.
struct DosRow {
    std::string name;
    std::uint32_t bytes = 0;
    bool directory = false;
    bool read_only = false;
    bool hidden = false;
    bool locked = false;
    Timestamp stamp;
};

/// What FREE reports: every sector accounted for, none lost or double counted.
struct DosFree {
    /// The drive's capacity in sectors.
    std::uint32_t sectors = 0;
    /// The sum of every allocation in the tree, the root directory's own sector
    /// included.
    std::uint32_t used = 0;
    /// `sectors - used`, so the two always add up to the capacity.
    std::uint32_t free = 0;
    /// 64 rows of 45 characters, '#' used and '.' free, each cell standing for its
    /// share of the capacity, filled in the order the sectors were allocated.
    std::vector<std::string> map;
};

/// Two drives of disk, one catalogue per drive, one image per drive.
class Dos {
public:
    static constexpr int kDrives = 2;
    static constexpr int kSectorSize = 512;
    static constexpr std::uint32_t kFloppySectors = 2880;    // 1.44 MB in A:
    static constexpr std::uint32_t kHardDiskSectors = 81920; // 40 MB in B:
    static constexpr std::size_t kNameLimit = 30;

    /// A machine that has just been switched on has both disks formatted.
    Dos();

    /// Formats a drive: empty root, capacity by drive number. Returns false with a
    /// sentence in error() for a drive that does not exist.
    bool format(int drive, const std::string& volume = "HBWARP2");

    /// The current directory of `drive`, as "/A/B". Setting it is cd(); it must be an
    /// existing directory or the call is refused.
    [[nodiscard]] std::string currentDirectory(int drive) const;
    bool setCurrentDirectory(int drive, const std::string& path);

    /// Creates a directory. The parent must exist; an existing name is refused.
    bool mkdir(int drive, const std::string& path);
    /// Removes an empty directory that is not the current one nor above it.
    bool rmdir(int drive, const std::string& path); // refused when not empty
    /// Creates or replaces a file, stamped with the clock. A write that does not
    /// fit, or a target that is locked or read-only, is refused and the old file is
    /// left exactly as it was.
    bool writeFile(int drive, const std::string& path,
                   const std::vector<std::uint8_t>& bytes);
    /// The file's bytes, or an empty vector with `?FILE NOT FOUND` in error().
    [[nodiscard]] std::vector<std::uint8_t> readFile(int drive, const std::string& path) const;
    /// Whether the path names anything, the root included.
    bool exists(int drive, const std::string& path) const;
    bool removeFile(int drive, const std::string& path); // refused when locked or read-only
    /// Gives an entry a new name, and optionally a new directory on the same drive.
    /// A name already in the destination is refused.
    bool rename(int drive, const std::string& from, const std::string& to);
    /// A new file with the same bytes: allowed from a locked file, because reading a
    /// locked file was never what the lock forbade.
    bool copy(int drive, const std::string& from, const std::string& to);
    /// rename across directories in the same drive.
    bool move(int drive, const std::string& from, const std::string& to);
    bool setLocked(int drive, const std::string& path, bool locked);
    bool setReadOnly(int drive, const std::string& path, bool read_only);
    bool setHidden(int drive, const std::string& path, bool hidden);
    bool setTimestamp(int drive, const std::string& path, const Timestamp& stamp);

    /// The catalogue of a directory (default: the current one), sorted by name with
    /// directories first. An entry whose name starts with '.' -- or one marked
    /// hidden -- is listed only when `include_hidden`.
    [[nodiscard]] std::vector<DosRow> list(int drive, const std::string& path = {},
                                           bool include_hidden = false) const;
    /// The space report: capacity, allocation, what is left, and the map.
    [[nodiscard]] DosFree free(int drive) const;
    [[nodiscard]] std::string volumeName(int drive) const;
    [[nodiscard]] std::uint32_t capacity(int drive) const;

    /// The machine's voice: "?FILE LOCKED", and empty when nothing has gone wrong.
    [[nodiscard]] const std::string& error() const noexcept { return error_; }
    void clearError();

    /// Serialised image, with a format number. Version 2 carries the tree; a version 1
    /// image (the 1982 flat volume) reads as a root full of files.
    [[nodiscard]] std::string serialise(int drive) const;
    bool deserialise(int drive, const std::string& image);

    /// The RTC reading new entries are stamped with. Set by the caller so that a
    /// listing is reproducible instead of depending on when the test ran.
    void setClock(const Timestamp& stamp);
    [[nodiscard]] Timestamp clock() const noexcept { return clock_; }

private:
    /// One drive: its volume name, its capacity, its tree and where in it the
    /// current directory is (as components below the root, which is how ".." is one
    /// pop).
    struct Volume {
        std::string name = "HBWARP2";
        std::uint32_t sectors = 0;
        DosEntry root;
        std::vector<std::string> current;
    };

    /// A path resolved to the place its last component lives: the directory holding
    /// it, that component's name, and the entry itself when something is there.
    struct Site {
        DosEntry* parent = nullptr;
        std::string leaf;
        DosEntry* entry = nullptr;
    };

    bool fail(std::string sentence) const;
    [[nodiscard]] bool ok() const noexcept {
        error_.clear();
        return true;
    }
    /// -1 with the error set for a drive that does not exist.
    [[nodiscard]] int checkedDrive(int drive) const;
    /// The path as upper-case components from the root, resolving "." and "..",
    /// starting at the current directory unless the path begins with '/'.
    [[nodiscard]] bool pathParts(int drive, const std::string& path,
                                 std::vector<std::string>& parts) const;
    /// The entry a path names, or null with `missing` in error().
    [[nodiscard]] const DosEntry* lookup(int drive, const std::string& path,
                                         const char* missing) const;
    /// Resolves a path to the directory that holds its last component, refusing the
    /// root itself. `site.entry` is null when the name is free.
    [[nodiscard]] bool siteOf(int drive, const std::string& path, Site& site);
    /// Charges `want` sectors in place of the `charged` the entry already holds.
    [[nodiscard]] bool roomFor(int drive, std::uint32_t charged, std::uint32_t want) const;
    [[nodiscard]] bool setFlag(int drive, const std::string& path, bool DosEntry::*flag,
                               bool value);

    std::vector<Volume> volumes_;
    Timestamp clock_;
    mutable std::string error_;
};

}  // namespace timewarp::hb2
