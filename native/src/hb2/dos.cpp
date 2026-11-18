// SPDX-License-Identifier: MIT

#include "timewarp/hb2/dos.hpp"

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <istream>
#include <ostream>
#include <sstream>
#include <string_view>
#include <utility>

namespace timewarp::hb2 {

namespace {

// 45 columns by 64 rows is 2880 cells: one cell per sector on a floppy, and each
// cell a share of the capacity on the hard disk.
constexpr int kMapRows = 64;
constexpr int kMapColumns = 45;
constexpr std::uint64_t kMapCells = static_cast<std::uint64_t>(kMapRows) * kMapColumns;
constexpr std::string_view kDefaultVolume = "HBWARP2";

std::string upper(std::string_view text) {
    std::string out;
    out.reserve(text.size());
    for (const char letter : text) {
        out.push_back(static_cast<char>(std::toupper(static_cast<unsigned char>(letter))));
    }
    return out;
}

std::string trim(std::string_view text) {
    std::size_t first = 0;
    std::size_t last = text.size();
    while (first < last && std::isspace(static_cast<unsigned char>(text[first])) != 0) ++first;
    while (last > first && std::isspace(static_cast<unsigned char>(text[last - 1])) != 0) --last;
    return std::string(text.substr(first, last - first));
}

/// The path's components, upper case, empties collapsed. Validation and the current
/// directory belong to a drive, so they are the caller's business.
std::vector<std::string> componentsOf(std::string_view path) {
    std::vector<std::string> parts;
    std::size_t start = 0;
    while (start < path.size()) {
        const std::size_t slash = path.find('/', start);
        const std::size_t end = slash == std::string_view::npos ? path.size() : slash;
        if (end > start) parts.push_back(upper(path.substr(start, end - start)));
        if (slash == std::string_view::npos) break;
        start = slash + 1;
    }
    return parts;
}

std::string joinPath(const std::vector<std::string>& parts) {
    std::string out;
    for (const std::string& part : parts) out += '/' + part;
    return out.empty() ? std::string("/") : out;
}

constexpr std::uint32_t sectorsFor(std::uint64_t bytes) {
    const auto size = static_cast<std::uint64_t>(Dos::kSectorSize);
    return static_cast<std::uint32_t>((bytes + size - 1) / size);
}

/// Every allocation in the tree: one sector per directory (the root's own included,
/// which is why a freshly formatted disk shows one sector used) and ceil(bytes/512)
/// per file.
std::uint32_t usedSectors(const DosEntry& directory) {
    std::uint32_t total = 1;
    for (const DosEntry& child : directory.children) {
        total += child.directory ? usedSectors(child) : sectorsFor(child.bytes);
    }
    return total;
}

/// The refusal deleting or writing over an entry earns, or null when it may be.
const char* replaceWhy(const DosEntry* entry) {
    if (entry == nullptr) return nullptr;
    if (entry->directory) return "?IS A DIRECTORY";
    if (entry->locked) return "?FILE LOCKED";
    return entry->read_only ? "?FILE PROTECTED" : nullptr;
}

DosEntry directoryEntry(std::string name, const Timestamp& stamp) {
    DosEntry made;
    made.name = std::move(name);
    made.directory = true;
    made.stamp = stamp;
    return made;
}

template <typename Entry>
Entry* childNamed(Entry& directory, std::string_view name) {
    for (Entry& child : directory.children) {
        if (child.name == name) return &child;
    }
    return nullptr;
}

/// Walks `parts` from `root`, never through a file. Deduced over const and
/// non-const entries, so one walk serves both readers and writers.
template <typename Entry>
Entry* walkTo(Entry& root, const std::vector<std::string>& parts) {
    Entry* found = &root;
    for (const std::string& part : parts) {
        if (!found->directory) return nullptr;
        found = childNamed(*found, part);
        if (found == nullptr) return nullptr;
    }
    return found;
}

std::string attributesOf(const DosEntry& entry) {
    std::string out = "---";
    if (entry.read_only) out[0] = 'r';
    if (entry.hidden) out[1] = 'h';
    if (entry.locked) out[2] = 'l';
    return out;
}

std::string hexOf(const std::vector<std::uint8_t>& bytes) {
    constexpr char kDigits[] = "0123456789abcdef";
    std::string out;
    out.reserve(bytes.size() * 2);
    for (const std::uint8_t byte : bytes) {
        out.push_back(kDigits[byte >> 4]);
        out.push_back(kDigits[byte & 0x0F]);
    }
    return out;
}

bool parseHex(const std::string& text, std::vector<std::uint8_t>& bytes) {
    bytes.clear();
    std::string digits;
    for (const char letter : text) {
        if (std::isxdigit(static_cast<unsigned char>(letter)) != 0) {
            digits.push_back(letter);
        } else if (std::isspace(static_cast<unsigned char>(letter)) == 0) {
            return false;
        }
    }
    if (digits.size() % 2 != 0) return false;
    // Case-folded, so one expression serves both spellings of a hex digit.
    const auto value = [](char digit) {
        const char folded = static_cast<char>(digit | 0x20);
        return folded <= '9' ? digit - '0' : folded - 'a' + 10;
    };
    for (std::size_t index = 0; index < digits.size(); index += 2) {
        bytes.push_back(
            static_cast<std::uint8_t>((value(digits[index]) << 4) | value(digits[index + 1])));
    }
    return true;
}

/// A count field: digits only, short enough to be a sector count.
bool parseCount(const std::string& text, std::uint32_t& value) {
    if (text.empty() || text.size() > 9) return false;
    std::uint32_t total = 0;
    for (const char digit : text) {
        if (digit < '0' || digit > '9') return false;
        total = total * 10 + static_cast<std::uint32_t>(digit - '0');
    }
    value = total;
    return true;
}

bool validStamp(const Timestamp& stamp) {
    return stamp.year >= 1 && stamp.month >= 1 && stamp.month <= 12 && stamp.day >= 1 &&
           stamp.day <= 31 && stamp.hour <= 23 && stamp.minute <= 59;
}

/// Reads the two tokens an image writes for a stamp: "92-10-31" and "12:05".
bool parseStamp(const std::string& date, const std::string& time, Timestamp& stamp) {
    unsigned year = 0, month = 0, day = 0, hour = 0, minute = 0;
    if (std::sscanf(date.c_str(), "%4u-%2u-%2u", &year, &month, &day) != 3 ||
        std::sscanf(time.c_str(), "%2u:%2u", &hour, &minute) != 2) {
        return false;
    }
    const Timestamp read{static_cast<std::uint16_t>(year), static_cast<std::uint8_t>(month),
                         static_cast<std::uint8_t>(day), static_cast<std::uint8_t>(hour),
                         static_cast<std::uint8_t>(minute)};
    if (!validStamp(read)) return false;
    stamp = read;
    return true;
}

bool bad(std::string& why) {
    why = "?BAD IMAGE";
    return false;
}

/// Places a parsed entry at its full path, making any directory an image names in a
/// path but never wrote a line for: an image may be typed by hand, and a missing
/// directory is not a reason to lose the files under it.
bool attach(DosEntry& root, const std::string& path, DosEntry entry, std::string& why) {
    const std::vector<std::string> parts = componentsOf(path);
    if (parts.empty() || parts.back().size() > Dos::kNameLimit) return bad(why);
    DosEntry* directory = &root;
    for (std::size_t step = 0; step + 1 < parts.size(); ++step) {
        if (parts[step].size() > Dos::kNameLimit) return bad(why);
        DosEntry* next = childNamed(*directory, parts[step]);
        if (next == nullptr) {
            directory->children.push_back(directoryEntry(parts[step], entry.stamp));
            next = &directory->children.back();
        }
        if (!next->directory) return bad(why);
        directory = next;
    }
    if (childNamed(*directory, parts.back()) != nullptr) return bad(why);
    entry.name = parts.back();
    directory->children.push_back(std::move(entry));
    return true;
}

/// The image's lines. A version 1 line is `F <bytes> <name>` and a line of hex, the
/// 1982 volume being flat; version 2 adds the directory line, the flags, the stamp
/// and the full path. Both end at `END` or at the end of the image.
bool readImage(std::istream& in, DosEntry& root, std::string& name, bool version2,
               std::string& why) {
    std::string line;
    while (std::getline(in, line)) {
        const std::string text = trim(line);
        if (text.empty()) continue;
        if (text == "END") return true;
        std::istringstream fields(text);
        std::string kind;
        if (!(fields >> kind)) return bad(why);
        if (kind == "VOLUME") {
            std::string rest;
            std::getline(fields, rest);
            name = upper(trim(rest));
            if (name.empty() || name.size() > Dos::kNameLimit) return bad(why);
            continue;
        }
        DosEntry entry;
        if (version2) {
            std::string attributes;
            std::string date;
            std::string time;
            if ((kind != "D" && kind != "F") || !(fields >> attributes >> date >> time) ||
                attributes.size() != 3 || !parseStamp(date, time, entry.stamp)) {
                return bad(why);
            }
            entry.directory = kind == "D";
            entry.read_only = attributes[0] == 'r' || attributes[0] == 'R';
            entry.hidden = attributes[1] == 'h' || attributes[1] == 'H';
            entry.locked = attributes[2] == 'l' || attributes[2] == 'L';
        } else if (kind != "F") {
            return bad(why);
        }
        if (!entry.directory) {
            std::string count;
            std::string payload;
            std::uint32_t declared = 0;
            if (!(fields >> count) || !parseCount(count, declared)) return bad(why);
            std::getline(in, payload);
            if (!parseHex(payload, entry.data) || entry.data.size() != declared) return bad(why);
            entry.bytes = static_cast<std::uint32_t>(entry.data.size());
        }
        std::string rest;
        std::getline(fields, rest);
        const std::string path = trim(rest);
        // A 1982 name is bare and lands in the root; a path would be the other
        // volume's grammar.
        if (!version2 && path.find('/') != std::string::npos) return bad(why);
        if (!attach(root, path, std::move(entry), why)) return false;
    }
    return true;
}

void writeTree(std::ostream& out, const DosEntry& directory, const std::string& prefix) {
    for (const DosEntry& child : directory.children) {
        const std::string path = prefix + '/' + child.name;
        out << (child.directory ? 'D' : 'F') << ' ' << attributesOf(child) << ' '
            << child.stamp.text();
        if (!child.directory) out << ' ' << child.bytes;
        out << ' ' << path << '\n';
        if (child.directory) {
            writeTree(out, child, path);
        } else {
            out << hexOf(child.data) << '\n';
        }
    }
}

}  // namespace

// --- Timestamp ---------------------------------------------------------------

std::string Timestamp::text() const {
    char text[24] = {};
    std::snprintf(text, sizeof text, "%02u-%02u-%02u %02u:%02u", static_cast<unsigned>(year),
                  static_cast<unsigned>(month), static_cast<unsigned>(day),
                  static_cast<unsigned>(hour), static_cast<unsigned>(minute));
    return text;
}

// --- The drives --------------------------------------------------------------

Dos::Dos() : volumes_(static_cast<std::size_t>(kDrives)) { format(0); format(1); }

bool Dos::fail(std::string sentence) const {
    error_ = std::move(sentence);
    return false;
}

void Dos::clearError() { error_.clear(); }

int Dos::checkedDrive(int drive) const {
    if (drive < 0 || drive >= kDrives) {
        fail("?NO SUCH DRIVE");
        return -1;
    }
    return drive;
}

bool Dos::format(int drive, const std::string& volume) {
    const int index = checkedDrive(drive);
    if (index < 0) return false;
    if (volume.size() > kNameLimit) return fail("?NAME TOO LONG");
    Volume& disk = volumes_[static_cast<std::size_t>(index)];
    disk.name = volume.empty() ? std::string(kDefaultVolume) : upper(volume);
    disk.sectors = index == 0 ? kFloppySectors : kHardDiskSectors;
    disk.root = directoryEntry("/", clock_);
    disk.current.clear();
    return ok();
}

bool Dos::pathParts(int drive, const std::string& path, std::vector<std::string>& parts) const {
    const Volume& disk = volumes_[static_cast<std::size_t>(drive)];
    parts = (path.empty() || path.front() == '/') ? std::vector<std::string>{} : disk.current;
    for (const std::string& part : componentsOf(path)) {
        if (part == ".") continue;
        if (part == "..") {
            if (!parts.empty()) parts.pop_back();
            continue;
        }
        if (part.size() > kNameLimit) return fail("?NAME TOO LONG");
        parts.push_back(part);
    }
    return ok();
}

const DosEntry* Dos::lookup(int drive, const std::string& path, const char* missing) const {
    std::vector<std::string> parts;
    if (!pathParts(drive, path, parts)) return nullptr;
    const DosEntry* found = walkTo(volumes_[static_cast<std::size_t>(drive)].root, parts);
    if (found == nullptr) fail(missing);
    return found;
}

bool Dos::siteOf(int drive, const std::string& path, Site& site) {
    std::vector<std::string> parts;
    if (!pathParts(drive, path, parts)) return false;
    if (parts.empty()) return fail("?INVALID PATH");
    site.leaf = parts.back();
    parts.pop_back();
    site.parent = walkTo(volumes_[static_cast<std::size_t>(drive)].root, parts);
    if (site.parent == nullptr || !site.parent->directory) return fail("?PATH NOT FOUND");
    site.entry = childNamed(*site.parent, site.leaf);
    return true;
}

bool Dos::roomFor(int drive, std::uint32_t charged, std::uint32_t want) const {
    const Volume& disk = volumes_[static_cast<std::size_t>(drive)];
    if (usedSectors(disk.root) - charged + want > disk.sectors) return fail("?DISK FULL");
    return true;
}

std::string Dos::currentDirectory(int drive) const {
    const int index = checkedDrive(drive);
    if (index < 0) return {};
    error_.clear();
    return joinPath(volumes_[static_cast<std::size_t>(index)].current);
}

bool Dos::setCurrentDirectory(int drive, const std::string& path) {
    const int index = checkedDrive(drive);
    if (index < 0) return false;
    std::vector<std::string> parts;
    if (!pathParts(index, path, parts)) return false;
    const DosEntry* found = walkTo(volumes_[static_cast<std::size_t>(index)].root, parts);
    if (found == nullptr) return fail("?PATH NOT FOUND");
    if (!found->directory) return fail("?NOT A DIRECTORY");
    volumes_[static_cast<std::size_t>(index)].current = parts;
    return ok();
}

// --- The catalogue -----------------------------------------------------------

bool Dos::mkdir(int drive, const std::string& path) {
    const int index = checkedDrive(drive);
    if (index < 0) return false;
    Site site;
    if (!siteOf(index, path, site)) return false;
    if (site.entry != nullptr) return fail("?FILE EXISTS");
    if (!roomFor(index, 0, 1)) return false;
    site.parent->children.push_back(directoryEntry(site.leaf, clock_));
    return ok();
}

bool Dos::rmdir(int drive, const std::string& path) {
    const int index = checkedDrive(drive);
    if (index < 0) return false;
    Site site;
    if (!siteOf(index, path, site)) return false;
    if (site.entry == nullptr) return fail("?PATH NOT FOUND");
    if (!site.entry->directory) return fail("?NOT A DIRECTORY");
    if (!site.entry->children.empty()) return fail("?DIRECTORY NOT EMPTY");
    std::vector<std::string> parts;
    if (!pathParts(index, path, parts)) return false;
    const std::vector<std::string>& current = volumes_[static_cast<std::size_t>(index)].current;
    if (parts.size() <= current.size() && std::equal(parts.begin(), parts.end(), current.begin())) {
        return fail("?DIRECTORY IN USE");
    }
    DosEntry* doomed = site.entry;
    std::erase_if(site.parent->children,
                  [doomed](const DosEntry& child) { return &child == doomed; });
    return ok();
}

bool Dos::writeFile(int drive, const std::string& path, const std::vector<std::uint8_t>& bytes) {
    const int index = checkedDrive(drive);
    if (index < 0) return false;
    Site site;
    if (!siteOf(index, path, site)) return false;
    if (const char* why = replaceWhy(site.entry); why != nullptr) return fail(why);
    // Charged, never truncated: a write that does not fit leaves the old file whole.
    const std::uint32_t charged = site.entry == nullptr ? 0 : sectorsFor(site.entry->bytes);
    if (!roomFor(index, charged, sectorsFor(bytes.size()))) return false;
    if (site.entry == nullptr) {
        DosEntry made;
        made.name = site.leaf;
        site.parent->children.push_back(std::move(made));
        site.entry = &site.parent->children.back();
    }
    site.entry->data = bytes;
    site.entry->bytes = static_cast<std::uint32_t>(bytes.size());
    site.entry->stamp = clock_;
    return ok();
}

std::vector<std::uint8_t> Dos::readFile(int drive, const std::string& path) const {
    const int index = checkedDrive(drive);
    if (index < 0) return {};
    const DosEntry* found = lookup(index, path, "?FILE NOT FOUND");
    if (found == nullptr) return {};
    if (found->directory) {
        fail("?IS A DIRECTORY");
        return {};
    }
    error_.clear();
    return found->data;
}

bool Dos::exists(int drive, const std::string& path) const {
    const int index = checkedDrive(drive);
    if (index < 0) return false;
    if (lookup(index, path, "?FILE NOT FOUND") == nullptr) return false;
    return ok();
}

bool Dos::removeFile(int drive, const std::string& path) {
    const int index = checkedDrive(drive);
    if (index < 0) return false;
    Site site;
    if (!siteOf(index, path, site)) return false;
    if (site.entry == nullptr) return fail("?FILE NOT FOUND");
    if (const char* why = replaceWhy(site.entry); why != nullptr) return fail(why);
    DosEntry* doomed = site.entry;
    std::erase_if(site.parent->children,
                  [doomed](const DosEntry& child) { return &child == doomed; });
    return ok();
}

bool Dos::rename(int drive, const std::string& from, const std::string& to) {
    const int index = checkedDrive(drive);
    if (index < 0) return false;
    Volume& disk = volumes_[static_cast<std::size_t>(index)];
    std::vector<std::string> source_parts;
    std::vector<std::string> target_parts;
    if (!pathParts(index, from, source_parts) || !pathParts(index, to, target_parts)) return false;
    if (source_parts.empty() || target_parts.empty()) return fail("?INVALID PATH");
    DosEntry* source = walkTo(disk.root, source_parts);
    if (source == nullptr) return fail("?FILE NOT FOUND");
    // A directory cannot be moved inside itself: the tree would leave the root.
    if (source->directory && target_parts.size() > source_parts.size() &&
        std::equal(source_parts.begin(), source_parts.end(), target_parts.begin())) {
        return fail("?INVALID PATH");
    }
    Site target;
    if (!siteOf(index, to, target)) return false;
    if (target.entry != nullptr) return fail("?FILE EXISTS");
    std::vector<std::string> source_parent_parts(source_parts.begin(), source_parts.end() - 1);
    DosEntry* source_parent = walkTo(disk.root, source_parent_parts);
    DosEntry moved = std::move(*source);
    std::erase_if(source_parent->children,
                  [source](const DosEntry& child) { return &child == source; });
    moved.name = target.leaf;
    // Resolved again: the erase moved the elements of the source's own directory, and
    // this cannot fail because every component of the destination was just checked.
    if (!siteOf(index, to, target)) return false;
    target.parent->children.push_back(std::move(moved));
    // A rename of the current directory, or of one above it, keeps it current.
    if (disk.current.size() >= source_parts.size() &&
        std::equal(source_parts.begin(), source_parts.end(), disk.current.begin())) {
        std::vector<std::string> updated = target_parts;
        updated.insert(updated.end(),
                       disk.current.begin() + static_cast<std::ptrdiff_t>(source_parts.size()),
                       disk.current.end());
        disk.current = std::move(updated);
    }
    return ok();
}

bool Dos::copy(int drive, const std::string& from, const std::string& to) {
    const int index = checkedDrive(drive);
    if (index < 0) return false;
    std::vector<std::string> source_parts;
    if (!pathParts(index, from, source_parts)) return false;
    if (source_parts.empty()) return fail("?INVALID PATH");
    // Reading a locked file is exactly what a lock permits.
    const DosEntry* source = walkTo(volumes_[static_cast<std::size_t>(index)].root, source_parts);
    if (source == nullptr) return fail("?FILE NOT FOUND");
    if (source->directory) return fail("?IS A DIRECTORY");
    std::vector<std::string> target_parts;
    if (!pathParts(index, to, target_parts)) return false;
    if (target_parts.empty()) return fail("?INVALID PATH");
    // "COPY A:X B:\Y" names a drawer, not a new name for the file.
    const DosEntry* target = walkTo(volumes_[static_cast<std::size_t>(index)].root, target_parts);
    if (target != nullptr && target->directory) target_parts.push_back(source_parts.back());
    if (target_parts == source_parts) return fail("?FILE EXISTS");
    // Copied before the write: the destination may share the source's directory, and
    // a push_back there moves its elements.
    const std::vector<std::uint8_t> payload = source->data;
    return writeFile(drive, joinPath(target_parts), payload);
}

bool Dos::move(int drive, const std::string& from, const std::string& to) {
    const int index = checkedDrive(drive);
    if (index < 0) return false;
    std::vector<std::string> target_parts;
    if (!pathParts(index, to, target_parts)) return false;
    const DosEntry* target = walkTo(volumes_[static_cast<std::size_t>(index)].root, target_parts);
    if (target == nullptr || !target->directory) return rename(drive, from, to);
    std::vector<std::string> source_parts;
    if (!pathParts(index, from, source_parts)) return false;
    if (source_parts.empty()) return fail("?INVALID PATH");
    target_parts.push_back(source_parts.back());
    return rename(drive, from, joinPath(target_parts));
}

bool Dos::setFlag(int drive, const std::string& path, bool DosEntry::*flag, bool value) {
    const int index = checkedDrive(drive);
    if (index < 0) return false;
    Site site;
    if (!siteOf(index, path, site)) return false;
    if (site.entry == nullptr) return fail("?FILE NOT FOUND");
    site.entry->*flag = value;
    return ok();
}

bool Dos::setLocked(int drive, const std::string& path, bool locked) {
    return setFlag(drive, path, &DosEntry::locked, locked);
}
bool Dos::setReadOnly(int drive, const std::string& path, bool read_only) {
    return setFlag(drive, path, &DosEntry::read_only, read_only);
}
bool Dos::setHidden(int drive, const std::string& path, bool hidden) {
    return setFlag(drive, path, &DosEntry::hidden, hidden);
}

bool Dos::setTimestamp(int drive, const std::string& path, const Timestamp& stamp) {
    if (!validStamp(stamp)) return fail("?INVALID DATE");
    const int index = checkedDrive(drive);
    if (index < 0) return false;
    Site site;
    if (!siteOf(index, path, site)) return false;
    if (site.entry == nullptr) return fail("?FILE NOT FOUND");
    site.entry->stamp = stamp;
    return ok();
}

std::vector<DosRow> Dos::list(int drive, const std::string& path, bool include_hidden) const {
    std::vector<DosRow> rows;
    const int index = checkedDrive(drive);
    if (index < 0) return rows;
    const DosEntry* directory = lookup(index, path, "?PATH NOT FOUND");
    if (directory == nullptr) return rows;
    if (!directory->directory) {
        fail("?NOT A DIRECTORY");
        return rows;
    }
    for (const DosEntry& child : directory->children) {
        const bool dotted = !child.name.empty() && child.name.front() == '.';
        if (!include_hidden && (child.hidden || dotted)) continue;
        rows.push_back(DosRow{child.name, child.directory ? 0 : child.bytes, child.directory,
                              child.read_only, child.hidden, child.locked, child.stamp});
    }
    std::sort(rows.begin(), rows.end(), [](const DosRow& left, const DosRow& right) {
        if (left.directory != right.directory) return left.directory && !right.directory;
        return left.name < right.name;
    });
    error_.clear();
    return rows;
}

DosFree Dos::free(int drive) const {
    DosFree report;
    const int index = checkedDrive(drive);
    if (index < 0) return report;
    const Volume& disk = volumes_[static_cast<std::size_t>(index)];
    report.sectors = disk.sectors;
    report.used = std::min(usedSectors(disk.root), disk.sectors);
    report.free = disk.sectors - report.used;
    report.map.assign(static_cast<std::size_t>(kMapRows), std::string(kMapColumns, '.'));
    // Filled in allocation order, which is the front of the disk: a sector is handed
    // out only when an entry needs one, so the used share is one run. A floppy gets a
    // cell per sector; the hard disk's twenty-eight sectors share a cell.
    const std::uint64_t share = static_cast<std::uint64_t>(report.used) * kMapCells;
    const std::uint64_t marks =
        disk.sectors == 0 ? 0 : std::min<std::uint64_t>((share + disk.sectors - 1) / disk.sectors,
                                                       kMapCells);
    for (std::uint64_t cell = 0; cell < marks; ++cell) {
        report.map[static_cast<std::size_t>(cell / kMapColumns)]
                  [static_cast<std::size_t>(cell % kMapColumns)] = '#';
    }
    error_.clear();
    return report;
}

std::string Dos::volumeName(int drive) const {
    const int index = checkedDrive(drive);
    if (index < 0) return {};
    error_.clear();
    return volumes_[static_cast<std::size_t>(index)].name;
}

std::uint32_t Dos::capacity(int drive) const {
    const int index = checkedDrive(drive);
    if (index < 0) return 0;
    error_.clear();
    return volumes_[static_cast<std::size_t>(index)].sectors;
}

// --- The image ---------------------------------------------------------------

std::string Dos::serialise(int drive) const {
    const int index = checkedDrive(drive);
    if (index < 0) return {};
    const Volume& disk = volumes_[static_cast<std::size_t>(index)];
    std::ostringstream out;
    out << "HBDOS 2\n" << "VOLUME " << disk.name << '\n';
    writeTree(out, disk.root, std::string{});
    out << "END\n";
    error_.clear();
    return out.str();
}

bool Dos::deserialise(int drive, const std::string& image) {
    const int index = checkedDrive(drive);
    if (index < 0) return false;
    std::istringstream in(image);
    std::string header;
    while (std::getline(in, header) && trim(header).empty()) {
    }
    header = trim(header);
    if (header != "HBDOS 1" && header != "HBDOS 2") return fail("?BAD IMAGE");
    DosEntry root = directoryEntry("/", clock_);
    // An image carries the tree, never the capacity: a volume whose files do not fit
    // the drive is a full disk, not a corrupt image.
    Volume& disk = volumes_[static_cast<std::size_t>(index)];
    std::string name = disk.name;
    std::string why;
    if (!readImage(in, root, name, header == "HBDOS 2", why)) return fail(why);
    if (usedSectors(root) > disk.sectors) return fail("?DISK FULL");
    disk.name = name;
    disk.root = std::move(root);
    disk.current.clear();
    return ok();
}

void Dos::setClock(const Timestamp& stamp) {
    if (!validStamp(stamp)) {
        fail("?INVALID DATE");
        return;
    }
    clock_ = stamp;
    error_.clear();
}

}  // namespace timewarp::hb2
