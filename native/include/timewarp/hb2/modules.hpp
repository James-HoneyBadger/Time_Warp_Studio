// SPDX-License-Identifier: MIT
//
// The module bay: the ROM socket stops being a fixed list.
//
// In 1982 the socket was the factory's: eleven cartridges, and no slot for a twelfth.
// By 1992 the cartridge had given way to the floppy, and in this parallel universe the
// eleven built-in ROMs stay exactly where they are while the socket becomes something
// a user can fill. A module is a file in a bay directory, and there are three kinds:
//
//   script  a list of prompt lines, replayed as though they had been typed;
//   binary  HB-16 code with a header saying where it loads;
//   image   a saved machine, restored whole.
//
// The bay is a plain format -- a header line and then lines -- so a module is
// something a person can read, diff and put in a repository, which is the same
// promise the HB DOS images make.

#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace timewarp::hb2 {

/// What a bay directory holds, as `MODULE` lists it.
struct ModuleRow {
    std::string name;
    std::string kind;
    std::size_t bytes = 0;
};

/// What a module file contains.
struct ModuleContent {
    std::string kind;
    std::uint32_t entry = 0x001000;
    std::vector<std::string> script;
    std::vector<std::uint8_t> binary;
    std::vector<std::uint8_t> image;
};

class Modules {
public:
    /// `directory` is the bay. Nothing is created until something is saved, so a
    /// machine with no bay is a machine with an empty one.
    explicit Modules(std::string directory = "modules");

    void setDirectory(std::string directory);
    [[nodiscard]] const std::string& directory() const noexcept { return directory_; }

    /// Every `.mod` file in the bay, sorted by name. A malformed file is listed with
    /// the kind `bad` rather than hidden, because a module that will not load is
    /// something the owner needs to see.
    [[nodiscard]] std::vector<ModuleRow> list() const;

    bool saveScript(const std::string& name, const std::vector<std::string>& lines);
    bool saveBinary(const std::string& name, const std::vector<std::uint8_t>& bytes,
                    std::uint32_t entry = 0x001000);
    bool saveImage(const std::string& name, const std::vector<std::uint8_t>& image);

    /// Reads one module. A malformed one is refused with a sentence and leaves the
    /// machine alone -- the caller has not touched it yet.
    bool read(const std::string& name, ModuleContent* content);

    [[nodiscard]] const std::string& lastError() const noexcept { return error_; }

private:
    [[nodiscard]] std::string pathFor(const std::string& name) const;

    std::string directory_;
    std::string error_;
};

}  // namespace timewarp::hb2
