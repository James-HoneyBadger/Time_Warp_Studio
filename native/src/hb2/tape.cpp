// SPDX-License-Identifier: MIT

#include "timewarp/hb2/tape.hpp"

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <sstream>
#include <string_view>

namespace timewarp::hb2 {

namespace {

constexpr std::string_view kMagic = "HBTAPE 1";

std::string upper(std::string_view text) {
    std::string out{text};
    std::transform(out.begin(), out.end(), out.begin(), [](unsigned char character) {
        return static_cast<char>(std::toupper(character));
    });
    return out;
}

std::string hex(const std::vector<std::uint8_t>& bytes) {
    static constexpr char kHex[] = "0123456789ABCDEF";
    std::string text;
    text.reserve(bytes.size() * 2);
    for (const std::uint8_t byte : bytes) {
        text.push_back(kHex[byte >> 4]);
        text.push_back(kHex[byte & 0x0F]);
    }
    return text;
}

bool unhex(const std::string& text, std::vector<std::uint8_t>* out) {
    const auto digit = [](char character) -> int {
        if (character >= '0' && character <= '9') {
            return character - '0';
        }
        if (character >= 'A' && character <= 'F') {
            return character - 'A' + 10;
        }
        if (character >= 'a' && character <= 'f') {
            return character - 'a' + 10;
        }
        return -1;
    };
    if (text.size() % 2 != 0) {
        return false;
    }
    out->clear();
    for (std::size_t index = 0; index + 1 < text.size(); index += 2) {
        const int high = digit(text[index]);
        const int low = digit(text[index + 1]);
        if (high < 0 || low < 0) {
            return false;
        }
        out->push_back(static_cast<std::uint8_t>((high << 4) | low));
    }
    return true;
}

}  // namespace

std::uint16_t TapeFile::computeChecksum() const { return Tape::checksumOf(data); }

std::uint16_t Tape::checksumOf(const std::vector<std::uint8_t>& bytes) {
    std::uint32_t sum = 0;
    for (const std::uint8_t byte : bytes) {
        sum = (sum + byte) & 0xFFFF;
    }
    return static_cast<std::uint16_t>(sum);
}

Tape::Tape() = default;

void Tape::clearError() { error_.clear(); }

void Tape::erase() {
    files_.clear();
    head_ = 0;
    error_.clear();
}

bool Tape::record(const std::string& name, const std::vector<std::uint8_t>& bytes) {
    error_.clear();
    std::string stored = upper(name);
    if (stored.empty()) {
        error_ = "?NO NAME";
        return false;
    }
    if (stored.size() > kNameLimit) {
        error_ = "?NAME TOO LONG";
        return false;
    }
    for (const TapeFile& file : files_) {
        if (file.name == stored) {
            // A cassette has no index to rewrite: recording the same name twice would
            // make `CLOAD` depend on which copy the head reached first.
            error_ = "?NAME IN USE";
            return false;
        }
    }
    TapeFile file;
    file.name = std::move(stored);
    file.data = bytes;
    file.checksum = checksumOf(bytes);
    files_.push_back(std::move(file));
    return true;
}

bool Tape::load(const std::string& name, std::vector<std::uint8_t>* bytes) {
    error_.clear();
    const std::string wanted = upper(name);
    // The head only goes forward: anything before it has already gone past.
    for (std::size_t index = 0; index < head_ && index < files_.size(); ++index) {
        if (files_[index].name == wanted) {
            error_ = "?PASSED at " + std::to_string(head_ + 1);
            return false;
        }
    }
    for (std::size_t index = head_; index < files_.size(); ++index) {
        if (files_[index].name != wanted) {
            ++head_;
            continue;
        }
        const TapeFile& file = files_[index];
        if (checksumOf(file.data) != file.checksum) {
            error_ = "?CHECKSUM";
            return false;
        }
        *bytes = file.data;
        ++head_;
        return true;
    }
    error_ = "?NOT FOUND on the tape";
    return false;
}

bool Tape::loadNext(std::vector<std::uint8_t>* bytes, std::string* name) {
    error_.clear();
    if (head_ >= files_.size()) {
        error_ = "?END OF TAPE";
        return false;
    }
    const TapeFile& file = files_[head_];
    if (checksumOf(file.data) != file.checksum) {
        error_ = "?CHECKSUM";
        return false;
    }
    *bytes = file.data;
    if (name != nullptr) {
        *name = file.name;
    }
    ++head_;
    return true;
}

void Tape::rewind() {
    head_ = 0;
    error_.clear();
}

bool Tape::forward() {
    if (head_ >= files_.size()) {
        error_ = "?END OF TAPE";
        return false;
    }
    ++head_;
    return true;
}

std::vector<std::string> Tape::catalog() const {
    std::vector<std::string> rows;
    for (std::size_t index = 0; index < files_.size(); ++index) {
        const TapeFile& file = files_[index];
        std::string row = index + 1 < 10 ? "0" + std::to_string(index + 1)
                                         : std::to_string(index + 1);
        row += "  ";
        row += file.name;
        if (row.size() < 20) {
            row.append(20 - row.size(), ' ');
        }
        row += std::to_string(file.data.size()) + " BYTES";
        row += index == head_ ? "  <HEAD" : (index < head_ ? "  passed" : "");
        rows.push_back(row);
    }
    if (rows.empty()) {
        rows.push_back("the tape is blank");
    }
    return rows;
}

std::string Tape::serialise() const {
    std::ostringstream out;
    out << kMagic << "\n";
    for (const TapeFile& file : files_) {
        out << "FILE " << file.name << " " << file.data.size() << " " << file.checksum << "\n"
            << hex(file.data) << "\n";
    }
    return out.str();
}

bool Tape::deserialise(const std::string& image) {
    error_.clear();
    std::istringstream stream{image};
    std::string header;
    if (!std::getline(stream, header) || header != kMagic) {
        error_ = "?NOT A TAPE";
        return false;
    }
    std::vector<TapeFile> loaded;
    std::string line;
    while (std::getline(stream, line)) {
        if (line.empty()) {
            continue;
        }
        std::istringstream words{line};
        std::string keyword;
        TapeFile file;
        std::size_t length = 0;
        words >> keyword >> file.name >> length >> file.checksum;
        if (keyword != "FILE") {
            error_ = "?TAPE DAMAGED";
            return false;
        }
        std::string payload;
        std::getline(stream, payload);
        if (!unhex(payload, &file.data) || file.data.size() != length) {
            error_ = "?TAPE DAMAGED";
            return false;
        }
        loaded.push_back(std::move(file));
    }
    files_ = std::move(loaded);
    head_ = 0;
    return true;
}

}  // namespace timewarp::hb2
