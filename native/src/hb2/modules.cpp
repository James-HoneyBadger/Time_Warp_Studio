// SPDX-License-Identifier: MIT

#include "timewarp/hb2/modules.hpp"

#include <algorithm>
#include <filesystem>
#include <fstream>
#include <string_view>
#include <sstream>

namespace timewarp::hb2 {

namespace {

constexpr std::string_view kMagic = "HBMOD 1";

std::string hexBytes(const std::vector<std::uint8_t>& bytes) {
    static constexpr char kHex[] = "0123456789ABCDEF";
    std::string text;
    text.reserve(bytes.size() * 2);
    for (const std::uint8_t byte : bytes) {
        text.push_back(kHex[byte >> 4]);
        text.push_back(kHex[byte & 0x0F]);
    }
    return text;
}

bool readHex(const std::string& text, std::vector<std::uint8_t>* out) {
    if (text.size() % 2 != 0) {
        return false;
    }
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

std::string baseName(std::string name) {
    if (name.size() > 4 && name.compare(name.size() - 4, 4, ".mod") == 0) {
        name.resize(name.size() - 4);
    }
    return name;
}

}  // namespace

Modules::Modules(std::string directory) : directory_(std::move(directory)) {}

void Modules::setDirectory(std::string directory) { directory_ = std::move(directory); }

std::string Modules::pathFor(const std::string& name) const {
    return directory_ + "/" + baseName(name) + ".mod";
}

std::vector<ModuleRow> Modules::list() const {
    std::vector<ModuleRow> rows;
    std::error_code error;
    const std::filesystem::path directory{directory_};
    if (!std::filesystem::is_directory(directory, error)) {
        return rows;
    }
    for (const auto& entry : std::filesystem::directory_iterator{directory, error}) {
        if (error || !entry.is_regular_file()) {
            continue;
        }
        if (entry.path().extension() != ".mod") {
            continue;
        }
        ModuleRow row;
        row.name = entry.path().stem().string();
        row.bytes = static_cast<std::size_t>(entry.file_size(error));
        // The kind is in the header, so a listing says what a module is before it is
        // booted. A file whose header is not a header is `bad`, not hidden.
        std::ifstream stream{entry.path()};
        std::string header;
        std::getline(stream, header);
        std::istringstream words{header};
        std::string magic;
        std::string version;
        words >> magic >> version >> row.kind;
        if (magic != "HBMOD" || version != "1" || row.kind.empty()) {
            row.kind = "bad";
        }
        rows.push_back(row);
    }
    std::sort(rows.begin(), rows.end(),
              [](const ModuleRow& left, const ModuleRow& right) { return left.name < right.name; });
    return rows;
}

bool Modules::saveScript(const std::string& name, const std::vector<std::string>& lines) {
    std::error_code error;
    std::filesystem::create_directories(directory_, error);
    std::ofstream stream{pathFor(name), std::ios::binary | std::ios::trunc};
    if (!stream) {
        error_ = "?CANNOT WRITE " + baseName(name);
        return false;
    }
    stream << kMagic << " script\n";
    for (const std::string& line : lines) {
        stream << line << "\n";
    }
    error_.clear();
    return true;
}

bool Modules::saveBinary(const std::string& name, const std::vector<std::uint8_t>& bytes,
                         std::uint32_t entry) {
    std::error_code error;
    std::filesystem::create_directories(directory_, error);
    std::ofstream stream{pathFor(name), std::ios::binary | std::ios::trunc};
    if (!stream) {
        error_ = "?CANNOT WRITE " + baseName(name);
        return false;
    }
    stream << kMagic << " binary " << entry << "\n" << hexBytes(bytes) << "\n";
    error_.clear();
    return true;
}

bool Modules::saveImage(const std::string& name, const std::vector<std::uint8_t>& image) {
    std::error_code error;
    std::filesystem::create_directories(directory_, error);
    std::ofstream stream{pathFor(name), std::ios::binary | std::ios::trunc};
    if (!stream) {
        error_ = "?CANNOT WRITE " + baseName(name);
        return false;
    }
    stream << kMagic << " image\n" << hexBytes(image) << "\n";
    error_.clear();
    return true;
}

bool Modules::savePackage(const std::string& name, const std::vector<ModuleProgram>& programs) {
    if (programs.empty()) {
        error_ = "?NOTHING TO PACK: no processes are running";
        return false;
    }
    std::error_code error;
    std::filesystem::create_directories(directory_, error);
    std::ofstream stream{pathFor(name), std::ios::binary | std::ios::trunc};
    if (!stream) {
        error_ = "?CANNOT WRITE " + baseName(name);
        return false;
    }
    stream << kMagic << " package\n";
    for (const ModuleProgram& program : programs) {
        std::istringstream lines{program.source};
        std::vector<std::string> body;
        std::string line;
        while (std::getline(lines, line)) {
            if (!line.empty()) {
                body.push_back(line);
            }
        }
        stream << "PROGRAM " << program.name << ' ' << body.size() << "\n";
        for (const std::string& row : body) {
            stream << row << "\n";
        }
    }
    stream << "END\n";
    error_.clear();
    return true;
}

bool Modules::read(const std::string& name, ModuleContent* content) {
    error_.clear();
    std::ifstream stream{pathFor(name), std::ios::binary};
    if (!stream) {
        error_ = "?MODULE NOT FOUND";
        return false;
    }
    std::string header;
    std::getline(stream, header);
    std::istringstream words{header};
    std::string magic;
    std::string version;
    ModuleContent parsed;
    words >> magic >> version >> parsed.kind;
    if (magic != "HBMOD" || version != "1") {
        error_ = "?NOT A MODULE";
        return false;
    }
    if (parsed.kind == "script") {
        std::string line;
        while (std::getline(stream, line)) {
            if (!line.empty()) {
                parsed.script.push_back(line);
            }
        }
    } else if (parsed.kind == "binary") {
        words >> parsed.entry;
        std::string payload;
        std::getline(stream, payload);
        if (!readHex(payload, &parsed.binary)) {
            error_ = "?MODULE DAMAGED";
            return false;
        }
    } else if (parsed.kind == "image") {
        std::string payload;
        std::getline(stream, payload);
        if (!readHex(payload, &parsed.image)) {
            error_ = "?MODULE DAMAGED";
            return false;
        }
    } else if (parsed.kind == "package") {
        std::string line;
        while (std::getline(stream, line)) {
            if (line == "END") {
                break;
            }
            std::istringstream header_words{line};
            std::string keyword;
            ModuleProgram program;
            std::size_t count = 0;
            header_words >> keyword >> program.name >> count;
            if (keyword != "PROGRAM" || program.name.empty()) {
                error_ = "?MODULE DAMAGED";
                return false;
            }
            for (std::size_t index = 0; index < count; ++index) {
                std::string body;
                if (!std::getline(stream, body)) {
                    error_ = "?MODULE DAMAGED";
                    return false;
                }
                program.source += body;
                program.source.push_back('\n');
            }
            parsed.programs.push_back(std::move(program));
        }
        if (parsed.programs.empty()) {
            error_ = "?MODULE DAMAGED";
            return false;
        }
    } else {
        error_ = "?UNKNOWN MODULE KIND";
        return false;
    }
    *content = std::move(parsed);
    return true;
}

}  // namespace timewarp::hb2
