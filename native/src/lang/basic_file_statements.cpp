// SPDX-License-Identifier: MIT
//
// Files: the confined path resolver, `OPEN`/`CLOSE`, printing to a file, `INPUT #`,
// `LINE INPUT #`, and the record statements.

#include "basic_internal.hpp"
#include "basic_statements_internal.hpp"

#include <algorithm>
#include <cctype>
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <cstddef>
#include <filesystem>
#include <optional>
#include <regex>
#include <string>
#include <string_view>
#include <thread>
#include <utility>
#include <vector>

#include "timewarp/audio/music.hpp"
#include "timewarp/lang/screen_mode.hpp"

namespace timewarp::lang::basic {

std::string resolve_confined_path(std::string_view filename, std::string_view base_dir) {
    // The base is the run's own directory -- a project's root, or the folder the
    // program was opened from -- and only falls back to the process's working
    // directory when no run has named one.
    std::filesystem::path base = base_dir.empty()
                                     ? std::filesystem::current_path()
                                     : std::filesystem::path{std::string{base_dir}};
    std::error_code error;
    const std::filesystem::path resolved_base = std::filesystem::weakly_canonical(base, error);
    if (!error) {
        base = resolved_base;
    }
    const std::string raw = strip_quotes(trim(filename));
    std::filesystem::path candidate = base / raw;
    std::error_code canonical_error;
    const std::filesystem::path canonical =
        std::filesystem::weakly_canonical(candidate, canonical_error);
    if (!canonical_error) {
        candidate = canonical;
    }
    // A prefix test is not a containment test. `/tmp/run` is a prefix of
    // `/tmp/run2`, so `OPEN "../run2/notes.txt"` from `/tmp/run` passed the old
    // check and wrote outside the directory the program was confined to. Compare
    // whole components: the candidate is inside when it *is* the base, or when
    // what follows the base is a separator.
    const std::string base_text = base.generic_string();
    const std::string candidate_text = candidate.generic_string();
    const bool at_base = candidate_text == base_text;
    const bool below_base =
        candidate_text.size() > base_text.size() &&
        candidate_text.compare(0, base_text.size(), base_text) == 0 &&
        (base_text.back() == '/' || candidate_text[base_text.size()] == '/');
    if (!at_base && !below_base) {
        throw BasicError("Path '" + raw + "' escapes sandbox directory");
    }
    return candidate_text;
}

std::string Machine::open_file(const std::string& args) {
    static const std::regex long_form{
        R"(^["']?([^"']+?)["']?\s+FOR\s+(INPUT|OUTPUT|APPEND|RANDOM)\s+AS\s+#?(\d+)\s*$)",
        std::regex::ECMAScript | std::regex::icase};
    static const std::regex short_form{R"(^["']?([IinOoAaRr])["']?\s*,\s*#?(\d+)\s*,\s*["']?([^"']+)["']?\s*$)",
                                       std::regex::ECMAScript};
    const std::string raw = trim(args);
    std::string filename;
    std::string mode;
    long channel = 0;
    std::smatch match;
    if (std::regex_match(raw, match, long_form)) {
        filename = trim(match[1].str());
        mode = upper(match[2].str());
        channel = std::stol(match[3].str());
    } else if (std::regex_match(raw, match, short_form)) {
        const std::string letter = upper(match[1].str());
        channel = std::stol(match[2].str());
        filename = trim(match[3].str());
        mode = letter == "I" ? "INPUT" : (letter == "A" ? "APPEND" : "OUTPUT");
    } else {
        return "❌ OPEN syntax error: expected OPEN \"file\" FOR INPUT|OUTPUT|APPEND AS #n\n";
    }

    const char* file_mode = "r";
    if (mode == "OUTPUT") {
        file_mode = "w";
    } else if (mode == "APPEND") {
        file_mode = "a";
    } else if (mode == "RANDOM") {
        file_mode = "r+";
    }
    try {
        const std::string path = resolve_confined_path(filename, interpreter_.file_base());
        if (mode == "RANDOM" && !std::filesystem::exists(path)) {
            std::FILE* created = std::fopen(path.c_str(), "w");
            if (created != nullptr) {
                std::fclose(created);
            }
        }
        std::FILE* handle = std::fopen(path.c_str(), file_mode);
        if (handle == nullptr) {
            return "❌ OPEN error: cannot open '" + filename + "'\n";
        }
        auto entry = std::make_shared<FileHandle>();
        entry->handle = handle;
        state_.files[static_cast<int>(channel)] = std::move(entry);
        return "📂 Opened file '" + filename + "' (" + mode + ") as #" +
               std::to_string(channel) + "\n";
    } catch (const std::exception& error) {
        return "❌ OPEN error: " + std::string{error.what()} + "\n";
    }
}

std::string Machine::close_files(const std::string& args) {
    const std::string raw = trim(args);
    if (raw.empty()) {
        state_.files.clear();
        return "📂 Closed all files\n";
    }
    std::vector<std::string> closed;
    for (const std::string& field : split_plain(raw, ',')) {
        std::string token = trim(field);
        if (!token.empty() && token.front() == '#') {
            token = trim(token.substr(1));
        }
        if (token.empty()) {
            continue;
        }
        const std::optional<long> channel = parse_integer(token);
        if (!channel.has_value()) {
            return "❌ CLOSE invalid channel: " + trim(field) + "\n";
        }
        state_.files.erase(static_cast<int>(*channel));
        closed.push_back("#" + std::to_string(*channel));
    }
    return "📂 Closed file " + join(closed, ", ") + "\n";
}

std::string Machine::print_to_file(const std::string& args) {
    static const std::regex pattern{R"(^#?(\d+)\s*,\s*(.*)$)", std::regex::ECMAScript};
    std::smatch match;
    const std::string text = trim(args);
    if (!std::regex_match(text, match, pattern)) {
        return "❌ PRINT # requires #channel, expression\n";
    }
    const auto channel = static_cast<int>(std::stol(match[1].str()));
    const auto found = state_.files.find(channel);
    if (found == state_.files.end() || found->second->handle == nullptr) {
        return "❌ File #" + std::to_string(channel) + " is not open\n";
    }
    const std::string rendered = print(match[2].str());
    if (starts_with(rendered, "❌")) {
        return rendered;
    }
    std::fwrite(rendered.data(), 1, rendered.size(), found->second->handle);
    std::fflush(found->second->handle);
    return {};
}

std::string Machine::input_file(const std::string& args) {
    static const std::regex pattern{R"(^#?(\d+)\s*,\s*(.+)$)", std::regex::ECMAScript};
    std::smatch match;
    const std::string text = trim(args);
    if (!std::regex_match(text, match, pattern)) {
        return "❌ INPUT # requires #channel, variable\n";
    }
    const auto channel = static_cast<int>(std::stol(match[1].str()));
    const auto found = state_.files.find(channel);
    if (found == state_.files.end() || found->second->handle == nullptr) {
        return "❌ File #" + std::to_string(channel) + " is not open\n";
    }
    for (const std::string& raw : split_plain(match[2].str(), ',')) {
        const std::string name = upper(trim(raw));
        if (name.empty()) {
            continue;
        }
        std::string line;
        char buffer[4096];
        bool read_any = false;
        while (std::fgets(buffer, sizeof buffer, found->second->handle) != nullptr) {
            line += buffer;
            read_any = true;
            if (!line.empty() && line.back() == '\n') {
                break;
            }
        }
        if (!read_any) {
            return "❌ Input past end of file on #" + std::to_string(channel) + "\n";
        }
        while (!line.empty() && (line.back() == '\n' || line.back() == '\r')) {
            line.pop_back();
        }
        assign(name, line);
    }
    return {};
}

std::string Machine::line_input(const std::string& args) {
    const std::string stripped = trim(args);
    if (!stripped.empty() && stripped.front() == '#') {
        return input_file(stripped);
    }
    return input(stripped);
}

std::string Machine::get_record(const std::string& args) {
    std::vector<std::string> parts;
    for (const std::string& field : split_trimmed(args, ',')) {
        if (!field.empty()) {
            parts.push_back(field);
        }
    }
    if (parts.empty()) {
        return "❌ GET requires #channel\n";
    }
    std::string channel_text = parts[0];
    if (!channel_text.empty() && channel_text.front() == '#') {
        channel_text = channel_text.substr(1);
    }
    const std::optional<long> channel = parse_integer(channel_text);
    if (!channel.has_value()) {
        return "❌ GET requires numeric #channel\n";
    }
    const auto found = state_.files.find(static_cast<int>(*channel));
    if (found == state_.files.end() || found->second->handle == nullptr) {
        return "❌ File #" + std::to_string(*channel) + " is not open\n";
    }
    std::string line;
    char buffer[4096];
    if (std::fgets(buffer, sizeof buffer, found->second->handle) != nullptr) {
        line = buffer;
    }
    while (!line.empty() && (line.back() == '\n' || line.back() == '\r')) {
        line.pop_back();
    }
    if (parts.size() >= 3) {
        assign(upper(parts[2]), line);
    } else {
        interpreter_.set_text("RECORD$", line);
    }
    return "📖 Read record from #" + std::to_string(*channel) + "\n";
}

std::string Machine::put_record(const std::string& args) {
    std::vector<std::string> parts;
    for (const std::string& field : split_trimmed(args, ',')) {
        if (!field.empty()) {
            parts.push_back(field);
        }
    }
    if (parts.empty()) {
        return "❌ PUT requires #channel\n";
    }
    std::string channel_text = parts[0];
    if (!channel_text.empty() && channel_text.front() == '#') {
        channel_text = channel_text.substr(1);
    }
    const std::optional<long> channel = parse_integer(channel_text);
    if (!channel.has_value()) {
        return "❌ PUT requires numeric #channel\n";
    }
    const auto found = state_.files.find(static_cast<int>(*channel));
    if (found == state_.files.end() || found->second->handle == nullptr) {
        return "❌ File #" + std::to_string(*channel) + " is not open\n";
    }
    std::string payload;
    if (parts.size() >= 3) {
        payload = print(parts[2]);
        while (!payload.empty() && payload.back() == '\n') {
            payload.pop_back();
        }
    } else {
        payload = text_of("RECORD$");
    }
    payload += "\n";
    std::fwrite(payload.data(), 1, payload.size(), found->second->handle);
    std::fflush(found->second->handle);
    return "📝 Wrote record to #" + std::to_string(*channel) + "\n";
}

// -- media and hardware ----------------------------------------------------

std::string Machine::sound(const std::string& args) {
    if (upper(trim(args)) == "OFF") {
        turtle_.stop_sound();
        return "🔇 SOUND OFF\n";
    }
    const std::vector<std::string> parts = split_trimmed(args, ',');
    if (parts.size() < 2) {
        return "❌ SOUND requires frequency, duration\n";
    }
    try {
        const auto frequency = static_cast<long>(eval(parts[0]));
        const auto duration = static_cast<long>(eval(parts[1]));
        const auto milliseconds = static_cast<long>(duration * timewarp::audio::kTickMilliseconds);
        // The text is the reference's, so the parity gate still compares equal.
        // The tone is the native build's, and a host without audio drops it.
        turtle_.tone(static_cast<double>(frequency), static_cast<int>(milliseconds),
                     "square");
        return "🔊 SOUND " + std::to_string(frequency) + "Hz for " +
               std::to_string(milliseconds) + "ms\n";
    } catch (const std::exception& error) {
        return "❌ SOUND error: " + std::string{error.what()} + "\n";
    }
}

std::string Machine::key_text() {
    // `INKEY$` is one key: the rest of what was typed stays queued for the next
    // call, which is what a game loop polling every frame expects.
    key_buffer_ = turtle_.take_keystrokes();
    if (key_buffer_.empty()) {
        return {};
    }
    const std::string key(1, key_buffer_.front());
    key_buffer_.erase(0, 1);
    return key;
}

std::string Machine::wait(const std::string& args) {
    try {
        const double milliseconds = eval(args);
        const auto clamped = static_cast<long>(std::clamp(milliseconds, 0.0, 60000.0));
        std::this_thread::sleep_for(std::chrono::milliseconds(clamped));
        return "ℹ️ WAIT " + std::to_string(clamped) + "ms\n";
    } catch (const std::exception& error) {
        return "❌ WAIT error: " + std::string{error.what()} + "\n";
    }
}

}  // namespace timewarp::lang::basic
