// SPDX-License-Identifier: MIT
//
// The confined path resolver, shared by every language that opens a file.

#include "timewarp/lang/file_paths.hpp"

#include <cctype>
#include <filesystem>
#include <system_error>

namespace timewarp::lang {

namespace {

std::string_view trim_view(std::string_view text) {
    while (!text.empty() && std::isspace(static_cast<unsigned char>(text.front())) != 0) {
        text.remove_prefix(1);
    }
    while (!text.empty() && std::isspace(static_cast<unsigned char>(text.back())) != 0) {
        text.remove_suffix(1);
    }
    return text;
}

/// A name as the language wrote it, without the quotes a string literal carries.
std::string_view unquote(std::string_view text) {
    text = trim_view(text);
    if (text.size() >= 2) {
        const char front = text.front();
        const char back = text.back();
        if ((front == '"' && back == '"') || (front == '\'' && back == '\'')) {
            text.remove_prefix(1);
            text.remove_suffix(1);
        }
    }
    return text;
}

}  // namespace

std::optional<std::string> confined_path(std::string_view filename,
                                         std::string_view base_dir) {
    std::filesystem::path base = base_dir.empty()
                                     ? std::filesystem::current_path()
                                     : std::filesystem::path{std::string{base_dir}};
    std::error_code error;
    const std::filesystem::path resolved_base = std::filesystem::weakly_canonical(base, error);
    if (!error) {
        base = resolved_base;
    }
    const std::string raw{unquote(filename)};
    std::filesystem::path candidate = base / raw;
    std::error_code canonical_error;
    const std::filesystem::path canonical =
        std::filesystem::weakly_canonical(candidate, canonical_error);
    if (!canonical_error) {
        candidate = canonical;
    }
    // A prefix test is not a containment test: `/tmp/run` is a prefix of `/tmp/run2`,
    // so comparing text alone let `../run2/notes.txt` out of the directory the
    // program was confined to. Compare whole components instead.
    const std::string base_text = base.generic_string();
    const std::string candidate_text = candidate.generic_string();
    const bool at_base = candidate_text == base_text;
    const bool below_base =
        candidate_text.size() > base_text.size() &&
        candidate_text.compare(0, base_text.size(), base_text) == 0 &&
        (base_text.back() == '/' || candidate_text[base_text.size()] == '/');
    if (!at_base && !below_base) {
        return std::nullopt;
    }
    return candidate_text;
}

}  // namespace timewarp::lang
