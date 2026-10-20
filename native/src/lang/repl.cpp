// SPDX-License-Identifier: MIT

#include "timewarp/lang/repl.hpp"

#include <cctype>

namespace timewarp::lang {

namespace {

std::string_view trim(const std::string_view text) {
    std::size_t begin = 0;
    while (begin < text.size() &&
           std::isspace(static_cast<unsigned char>(text[begin])) != 0) {
        ++begin;
    }
    std::size_t end = text.size();
    while (end > begin &&
           std::isspace(static_cast<unsigned char>(text[end - 1])) != 0) {
        --end;
    }
    return text.substr(begin, end - begin);
}

bool is_word(const std::string_view text, const std::string_view word) {
    if (text.size() != word.size()) {
        return false;
    }
    for (std::size_t index = 0; index < text.size(); ++index) {
        if (std::toupper(static_cast<unsigned char>(text[index])) !=
            std::toupper(static_cast<unsigned char>(word[index]))) {
            return false;
        }
    }
    return true;
}

}  // namespace

ReplSession::ReplSession(Language language, TurtleHost& turtle)
    : interpreter_{language, turtle} {}

std::string ReplSession::execute(std::string_view submission) {
    // BASIC's NEW clears the workspace. In a REPL the document text is replaced
    // by every submission, so the useful half is the bindings: NEW drops them
    // and acknowledges the command instead of reporting an unknown statement.
    if (interpreter_.language() == Language::Basic &&
        is_word(trim(submission), "NEW")) {
        reset();
        return "✅ BASIC workspace cleared\n";
    }
    return interpreter_.run_snippet(submission);
}

}  // namespace timewarp::lang
