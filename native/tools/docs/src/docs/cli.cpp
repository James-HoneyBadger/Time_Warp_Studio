// SPDX-License-Identifier: MIT
//
// Command-line parsing for the three gates. See cli.hpp for the shape.

#include "docs/cli.hpp"

#include <algorithm>

namespace docs {
namespace {

bool names(std::initializer_list<std::string_view> list, std::string_view candidate) {
    return std::find(list.begin(), list.end(), candidate) != list.end();
}

/// The key an option is stored under: its name without the leading dashes, which is
/// the name argparse gives it, so `--self-test` is looked up as `self-test`.
std::string stored_name(std::string_view name) {
    while (!name.empty() && name.front() == '-') {
        name.remove_prefix(1);
    }
    return std::string(name);
}

}  // namespace

bool Arguments::has(std::string_view name) const { return options.find(name) != options.end(); }

std::optional<std::string_view> Arguments::value(std::string_view name) const {
    const auto found = options.find(name);
    if (found == options.end()) {
        return std::nullopt;
    }
    return found->second;
}

Arguments parse_arguments(int argc, char** argv, int first,
                          std::initializer_list<std::string_view> value_options,
                          std::initializer_list<std::string_view> flag_options) {
    Arguments result;
    bool options_done = false;
    for (int index = first; index < argc; ++index) {
        const std::string_view word = argv[index];
        if (!options_done && word == "--") {
            options_done = true;
            continue;
        }
        if (options_done || word.size() < 2 || word.front() != '-') {
            result.positionals.emplace_back(word);
            continue;
        }

        std::string_view name = word;
        std::string_view given;
        bool has_given = false;
        if (const std::size_t equals = word.find('='); equals != std::string_view::npos) {
            name = word.substr(0, equals);
            given = word.substr(equals + 1);
            has_given = true;
        }

        if (names(value_options, name)) {
            if (!has_given) {
                if (index + 1 >= argc) {
                    result.error = "option " + std::string(name) + " needs a value";
                    return result;
                }
                given = argv[++index];
            }
            result.options.emplace(stored_name(name), std::string(given));
            continue;
        }
        if (names(flag_options, name)) {
            if (has_given) {
                result.error = "option " + std::string(name) + " takes no value";
                return result;
            }
            result.options.emplace(stored_name(name), std::string());
            continue;
        }
        result.error = "unrecognised option: " + std::string(word);
        return result;
    }
    return result;
}

std::optional<std::filesystem::path> root_of(const Arguments& arguments, std::string& error) {
    std::error_code failure;
    std::filesystem::path root = std::filesystem::current_path(failure);
    if (failure) {
        error = "cannot read the working directory: " + failure.message();
        return std::nullopt;
    }
    if (const auto given = arguments.value("root")) {
        root = std::filesystem::path(std::string(*given));
    }

    std::filesystem::path absolute = std::filesystem::absolute(root, failure);
    if (failure) {
        error = "cannot resolve " + root.string() + ": " + failure.message();
        return std::nullopt;
    }
    // `weakly_canonical` is as close as the standard library gets to Python's
    // `Path.resolve()`: symlinks resolved for the part that exists, the rest made
    // lexical. A caller that passes a path through a symlink gets the same paths in
    // the messages the Python would have printed.
    const std::filesystem::path canonical = std::filesystem::weakly_canonical(absolute, failure);
    if (failure) {
        return absolute.lexically_normal();
    }
    return canonical;
}

}  // namespace docs
