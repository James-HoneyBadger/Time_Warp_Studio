// SPDX-License-Identifier: MIT
//
// The command-line vocabulary the three gates share.
//
// Python had argparse and, in check_doc_links.py, bare sys.argv. Both accepted
// `--name value` and `--name=value`, both stopped at `--`, and both ended at exit
// status 2 when the words did not describe a run; this is that much and no more.

#pragma once

#include <filesystem>
#include <initializer_list>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace docs {

/// One parsed command line: each option by name (an empty value for a flag), and
/// the words that were not options. `error` is empty when parsing succeeded.
struct Arguments {
    std::map<std::string, std::string, std::less<>> options;
    std::vector<std::string> positionals;
    std::string error;

    [[nodiscard]] bool has(std::string_view name) const;
    [[nodiscard]] std::optional<std::string_view> value(std::string_view name) const;
};

/// Parses `argv[first:]`. Options in `value_options` consume the following word
/// unless the value was given as `--name=value`; `flag_options` take none; any
/// other word beginning with `-` is an error, and `--` ends the options. Short
/// options are not clustered, because there is only one and it has a long name.
[[nodiscard]] Arguments parse_arguments(int argc, char** argv, int first,
                                        std::initializer_list<std::string_view> value_options,
                                        std::initializer_list<std::string_view> flag_options);

/// The `--root` the gates check, defaulting to the working directory and made
/// absolute, because that is what the Python's `Path(__file__).resolve()` was: a
/// fixed, fully qualified place to resolve `docs/A.md` against.
[[nodiscard]] std::optional<std::filesystem::path> root_of(const Arguments& arguments,
                                                           std::string& error);

}  // namespace docs
