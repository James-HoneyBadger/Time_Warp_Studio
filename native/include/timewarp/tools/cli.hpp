// SPDX-License-Identifier: MIT
//
// The command-line vocabulary every tool in `native/tools/` speaks.
//
// There are five of them -- `time_warp_gates`, `time_warp_docs`, `time_warp_teach`,
// `time_warp_learn` and the icon generator -- and each grew its own argument loop.
// The loops disagreed: `--root` was `--root DIR` in one and inferred from `argv[0]`
// in another, `--json` took a path in a third and did not exist in the rest, and
// `--help` and an unknown command returned different statuses. A person switching
// between them had to read four usage messages to learn one thing.
//
// So the vocabulary is here, once:
//
//   <tool> <command> [options]        the shape every tool has
//   --root DIR, --root=DIR            the checkout to work on (default: the cwd)
//   --json PATH, --json=PATH          write the machine report to PATH ("-" = stdout)
//   --quiet                           only what failed
//   --help, -h                        the usage message, exit 0
//
// Every tool accepts all of them in any position -- before or after the command --
// and every tool answers a command line it cannot use with status 2 and the usage
// message on stderr, because "you asked wrongly" and "the answer is no" are
// different reports and a script has to tell them apart. Status 1 means the checks
// ran and something failed.
//
// This header links nothing, not even Qt: the documentation gates build in a project
// of their own with no desktop stack, and they are one of the callers.

#pragma once

#include <cstdio>
#include <filesystem>
#include <fstream>
#include <initializer_list>
#include <iostream>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <vector>

#if defined(_WIN32)
#include <io.h>
#else
#include <unistd.h>
#endif

namespace timewarp::cli {

/// One parsed command line.
struct Invocation {
    /// The program's name, for messages: `time_warp_teach`.
    std::string tool;
    /// The subcommand. Empty when none was given, which every tool treats as
    /// "print the usage".
    std::string command;
    /// Everything after the command with the shared options removed, in the order it
    /// was written: a command parses its own options from this.
    std::vector<std::string> arguments;
    /// `--root`, made absolute, or the working directory.
    std::filesystem::path root;
    bool root_given = false;
    /// Where the machine report goes, or empty when `--json` was not given.
    std::filesystem::path json;
    bool json_stdout = false;
    bool quiet = false;
    bool help = false;
    /// Why the line cannot be used. Empty when it can.
    std::string error;

    [[nodiscard]] bool ok() const { return error.empty(); }
    [[nodiscard]] bool wants_json() const { return json_stdout || !json.empty(); }
    /// The words after the command that are not options, in order: a lesson id, a
    /// file, a bundle name. `value_options` names the command's own options that
    /// take a value, so `--language Logo` does not hand `Logo` back as a word.
    [[nodiscard]] std::vector<std::string> positionals(
        std::initializer_list<std::string_view> value_options = {}) const;
    /// Whether a command-specific option was given.
    [[nodiscard]] bool has(std::string_view name) const;
    /// The value of `--name value` or `--name=value`, if it was given.
    [[nodiscard]] std::optional<std::string> value(std::string_view name) const;
};

namespace detail {

[[nodiscard]] inline std::string_view after_equals(std::string_view argument,
                                                   std::string_view name) {
    if (argument.size() > name.size() + 1 && argument.substr(0, name.size()) == name &&
        argument[name.size()] == '=') {
        return argument.substr(name.size() + 1);
    }
    return {};
}

[[nodiscard]] inline std::filesystem::path absolute(const std::string& path) {
    std::error_code ignored;
    const std::filesystem::path made = std::filesystem::absolute(path, ignored);
    return ignored ? std::filesystem::path{path} : made.lexically_normal();
}

[[nodiscard]] inline std::string escape(std::string_view text) {
    std::string out;
    for (const char character : text) {
        switch (character) {
            case '"': out += "\\\""; break;
            case '\\': out += "\\\\"; break;
            case '\n': out += "\\n"; break;
            case '\r': out += "\\r"; break;
            case '\t': out += "\\t"; break;
            default:
                if (static_cast<unsigned char>(character) < 0x20) {
                    char buffer[8];
                    std::snprintf(buffer, sizeof buffer, "\\u%04x",
                                  static_cast<unsigned>(character));
                    out += buffer;
                } else {
                    out += character;
                }
        }
    }
    return out;
}

}  // namespace detail

inline std::vector<std::string> Invocation::positionals(
    std::initializer_list<std::string_view> value_options) const {
    const auto takes_value = [&](std::string_view argument) {
        for (const std::string_view option : value_options) {
            if (argument == option) {
                return true;
            }
        }
        return false;
    };
    std::vector<std::string> words;
    for (std::size_t index = 0; index < arguments.size(); ++index) {
        const std::string& argument = arguments[index];
        if (argument == "--") {
            for (++index; index < arguments.size(); ++index) {
                words.push_back(arguments[index]);
            }
            break;
        }
        if (takes_value(argument)) {
            ++index;
            continue;
        }
        bool inline_value = false;
        for (const std::string_view option : value_options) {
            if (!detail::after_equals(argument, option).empty()) {
                inline_value = true;
                break;
            }
        }
        if (inline_value) {
            continue;
        }
        if (argument.size() > 1 && argument.front() == '-' && argument != "-") {
            continue;
        }
        words.push_back(argument);
    }
    return words;
}

inline bool Invocation::has(std::string_view name) const {
    for (const std::string& argument : arguments) {
        if (argument == name || argument == std::string{name} + "=" ||
            !detail::after_equals(argument, name).empty()) {
            return true;
        }
    }
    return false;
}

inline std::optional<std::string> Invocation::value(std::string_view name) const {
    for (std::size_t index = 0; index < arguments.size(); ++index) {
        if (arguments[index] == name) {
            if (index + 1 < arguments.size()) {
                return arguments[index + 1];
            }
            return std::nullopt;
        }
        const std::string_view inline_value = detail::after_equals(arguments[index], name);
        if (!inline_value.empty()) {
            return std::string{inline_value};
        }
    }
    return std::nullopt;
}

/// Parses `argv[1:]`. `expects_command` is false for a tool that has one job and no
/// subcommands: its words are all arguments, and having none is not an error.
[[nodiscard]] inline Invocation parse(std::string tool, int argc, char** argv,
                                     bool expects_command = true) {
    Invocation invocation;
    invocation.tool = std::move(tool);

    bool options_done = false;
    for (int index = 1; index < argc; ++index) {
        const std::string_view argument{argv[index]};
        if (!options_done && argument == "--") {
            options_done = true;
            continue;
        }
        if (!options_done) {
            const auto take_value = [&](const char* name) -> std::optional<std::string> {
                const std::string_view inline_value = detail::after_equals(argument, name);
                if (!inline_value.empty()) {
                    return std::string{inline_value};
                }
                if (argument == name) {
                    if (index + 1 < argc) {
                        return std::string{argv[++index]};
                    }
                    invocation.error = std::string{name} + " needs a value";
                    return std::nullopt;
                }
                return std::nullopt;
            };

            if (argument == "--help" || argument == "-h") {
                invocation.help = true;
                continue;
            }
            if (argument == "--quiet") {
                invocation.quiet = true;
                continue;
            }
            if (argument == "--root" || argument.starts_with("--root=")) {
                const std::optional<std::string> value = take_value("--root");
                if (!value.has_value()) {
                    return invocation;
                }
                invocation.root = detail::absolute(*value);
                invocation.root_given = true;
                continue;
            }
            if (argument == "--json" || argument.starts_with("--json=")) {
                const std::optional<std::string> value = take_value("--json");
                if (!value.has_value()) {
                    return invocation;
                }
                if (*value == "-") {
                    invocation.json_stdout = true;
                } else {
                    invocation.json = detail::absolute(*value);
                }
                continue;
            }
        }
        if (expects_command && invocation.command.empty() &&
            !(argument.size() > 1 && argument.front() == '-')) {
            invocation.command = std::string{argument};
            continue;
        }
        invocation.arguments.emplace_back(argument);
    }

    if (!invocation.root_given) {
        std::error_code ignored;
        invocation.root = std::filesystem::current_path(ignored);
    }
    if (expects_command && invocation.command.empty() && !invocation.help &&
        invocation.error.empty()) {
        invocation.error = "no command";
    }
    return invocation;
}

/// One subcommand: what it is called, what it answers, and how to run it.
struct Command {
    std::string_view name;
    std::string_view summary;
    int (*run)(const Invocation&);
};

inline void print_usage(std::ostream& stream, const Invocation& invocation,
                        std::string_view synopsis, std::span<const Command> commands) {
    stream << "usage: " << invocation.tool << ' ' << synopsis << '\n';
    stream << "\n  --root DIR    the checkout to work on (default: this directory)\n"
              "  --json PATH   write the machine report to PATH (\"-\" for stdout)\n"
              "  --quiet       only what failed\n"
              "  --help        this message\n";
    if (commands.empty()) {
        return;
    }
    stream << "\ncommands:\n";
    for (const Command& command : commands) {
        stream << "  " << command.name;
        for (std::size_t pad = command.name.size(); pad < 14; ++pad) {
            stream << ' ';
        }
        stream << command.summary << '\n';
    }
}

/// A command line that does not allow an answer: the message, then the usage, then
/// status 2.
inline int usage_error(const Invocation& invocation, std::string_view message,
                       std::string_view synopsis, std::span<const Command> commands) {
    std::cerr << invocation.tool;
    if (!invocation.command.empty()) {
        std::cerr << ' ' << invocation.command;
    }
    std::cerr << ": " << message << '\n';
    print_usage(std::cerr, invocation, synopsis, commands);
    return 2;
}

inline int dispatch(const Invocation& invocation, std::string_view synopsis,
                    std::span<const Command> commands) {
    if (invocation.help) {
        print_usage(std::cout, invocation, synopsis, commands);
        return 0;
    }
    if (!invocation.ok()) {
        return usage_error(invocation, invocation.error, synopsis, commands);
    }
    for (const Command& command : commands) {
        if (command.name == invocation.command) {
            return command.run(invocation);
        }
    }
    return usage_error(invocation, "no such command: " + invocation.command, synopsis,
                       commands);
}

namespace detail {

#if defined(_WIN32)
[[nodiscard]] inline int duplicate_fd(int fd) { return _dup(fd); }
inline int restore_fd(int from, int to) { return _dup2(from, to); }
[[nodiscard]] inline int descriptor_of(FILE* stream) { return _fileno(stream); }
inline int close_fd(int fd) { return _close(fd); }
#else
[[nodiscard]] inline int duplicate_fd(int fd) { return ::dup(fd); }
inline int restore_fd(int from, int to) { return ::dup2(from, to); }
[[nodiscard]] inline int descriptor_of(FILE* stream) { return ::fileno(stream); }
inline int close_fd(int fd) { return ::close(fd); }
#endif

}  // namespace detail

/// Sends everything a command writes to stdout -- `std::cout` and `std::printf`
/// both -- to stderr while it lives, so a machine report written afterwards is the
/// only thing on stdout. A tool that writes its report through `Report` and nothing
/// else does not need this; one whose commands print as they go does.
///
/// It works on the file descriptor rather than on a stream buffer, because a tool
/// that prints with `printf` would otherwise slip past a buffer swap.
class ProseToStderr {
public:
    explicit ProseToStderr(bool active) : active_{active} {
        if (!active_) {
            return;
        }
        flush();
        saved_ = detail::duplicate_fd(detail::descriptor_of(stdout));
        if (saved_ >= 0) {
            detail::restore_fd(detail::descriptor_of(stderr), detail::descriptor_of(stdout));
        }
    }
    ProseToStderr(const ProseToStderr&) = delete;
    ProseToStderr& operator=(const ProseToStderr&) = delete;
    ~ProseToStderr() {
        if (!active_) {
            return;
        }
        flush();
        if (saved_ >= 0) {
            detail::restore_fd(saved_, detail::descriptor_of(stdout));
            detail::close_fd(saved_);
        }
    }

private:
    static void flush() {
        std::cout.flush();
        std::fflush(stdout);
    }

    bool active_ = false;
    int saved_ = -1;
};

/// What a tool found, in the two shapes a caller can want.
///
/// Prose is for a person at a terminal: a note per thing counted, a line per
/// failure, and a tally. `--quiet` drops everything that passed. `--json PATH` writes
/// one object instead, so a script reads the same answer without parsing prose:
///
///     {"tool":"time_warp_gates","command":"structure","ok":false,
///      "checks":[{"name":"...","ok":false,"detail":"..."}]}
class Report {
public:
    Report(std::string tool, const Invocation& invocation)
        : tool_{std::move(tool)},
          command_{invocation.command},
          quiet_{invocation.quiet},
          json_{invocation.json},
          json_stdout_{invocation.json_stdout} {}

    /// One finding. `detail` says why when `ok` is false.
    void check(std::string_view name, bool ok, std::string_view detail = {}) {
        checks_.push_back(Entry{std::string{name}, ok, std::string{detail}});
    }

    /// Something counted or observed that is not a verdict: `69 examples`.
    void note(std::string_view text) { notes_.emplace_back(text); }

    [[nodiscard]] bool ok() const {
        for (const Entry& entry : checks_) {
            if (!entry.ok) {
                return false;
            }
        }
        return true;
    }

    [[nodiscard]] std::size_t failures() const {
        std::size_t count = 0;
        for (const Entry& entry : checks_) {
            if (!entry.ok) {
                ++count;
            }
        }
        return count;
    }

    /// Prints what was found and returns the status: 0 all passed, 1 something
    /// failed, 2 the report could not be written.
    int finish() {
        if (wants_json()) {
            if (!write_json()) {
                return 2;
            }
            return ok() ? 0 : 1;
        }
        std::ostream& out = std::cout;
        if (!quiet_) {
            for (const std::string& note : notes_) {
                out << note << '\n';
            }
        }
        for (const Entry& entry : checks_) {
            if (entry.ok) {
                continue;
            }
            out << "❌ " << entry.name;
            if (!entry.detail.empty()) {
                out << ": " << entry.detail;
            }
            out << '\n';
        }
        if (!quiet_ && !checks_.empty()) {
            if (ok()) {
                out << "✅ " << checks_.size() << " check(s) passed\n";
            } else {
                out << "❌ " << failures() << " of " << checks_.size()
                    << " check(s) failed\n";
            }
        }
        out.flush();
        return ok() ? 0 : 1;
    }

    [[nodiscard]] std::string render() const {
        std::string out = "{\"tool\":\"" + detail::escape(tool_) + "\"";
        out += ",\"command\":\"" + detail::escape(command_) + "\"";
        out += ok() ? ",\"ok\":true" : ",\"ok\":false";
        out += ",\"checks\":[";
        for (std::size_t index = 0; index < checks_.size(); ++index) {
            const Entry& entry = checks_[index];
            if (index != 0) {
                out += ',';
            }
            out += "{\"name\":\"" + detail::escape(entry.name) + "\",\"ok\":";
            out += entry.ok ? "true" : "false";
            out += ",\"detail\":\"" + detail::escape(entry.detail) + "\"}";
        }
        out += "]}";
        return out;
    }

private:
    struct Entry {
        std::string name;
        bool ok = true;
        std::string detail;
    };

    [[nodiscard]] bool wants_json() const { return json_stdout_ || !json_.empty(); }

    [[nodiscard]] bool write_json() const {
        const std::string text = render() + '\n';
        if (json_stdout_) {
            std::cout << text;
            std::cout.flush();
            return true;
        }
        std::ofstream file{json_, std::ios::binary | std::ios::trunc};
        if (!file) {
            std::cerr << tool_ << ": cannot write " << json_.string() << '\n';
            return false;
        }
        file << text;
        return file.good();
    }

    std::string tool_;
    std::string command_;
    bool quiet_ = false;
    std::filesystem::path json_;
    bool json_stdout_ = false;
    std::vector<Entry> checks_;
    std::vector<std::string> notes_;
};

}  // namespace timewarp::cli
