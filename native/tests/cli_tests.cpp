// SPDX-License-Identifier: MIT
//
// The command-line convention the tools share.
//
// Five tools speak it, so it is worth more than the tools' own confidence: a `--root`
// that one of them silently ignored, or a `--` that swallowed the word after it, is a
// gate that checked the wrong directory and said it passed. These cases pin the
// vocabulary itself.

#include "check.hpp"
#include "timewarp/tools/cli.hpp"

#include <cstdio>
#include <optional>
#include <string>
#include <vector>

namespace {

using timewarp::cli::Invocation;

/// A command line as `main` receives it. `argv[0]` is the tool's name.
class Line {
public:
    explicit Line(std::vector<std::string> words) : words_{std::move(words)} {
        for (std::string& word : words_) {
            pointers_.push_back(word.data());
        }
    }

    [[nodiscard]] Invocation parse(std::string tool = "tool") {
        return timewarp::cli::parse(std::move(tool), static_cast<int>(pointers_.size()),
                                    pointers_.data());
    }

private:
    std::vector<std::string> words_;
    std::vector<char*> pointers_;
};

Invocation parse_line(std::vector<std::string> words) {
    Line line{std::move(words)};
    return line.parse();
}

void test_the_command_is_the_first_word_and_the_rest_are_its_arguments() {
    const Invocation invocation = parse_line({"tool", "claims", "--self-test", "A.md"});
    TW_CHECK_EQ(invocation.command, std::string{"claims"});
    TW_CHECK_EQ(invocation.arguments.size(), std::size_t{2});
    TW_CHECK_EQ(invocation.arguments.at(0), std::string{"--self-test"});
    TW_CHECK_EQ(invocation.arguments.at(1), std::string{"A.md"});
    TW_CHECK(invocation.ok());
}

void test_root_takes_a_word_or_an_equals_form() {
    // The parser makes the root absolute, and on Windows that means the same
    // path on the current drive ("/tmp" becomes "D:\tmp"). The expectation is
    // the same transformation rather than the literal, which is a POSIX answer.
    const std::string expected = timewarp::cli::detail::absolute("/tmp").string();
    const Invocation spaced = parse_line({"tool", "links", "--root", "/tmp"});
    TW_CHECK(spaced.root_given);
    TW_CHECK_EQ(spaced.root.string(), expected);
    TW_CHECK(spaced.arguments.empty());

    const Invocation joined = parse_line({"tool", "links", "--root=/tmp"});
    TW_CHECK(joined.root_given);
    TW_CHECK_EQ(joined.root.string(), expected);
    TW_CHECK(joined.arguments.empty());
}

void test_root_defaults_to_the_working_directory() {
    const Invocation invocation = parse_line({"tool", "links"});
    TW_CHECK(!invocation.root_given);
    TW_CHECK_EQ(invocation.root, std::filesystem::current_path());
}

void test_a_shared_option_is_recognised_after_the_command() {
    const Invocation invocation =
        parse_line({"tool", "md", "--quiet", "docs/A.md", "--json", "out.json"});
    TW_CHECK(invocation.quiet);
    // Made absolute, because a tool that changes directory must still write
    // where the caller asked.
    TW_CHECK_EQ(invocation.json, std::filesystem::current_path() / "out.json");
    TW_CHECK_EQ(invocation.command, std::string{"md"});
    TW_CHECK_EQ(invocation.arguments.size(), std::size_t{1});
    TW_CHECK_EQ(invocation.arguments.at(0), std::string{"docs/A.md"});
}

void test_json_dash_means_stdout() {
    const Invocation invocation = parse_line({"tool", "smoke", "--json", "-"});
    TW_CHECK(invocation.json_stdout);
    TW_CHECK(invocation.json.empty());
    TW_CHECK(invocation.wants_json());
}

void test_a_double_dash_ends_the_options() {
    const Invocation invocation = parse_line({"tool", "check", "--", "--quiet"});
    TW_CHECK(!invocation.quiet);
    TW_CHECK_EQ(invocation.arguments.size(), std::size_t{1});
    TW_CHECK_EQ(invocation.arguments.at(0), std::string{"--quiet"});
}

void test_positionals_skip_a_commands_own_options() {
    const Invocation invocation =
        parse_line({"tool", "plans", "--language", "Logo", "--write", "docs/lessons"});
    const std::vector<std::string> words =
        invocation.positionals({"--language", "--write"});
    TW_CHECK(words.empty());

    const Invocation list = parse_line({"tool", "new", "my-lesson", "--language=Logo"});
    const std::vector<std::string> names = list.positionals({"--language"});
    TW_CHECK(names.size() == 1 && names.at(0) == "my-lesson");
}

void test_a_command_can_read_its_own_option_by_either_spelling() {
    const Invocation spaced = parse_line({"tool", "commands", "--language", "LOGO"});
    const std::optional<std::string> spaced_language = spaced.value("--language");
    TW_CHECK(spaced_language.has_value());
    if (spaced_language.has_value()) {
        TW_CHECK_EQ(*spaced_language, std::string{"LOGO"});
    }
    TW_CHECK(spaced.has("--language"));

    const Invocation joined = parse_line({"tool", "commands", "--language=LOGO"});
    // Bound to a local: `value()` returns an optional by value, and comparing through
    // `*joined.value(...)` reads a temporary that has already gone by the time the
    // assertion runs. AddressSanitizer called it stack-use-after-scope.
    const std::optional<std::string> language = joined.value("--language");
    TW_CHECK(language.has_value());
    if (language.has_value()) {
        TW_CHECK_EQ(*language, std::string{"LOGO"});
    }
    TW_CHECK(!joined.value("--explain").has_value());
}

void test_a_dangling_option_value_is_a_usage_error() {
    const Invocation invocation = parse_line({"tool", "links", "--root"});
    TW_CHECK(!invocation.ok());
    TW_CHECK_EQ(invocation.error, std::string{"--root needs a value"});

    const Invocation empty = parse_line({"tool"});
    TW_CHECK(!empty.ok());
    TW_CHECK_EQ(empty.error, std::string{"no command"});

    const Invocation helped = parse_line({"tool", "--help"});
    TW_CHECK(helped.help);
    TW_CHECK(helped.ok());
}

void test_dispatch_answers_a_known_command() {
    static int ran = 0;
    static const std::string seen = "none";
    const auto run_probe = [](const Invocation& invocation) {
        ran = 1;
        // A command sees the words after it and the shared options already parsed.
        return invocation.arguments.empty() ? 0 : 7;
    };
    static const timewarp::cli::Command commands[] = {
        {"probe", "runs the probe", run_probe},
    };
    const std::span<const timewarp::cli::Command> table{commands};

    const Invocation known = parse_line({"tool", "probe"});
    TW_CHECK_EQ(timewarp::cli::dispatch(known, "[command]", table), 0);
    TW_CHECK_EQ(ran, 1);

    const Invocation helped = parse_line({"tool", "--help"});
    TW_CHECK_EQ(timewarp::cli::dispatch(helped, "[command]", table), 0);

    const Invocation unknown = parse_line({"tool", "nonesuch"});
    TW_CHECK_EQ(timewarp::cli::dispatch(unknown, "[command]", table), 2);

    const Invocation bare = parse_line({"tool"});
    TW_CHECK_EQ(timewarp::cli::dispatch(bare, "[command]", table), 2);
    (void)seen;
}

void test_a_report_states_its_checks_and_escapes_them() {
    const Invocation invocation = parse_line({"tool", "structure", "--json", "-"});
    timewarp::cli::Report report{"tool", invocation};
    report.check("one", true);
    report.check("two", false, "a \"quoted\" reason\nand a newline");
    TW_CHECK(!report.ok());
    TW_CHECK_EQ(report.failures(), std::size_t{1});

    const std::string text = report.render();
    TW_CHECK(text.find("\"tool\":\"tool\"") != std::string::npos);
    TW_CHECK(text.find("\"command\":\"structure\"") != std::string::npos);
    TW_CHECK(text.find("\"ok\":false") != std::string::npos);
    TW_CHECK(text.find("a \\\"quoted\\\" reason\\nand a newline") != std::string::npos);

    timewarp::cli::Report passing{"tool", parse_line({"tool", "smoke"})};
    passing.check("one", true);
    TW_CHECK(passing.ok());
    TW_CHECK(passing.render().find("\"ok\":true") != std::string::npos);
}

void test_a_report_writes_the_file_it_was_given() {
    const std::filesystem::path path =
        std::filesystem::temp_directory_path() / "time_warp_cli_report.json";
    std::filesystem::remove(path);

    const Invocation invocation =
        parse_line({"tool", "tree", "--json", path.string()});
    timewarp::cli::Report report{"tool", invocation};
    report.check("tree holds no Python", true);
    TW_CHECK_EQ(report.finish(), 0);

    const std::string text = [&] {
        std::ifstream file{path, std::ios::binary};
        return std::string{std::istreambuf_iterator<char>{file},
                           std::istreambuf_iterator<char>{}};
    }();
    TW_CHECK(text.find("\"name\":\"tree holds no Python\"") != std::string::npos);
    TW_CHECK(text.ends_with("}\n"));

    std::filesystem::remove(path);
}

void test_a_failing_report_returns_one() {
    const Invocation invocation = parse_line({"tool", "golden", "--quiet"});
    timewarp::cli::Report report{"tool", invocation};
    report.check("the corpus agrees", false, "467 of 470");
    TW_CHECK_EQ(report.finish(), 1);
}

}  // namespace

int main() {
    test_the_command_is_the_first_word_and_the_rest_are_its_arguments();
    test_root_takes_a_word_or_an_equals_form();
    test_root_defaults_to_the_working_directory();
    test_a_shared_option_is_recognised_after_the_command();
    test_json_dash_means_stdout();
    test_a_double_dash_ends_the_options();
    test_positionals_skip_a_commands_own_options();
    test_a_command_can_read_its_own_option_by_either_spelling();
    test_a_dangling_option_value_is_a_usage_error();
    test_dispatch_answers_a_known_command();
    test_a_report_states_its_checks_and_escapes_them();
    test_a_report_writes_the_file_it_was_given();
    test_a_failing_report_returns_one();
    return tw::test::summary("cli");
}
