// SPDX-License-Identifier: MIT
//
// The verifier. See `verify.hpp`: this is the reference's semantics, ported
// rather than reinvented, because the panel and the two commands must agree.

#include "timewarp/lessons/verify.hpp"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstddef>
#include <cstdio>
#include <string_view>

#include "timewarp/graphics/recording_host.hpp"
#include "timewarp/lang/interpreter.hpp"
#include "timewarp/scheme/host.hpp"
#include "timewarp/scheme/machine.hpp"
#include "timewarp/scheme/value.hpp"

namespace timewarp::lessons {
namespace {

/// The commentary the interpreters emit alongside a program's output. A lesson
/// asserts what the program *printed*; "🎨 Graphics mode" is the engine talking.
constexpr std::string_view kStatusPrefixes[] = {"✅", "ℹ️", "🎨", "🚀",
                                                "🐢", "📝", "💡", "📎", "⚠️"};

[[nodiscard]] std::string strip(std::string_view text) {
    const auto is_space = [](char character) {
        return character == ' ' || character == '\t' || character == '\r' ||
               character == '\n';
    };
    std::size_t begin = 0;
    std::size_t end = text.size();
    while (begin < end && is_space(text[begin])) {
        ++begin;
    }
    while (end > begin && is_space(text[end - 1])) {
        --end;
    }
    return std::string{text.substr(begin, end - begin)};
}

[[nodiscard]] bool starts_with_any(std::string_view text) {
    for (const std::string_view prefix : kStatusPrefixes) {
        if (text.rfind(prefix, 0) == 0) {
            return true;
        }
    }
    return false;
}

[[nodiscard]] std::string joined(const std::vector<std::string>& lines,
                                 const char* separator) {
    std::string text;
    for (std::size_t index = 0; index < lines.size(); ++index) {
        if (index > 0) {
            text += separator;
        }
        text += lines[index];
    }
    return text;
}

[[nodiscard]] bool contains_case_insensitive(std::string_view haystack,
                                             std::string_view needle) {
    return std::search(haystack.begin(), haystack.end(), needle.begin(), needle.end(),
                       [](char left, char right) {
                           return std::tolower(static_cast<unsigned char>(left)) ==
                                  std::tolower(static_cast<unsigned char>(right));
                       }) != haystack.end();
}

[[nodiscard]] VerifyResult check_lines(const std::vector<std::string>& expected,
                                       const std::vector<std::string>& output) {
    const std::vector<std::string> printed = printed_lines(output);
    if (expected == printed) {
        return VerifyResult{true, "✅ Checkpoint passed!", output};
    }
    const std::string shown = printed.empty() ? "(nothing)" : joined(printed, "\n");
    return VerifyResult{
        false,
        "Not yet. Expected output:\n" + joined(expected, "\n") +
            "\n\nYour program printed:\n" + shown,
        output};
}

[[nodiscard]] VerifyResult check_turtle(const TurtleExpectation& spec,
                                        const ProgramRun& run) {
    const int drawn = run.segments;
    if (spec.segments.has_value() && drawn != *spec.segments) {
        return VerifyResult{false,
                            "Not yet. The turtle drew " + std::to_string(drawn) +
                                " pen segment(s); this checkpoint needs exactly " +
                                std::to_string(*spec.segments) + ".",
                            run.output};
    }
    if (spec.min_segments.has_value() && drawn < *spec.min_segments) {
        return VerifyResult{false,
                            "Not yet. The turtle drew " + std::to_string(drawn) +
                                " pen segment(s); this checkpoint needs at least " +
                                std::to_string(*spec.min_segments) + ".",
                            run.output};
    }
    const int shapes = run.shapes;
    if (spec.shapes.has_value() && shapes != *spec.shapes) {
        return VerifyResult{false,
                            "Not yet. The program painted " + std::to_string(shapes) +
                                " shape(s); this checkpoint needs exactly " +
                                std::to_string(*spec.shapes) + ".",
                            run.output};
    }
    if (spec.min_shapes.has_value() && shapes < *spec.min_shapes) {
        return VerifyResult{false,
                            "Not yet. The program painted " + std::to_string(shapes) +
                                " shape(s); this checkpoint needs at least " +
                                std::to_string(*spec.min_shapes) + ".",
                            run.output};
    }
    if (spec.returns_home) {
        // The turtle starts at the origin in every language the curriculum
        // teaches, which is what makes "back where it started" checkable.
        const double gap = std::hypot(run.turtle_x, run.turtle_y);
        if (gap > 0.5) {
            char buffer[160];
            std::snprintf(buffer, sizeof buffer,
                          "Not yet. The shape is not closed: the turtle ended %.0f "
                          "steps away from where it started.",
                          gap);
            return VerifyResult{false, buffer, run.output};
        }
    }
    return VerifyResult{true, "✅ Checkpoint passed!", {}};
}

[[nodiscard]] VerifyResult check_error(const std::string& wanted,
                                       const std::vector<std::string>& output) {
    for (const std::string& line : output) {
        if (contains_case_insensitive(line, wanted)) {
            return VerifyResult{true, "✅ Checkpoint passed!", output};
        }
    }
    return VerifyResult{false,
                        "Not yet. This checkpoint needs the program to report:\n" + wanted,
                        output};
}

}  // namespace

std::vector<std::string> printed_lines(const std::vector<std::string>& output) {
    std::string blob;
    for (const std::string& entry : output) {
        blob += entry;
        if (entry.empty() || entry.back() != '\n') {
            blob += '\n';
        }
    }
    std::vector<std::string> lines;
    std::string current;
    for (const char character : blob) {
        if (character == '\n') {
            lines.push_back(current);
            current.clear();
        } else {
            current.push_back(character);
        }
    }
    if (!current.empty()) {
        lines.push_back(current);
    }
    while (!lines.empty() && strip(lines.back()).empty()) {
        lines.pop_back();
    }
    std::vector<std::string> printed;
    for (const std::string& line : lines) {
        const std::string trimmed = strip(line);
        if (!starts_with_any(trimmed)) {
            printed.push_back(trimmed);
        }
    }
    return printed;
}

ProgramRun run_program(lang::Language language, const std::string& source) {
    ProgramRun run;
    timewarp::graphics::RecordingHost host;

    if (language == lang::Language::Scheme) {
        timewarp::scheme::Arena arena;
        timewarp::scheme::Machine machine{arena, host};
        // `execute` renders failures exactly as the executor does, including the
        // "❌ Scheme error:" prefix the verifier looks for.
        const std::string text = machine.execute(source);
        std::string line;
        for (const char character : text) {
            if (character == '\n') {
                run.output.push_back(line);
                line.clear();
            } else {
                line.push_back(character);
            }
        }
        if (!line.empty()) {
            run.output.push_back(line);
        }
    } else {
        lang::Interpreter interpreter{language, host};
        // A lesson program that waits for input cannot be checked without a
        // person sitting there, so the prompt is answered with nothing and
        // reported below rather than left pending.
        bool waited_for_input = false;
        interpreter.set_input_callback([&waited_for_input](const std::string&) {
            waited_for_input = true;
            return std::string{};
        });
        interpreter.load_program(source);
        interpreter.run();
        run.output = interpreter.output();
        if (waited_for_input) {
            run.output.push_back("❌ A lesson program must not wait for keyboard input.");
        }
    }

    run.segments = static_cast<int>(host.segmentCount());
    run.shapes = static_cast<int>(host.shapeCount());
    run.turtle_x = host.turtleX();
    run.turtle_y = host.turtleY();
    return run;
}

VerifyResult verify_checkpoint(const Checkpoint& checkpoint, lang::Language language,
                               const std::string& source) {
    if (strip(source).empty()) {
        return VerifyResult{false, "There is nothing to check yet.", {}};
    }
    const ProgramRun run = run_program(language, source);

    // A checkpoint whose answer *is* an error is checked first: the branch below
    // that reports a broken program would otherwise swallow every correct answer
    // to it.
    if (checkpoint.expect_error.has_value()) {
        return check_error(*checkpoint.expect_error, run.output);
    }

    for (const std::string& line : run.output) {
        if (strip(line).rfind("❌", 0) == 0) {
            return VerifyResult{false, "Your program reported an error:\n" + strip(line),
                                run.output};
        }
    }
    if (checkpoint.expect_lines.has_value()) {
        return check_lines(*checkpoint.expect_lines, run.output);
    }
    if (checkpoint.expect_turtle.has_value()) {
        return check_turtle(*checkpoint.expect_turtle, run);
    }
    return VerifyResult{false, "This checkpoint has no expectation to check against.", {}};
}

}  // namespace timewarp::lessons
