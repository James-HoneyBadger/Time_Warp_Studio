// SPDX-License-Identifier: MIT

#include "timewarp/lang/interpreter.hpp"

#include "timewarp/expr/errors.hpp"
#include "timewarp/expr/evaluator.hpp"
#include "timewarp/lang/chip8.hpp"
#include "timewarp/lang/focal.hpp"
#include "timewarp/lang/pilot.hpp"

#include <cstdio>

#include <algorithm>
#include <cctype>
#include <chrono>
#include <cmath>
#include <cstdlib>
#include <string>
#include <string_view>

namespace timewarp::lang {
namespace {

bool is_blank(std::string_view text) {
    return std::all_of(text.begin(), text.end(), [](unsigned char character) {
        return std::isspace(character) != 0;
    });
}

std::string rstrip(std::string_view text) {
    std::size_t end = text.size();
    while (end > 0 && std::isspace(static_cast<unsigned char>(text[end - 1])) != 0) {
        --end;
    }
    return std::string{text.substr(0, end)};
}

std::string lstrip(std::string_view text) {
    std::size_t start = 0;
    while (start < text.size() &&
           std::isspace(static_cast<unsigned char>(text[start])) != 0) {
        ++start;
    }
    return std::string{text.substr(start)};
}

std::string strip(std::string_view text) {
    return lstrip(rstrip(text));
}

/// Python's `float(text)`, including the rule that a non-number is zero where
/// the reference coerces typed input.
double python_float_or_zero(std::string_view text) {
    const std::string trimmed = strip(text);
    if (trimmed.empty()) {
        return 0.0;
    }
    char* end = nullptr;
    const double value = std::strtod(trimmed.c_str(), &end);
    if (end == trimmed.c_str()) {
        return 0.0;
    }
    // Python rejects trailing text, and accepts nothing but whitespace after
    // the number. `strtod` stops at the first non-number, which is where we
    // check what it left behind.
    while (end != nullptr && *end != '\0' &&
           std::isspace(static_cast<unsigned char>(*end)) != 0) {
        ++end;
    }
    if (end == nullptr || *end != '\0') {
        return 0.0;
    }
    return value;
}

/// Splits a whole program into lines, treating CRLF and a lone CR as newlines
/// the way Python's `splitlines()` does for the files this engine reads.
std::vector<std::string> split_lines(std::string_view source) {
    std::vector<std::string> lines;
    std::string current;
    for (std::size_t index = 0; index < source.size(); ++index) {
        const char character = source[index];
        if (character == '\r') {
            lines.push_back(current);
            current.clear();
            if (index + 1 < source.size() && source[index + 1] == '\n') {
                ++index;
            }
        } else if (character == '\n') {
            lines.push_back(current);
            current.clear();
        } else {
            current.push_back(character);
        }
    }
    // Python's splitlines() does not invent a final empty line for a trailing
    // newline, but it does return a line for content after the last one.
    if (!current.empty()) {
        lines.push_back(current);
    } else if (!source.empty() && source.back() != '\n' && source.back() != '\r') {
        lines.push_back(current);
    }
    return lines;
}

/// The standard line-number parse: a leading run of digits is a line number.
std::pair<std::optional<int>, std::string> parse_standard_line(std::string_view raw) {
    const std::string line = rstrip(raw);
    std::size_t split = 0;
    while (split < line.size() &&
           std::isspace(static_cast<unsigned char>(line[split])) != 0) {
        ++split;
    }
    const std::size_t number_start = split;
    while (split < line.size() &&
           std::isdigit(static_cast<unsigned char>(line[split])) != 0) {
        ++split;
    }
    if (split > number_start &&
        (split == line.size() ||
         std::isspace(static_cast<unsigned char>(line[split])) != 0)) {
        const int number = std::atoi(std::string{line.substr(number_start, split - number_start)}.c_str());
        return {number, strip(line.substr(split))};
    }
    // Not numbered: the text is the line as written (trailing space removed),
    // because labels and indentation are part of what the executor reads.
    return {std::nullopt, line};
}

}  // namespace

Interpreter::Interpreter(Language language, TurtleHost& turtle)
    : language_{language}, turtle_{turtle} {
    reset();
}

void Interpreter::reset() {
    program_source_.clear();
    program_lines_.clear();
    labels_.clear();
    variables_.clear();
    last_input_.clear();
    subroutine_stack.clear();
    last_match_succeeded = false;
    line_number_map.clear();
    arrays.clear();
    output_.clear();
    executor_state_.reset();
    current_line = 0;
    running = false;
    stop_requested_.store(false, std::memory_order_release);
    started_ = {};
    steps_used_ = 0;
    elapsed_ms_ = 0.0;
    timed_out_ = false;
    iteration_capped_ = false;
    error_.clear();
}

void Interpreter::load_program(std::string_view source) {
    reset();
    program_source_ = std::string{source};
    parse_lines(source);
}

void Interpreter::parse_lines(std::string_view source) {
    const std::vector<std::string> lines = split_lines(source);
    const bool verbatim = is_whole_program(language_) || language_ == Language::Apl;
    program_lines_.reserve(lines.size());
    for (const std::string& line : lines) {
        if (verbatim) {
            // APL lines are expressions and may begin with a number: `2 × 3`
            // is not line 2. Whole-program languages have no numbered lines.
            program_lines_.push_back(ProgramLine{std::nullopt, line});
            continue;
        }
        auto [number, text] = parse_standard_line(line);
        if (number.has_value()) {
            line_number_map[*number] = program_lines_.size();
        }
        const std::string& raw = text;
        if (raw.rfind("L:", 0) == 0) {
            labels_[strip(std::string_view{raw}.substr(2))] = program_lines_.size();
        } else if (!raw.empty() && raw.front() == '*') {
            labels_[strip(std::string_view{raw}.substr(1))] = program_lines_.size();
        }
        program_lines_.push_back(ProgramLine{number, std::move(text)});
    }
}

void Interpreter::run() {
    // A fresh run starts clean. The reference does this on `current_line == 0`,
    // which also makes a resumed run keep the output it has already produced.
    if (current_line == 0) {
        output_.clear();
    }
    error_.clear();
    timed_out_ = false;
    iteration_capped_ = false;
    steps_used_ = 0;
    started_ = std::chrono::steady_clock::now();
    elapsed_ms_ = 0.0;
    running = true;

    const Executor executor = executor_for(language_);
    if (executor == nullptr) {
        // Not an executor bug: the language simply has not been ported, and the
        // message says which one so the caller knows to keep using the bridge.
        log_output(std::string{"❌ Error: "} + std::string{display_name(language_)} +
                   " is not implemented natively yet\n");
        running = false;
        elapsed_ms_ =
            std::chrono::duration<double, std::milli>(std::chrono::steady_clock::now() -
                                                      started_)
                .count();
        return;
    }

    if (is_whole_program(language_)) {
        std::string text;
        try {
            text = executor(*this, program_source_, turtle_);
        } catch (const std::exception& thrown) {
            error_ = thrown.what();
            text = std::string{"❌ Runtime error: "} + thrown.what() + "\n";
        }
        log_output(text);
        running = false;
        elapsed_ms_ =
            std::chrono::duration<double, std::milli>(std::chrono::steady_clock::now() -
                                                      started_)
                .count();
        return;
    }

    std::uint64_t iterations = 0;
    while (current_line < program_lines_.size() && iterations < kMaxIterations) {
        if (!check_budget()) {
            break;
        }

        const std::size_t physical_index = current_line;
        const std::string command = program_lines_[current_line].text;
        if (is_blank(command)) {
            ++current_line;
            continue;
        }

        ++iterations;
        steps_used_ = iterations;
        (void)execute_statement(command);

        if (!running) {
            break;
        }
        // A jump moved the program counter; the loop must not add one as well.
        if (current_line != physical_index) {
            continue;
        }
        ++current_line;
    }

    if (iterations >= kMaxIterations) {
        iteration_capped_ = true;
        log_output("⚠️ Warning: Maximum iterations reached\n");
    }
    running = false;
    elapsed_ms_ =
        std::chrono::duration<double, std::milli>(std::chrono::steady_clock::now() - started_)
            .count();
}

bool Interpreter::execute_statement(std::string_view text) {
    const Executor executor = executor_for(language_);
    if (executor == nullptr) {
        return false;
    }
    std::string out;
    try {
        out = executor(*this, text, turtle_);
    } catch (const std::exception& thrown) {
        // The reference breaks out of this statement and carries on with the
        // next line. Reporting and returning is the same behaviour without
        // losing the run to an exception.
        error_ = thrown.what();
        log_output(std::string{"❌ Error at line "} + std::to_string(display_line()) + ": " +
                   thrown.what() + "\n");
        return false;
    }
    inject_line_number(out);
    log_output(out);
    return true;
}

void Interpreter::inject_line_number(std::string& text) const {
    if (text.empty() || text.find("❌") == std::string::npos) {
        return;
    }
    const std::string display = std::to_string(display_line());
    std::string result;
    std::size_t start = 0;
    while (start <= text.size()) {
        const std::size_t end = text.find('\n', start);
        const std::string line =
            end == std::string::npos ? text.substr(start) : text.substr(start, end - start);
        if (!result.empty()) {
            result.push_back('\n');
        }
        if (line.rfind("❌", 0) == 0 && line.find("[line ") == std::string::npos &&
            line.find("at line ") == std::string::npos) {
            // `line[1:]` in the reference is the rest after the emoji, which is
            // four UTF-8 bytes rather than one character.
            result += "❌ [line " + display + "] " + lstrip(line.substr(std::string_view{"❌"}.size()));
        } else {
            result += line;
        }
        if (end == std::string::npos) {
            break;
        }
        start = end + 1;
    }
    text = std::move(result);
}

void Interpreter::request_stop() noexcept {
    stop_requested_.store(true, std::memory_order_release);
}

bool Interpreter::stop_requested() const noexcept {
    return stop_requested_.load(std::memory_order_acquire);
}

bool Interpreter::check_budget() {
    if (stop_requested_.load(std::memory_order_acquire)) {
        running = false;
        return false;
    }
    if (started_ != std::chrono::steady_clock::time_point{}) {
        const double seconds =
            std::chrono::duration<double>(std::chrono::steady_clock::now() - started_).count();
        if (seconds > kMaxSeconds) {
            timed_out_ = true;
            running = false;
            const std::string message =
                "❌ Error: Execution timeout (" + std::to_string(kMaxSeconds) + "s exceeded)";
            log_output(message);
            return false;
        }
    }
    return true;
}

double Interpreter::number(const std::string& name) const {
    const auto found = variables_.find(name);
    return found == variables_.end() ? 0.0 : found->second.number();
}

bool Interpreter::has_variable(const std::string& name) const {
    return variables_.find(name) != variables_.end();
}

void Interpreter::remove_variable(const std::string& name) {
    variables_.erase(name);
}

void Interpreter::set_number(const std::string& name, double value) {
    variables_[name] = Value{value};
}

void Interpreter::set_text(const std::string& name, std::string value) {
    variables_[name] = Value{std::move(value)};
}

void Interpreter::log_output(std::string_view text) {
    if (text.empty()) {
        return;
    }
    std::size_t start = 0;
    while (start <= text.size()) {
        const std::size_t end = text.find('\n', start);
        const std::string_view line =
            end == std::string_view::npos ? text.substr(start) : text.substr(start, end - start);
        // A trailing newline produces one final empty piece, which is not a
        // line the program printed.
        if (!(end == std::string_view::npos && line.empty())) {
            output_.emplace_back(line);
            if (output_callback_) {
                output_callback_(output_.back());
            }
        }
        if (end == std::string_view::npos) {
            break;
        }
        start = end + 1;
    }
}

void Interpreter::request_input(const std::string& prompt, const std::string& variable,
                                bool numeric) {
    std::string answer;
    if (input_callback_) {
        answer = input_callback_(prompt);
    }
    last_input_ = answer;
    if (numeric) {
        set_number(variable, python_float_or_zero(answer));
    } else {
        set_text(variable, std::move(answer));
    }
}

double Interpreter::evaluate_expression(std::string_view expression) {
    expr::ExpressionEvaluator evaluator;
    for (const auto& [name, value] : variables_) {
        if (value.is_number()) {
            evaluator.set_variable(name, value.number());
        }
    }
    // `NAME(index)` is an array lookup in the expression grammar, so DIM'd
    // arrays have to travel with the variables.
    for (const auto& [name, values] : arrays) {
        evaluator.set_array(name, values);
    }
    // String views must outlive the evaluator, hence the local map. The
    // evaluator only reads them during this call.
    std::map<std::string, std::string> strings;
    for (const auto& [name, value] : variables_) {
        if (value.is_text()) {
            strings.emplace(name, value.text());
        }
    }
    for (const auto& [name, text] : strings) {
        evaluator.set_string_variable(name, text);
    }
    if (function_hook) {
        evaluator.set_function_hook(function_hook);
    }
    try {
        return evaluator.evaluate(expression);
    } catch (const timewarp::expr::ExprError& error) {
        throw ExecutionError(error.what());
    }
}

std::string Interpreter::interpolate_text(std::string_view text) const {
    std::string result{text};
    const auto lookup = [this](const std::string& name) -> std::optional<std::string> {
        const auto exact = variables_.find(name);
        if (exact != variables_.end()) {
            return exact->second.render();
        }
        const auto text_value = variables_.find(name + "$");
        if (text_value != variables_.end() && text_value->second.is_text()) {
            return text_value->second.text();
        }
        return std::nullopt;
    };
    // `#VAR`, case-insensitive.
    for (std::size_t index = 0; index < result.size();) {
        if (result[index] != '#') {
            ++index;
            continue;
        }
        std::size_t end = index + 1;
        if (end < result.size() &&
            (std::isalpha(static_cast<unsigned char>(result[end])) != 0 || result[end] == '_')) {
            ++end;
            while (end < result.size() &&
                   (std::isalnum(static_cast<unsigned char>(result[end])) != 0 ||
                    result[end] == '_')) {
                ++end;
            }
            std::string name = result.substr(index + 1, end - index - 1);
            for (char& character : name) {
                character = static_cast<char>(std::toupper(static_cast<unsigned char>(character)));
            }
            if (const auto value = lookup(name); value.has_value()) {
                result.replace(index, end - index, *value);
                index += value->size();
                continue;
            }
        }
        ++index;
    }
    // `*VAR*`, upper-case names as the reference's pattern requires.
    for (std::size_t index = 0; index < result.size();) {
        if (result[index] != '*') {
            ++index;
            continue;
        }
        std::size_t end = index + 1;
        if (end < result.size() &&
            (std::isupper(static_cast<unsigned char>(result[end])) != 0 || result[end] == '_')) {
            ++end;
            while (end < result.size() &&
                   (std::isupper(static_cast<unsigned char>(result[end])) != 0 ||
                    std::isdigit(static_cast<unsigned char>(result[end])) != 0 ||
                    result[end] == '_')) {
                ++end;
            }
            if (end < result.size() && result[end] == '*') {
                const std::string name = result.substr(index + 1, end - index - 1);
                if (const auto value = lookup(name); value.has_value()) {
                    result.replace(index, end - index + 1, *value);
                    index += value->size();
                    continue;
                }
            }
        }
        ++index;
    }
    return result;
}

void Interpreter::jump_to_label(std::string_view label) {
    std::string name = strip(label);
    if (!name.empty() && name.front() == '*') {
        name = strip(std::string_view{name}.substr(1));
    }
    auto found = labels_.find(name);
    if (found == labels_.end()) {
        std::string upper_name = name;
        for (char& character : upper_name) {
            character = static_cast<char>(std::toupper(static_cast<unsigned char>(character)));
        }
        found = labels_.find(upper_name);
    }
    if (found == labels_.end()) {
        throw std::runtime_error("Label '" + name + "' not found");
    }
    current_line = found->second;
}

double Interpreter::elapsed_ms() const noexcept {
    if (running && started_ != std::chrono::steady_clock::time_point{}) {
        return std::chrono::duration<double, std::milli>(std::chrono::steady_clock::now() -
                                                         started_)
            .count();
    }
    return elapsed_ms_;
}

int Interpreter::source_line(std::size_t index) const {
    if (index < program_lines_.size() && program_lines_[index].number.has_value()) {
        return *program_lines_[index].number;
    }
    return static_cast<int>(index) + 1;
}

int Interpreter::display_line() const {
    return source_line(current_line);
}

}  // namespace timewarp::lang
