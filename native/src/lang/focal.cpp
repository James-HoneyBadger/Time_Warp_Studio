// SPDX-License-Identifier: MIT
//
// FOCAL-69, ported from `time_warp/languages/focal.py`.
//
// Three details are faithful on purpose, because a teaching language that
// quietly differs from the one in the book teaches the wrong thing:
//
//   * Branching is by sign, not by comparison.
//   * DO returns at the end of the program; there is no RETURN.
//   * Undefined variables are 0 -- the BASIC executor does the opposite, and
//     echoing a name back is the failure mode that makes a typo look like a
//     result.
//
// The one deliberate choice the reference documents is field width: `%n` means
// n decimal places, with 2 as the default.

#include "timewarp/lang/focal.hpp"

#include "timewarp/core/charconv.hpp"

#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/mersenne_random.hpp"
#include "timewarp/lang/turtle_host.hpp"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstddef>
#include <cstdlib>
#include <limits>
#include <map>
#include <optional>
#include <iterator>
#include <random>
#include <stdexcept>
#include <system_error>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace timewarp::lang {
namespace {

class FocalError : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

constexpr long kMaxSteps = 1'000'000;
constexpr int kMaxDoDepth = 64;

struct FocalState {
    FocalState() {
        std::random_device device;
        random.seed_unsigned((static_cast<std::uint64_t>(device()) << 32) | device());
    }

    std::map<std::pair<int, int>, std::size_t> labels;
    int format = 2;
    int do_depth = 0;
    long steps = 0;
    MersenneRandom random;
};

bool is_space(char character) {
    return std::isspace(static_cast<unsigned char>(character)) != 0;
}

bool is_digit(char character) {
    return std::isdigit(static_cast<unsigned char>(character)) != 0;
}

std::string trim(std::string_view text) {
    std::size_t start = 0;
    std::size_t end = text.size();
    while (start < end && is_space(text[start])) {
        ++start;
    }
    while (end > start && is_space(text[end - 1])) {
        --end;
    }
    return std::string{text.substr(start, end - start)};
}

std::string upper(std::string_view text) {
    std::string result{text};
    for (char& character : result) {
        character = static_cast<char>(std::toupper(static_cast<unsigned char>(character)));
    }
    return result;
}

/// Python's `repr` for the strings that reach an error message.
std::string quoted_text(std::string_view text) {
    return "'" + std::string{text} + "'";
}

bool is_identifier(std::string_view text) {
    if (text.empty() || !(std::isalpha(static_cast<unsigned char>(text[0])) != 0)) {
        return false;
    }
    return std::all_of(text.begin() + 1, text.end(), [](char character) {
        return std::isalnum(static_cast<unsigned char>(character)) != 0 || character == '_';
    });
}

struct LabelMatch {
    std::string label;
    std::string rest;
};

/// `^\s*(\d{1,2}\.\d{1,2})\s*(.*)$`, exactly as the reference reads a label.
std::optional<LabelMatch> match_label(std::string_view text) {
    std::size_t index = 0;
    while (index < text.size() && is_space(text[index])) {
        ++index;
    }
    const std::size_t group_start = index;
    int group_digits = 0;
    while (index < text.size() && is_digit(text[index]) && group_digits < 2) {
        ++index;
        ++group_digits;
    }
    if (group_digits == 0 || index >= text.size() || text[index] != '.') {
        return std::nullopt;
    }
    const std::string group{text.substr(group_start, index - group_start)};
    ++index;
    const std::size_t member_start = index;
    int member_digits = 0;
    while (index < text.size() && is_digit(text[index]) && member_digits < 2) {
        ++index;
        ++member_digits;
    }
    if (member_digits == 0) {
        return std::nullopt;
    }
    const std::string member{text.substr(member_start, index - member_start)};
    return LabelMatch{group + "." + member, trim(text.substr(index))};
}

std::pair<int, int> label_key(std::string_view label) {
    const std::size_t dot = label.find('.');
    if (dot == std::string_view::npos) {
        return {0, 0};
    }
    return {std::atoi(std::string{label.substr(0, dot)}.c_str()),
            std::atoi(std::string{label.substr(dot + 1)}.c_str())};
}

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

struct Token {
    enum class Kind { Number, Name, Operator };
    Kind kind = Kind::Number;
    std::string text;
};

bool token_starts_number(std::string_view text, std::size_t position) {
    const char character = text[position];
    if (is_digit(character)) {
        return true;
    }
    return character == '.' && position + 1 < text.size() && is_digit(text[position + 1]);
}

/// Consumes a FOCAL number: digits, an optional fraction, an optional exponent.
std::size_t scan_number(std::string_view text, std::size_t start) {
    std::size_t index = start;
    while (index < text.size() && is_digit(text[index])) {
        ++index;
    }
    if (index < text.size() && text[index] == '.') {
        ++index;
        while (index < text.size() && is_digit(text[index])) {
            ++index;
        }
    }
    if (index < text.size() && (text[index] == 'E' || text[index] == 'e')) {
        std::size_t exponent = index + 1;
        if (exponent < text.size() && (text[exponent] == '+' || text[exponent] == '-')) {
            ++exponent;
        }
        if (exponent < text.size() && is_digit(text[exponent])) {
            index = exponent + 1;
            while (index < text.size() && is_digit(text[index])) {
                ++index;
            }
        }
    }
    return index;
}

std::vector<Token> tokenize(std::string_view text) {
    std::vector<Token> tokens;
    std::size_t position = 0;
    while (position < text.size()) {
        if (is_space(text[position])) {
            ++position;
            continue;
        }
        if (token_starts_number(text, position)) {
            const std::size_t end = scan_number(text, position);
            tokens.push_back(Token{Token::Kind::Number, std::string{text.substr(position, end - position)}});
            position = end;
            continue;
        }
        if (std::isalpha(static_cast<unsigned char>(text[position])) != 0) {
            std::size_t end = position + 1;
            while (end < text.size() &&
                   (std::isalnum(static_cast<unsigned char>(text[end])) != 0 ||
                    text[end] == '_')) {
                ++end;
            }
            tokens.push_back(Token{Token::Kind::Name, std::string{text.substr(position, end - position)}});
            position = end;
            continue;
        }
        const char character = text[position];
        if (std::string_view{"+-*/^(),"}.find(character) != std::string_view::npos) {
            tokens.push_back(Token{Token::Kind::Operator, std::string(1, character)});
            ++position;
            continue;
        }
        throw FocalError("unexpected character " + quoted_text(std::string(1, character)));
    }
    return tokens;
}

class Parser {
public:
    Parser(std::vector<Token> tokens, const std::map<std::string, Value>& variables,
           FocalState& state)
        : tokens_{std::move(tokens)}, variables_{variables}, state_{state} {}

    double expression() {
        double value = term();
        for (;;) {
            const Token* token = peek();
            if (token == nullptr || (token->text != "+" && token->text != "-")) {
                return value;
            }
            take();
            const double right = term();
            value = token->text == "+" ? value + right : value - right;
        }
    }

    [[nodiscard]] std::size_t position() const noexcept { return index_; }
    [[nodiscard]] std::size_t size() const noexcept { return tokens_.size(); }

private:
    [[nodiscard]] const Token* peek() const {
        return index_ < tokens_.size() ? &tokens_[index_] : nullptr;
    }

    const Token& take() {
        if (index_ >= tokens_.size()) {
            throw FocalError("expression ended early");
        }
        return tokens_[index_++];
    }

    bool accept(std::string_view value) {
        const Token* token = peek();
        if (token != nullptr && token->text == value) {
            ++index_;
            return true;
        }
        return false;
    }

    double term() {
        double value = power();
        for (;;) {
            const Token* token = peek();
            if (token == nullptr || (token->text != "*" && token->text != "/")) {
                return value;
            }
            take();
            const double right = power();
            if (token->text == "*") {
                value *= right;
            } else {
                if (right == 0.0) {
                    throw FocalError("division by zero");
                }
                value /= right;
            }
        }
    }

    double power() {
        const double value = unary();
        if (accept("^")) {
            // Right-associative, so 2^3^2 is 2^(3^2).
            const double exponent = power();
            if (value == 0.0 && exponent < 0.0) {
                throw FocalError("0.0 cannot be raised to a negative power");
            }
            return std::pow(value, exponent);
        }
        return value;
    }

    double unary() {
        if (accept("-")) {
            return -unary();
        }
        if (accept("+")) {
            return unary();
        }
        return primary();
    }

    double primary() {
        const Token& token = take();
        if (token.kind == Token::Kind::Number) {
            return std::strtod(token.text.c_str(), nullptr);
        }
        if (token.kind == Token::Kind::Operator && token.text == "(") {
            const double value = expression();
            if (!accept(")")) {
                throw FocalError("missing )");
            }
            return value;
        }
        if (token.kind == Token::Kind::Name) {
            const std::string name = upper(token.text);
            if (accept("(")) {
                std::vector<double> arguments;
                if (!accept(")")) {
                    arguments.push_back(expression());
                    while (accept(",")) {
                        arguments.push_back(expression());
                    }
                    if (!accept(")")) {
                        throw FocalError("missing )");
                    }
                }
                return call(name, arguments);
            }
            if (is_no_argument_function(name)) {
                return call(name, {});
            }
            if (is_function(name)) {
                throw FocalError(name + " needs an argument, as in " + name + "(X)");
            }
            // A variable that has never been assigned is zero, as FOCAL's
            // loader leaves it.
            const auto found = variables_.find(name);
            return found == variables_.end() ? 0.0 : found->second.number();
        }
        throw FocalError("unexpected " + quoted_text(token.text) + " in expression");
    }

    static bool is_function(std::string_view name) {
        return name == "FABS" || name == "FSQT" || name == "FEXP" || name == "FLOG" ||
               name == "FSIN" || name == "FCOS" || name == "FATN" || name == "FITR" ||
               name == "FRAN" || name == "FSGN" || name == "FPI";
    }

    static bool is_no_argument_function(std::string_view name) {
        return name == "FPI" || name == "FRAN";
    }

    double call(const std::string& name, const std::vector<double>& arguments) {
        if (!is_function(name)) {
            throw FocalError("unknown function " + name);
        }
        if (is_no_argument_function(name)) {
            if (arguments.empty()) {
                return no_argument(name);
            }
            if (name == "FRAN" && arguments.size() == 1) {
                const double seed = std::trunc(arguments[0]);
                if (std::isnan(seed)) {
                    throw FocalError("cannot convert float NaN to integer");
                }
                if (std::isinf(seed)) {
                    throw FocalError("cannot convert float infinity to integer");
                }
                const double clamped =
                    std::clamp(seed, -9.0e18, 9.0e18);
                state_.random.seed(static_cast<std::int64_t>(clamped));
                return state_.random.random();
            }
            throw FocalError(name + " takes no argument");
        }
        if (arguments.size() != 1) {
            throw FocalError(name + " takes one argument");
        }
        return one_argument(name, arguments[0]);
    }

    double no_argument(const std::string& name) {
        if (name == "FPI") {
            return 3.141592653589793238462643383279502884;
        }
        return state_.random.random();
    }

    double one_argument(const std::string& name, double value) {
        if (name == "FABS") {
            return std::abs(value);
        }
        if (name == "FSQT") {
            if (value < 0.0) {
                throw FocalError(name + ": math domain error");
            }
            return std::sqrt(value);
        }
        if (name == "FEXP") {
            const double result = std::exp(value);
            if (std::isinf(result) && std::isfinite(value)) {
                // Python's math.exp raises OverflowError, which the executor
                // reports without the function prefix.
                throw FocalError("math range error");
            }
            return result;
        }
        if (name == "FLOG") {
            if (!(value > 0.0)) {
                throw FocalError(name + ": math domain error");
            }
            return std::log(value);
        }
        if (name == "FSIN") {
            if (!std::isfinite(value)) {
                throw FocalError(name + ": math domain error");
            }
            return std::sin(value);
        }
        if (name == "FCOS") {
            if (!std::isfinite(value)) {
                throw FocalError(name + ": math domain error");
            }
            return std::cos(value);
        }
        if (name == "FATN") {
            return std::atan(value);
        }
        if (name == "FITR") {
            return std::trunc(value);
        }
        if (name == "FSGN") {
            return (value > 0.0 ? 1.0 : 0.0) - (value < 0.0 ? 1.0 : 0.0);
        }
        throw FocalError("unknown function " + name);
    }

private:
    std::vector<Token> tokens_;
    const std::map<std::string, Value>& variables_;
    /// FRAN's seeded form changes the generator the executor keeps, so the
    /// parser is handed the state rather than owning a private generator.
    FocalState& state_;
    std::size_t index_ = 0;
};

double evaluate(std::string_view text, const std::map<std::string, Value>& variables,
                FocalState& state) {
    Parser parser{tokenize(text), variables, state};
    const double value = parser.expression();
    if (parser.position() != parser.size()) {
        throw FocalError("trailing text in expression: " + quoted_text(trim(text)));
    }
    return value;
}

std::string format_number(double value, int digits) {
    char buffer[4096];
    const int precision = std::max(0, digits);
    const std::size_t length = timewarp::core::format_double_fixed(
        buffer, buffer + sizeof buffer, value, precision);
    if (length == 0) {
        return "?";
    }
    return std::string{buffer, length};
}

// ---------------------------------------------------------------------------
// Output helpers
// ---------------------------------------------------------------------------

std::vector<std::string> split_top_level(std::string_view text, char separator = ',') {
    std::vector<std::string> parts;
    std::string current;
    int depth = 0;
    bool in_string = false;
    for (char character : text) {
        if (character == '"') {
            in_string = !in_string;
        } else if (!in_string) {
            if (character == '(') {
                ++depth;
            } else if (character == ')') {
                --depth;
            } else if (character == separator && depth == 0) {
                parts.push_back(current);
                current.clear();
                continue;
            }
        }
        current.push_back(character);
    }
    parts.push_back(current);
    return parts;
}

/// Splits `"text"X` into a literal and an expression.
std::vector<std::string> split_literals(std::string_view item) {
    std::vector<std::string> pieces;
    std::string current;
    bool in_string = false;
    for (char character : item) {
        if (character == '"') {
            if (in_string) {
                current.push_back(character);
                pieces.push_back(current);
                current.clear();
                in_string = false;
                continue;
            }
            if (!trim(current).empty()) {
                pieces.push_back(current);
            }
            current = "\"";
            in_string = true;
            continue;
        }
        current.push_back(character);
    }
    if (!trim(current).empty()) {
        pieces.push_back(current);
    }
    return pieces;
}

std::string unquote(std::string_view text) {
    const std::string trimmed = trim(text);
    if (trimmed.size() >= 2 && trimmed.front() == '"' && trimmed.back() == '"') {
        return trimmed.substr(1, trimmed.size() - 2);
    }
    if (!trimmed.empty() && trimmed.front() == '"') {
        return trimmed.substr(1);
    }
    return trimmed;
}

void reject_unquoted_word(const std::string& piece, const std::map<std::string, Value>& variables) {
    const std::string word = trim(piece);
    if (!is_identifier(word)) {
        return;
    }
    if (word.size() == 1) {
        return;
    }
    const std::string name = upper(word);
    static const char* const kFunctions[] = {"FABS", "FSQT", "FEXP", "FLOG", "FSIN",
                                             "FCOS", "FATN", "FITR", "FRAN", "FSGN",
                                             "FPI"};
    const bool function = std::any_of(std::begin(kFunctions), std::end(kFunctions),
                                      [&name](const char* candidate) { return name == candidate; });
    if (variables.find(name) != variables.end() || function) {
        return;
    }
    throw FocalError(word + " is not a variable; text needs quotes");
}

// ---------------------------------------------------------------------------
// Statements
// ---------------------------------------------------------------------------

struct StatementResult {
    std::optional<std::size_t> target;
    std::string output;
};

class FocalExecutor {
public:
    FocalExecutor(Interpreter& interpreter, TurtleHost& turtle, FocalState& state)
        : interpreter_{interpreter}, turtle_{turtle}, state_{state} {}

    StatementResult statement(std::string_view text) {
        std::string stripped = trim(text);
        // A DO group reaches this function directly, and a labelled line inside
        // a group would otherwise be read as a command called "02.10".
        if (const auto label = match_label(stripped); label.has_value()) {
            stripped = trim(label->rest);
        }
        if (stripped.empty()) {
            return {};
        }

        // Python's `partition(" ")`: the command ends at the first literal
        // space, not at a tab. A listing that uses a tab is malformed in the
        // reference too, and the error naming the whole token is the useful
        // half of that.
        // `;` separates commands on a line: `S A=1;S B=2` is two statements, and a
        // FOR keeps its own, because its first `;` introduces the body. Before this,
        // a `;` anywhere but inside a FOR reached the expression tokenizer, which
        // reported it as an unexpected character.
        const std::string word_before = upper(stripped.substr(0, stripped.find(' ')));
        if (stripped.find(';') != std::string::npos && alias(word_before) != "FOR" &&
            turtle_action(word_before).empty()) {
            // Only a *top-level* `;` separates statements. The test above finds any of
            // them, which includes the one inside `"; this draws a "`, so the split has
            // to speak for itself: when it finds nothing to split, this is an ordinary
            // statement and recursing on it would not terminate.
            const std::vector<std::string> parts = split_top_level(stripped, ';');
            if (parts.size() > 1) {
                StatementResult combined;
                for (const std::string& part : parts) {
                    if (trim(part).empty()) {
                        continue;
                    }
                    StatementResult piece = statement(part);
                    combined.output += piece.output;
                    if (piece.target.has_value()) {
                        combined.target = piece.target;
                    }
                }
                return combined;
            }
        }

        const std::size_t space = stripped.find(' ');
        const std::string head = stripped.substr(0, space);
        const std::string rest =
            space == std::string::npos ? std::string{} : stripped.substr(space + 1);
        const std::string word = upper(head);

        // A turtle word before the alias table: FORWARD shares a first letter
        // with FOR but is not a loop.
        if (const auto turtle = turtle_action(word); !turtle.empty()) {
            return {std::nullopt, turtle_statement(turtle, rest)};
        }

        const std::string command = alias(word);
        if (command.empty()) {
            throw FocalError("unknown command " + quoted_text(head));
        }
        if (command == "TYPE") {
            return {std::nullopt, type_statement(rest)};
        }
        if (command == "COMMENT") {
            return {};
        }
        if (command == "SET") {
            set_statement(rest);
            return {};
        }
        if (command == "ASK") {
            ask_statement(rest);
            return {};
        }
        if (command == "IF") {
            return {if_statement(rest), ""};
        }
        if (command == "FOR") {
            return {std::nullopt, for_statement(rest)};
        }
        if (command == "GOTO") {
            return {resolve(label_of(rest)), ""};
        }
        if (command == "DO") {
            return {std::nullopt, do_statement(rest)};
        }
        if (command == "ERASE") {
            erase_statement(rest);
            return {};
        }
        if (command == "QUIT") {
            interpreter_.running = false;
            return {};
        }
        throw FocalError("unhandled command " + quoted_text(command));
    }

private:
    static std::string alias(std::string_view word) {
        static const std::map<std::string, std::string> kAliases = {
            {"A", "ASK"},      {"ASK", "ASK"},     {"T", "TYPE"},   {"TYPE", "TYPE"},
            {"S", "SET"},      {"SET", "SET"},     {"I", "IF"},     {"IF", "IF"},
            {"F", "FOR"},      {"FOR", "FOR"},     {"D", "DO"},     {"DO", "DO"},
            {"G", "GOTO"},     {"GOTO", "GOTO"},   {"C", "COMMENT"}, {"COMMENT", "COMMENT"},
            {"E", "ERASE"},    {"ERASE", "ERASE"}, {"Q", "QUIT"},   {"QUIT", "QUIT"},
        };
        const auto found = kAliases.find(std::string{word});
        return found == kAliases.end() ? std::string{} : found->second;
    }

    static std::string turtle_action(std::string_view word) {
        static const std::map<std::string, std::string> kTurtle = {
            {"FORWARD", "forward"}, {"FD", "forward"}, {"BACK", "back"},     {"BK", "back"},
            {"RIGHT", "right"},     {"RT", "right"},   {"LEFT", "left"},     {"LT", "left"},
            {"PENUP", "penup"},     {"PU", "penup"},   {"PENDOWN", "pendown"}, {"PD", "pendown"},
            {"HOME", "home"},
        };
        const auto found = kTurtle.find(std::string{word});
        return found == kTurtle.end() ? std::string{} : found->second;
    }

    std::string turtle_statement(const std::string& action, const std::string& rest) {
        if (action == "home") {
            turtle_.home();
            return {};
        }
        if (action == "penup") {
            turtle_.pen_up();
            return {};
        }
        if (action == "pendown") {
            turtle_.pen_down();
            return {};
        }
        if (rest.empty()) {
            throw FocalError(upper(action) + " needs a distance or angle");
        }
        const double value = evaluate(rest, interpreter_.variables(), state_);
        if (action == "forward") {
            turtle_.forward(value);
        } else if (action == "back") {
            turtle_.back(value);
        } else if (action == "right") {
            turtle_.right(value);
        } else {
            turtle_.left(value);
        }
        return {};
    }

    std::string type_statement(const std::string& arguments) {
        bool newline = true;
        std::string pieces;
        for (const std::string& item : split_top_level(arguments)) {
            for (std::string piece : split_literals(trim(item))) {
                piece = trim(piece);
                if (piece.empty()) {
                    continue;
                }
                if (piece.front() == '"') {
                    pieces += unquote(piece);
                    continue;
                }
                if (piece == "!") {
                    newline = false;
                    continue;
                }
                // %n sets the field for what follows, and prints nothing.
                if (piece.size() >= 1 && piece.size() <= 2 && piece[0] == '%' &&
                    (piece.size() == 1 || is_digit(piece[1]))) {
                    state_.format = piece.size() == 1 ? 0 : piece[1] - '0';
                    continue;
                }
                reject_unquoted_word(piece, interpreter_.variables());
                pieces += format_number(evaluate(piece, interpreter_.variables(), state_),
                                        state_.format);
            }
        }
        return pieces + (newline ? "\n" : "");
    }

    void set_statement(const std::string& arguments) {
        for (const std::string& raw : split_top_level(arguments)) {
            const std::string assignment = trim(raw);
            if (assignment.empty()) {
                continue;
            }
            const std::size_t equals = assignment.find('=');
            if (equals == std::string::npos) {
                throw FocalError("SET needs NAME=value, got " + quoted_text(assignment));
            }
            const std::string key = upper(trim(std::string_view{assignment}.substr(0, equals)));
            if (!is_identifier(key)) {
                throw FocalError("bad variable name " + quoted_text(trim(std::string_view{assignment}.substr(0, equals))));
            }
            const double value = evaluate(std::string_view{assignment}.substr(equals + 1),
                                          interpreter_.variables(), state_);
            interpreter_.set_number(key, value);
        }
    }

    void ask_statement(const std::string& arguments) {
        std::string prompt;
        std::string rest = trim(arguments);
        if (!rest.empty() && rest.front() == '"') {
            const std::size_t end = rest.find('"', 1);
            if (end == std::string::npos) {
                throw FocalError("unterminated prompt");
            }
            prompt = rest.substr(1, end - 1);
            rest = rest.substr(end + 1);
        }
        // A comma is accepted before the name because every other FOCAL
        // statement separates its items with one.
        std::string name = trim(rest);
        if (!name.empty() && name.front() == ',') {
            name = trim(std::string_view{name}.substr(1));
        }
        name = upper(name);
        if (!is_identifier(name)) {
            throw FocalError("ASK needs a variable name, got " + quoted_text(trim(rest)));
        }
        // The answer is written into the variable by request_input, which is
        // why nothing is stored here.
        if (!interpreter_.has_variable(name)) {
            interpreter_.set_number(name, 0.0);
        }
        interpreter_.request_input(prompt, name, true);
    }

    std::optional<std::size_t> if_statement(const std::string& arguments) {
        const std::size_t open = arguments.find('(');
        const std::size_t close = arguments.rfind(')');
        if (open == std::string::npos || close == std::string::npos || close < open) {
            throw FocalError("IF needs a parenthesised expression, e.g. IF (X) 1.1,2.1,3.1");
        }
        const double value =
            evaluate(std::string_view{arguments}.substr(open + 1, close - open - 1),
                     interpreter_.variables(), state_);
        std::vector<std::string> targets;
        for (const std::string& item : split_top_level(std::string_view{arguments}.substr(close + 1))) {
            targets.push_back(trim(item));
        }
        if (targets.size() > 3) {
            throw FocalError("IF takes at most three targets");
        }
        std::string chosen;
        if (value < 0.0) {
            chosen = targets.empty() ? "" : targets[0];
        } else if (value == 0.0) {
            chosen = targets.size() > 1 ? targets[1] : "";
        } else {
            chosen = targets.size() > 2 ? targets[2] : "";
        }
        if (chosen.empty()) {
            return std::nullopt;
        }
        return resolve(label_of(chosen));
    }

    std::string for_statement(const std::string& arguments) {
        const std::size_t equals = arguments.find('=');
        if (equals == std::string::npos) {
            throw FocalError("FOR needs V=values; body");
        }
        const std::string variable = upper(trim(std::string_view{arguments}.substr(0, equals)));
        if (!is_identifier(variable)) {
            throw FocalError("bad FOR variable " + quoted_text(trim(std::string_view{arguments}.substr(0, equals))));
        }
        const std::string remainder{std::string_view{arguments}.substr(equals + 1)};
        const std::size_t semicolon = remainder.find(';');
        if (semicolon == std::string::npos) {
            throw FocalError("FOR needs a ; before its body");
        }
        const std::string head = remainder.substr(0, semicolon);
        const std::string body = remainder.substr(semicolon + 1);
        std::vector<std::string> body_parts;
        for (const std::string& part : split_top_level(body, ';')) {
            if (!trim(part).empty()) {
                body_parts.push_back(part);
            }
        }
        if (body_parts.empty()) {
            throw FocalError("FOR needs a statement after its ;");
        }

        std::vector<double> numbers;
        for (const std::string& part : split_top_level(head)) {
            if (!trim(part).empty()) {
                numbers.push_back(evaluate(part, interpreter_.variables(), state_));
            }
        }
        if (numbers.empty()) {
            throw FocalError("FOR needs at least one value");
        }

        double start = 0.0;
        double step = 1.0;
        std::optional<double> limit;
        if (numbers.size() == 1) {
            start = numbers[0];
        } else if (numbers.size() == 2) {
            start = numbers[0];
            limit = numbers[1];
        } else {
            start = numbers[0];
            step = numbers[1];
            limit = numbers[2];
        }
        if (step == 0.0) {
            throw FocalError("FOR step cannot be zero");
        }

        std::string output;
        double value = start;
        for (;;) {
            ++state_.steps;
            if (state_.steps > kMaxSteps) {
                throw FocalError("FOR loop exceeded the step limit");
            }
            interpreter_.set_number(variable, value);
            for (const std::string& part : body_parts) {
                const StatementResult result = statement(part);
                output += result.output;
                if (result.target.has_value()) {
                    throw FocalError("GOTO inside a FOR body is not supported");
                }
                if (!interpreter_.running) {
                    break;
                }
            }
            if (!interpreter_.running) {
                break;
            }
            if (!limit.has_value()) {
                break;
            }
            value += step;
            if (step > 0.0 && value > *limit) {
                break;
            }
            if (step < 0.0 && value < *limit) {
                break;
            }
        }
        return output;
    }

    std::string do_statement(const std::string& arguments) {
        if (state_.do_depth >= kMaxDoDepth) {
            throw FocalError("DO nested more than " + std::to_string(kMaxDoDepth) + " deep");
        }
        const std::size_t start = resolve(label_of(arguments));
        ++state_.do_depth;
        try {
            const std::string output = run_group(start);
            --state_.do_depth;
            return output;
        } catch (...) {
            // A group that fails still returns: the depth belongs to the call,
            // not to whether it succeeded.
            --state_.do_depth;
            throw;
        }
    }

    std::string run_group(std::size_t start) {
        std::string output;
        std::size_t index = start;
        const std::size_t total = interpreter_.program_lines().size();
        while (index < total && interpreter_.running) {
            ++state_.steps;
            if (state_.steps > kMaxSteps) {
                throw FocalError("execution exceeded the step limit");
            }
            const StatementResult result =
                statement(interpreter_.program_lines()[index].text);
            output += result.output;
            index = result.target.has_value() ? *result.target : index + 1;
        }
        return output;
    }

    void erase_statement(const std::string& arguments) {
        std::string spaced;
        for (char character : arguments) {
            spaced.push_back(character == ',' ? ' ' : character);
        }
        std::vector<std::string> words;
        std::string current;
        for (char character : spaced) {
            if (is_space(character)) {
                if (!current.empty()) {
                    words.push_back(current);
                    current.clear();
                }
            } else {
                current.push_back(character);
            }
        }
        if (!current.empty()) {
            words.push_back(current);
        }
        for (std::string& word : words) {
            word = upper(word);
        }
        if (words.empty()) {
            throw FocalError("ERASE needs ALL or a list of variables");
        }
        if (words[0] == "ALL") {
            interpreter_.variables().clear();
            return;
        }
        for (const std::string& word : words) {
            interpreter_.remove_variable(word);
        }
    }

    [[nodiscard]] static std::string label_of(std::string_view text) {
        for (std::size_t index = 0; index < text.size(); ++index) {
            if (!is_digit(text[index])) {
                continue;
            }
            std::size_t cursor = index;
            int group_digits = 0;
            while (cursor < text.size() && is_digit(text[cursor]) && group_digits < 2) {
                ++cursor;
                ++group_digits;
            }
            if (group_digits == 0 || cursor >= text.size() || text[cursor] != '.') {
                continue;
            }
            ++cursor;
            int member_digits = 0;
            while (cursor < text.size() && is_digit(text[cursor]) && member_digits < 2) {
                ++cursor;
                ++member_digits;
            }
            if (member_digits == 0) {
                continue;
            }
            return std::string{text.substr(index, cursor - index)};
        }
        throw FocalError("expected a line label (NN.NN) in " + quoted_text(trim(text)));
    }

    std::size_t resolve(std::string_view label) {
        build_labels();
        const auto found = state_.labels.find(label_key(label));
        if (found == state_.labels.end()) {
            throw FocalError("no line labelled " + std::string{label});
        }
        return found->second;
    }

public:
    void build_labels() {
        if (!state_.labels.empty()) {
            return;
        }
        for (std::size_t index = 0; index < interpreter_.program_lines().size(); ++index) {
            const std::string& text = interpreter_.program_lines()[index].text;
            if (const auto label = match_label(text); label.has_value()) {
                state_.labels[label_key(label->label)] = index;
            }
        }
    }

private:
    Interpreter& interpreter_;
    TurtleHost& turtle_;
    FocalState& state_;
};

}  // namespace

std::string execute_focal(Interpreter& interpreter, std::string_view source, TurtleHost& turtle) {
    FocalState& state = interpreter.executor_state<FocalState>();
    FocalExecutor executor{interpreter, turtle, state};
    executor.build_labels();
    try {
        const StatementResult result = executor.statement(source);
        if (result.target.has_value()) {
            interpreter.jump_to(*result.target);
        }
        return result.output;
    } catch (const FocalError& error) {
        return "❌ FOCAL: " + std::string{error.what()} + "\n";
    } catch (const std::domain_error& error) {
        return "❌ FOCAL: " + std::string{error.what()} + "\n";
    } catch (const std::overflow_error& error) {
        return "❌ FOCAL: " + std::string{error.what()} + "\n";
    }
}

}  // namespace timewarp::lang
