// SPDX-License-Identifier: MIT
//
// REXX, ported from `time_warp/languages/rexx.py`.
//
// REXX is typeless: every value is a string that may happen to be a number, so
// the value model here is one string plus one number and a flag saying which
// one the program last wrote. That is what makes `X = 42` compare numerically
// while `X = '4 2'` does not, without a type declaration anywhere.
//
// The parser is recursive descent with REXX's three precedence levels that look
// unusual: there is no operator precedence between AND/OR, `||` is explicit
// concatenation while *abutting* two terms also concatenates, and comparisons
// turn both sides into numbers when both look numeric.

#include "timewarp/lang/rexx.hpp"

#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/turtle_host.hpp"

#include <algorithm>
#include <cctype>
#include <charconv>
#include <cmath>
#include <cstdlib>
#include <map>
#include <optional>
#include <regex>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace timewarp::lang {
namespace {

class RexxError : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

struct RValue {
    bool text = false;
    double number = 0.0;
    std::string words;

    static RValue numeric(double value) {
        RValue result;
        result.number = value;
        return result;
    }
    static RValue word(std::string value) {
        RValue result;
        result.text = true;
        result.words = std::move(value);
        return result;
    }
};

std::string upper(std::string_view text) {
    std::string result{text};
    for (char& character : result) {
        character =
            static_cast<char>(std::toupper(static_cast<unsigned char>(character)));
    }
    return result;
}

bool is_space(char character) {
    return std::isspace(static_cast<unsigned char>(character)) != 0;
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

bool starts_with(std::string_view text, std::string_view prefix) {
    return text.size() >= prefix.size() && text.substr(0, prefix.size()) == prefix;
}

/// The reference's `repr(float)` is the shortest round-trip form.
std::string float_repr(double value) {
    if (std::isfinite(value) && std::trunc(value) == value &&
        std::fabs(value) < 9.0e15) {
        char buffer[32];
        std::snprintf(buffer, sizeof buffer, "%.0f", value);
        return buffer;
    }
    char buffer[64];
    const auto result =
        std::to_chars(buffer, buffer + sizeof buffer, value, std::chars_format::general);
    if (result.ec == std::errc()) {
        return std::string{buffer, result.ptr};
    }
    std::snprintf(buffer, sizeof buffer, "%g", value);
    return buffer;
}

std::string fmt(const RValue& value) {
    return value.text ? value.words : float_repr(value.number);
}

double num(const RValue& value) {
    if (!value.text) {
        return value.number;
    }
    const std::string text = trim(value.words);
    if (text.empty()) {
        return 0.0;
    }
    char* end = nullptr;
    const double parsed = std::strtod(text.c_str(), &end);
    return end == text.c_str() + text.size() ? parsed : 0.0;
}

bool is_numeric(const std::string& text) {
    const std::string trimmed = trim(text);
    if (trimmed.empty()) {
        return false;
    }
    char* end = nullptr;
    std::strtod(trimmed.c_str(), &end);
    return end == trimmed.c_str() + trimmed.size();
}

bool truth(const RValue& value) {
    const std::string text = trim(fmt(value));
    if (text.empty()) {
        return false;
    }
    if (is_numeric(text)) {
        return num(RValue::word(text)) != 0.0;
    }
    return true;
}

std::vector<std::string> split_ws(const std::string& text) {
    std::vector<std::string> words;
    std::size_t index = 0;
    while (index < text.size()) {
        while (index < text.size() && is_space(text[index])) {
            ++index;
        }
        const std::size_t start = index;
        while (index < text.size() && !is_space(text[index])) {
            ++index;
        }
        if (index > start) {
            words.push_back(text.substr(start, index - start));
        }
    }
    return words;
}

// ---------------------------------------------------------------------------
// Run state
// ---------------------------------------------------------------------------

struct Frame {
    std::string kind;
    std::size_t do_index = 0;
    bool has_do_index = false;
    std::string variable;
    RValue limit;
    RValue step;
    std::string condition;
    long long count = 0;
    std::vector<std::string> items;
    std::size_t index = 0;
    bool matched = false;
};

struct RexxState {
    std::map<std::string, RValue> vars;
    std::vector<Frame> stack;
    std::vector<std::pair<std::size_t, std::vector<RValue>>> call_stack;
    std::optional<std::map<std::string, std::size_t>> labels;
    std::optional<std::string> waiting;
    std::vector<RValue> args;
};

bool is_loop(const std::string& kind) {
    return kind == "while" || kind == "until" || kind == "forever" || kind == "count" ||
           kind == "to" || kind == "over";
}

RValue get_var(Interpreter& interpreter, const std::string& name) {
    RexxState& state = interpreter.executor_state<RexxState>();
    const std::string key = upper(name);
    const auto found = state.vars.find(key);
    if (found != state.vars.end()) {
        return found->second;
    }
    // An uninitialised REXX variable evaluates to its own (upper-cased) name.
    return RValue::word(key);
}

void set_var(Interpreter& interpreter, const std::string& name, const RValue& value) {
    RexxState& state = interpreter.executor_state<RexxState>();
    const std::string key = upper(name);
    state.vars[key] = value;
    if (!value.text) {
        interpreter.set_number(key, value.number);
    }
}

// ---------------------------------------------------------------------------
// Tokenizer
// ---------------------------------------------------------------------------

enum class TokenKind { Number, Text, Symbol, Operator };

struct Token {
    TokenKind kind;
    std::string text;
};

const std::vector<std::string>& block_openers() {
    static const std::vector<std::string> words = {"DO", "SELECT"};
    return words;
}

bool in_words(const std::vector<std::string>& words, std::string_view value) {
    return std::find(words.begin(), words.end(), value) != words.end();
}

const std::vector<std::string>& concat_keywords() {
    static const std::vector<std::string> words = {
        "AND", "OR", "NOT", "THEN", "ELSE", "TO", "BY", "FOR",
        "WHILE", "UNTIL", "OVER", "SELECT", "WHEN", "OTHERWISE"};
    return words;
}

std::vector<Token> tokenize(const std::string& expr) {
    std::vector<Token> tokens;
    std::size_t index = 0;
    while (index < expr.size()) {
        const char character = expr[index];
        if (is_space(character)) {
            ++index;
            continue;
        }
        if (character == '\'' || character == '"') {
            const char quote = character;
            std::size_t cursor = index + 1;
            std::string text;
            while (cursor < expr.size() && expr[cursor] != quote) {
                text.push_back(expr[cursor]);
                ++cursor;
            }
            tokens.push_back({TokenKind::Text, text});
            index = cursor + 1;
            continue;
        }
        if (std::isdigit(static_cast<unsigned char>(character)) != 0 ||
            (character == '.' && index + 1 < expr.size() &&
             std::isdigit(static_cast<unsigned char>(expr[index + 1])) != 0)) {
            std::size_t cursor = index;
            while (cursor < expr.size() &&
                   (std::isdigit(static_cast<unsigned char>(expr[cursor])) != 0 ||
                    expr[cursor] == '.')) {
                ++cursor;
            }
            tokens.push_back({TokenKind::Number, expr.substr(index, cursor - index)});
            index = cursor;
            continue;
        }
        const bool negation = starts_with(expr.substr(index), "\xC2\xAC");
        if (std::isalpha(static_cast<unsigned char>(character)) != 0 ||
            character == '_' || character == '!') {
            std::size_t cursor = index;
            while (cursor < expr.size() &&
                   (std::isalnum(static_cast<unsigned char>(expr[cursor])) != 0 ||
                    expr[cursor] == '_' || expr[cursor] == '!' || expr[cursor] == '.' ||
                    expr[cursor] == '?')) {
                ++cursor;
            }
            tokens.push_back({TokenKind::Symbol, expr.substr(index, cursor - index)});
            index = cursor;
            continue;
        }
        if (character == '(' || character == ')' || character == ',') {
            tokens.push_back({TokenKind::Operator, std::string(1, character)});
            ++index;
            continue;
        }
        if (negation && index + 3 <= expr.size() && expr[index + 2] == '=') {
            tokens.push_back({TokenKind::Operator, "\\="});
            index += 3;
            continue;
        }
        if (negation) {
            tokens.push_back({TokenKind::Operator, "\\"});
            index += 2;
            continue;
        }
        const std::string two = expr.substr(index, 2);
        if (two == "<=" || two == ">=" || two == "<>" || two == "==" || two == "//" ||
            two == "**" || two == "&&" || two == "||" || two == "\\=" ||
            two == "\\<" || two == "\\>") {
            tokens.push_back({TokenKind::Operator, two});
            index += 2;
            continue;
        }
        if (std::string("+-*/%&|<>=¬\\").find(character) != std::string::npos) {
            tokens.push_back({TokenKind::Operator, std::string(1, character)});
            ++index;
            continue;
        }
        throw RexxError("unexpected character '" + std::string(1, character) +
                        "' in expression");
    }
    return tokens;
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

RValue call_builtin(Interpreter& interpreter, const std::string& name,
                    const std::vector<RValue>& args);

class Parser {
public:
    Parser(Interpreter& interpreter, std::vector<Token> tokens)
        : interpreter_{interpreter}, tokens_{std::move(tokens)} {}

    RValue parse() {
        RValue value = or_expr();
        if (position_ != tokens_.size()) {
            throw RexxError("unexpected token '" + tokens_[position_].text +
                            "' in expression");
        }
        return value;
    }

private:
    const Token* peek() const {
        return position_ < tokens_.size() ? &tokens_[position_] : nullptr;
    }
    Token next() { return tokens_[position_++]; }

    bool accept_op(std::string_view op) {
        const Token* token = peek();
        if (token != nullptr && token->kind == TokenKind::Operator && token->text == op) {
            ++position_;
            return true;
        }
        return false;
    }
    bool at_keyword(const std::string& word) const {
        const Token* token = peek();
        if (token == nullptr || token->kind != TokenKind::Symbol ||
            upper(token->text) != word) {
            return false;
        }
        const Token* after =
            position_ + 1 < tokens_.size() ? &tokens_[position_ + 1] : nullptr;
        return after == nullptr || !(after->kind == TokenKind::Operator &&
                                     after->text == "(");
    }
    bool accept_keyword(const std::string& word) {
        if (at_keyword(word)) {
            ++position_;
            return true;
        }
        return false;
    }

    RValue or_expr() {
        RValue left = and_expr();
        while (true) {
            if (accept_op("|")) {
                // fall through to the operand below
            } else if (accept_keyword("OR")) {
                // as above
            } else {
                break;
            }
            const RValue right = and_expr();
            left = RValue::numeric((truth(left) || truth(right)) ? 1 : 0);
        }
        return left;
    }

    RValue and_expr() {
        RValue left = compare_expr();
        while (true) {
            bool matched = accept_op("&");
            if (!matched) {
                matched = accept_op("&&");
            }
            if (!matched) {
                matched = accept_keyword("AND");
            }
            if (!matched) {
                break;
            }
            const RValue right = compare_expr();
            left = RValue::numeric((truth(left) && truth(right)) ? 1 : 0);
        }
        return left;
    }

    RValue compare_expr() {
        RValue left = concat_expr();
        while (true) {
            const Token* token = peek();
            if (token == nullptr || token->kind != TokenKind::Operator) {
                return left;
            }
            const std::string op = token->text;
            if (op != "=" && op != "==" && op != "<>" && op != "\\=" && op != "<" &&
                op != ">" && op != "<=" && op != ">=" && op != "\\<" && op != "\\>") {
                return left;
            }
            ++position_;
            const RValue right = concat_expr();
            left = RValue::numeric(compare(op, left, right) ? 1 : 0);
        }
    }

    RValue concat_expr() {
        RValue left = add_expr();
        while (true) {
            const Token* token = peek();
            if (token == nullptr) {
                return left;
            }
            if (token->kind == TokenKind::Operator && token->text == "||") {
                ++position_;
                left = RValue::word(fmt(left) + fmt(add_expr()));
                continue;
            }
            const bool term =
                token->kind == TokenKind::Number || token->kind == TokenKind::Text ||
                (token->kind == TokenKind::Operator && token->text == "(") ||
                (token->kind == TokenKind::Symbol &&
                 !in_words(concat_keywords(), upper(token->text)));
            if (term) {  // REXX abuttal concatenation
                left = RValue::word(fmt(left) + fmt(add_expr()));
                continue;
            }
            return left;
        }
    }

    RValue add_expr() {
        RValue left = mul_expr();
        while (true) {
            const Token* token = peek();
            if (token != nullptr && token->kind == TokenKind::Operator &&
                (token->text == "+" || token->text == "-")) {
                ++position_;
                const RValue right = mul_expr();
                left = RValue::numeric(token->text == "+" ? num(left) + num(right)
                                                          : num(left) - num(right));
            } else {
                return left;
            }
        }
    }

    RValue mul_expr() {
        RValue left = pow_expr();
        while (true) {
            const Token* token = peek();
            if (token == nullptr || token->kind != TokenKind::Operator ||
                (token->text != "*" && token->text != "/" && token->text != "%" &&
                 token->text != "//")) {
                return left;
            }
            ++position_;
            const RValue right = pow_expr();
            const double a = num(left);
            const double b = num(right);
            if (b == 0.0 && (token->text == "/" || token->text == "%" ||
                             token->text == "//")) {
                throw RexxError("division by zero");
            }
            double value = 0.0;
            if (token->text == "*") {
                value = a * b;
            } else if (token->text == "/") {
                value = a / b;
            } else if (token->text == "%") {
                value = std::trunc(a / b);
            } else {
                value = a - b * std::trunc(a / b);
            }
            left = RValue::numeric(value);
        }
    }

    RValue pow_expr() {
        RValue base = unary();
        if (accept_op("**")) {
            return RValue::numeric(std::pow(num(base), num(pow_expr())));
        }
        return base;
    }

    RValue unary() {
        const Token* token = peek();
        if (token != nullptr && token->kind == TokenKind::Operator &&
            (token->text == "-" || token->text == "+" || token->text == "\\" ||
             token->text == "¬")) {
            ++position_;
            const RValue value = unary();
            if (token->text == "-") {
                return RValue::numeric(-num(value));
            }
            if (token->text == "+") {
                return RValue::numeric(num(value));
            }
            return RValue::numeric(truth(value) ? 0 : 1);
        }
        return primary();
    }

    RValue primary() {
        if (position_ >= tokens_.size()) {
            throw RexxError("unexpected end of expression");
        }
        const Token token = next();
        if (token.kind == TokenKind::Number) {
            return RValue::numeric(std::strtod(token.text.c_str(), nullptr));
        }
        if (token.kind == TokenKind::Text) {
            return RValue::word(token.text);
        }
        if (token.kind == TokenKind::Operator && token.text == "(") {
            const RValue value = or_expr();
            if (position_ >= tokens_.size() || tokens_[position_].text != ")") {
                throw RexxError("missing closing parenthesis");
            }
            ++position_;
            return value;
        }
        if (token.kind == TokenKind::Symbol) {
            if (peek() != nullptr && peek()->kind == TokenKind::Operator &&
                peek()->text == "(") {
                ++position_;
                std::vector<RValue> arguments;
                if (!(peek() != nullptr && peek()->text == ")")) {
                    arguments.push_back(or_expr());
                    while (accept_op(",")) {
                        arguments.push_back(or_expr());
                    }
                }
                if (position_ >= tokens_.size() || tokens_[position_].text != ")") {
                    throw RexxError("missing closing parenthesis in call");
                }
                ++position_;
                return call_builtin(interpreter_, upper(token.text), arguments);
            }
            return get_var(interpreter_, token.text);
        }
        throw RexxError("unexpected token '" + token.text + "'");
    }

    static bool compare(const std::string& op, const RValue& left, const RValue& right) {
        const std::string ls = fmt(left);
        const std::string rs = fmt(right);
        const bool numeric = is_numeric(ls) && is_numeric(rs);
        const double ln = num(left);
        const double rn = num(right);
        if (op == "=" || op == "==") {
            return numeric ? ln == rn : ls == rs;
        }
        if (op == "<>" || op == "\\=") {
            return numeric ? ln != rn : ls != rs;
        }
        if (op == "<") {
            return numeric ? ln < rn : ls < rs;
        }
        if (op == ">") {
            return numeric ? ln > rn : ls > rs;
        }
        if (op == "<=") {
            return numeric ? ln <= rn : ls <= rs;
        }
        if (op == ">=") {
            return numeric ? ln >= rn : ls >= rs;
        }
        if (op == "\\<") {  // not less than => greater or equal
            return numeric ? ln >= rn : ls >= rs;
        }
        if (op == "\\>") {  // not greater than => less or equal
            return numeric ? ln <= rn : ls <= rs;
        }
        return false;
    }

    Interpreter& interpreter_;
    std::vector<Token> tokens_;
    std::size_t position_ = 0;
};

RValue eval_expr(Interpreter& interpreter, const std::string& expression) {
    return Parser{interpreter, tokenize(expression)}.parse();
}

// ---------------------------------------------------------------------------
// Line helpers
// ---------------------------------------------------------------------------

std::string strip_comment(const std::string& line) {
    std::string result;
    bool in_string = false;
    char quote = 0;
    std::size_t index = 0;
    while (index < line.size()) {
        if (in_string) {
            result.push_back(line[index]);
            if (line[index] == quote) {
                in_string = false;
            }
            ++index;
            continue;
        }
        if (line[index] == '\'' || line[index] == '"') {
            in_string = true;
            quote = line[index];
            result.push_back(line[index]);
            ++index;
            continue;
        }
        if (line[index] == '/' && index + 1 < line.size() && line[index + 1] == '*') {
            const std::size_t close = line.find("*/", index + 2);
            if (close == std::string::npos) {
                break;  // an unterminated comment runs to the end of the line
            }
            result.push_back(' ');
            index = close + 2;
            continue;
        }
        result.push_back(line[index]);
        ++index;
    }
    const std::string trimmed = trim(result);
    if (starts_with(trimmed, "--")) {
        return {};
    }
    return result;
}

std::vector<std::string> split_statements(const std::string& line) {
    std::vector<std::string> parts;
    int depth = 0;
    bool in_string = false;
    char quote = 0;
    std::string current;
    for (char character : line) {
        if (in_string) {
            current.push_back(character);
            if (character == quote) {
                in_string = false;
            }
            continue;
        }
        if (character == '\'' || character == '"') {
            in_string = true;
            quote = character;
            current.push_back(character);
            continue;
        }
        if (character == '(') {
            ++depth;
        } else if (character == ')') {
            --depth;
        }
        if (character == ';' && depth == 0) {
            parts.push_back(trim(current));
            current.clear();
            continue;
        }
        current.push_back(character);
    }
    const std::string tail = trim(current);
    if (!tail.empty()) {
        parts.push_back(tail);
    }
    std::vector<std::string> result;
    for (const std::string& part : parts) {
        if (!part.empty()) {
            result.push_back(part);
        }
    }
    return result;
}

std::vector<std::string> split_args(const std::string& text) {
    std::vector<std::string> parts;
    if (trim(text).empty()) {
        return parts;
    }
    int depth = 0;
    bool in_string = false;
    char quote = 0;
    std::string current;
    for (char character : text) {
        if (in_string) {
            current.push_back(character);
            if (character == quote) {
                in_string = false;
            }
            continue;
        }
        if (character == '\'' || character == '"') {
            in_string = true;
            quote = character;
            current.push_back(character);
            continue;
        }
        if (character == '(') {
            ++depth;
        } else if (character == ')') {
            --depth;
        }
        if (character == ',' && depth == 0) {
            parts.push_back(trim(current));
            current.clear();
            continue;
        }
        current.push_back(character);
    }
    const std::string tail = trim(current);
    if (!tail.empty()) {
        parts.push_back(tail);
    }
    std::vector<std::string> result;
    for (const std::string& part : parts) {
        if (!part.empty()) {
            result.push_back(part);
        }
    }
    return result;
}

std::vector<std::string> code_words(const std::string& line) {
    std::vector<std::string> words;
    bool in_string = false;
    char quote = 0;
    std::string current;
    for (char character : line) {
        if (in_string) {
            if (character == quote) {
                in_string = false;
            }
            continue;
        }
        if (character == '\'' || character == '"') {
            in_string = true;
            quote = character;
            if (!current.empty()) {
                words.push_back(current);
                current.clear();
            }
            continue;
        }
        if (std::isalnum(static_cast<unsigned char>(character)) != 0 ||
            character == '_') {
            current.push_back(character);
        } else if (!current.empty()) {
            words.push_back(current);
            current.clear();
        }
    }
    if (!current.empty()) {
        words.push_back(current);
    }
    return words;
}

std::size_t scan_block_end(Interpreter& interpreter, std::size_t start) {
    int depth = 1;
    std::size_t index = start + 1;
    const auto& lines = interpreter.program_lines();
    while (index < lines.size()) {
        for (const std::string& word : code_words(strip_comment(lines[index].text))) {
            const std::string up = upper(word);
            if (in_words(block_openers(), up)) {
                ++depth;
            } else if (up == "END") {
                --depth;
            }
        }
        if (depth <= 0) {
            return index;
        }
        ++index;
    }
    throw RexxError("missing END for block");
}

const std::regex& label_pattern() {
    static const std::regex pattern{R"(^\s*([A-Za-z_!?][A-Za-z0-9_!?.]*)\s*:\s*(.*)$)",
                                    std::regex::ECMAScript};
    return pattern;
}

std::map<std::string, std::size_t> labels(Interpreter& interpreter) {
    RexxState& state = interpreter.executor_state<RexxState>();
    if (!state.labels.has_value()) {
        std::map<std::string, std::size_t> found;
        const auto& lines = interpreter.program_lines();
        for (std::size_t index = 0; index < lines.size(); ++index) {
            std::smatch match;
            const std::string text = strip_comment(lines[index].text);
            if (std::regex_match(text, match, label_pattern())) {
                found.emplace(upper(match[1].str()), index);
            }
        }
        state.labels = std::move(found);
    }
    return *state.labels;
}

std::string statement_after_label(const std::string& text) {
    std::smatch match;
    if (std::regex_match(text, match, label_pattern())) {
        return trim(match[2].str());
    }
    return trim(text);
}

// ---------------------------------------------------------------------------
// Executor
// ---------------------------------------------------------------------------

enum class Flow { None, Jump };

struct RexxMachine {
    Interpreter& interpreter;
    TurtleHost& turtle;
    RexxState& state;
    std::vector<std::string>& output;

    Flow exec_statement(const std::string& statement);
    std::optional<Flow> handle_flow(const std::string& text, const std::string& shouted);
    Flow handle_pull(const std::string& text);
    Flow handle_parse_var(const std::string& body);
    Flow handle_if(const std::string& text);
    Flow after_block(std::size_t block_end);
    Flow handle_do(const std::string& text);
    Flow handle_end();
    Flow skip_else();
    Frame* find_select();
    Flow handle_when(const std::string& text);
    Flow handle_otherwise(const std::string& text);
    Flow handle_call(const std::string& name);
    Flow handle_return(const std::string& text);
    std::optional<std::size_t> nearest_loop();
    Flow handle_leave();
    Flow handle_iterate();
    bool handle_turtle(const std::string& shouted, const std::string& text);
    void sync_pending_input();
    Flow run_statements(const std::string& text);
};

void RexxMachine::sync_pending_input() {
    if (!state.waiting.has_value()) {
        return;
    }
    const std::string name = upper(*state.waiting);
    state.waiting.reset();
    std::string stored;
    const auto found = interpreter.variables().find(name + "$");
    if (found != interpreter.variables().end()) {
        stored = found->second.is_text() ? found->second.text() : found->second.render();
    } else {
        const auto number = interpreter.variables().find(name);
        stored = number != interpreter.variables().end() ? number->second.render() : "";
    }
    set_var(interpreter, name, RValue::word(stored));
}

Flow RexxMachine::run_statements(const std::string& text) {
    for (const std::string& statement : split_statements(statement_after_label(text))) {
        Flow flow = Flow::None;
        try {
            flow = exec_statement(statement);
        } catch (const RexxError& error) {
            output.push_back(std::string{"❌ REXX error: "} + error.what());
            break;
        }
        if (flow == Flow::Jump) {
            break;
        }
    }
    return Flow::None;
}

Flow RexxMachine::exec_statement(const std::string& statement) {
    const std::string text = trim(statement);
    if (text.empty()) {
        return Flow::None;
    }
    const std::string shouted = upper(text);

    if (shouted == "NOP") {
        return Flow::None;
    }
    if (shouted == "END") {
        return handle_end();
    }

    if (const std::optional<Flow> flow = handle_flow(text, shouted)) {
        return *flow;
    }

    if (starts_with(shouted, "SAY")) {
        output.push_back(fmt(eval_expr(interpreter, trim(text.substr(3)))));
        return Flow::None;
    }

    if (shouted == "EXIT" || shouted == "STOP" || starts_with(shouted, "EXIT ") ||
        starts_with(shouted, "STOP ")) {
        interpreter.running = false;
        return Flow::Jump;
    }

    if (handle_turtle(shouted, text)) {
        return Flow::None;
    }

    static const std::regex assignment{R"(^([A-Za-z_!][A-Za-z0-9_!.]*)\s*=(.*)$)",
                                       std::regex::ECMAScript};
    std::smatch match;
    if (std::regex_match(text, match, assignment)) {
        const std::string rest = match[2].str();
        // `X == 5` is a comparison, not an assignment.
        if (!starts_with(rest, "=")) {
            set_var(interpreter, match[1].str(), eval_expr(interpreter, trim(rest)));
            return Flow::None;
        }
    }

    throw RexxError("unrecognised statement: " + text);
}

std::optional<Flow> RexxMachine::handle_flow(const std::string& text,
                                          const std::string& shouted) {
    if (starts_with(shouted, "PULL") || starts_with(shouted, "PARSE PULL")) {
        return handle_pull(text);
    }
    if (starts_with(shouted, "PARSE VAR")) {
        return handle_parse_var(trim(text.substr(9)));
    }
    if (starts_with(shouted, "IF ")) {
        return handle_if(text);
    }
    if (shouted == "DO" || starts_with(shouted, "DO ")) {
        return handle_do(text);
    }
    if (shouted == "SELECT") {
        Frame frame;
        frame.kind = "select";
        state.stack.push_back(std::move(frame));
        return Flow::None;
    }
    if (starts_with(shouted, "WHEN ")) {
        return handle_when(text);
    }
    if (shouted == "OTHERWISE" || starts_with(shouted, "OTHERWISE ")) {
        return handle_otherwise(text);
    }
    if (starts_with(shouted, "CALL ")) {
        return handle_call(trim(text.substr(5)));
    }
    if (shouted == "RETURN" || starts_with(shouted, "RETURN ")) {
        return handle_return(text);
    }
    if (shouted == "LEAVE") {
        return handle_leave();
    }
    if (shouted == "ITERATE") {
        return handle_iterate();
    }
    return std::nullopt;
}

Flow RexxMachine::handle_pull(const std::string& text) {
    static const std::regex pattern{R"(^\s*(PARSE\s+)?PULL\s*)",
                                    std::regex::ECMAScript | std::regex::icase};
    const std::string body = trim(std::regex_replace(text, pattern, ""));
    if (body.empty()) {
        return Flow::None;
    }
    const std::string name = split_ws(body).front();
    state.waiting = name;
    interpreter.request_input("", upper(name) + "$", false);
    return Flow::Jump;
}

Flow RexxMachine::handle_parse_var(const std::string& body) {
    const std::vector<std::string> parts = split_ws(body);
    if (parts.size() < 2) {
        return Flow::None;
    }
    const std::vector<std::string> words = split_ws(fmt(get_var(interpreter, parts[0])));
    for (std::size_t position = 1; position < parts.size(); ++position) {
        set_var(interpreter, parts[position],
                RValue::word(position - 1 < words.size() ? words[position - 1] : ""));
    }
    return Flow::None;
}

Flow RexxMachine::handle_if(const std::string& text) {
    static const std::regex pattern{R"(^IF\s+(.*?)\s+THEN\b(.*)$)",
                                    std::regex::ECMAScript | std::regex::icase};
    std::smatch match;
    if (!std::regex_match(text, match, pattern)) {
        throw RexxError("IF requires a THEN clause");
    }
    const std::string condition = trim(match[1].str());
    const std::string rest = trim(match[2].str());
    const bool taken = truth(eval_expr(interpreter, condition));

    if (starts_with(upper(rest), "DO") && code_words(rest).size() <= 1) {
        const std::size_t block_end = scan_block_end(interpreter, interpreter.current_line);
        if (taken) {
            Frame frame;
            frame.kind = "if_then";
            state.stack.push_back(std::move(frame));
            return Flow::None;  // fall through into the block
        }
        return after_block(block_end);
    }

    std::string then_statement = rest;
    std::string else_statement;
    static const std::regex else_pattern{R"(^(.*?)\s+ELSE\s+(.*)$)",
                                         std::regex::ECMAScript | std::regex::icase};
    std::smatch else_match;
    if (std::regex_match(rest, else_match, else_pattern)) {
        then_statement = trim(else_match[1].str());
        else_statement = trim(else_match[2].str());
    }

    if (taken) {
        return then_statement.empty() ? Flow::None : exec_statement(then_statement);
    }
    if (!else_statement.empty()) {
        return exec_statement(else_statement);
    }
    return Flow::None;
}

Flow RexxMachine::after_block(std::size_t block_end) {
    const auto& lines = interpreter.program_lines();
    const std::size_t next = block_end + 1;
    if (next < lines.size()) {
        const std::string rest =
            statement_after_label(strip_comment(lines[next].text));
        static const std::regex else_pattern{R"(^ELSE\b)",
                                             std::regex::ECMAScript | std::regex::icase};
        if (std::regex_search(rest, else_pattern)) {
            const std::string body =
                trim(std::regex_replace(rest, else_pattern, ""));
            interpreter.current_line = next + 1;
            if (starts_with(upper(body), "DO")) {
                Frame frame;
                frame.kind = "else_body";
                state.stack.push_back(std::move(frame));
            } else if (!body.empty()) {
                exec_statement(body);
            }
            return Flow::Jump;
        }
    }
    interpreter.current_line = next;
    return Flow::Jump;
}

Flow RexxMachine::handle_do(const std::string& text) {
    const std::size_t do_index = interpreter.current_line;
    static const std::regex prefix{R"(^DO\b)", std::regex::ECMAScript | std::regex::icase};
    const std::string spec = trim(std::regex_replace(text, prefix, ""));
    const std::string spec_upper = upper(spec);

    if (starts_with(spec_upper, "WHILE ")) {
        const std::string condition = trim(spec.substr(6));
        if (!truth(eval_expr(interpreter, condition))) {
            interpreter.current_line = scan_block_end(interpreter, do_index) + 1;
            return Flow::Jump;
        }
        Frame frame;
        frame.kind = "while";
        frame.do_index = do_index;
        frame.has_do_index = true;
        frame.condition = condition;
        state.stack.push_back(std::move(frame));
        return Flow::None;
    }
    if (starts_with(spec_upper, "UNTIL ")) {
        Frame frame;
        frame.kind = "until";
        frame.do_index = do_index;
        frame.has_do_index = true;
        frame.condition = trim(spec.substr(6));
        state.stack.push_back(std::move(frame));
        return Flow::None;
    }
    if (starts_with(spec_upper, "FOREVER")) {
        Frame frame;
        frame.kind = "forever";
        frame.do_index = do_index;
        frame.has_do_index = true;
        state.stack.push_back(std::move(frame));
        return Flow::None;
    }

    static const std::regex control{
        R"(^([A-Za-z_!][A-Za-z0-9_!.]*)\s*=\s*(.+?)\s+TO\s+(.+?)(?:\s+BY\s+(.+?))?(?:\s+FOR\s+.+)?$)",
        std::regex::ECMAScript | std::regex::icase};
    std::smatch match;
    if (std::regex_match(spec, match, control)) {
        set_var(interpreter, match[1].str(), eval_expr(interpreter, trim(match[2].str())));
        Frame frame;
        frame.kind = "to";
        frame.do_index = do_index;
        frame.has_do_index = true;
        frame.variable = match[1].str();
        frame.limit = eval_expr(interpreter, trim(match[3].str()));
        frame.step = match[4].matched ? eval_expr(interpreter, trim(match[4].str()))
                                      : RValue::numeric(1.0);
        state.stack.push_back(std::move(frame));
        return Flow::None;
    }

    static const std::regex over{R"(^([A-Za-z_!][A-Za-z0-9_!.]*)\s+OVER\s+(.+)$)",
                                 std::regex::ECMAScript | std::regex::icase};
    if (std::regex_match(spec, match, over)) {
        const std::vector<std::string> items =
            split_ws(fmt(eval_expr(interpreter, trim(match[2].str()))));
        if (items.empty()) {
            interpreter.current_line = scan_block_end(interpreter, do_index) + 1;
            return Flow::Jump;
        }
        set_var(interpreter, match[1].str(), RValue::word(items.front()));
        Frame frame;
        frame.kind = "over";
        frame.do_index = do_index;
        frame.has_do_index = true;
        frame.variable = match[1].str();
        frame.items = items;
        state.stack.push_back(std::move(frame));
        return Flow::None;
    }

    if (!spec.empty()) {
        const long long count = static_cast<long long>(num(eval_expr(interpreter, spec)));
        if (count <= 0) {
            interpreter.current_line = scan_block_end(interpreter, do_index) + 1;
            return Flow::Jump;
        }
        Frame frame;
        frame.kind = "count";
        frame.do_index = do_index;
        frame.has_do_index = true;
        frame.count = count;
        state.stack.push_back(std::move(frame));
        return Flow::None;
    }

    Frame frame;
    frame.kind = "block";
    frame.do_index = do_index;
    state.stack.push_back(std::move(frame));
    return Flow::None;
}

Flow RexxMachine::handle_end() {
    if (state.stack.empty()) {
        return Flow::None;
    }
    Frame frame = state.stack.back();
    state.stack.pop_back();
    const std::string& kind = frame.kind;
    const std::size_t do_index =
        frame.has_do_index ? frame.do_index : interpreter.current_line;

    if (kind == "if_then") {
        return skip_else();
    }
    if (kind == "block" || kind == "select" || kind == "else_body" ||
        kind == "when_body" || kind == "otherwise_body") {
        return Flow::None;
    }
    if (kind == "while") {
        if (truth(eval_expr(interpreter, frame.condition))) {
            state.stack.push_back(frame);
            interpreter.current_line = do_index + 1;
            return Flow::Jump;
        }
        return Flow::None;
    }
    if (kind == "until") {
        if (truth(eval_expr(interpreter, frame.condition))) {
            return Flow::None;
        }
        state.stack.push_back(frame);
        interpreter.current_line = do_index + 1;
        return Flow::Jump;
    }
    if (kind == "forever") {
        state.stack.push_back(frame);
        interpreter.current_line = do_index + 1;
        return Flow::Jump;
    }
    if (kind == "count") {
        frame.count -= 1;
        if (frame.count > 0) {
            state.stack.push_back(frame);
            interpreter.current_line = do_index + 1;
            return Flow::Jump;
        }
        return Flow::None;
    }
    if (kind == "to") {
        const double step = num(frame.step);
        const double current = num(get_var(interpreter, frame.variable)) +
                               (step == 0.0 ? 1.0 : step);
        const double limit = num(frame.limit);
        if ((step >= 0.0 && current <= limit) || (step < 0.0 && current >= limit)) {
            set_var(interpreter, frame.variable, RValue::numeric(current));
            state.stack.push_back(frame);
            interpreter.current_line = do_index + 1;
            return Flow::Jump;
        }
        return Flow::None;
    }
    if (kind == "over") {
        ++frame.index;
        if (frame.index < frame.items.size()) {
            set_var(interpreter, frame.variable, RValue::word(frame.items[frame.index]));
            state.stack.push_back(frame);
            interpreter.current_line = do_index + 1;
            return Flow::Jump;
        }
        return Flow::None;
    }
    return Flow::None;
}

Flow RexxMachine::skip_else() {
    const auto& lines = interpreter.program_lines();
    const std::size_t next = interpreter.current_line + 1;
    if (next >= lines.size()) {
        return Flow::None;
    }
    const std::string rest = statement_after_label(strip_comment(lines[next].text));
    static const std::regex else_pattern{R"(^ELSE\b)",
                                         std::regex::ECMAScript | std::regex::icase};
    if (!std::regex_search(rest, else_pattern)) {
        return Flow::None;
    }
    const std::string body = trim(std::regex_replace(rest, else_pattern, ""));
    if (starts_with(upper(body), "DO")) {
        interpreter.current_line = scan_block_end(interpreter, next) + 1;
    } else {
        interpreter.current_line = next + 1;
    }
    return Flow::Jump;
}

Frame* RexxMachine::find_select() {
    for (auto it = state.stack.rbegin(); it != state.stack.rend(); ++it) {
        if (it->kind == "select") {
            return &*it;
        }
    }
    return nullptr;
}

Flow RexxMachine::handle_when(const std::string& text) {
    Frame* frame = find_select();
    if (frame == nullptr) {
        throw RexxError("WHEN outside of SELECT");
    }
    static const std::regex pattern{R"(^WHEN\s+(.*?)\s+THEN\b(.*)$)",
                                    std::regex::ECMAScript | std::regex::icase};
    std::smatch match;
    if (!std::regex_match(text, match, pattern)) {
        throw RexxError("WHEN requires a THEN clause");
    }
    const std::string condition = trim(match[1].str());
    const std::string body = trim(match[2].str());
    const std::string body_upper = upper(body);

    if (frame->matched || !truth(eval_expr(interpreter, condition))) {
        if (starts_with(body_upper, "DO")) {
            interpreter.current_line = scan_block_end(interpreter, interpreter.current_line) + 1;
            return Flow::Jump;
        }
        return Flow::None;
    }

    frame->matched = true;
    if (starts_with(body_upper, "DO") && code_words(body).size() <= 1) {
        Frame body_frame;
        body_frame.kind = "when_body";
        state.stack.push_back(std::move(body_frame));
        return Flow::None;
    }
    return body.empty() ? Flow::None : exec_statement(body);
}

Flow RexxMachine::handle_otherwise(const std::string& text) {
    Frame* frame = find_select();
    if (frame == nullptr) {
        throw RexxError("OTHERWISE outside of SELECT");
    }
    static const std::regex prefix{R"(^OTHERWISE\b)",
                                   std::regex::ECMAScript | std::regex::icase};
    const std::string body = trim(std::regex_replace(text, prefix, ""));
    const std::string body_upper = upper(body);

    if (frame->matched) {
        if (starts_with(body_upper, "DO")) {
            interpreter.current_line = scan_block_end(interpreter, interpreter.current_line) + 1;
            return Flow::Jump;
        }
        return Flow::None;
    }

    frame->matched = true;
    if (starts_with(body_upper, "DO") && code_words(body).size() <= 1) {
        Frame body_frame;
        body_frame.kind = "otherwise_body";
        state.stack.push_back(std::move(body_frame));
        return Flow::None;
    }
    return body.empty() ? Flow::None : exec_statement(body);
}

Flow RexxMachine::handle_call(const std::string& name) {
    static const std::regex pattern{
        R"(^([A-Za-z_!?][A-Za-z0-9_!?.]*)\s*(?:\((.*)\))?\s*(.*)$)",
        std::regex::ECMAScript};
    std::smatch match;
    if (!std::regex_match(name, match, pattern)) {
        throw RexxError("malformed CALL statement");
    }
    const std::string label = upper(match[1].str());
    const std::string argument_text =
        match[2].matched ? match[2].str() : match[3].str();
    std::vector<RValue> arguments;
    for (const std::string& part : split_args(argument_text)) {
        // The reference stores CALL arguments as the strings they will be read
        // back as by ARG(n), so `n = ARG(1)` assigns text, not a number.
        arguments.push_back(RValue::word(fmt(eval_expr(interpreter, part))));
    }
    const std::map<std::string, std::size_t> table = labels(interpreter);
    const auto target = table.find(label);
    if (target == table.end()) {
        throw RexxError("unknown subroutine '" + label + "'");
    }
    state.call_stack.emplace_back(interpreter.current_line, state.args);
    state.args = arguments;
    interpreter.current_line = target->second + 1;
    return Flow::Jump;
}

Flow RexxMachine::handle_return(const std::string& text) {
    static const std::regex prefix{R"(^RETURN\b)",
                                   std::regex::ECMAScript | std::regex::icase};
    const std::string expression = trim(std::regex_replace(text, prefix, ""));
    if (!expression.empty()) {
        set_var(interpreter, "RESULT", eval_expr(interpreter, expression));
    }
    if (!state.call_stack.empty()) {
        const auto [return_line, previous_args] = state.call_stack.back();
        state.call_stack.pop_back();
        state.args = previous_args;
        interpreter.current_line = return_line + 1;
        return Flow::Jump;
    }
    interpreter.running = false;
    return Flow::Jump;
}

std::optional<std::size_t> RexxMachine::nearest_loop() {
    for (std::size_t index = state.stack.size(); index-- > 0;) {
        if (is_loop(state.stack[index].kind)) {
            return index;
        }
    }
    return std::nullopt;
}

Flow RexxMachine::handle_leave() {
    const std::optional<std::size_t> found = nearest_loop();
    if (!found.has_value()) {
        throw RexxError("LEAVE outside of a loop");
    }
    const Frame frame = state.stack[*found];
    state.stack.erase(state.stack.begin() + static_cast<std::ptrdiff_t>(*found),
                      state.stack.end());
    interpreter.current_line = scan_block_end(interpreter, frame.do_index) + 1;
    return Flow::Jump;
}

Flow RexxMachine::handle_iterate() {
    const std::optional<std::size_t> found = nearest_loop();
    if (!found.has_value()) {
        throw RexxError("ITERATE outside of a loop");
    }
    const Frame frame = state.stack[*found];
    interpreter.current_line = scan_block_end(interpreter, frame.do_index);
    return Flow::Jump;
}

bool RexxMachine::handle_turtle(const std::string& shouted, const std::string& text) {
    const std::vector<std::string> words = split_ws(shouted);
    const std::string keyword = words.empty() ? "" : words.front();
    const std::string argument = trim(text.substr(std::min(text.size(), keyword.size())));
    const auto amount = [this, &argument]() {
        return argument.empty() ? 0.0 : num(eval_expr(interpreter, argument));
    };
    if (keyword == "FORWARD" || keyword == "FD" || keyword == "MOVE") {
        turtle.forward(amount());
        return true;
    }
    if (keyword == "BACK" || keyword == "BK") {
        turtle.back(amount());
        return true;
    }
    if (keyword == "RIGHT" || keyword == "RT" || keyword == "TURN") {
        turtle.right(amount());
        return true;
    }
    if (keyword == "LEFT" || keyword == "LT") {
        turtle.left(amount());
        return true;
    }
    if (keyword == "PENUP") {
        turtle.pen_up();
        return true;
    }
    if (keyword == "PENDOWN") {
        turtle.pen_down();
        return true;
    }
    if (keyword == "HOME") {
        turtle.home();
        return true;
    }
    return false;
}

// ---------------------------------------------------------------------------
// Built-in functions
// ---------------------------------------------------------------------------

RValue call_builtin(Interpreter& interpreter, const std::string& name,
                    const std::vector<RValue>& args) {
    const auto s = [&args](std::size_t index) {
        return index < args.size() ? fmt(args[index]) : std::string{};
    };
    const auto n = [&args](std::size_t index, double fallback = 0.0) {
        return index < args.size() ? num(args[index]) : fallback;
    };

    if (name == "ARG") {
        RexxState& state = interpreter.executor_state<RexxState>();
        const long long index = static_cast<long long>(n(0, 1)) - 1;
        if (index >= 0 && static_cast<std::size_t>(index) < state.args.size()) {
            return state.args[static_cast<std::size_t>(index)];
        }
        return RValue::word("");
    }
    if (name == "LENGTH") {
        return RValue::numeric(static_cast<double>(s(0).size()));
    }
    if (name == "SUBSTR") {
        const std::string text = s(0);
        const long long start = static_cast<long long>(n(1, 1));
        const long long begin = start - 1;
        if (args.size() >= 3) {
            const long long count = static_cast<long long>(n(2));
            if (begin < 0 || static_cast<std::size_t>(begin) >= text.size()) {
                return RValue::word("");
            }
            return RValue::word(text.substr(static_cast<std::size_t>(begin),
                                            static_cast<std::size_t>(std::max(0LL, count))));
        }
        if (begin < 0 || static_cast<std::size_t>(begin) >= text.size()) {
            return RValue::word("");
        }
        return RValue::word(text.substr(static_cast<std::size_t>(begin)));
    }
    if (name == "LEFT") {
        const long long count = static_cast<long long>(n(1));
        return RValue::word(count <= 0
                                ? std::string{}
                                : s(0).substr(0, static_cast<std::size_t>(count)));
    }
    if (name == "RIGHT") {
        const std::string text = s(0);
        const long long count = static_cast<long long>(n(1));
        if (count <= 0) {
            return RValue::word("");
        }
        const std::size_t wanted = std::min(text.size(), static_cast<std::size_t>(count));
        return RValue::word(text.substr(text.size() - wanted));
    }
    if (name == "COPIES") {
        const long long count = std::max(0LL, static_cast<long long>(n(1)));
        std::string result;
        for (long long index = 0; index < count; ++index) {
            result += s(0);
        }
        return RValue::word(result);
    }
    if (name == "REVERSE") {
        std::string text = s(0);
        std::reverse(text.begin(), text.end());
        return RValue::word(text);
    }
    if (name == "UPPER") {
        return RValue::word(upper(s(0)));
    }
    if (name == "LOWER") {
        std::string text = s(0);
        for (char& character : text) {
            character =
                static_cast<char>(std::tolower(static_cast<unsigned char>(character)));
        }
        return RValue::word(text);
    }
    if (name == "STRIP") {
        return RValue::word(trim(s(0)));
    }
    if (name == "SPACE") {
        std::vector<std::string> words;
        std::size_t index = 0;
        const std::string text = s(0);
        while (index < text.size()) {
            while (index < text.size() && is_space(text[index])) {
                ++index;
            }
            const std::size_t start = index;
            while (index < text.size() && !is_space(text[index])) {
                ++index;
            }
            if (index > start) {
                words.push_back(text.substr(start, index - start));
            }
        }
        if (words.empty()) {
            return RValue::word("");
        }
        const long long count = std::max(0LL, static_cast<long long>(n(1, 1)));
        const std::string pad = s(2).empty() ? " " : s(2);
        std::string result;
        for (std::size_t word = 0; word < words.size(); ++word) {
            if (word != 0) {
                for (long long repeat = 0; repeat < count; ++repeat) {
                    result += pad;
                }
            }
            result += words[word];
        }
        return RValue::word(result);
    }
    if (name == "WORD") {
        const std::vector<std::string> words = split_ws(s(0));
        const long long index = static_cast<long long>(n(1, 1)) - 1;
        if (index >= 0 && static_cast<std::size_t>(index) < words.size()) {
            return RValue::word(words[static_cast<std::size_t>(index)]);
        }
        return RValue::word("");
    }
    if (name == "WORDS") {
        return RValue::numeric(static_cast<double>(split_ws(s(0)).size()));
    }
    if (name == "INDEX") {
        const std::size_t found = s(0).find(s(1));
        return RValue::numeric(found == std::string::npos
                                   ? 0.0
                                   : static_cast<double>(found + 1));
    }
    if (name == "ABS") {
        return RValue::numeric(std::fabs(n(0)));
    }
    if (name == "MAX" || name == "MIN") {
        if (args.empty()) {
            return RValue::numeric(0.0);
        }
        double result = num(args.front());
        for (const RValue& argument : args) {
            result = name == "MAX" ? std::max(result, num(argument))
                                   : std::min(result, num(argument));
        }
        return RValue::numeric(result);
    }
    if (name == "TRUNC") {
        return RValue::numeric(std::trunc(n(0)));
    }
    if (name == "SIGN") {
        const double value = n(0);
        return RValue::numeric(value > 0 ? 1 : (value < 0 ? -1 : 0));
    }
    if (name == "FORMAT") {
        return RValue::word(fmt(RValue::numeric(n(0))));
    }
    if (name == "DATATYPE") {
        return RValue::word(is_numeric(s(0)) ? "NUM" : "CHAR");
    }
    throw RexxError("unknown function " + name);
}

}  // namespace

std::string execute_rexx(Interpreter& interpreter, std::string_view source,
                         TurtleHost& turtle) {
    RexxState& state = interpreter.executor_state<RexxState>();
    std::vector<std::string> output;
    RexxMachine executor{interpreter, turtle, state, output};
    executor.sync_pending_input();

    const std::string raw = strip_comment(std::string{source});
    if (trim(raw).empty()) {
        return "";
    }

    executor.run_statements(raw);

    if (output.empty()) {
        return "";
    }
    std::string joined;
    for (const std::string& line : output) {
        if (!joined.empty()) {
            joined.push_back('\n');
        }
        joined += line;
    }
    joined.push_back('\n');
    return joined;
}

}  // namespace timewarp::lang
