// SPDX-License-Identifier: MIT
//
// The tokenizer and the expression parser: REXX's three precedence levels, `||` and
// abutting concatenation, and comparisons that go numeric when both sides look numeric.

#include "rexx_internal.hpp"

#include "timewarp/lang/turtle_host.hpp"

#include <algorithm>
#include <cctype>
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
namespace rexx_detail {

// ---------------------------------------------------------------------------

enum class TokenKind { Number, Text, Symbol, Operator };

struct Token {
    TokenKind kind;
    std::string text;
};

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

}  // namespace rexx_detail
}  // namespace timewarp::lang
