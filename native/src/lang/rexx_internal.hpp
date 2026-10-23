// SPDX-License-Identifier: MIT
//
// What the REXX engine's translation units share.
//
// `rexx.cpp` was 1,801 lines in one anonymous namespace: the value type and its
// helpers, the execution state, the tokenizer and parser, the line helpers, the
// machine and the built-in functions. The value type, the state and the helpers cross
// all of those, so they are declared here, in a named `rexx_detail` namespace rather
// than an anonymous one -- a type that several translation units pass around needs one
// definition they agree on.
//
// Internal to the REXX engine: nothing outside `src/lang/rexx*.cpp` includes it.

#pragma once

#include "timewarp/core/charconv.hpp"

#include "timewarp/lang/interpreter.hpp"

#include <cctype>
#include <cmath>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <optional>
#include <regex>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace timewarp::lang::rexx_detail {

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

std::string upper(std::string_view text);
bool is_space(char character);
std::string trim(std::string_view text);
bool starts_with(std::string_view text, std::string_view prefix);
/// The reference's `repr(float)` is the shortest round-trip form.
std::string float_repr(double value);
std::string fmt(const RValue& value);
double num(const RValue& value);
bool is_numeric(const std::string& text);
bool truth(const RValue& value);
std::vector<std::string> split_ws(const std::string& text);

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
    /// The ELSE lines a taken single-statement IF has to skip. A DO body ends at its
    /// END, where `handle_end` can skip; a one-line body has no END, so the ELSE line
    /// that follows it is recorded and looked at when execution gets there. A list
    /// rather than one line, because an IF's body may be another IF with an ELSE of
    /// its own, and both have to be skipped when both branches were taken.
    std::vector<std::size_t> skip_else_lines;
};

bool is_loop(const std::string& kind);
RValue get_var(Interpreter& interpreter, const std::string& name);
void set_var(Interpreter& interpreter, const std::string& name, const RValue& value);
/// The expression parser's only entry point: tokens in, a value out.
RValue eval_expr(Interpreter& interpreter, const std::string& expression);

/// A built-in function, by name, with its arguments already evaluated.
RValue call_builtin(Interpreter& interpreter, const std::string& name,
                    const std::vector<RValue>& args);

// -- the line helpers the machine reads ---------------------------------------

std::string strip_comment(const std::string& line);
std::vector<std::string> split_statements(const std::string& line);
std::vector<std::string> split_args(const std::string& text);
std::vector<std::string> code_words(const std::string& line);
const std::vector<std::string>& block_openers();
bool in_words(const std::vector<std::string>& words, std::string_view value);
std::size_t scan_block_end(Interpreter& interpreter, std::size_t start);
const std::regex& label_pattern();
std::map<std::string, std::size_t> labels(Interpreter& interpreter);
std::string statement_after_label(const std::string& text);
bool line_starts_do(Interpreter& interpreter, std::size_t line);
std::size_t after_statement(Interpreter& interpreter, std::size_t line);
std::size_t after_else_clause(Interpreter& interpreter, std::size_t else_line);

}  // namespace timewarp::lang::rexx_detail
