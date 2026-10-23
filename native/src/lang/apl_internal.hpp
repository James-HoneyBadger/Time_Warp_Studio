// SPDX-License-Identifier: MIT
//
// What the APL engine's translation units share.
//
// `apl.cpp` was 1,571 lines in one anonymous namespace: the value type, the array
// operations, the function tables, the tokenizer, the run state, the evaluator and the
// statements. The value type and the operations on it cross all of those, so they are
// declared here, in a named `apl_detail` namespace rather than an anonymous one -- a
// type that several translation units pass around needs one definition they agree on.
//
// Internal to the APL engine: nothing outside `src/lang/apl*.cpp` includes it.

#pragma once

#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/turtle_host.hpp"

#include <cstddef>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace timewarp::lang::apl_detail {

class AplError : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

// ---------------------------------------------------------------------------
// Values
// ---------------------------------------------------------------------------

/// A number, a string, or a (possibly nested) array of them.
struct Value {
    enum class Kind { Number, Text, Array, Nothing };

    Kind kind = Kind::Number;
    double number = 0.0;
    std::string text;
    std::vector<Value> items;

    static Value numbered(double value) {
        Value result;
        result.number = value;
        return result;
    }
    static Value word(std::string value) {
        Value result;
        result.kind = Kind::Text;
        result.text = std::move(value);
        return result;
    }
    static Value array(std::vector<Value> values) {
        Value result;
        result.kind = Kind::Array;
        result.items = std::move(values);
        return result;
    }
    static Value nothing() {
        Value result;
        result.kind = Kind::Nothing;
        return result;
    }
};

// ---------------------------------------------------------------------------
// Run state
// ---------------------------------------------------------------------------

struct Header {
    std::string name;
    std::vector<std::string> parameters;
    std::string result;
};

struct Function {
    std::vector<std::string> parameters;
    std::vector<std::string> body;
};

struct Definition {
    Header header;
    std::vector<std::string> body;
};

struct AplState {
    std::map<std::string, Value> variables;
    std::map<std::string, Function> functions;
    std::optional<Definition> defining;
    bool quiet = false;
    /// The reference reads `turtle.x` and `turtle.heading`; the host has no
    /// readback, so the pose is mirrored here with the same maths the hosts
    /// use. `TX`, `TY` and `THETA` report it.
    TurtlePose pose;
};

bool is_array(const Value& value);
std::string upper(std::string_view text);
bool is_space(char character);
std::string trim(std::string_view text);
bool starts_with(std::string_view text, std::string_view prefix);
std::string replace_all(std::string text, std::string_view from,
                        std::string_view to);
std::string display_text(const Value& value);
// ---------------------------------------------------------------------------
// Arrays
// ---------------------------------------------------------------------------

std::vector<Value> items_of(const Value& value);
std::vector<Value> raze(const Value& value);
std::vector<double> shape_of(const Value& value);
Value from_flat(const std::vector<double>& shape, const std::vector<Value>& flat);
Value each1(const std::function<Value(const Value&)>& function, const Value& value);
Value each2(const std::function<Value(const Value&, const Value&)>& function,
            const Value& left, const Value& right);
double number_of(const Value& value);
bool values_equal(const Value& left, const Value& right);
bool value_less(const Value& left, const Value& right);
// ---------------------------------------------------------------------------
// Scalar functions
// ---------------------------------------------------------------------------

Value divide(const Value& left, const Value& right);
/// `X | Y` is Y modulo X, and the result takes X's sign, as in APL.
Value residue(const Value& left, const Value& right);
Value apply_canonical(const std::string& glyph, const Value* left,
                      const Value& right);;
// ---------------------------------------------------------------------------
// Structural functions
// ---------------------------------------------------------------------------

Value iota(const Value& right);
Value index_of(const Value& left, const Value& right);
Value reverse(const Value& right);
Value rotate(const Value& left, const Value& right);
Value take(const Value& left, const Value& right);
Value drop(const Value& left, const Value& right);
Value transpose(const Value& right);
Value catenate(const Value& left, const Value& right);
Value membership(const Value& left, const Value& right);
Value grade(const Value& right, bool descending);
Value reshape(const Value& left, const Value& right);
// ---------------------------------------------------------------------------
// Function tables
// ---------------------------------------------------------------------------
const std::vector<std::string>& structural();
const std::vector<std::string>& scalars();
bool in_list(const std::vector<std::string>& list, std::string_view value);
const std::map<std::string, std::string>& turtle_words();
std::string canonical(std::string_view token);
// ---------------------------------------------------------------------------
// Tokens
// ---------------------------------------------------------------------------
std::vector<std::string> tokenize(std::string_view text);
bool is_function(const std::string& token, const AplState* state);
bool is_operand(const std::string& token, const AplState* state);
Value value_of(const std::string& token, const std::map<std::string, Value>& variables);
// ---------------------------------------------------------------------------
// Evaluation
// ---------------------------------------------------------------------------

Value evaluate(const std::vector<std::string>& tokens, AplState& state,
               TurtleHost* turtle);;
Value apply(const std::string& token, const Value* left, const Value& right,
            AplState& state, TurtleHost* turtle);;
Value reduce(const std::string& operation, const Value& right);
Value scan(const std::string& operation, const Value& right);
Value call_user_function(const std::string& name, const Value* left,
                         const Value& right, AplState& state,
                         TurtleHost* turtle);;
// ---------------------------------------------------------------------------
// Statements
// ---------------------------------------------------------------------------

std::pair<Value, bool> statement(const std::string& line, AplState& state,
                                 TurtleHost* turtle);
Header parse_header(const std::string& text);
Value turtle_state_value(double raw);

}  // namespace timewarp::lang::apl_detail
