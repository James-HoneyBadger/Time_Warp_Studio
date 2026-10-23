// SPDX-License-Identifier: MIT
//
// The function tables: which names APL knows, which of them take a left argument, the
// turtle words, and how a value is printed.

#include "apl_internal.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace timewarp::lang {
namespace apl_detail {

// ---------------------------------------------------------------------------
// Function tables
// ---------------------------------------------------------------------------

const std::map<std::string, std::string>& aliases() {
    static const std::map<std::string, std::string> table = {
        {"PLUS", "+"},         {"MINUS", "-"},      {"TIMES", "×"},
        {"DIVIDE", "÷"},       {"POWER", "*"},      {"LOG", "⍟"},
        {"CEILING", "⌈"},      {"FLOOR", "⌊"},      {"MAX", "⌈"},
        {"MIN", "⌊"},          {"ABS", "|"},        {"RESIDUE", "|"},
        {"SIGNUM", "×"},       {"RECIPROCAL", "÷"}, {"RHO", "⍴"},
        {"SHAPE", "⍴"},        {"RESHAPE", "⍴"},    {"IOTA", "⍳"},
        {"INDEXOF", "⍳"},      {"REVERSE", "⌽"},    {"ROTATE", "⌽"},
        {"TAKE", "↑"},         {"DROP", "↓"},       {"TRANSPOSE", "⍉"},
        {"RAVEL", ","},        {"CATENATE", ","},   {"MEMBERSHIP", "∈"},
        {"NOT", "~"},          {"AND", "∧"},        {"OR", "∨"},
        {"NAND", "⍲"},         {"NOR", "⍱"},        {"EQUAL", "="},
        {"NOTEQUAL", "≠"},     {"LESS", "<"},       {"LESSEQ", "≤"},
        {"GREATER", ">"},      {"GREATEREQ", "≥"},  {"GRADEUP", "⍋"},
        {"GRADEDOWN", "⍒"},    {"FORMAT", "⍕"},     {"ROLL", "?"},
        {"REDUCE", "/"},       {"SCAN", "\\"},
    };
    return table;
}

const std::vector<std::string>& structural() {
    static const std::vector<std::string> table = {
        "⍳", "⍴", "⌽", "↑", "↓", "⍉", ",", "∈", "⍋", "⍒", "⍕"};
    return table;
}

const std::vector<std::string>& scalars() {
    static const std::vector<std::string> table = {
        "+", "-", "×", "÷", "*", "⍟", "⌈", "⌊", "|", "=", "≠", "<",
        "≤", ">", "≥", "∧", "∨", "⍲", "⍱", "~", "?"};
    return table;
}

bool in_list(const std::vector<std::string>& list, std::string_view value) {
    return std::find(list.begin(), list.end(), value) != list.end();
}

const std::map<std::string, std::string>& turtle_words() {
    static const std::map<std::string, std::string> table = {
        {"FD", "FD"},   {"FORWARD", "FD"}, {"BK", "BK"},  {"BACK", "BK"},
        {"RT", "RT"},   {"RIGHT", "RT"},   {"LT", "LT"},  {"LEFT", "LT"},
        {"PU", "PU"},   {"PENUP", "PU"},   {"PD", "PD"},  {"PENDOWN", "PD"},
        {"HOME", "HOME"}, {"CS", "CS"},    {"CLEAR", "CS"}};
    return table;
}

std::string canonical(std::string_view token) {
    const std::string shouted = upper(token);
    const auto found = aliases().find(shouted);
    return found == aliases().end() ? shouted : found->second;
}

std::string format_value(const Value& value) { return display_text(value); }

Value apply_canonical(const std::string& glyph, const Value* left,
                      const Value& right) {
    if (in_list(structural(), glyph)) {
        if (glyph == "⍳") {
            return left == nullptr ? iota(right) : index_of(*left, right);
        }
        if (glyph == "⍴") {
            return left == nullptr ? Value::array([&right]() {
                std::vector<Value> shape;
                for (double size : shape_of(right)) {
                    shape.push_back(Value::numbered(size));
                }
                return shape;
            }())
                                   : reshape(*left, right);
        }
        if (glyph == "⌽") {
            return left == nullptr ? reverse(right) : rotate(*left, right);
        }
        if (glyph == "↑") {
            if (left == nullptr) {
                throw AplError("↑ has no monadic form");
            }
            return take(*left, right);
        }
        if (glyph == "↓") {
            if (left == nullptr) {
                throw AplError("↓ has no monadic form");
            }
            return drop(*left, right);
        }
        if (glyph == "⍉") {
            if (left != nullptr) {
                throw AplError("⍉ has no dyadic form");
            }
            return transpose(right);
        }
        if (glyph == ",") {
            return left == nullptr ? Value::array(raze(right)) : catenate(*left, right);
        }
        if (glyph == "∈") {
            if (left == nullptr) {
                throw AplError("∈ has no monadic form");
            }
            return membership(*left, right);
        }
        if (glyph == "⍋") {
            if (left != nullptr) {
                throw AplError("⍋ has no dyadic form");
            }
            return grade(right, false);
        }
        if (glyph == "⍒") {
            if (left != nullptr) {
                throw AplError("⍒ has no dyadic form");
            }
            return grade(right, true);
        }
        if (glyph == "⍕") {
            if (left != nullptr) {
                throw AplError("⍕ has no dyadic form");
            }
            return Value::word(format_value(right));
        }
    }

    if (!in_list(scalars(), glyph)) {
        throw AplError(glyph + " is not a function this build knows");
    }

    const auto monad = [&](const std::function<Value(const Value&)>& function) {
        return each1(function, right);
    };
    const auto dyad = [&](const std::function<Value(const Value&, const Value&)>& function) {
        return each2(function, *left, right);
    };

    if (glyph == "+") {
        if (left == nullptr) {
            throw AplError("+ has no monadic form");
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(number_of(a) + number_of(b));
        });
    }
    if (glyph == "-") {
        if (left == nullptr) {
            return monad([](const Value& value) {
                return Value::numbered(-number_of(value));
            });
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(number_of(a) - number_of(b));
        });
    }
    if (glyph == "×") {
        if (left == nullptr) {
            return monad([](const Value& value) {
                const double number = number_of(value);
                return Value::numbered(number > 0 ? 1 : (number < 0 ? -1 : 0));
            });
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(number_of(a) * number_of(b));
        });
    }
    if (glyph == "÷") {
        if (left == nullptr) {
            if (number_of(right) == 0.0) {
                throw AplError("reciprocal of zero");
            }
            return Value::numbered(1.0 / number_of(right));
        }
        return dyad(divide);
    }
    if (glyph == "*") {
        if (left == nullptr) {
            return monad([](const Value& value) {
                return Value::numbered(std::exp(number_of(value)));
            });
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(std::pow(number_of(a), number_of(b)));
        });
    }
    if (glyph == "⍟") {
        if (left == nullptr) {
            if (number_of(right) <= 0.0) {
                throw AplError("log of a non-positive");
            }
            return Value::numbered(std::log(number_of(right)));
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(std::log(number_of(b)) / std::log(number_of(a)));
        });
    }
    if (glyph == "⌈") {
        if (left == nullptr) {
            return monad([](const Value& value) {
                return Value::numbered(std::ceil(number_of(value)));
            });
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(std::max(number_of(a), number_of(b)));
        });
    }
    if (glyph == "⌊") {
        if (left == nullptr) {
            return monad([](const Value& value) {
                return Value::numbered(std::floor(number_of(value)));
            });
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(std::min(number_of(a), number_of(b)));
        });
    }
    if (glyph == "|") {
        if (left == nullptr) {
            return monad([](const Value& value) {
                return Value::numbered(std::fabs(number_of(value)));
            });
        }
        return dyad(residue);
    }
    if (glyph == "=") {
        if (left == nullptr) {
            throw AplError("= has no monadic form");
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(number_of(a) == number_of(b) ? 1.0 : 0.0);
        });
    }
    if (glyph == "≠") {
        if (left == nullptr) {
            throw AplError("≠ has no monadic form");
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(number_of(a) != number_of(b) ? 1.0 : 0.0);
        });
    }
    if (glyph == "<") {
        if (left == nullptr) {
            throw AplError("< has no monadic form");
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(number_of(a) < number_of(b) ? 1.0 : 0.0);
        });
    }
    if (glyph == "≤") {
        if (left == nullptr) {
            throw AplError("≤ has no monadic form");
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(number_of(a) <= number_of(b) ? 1.0 : 0.0);
        });
    }
    if (glyph == ">") {
        if (left == nullptr) {
            throw AplError("> has no monadic form");
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(number_of(a) > number_of(b) ? 1.0 : 0.0);
        });
    }
    if (glyph == "≥") {
        if (left == nullptr) {
            throw AplError("≥ has no monadic form");
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(number_of(a) >= number_of(b) ? 1.0 : 0.0);
        });
    }
    if (glyph == "∧" || glyph == "∨" || glyph == "⍲" || glyph == "⍱") {
        if (left == nullptr) {
            throw AplError(glyph + " has no monadic form");
        }
        return dyad([glyph](const Value& a, const Value& b) {
            const bool first = number_of(a) != 0.0;
            const bool second = number_of(b) != 0.0;
            bool answer = false;
            if (glyph == "∧") {
                answer = first && second;
            } else if (glyph == "∨") {
                answer = first || second;
            } else if (glyph == "⍲") {
                answer = !(first && second);
            } else {
                answer = !(first || second);
            }
            return Value::numbered(answer ? 1.0 : 0.0);
        });
    }
    if (glyph == "~") {
        if (left != nullptr) {
            throw AplError("~ has no dyadic form");
        }
        return monad([](const Value& value) {
            return Value::numbered(number_of(value) == 0.0 ? 1.0 : 0.0);
        });
    }
    if (glyph == "?") {
        if (left == nullptr) {
            const long long bound = static_cast<long long>(number_of(right));
            if (bound < 1) {
                throw AplError("ROLL needs a positive count");
            }
            return Value::numbered(
                static_cast<double>(1 + std::rand() % bound));
        }
        const long long count = static_cast<long long>(number_of(*left));
        const long long bound = static_cast<long long>(number_of(right));
        if (bound < 1) {
            throw AplError("ROLL needs a positive count");
        }
        std::vector<Value> result;
        result.reserve(static_cast<std::size_t>(std::max(0LL, count)));
        for (long long index = 0; index < count; ++index) {
            result.push_back(Value::numbered(
                static_cast<double>(1 + std::rand() % bound)));
        }
        return Value::array(std::move(result));
    }

    throw AplError(glyph + " is not a function this build knows");
}

}  // namespace apl_detail
}  // namespace timewarp::lang
