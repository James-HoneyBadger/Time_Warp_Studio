// SPDX-License-Identifier: MIT
//
// The Smalltalk class library: one primitive per message the language is taught
// with, in the same shape as `_build_library()` in the reference.
//
// Two things are deliberate rather than incidental. `Transcript show:` and the
// turtle messages answer nil, because a chunk echoes its final value and
// answering the receiver would end a chunk with "a Transcript". And only `true`
// is true: a block that answers nil stops `whileTrue:` rather than looping.

#include "smalltalk_internal.hpp"

#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/turtle_host.hpp"

#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <functional>
#include <string>
#include <utility>
#include <vector>

namespace timewarp::lang {
namespace smalltalk {
namespace {

using Primitive = std::function<Ptr(Ptr, std::vector<Ptr>, FramePtr)>;

Runtime& runtime_of(const FramePtr& frame) { return frame->runtime(); }

ClassPtr make_class(const std::string& name, ClassPtr superclass = nullptr,
                    std::vector<std::string> variables = {}) {
    auto result = std::make_shared<StClass>();
    result->name = name;
    result->superclass = std::move(superclass);
    result->instance_variables = std::move(variables);
    return result;
}

void add(const ClassPtr& klass, const std::string& selector,
         std::vector<std::string> parameters, Primitive primitive,
         bool class_side = false) {
    auto method = std::make_shared<Method>();
    method->selector = selector;
    method->parameters = std::move(parameters);
    method->primitive = std::move(primitive);
    (class_side ? klass->class_methods : klass->methods)[selector] = std::move(method);
}

std::string upper(std::string_view text) {
    std::string result{text};
    for (char& character : result) {
        character =
            static_cast<char>(std::toupper(static_cast<unsigned char>(character)));
    }
    return result;
}

std::string lower(std::string_view text) {
    std::string result{text};
    for (char& character : result) {
        character =
            static_cast<char>(std::tolower(static_cast<unsigned char>(character)));
    }
    return result;
}

std::string trim(std::string_view text) {
    std::size_t start = 0;
    std::size_t end = text.size();
    while (start < end && std::isspace(static_cast<unsigned char>(text[start])) != 0) {
        ++start;
    }
    while (end > start && std::isspace(static_cast<unsigned char>(text[end - 1])) != 0) {
        --end;
    }
    return std::string{text.substr(start, end - start)};
}

std::vector<std::string> split_words(const std::string& text) {
    std::vector<std::string> words;
    std::size_t index = 0;
    while (index < text.size()) {
        while (index < text.size() &&
               std::isspace(static_cast<unsigned char>(text[index])) != 0) {
            ++index;
        }
        const std::size_t start = index;
        while (index < text.size() &&
               std::isspace(static_cast<unsigned char>(text[index])) == 0) {
            ++index;
        }
        if (index > start) {
            words.push_back(text.substr(start, index - start));
        }
    }
    return words;
}

std::vector<Ptr> items_of(const Ptr& value) {
    if (value->kind == Value::Kind::Text) {
        std::vector<Ptr> items;
        items.reserve(value->text.size());
        for (char character : value->text) {
            items.push_back(Value::text_value(std::string(1, character)));
        }
        return items;
    }
    if (value->kind == Value::Kind::Array || value->kind == Value::Kind::Ordered) {
        return value->items;
    }
    throw StError(describe(value) + " is not a collection");
}

bool equals(const Ptr& left, const Ptr& right) {
    if (left->kind == Value::Kind::Symbol || right->kind == Value::Kind::Symbol) {
        return left->kind == Value::Kind::Symbol && right->kind == Value::Kind::Symbol &&
               left->text == right->text;
    }
    if (is_number(left) && is_number(right)) {
        const double a = left->kind == Value::Kind::Integer
                             ? static_cast<double>(left->integer)
                             : left->real;
        const double b = right->kind == Value::Kind::Integer
                             ? static_cast<double>(right->integer)
                             : right->real;
        return a == b;
    }
    if (left->kind == Value::Kind::Boolean || right->kind == Value::Kind::Boolean) {
        return left == right;
    }
    if (left->kind == Value::Kind::Array || left->kind == Value::Kind::Ordered) {
        if (right->kind != Value::Kind::Array && right->kind != Value::Kind::Ordered) {
            return false;
        }
        if (left->items.size() != right->items.size()) {
            return false;
        }
        for (std::size_t index = 0; index < left->items.size(); ++index) {
            if (!equals(left->items[index], right->items[index])) {
                return false;
            }
        }
        return true;
    }
    if (left->kind == Value::Kind::Text && right->kind == Value::Kind::Text) {
        return left->text == right->text;
    }
    return left == right;
}

bool is_kind_of(const Ptr& value, const Ptr& klass_value) {
    if (klass_value->kind != Value::Kind::Class) {
        throw StError("isKindOf: needs a class");
    }
    ClassPtr node = class_of(value);
    while (node != nullptr) {
        if (node == klass_value->klass) {
            return true;
        }
        node = node->superclass;
    }
    return false;
}

long long integer_of(const Ptr& value) {
    if (value->kind == Value::Kind::Integer) {
        return value->integer;
    }
    return static_cast<long long>(value->real);
}

double double_of(const Ptr& value) {
    return value->kind == Value::Kind::Integer ? static_cast<double>(value->integer)
                                               : value->real;
}

Ptr number_add(const Ptr& left, const Ptr& right) {
    if (left->kind == Value::Kind::Integer && right->kind == Value::Kind::Integer) {
        return Value::of_integer(left->integer + right->integer);
    }
    return Value::of_float(double_of(left) + double_of(right));
}

Ptr number_sub(const Ptr& left, const Ptr& right) {
    if (left->kind == Value::Kind::Integer && right->kind == Value::Kind::Integer) {
        return Value::of_integer(left->integer - right->integer);
    }
    return Value::of_float(double_of(left) - double_of(right));
}

Ptr number_mul(const Ptr& left, const Ptr& right) {
    if (left->kind == Value::Kind::Integer && right->kind == Value::Kind::Integer) {
        return Value::of_integer(left->integer * right->integer);
    }
    return Value::of_float(double_of(left) * double_of(right));
}

Primitive guarded(const Primitive& operation) {
    return [operation](Ptr receiver, std::vector<Ptr> arguments, FramePtr frame) -> Ptr {
        if (arguments.size() != 1 || !is_number(arguments[0])) {
            throw StError(arguments.empty() ? "a number is missing"
                                            : describe(arguments[0]) + " is not a number");
        }
        return operation(std::move(receiver), std::move(arguments), std::move(frame));
    };
}

Primitive binary(const std::function<Ptr(const Ptr&, const Ptr&)>& function) {
    return [function](Ptr receiver, std::vector<Ptr> arguments, FramePtr) -> Ptr {
        return function(receiver, arguments.at(0));
    };
}

Ptr to_do(const Ptr& start, const Ptr& stop, const Ptr& step, const Ptr& block) {
    if (step->kind == Value::Kind::Integer && step->integer == 0) {
        throw StError("to:by:do: cannot step by zero");
    }
    if (block->kind != Value::Kind::Block) {
        throw StError("to:do: needs a block");
    }
    Ptr value = start;
    if (step->kind == Value::Kind::Integer ? step->integer > 0 : step->real > 0) {
        while (double_of(value) <= double_of(stop)) {
            block->block->value({value});
            value = number_add(value, step);
        }
    } else {
        while (double_of(value) >= double_of(stop)) {
            block->block->value({value});
            value = number_add(value, step);
        }
    }
    return start;
}

constexpr long long kMaxLoopIterations = 5'000'000;

Ptr loop_block(const Ptr& condition, const Ptr& body, bool wanted) {
    if (condition->kind != Value::Kind::Block) {
        throw StError("whileTrue:/whileFalse: needs a block as its receiver");
    }
    if (body->kind != Value::Kind::Block) {
        throw StError(
            "whileTrue and whileFalse need a body: write [cond] whileTrue: [body]");
    }
    long long guard = 0;
    while (truth_of(condition->block->value({})) == wanted) {
        body->block->value({});
        ++guard;
        if (guard > kMaxLoopIterations) {
            throw StError("the loop ran for too long");
        }
    }
    return Value::nil();
}

}  // namespace

const Library& library() {
    static const Library built = [] {
        Library result;
        result.object = make_class("Object");
        result.class_class = make_class("Class", result.object);
        result.boolean = make_class("Boolean", result.object);
        result.true_class = make_class("True", result.boolean);
        result.false_class = make_class("False", result.boolean);
        result.undefined = make_class("UndefinedObject", result.object);
        result.magnitude = make_class("Magnitude", result.object);
        result.number = make_class("Number", result.magnitude);
        result.integer = make_class("Integer", result.number);
        result.float_class = make_class("Float", result.number);
        result.character = make_class("Character", result.magnitude);
        result.collection = make_class("Collection", result.object);
        result.sequenceable = make_class("SequenceableCollection", result.collection);
        result.array_class = make_class("Array", result.sequenceable);
        result.ordered = make_class("OrderedCollection", result.sequenceable);
        result.string_class = make_class("String", result.sequenceable);
        result.symbol = make_class("Symbol", result.string_class);
        result.block_closure = make_class("BlockClosure", result.object);
        result.transcript = make_class("Transcript", result.object);
        result.turtle = make_class("Turtle", result.object);
        result.sql = make_class("SQL", result.object);

        // -- Object --------------------------------------------------------
        add(result.object, "doesNotUnderstand:", {"aSelector"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) -> Ptr {
                throw StError(describe(receiver) + " does not understand " +
                              print_string(arguments.at(0)));
            });
        add(result.object, "class", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::class_value(class_of(receiver));
        });
        add(result.object, "isNil", {}, [](Ptr, std::vector<Ptr>, FramePtr) {
            return Value::of_boolean(false);
        });
        add(result.object, "notNil", {}, [](Ptr, std::vector<Ptr>, FramePtr) {
            return Value::of_boolean(true);
        });
        add(result.object, "isKindOf:", {"aClass"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                return Value::of_boolean(is_kind_of(receiver, arguments.at(0)));
            });
        add(result.object, "==", {"anObject"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                return Value::of_boolean(receiver == arguments.at(0));
            });
        add(result.object, "~~", {"anObject"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                return Value::of_boolean(receiver != arguments.at(0));
            });
        add(result.object, "=", {"anObject"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                return Value::of_boolean(equals(receiver, arguments.at(0)));
            });
        add(result.object, "~=", {"anObject"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                return Value::of_boolean(!equals(receiver, arguments.at(0)));
            });
        add(result.object, "yourself", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return receiver;
        });
        add(result.object, "printString", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::text_value(print_string(receiver));
        });
        add(result.object, "displayString", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::text_value(display_string(receiver));
        });
        add(result.object, "ifNil:", {"aBlock"}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return receiver;
        });
        add(result.object, "ifNotNil:", {"aBlock"},
            [](Ptr, std::vector<Ptr> arguments, FramePtr) {
                return arguments.at(0)->block->value({});
            });
        add(result.object, "error:", {"aString"},
            [](Ptr, std::vector<Ptr> arguments, FramePtr) -> Ptr {
                throw StError(display_string(arguments.at(0)));
            });

        // -- Class side ----------------------------------------------------
        add(result.class_class, "new", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr frame) -> Ptr {
                if (receiver->kind != Value::Kind::Class) {
                    throw StError("new needs a class");
                }
                Ptr instance = Value::instance(receiver->klass);
                const auto initializer = receiver->klass->lookup("initialize", false);
                if (initializer != nullptr) {
                    initializer->invoke(instance, {}, frame, receiver->klass);
                }
                return instance;
            },
            true);
        add(result.class_class, "name", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                return Value::symbol(receiver->klass->name);
            },
            true);
        add(result.class_class, "superclass", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) -> Ptr {
                return receiver->klass->superclass != nullptr
                           ? Value::class_value(receiver->klass->superclass)
                           : Value::nil();
            },
            true);
        add(result.class_class, "subclass:instanceVariableNames:", {"name", "variables"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr frame) -> Ptr {
                const std::string name = arguments.at(0)->kind == Value::Kind::Symbol
                                             ? arguments.at(0)->text
                                             : display_string(arguments.at(0));
                std::vector<std::string> variables;
                std::string text = display_string(arguments.at(1));
                std::replace(text.begin(), text.end(), ',', ' ');
                for (std::string& word : split_words(text)) {
                    variables.push_back(std::move(word));
                }
                Runtime& runtime = runtime_of(frame);
                if (runtime.classes.count(name) != 0 ||
                    library().globals.count(name) != 0) {
                    throw StError(name + " is already defined");
                }
                ClassPtr klass = make_class(name, receiver->klass, std::move(variables));
                runtime.classes[name] = klass;
                runtime.globals[name] = Value::class_value(klass);
                return Value::class_value(klass);
            },
            true);
        for (const char* selector :
             {"instanceVariableNames:", "classVariableNames:", "package:"}) {
            add(result.class_class, selector, {"variables"},
                [](Ptr receiver, std::vector<Ptr>, FramePtr) { return receiver; }, true);
        }

        // -- nil -----------------------------------------------------------
        add(result.undefined, "isNil", {}, [](Ptr, std::vector<Ptr>, FramePtr) {
            return Value::of_boolean(true);
        });
        add(result.undefined, "notNil", {}, [](Ptr, std::vector<Ptr>, FramePtr) {
            return Value::of_boolean(false);
        });
        add(result.undefined, "ifNil:", {"aBlock"},
            [](Ptr, std::vector<Ptr> arguments, FramePtr) {
                return arguments.at(0)->block->value({});
            });
        add(result.undefined, "ifNotNil:", {"aBlock"},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) { return receiver; });
        add(result.undefined, "ifNil:ifNotNil:", {"nilBlock", "otherBlock"},
            [](Ptr, std::vector<Ptr> arguments, FramePtr) {
                return arguments.at(0)->block->value({});
            });
        add(result.undefined, "printString", {}, [](Ptr, std::vector<Ptr>, FramePtr) {
            return Value::text_value("nil");
        });
        add(result.undefined, "yourself", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return receiver;
        });

        // -- Boolean -------------------------------------------------------
        add(result.true_class, "ifTrue:", {"aBlock"},
            [](Ptr, std::vector<Ptr> arguments, FramePtr) {
                return arguments.at(0)->block->value({});
            });
        add(result.true_class, "ifFalse:", {"aBlock"},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) { return receiver; });
        add(result.true_class, "ifTrue:ifFalse:", {"t", "f"},
            [](Ptr, std::vector<Ptr> arguments, FramePtr) {
                return arguments.at(0)->block->value({});
            });
        add(result.true_class, "ifFalse:ifTrue:", {"f", "t"},
            [](Ptr, std::vector<Ptr> arguments, FramePtr) {
                return arguments.at(1)->block->value({});
            });
        add(result.true_class, "and:", {"aBlock"},
            [](Ptr, std::vector<Ptr> arguments, FramePtr) {
                return arguments.at(0)->block->value({});
            });
        add(result.true_class, "or:", {"aBlock"},
            [](Ptr, std::vector<Ptr>, FramePtr) { return Value::of_boolean(true); });
        add(result.true_class, "not", {}, [](Ptr, std::vector<Ptr>, FramePtr) {
            return Value::of_boolean(false);
        });
        add(result.false_class, "ifTrue:", {"aBlock"},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) { return receiver; });
        add(result.false_class, "ifFalse:", {"aBlock"},
            [](Ptr, std::vector<Ptr> arguments, FramePtr) {
                return arguments.at(0)->block->value({});
            });
        add(result.false_class, "ifTrue:ifFalse:", {"t", "f"},
            [](Ptr, std::vector<Ptr> arguments, FramePtr) {
                return arguments.at(1)->block->value({});
            });
        add(result.false_class, "ifFalse:ifTrue:", {"f", "t"},
            [](Ptr, std::vector<Ptr> arguments, FramePtr) {
                return arguments.at(0)->block->value({});
            });
        add(result.false_class, "and:", {"aBlock"},
            [](Ptr, std::vector<Ptr>, FramePtr) { return Value::of_boolean(false); });
        add(result.false_class, "or:", {"aBlock"},
            [](Ptr, std::vector<Ptr> arguments, FramePtr) {
                return arguments.at(0)->block->value({});
            });
        add(result.false_class, "not", {}, [](Ptr, std::vector<Ptr>, FramePtr) {
            return Value::of_boolean(true);
        });

        // -- Number --------------------------------------------------------
        add(result.number, "+", {"aNumber"}, guarded(binary(number_add)));
        add(result.number, "-", {"aNumber"}, guarded(binary(number_sub)));
        add(result.number, "*", {"aNumber"}, guarded(binary(number_mul)));
        add(result.number, "/", {"aNumber"},
            guarded(binary([](const Ptr& left, const Ptr& right) -> Ptr {
                const double divisor = double_of(right);
                if (divisor == 0.0) {
                    throw StError("division by zero");
                }
                return Value::of_float(double_of(left) / divisor);
            })));
        add(result.number, "//", {"aNumber"},
            guarded(binary([](const Ptr& left, const Ptr& right) -> Ptr {
                const double divisor = double_of(right);
                if (divisor == 0.0) {
                    throw StError("division by zero");
                }
                return Value::of_integer(
                    static_cast<long long>(std::floor(double_of(left) / divisor)));
            })));
        add(result.number, "\\\\", {"aNumber"},
            guarded(binary([](const Ptr& left, const Ptr& right) -> Ptr {
                const double divisor = double_of(right);
                if (divisor == 0.0) {
                    throw StError("division by zero");
                }
                const double quotient = std::floor(double_of(left) / divisor);
                if (left->kind == Value::Kind::Integer &&
                    right->kind == Value::Kind::Integer) {
                    return Value::of_integer(
                        left->integer - right->integer *
                                            static_cast<long long>(quotient));
                }
                return Value::of_float(double_of(left) - divisor * quotient);
            })));
        add(result.number, "quo:", {"aNumber"},
            guarded(binary([](const Ptr& left, const Ptr& right) -> Ptr {
                return Value::of_integer(
                    static_cast<long long>(double_of(left) / double_of(right)));
            })));
        add(result.number, "rem:", {"aNumber"},
            guarded(binary([](const Ptr& left, const Ptr& right) -> Ptr {
                return Value::of_float(std::fmod(double_of(left), double_of(right)));
            })));
        for (const auto& [selector, comparison] :
             std::vector<std::pair<std::string, std::function<bool(double, double)>>>{
                 {"<", [](double a, double b) { return a < b; }},
                 {"<=", [](double a, double b) { return a <= b; }},
                 {">", [](double a, double b) { return a > b; }},
                 {">=", [](double a, double b) { return a >= b; }}}) {
            add(result.number, selector, {"aNumber"},
                [comparison](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                    return Value::of_boolean(
                        comparison(double_of(receiver), double_of(arguments.at(0))));
                });
        }
        add(result.number, "max:", {"aNumber"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) -> Ptr {
                return double_of(receiver) >= double_of(arguments.at(0)) ? receiver
                                                                         : arguments.at(0);
            });
        add(result.number, "min:", {"aNumber"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) -> Ptr {
                return double_of(receiver) <= double_of(arguments.at(0)) ? receiver
                                                                         : arguments.at(0);
            });
        add(result.number, "negated", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) -> Ptr {
                return receiver->kind == Value::Kind::Integer
                           ? Value::of_integer(-receiver->integer)
                           : Value::of_float(-receiver->real);
            });
        add(result.number, "abs", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) -> Ptr {
            return receiver->kind == Value::Kind::Integer
                       ? Value::of_integer(std::llabs(receiver->integer))
                       : Value::of_float(std::fabs(receiver->real));
        });
        add(result.number, "squared", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) { return number_mul(receiver, receiver); });
        add(result.number, "sqrt", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::of_float(std::sqrt(double_of(receiver)));
        });
        add(result.number, "truncated", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::of_integer(static_cast<long long>(std::trunc(double_of(receiver))));
        });
        add(result.number, "rounded", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::of_integer(
                static_cast<long long>(std::nearbyint(double_of(receiver))));
        });
        add(result.number, "asFloat", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::of_float(double_of(receiver));
        });
        add(result.number, "asInteger", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::of_integer(integer_of(receiver));
        });
        add(result.number, "isZero", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::of_boolean(double_of(receiver) == 0.0);
        });
        add(result.number, "isNegative", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::of_boolean(double_of(receiver) < 0.0);
        });
        add(result.number, "even", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::of_boolean(integer_of(receiver) % 2 == 0);
        });
        add(result.number, "odd", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::of_boolean(integer_of(receiver) % 2 != 0);
        });
        add(result.number, "printString", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                return Value::text_value(number_string(receiver));
            });
        add(result.number, "displayString", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                return Value::text_value(number_string(receiver));
            });
        add(result.number, "timesRepeat:", {"aBlock"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) -> Ptr {
                const Ptr& block = arguments.at(0);
                for (long long index = 0; index < integer_of(receiver); ++index) {
                    block->block->value({});
                }
                return receiver;
            });
        add(result.number, "to:do:", {"stop", "aBlock"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                return to_do(receiver, arguments.at(0), Value::of_integer(1),
                             arguments.at(1));
            });
        add(result.number, "to:", {"stop"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) -> Ptr {
                std::vector<Ptr> range;
                for (long long value = integer_of(receiver);
                     value <= integer_of(arguments.at(0)); ++value) {
                    range.push_back(Value::of_integer(value));
                }
                return Value::array(std::move(range));
            });
        add(result.number, "to:by:", {"stop", "step"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) -> Ptr {
                const long long stop = integer_of(arguments.at(0));
                const long long step = integer_of(arguments.at(1));
                if (step == 0) {
                    throw StError("to:by: cannot step by zero");
                }
                std::vector<Ptr> range;
                if (step > 0) {
                    for (long long value = integer_of(receiver); value <= stop; value += step) {
                        range.push_back(Value::of_integer(value));
                    }
                } else {
                    for (long long value = integer_of(receiver); value >= stop; value += step) {
                        range.push_back(Value::of_integer(value));
                    }
                }
                return Value::array(std::move(range));
            });
        add(result.number, "to:by:do:", {"stop", "step", "aBlock"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                return to_do(receiver, arguments.at(0), arguments.at(1), arguments.at(2));
            });

        // -- Character -----------------------------------------------------
        add(result.character, "asString", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::text_value(receiver->text);
        });
        add(result.character, "asInteger", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::of_integer(static_cast<unsigned char>(receiver->text.at(0)));
        });
        add(result.character, "codePoint", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::of_integer(static_cast<unsigned char>(receiver->text.at(0)));
        });
        add(result.character, "printString", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                return Value::text_value("$" + receiver->text);
            });
        add(result.character, "isDigit", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::of_boolean(
                std::isdigit(static_cast<unsigned char>(receiver->text.at(0))) != 0);
        });
        add(result.character, "isLetter", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::of_boolean(
                std::isalpha(static_cast<unsigned char>(receiver->text.at(0))) != 0);
        });

        // -- Collections ---------------------------------------------------
        add(result.collection, "size", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::of_integer(static_cast<long long>(items_of(receiver).size()));
        });
        add(result.collection, "isEmpty", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::of_boolean(items_of(receiver).empty());
        });
        add(result.collection, "notEmpty", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::of_boolean(!items_of(receiver).empty());
        });
        add(result.collection, "do:", {"aBlock"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) -> Ptr {
                for (const Ptr& item : items_of(receiver)) {
                    arguments.at(0)->block->value({item});
                }
                return receiver;
            });
        add(result.collection, "collect:", {"aBlock"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                std::vector<Ptr> collected;
                for (const Ptr& item : items_of(receiver)) {
                    collected.push_back(arguments.at(0)->block->value({item}));
                }
                return Value::array(std::move(collected));
            });
        add(result.collection, "select:", {"aBlock"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                std::vector<Ptr> selected;
                for (const Ptr& item : items_of(receiver)) {
                    if (truth_of(arguments.at(0)->block->value({item}))) {
                        selected.push_back(item);
                    }
                }
                return Value::array(std::move(selected));
            });
        add(result.collection, "reject:", {"aBlock"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                std::vector<Ptr> kept;
                for (const Ptr& item : items_of(receiver)) {
                    if (!truth_of(arguments.at(0)->block->value({item}))) {
                        kept.push_back(item);
                    }
                }
                return Value::array(std::move(kept));
            });
        add(result.collection, "detect:", {"aBlock"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) -> Ptr {
                for (const Ptr& item : items_of(receiver)) {
                    if (truth_of(arguments.at(0)->block->value({item}))) {
                        return item;
                    }
                }
                throw StError("detect: found nothing");
            });
        add(result.collection, "includes:", {"anObject"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                for (const Ptr& item : items_of(receiver)) {
                    if (equals(item, arguments.at(0))) {
                        return Value::of_boolean(true);
                    }
                }
                return Value::of_boolean(false);
            });
        add(result.collection, "inject:into:", {"initial", "aBlock"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) -> Ptr {
                Ptr total = arguments.at(0);
                for (const Ptr& item : items_of(receiver)) {
                    total = arguments.at(1)->block->value({total, item});
                }
                return total;
            });
        add(result.collection, "first", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                return items_of(receiver).front();
            });
        add(result.collection, "last", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                return items_of(receiver).back();
            });
        add(result.collection, "asArray", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::array(items_of(receiver));
        });
        add(result.collection, "printString", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                return Value::text_value(print_string(receiver));
            });
        add(result.collection, "displayString", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                return Value::text_value(print_string(receiver));
            });
        add(result.collection, ",", {"other"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) -> Ptr {
                if (receiver->kind == Value::Kind::Text) {
                    return Value::text_value(receiver->text +
                                             display_string(arguments.at(0)));
                }
                std::vector<Ptr> items = items_of(receiver);
                const std::vector<Ptr> tail = items_of(arguments.at(0));
                items.insert(items.end(), tail.begin(), tail.end());
                return Value::array(std::move(items));
            });
        add(result.collection, "=", {"other"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                return Value::of_boolean(equals(receiver, arguments.at(0)));
            });
        add(result.array_class, "at:", {"index"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) -> Ptr {
                const std::vector<Ptr> items = items_of(receiver);
                const long long index = integer_of(arguments.at(0));
                if (index < 1 || index > static_cast<long long>(items.size())) {
                    throw StError("index " + std::to_string(index) +
                                  " is out of range for a collection of " +
                                  std::to_string(items.size()));
                }
                return items[static_cast<std::size_t>(index - 1)];
            });
        add(result.array_class, "at:put:", {"index", "value"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) -> Ptr {
                const long long index = integer_of(arguments.at(0)) - 1;
                if (index < 0 || index >= static_cast<long long>(receiver->items.size())) {
                    throw StError("index " + print_string(arguments.at(0)) +
                                  " is out of range for a collection of " +
                                  std::to_string(receiver->items.size()));
                }
                receiver->items[static_cast<std::size_t>(index)] = arguments.at(1);
                return arguments.at(1);
            });
        add(result.array_class, "copyFrom:to:", {"start", "stop"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                const std::vector<Ptr> items = items_of(receiver);
                const long long start = integer_of(arguments.at(0)) - 1;
                const long long stop = integer_of(arguments.at(1));
                std::vector<Ptr> copied;
                for (long long index = start;
                     index < stop && index < static_cast<long long>(items.size()); ++index) {
                    if (index >= 0) {
                        copied.push_back(items[static_cast<std::size_t>(index)]);
                    }
                }
                return Value::array(std::move(copied));
            });
        add(result.array_class, "new:", {"size"},
            [](Ptr, std::vector<Ptr> arguments, FramePtr) -> Ptr {
                const long long size = integer_of(arguments.at(0));
                if (size < 0) {
                    throw StError("an array cannot have a negative size");
                }
                return Value::array(std::vector<Ptr>(static_cast<std::size_t>(size),
                                                     Value::nil()));
            },
            true);
        add(result.array_class, "withAll:", {"aCollection"},
            [](Ptr, std::vector<Ptr> arguments, FramePtr) {
                return Value::array(items_of(arguments.at(0)));
            },
            true);
        add(result.ordered, "new", {},
            [](Ptr, std::vector<Ptr>, FramePtr) { return Value::ordered({}); }, true);
        add(result.ordered, "withAll:", {"aCollection"},
            [](Ptr, std::vector<Ptr> arguments, FramePtr) {
                return Value::ordered(items_of(arguments.at(0)));
            },
            true);
        add(result.ordered, "add:", {"anObject"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) -> Ptr {
                receiver->items.push_back(arguments.at(0));
                return arguments.at(0);
            });

        // -- String and Symbol ---------------------------------------------
        add(result.string_class, "size", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return Value::of_integer(static_cast<long long>(receiver->text.size()));
        });
        add(result.string_class, "at:", {"index"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) -> Ptr {
                const long long index = integer_of(arguments.at(0));
                if (index < 1 || index > static_cast<long long>(receiver->text.size())) {
                    throw StError("index " + std::to_string(index) +
                                  " is out of range for a collection of " +
                                  std::to_string(receiver->text.size()));
                }
                return Value::character(
                    std::string(1, receiver->text[static_cast<std::size_t>(index - 1)]));
            });
        add(result.string_class, ",", {"other"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                const Ptr& other = arguments.at(0);
                const std::string tail =
                    other->kind == Value::Kind::Text ||
                            other->kind == Value::Kind::Symbol ||
                            other->kind == Value::Kind::Character
                        ? other->text
                        : display_string(other);
                return Value::text_value(receiver->text + tail);
            });
        add(result.string_class, "asUppercase", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                return Value::text_value(upper(receiver->text));
            });
        add(result.string_class, "asLowercase", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                return Value::text_value(lower(receiver->text));
            });
        add(result.string_class, "reverse", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                std::string text = receiver->text;
                std::reverse(text.begin(), text.end());
                return Value::text_value(std::move(text));
            });
        add(result.string_class, "asSymbol", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                return Value::symbol(receiver->text);
            });
        add(result.string_class, "asString", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                return Value::text_value(receiver->text);
            });
        add(result.string_class, "asNumber", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) -> Ptr {
                const std::string text = trim(receiver->text);
                char* end = nullptr;
                const long long integer = std::strtoll(text.c_str(), &end, 10);
                if (end == text.c_str() + text.size()) {
                    return Value::of_integer(integer);
                }
                end = nullptr;
                const double real = std::strtod(text.c_str(), &end);
                if (end == text.c_str() + text.size()) {
                    return Value::of_float(real);
                }
                throw StError("'" + text + "' is not a number");
            });
        add(result.string_class, "do:", {"aBlock"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) -> Ptr {
                for (const Ptr& item : items_of(receiver)) {
                    arguments.at(0)->block->value({item});
                }
                return receiver;
            });
        add(result.string_class, "isEmpty", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                return Value::of_boolean(receiver->text.empty());
            });
        add(result.string_class, "printString", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                return Value::text_value(print_string(receiver));
            });
        add(result.string_class, "displayString", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                return Value::text_value(receiver->text);
            });
        add(result.string_class, "=", {"other"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                return Value::of_boolean(equals(receiver, arguments.at(0)));
            });
        add(result.symbol, "printString", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                return Value::text_value("#" + receiver->text);
            });
        add(result.symbol, "displayString", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                return Value::text_value(receiver->text);
            });
        add(result.symbol, "asString", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                return Value::text_value(receiver->text);
            });
        add(result.symbol, "=", {"other"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                return Value::of_boolean(arguments.at(0)->kind == Value::Kind::Symbol &&
                                         arguments.at(0)->text == receiver->text);
            });
        add(result.symbol, "value:", {"aString"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                return Value::symbol(receiver->text + print_string(arguments.at(0)));
            });

        // -- BlockClosure ---------------------------------------------------
        add(result.block_closure, "value", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                return receiver->block->value({});
            });
        add(result.block_closure, "value:", {"a"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                return receiver->block->value({arguments.at(0)});
            });
        add(result.block_closure, "value:value:", {"a", "b"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                return receiver->block->value({arguments.at(0), arguments.at(1)});
            });
        add(result.block_closure, "whileTrue:", {"aBlock"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                return loop_block(receiver, arguments.at(0), true);
            });
        add(result.block_closure, "whileFalse:", {"aBlock"},
            [](Ptr receiver, std::vector<Ptr> arguments, FramePtr) {
                return loop_block(receiver, arguments.at(0), false);
            });
        add(result.block_closure, "whileTrue", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return loop_block(receiver, Value::nil(), true);
        });
        add(result.block_closure, "whileFalse", {}, [](Ptr receiver, std::vector<Ptr>, FramePtr) {
            return loop_block(receiver, Value::nil(), false);
        });
        add(result.block_closure, "printString", {},
            [](Ptr, std::vector<Ptr>, FramePtr) {
                return Value::text_value("a BlockClosure");
            });

        // -- Transcript ------------------------------------------------------
        add(result.transcript, "show:", {"anObject"},
            [](Ptr, std::vector<Ptr> arguments, FramePtr frame) -> Ptr {
                runtime_of(frame).transcript_buffer.push_back(
                    display_string(arguments.at(0)));
                return Value::nil();
            });
        add(result.transcript, "cr", {},
            [](Ptr, std::vector<Ptr>, FramePtr frame) -> Ptr {
                Runtime& runtime = runtime_of(frame);
                std::string line;
                for (const std::string& piece : runtime.transcript_buffer) {
                    line += piece;
                }
                runtime.transcript_buffer.clear();
                runtime.emit(std::move(line));
                return Value::nil();
            });
        add(result.transcript, "show:cr:", {"anObject", "aString"},
            [](Ptr, std::vector<Ptr> arguments, FramePtr frame) -> Ptr {
                runtime_of(frame).emit(display_string(arguments.at(0)));
                return Value::nil();
            });
        add(result.transcript, "nl", {}, [](Ptr, std::vector<Ptr>, FramePtr frame) -> Ptr {
            Runtime& runtime = runtime_of(frame);
            std::string line;
            for (const std::string& piece : runtime.transcript_buffer) {
                line += piece;
            }
            runtime.transcript_buffer.clear();
            runtime.emit(std::move(line));
            return Value::nil();
        });
        add(result.transcript, "new", {},
            [](Ptr, std::vector<Ptr>, FramePtr) {
                return Value::instance(library().transcript);
            },
            true);

        // -- Turtle ----------------------------------------------------------
        add(result.turtle, "forward:", {"aNumber"},
            [](Ptr, std::vector<Ptr> arguments, FramePtr frame) -> Ptr {
                Runtime& runtime = runtime_of(frame);
                const double distance = double_of(arguments.at(0));
                if (runtime.turtle != nullptr) {
                    runtime.turtle->forward(distance);
                }
                const double radians = runtime.heading * 3.14159265358979323846 / 180.0;
                runtime.x += distance * std::sin(radians);
                runtime.y += distance * std::cos(radians);
                return Value::nil();
            });
        add(result.turtle, "back:", {"aNumber"},
            [](Ptr, std::vector<Ptr> arguments, FramePtr frame) -> Ptr {
                Runtime& runtime = runtime_of(frame);
                const double distance = double_of(arguments.at(0));
                if (runtime.turtle != nullptr) {
                    runtime.turtle->back(distance);
                }
                const double radians = runtime.heading * 3.14159265358979323846 / 180.0;
                runtime.x -= distance * std::sin(radians);
                runtime.y -= distance * std::cos(radians);
                return Value::nil();
            });
        for (const char* selector : {"right:", "left:"}) {
            const bool right = std::string{selector} == "right:";
            add(result.turtle, selector, {"aNumber"},
                [right](Ptr, std::vector<Ptr> arguments, FramePtr frame) -> Ptr {
                    Runtime& runtime = runtime_of(frame);
                    const double degrees = double_of(arguments.at(0));
                    if (runtime.turtle != nullptr) {
                        if (right) {
                            runtime.turtle->right(degrees);
                        } else {
                            runtime.turtle->left(degrees);
                        }
                    }
                    runtime.heading += right ? degrees : -degrees;
                    while (runtime.heading < 0.0) {
                        runtime.heading += 360.0;
                    }
                    while (runtime.heading >= 360.0) {
                        runtime.heading -= 360.0;
                    }
                    return Value::nil();
                });
        }
        add(result.turtle, "penUp", {}, [](Ptr, std::vector<Ptr>, FramePtr frame) {
            if (runtime_of(frame).turtle != nullptr) {
                runtime_of(frame).turtle->pen_up();
            }
            return Value::nil();
        });
        add(result.turtle, "penDown", {}, [](Ptr, std::vector<Ptr>, FramePtr frame) {
            if (runtime_of(frame).turtle != nullptr) {
                runtime_of(frame).turtle->pen_down();
            }
            return Value::nil();
        });
        add(result.turtle, "home", {}, [](Ptr, std::vector<Ptr>, FramePtr frame) {
            Runtime& runtime = runtime_of(frame);
            if (runtime.turtle != nullptr) {
                runtime.turtle->home();
            }
            runtime.x = 0.0;
            runtime.y = 0.0;
            runtime.heading = 0.0;
            return Value::nil();
        });
        add(result.turtle, "clear", {}, [](Ptr, std::vector<Ptr>, FramePtr frame) {
            if (runtime_of(frame).turtle != nullptr) {
                runtime_of(frame).turtle->clear();
            }
            return Value::nil();
        });
        add(result.turtle, "x", {}, [](Ptr, std::vector<Ptr>, FramePtr frame) {
            return Value::of_float(std::round(runtime_of(frame).x * 1e6) / 1e6);
        });
        add(result.turtle, "y", {}, [](Ptr, std::vector<Ptr>, FramePtr frame) {
            return Value::of_float(std::round(runtime_of(frame).y * 1e6) / 1e6);
        });
        add(result.turtle, "heading", {}, [](Ptr, std::vector<Ptr>, FramePtr frame) {
            return Value::of_float(std::round(runtime_of(frame).heading * 1e6) / 1e6);
        });
        add(result.turtle, "new", {},
            [](Ptr receiver, std::vector<Ptr>, FramePtr) {
                return Value::instance(receiver->klass);
            },
            true);

        // -- SQL -------------------------------------------------------------
        add(result.sql, "run:", {"aString"},
            [](Ptr, std::vector<Ptr> arguments, FramePtr frame) -> Ptr {
                Runtime& runtime = runtime_of(frame);
                if (!runtime.sql) {
                    throw StError("no database is open");
                }
                return Value::text_value(runtime.sql(display_string(arguments.at(0))));
            },
            true);

        result.globals = {
            {"Object", Value::class_value(result.object)},
            {"Class", Value::class_value(result.class_class)},
            {"Boolean", Value::class_value(result.boolean)},
            {"True", Value::of_boolean(true)},
            {"False", Value::of_boolean(false)},
            {"nil", Value::nil()},
            {"Magnitude", Value::class_value(result.magnitude)},
            {"Number", Value::class_value(result.number)},
            {"Integer", Value::class_value(result.integer)},
            {"Float", Value::class_value(result.float_class)},
            {"Character", Value::class_value(result.character)},
            {"Collection", Value::class_value(result.collection)},
            {"SequenceableCollection", Value::class_value(result.sequenceable)},
            {"Array", Value::class_value(result.array_class)},
            {"OrderedCollection", Value::class_value(result.ordered)},
            {"String", Value::class_value(result.string_class)},
            {"Symbol", Value::class_value(result.symbol)},
            {"BlockClosure", Value::class_value(result.block_closure)},
            {"Transcript", Value::instance(result.transcript)},
            {"Turtle", Value::class_value(result.turtle)},
            {"SQL", Value::class_value(result.sql)},
        };
        return result;
    }();
    return built;
}

Ptr make_method_call(const std::shared_ptr<Method>& method, const Ptr& receiver,
                     std::vector<Ptr> arguments, const FramePtr& frame,
                     const ClassPtr& defining) {
    return method->invoke(receiver, std::move(arguments), frame, defining);
}

}  // namespace smalltalk
}  // namespace timewarp::lang
