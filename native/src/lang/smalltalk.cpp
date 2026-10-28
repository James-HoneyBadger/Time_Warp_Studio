// SPDX-License-Identifier: MIT
//
// Smalltalk: tokens, the parser, evaluation, dispatch and printing.
//
// The language is *messages all the way down*: `Object subclass: #Thing` is a
// keyword message, `[:x | x]` is a block that closes over its frame, and `^` is
// a non-local return that unwinds to the method that owns it. The parser and the
// send rules here are transcribed from `time_warp/languages/smalltalk.py`
// because that is the behaviour the port is checked against.

#include "smalltalk_internal.hpp"

#include "timewarp/lang/turtle_host.hpp"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace timewarp::lang {
namespace smalltalk {
namespace {

// ---------------------------------------------------------------------------
// Tokens
// ---------------------------------------------------------------------------

enum class TokenKind {
    Number,
    String,
    Char,
    Symbol,
    HashArray,
    Ident,
    Keyword,
    Assign,
    Method,
    Binary,
    LBracket,
    RBracket,
    LParen,
    RParen,
    Dot,
    Semi,
    Bar,
    Caret,
    Colon,
};

struct Token {
    TokenKind kind;
    std::string text;
    /// The 1-based source line the token starts on, so a message send can name
    /// the line the debugger should show.
    int line = 1;
};

/// Newlines in `source[from, to)`, clamped to the string.
int newlines_in(const std::string& source, std::size_t from, std::size_t to) {
    int count = 0;
    for (std::size_t index = from; index < to && index < source.size(); ++index) {
        if (source[index] == '\n') {
            ++count;
        }
    }
    return count;
}

bool is_ident_start(char character) {
    return std::isalpha(static_cast<unsigned char>(character)) != 0 || character == '_';
}

bool is_ident_body(char character) {
    return is_ident_start(character) ||
           std::isdigit(static_cast<unsigned char>(character)) != 0;
}

bool is_binary_char(char character) {
    static const std::string chars = "+-*/\\~<>=&@%,?!";
    return chars.find(character) != std::string::npos;
}

bool is_space(char character) {
    return std::isspace(static_cast<unsigned char>(character)) != 0;
}

bool starts_with(std::string_view text, std::string_view prefix) {
    return text.size() >= prefix.size() && text.substr(0, prefix.size()) == prefix;
}

std::vector<Token> tokenize(const std::string& source, int first_line = 1) {
    std::vector<Token> tokens;
    std::size_t index = 0;
    int line = first_line;
    const std::size_t length = source.size();
    while (index < length) {
        const char character = source[index];
        if (is_space(character)) {
            if (character == '\n') {
                ++line;
            }
            ++index;
            continue;
        }
        if (character == '"') {
            const std::size_t end = source.find('"', index + 1);
            if (end == std::string::npos) {
                throw StError("unterminated comment");
            }
            line += newlines_in(source, index, end + 1);
            index = end + 1;
            continue;
        }
        if (character == '\'') {
            const std::size_t literal_start = index;
            ++index;
            std::string text;
            while (true) {
                if (index >= length) {
                    throw StError("unterminated string");
                }
                if (source[index] == '\'') {
                    if (index + 1 < length && source[index + 1] == '\'') {
                        text.push_back('\'');
                        index += 2;
                        continue;
                    }
                    ++index;
                    break;
                }
                text.push_back(source[index]);
                ++index;
            }
            // The token starts on the line the opening quote was on; the line
            // counter then moves past any newlines inside the literal.
            tokens.push_back({TokenKind::String, std::move(text), line});
            line += newlines_in(source, literal_start, index);
            continue;
        }
        if (character == '$') {
            if (index + 1 >= length) {
                throw StError("a $ must be followed by the character it names");
            }
            tokens.push_back({TokenKind::Char, std::string(1, source[index + 1]), line});
            index += 2;
            continue;
        }
        if (character == '#') {
            if (index + 1 < length && source[index + 1] == '(') {
                tokens.push_back({TokenKind::HashArray, "#(", line});
                index += 2;
                continue;
            }
            if (index + 1 < length && source[index + 1] == '\'') {
                const std::size_t end = source.find('\'', index + 2);
                if (end == std::string::npos) {
                    throw StError("unterminated symbol");
                }
                tokens.push_back(
                    {TokenKind::Symbol, source.substr(index + 2, end - index - 2), line});
                line += newlines_in(source, index, end + 1);
                index = end + 1;
                continue;
            }
            if (index + 1 < length && is_ident_start(source[index + 1])) {
                std::size_t start = index + 1;
                index = start;
                while (index < length && is_ident_body(source[index])) {
                    ++index;
                }
                tokens.push_back({TokenKind::Symbol, source.substr(start, index - start), line});
                continue;
            }
            if (index + 1 < length && is_binary_char(source[index + 1])) {
                std::size_t start = index + 1;
                index = start;
                while (index < length && is_binary_char(source[index])) {
                    ++index;
                }
                tokens.push_back({TokenKind::Symbol, source.substr(start, index - start), line});
                continue;
            }
            throw StError("a # must name a symbol or start an array");
        }
        if (std::isdigit(static_cast<unsigned char>(character)) != 0) {
            const std::size_t start = index;
            while (index < length &&
                   std::isdigit(static_cast<unsigned char>(source[index])) != 0) {
                ++index;
            }
            if (index < length && source[index] == 'r') {
                ++index;
                while (index < length &&
                       std::isalnum(static_cast<unsigned char>(source[index])) != 0) {
                    ++index;
                }
                tokens.push_back({TokenKind::Number, source.substr(start, index - start), line});
                continue;
            }
            if (index < length && source[index] == '.' && index + 1 < length &&
                std::isdigit(static_cast<unsigned char>(source[index + 1])) != 0) {
                ++index;
                while (index < length &&
                       std::isdigit(static_cast<unsigned char>(source[index])) != 0) {
                    ++index;
                }
            }
            if (index < length && (source[index] == 'e' || source[index] == 'E')) {
                std::size_t probe = index + 1;
                if (probe < length && (source[probe] == '+' || source[probe] == '-')) {
                    ++probe;
                }
                if (probe < length &&
                    std::isdigit(static_cast<unsigned char>(source[probe])) != 0) {
                    index = probe;
                    while (index < length &&
                           std::isdigit(static_cast<unsigned char>(source[index])) != 0) {
                        ++index;
                    }
                }
            }
            tokens.push_back({TokenKind::Number, source.substr(start, index - start), line});
            continue;
        }
        if (is_ident_start(character)) {
            const std::size_t start = index;
            while (index < length && is_ident_body(source[index])) {
                ++index;
            }
            if (index < length && source[index] == ':' &&
                !starts_with(std::string_view{source}.substr(index), ":=")) {
                tokens.push_back(
                    {TokenKind::Keyword, source.substr(start, index - start + 1), line});
                ++index;
                continue;
            }
            tokens.push_back({TokenKind::Ident, source.substr(start, index - start), line});
            continue;
        }
        if (starts_with(std::string_view{source}.substr(index), ":=")) {
            tokens.push_back({TokenKind::Assign, ":=", line});
            index += 2;
            continue;
        }
        if (starts_with(std::string_view{source}.substr(index), ">>")) {
            tokens.push_back({TokenKind::Method, ">>", line});
            index += 2;
            continue;
        }
        if (is_binary_char(character)) {
            const std::size_t start = index;
            while (index < length && is_binary_char(source[index])) {
                ++index;
            }
            tokens.push_back({TokenKind::Binary, source.substr(start, index - start), line});
            continue;
        }
        switch (character) {
            case '[':
                tokens.push_back({TokenKind::LBracket, "[", line});
                ++index;
                continue;
            case ']':
                tokens.push_back({TokenKind::RBracket, "]", line});
                ++index;
                continue;
            case '(':
                tokens.push_back({TokenKind::LParen, "(", line});
                ++index;
                continue;
            case ')':
                tokens.push_back({TokenKind::RParen, ")", line});
                ++index;
                continue;
            case '.':
                tokens.push_back({TokenKind::Dot, ".", line});
                ++index;
                continue;
            case ';':
                tokens.push_back({TokenKind::Semi, ";", line});
                ++index;
                continue;
            case '|':
                tokens.push_back({TokenKind::Bar, "|", line});
                ++index;
                continue;
            case '^':
                tokens.push_back({TokenKind::Caret, "^", line});
                ++index;
                continue;
            case ':':
                tokens.push_back({TokenKind::Colon, ":", line});
                ++index;
                continue;
            default:
                break;
        }
        throw StError(std::string{"cannot read '"} + character + "'");
    }
    return tokens;
}

// ---------------------------------------------------------------------------
// Values
// ---------------------------------------------------------------------------

Ptr nil_value() {
    static const Ptr value = [] {
        auto result = std::make_shared<Value>();
        result->kind = Value::Kind::Nil;
        return result;
    }();
    return value;
}

Ptr make_simple(Value::Kind kind) {
    auto value = std::make_shared<Value>();
    value->kind = kind;
    return value;
}

}  // namespace

Ptr Value::nil() { return nil_value(); }

Ptr Value::of_boolean(bool value) {
    auto result = make_simple(Value::Kind::Boolean);
    result->boolean = value;
    return result;
}

Ptr Value::of_integer(long long value) {
    auto result = make_simple(Value::Kind::Integer);
    result->integer = value;
    return result;
}

Ptr Value::of_float(double value) {
    auto result = make_simple(Value::Kind::Float);
    result->real = value;
    return result;
}

Ptr Value::text_value(std::string value) {
    auto result = make_simple(Value::Kind::Text);
    result->text = std::move(value);
    return result;
}

Ptr Value::symbol(std::string value) {
    auto result = make_simple(Value::Kind::Symbol);
    result->text = std::move(value);
    return result;
}

Ptr Value::character(std::string value) {
    auto result = make_simple(Value::Kind::Character);
    result->text = std::move(value);
    return result;
}

Ptr Value::array(std::vector<Ptr> values) {
    auto result = make_simple(Value::Kind::Array);
    result->items = std::move(values);
    return result;
}

Ptr Value::ordered(std::vector<Ptr> values) {
    auto result = make_simple(Value::Kind::Ordered);
    result->items = std::move(values);
    return result;
}

Ptr Value::class_value(ClassPtr value) {
    auto result = make_simple(Value::Kind::Class);
    result->klass = std::move(value);
    return result;
}

Ptr Value::instance(ClassPtr value) {
    auto result = make_simple(Value::Kind::Instance);
    result->klass = std::move(value);
    return result;
}

Ptr Value::block_value(std::shared_ptr<StBlock> value) {
    auto result = make_simple(Value::Kind::Block);
    result->block = std::move(value);
    return result;
}

Ptr Value::super_proxy(Ptr receiver, ClassPtr defining) {
    auto result = make_simple(Value::Kind::Super);
    result->super_receiver = std::move(receiver);
    result->super_defining = std::move(defining);
    return result;
}

bool is_number(const Ptr& value) {
    return value->kind == Value::Kind::Integer || value->kind == Value::Kind::Float;
}

bool truth_of(const Ptr& value) {
    return value->kind == Value::Kind::Boolean && value->boolean;
}

// ---------------------------------------------------------------------------
// Printing
// ---------------------------------------------------------------------------

std::string number_string(const Ptr& value) {
    if (value->kind == Value::Kind::Integer) {
        return std::to_string(value->integer);
    }
    const double number = value->real;
    if (std::isfinite(number) && std::trunc(number) == number &&
        std::fabs(number) < 1e16) {
        return std::to_string(static_cast<long long>(number));
    }
    char buffer[64];
    std::snprintf(buffer, sizeof buffer, "%g", number);
    return buffer;
}

std::string print_string(const Ptr& value) {
    switch (value->kind) {
        case Value::Kind::Nil:
            return "nil";
        case Value::Kind::Boolean:
            return value->boolean ? "true" : "false";
        case Value::Kind::Integer:
        case Value::Kind::Float:
            return number_string(value);
        case Value::Kind::Character:
            return "$" + value->text;
        case Value::Kind::Symbol:
            return "#" + value->text;
        case Value::Kind::Text: {
            std::string result{"'"};
            for (char character : value->text) {
                result.push_back(character);
                if (character == '\'') {
                    result.push_back('\'');
                }
            }
            result.push_back('\'');
            return result;
        }
        case Value::Kind::Array:
        case Value::Kind::Ordered: {
            std::string result{"("};
            for (std::size_t index = 0; index < value->items.size(); ++index) {
                if (index != 0) {
                    result.push_back(' ');
                }
                result += print_string(value->items[index]);
            }
            result.push_back(')');
            return result;
        }
        case Value::Kind::Instance:
            return "a " + value->klass->name;
        case Value::Kind::Class:
            return value->klass->name;
        case Value::Kind::Block:
            return "a BlockClosure";
        case Value::Kind::Super:
            return print_string(value->super_receiver);
    }
    return "nil";
}

std::string display_string(const Ptr& value) {
    if (value->kind == Value::Kind::Text) {
        return value->text;
    }
    return print_string(value);
}

std::string describe(const Ptr& value) {
    if (value->kind == Value::Kind::Instance) {
        return "a " + value->klass->name;
    }
    return print_string(value);
}

// ---------------------------------------------------------------------------
// Object model
// ---------------------------------------------------------------------------

std::shared_ptr<Method> StClass::lookup(const std::string& selector,
                                        bool class_side) const {
    const StClass* node = this;
    while (node != nullptr) {
        const auto& table = class_side ? node->class_methods : node->methods;
        const auto found = table.find(selector);
        if (found != table.end()) {
            return found->second;
        }
        node = node->superclass.get();
    }
    if (class_side) {
        // A class object also answers the universal class-side messages: `new`,
        // `subclass:instanceVariableNames:` and friends live in Class.
        const auto& library = smalltalk::library();
        node = library.class_class.get();
        while (node != nullptr) {
            const auto found = node->class_methods.find(selector);
            if (found != node->class_methods.end()) {
                return found->second;
            }
            node = node->superclass.get();
        }
    }
    return nullptr;
}

std::shared_ptr<Method> StClass::lookup_above(const std::string& selector,
                                              const ClassPtr& start) const {
    const StClass* node = start->superclass.get();
    while (node != nullptr) {
        const auto found = node->methods.find(selector);
        if (found != node->methods.end()) {
            return found->second;
        }
        node = node->superclass.get();
    }
    return nullptr;
}

std::vector<std::string> StClass::all_instance_variables() const {
    std::vector<std::string> names;
    if (superclass != nullptr) {
        names = superclass->all_instance_variables();
    }
    names.insert(names.end(), instance_variables.begin(), instance_variables.end());
    return names;
}

Ptr Method::invoke(const Ptr& receiver, std::vector<Ptr> arguments, FramePtr frame,
                   ClassPtr defining) {
    if (primitive) {
        return primitive(receiver, std::move(arguments), std::move(frame));
    }
    if (arguments.size() != parameters.size()) {
        throw StError(selector + " takes " + std::to_string(parameters.size()) +
                      " argument(s)");
    }
    auto activation = std::make_shared<Activation>();
    activation->receiver = receiver;
    activation->klass = std::move(defining);
    activation->runtime = frame->activation->runtime;
    for (std::size_t index = 0; index < parameters.size(); ++index) {
        activation->variables[parameters[index]] = arguments[index];
    }
    auto inner = std::make_shared<Frame>();
    inner->activation = activation;
    try {
        auto sequence = std::make_shared<Node>();
        sequence->kind = Node::Kind::Sequence;
        sequence->items = body;
        return evaluate(*sequence, inner);
    } catch (const NonLocalReturn& unwind) {
        if (unwind.activation == activation) {
            return unwind.value;
        }
        throw;
    }
}

Ptr StBlock::value(std::vector<Ptr> arguments) {
    if (arguments.size() != parameters.size()) {
        throw StError("a block taking " + std::to_string(parameters.size()) +
                      " argument(s) was given " + std::to_string(arguments.size()));
    }
    auto nested = std::make_shared<Frame>();
    nested->activation = frame->activation;
    nested->parent = frame;
    for (std::size_t index = 0; index < parameters.size(); ++index) {
        nested->temporaries[parameters[index]] = arguments[index];
    }
    // A return inside the block belongs to the method, so it is deliberately
    // not caught here.
    auto sequence = std::make_shared<Node>();
    sequence->kind = Node::Kind::Sequence;
    sequence->items = body;
    return evaluate(*sequence, nested);
}

Runtime& Frame::runtime() const { return *activation->runtime; }

Ptr Frame::self_object() const { return activation->receiver; }

Ptr global_value(Runtime& runtime, const std::string& name) {
    const auto value = runtime.globals.find(name);
    if (value != runtime.globals.end()) {
        return value->second;
    }
    const auto klass = runtime.classes.find(name);
    if (klass != runtime.classes.end()) {
        return Value::class_value(klass->second);
    }
    const auto& globals = smalltalk::library().globals;
    const auto library_value = globals.find(name);
    if (library_value != globals.end()) {
        return library_value->second;
    }
    throw StError("unknown variable or global " + name);
}

Ptr Frame::lookup(const std::string& name) const {
    const Frame* node = this;
    while (node != nullptr) {
        const auto found = node->temporaries.find(name);
        if (found != node->temporaries.end()) {
            return found->second;
        }
        node = node->parent.get();
    }
    const auto variable = activation->variables.find(name);
    if (variable != activation->variables.end()) {
        return variable->second;
    }
    // An instance variable is reached through `self`, and only through `self`.
    const StClass* klass = activation->klass.get();
    const Ptr& receiver = activation->receiver;
    while (klass != nullptr) {
        if (std::find(klass->instance_variables.begin(),
                      klass->instance_variables.end(),
                      name) != klass->instance_variables.end()) {
            if (receiver->kind == Value::Kind::Instance) {
                const auto found = receiver->ivars.find(name);
                return found == receiver->ivars.end() ? nil_value() : found->second;
            }
            return nil_value();
        }
        klass = klass->superclass.get();
    }
    return global_value(runtime(), name);
}

void Frame::assign(const std::string& name, const Ptr& value) {
    Frame* node = this;
    while (node != nullptr) {
        if (node->temporaries.count(name) != 0) {
            node->temporaries[name] = value;
            return;
        }
        node = node->parent.get();
    }
    if (activation->variables.count(name) != 0) {
        activation->variables[name] = value;
        return;
    }
    const StClass* klass = activation->klass.get();
    const Ptr& receiver = activation->receiver;
    while (klass != nullptr) {
        if (std::find(klass->instance_variables.begin(),
                      klass->instance_variables.end(),
                      name) != klass->instance_variables.end()) {
            if (receiver->kind == Value::Kind::Instance) {
                receiver->ivars[name] = value;
            }
            return;
        }
        klass = klass->superclass.get();
    }
    runtime().globals[name] = value;
}

// ---------------------------------------------------------------------------
// Dispatch
// ---------------------------------------------------------------------------

ClassPtr class_of(const Ptr& value) {
    const auto& library = smalltalk::library();
    switch (value->kind) {
        case Value::Kind::Nil:
            return library.undefined;
        case Value::Kind::Boolean:
            return value->boolean ? library.true_class : library.false_class;
        case Value::Kind::Super:
            return class_of(value->super_receiver);
        case Value::Kind::Class:
            return library.class_class;
        case Value::Kind::Instance:
            return value->klass;
        case Value::Kind::Symbol:
            return library.symbol;
        case Value::Kind::Character:
            return library.character;
        case Value::Kind::Text:
            return library.string_class;
        case Value::Kind::Integer:
            return library.integer;
        case Value::Kind::Float:
            return library.float_class;
        case Value::Kind::Block:
            return library.block_closure;
        case Value::Kind::Ordered:
            return library.ordered;
        case Value::Kind::Array:
            return library.array_class;
    }
    return library.object;
}

Ptr send_message(const Ptr& receiver, const std::string& selector,
                 std::vector<Ptr> arguments, const FramePtr& frame, int line) {
    // A primitive is one C++ call that answers rather than a Smalltalk frame -- a
    // stack with `Integer>>+` at every level would bury the program -- so only a
    // method body is recorded. `defining` is the class the lookup started at: the
    // receiver's class for an ordinary send, the superclass for `super`.
    const auto invoke = [&](const Ptr& target, const std::shared_ptr<Method>& method,
                            const ClassPtr& defining, std::string procedure) {
        if (method->primitive) {
            return method->invoke(target, std::move(arguments), frame, defining);
        }
        const ActiveSendScope scope{frame->runtime(), std::move(procedure), line};
        return method->invoke(target, std::move(arguments), frame, defining);
    };
    if (receiver->kind == Value::Kind::Super) {
        const ClassPtr start = receiver->super_defining;
        const auto method = start->lookup_above(selector, start);
        if (method == nullptr) {
            throw StError("super sent " + selector + ", but no superclass of " +
                          start->name + " implements it");
        }
        return invoke(receiver->super_receiver, method, start,
                      start->name + ">>" + selector);
    }
    if (receiver->kind == Value::Kind::Class) {
        const auto method = receiver->klass->lookup(selector, true);
        if (method == nullptr) {
            throw StError(receiver->klass->name + " class does not understand " +
                          selector);
        }
        return invoke(receiver, method, receiver->klass,
                      receiver->klass->name + " class>>" + selector);
    }
    const ClassPtr klass = class_of(receiver);
    const auto method = klass->lookup(selector, false);
    if (method == nullptr) {
        throw StError(describe(receiver) + " does not understand " + selector);
    }
    return invoke(receiver, method, klass, klass->name + ">>" + selector);
}

// ---------------------------------------------------------------------------
// Evaluation
// ---------------------------------------------------------------------------

Ptr evaluate(const Node& node, const FramePtr& frame) {
    switch (node.kind) {
        case Node::Kind::Literal:
            return node.value != nullptr ? node.value : nil_value();
        case Node::Kind::ArrayLiteral: {
            std::vector<Ptr> items;
            items.reserve(node.items.size());
            for (const NodePtr& item : node.items) {
                items.push_back(evaluate(*item, frame));
            }
            return Value::array(std::move(items));
        }
        case Node::Kind::Variable:
            return frame->lookup(node.name);
        case Node::Kind::Assignment: {
            const Ptr value = evaluate(*node.child, frame);
            frame->assign(node.name, value);
            return value;
        }
        case Node::Kind::Self:
            return frame->self_object();
        case Node::Kind::Super:
            return Value::super_proxy(frame->self_object(), frame->activation->klass);
        case Node::Kind::Block: {
            auto block = std::make_shared<StBlock>();
            block->parameters = node.names;
            block->body = node.items;
            block->frame = frame;
            return Value::block_value(std::move(block));
        }
        case Node::Kind::Send: {
            // One pause point per message send, with the line it was written
            // on. The debugger decides whether this send stops the run.
            if (const struct Runtime& runtime = frame->runtime(); runtime.pause) {
                if (!runtime.pause(node.line)) {
                    throw DebugStop{};
                }
            }
            const Ptr receiver = evaluate(*node.child, frame);
            std::vector<Ptr> arguments;
            arguments.reserve(node.arguments.size());
            for (const NodePtr& argument : node.arguments) {
                arguments.push_back(evaluate(*argument, frame));
            }
            Ptr result = send_message(receiver, node.selector, std::move(arguments),
                                      frame, node.line);
            // Every cascaded send goes to the *original* receiver; that is what
            // distinguishes `;` from another message on the result.
            for (const auto& [selector, nodes] : node.cascades) {
                std::vector<Ptr> values;
                values.reserve(nodes.size());
                for (const NodePtr& argument : nodes) {
                    values.push_back(evaluate(*argument, frame));
                }
                result = send_message(receiver, selector, std::move(values), frame,
                                      node.line);
            }
            return result;
        }
        case Node::Kind::Return:
            throw NonLocalReturn(frame->activation, evaluate(*node.child, frame));
        case Node::Kind::Sequence: {
            Ptr result = nil_value();
            for (const NodePtr& statement : node.items) {
                result = evaluate(*statement, frame);
            }
            return result;
        }
        case Node::Kind::Temporaries:
            for (const std::string& name : node.names) {
                frame->temporaries.emplace(name, nil_value());
            }
            return nil_value();
    }
    return nil_value();
}

// ---------------------------------------------------------------------------
// The parser
// ---------------------------------------------------------------------------

class Parser {
public:
    explicit Parser(std::vector<Token> tokens) : tokens_{std::move(tokens)} {}

    std::pair<std::vector<std::string>, std::vector<NodePtr>> parse_body(
        const std::vector<TokenKind>& stop_kinds) {
        std::vector<std::string> temporaries;
        if (accept(TokenKind::Bar)) {
            while (!accept(TokenKind::Bar)) {
                temporaries.push_back(expect(TokenKind::Ident, "a temporary variable name").text);
            }
        }
        std::vector<NodePtr> statements;
        while (true) {
            while (accept(TokenKind::Dot)) {
            }
            const Token* token = peek();
            if (token == nullptr ||
                std::find(stop_kinds.begin(), stop_kinds.end(), token->kind) !=
                    stop_kinds.end()) {
                break;
            }
            statements.push_back(parse_statement());
            if (peek() != nullptr && peek()->kind == TokenKind::Dot) {
                continue;
            }
            break;
        }
        return {std::move(temporaries), std::move(statements)};
    }

private:
    const Token* peek() const {
        return index_ < tokens_.size() ? &tokens_[index_] : nullptr;
    }

    Token advance() {
        if (peek() == nullptr) {
            throw StError("the input ended in the middle of an expression");
        }
        return tokens_[index_++];
    }

    bool accept(TokenKind kind) {
        const Token* token = peek();
        if (token != nullptr && token->kind == kind) {
            ++index_;
            return true;
        }
        return false;
    }

    Token expect(TokenKind kind, const char* what) {
        const Token* token = peek();
        if (token == nullptr || token->kind != kind) {
            const std::string found =
                token == nullptr ? std::string{"the end of the input"}
                                 : "'" + token->text + "'";
            throw StError(std::string{"expected "} + what + ", found " + found);
        }
        return tokens_[index_++];
    }

    NodePtr parse_statement() {
        if (accept(TokenKind::Caret)) {
            auto node = std::make_shared<Node>();
            node->kind = Node::Kind::Return;
            node->child = parse_expression();
            return node;
        }
        return parse_expression();
    }

    NodePtr parse_expression() { return parse_keyword_message(); }

    NodePtr parse_keyword_message() {
        NodePtr receiver = parse_binary_message();
        while (true) {
            const Token* token = peek();
            if (token == nullptr || token->kind != TokenKind::Keyword) {
                return with_cascades(std::move(receiver));
            }
            const int send_line = token->line;
            std::string selector;
            std::vector<NodePtr> arguments;
            while (true) {
                const Token* keyword = peek();
                if (keyword == nullptr || keyword->kind != TokenKind::Keyword) {
                    break;
                }
                ++index_;
                selector += keyword->text;
                arguments.push_back(parse_binary_message());
            }
            auto send = std::make_shared<Node>();
            send->kind = Node::Kind::Send;
            send->line = send_line;
            send->child = std::move(receiver);
            send->selector = std::move(selector);
            send->arguments = std::move(arguments);
            receiver = std::move(send);
        }
    }

    NodePtr parse_binary_message() {
        NodePtr receiver = parse_unary_message();
        while (true) {
            const Token* token = peek();
            if (token == nullptr || token->kind != TokenKind::Binary) {
                return receiver;
            }
            ++index_;
            auto send = std::make_shared<Node>();
            send->kind = Node::Kind::Send;
            send->line = token->line;
            send->child = std::move(receiver);
            send->selector = token->text;
            send->arguments.push_back(parse_unary_message());
            receiver = std::move(send);
        }
    }

    NodePtr parse_unary_message() {
        NodePtr receiver = parse_primary();
        while (true) {
            const Token* token = peek();
            if (token == nullptr || token->kind != TokenKind::Ident) {
                return receiver;
            }
            if (token->text == "self" || token->text == "super" ||
                token->text == "true" || token->text == "false" ||
                token->text == "nil") {
                return receiver;
            }
            ++index_;
            auto send = std::make_shared<Node>();
            send->kind = Node::Kind::Send;
            send->line = token->line;
            send->child = std::move(receiver);
            send->selector = token->text;
            receiver = std::move(send);
        }
    }

    NodePtr with_cascades(NodePtr node) {
        if (node->kind != Node::Kind::Send) {
            return node;
        }
        while (accept(TokenKind::Semi)) {
            const Token* token = peek();
            if (token == nullptr) {
                throw StError("a cascade ended without a message");
            }
            std::string selector;
            std::vector<NodePtr> arguments;
            if (token->kind == TokenKind::Keyword) {
                while (true) {
                    const Token* keyword = peek();
                    if (keyword == nullptr || keyword->kind != TokenKind::Keyword) {
                        break;
                    }
                    ++index_;
                    selector += keyword->text;
                    arguments.push_back(parse_binary_message());
                }
            } else if (token->kind == TokenKind::Binary) {
                ++index_;
                selector = token->text;
                arguments.push_back(parse_unary_message());
            } else if (token->kind == TokenKind::Ident) {
                ++index_;
                selector = token->text;
            } else {
                throw StError("'" + token->text + "' cannot start a cascaded message");
            }
            node->cascades.emplace_back(std::move(selector), std::move(arguments));
        }
        return node;
    }

    NodePtr parse_primary() {
        const Token* token = peek();
        if (token == nullptr) {
            throw StError("the input ended where a value belongs");
        }
        if (token->kind == TokenKind::Number) {
            ++index_;
            return literal(number_of(token->text));
        }
        if (token->kind == TokenKind::String) {
            ++index_;
            return literal(Value::text_value(token->text));
        }
        if (token->kind == TokenKind::Char) {
            ++index_;
            return literal(Value::character(token->text));
        }
        if (token->kind == TokenKind::Symbol) {
            ++index_;
            return literal(Value::symbol(token->text));
        }
        if (token->kind == TokenKind::HashArray) {
            ++index_;
            return parse_array_literal();
        }
        if (token->kind == TokenKind::Ident) {
            ++index_;
            if (token->text == "self") {
                auto node = std::make_shared<Node>();
                node->kind = Node::Kind::Self;
                return node;
            }
            if (token->text == "super") {
                auto node = std::make_shared<Node>();
                node->kind = Node::Kind::Super;
                return node;
            }
            if (token->text == "true") {
                return literal(Value::of_boolean(true));
            }
            if (token->text == "false") {
                return literal(Value::of_boolean(false));
            }
            if (token->text == "nil") {
                return literal(nil_value());
            }
            if (accept(TokenKind::Assign)) {
                auto node = std::make_shared<Node>();
                node->kind = Node::Kind::Assignment;
                node->name = token->text;
                node->child = parse_expression();
                return node;
            }
            auto node = std::make_shared<Node>();
            node->kind = Node::Kind::Variable;
            node->name = token->text;
            return node;
        }
        if (token->kind == TokenKind::LParen) {
            ++index_;
            NodePtr inner = parse_expression();
            expect(TokenKind::RParen, "a closing parenthesis");
            return inner;
        }
        if (token->kind == TokenKind::LBracket) {
            ++index_;
            std::vector<std::string> parameters;
            while (accept(TokenKind::Colon)) {
                parameters.push_back(
                    expect(TokenKind::Ident, "a block argument name").text);
            }
            if (!parameters.empty()) {
                expect(TokenKind::Bar, "a | after a block's arguments");
            }
            auto [temporaries, body] = parse_body({TokenKind::RBracket});
            expect(TokenKind::RBracket, "a closing bracket");
            if (!temporaries.empty()) {
                auto declaration = std::make_shared<Node>();
                declaration->kind = Node::Kind::Temporaries;
                declaration->names = temporaries;
                body.insert(body.begin(), declaration);
            }
            auto node = std::make_shared<Node>();
            node->kind = Node::Kind::Block;
            node->names = std::move(parameters);
            node->items = std::move(body);
            return node;
        }
        if (token->kind == TokenKind::Bar) {
            throw StError(
                "a | here declares temporaries, and Smalltalk allows them only "
                "once, at the start of a method or block");
        }
        throw StError("'" + token->text + "' is not the start of a value");
    }

    NodePtr parse_array_literal() {
        std::vector<NodePtr> items;
        while (true) {
            const Token* token = peek();
            if (token == nullptr) {
                throw StError("an array literal was not closed");
            }
            if (token->kind == TokenKind::RParen) {
                ++index_;
                auto node = std::make_shared<Node>();
                node->kind = Node::Kind::ArrayLiteral;
                node->items = std::move(items);
                return node;
            }
            items.push_back(parse_primary());
        }
    }

    static NodePtr literal(Ptr value) {
        auto node = std::make_shared<Node>();
        node->kind = Node::Kind::Literal;
        node->value = std::move(value);
        return node;
    }

    static Ptr number_of(const std::string& text) {
        const std::size_t radix_mark = text.find('r');
        if (radix_mark != std::string::npos) {
            const long long radix = std::stoll(text.substr(0, radix_mark));
            const std::string digits = text.substr(radix_mark + 1);
            return Value::of_integer(std::stoll(digits, nullptr,
                                                static_cast<int>(radix)));
        }
        if (text.find('.') != std::string::npos || text.find('e') != std::string::npos ||
            text.find('E') != std::string::npos) {
            return Value::of_float(std::stod(text));
        }
        return Value::of_integer(std::stoll(text));
    }

    std::vector<Token> tokens_;
    std::size_t index_ = 0;
};

// The parser is also used by the driver, for method bodies and top-level code.
// `first_line` is where that text starts in the file: the driver hands over a
// chunk, and a send's line has to be the number the gutter shows or a breakpoint
// set by clicking one would never match.
std::vector<NodePtr> parse_statements(std::string_view source,
                                      std::vector<std::string>& temporaries,
                                      int first_line) {
    Parser parser{tokenize(std::string{source}, first_line)};
    auto [names, statements] = parser.parse_body({});
    temporaries = std::move(names);
    return statements;
}

void parse_method_pattern(std::string_view pattern, std::string& selector,
                          std::vector<std::string>& parameters) {
    selector.clear();
    parameters.clear();
    std::string text{pattern};
    const std::size_t comment = text.find('"');
    if (comment != std::string::npos) {
        text = text.substr(0, comment);
    }
    while (!text.empty() && is_space(text.back())) {
        text.pop_back();
    }
    std::size_t start = 0;
    while (start < text.size() && is_space(text[start])) {
        ++start;
    }
    text = text.substr(start);
    if (text.empty()) {
        throw StError("a method definition needs a selector after >>");
    }
    const std::vector<Token> tokens = tokenize(text);
    if (tokens.empty()) {
        throw StError("a method definition needs a selector after >>");
    }
    if (tokens[0].kind == TokenKind::Ident && tokens.size() == 1) {
        selector = tokens[0].text;
        return;
    }
    if (tokens[0].kind == TokenKind::Binary) {
        if (tokens.size() != 2 || tokens[1].kind != TokenKind::Ident) {
            throw StError("'" + text + "' is not a binary method pattern");
        }
        selector = tokens[0].text;
        parameters.push_back(tokens[1].text);
        return;
    }
    if (tokens[0].kind == TokenKind::Keyword) {
        std::size_t index = 0;
        while (index < tokens.size()) {
            if (tokens[index].kind != TokenKind::Keyword) {
                throw StError("'" + text + "' is not a keyword method pattern");
            }
            selector += tokens[index].text;
            ++index;
            if (index >= tokens.size() || tokens[index].kind != TokenKind::Ident) {
                throw StError("'" + text + "' is missing the argument after " +
                              tokens[index - 1].text);
            }
            parameters.push_back(tokens[index].text);
            ++index;
        }
        return;
    }
    throw StError("'" + text + "' is not a method pattern");
}

FramePtr Runtime::workspace_frame() {
    if (workspace == nullptr) {
        auto activation = std::make_shared<Activation>();
        activation->receiver = Value::nil();
        activation->klass = library().object;
        activation->runtime = this;
        workspace = std::make_shared<Frame>();
        workspace->activation = std::move(activation);
    }
    return workspace;
}

}  // namespace smalltalk
}  // namespace timewarp::lang
