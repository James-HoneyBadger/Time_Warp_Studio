// SPDX-License-Identifier: MIT
//
// The object model shared by the Smalltalk port's four translation units.
// Everything a value can be is one struct with a tag, because a Smalltalk
// program can hold any of them in any variable -- there is no static type to
// hang a class hierarchy on.

#pragma once

#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace timewarp::lang {

class Interpreter;
class TurtleHost;

namespace smalltalk {

struct Value;
struct StClass;
struct Method;
struct Frame;
struct Activation;
struct Node;
struct StBlock;

using Ptr = std::shared_ptr<Value>;
using FramePtr = std::shared_ptr<Frame>;
using ActivationPtr = std::shared_ptr<Activation>;
using ClassPtr = std::shared_ptr<StClass>;
using NodePtr = std::shared_ptr<Node>;

/// Raised while reading or running Smalltalk; reported with ❌.
class StError : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

struct Value {
    enum class Kind {
        Nil,
        Boolean,
        Integer,
        Float,
        Text,
        Symbol,
        Character,
        Array,
        Ordered,
        Block,
        Class,
        Instance,
        Super,
    };

    Kind kind = Kind::Nil;
    bool boolean = false;
    long long integer = 0;
    double real = 0.0;
    std::string text;
    std::vector<Ptr> items;
    std::map<std::string, Ptr> ivars;
    ClassPtr klass;
    std::shared_ptr<StBlock> block;
    Ptr super_receiver;
    ClassPtr super_defining;

    static Ptr nil();
    static Ptr of_boolean(bool value);
    static Ptr of_integer(long long value);
    static Ptr of_float(double value);
    static Ptr text_value(std::string value);
    static Ptr symbol(std::string value);
    static Ptr character(std::string value);
    static Ptr array(std::vector<Ptr> values);
    static Ptr ordered(std::vector<Ptr> values);
    static Ptr class_value(ClassPtr value);
    static Ptr instance(ClassPtr value);
    static Ptr block_value(std::shared_ptr<StBlock> value);
    static Ptr super_proxy(Ptr receiver, ClassPtr defining);
};

bool is_number(const Ptr& value);
bool truth_of(const Ptr& value);  // only true is true
std::string print_string(const Ptr& value);
std::string display_string(const Ptr& value);
std::string number_string(const Ptr& value);
/// How a receiver is named in an error message.
std::string describe(const Ptr& value);

struct Method {
    std::string selector;
    std::vector<std::string> parameters;
    std::vector<NodePtr> body;
    std::function<Ptr(Ptr, std::vector<Ptr>, FramePtr)> primitive;

    Ptr invoke(const Ptr& receiver, std::vector<Ptr> arguments, FramePtr frame,
               ClassPtr defining);
};

struct StClass {
    std::string name;
    ClassPtr superclass;
    std::vector<std::string> instance_variables;
    std::map<std::string, std::shared_ptr<Method>> methods;
    std::map<std::string, std::shared_ptr<Method>> class_methods;

    std::shared_ptr<Method> lookup(const std::string& selector, bool class_side) const;
    std::shared_ptr<Method> lookup_above(const std::string& selector,
                                         const ClassPtr& start) const;
    std::vector<std::string> all_instance_variables() const;
};

struct StBlock {
    std::vector<std::string> parameters;
    std::vector<NodePtr> body;
    FramePtr frame;

    Ptr value(std::vector<Ptr> arguments);
};

struct Activation {
    Ptr receiver;
    ClassPtr klass;
    struct Runtime* runtime = nullptr;
    std::map<std::string, Ptr> variables;
};

/// Everything one run owns: globals, classes, the Transcript's pending line.
struct Runtime {
    TurtleHost* turtle = nullptr;
    std::vector<std::string> output;
    std::map<std::string, Ptr> globals;
    std::map<std::string, ClassPtr> classes;
    std::vector<std::string> transcript_buffer;
    FramePtr workspace;
    /// The turtle's pose, mirrored for `x`, `y` and `heading`; the host has no
    /// readback, so the library updates this with the same maths the hosts use.
    double x = 0.0;
    double y = 0.0;
    double heading = 0.0;

    void emit(std::string text) { output.push_back(std::move(text)); }
    FramePtr workspace_frame();
};

struct Frame {
    ActivationPtr activation;
    std::map<std::string, Ptr> temporaries;
    FramePtr parent;

    struct Runtime& runtime() const;
    Ptr self_object() const;
    Ptr lookup(const std::string& name) const;
    void assign(const std::string& name, const Ptr& value);
};

/// ``^``, including from inside a block: unwind to the owning activation.
class NonLocalReturn {
public:
    NonLocalReturn(ActivationPtr target, Ptr returned)
        : activation{std::move(target)}, value{std::move(returned)} {}
    ActivationPtr activation;
    Ptr value;
};

struct Node {
    enum class Kind {
        Literal,
        ArrayLiteral,
        Variable,
        Assignment,
        Self,
        Super,
        Block,
        Send,
        Return,
        Sequence,
        Temporaries,
    };

    Kind kind = Kind::Literal;
    Ptr value;
    std::string name;
    NodePtr child;
    std::vector<NodePtr> items;
    std::vector<std::string> names;
    std::string selector;
    std::vector<NodePtr> arguments;
    std::vector<std::pair<std::string, std::vector<NodePtr>>> cascades;
};

Ptr evaluate(const Node& node, const FramePtr& frame);
Ptr send_message(const Ptr& receiver, const std::string& selector,
                 std::vector<Ptr> arguments, const FramePtr& frame);
ClassPtr class_of(const Ptr& value);

// -- the library, built once by `smalltalk_library.cpp` ----------------------

struct Library {
    ClassPtr object;
    ClassPtr class_class;
    ClassPtr boolean;
    ClassPtr true_class;
    ClassPtr false_class;
    ClassPtr undefined;
    ClassPtr magnitude;
    ClassPtr number;
    ClassPtr integer;
    ClassPtr float_class;
    ClassPtr character;
    ClassPtr collection;
    ClassPtr sequenceable;
    ClassPtr array_class;
    ClassPtr ordered;
    ClassPtr string_class;
    ClassPtr symbol;
    ClassPtr block_closure;
    ClassPtr transcript;
    ClassPtr turtle;
    std::map<std::string, Ptr> globals;
};

const Library& library();

// -- primitives shared with the driver ---------------------------------------

Ptr make_method_call(const std::shared_ptr<Method>& method, const Ptr& receiver,
                     std::vector<Ptr> arguments, const FramePtr& frame,
                     const ClassPtr& defining);

/// Parses a chunk of code into statements. `temporaries` receives any `| a b |`.
std::vector<NodePtr> parse_statements(std::string_view source,
                                      std::vector<std::string>& temporaries);

/// Parses what follows `>>`: `foo`, `+ a`, or `foo: a bar: b`.
void parse_method_pattern(std::string_view pattern, std::string& selector,
                          std::vector<std::string>& parameters);

}  // namespace smalltalk
}  // namespace timewarp::lang
