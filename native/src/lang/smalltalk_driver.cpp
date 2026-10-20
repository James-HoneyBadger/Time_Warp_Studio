// SPDX-License-Identifier: MIT
//
// Reading a Smalltalk file: the chunk splitter, method installation, and the
// top-level echo. Transcribed from `_install_method`, `_run_code` and
// `split_chunks` in the reference.

#include "smalltalk_internal.hpp"

#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/smalltalk.hpp"
#include "timewarp/lang/turtle_host.hpp"

#include <algorithm>
#include <cctype>
#include <regex>
#include <string>
#include <utility>
#include <vector>

namespace timewarp::lang {
namespace smalltalk {
namespace {

const std::regex& method_header() {
    static const std::regex pattern{R"(^\s*([A-Za-z_]\w*)\s*(class)?\s*>>\s*(.*)$)",
                                    std::regex::ECMAScript};
    return pattern;
}

const std::regex& class_header() {
    static const std::regex pattern{R"(^\s*[A-Z]\w*\s+subclass:)",
                                    std::regex::ECMAScript};
    return pattern;
}

bool is_blank(const std::string& line) {
    return std::all_of(line.begin(), line.end(), [](char character) {
        return std::isspace(static_cast<unsigned char>(character)) != 0;
    });
}

/// One top-level unit the driver executes, with the source line it starts on so
/// a breakpoint in the gutter means the same thing here as in a line loop.
struct Chunk {
    std::string text;
    int line = 0;
};

/// Group the source into definitions and blocks of top-level code.
///
/// A definition is a header line followed by *indented* lines and ends at the
/// next line that begins in column 0; top-level lines form one block however they
/// are indented. That is what stands in for a file-in's `!` terminator.
std::vector<Chunk> split_chunks(const std::string& source) {
    std::vector<Chunk> chunks;
    std::vector<std::string> current;
    int current_line = 0;
    bool inside_definition = false;

    const auto flush = [&chunks, &current, &current_line]() {
        if (!current.empty()) {
            Chunk chunk;
            for (const std::string& line : current) {
                if (!chunk.text.empty()) {
                    chunk.text.push_back('\n');
                }
                chunk.text += line;
            }
            chunk.line = current_line;
            chunks.push_back(std::move(chunk));
            current.clear();
        }
    };

    std::size_t start = 0;
    int line_number = 1;
    while (start <= source.size()) {
        const std::size_t end = source.find('\n', start);
        const std::string line =
            source.substr(start, end == std::string::npos ? std::string::npos
                                                          : end - start);
        const int this_line = line_number;
        start = end == std::string::npos ? source.size() + 1 : end + 1;
        if (end != std::string::npos) {
            ++line_number;
        }

        if (std::regex_match(line, method_header()) ||
            std::regex_match(line, class_header())) {
            flush();
            current_line = this_line;
            current.push_back(line);
            inside_definition = true;
            continue;
        }
        if (current.empty()) {
            current_line = this_line;
            current.push_back(line);
            inside_definition = false;
            continue;
        }
        if (inside_definition && !is_blank(line) && !line.empty() &&
            std::isspace(static_cast<unsigned char>(line[0])) == 0) {
            flush();
            current_line = this_line;
            current.push_back(line);
            inside_definition = false;
            continue;
        }
        current.push_back(line);
    }
    flush();

    std::vector<Chunk> result;
    for (Chunk& chunk : chunks) {
        if (!is_blank(chunk.text)) {
            result.push_back(std::move(chunk));
        }
    }
    return result;
}

std::vector<std::string> split_lines(const std::string& text) {
    std::vector<std::string> lines;
    std::size_t start = 0;
    while (start <= text.size()) {
        const std::size_t end = text.find('\n', start);
        lines.push_back(text.substr(start, end == std::string::npos ? std::string::npos
                                                                   : end - start));
        if (end == std::string::npos) {
            break;
        }
        start = end + 1;
    }
    return lines;
}

std::string install_method(const std::string& chunk, Runtime& runtime) {
    const std::vector<std::string> lines = split_lines(chunk);
    std::smatch match;
    if (!std::regex_match(lines.front(), match, method_header())) {
        throw StError("a method definition needs a header");
    }
    const std::string class_name = match[1].str();
    const bool class_side = match[2].matched;
    const std::string pattern = match[3].str();

    std::string body_source;
    for (std::size_t index = 1; index < lines.size(); ++index) {
        if (!body_source.empty()) {
            body_source.push_back('\n');
        }
        body_source += lines[index];
    }

    ClassPtr klass;
    const auto user = runtime.classes.find(class_name);
    if (user != runtime.classes.end()) {
        klass = user->second;
    } else {
        const auto found = library().globals.find(class_name);
        if (found == library().globals.end() ||
            found->second->kind != Value::Kind::Class) {
            throw StError(class_name + " is not a class this build knows");
        }
        klass = found->second->klass;
    }

    std::string selector;
    std::vector<std::string> parameters;
    parse_method_pattern(pattern, selector, parameters);

    std::vector<std::string> temporaries;
    std::vector<NodePtr> statements;
    try {
        statements = parse_statements(body_source, temporaries);
    } catch (const StError& error) {
        throw StError("in " + class_name + " >> " + selector + ": " + error.what());
    }
    if (statements.empty()) {
        throw StError(class_name + " >> " + selector + " has an empty body");
    }
    if (!temporaries.empty()) {
        auto declaration = std::make_shared<Node>();
        declaration->kind = Node::Kind::Temporaries;
        declaration->names = std::move(temporaries);
        statements.insert(statements.begin(), declaration);
    }

    auto& table = class_side ? klass->class_methods : klass->methods;
    if (table.count(selector) != 0) {
        throw StError(class_name + (class_side ? " class" : "") +
                      " already implements " + selector);
    }
    auto method = std::make_shared<Method>();
    method->selector = selector;
    method->parameters = std::move(parameters);
    method->body = std::move(statements);
    table[selector] = std::move(method);

    return (class_side ? class_name + " class" : class_name) + " >> " + selector;
}

std::vector<std::string> run_code(const std::string& chunk, Runtime& runtime) {
    std::vector<std::string> lines;
    std::vector<std::string> temporaries;
    const std::vector<NodePtr> statements = parse_statements(chunk, temporaries);
    if (statements.empty()) {
        return lines;
    }
    FramePtr frame = runtime.workspace_frame();
    for (const std::string& name : temporaries) {
        frame->temporaries.emplace(name, Value::nil());
    }
    Ptr value = Value::nil();
    for (const NodePtr& statement : statements) {
        try {
            value = evaluate(*statement, frame);
        } catch (const NonLocalReturn& unwind) {
            value = unwind.value;
        }
    }
    if (value->kind != Value::Kind::Nil) {
        lines.push_back(print_string(value));
    }
    return lines;
}

std::string join_output(const Runtime& runtime) {
    std::string result;
    for (const std::string& line : runtime.output) {
        result += line;
        if (result.empty() || result.back() != '\n') {
            result.push_back('\n');
        }
    }
    return result;
}

/// Dismantles the frame graph when a run ends.
///
/// A block closure holds the frame that created it, and `| b | b := [:x | ...]`
/// stores that closure in the same frame, so the run owns a reference cycle that
/// `shared_ptr` cannot collect. The frames are dead once the run is over, so
/// clearing their bindings is teardown rather than collection -- and it is a
/// destructor because the run has five exits (two errors, a non-local return
/// outside a method, and the normal end). Without it every program that creates
/// a block leaks its AST under LeakSanitizer.
struct RunTeardown {
    Runtime& runtime;

    ~RunTeardown() {
        for (FramePtr frame = runtime.workspace; frame; frame = frame->parent) {
            frame->temporaries.clear();
            if (frame->activation) {
                frame->activation->variables.clear();
            }
        }
        runtime.globals.clear();
        runtime.classes.clear();
    }
};

}  // namespace

}  // namespace smalltalk

std::string execute_smalltalk(Interpreter& interpreter, std::string_view source,
                              TurtleHost& turtle) {
    using namespace smalltalk;  // NOLINT(google-build-using-namespace)
    Runtime runtime;
    runtime.turtle = &turtle;
    runtime.pause = [&interpreter](int line) {
        return interpreter.pause_if_requested(line);
    };
    const RunTeardown teardown{runtime};

    std::vector<Chunk> chunks;
    try {
        chunks = split_chunks(std::string{source});
    } catch (const StError& error) {
        return std::string{"❌ Smalltalk: "} + error.what() + "\n";
    }

    for (const Chunk& chunk : chunks) {
        // A chunk boundary is the driver's own statement boundary. Message sends
        // inside it pause through `runtime.pause` with their own lines.
        if (!interpreter.pause_if_requested(chunk.line)) {
            return join_output(runtime);
        }
        const std::string first = split_lines(chunk.text).front();
        try {
            if (std::regex_match(first, method_header())) {
                runtime.emit(install_method(chunk.text, runtime));
                continue;
            }
            for (const std::string& line : run_code(chunk.text, runtime)) {
                runtime.emit(line);
            }
        } catch (const DebugStop&) {
            return join_output(runtime);
        } catch (const StError& error) {
            return join_output(runtime) + "❌ Smalltalk: " + error.what() + "\n";
        } catch (const NonLocalReturn& unwind) {
            return join_output(runtime) + "❌ Smalltalk: ^ outside a method returned " +
                   print_string(unwind.value) + "\n";
        } catch (const std::exception& error) {
            return join_output(runtime) + "❌ Smalltalk: " + error.what() + "\n";
        }
    }

    if (!runtime.transcript_buffer.empty()) {
        std::string line;
        for (const std::string& piece : runtime.transcript_buffer) {
            line += piece;
        }
        runtime.transcript_buffer.clear();
        runtime.emit(std::move(line));
    }
    return join_output(runtime);
}

}  // namespace timewarp::lang
