// SPDX-License-Identifier: MIT
//
// Logo: the C++ port of `native/bridge/time_warp/languages/logo.py`.
//
// Logo is a whole-program language here, although the reference runs it line by
// line, because its *loader* is not the generic one: a `TO ... END` block
// defines a procedure rather than executing, and a command whose brackets span
// several lines is one statement. The reference does that work in
// `_parse_logo_program` before its loop starts, and then hands each joined
// statement to `execute_logo`. Doing both halves here means the statement
// boundaries come from the same code that knows what a procedure is.
//
// The output is streamed statement by statement (`log_output`), so the console
// fills while the program runs rather than at the end, and the `[line N]`
// annotation the IDE puts on an error is applied here with the same numbering
// the reference uses -- the statement's index, because a Logo line has no
// number of its own.
//
// Every quirk below is deliberate, because the reference has it and the
// differential harness compares against the reference. Where a behaviour looks
// like a bug (a list printed as `['1.0', '2.0']`, a `LOCAL "X` that makes a
// variable literally called `"X`), the port reproduces it and says so, rather
// than quietly fixing it and reporting a difference that looks like a defect.

#include "timewarp/lang/logo.hpp"

#include "timewarp/core/charconv.hpp"

#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/mersenne_random.hpp"
#include "timewarp/lang/turtle_host.hpp"

#include <algorithm>
#include <array>
#include <chrono>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <functional>
#include <map>
#include <optional>
#include <random>
#include <set>
#include <stdexcept>
#include <string>
#include <string_view>
#include <thread>
#include <utility>
#include <vector>

namespace timewarp::lang {
namespace {

// ---------------------------------------------------------------------------
// Text, the way Python's str methods behave.
// ---------------------------------------------------------------------------

constexpr double kPi = 3.14159265358979323846;

bool is_space(char character) {
    return character == ' ' || character == '\t' || character == '\n' ||
           character == '\r' || character == '\f' || character == '\v';
}

std::string upper(std::string_view text) {
    std::string result{text};
    for (char& character : result) {
        character = static_cast<char>(std::toupper(static_cast<unsigned char>(character)));
    }
    return result;
}

std::string lstrip(std::string_view text) {
    std::size_t start = 0;
    while (start < text.size() && is_space(text[start])) {
        ++start;
    }
    return std::string{text.substr(start)};
}

std::string rstrip(std::string_view text) {
    std::size_t end = text.size();
    while (end > 0 && is_space(text[end - 1])) {
        --end;
    }
    return std::string{text.substr(0, end)};
}

std::string strip(std::string_view text) { return lstrip(rstrip(text)); }

std::string strip_char(std::string_view text, char character) {
    std::size_t start = 0;
    std::size_t end = text.size();
    while (start < end && text[start] == character) {
        ++start;
    }
    while (end > start && text[end - 1] == character) {
        --end;
    }
    return std::string{text.substr(start, end - start)};
}

std::vector<std::string> split_ws(std::string_view text) {
    std::vector<std::string> parts;
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
            parts.emplace_back(text.substr(start, index - start));
        }
    }
    return parts;
}

/// Splits into at most `limit` pieces on whitespace, the way
/// `str.split(None, limit)` does. Empty pieces are dropped, so the first piece
/// is the command word and the rest is its argument text with the user's case.
std::vector<std::string> split_ws_limit(std::string_view text, std::size_t limit) {
    std::vector<std::string> parts;
    std::size_t index = 0;
    while (index < text.size() && is_space(text[index])) {
        ++index;
    }
    while (index < text.size()) {
        if (parts.size() == limit) {
            parts.emplace_back(rstrip(text.substr(index)));
            return parts;
        }
        const std::size_t start = index;
        while (index < text.size() && !is_space(text[index])) {
            ++index;
        }
        parts.emplace_back(text.substr(start, index - start));
        while (index < text.size() && is_space(text[index])) {
            ++index;
        }
    }
    return parts;
}

/// Python's `splitlines()`: CRLF, a lone CR and LF all end a line, and a
/// trailing newline does not invent an empty last line.
std::vector<std::string> split_lines(std::string_view source) {
    std::vector<std::string> lines;
    std::string current;
    for (std::size_t index = 0; index < source.size(); ++index) {
        const char character = source[index];
        if (character == '\r') {
            lines.push_back(current);
            current.clear();
            if (index + 1 < source.size() && source[index + 1] == '\n') {
                ++index;
            }
        } else if (character == '\n') {
            lines.push_back(current);
            current.clear();
        } else {
            current.push_back(character);
        }
    }
    if (!current.empty()) {
        lines.push_back(current);
    } else if (!source.empty() && source.back() != '\n' && source.back() != '\r') {
        lines.push_back(current);
    }
    return lines;
}

bool starts_with_upper(std::string_view text, std::string_view prefix) {
    return text.size() >= prefix.size() && upper(text.substr(0, prefix.size())) == prefix;
}

// ---------------------------------------------------------------------------
// Numbers, rendered the way Python renders them.
//
// Two different formats are in play and both are compared against the
// reference: `str(float)` (shortest round-trip, always with a decimal point)
// and `f"{value:g}"` (six significant digits). A Logo program tells them apart
// constantly, since PRINT shows one and COUNT measures the other's length.
// ---------------------------------------------------------------------------

std::string repr_real(double value) {
    if (std::isnan(value)) {
        return "nan";
    }
    if (std::isinf(value)) {
        return value < 0.0 ? "-inf" : "inf";
    }
    char buffer[64];
    const std::size_t length =
        timewarp::core::format_double(buffer, buffer + sizeof buffer, value);
    std::string text{buffer, length};
    if (text.find('.') == std::string::npos && text.find('e') == std::string::npos &&
        text.find('n') == std::string::npos && text.find('i') == std::string::npos) {
        text += ".0";
    }
    return text;
}

std::string general_text(double value) {
    if (std::isnan(value)) {
        return "nan";
    }
    if (std::isinf(value)) {
        return value < 0.0 ? "-inf" : "inf";
    }
    char buffer[64];
    const std::size_t length =
        timewarp::core::format_double_general(buffer, buffer + sizeof buffer, value, 6);
    return std::string{buffer, length};
}

/// `str(int(value))` -- Python truncates toward zero.
std::string repr_integer(double value) {
    if (std::isnan(value) || std::isinf(value)) {
        return repr_real(value);
    }
    const double truncated = std::trunc(value);
    char buffer[64];
    const auto result = std::to_chars(buffer, buffer + sizeof buffer,
                                      static_cast<long long>(truncated));
    return std::string{buffer, result.ptr};
}

/// `str(value)` for a value the reference may hold as a float or as a word.
std::string repr_value(double value) { return repr_real(value); }

/// `repr()` of a Python list, which is what `str(list)` prints: strings are
/// quoted with single quotes.
std::string repr_list(const std::vector<std::string>& words) {
    std::string result = "[";
    for (std::size_t index = 0; index < words.size(); ++index) {
        if (index > 0) {
            result += ", ";
        }
        result += "'" + words[index] + "'";
    }
    return result + "]";
}

// ---------------------------------------------------------------------------
// The word lists the reference keeps as module constants.
// ---------------------------------------------------------------------------

bool in_set(const std::set<std::string_view>& words, std::string_view word) {
    return words.find(word) != words.end();
}

/// Every word that can begin a command. A token in this set splits a line.
const std::set<std::string_view>& logo_commands() {
    static const std::set<std::string_view> words{
        "FORWARD",      "FD",          "BACK",       "BK",          "BACKWARD",
        "LEFT",         "LT",          "RIGHT",      "RT",          "PENUP",
        "PU",           "PENDOWN",     "PD",         "HOME",        "CLEARSCREEN",
        "CS",           "CLEAR",       "CLEAN",      "HIDETURTLE",  "HT",
        "SHOWTURTLE",   "ST",          "SETXY",      "SETPOSITION", "SETPOS",
        "SETX",         "SETY",        "SETHEADING", "SETH",        "SETPENCOLOR",
        "SETPC",        "SETCOLOR",    "SETBGCOLOR", "SETBG",       "SETPENWIDTH",
        "SETPW",        "PENWIDTH",    "PENSIZE",    "SETPENSIZE",  "REPEAT",
        "IF",           "IFELSE",      "STOP",       "OUTPUT",      "TO",
        "END",          "MAKE",        "PRINT",      "SHOW",        "TYPE",
        "ARC",          "FILLED",      "PENSTYLE",   "BEZIER",      "LABEL",
        "WAIT",
        "BYE",
        "OP",           "FOREVER",     "REM",        "WHILE",       "UNTIL",
        "FOREACH",      "MAP",         "FILTER",     "REDUCE",      "LOCAL",
        "THING",        "STAMP",       "STAMPCLEAN", "TEXTSIZE",    "SETLABELHEIGHT",
        "DOT",          "PENPAINT",    "PPT",        "PENERASE",    "PE",
        "PENREVERSE",   "PX",          "PENMODE",
    };
    return words;
}

/// Words that take no arguments, so a following procedure name cannot be one.
const std::set<std::string_view>& no_argument_commands() {
    static const std::set<std::string_view> words{
        "PENUP", "PU", "PENDOWN", "PD", "HOME", "CLEARSCREEN", "CS",
        "CLEAN", "CLEAR", "HIDETURTLE", "HT", "SHOWTURTLE", "ST", "STOP", "BYE",
    };
    return words;
}

/// Words that *return* a value. A reporter cannot begin a statement, so the
/// splitter must never start a new command at one: `PRINT THING "X` would
/// otherwise become two commands and print a stray line.
const std::set<std::string_view>& logo_reporters() {
    static const std::set<std::string_view> words{
        "SUM",    "DIFFERENCE", "PRODUCT", "QUOTIENT", "RANDOM", "WORD",
        "LIST",   "SENTENCE",   "FIRST",   "LAST",     "BUTFIRST", "BUTLAST",
        "ITEM",   "THING",      "COUNT",   "REPCOUNT", "POS",    "XCOR",
        "YCOR",   "HEADING",    "ROLL",    "REDUCE",
    };
    return words;
}

/// The reporters whose result can stand in for a colour argument.
const std::set<std::string_view>& colour_reporters() {
    static const std::set<std::string_view> words{
        "SUM",  "DIFFERENCE", "PRODUCT", "QUOTIENT", "RANDOM", "WORD",
        "LIST", "SENTENCE",   "FIRST",   "LAST",     "BUTFIRST", "BUTLAST",
        "ITEM", "THING",
    };
    return words;
}

/// The prefix math functions, which Logo writes as `COS x` and the expression
/// evaluator only understands as `COS(x)`.
const std::set<std::string_view>& math_prefix_functions() {
    static const std::set<std::string_view> words{
        "ACOS", "ASIN",  "ATAN", "CEIL", "CINT",  "COS",  "COSH", "EXP",
        "FIX",  "FLOOR", "INT",  "LOG10", "LOG2", "LOG",  "ROUND", "SGN",
        "SIN",  "SINH",  "SQR",  "SQRT", "TAN",   "TANH",
    };
    return words;
}

/// The functions whose call bracket may be written with a space before it,
/// `ABS (x)`, which the expression evaluator only accepts tight.
const std::set<std::string_view>& spaced_call_functions() {
    static const std::set<std::string_view> words{
        "ABS",  "SIN",  "COS",  "TAN",  "ASIN", "ACOS", "ATAN", "SINH",
        "COSH", "TANH", "SQRT", "SQR",  "FLOOR", "CEIL", "EXP", "LOG",
        "LOG10", "LOG2", "INT",  "ROUND", "SGN",  "MIN",  "MAX",  "POW",
        "LEN",
    };
    return words;
}

const std::map<std::string_view, Rgb>& colour_names() {
    static const std::map<std::string_view, Rgb> table{
        {"BLACK", {0, 0, 0}},       {"WHITE", {255, 255, 255}},
        {"RED", {255, 0, 0}},       {"GREEN", {0, 255, 0}},
        {"BLUE", {0, 0, 255}},      {"YELLOW", {255, 255, 0}},
        {"CYAN", {0, 255, 255}},    {"MAGENTA", {255, 0, 255}},
        {"PINK", {255, 192, 203}},  {"ORANGE", {255, 165, 0}},
        {"PURPLE", {128, 0, 128}},  {"BROWN", {139, 69, 19}},
        {"LIME", {0, 255, 0}},      {"NAVY", {0, 0, 128}},
        {"TEAL", {0, 128, 128}},    {"MAROON", {128, 0, 0}},
        {"OLIVE", {128, 128, 0}},   {"SILVER", {192, 192, 192}},
        {"GOLD", {255, 215, 0}},    {"GRAY", {128, 128, 128}},
        {"GREY", {128, 128, 128}},
    };
    return table;
}

std::optional<Rgb> named_colour(std::string_view name) {
    const auto found = colour_names().find(upper(name));
    if (found == colour_names().end()) {
        return std::nullopt;
    }
    return found->second;
}

int clamp_channel(double value) {
    if (!(value > 0.0)) {
        return 0;
    }
    if (value > 255.0) {
        return 255;
    }
    return static_cast<int>(value);
}

/// `interpreter.variables` holds numbers and words in one map here, so a value
/// the reference would call a string is still a `Value`.
bool looks_like_number(std::string_view text, double& value) {
    const std::string trimmed = strip(text);
    if (trimmed.empty()) {
        return false;
    }
    std::size_t index = 0;
    if (trimmed[index] == '+' || trimmed[index] == '-') {
        ++index;
    }
    bool digit = false;
    bool dot = false;
    for (; index < trimmed.size(); ++index) {
        const char character = trimmed[index];
        if (character >= '0' && character <= '9') {
            digit = true;
        } else if (character == '.' && !dot) {
            dot = true;
        } else if ((character == 'e' || character == 'E') && digit) {
            break;
        } else {
            return false;
        }
    }
    if (!digit) {
        return false;
    }
    try {
        std::size_t used = 0;
        value = std::stod(trimmed, &used);
        return used == trimmed.size();
    } catch (const std::exception&) {
        return false;
    }
}

/// Thrown to unwind a run that has been stopped, timed out or run out of
/// steps. The reference raises out of `tick()` for the same reason.
struct BudgetStop {};

/// Thrown when a Logo error must abandon the current statement.
struct LogoError : std::runtime_error {
    using std::runtime_error::runtime_error;
};

/// Puts the `[line N]` annotation on each `❌` line, as the reference's
/// `_inject_line_number` does. A Logo statement has no number of its own, so
/// `line` is the position the loader gave it.
std::string annotate(std::string_view text, int line) {
    if (text.empty() || text.find("❌") == std::string_view::npos) {
        return std::string{text};
    }
    const std::string display = std::to_string(line);
    std::string result;
    std::size_t start = 0;
    while (start <= text.size()) {
        const std::size_t end = text.find('\n', start);
        const std::string_view current =
            end == std::string_view::npos ? text.substr(start) : text.substr(start, end - start);
        if (!result.empty()) {
            result.push_back('\n');
        }
        if (current.rfind("❌", 0) == 0 && current.find("[line ") == std::string_view::npos &&
            current.find("at line ") == std::string_view::npos) {
            result += "❌ [line " + display + "] " +
                      lstrip(current.substr(std::string_view{"❌"}.size()));
        } else {
            result += current;
        }
        if (end == std::string_view::npos) {
            break;
        }
        start = end + 1;
    }
    return result;
}

// ---------------------------------------------------------------------------
// The machine.
// ---------------------------------------------------------------------------

class LogoMachine {
public:
    LogoMachine(Interpreter& interpreter, TurtleHost& turtle)
        : interpreter_{interpreter}, turtle_{turtle} {}

    /// Loads *source* and runs it, streaming each statement's output.
    void run(std::string_view source);

    /// Runs one command in the caller's interpreter, returning its text.
    ///
    /// BASIC reaches this for statements it does not know, as the reference
    /// does. The machine is built per statement -- procedures cannot span two
    /// BASIC lines -- but the words are folded back into the interpreter so
    /// `MAKE` survives to the next line.
    std::string run_one(std::string_view command);

private:
    // -- loading ---------------------------------------------------------
    void load(std::string_view source);
    void parse_procedure(const std::vector<std::string>& lines, std::size_t start);

    // -- statements ------------------------------------------------------
    std::string run_text(std::string_view text);
    std::string run_command(std::string_view command);
    std::vector<std::string> split_commands(std::string_view text) const;
    bool head_has_all_arguments(const std::vector<std::string>& collected) const;
    std::vector<std::string> parse_body(std::string_view body) const;

    // -- evaluation ------------------------------------------------------
    double eval(std::string_view expression) const;
    double eval_arg(std::string_view argument) const;
    std::optional<double> eval_number(std::string_view argument) const;
    std::string substitute_template_variables(std::string_view expression) const;
    std::vector<std::string> consume_args(const std::vector<std::string>& args,
                                          std::size_t count) const;
    std::string format_value(std::string_view text, bool as_text) const;

    // -- variables -------------------------------------------------------
    void set_number(const std::string& name, double value);
    void set_word(const std::string& name, std::string value);

    // -- turtle ----------------------------------------------------------
    void forward(double distance);
    void back(double distance);
    void right(double degrees);
    void left(double degrees);
    void go_to(double x, double y);
    void set_heading(double degrees);
    void home();
    void clear_screen();

    void tick();

    // -- individual commands ---------------------------------------------
    std::string call_procedure(const std::string& name, const std::vector<std::string>& args);
    std::string do_forward(const std::vector<std::string>& args);
    std::string do_back(const std::vector<std::string>& args);
    std::string do_left(const std::vector<std::string>& args);
    std::string do_right(const std::vector<std::string>& args);
    std::string do_setxy(const std::vector<std::string>& args);
    std::string do_setposition(const std::vector<std::string>& args);
    std::string do_setx(const std::vector<std::string>& args);
    std::string do_sety(const std::vector<std::string>& args);
    std::string do_setheading(const std::vector<std::string>& args);
    std::string do_setcolor(const std::vector<std::string>& args);
    std::string do_setbgcolor(const std::vector<std::string>& args);
    std::string do_setpenwidth(const std::vector<std::string>& args);
    std::string do_repeat(std::string_view command);
    std::string do_if(std::string_view command);
    std::string do_ifelse(std::string_view command);
    std::string do_while(std::string_view command, bool until_mode);
    std::string do_forever(std::string_view command);
    std::string do_output(const std::vector<std::string>& args);
    std::string do_to(std::string_view command);
    std::string do_make(const std::vector<std::string>& args, std::string_view raw);
    std::string do_thing(const std::vector<std::string>& args);
    std::string do_print(const std::vector<std::string>& args, std::string_view raw);
    std::string do_show(const std::vector<std::string>& args);
    std::string do_type(const std::vector<std::string>& args);
    std::string do_word(const std::vector<std::string>& args);
    std::string do_list(const std::vector<std::string>& args);
    std::string do_sentence(const std::vector<std::string>& args);
    std::string do_first(const std::vector<std::string>& args, bool last);
    std::string do_butfirst(const std::vector<std::string>& args, bool last);
    std::string do_item(const std::vector<std::string>& args);
    std::string do_count(const std::vector<std::string>& args);
    std::string do_arithmetic(const std::vector<std::string>& args, char operation);
    std::string do_random(const std::vector<std::string>& args);
    std::string do_arc(const std::vector<std::string>& args);
    std::string do_bezier(const std::vector<std::string>& args);
    std::string do_filled(std::string_view command);
    std::string do_label(std::string_view command);
    std::string do_stamp();
    std::string do_textsize(const std::vector<std::string>& args);
    std::string do_dot(const std::vector<std::string>& args);
    std::string do_foreach(std::string_view command);
    std::string do_higher_order(std::string_view command, char which);
    std::string run_template(std::string_view template_text,
                             const std::map<std::string, Value>& bindings);
    std::string do_local(const std::vector<std::string>& args);
    /// One parameter binding, remembered so a procedure can put it back.
    struct Saved {
        std::string name;
        std::optional<Value> number;
        std::optional<std::string> word;
    };
    void restore(const std::vector<Saved>& saved, double depth);
    std::string resolve_colour_argument(const std::vector<std::string>& args);
    std::vector<std::string> group_rgb_arguments(const std::vector<std::string>& args) const;

    // -- state -----------------------------------------------------------
    Interpreter& interpreter_;
    TurtleHost& turtle_;

    /// Statements in execution order, as the loader joined them.
    std::vector<std::string> statements_;
    /// The number each statement reports in a `[line N]` annotation.
    std::vector<int> statement_lines_;
    /// The physical source line each statement starts on, for the debugger.
    /// Separate from `statement_lines_`: an annotation reports a numbered
    /// statement's own number, which is not where a gutter breakpoint sits.
    std::vector<int> statement_source_lines_;

    std::map<std::string, std::string> procedures_;
    std::map<std::string, std::vector<std::string>> procedure_params_;
    /// The value the most recent OUTPUT produced. The command path carries it
    /// in the "STOP_PROCEDURE" marker, but a caller that uses a procedure as a
    /// reporter (`PRINT DOUBLE 21`) needs the number itself.
    std::optional<double> last_output_;
    std::map<std::string, std::string> words_;
    std::map<std::string, std::vector<std::string>> lists_;

    std::string pen_mode_{"paint"};
    int font_size_ = 12;
    double pen_width_ = 2.0;
    bool pen_down_ = true;
    TurtlePose pose_;
    /// Called after every pose change, as the reference's `on_change` hook is.
    /// FILLED uses it to trace the path the body walks.
    std::function<void()> on_change_;
    MersenneRandom random_;
};

void LogoMachine::tick() {
    interpreter_.note_step();
    if (!interpreter_.check_budget()) {
        throw BudgetStop{};
    }
}

// -- loading ---------------------------------------------------------------

void LogoMachine::load(std::string_view source) {
    const std::vector<std::string> lines = split_lines(source);
    std::vector<std::string> pending;
    int bracket_depth = 0;
    std::size_t index = 0;
    // Where the statement being collected begins in the source, 1-based.
    int pending_source_line = 0;

    const auto flush = [this, &pending, &pending_source_line]() {
        if (pending.empty()) {
            return;
        }
        std::string joined;
        for (const std::string& piece : pending) {
            if (!joined.empty()) {
                joined.push_back(' ');
            }
            joined += piece;
        }
        pending.clear();
        // The reference re-reads a joined statement through its line-number
        // parser, so a statement that begins with a number loses it and
        // reports that number in an error annotation.
        const std::vector<std::string> parts = split_ws_limit(joined, 1);
        int reported = static_cast<int>(statements_.size()) + 1;
        if (!parts.empty() && !parts[0].empty() &&
            std::all_of(parts[0].begin(), parts[0].end(), [](char character) {
                return character >= '0' && character <= '9';
            })) {
            reported = std::atoi(parts[0].c_str());
            statements_.push_back(parts.size() > 1 ? strip(parts[1]) : std::string{});
        } else {
            statements_.push_back(rstrip(joined));
        }
        statement_lines_.push_back(reported);
        statement_source_lines_.push_back(pending_source_line);
    };

    while (index < lines.size()) {
        std::string line = strip(lines[index]);
        if (line.empty()) {
            ++index;
            continue;
        }
        const std::size_t comment = line.find(';');
        if (comment != std::string::npos) {
            line = strip(std::string_view{line}.substr(0, comment));
        }
        if (line.empty()) {
            ++index;
            continue;
        }

        if (bracket_depth == 0 && starts_with_upper(line, "TO ")) {
            flush();
            parse_procedure(lines, index);
            // Skip to the END that closes the definition. The reference skips
            // on the raw line, so `END ; done` does not close a block there
            // either.
            while (index < lines.size() && upper(strip(lines[index])) != "END") {
                ++index;
            }
            if (index < lines.size()) {
                ++index;
            }
            continue;
        }

        if (pending.empty()) {
            pending_source_line = static_cast<int>(index) + 1;
        }
        pending.push_back(line);
        bracket_depth += static_cast<int>(std::count(line.begin(), line.end(), '['));
        bracket_depth -= static_cast<int>(std::count(line.begin(), line.end(), ']'));
        if (bracket_depth <= 0) {
            flush();
            bracket_depth = 0;
        }
        ++index;
    }
    flush();
}

void LogoMachine::parse_procedure(const std::vector<std::string>& lines, std::size_t start) {
    const std::vector<std::string> parts = split_ws(upper(strip(lines[start])));
    if (parts.size() < 2 || parts[0] != "TO") {
        return;
    }
    const std::string name = parts[1];
    std::vector<std::string> params;
    for (std::size_t index = 2; index < parts.size(); ++index) {
        if (parts[index].front() == ':' && parts[index].size() > 1) {
            params.push_back(parts[index].substr(1));
        }
    }
    std::size_t end_index = start + 1;
    while (end_index < lines.size() && upper(strip(lines[end_index])) != "END") {
        ++end_index;
    }
    if (end_index >= lines.size()) {
        return;  // No END: not a procedure, and the reference ignores it too.
    }
    std::string body;
    for (std::size_t index = start + 1; index < end_index; ++index) {
        if (!body.empty()) {
            body.push_back('\n');
        }
        body += strip(lines[index]);
    }
    procedures_[name] = body;
    procedure_params_[name] = params;
}

// -- splitting -------------------------------------------------------------

bool LogoMachine::head_has_all_arguments(const std::vector<std::string>& collected) const {
    if (collected.empty()) {
        return false;
    }
    const std::string head = upper(collected[0]);
    if (in_set(no_argument_commands(), head)) {
        return true;
    }
    const auto found = procedure_params_.find(head);
    if (found == procedure_params_.end()) {
        return false;
    }
    return collected.size() - 1 >= found->second.size();
}

std::vector<std::string> LogoMachine::split_commands(std::string_view text) const {
    // `;` starts a comment, and the splitter must not see command names inside
    // one: `; Demonstrates: FORWARD, RIGHT` used to split mid-comment.
    std::string stripped;
    {
        std::size_t start = 0;
        while (true) {
            const std::size_t end = text.find('\n', start);
            std::string line{text.substr(start, end == std::string_view::npos
                                                    ? std::string_view::npos
                                                    : end - start)};
            const std::size_t comment = line.find(';');
            if (comment != std::string::npos) {
                line.resize(comment);
            }
            if (!stripped.empty()) {
                stripped.push_back('\n');
            }
            stripped += line;
            if (end == std::string_view::npos) {
                break;
            }
            start = end + 1;
        }
    }

    // Tokens: a bracket, a quoted word, or a bare word.
    std::vector<std::string> tokens;
    std::vector<bool> starts_line;
    {
        bool seen_newline = true;
        std::size_t index = 0;
        while (index < stripped.size()) {
            const char character = stripped[index];
            if (is_space(character)) {
                if (character == '\n') {
                    seen_newline = true;
                }
                ++index;
                continue;
            }
            std::size_t end = index + 1;
            if (character == '[' || character == ']') {
                // One character, already consumed.
            } else {
                while (end < stripped.size() && !is_space(stripped[end]) &&
                       stripped[end] != '[' && stripped[end] != ']') {
                    ++end;
                }
            }
            tokens.emplace_back(stripped.substr(index, end - index));
            starts_line.push_back(seen_newline);
            seen_newline = false;
            index = end;
        }
    }

    std::vector<std::string> commands;
    std::vector<std::string> current;
    std::set<std::string> local_procedures;
    int bracket_depth = 0;
    bool in_to_block = false;

    const auto flush = [&commands, &current]() {
        if (current.empty()) {
            return;
        }
        std::string joined;
        for (const std::string& piece : current) {
            if (!joined.empty()) {
                joined.push_back(' ');
            }
            joined += piece;
        }
        current.clear();
        commands.push_back(joined);
    };

    for (std::size_t index = 0; index < tokens.size(); ++index) {
        const std::string& token = tokens[index];
        const bool at_line_start = starts_line[index];
        if (token == "[") {
            ++bracket_depth;
            current.push_back(token);
            continue;
        }
        if (token == "]") {
            --bracket_depth;
            current.push_back(token);
            continue;
        }
        if (bracket_depth == 0) {
            const std::string name = upper(token);
            if (name == "TO") {
                if (!current.empty() && !in_to_block) {
                    flush();
                }
                in_to_block = true;
            } else if (name == "END" && in_to_block) {
                current.push_back(token);
                std::string joined;
                for (const std::string& piece : current) {
                    if (!joined.empty()) {
                        joined.push_back(' ');
                    }
                    joined += piece;
                }
                const std::vector<std::string> header = split_ws(joined);
                if (header.size() >= 2) {
                    local_procedures.insert(upper(header[1]));
                }
                flush();
                in_to_block = false;
                continue;
            }
        }
        if (in_to_block) {
            current.push_back(token);
            continue;
        }

        const std::string name = upper(token);
        bool is_command =
            in_set(logo_commands(), name) && !in_set(logo_reporters(), name);
        if (!is_command && bracket_depth == 0) {
            const bool known =
                local_procedures.find(name) != local_procedures.end() ||
                procedures_.find(name) != procedures_.end();
            if (at_line_start && known) {
                is_command = true;
            } else if (procedures_.find(name) != procedures_.end() &&
                       head_has_all_arguments(current)) {
                is_command = true;
            }
        }
        if (bracket_depth == 0 && is_command) {
            flush();
        }
        current.push_back(token);
    }
    flush();
    return commands;
}

std::string LogoMachine::run_text(std::string_view text) {
    std::string output;
    for (const std::string& command : split_commands(text)) {
        const std::string result = run_command(command);
        output += result;
        if (result.find("STOP_PROCEDURE") != std::string::npos) {
            break;
        }
    }
    return output;
}

// -- evaluation ------------------------------------------------------------

std::string LogoMachine::substitute_template_variables(std::string_view expression) const {
    std::string result;
    for (std::size_t index = 0; index < expression.size();) {
        if (expression[index] == ':' && index + 1 < expression.size() &&
            expression[index + 1] == '?') {
            std::size_t end = index + 2;
            if (end < expression.size() && (expression[end] == '1' || expression[end] == '2')) {
                ++end;
            }
            const std::string key = std::string{expression.substr(index + 1, end - index - 1)};
            const auto found = interpreter_.variables().find(key);
            double value = 0.0;
            if (found != interpreter_.variables().end()) {
                if (found->second.is_number()) {
                    value = found->second.number();
                } else if (looks_like_number(found->second.text(), value)) {
                    // Kept as parsed.
                } else {
                    value = 0.0;
                }
            }
            result += std::trunc(value) == value ? repr_integer(value) : repr_real(value);
            index = end;
            continue;
        }
        result.push_back(expression[index]);
        ++index;
    }
    return result;
}

double LogoMachine::eval(std::string_view expression) const {
    std::string text = substitute_template_variables(expression);

    // `:NAME` becomes `NAME`: the evaluator takes bare names, and upper-cases
    // them itself, so a variable written `:x` finds one MAKEd as `X`.
    std::string without_colons;
    for (std::size_t index = 0; index < text.size();) {
        if (text[index] == ':' && index + 1 < text.size() &&
            (std::isalpha(static_cast<unsigned char>(text[index + 1])) != 0 ||
             text[index + 1] == '_')) {
            ++index;  // Drop the colon only.
            continue;
        }
        without_colons.push_back(text[index]);
        ++index;
    }
    text = std::move(without_colons);

    // `RANDOM n` is `FLOOR(RND * n)`: Logo's RANDOM is an expression here, not
    // a command.
    {
        std::string rewritten;
        std::size_t index = 0;
        while (index < text.size()) {
            const bool boundary =
                index == 0 ||
                std::isalnum(static_cast<unsigned char>(text[index - 1])) == 0;
            if (boundary && index + 6 <= text.size() &&
                upper(text.substr(index, 6)) == "RANDOM") {
                std::size_t cursor = index + 6;
                if (cursor < text.size() && is_space(text[cursor])) {
                    while (cursor < text.size() && is_space(text[cursor])) {
                        ++cursor;
                    }
                    const std::size_t start = cursor;
                    while (cursor < text.size() &&
                           (std::isalnum(static_cast<unsigned char>(text[cursor])) != 0 ||
                            text[cursor] == '_' || text[cursor] == '.')) {
                        ++cursor;
                    }
                    if (cursor > start) {
                        rewritten += "FLOOR(RND * ";
                        rewritten += text.substr(start, cursor - start);
                        rewritten += ")";
                        index = cursor;
                        continue;
                    }
                }
            }
            rewritten.push_back(text[index]);
            ++index;
        }
        text = std::move(rewritten);
    }

    // `(* A B C)` is Logo's prefix multiplication; the evaluator wants infix.
    {
        std::string rewritten;
        std::size_t index = 0;
        while (index < text.size()) {
            if (text[index] == '(') {
                std::size_t star = index + 1;
                while (star < text.size() && is_space(text[star])) {
                    ++star;
                }
                if (star < text.size() && text[star] == '*') {
                    std::size_t cursor = star + 1;
                    while (cursor < text.size() && is_space(text[cursor])) {
                        ++cursor;
                    }
                    const std::size_t content_start = cursor;
                    bool nested = false;
                    while (cursor < text.size()) {
                        if (text[cursor] == '(') {
                            nested = true;
                            break;
                        }
                        if (text[cursor] == ')') {
                            break;
                        }
                        ++cursor;
                    }
                    if (cursor < text.size() && text[cursor] == ')' && !nested) {
                        const std::vector<std::string> parts =
                            split_ws(std::string_view{text}.substr(
                                content_start, cursor - content_start));
                        std::string joined;
                        for (const std::string& part : parts) {
                            if (!joined.empty()) {
                                joined += " * ";
                            }
                            joined += part;
                        }
                        rewritten += "(" + joined + ")";
                        index = cursor + 1;
                        continue;
                    }
                }
            }
            rewritten.push_back(text[index]);
            ++index;
        }
        text = std::move(rewritten);
    }

    // `COS x` -> `COS(x)`, and `ABS (x)` -> `ABS(x)`.
    {
        std::string rewritten;
        std::size_t index = 0;
        while (index < text.size()) {
            const bool boundary =
                index == 0 ||
                (std::isalnum(static_cast<unsigned char>(text[index - 1])) == 0 &&
                 text[index - 1] != '_');
            if (boundary && std::isalpha(static_cast<unsigned char>(text[index])) != 0) {
                std::size_t end = index;
                while (end < text.size() &&
                       (std::isalnum(static_cast<unsigned char>(text[end])) != 0 ||
                        text[end] == '_')) {
                    ++end;
                }
                const std::string name = upper(text.substr(index, end - index));
                if (in_set(math_prefix_functions(), name)) {
                    std::size_t argument = end;
                    while (argument < text.size() && is_space(text[argument])) {
                        ++argument;
                    }
                    if (argument < text.size() && text[argument] != '(') {
                        std::size_t argument_end = argument;
                        while (argument_end < text.size() &&
                               (std::isalnum(static_cast<unsigned char>(text[argument_end])) !=
                                    0 ||
                                text[argument_end] == '_' || text[argument_end] == '.')) {
                            ++argument_end;
                        }
                        if (argument_end > argument) {
                            rewritten += name;
                            rewritten += "(";
                            rewritten += text.substr(argument, argument_end - argument);
                            rewritten += ")";
                            index = argument_end;
                            continue;
                        }
                    }
                    // Already parenthesised: drop the space between the two.
                    rewritten += name;
                    index = argument;
                    continue;
                }
            }
            rewritten.push_back(text[index]);
            ++index;
        }
        text = std::move(rewritten);
    }

    // `ABS (x)` -> `ABS(x)`: Logo allows the space, the evaluator does not.
    {
        std::string rewritten;
        std::size_t index = 0;
        while (index < text.size()) {
            const bool boundary =
                index == 0 ||
                (std::isalnum(static_cast<unsigned char>(text[index - 1])) == 0 &&
                 text[index - 1] != '_');
            if (boundary && std::isalpha(static_cast<unsigned char>(text[index])) != 0) {
                std::size_t end = index;
                while (end < text.size() &&
                       (std::isalnum(static_cast<unsigned char>(text[end])) != 0 ||
                        text[end] == '_')) {
                    ++end;
                }
                const std::string name = upper(text.substr(index, end - index));
                if (in_set(spaced_call_functions(), name)) {
                    std::size_t argument = end;
                    while (argument < text.size() && is_space(text[argument])) {
                        ++argument;
                    }
                    if (argument < text.size() && text[argument] == '(') {
                        rewritten += name;
                        index = argument;
                        continue;
                    }
                }
            }
            rewritten.push_back(text[index]);
            ++index;
        }
        text = std::move(rewritten);
    }

    return interpreter_.evaluate_expression(text);
}

double LogoMachine::eval_arg(std::string_view argument) const {
    const std::string text = strip(argument);
    if (!text.empty() && text.front() == ':' && text.find(' ') == std::string::npos) {
        const std::string name = upper(std::string_view{text}.substr(1));
        const auto found = interpreter_.variables().find(name);
        if (found == interpreter_.variables().end()) {
            throw LogoError("Undefined variable: " + text);
        }
        return found->second.number();
    }
    try {
        return eval(text);
    } catch (const std::exception& error) {
        throw LogoError("Cannot evaluate argument '" + text + "': " + error.what());
    }
}

std::optional<double> LogoMachine::eval_number(std::string_view argument) const {
    try {
        return eval_arg(argument);
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

std::vector<std::string> LogoMachine::consume_args(const std::vector<std::string>& args,
                                                   std::size_t count) const {
    static const std::set<std::string> operators{"+", "-",  "*", "/",  "%",
                                                 "^", "=",  "<", ">", "<=",
                                                 ">=", "<>"};
    std::vector<std::string> consumed;
    std::size_t index = 0;
    for (std::size_t wanted = 0; wanted < count; ++wanted) {
        if (index >= args.size()) {
            break;
        }
        std::vector<std::string> pieces;
        int balance = 0;
        while (index < args.size()) {
            const std::string& token = args[index];
            pieces.push_back(token);
            ++index;
            balance += static_cast<int>(std::count(token.begin(), token.end(), '('));
            balance -= static_cast<int>(std::count(token.begin(), token.end(), ')'));
            // A list literal is one argument, so brackets count too.
            balance += static_cast<int>(std::count(token.begin(), token.end(), '['));
            balance -= static_cast<int>(std::count(token.begin(), token.end(), ']'));
            if (balance != 0) {
                continue;
            }
            if (operators.find(token) != operators.end()) {
                continue;
            }
            if (index < args.size() && operators.find(args[index]) != operators.end()) {
                continue;
            }
            break;
        }
        std::string joined;
        for (const std::string& piece : pieces) {
            if (!joined.empty()) {
                joined.push_back(' ');
            }
            joined += piece;
        }
        consumed.push_back(joined);
    }
    return consumed;
}

// -- variables -------------------------------------------------------------

void LogoMachine::set_number(const std::string& name, double value) {
    interpreter_.set_number(name, value);
    words_.erase(name);
}

void LogoMachine::set_word(const std::string& name, std::string value) {
    words_[name] = std::move(value);
}

// -- turtle ----------------------------------------------------------------

void LogoMachine::forward(double distance) {
    pose_.move(distance);
    turtle_.forward(distance);
    if (on_change_) {
        on_change_();
    }
}

void LogoMachine::back(double distance) {
    pose_.move(-distance);
    turtle_.back(distance);
    if (on_change_) {
        on_change_();
    }
}

void LogoMachine::right(double degrees) {
    pose_.turn(degrees);
    turtle_.right(degrees);
    if (on_change_) {
        on_change_();
    }
}

void LogoMachine::left(double degrees) {
    pose_.turn(-degrees);
    turtle_.left(degrees);
    if (on_change_) {
        on_change_();
    }
}

void LogoMachine::go_to(double x, double y) {
    pose_.go_to(x, y);
    turtle_.goto_xy(x, y);
    if (on_change_) {
        on_change_();
    }
}

void LogoMachine::set_heading(double degrees) {
    pose_.set_heading(degrees);
    turtle_.set_heading(degrees);
    if (on_change_) {
        on_change_();
    }
}

void LogoMachine::home() {
    pose_.go_to(0.0, 0.0);
    pose_.set_heading(0.0);
    turtle_.home();
    if (on_change_) {
        on_change_();
    }
}

void LogoMachine::clear_screen() {
    pose_.home();
    pen_down_ = true;
    pen_width_ = 2.0;
    turtle_.reset();
    if (on_change_) {
        on_change_();
    }
}

// -- helpers shared by several commands ------------------------------------

std::vector<std::string> LogoMachine::group_rgb_arguments(
    const std::vector<std::string>& args) const {
    static const std::set<std::string> operators{"+", "-", "*", "/", "%", "^"};
    std::vector<std::string> groups;
    std::size_t index = 0;
    while (index < args.size() && groups.size() < 3) {
        std::vector<std::string> pieces;
        const std::string& token = args[index];
        pieces.push_back(token);
        ++index;
        if (in_set(math_prefix_functions(), upper(token)) && index < args.size()) {
            pieces.push_back(args[index]);
            ++index;
        }
        while (index + 1 < args.size() &&
               operators.find(args[index]) != operators.end()) {
            pieces.push_back(args[index]);
            pieces.push_back(args[index + 1]);
            index += 2;
        }
        std::string joined;
        for (const std::string& piece : pieces) {
            if (!joined.empty()) {
                joined.push_back(' ');
            }
            joined += piece;
        }
        groups.push_back(joined);
    }
    return groups;
}

std::string LogoMachine::resolve_colour_argument(
    const std::vector<std::string>& args) {
    if (!args.empty() && in_set(colour_reporters(), upper(args[0]))) {
        std::string joined;
        for (const std::string& piece : args) {
            if (!joined.empty()) {
                joined.push_back(' ');
            }
            joined += piece;
        }
        // The reference runs the reporter with no turtle at all. The answer is a
        // value either way, and no reporter is a turtle command.
        const std::string result = run_text(joined);
        const std::string trimmed = strip(result);
        if (trimmed.rfind("❌", 0) == 0) {
            return trimmed;
        }
        return strip_char(trimmed, '"');
    }
    std::string token = strip_char(strip(args.empty() ? std::string{} : args[0]), '"');
    if (!token.empty() && token.front() == ':') {
        const std::string name = upper(std::string_view{token}.substr(1));
        const auto number = interpreter_.variables().find(name);
        if (number != interpreter_.variables().end()) {
            return strip_char(number->second.render(), '"');
        }
        const auto word = words_.find(name);
        if (word != words_.end()) {
            return strip_char(word->second, '"');
        }
        return token;
    }
    return token;
}

// -- dispatch --------------------------------------------------------------

std::string LogoMachine::run_command(std::string_view command_text) {
    std::string command{command_text};
    const std::size_t comment = command.find(';');
    if (comment != std::string::npos) {
        command.resize(comment);
    }
    const std::string trimmed = strip(command);
    if (trimmed.empty()) {
        return {};
    }
    const std::string shouted = upper(trimmed);
    const std::vector<std::string> words = split_ws(shouted);
    if (words.empty()) {
        return {};
    }
    const std::string& name = words[0];
    std::vector<std::string> args(words.begin() + 1, words.end());

    std::string raw_args;
    {
        const std::vector<std::string> parts = split_ws_limit(trimmed, 1);
        if (parts.size() > 1) {
            raw_args = strip(parts[1]);
        }
    }

    // A procedure shadows a primitive, as it does in the reference.
    const auto procedure = procedures_.find(name);
    if (procedure != procedures_.end()) {
        return call_procedure(name, args);
    }

    try {
        if (name == "FORWARD" || name == "FD") {
            return do_forward(args);
        }
        if (name == "BACK" || name == "BK" || name == "BACKWARD") {
            return do_back(args);
        }
        if (name == "LEFT" || name == "LT") {
            return do_left(args);
        }
        if (name == "RIGHT" || name == "RT") {
            return do_right(args);
        }
        if (name == "PENUP" || name == "PU") {
            pen_down_ = false;
            turtle_.pen_up();
            return {};
        }
        if (name == "PENDOWN" || name == "PD") {
            pen_down_ = true;
            turtle_.pen_down();
            return {};
        }
        if (name == "HOME") {
            home();
            return {};
        }
        if (name == "CLEARSCREEN" || name == "CS") {
            clear_screen();
            return {};
        }
        if (name == "CLEAN" || name == "CLEAR") {
            turtle_.clear();
            return {};
        }
        if (name == "HIDETURTLE" || name == "HT") {
            turtle_.hide_turtle();
            return {};
        }
        if (name == "SHOWTURTLE" || name == "ST") {
            turtle_.show_turtle();
            return {};
        }
        if (name == "SETXY") {
            return do_setxy(args);
        }
        if (name == "SETPOSITION" || name == "SETPOS") {
            return do_setposition(args);
        }
        if (name == "SETX") {
            return do_setx(args);
        }
        if (name == "SETY") {
            return do_sety(args);
        }
        if (name == "SETHEADING" || name == "SETH") {
            return do_setheading(args);
        }
        if (name == "SETPENCOLOR" || name == "SETPC" || name == "SETCOLOR") {
            return do_setcolor(args);
        }
        if (name == "SETBGCOLOR" || name == "SETBG") {
            return do_setbgcolor(args);
        }
        if (name == "SETPENWIDTH" || name == "SETPW" || name == "PENWIDTH" ||
            name == "PENSIZE" || name == "SETPENSIZE") {
            return do_setpenwidth(args);
        }
        if (name == "REPEAT") {
            return do_repeat(trimmed);
        }
        if (name == "IF") {
            return do_if(trimmed);
        }
        if (name == "IFELSE") {
            return do_ifelse(trimmed);
        }
        if (name == "STOP") {
            return "STOP_PROCEDURE";
        }
        if (name == "OUTPUT") {
            return do_output(args);
        }
        if (name == "TO") {
            return do_to(trimmed);
        }
        if (name == "END") {
            return {};
        }
        if (name == "MAKE") {
            return do_make(args, raw_args);
        }
        if (name == "THING") {
            return do_thing(args);
        }
        if (name == "PRINT") {
            return do_print(args, raw_args);
        }
        if (name == "SQL") {
            // The rest of the line is the statement: SQL is not a Logo reporter,
            // so it must not be split into Logo words or evaluated.
            return interpreter_.run_sql(raw_args);
        }
        if (name == "SHOW") {
            return do_show(args);
        }
        if (name == "TYPE") {
            return do_type(args);
        }
        if (name == "WORD") {
            return do_word(args);
        }
        if (name == "LIST") {
            return do_list(args);
        }
        if (name == "SENTENCE") {
            return do_sentence(args);
        }
        if (name == "FIRST") {
            return do_first(args, false);
        }
        if (name == "LAST") {
            return do_first(args, true);
        }
        if (name == "BUTFIRST") {
            return do_butfirst(args, false);
        }
        if (name == "BUTLAST") {
            return do_butfirst(args, true);
        }
        if (name == "ITEM") {
            return do_item(args);
        }
        if (name == "COUNT") {
            return do_count(args);
        }
        if (name == "SUM" || name == "+") {
            return do_arithmetic(args, '+');
        }
        if (name == "DIFFERENCE" || name == "-") {
            return do_arithmetic(args, '-');
        }
        if (name == "PRODUCT" || name == "*") {
            return do_arithmetic(args, '*');
        }
        if (name == "QUOTIENT" || name == "/") {
            return do_arithmetic(args, '/');
        }
        if (name == "RANDOM") {
            return do_random(args);
        }
        if (name == "FOREVER") {
            return do_forever(trimmed);
        }
        if (name == "REPCOUNT") {
            return repr_integer(interpreter_.number("REPCOUNT"));
        }
        if (name == "ARC") {
            return do_arc(args);
        }
        if (name == "BEZIER") {
            return do_bezier(args);
        }
        if (name == "FILLED") {
            return do_filled(trimmed);
        }
        if (name == "PENSTYLE") {
            // PENSTYLE n [, length] -- 0 solid, 1 dashed, 2 dotted, 3 dash-dot,
            // 4 invisible. A native extension rather than a reference command: the
            // canvas learned to draw a dashed pen in 19.0 and a pen style nothing
            // can ask for is a feature nobody has.
            // `args` is already split by the dispatcher.
            const std::vector<std::string>& fields = args;
            if (fields.empty()) {
                return "❌ PENSTYLE needs a style number (0 solid, 1 dashed, "
                       "2 dotted, 3 dash-dot, 4 invisible)\n";
            }
            const std::optional<double> style = eval_number(fields[0]);
            if (!style.has_value()) {
                return "❌ PENSTYLE needs a number, got '" + fields[0] + "'\n";
            }
            const int which = static_cast<int>(*style);
            if (which < 0 || which > 4) {
                return "❌ Value " + std::to_string(which) +
                       " for PENSTYLE is outside 0-4\n";
            }
            double length = 1.0;
            if (fields.size() >= 2) {
                if (const std::optional<double> given = eval_number(fields[1])) {
                    length = *given;
                }
            }
            turtle_.set_pen_style(which, length);
            return {};
        }
        if (name == "LABEL") {
            return do_label(trimmed);
        }
        if (name == "WAIT") {
            if (!args.empty()) {
                const std::optional<double> ticks = eval_number(args[0]);
                if (!ticks.has_value()) {
                    return "❌ WAIT requires a numeric argument\n";
                }
                const double seconds = std::max(0.0, *ticks / 60.0);
                if (seconds > 0.0) {
                    std::this_thread::sleep_for(std::chrono::duration<double>(seconds));
                }
            }
            return {};
        }
        if (name == "BYE") {
            interpreter_.running = false;
            return "✅ Goodbye!\n";
        }
        if (name == "OP" || name == "OUTPUT") {
            if (!args.empty()) {
                const std::optional<double> value = eval_number(args[0]);
                if (value.has_value()) {
                    return repr_value(*value);
                }
                std::string joined;
                for (const std::string& piece : args) {
                    if (!joined.empty()) {
                        joined.push_back(' ');
                    }
                    joined += piece;
                }
                return joined;
            }
            return {};
        }
        if (name == "REM") {
            return {};
        }
        if (name == "FOREACH") {
            return do_foreach(trimmed);
        }
        if (name == "MAP") {
            return do_higher_order(trimmed, 'm');
        }
        if (name == "FILTER") {
            return do_higher_order(trimmed, 'f');
        }
        if (name == "REDUCE") {
            return do_higher_order(trimmed, 'r');
        }
        if (name == "WHILE") {
            return do_while(trimmed, false);
        }
        if (name == "UNTIL") {
            return do_while(trimmed, true);
        }
        if (name == "LOCAL") {
            return do_local(args);
        }
        if (name == "WINDOW" || name == "WRAP" || name == "FENCE") {
            return "ℹ️ Screen mode: " + name + "\n";
        }
        if (name == "STAMP" || name == "STAMPCLEAN") {
            // The reference stamps for both spellings; STAMPCLEAN is otherwise
            // the same call.
            return do_stamp();
        }
        if (name == "TEXTSIZE" || name == "SETLABELHEIGHT") {
            return do_textsize(args);
        }
        if (name == "DOT") {
            return do_dot(args);
        }
        if (name == "PENPAINT" || name == "PPT") {
            pen_down_ = true;
            turtle_.pen_down();
            return {};
        }
        if (name == "PENERASE" || name == "PE") {
            pen_mode_ = "erase";
            return {};
        }
        if (name == "PENREVERSE" || name == "PX") {
            pen_mode_ = "reverse";
            return {};
        }
        if (name == "PENMODE") {
            return pen_mode_ + "\n";
        }
        if (name == "ENABLE3D" || name == "3DON") {
            pose_.enable_3d();
            turtle_.enable_3d();
            return "🐢 3D turtle mode enabled\n";
        }
        if (name == "DISABLE3D" || name == "3DOFF" || name == "2DON") {
            pose_.disable_3d();
            turtle_.disable_3d();
            return "🐢 2D turtle mode restored\n";
        }
        if (name == "PITCH") {
            if (args.empty()) {
                return "❌ PITCH requires an angle\n";
            }
            const std::optional<double> angle = eval_number(args[0]);
            if (!angle.has_value()) {
                return "❌ Expected a number, got: " + args[0] + "\n";
            }
            if (!pose_.is_3d()) {
                pose_.enable_3d();
                turtle_.enable_3d();
            }
            pose_.pitch(*angle);
            turtle_.pitch(*angle);
            return {};
        }
        if (name == "ROLL" || name == "TILTROLL") {
            if (args.empty()) {
                return "❌ ROLL requires an angle\n";
            }
            const std::optional<double> angle = eval_number(args[0]);
            if (!angle.has_value()) {
                return "❌ Expected a number, got: " + args[0] + "\n";
            }
            if (!pose_.is_3d()) {
                pose_.enable_3d();
                turtle_.enable_3d();
            }
            pose_.roll(*angle);
            turtle_.roll(*angle);
            return {};
        }
        if (name == "YAW") {
            if (args.empty()) {
                return "❌ YAW requires an angle\n";
            }
            const std::optional<double> angle = eval_number(args[0]);
            if (!angle.has_value()) {
                return "❌ Expected a number, got: " + args[0] + "\n";
            }
            if (!pose_.is_3d()) {
                pose_.enable_3d();
                turtle_.enable_3d();
            }
            right(*angle);
            return {};
        }
        if (name == "TILTUP" || name == "UPWARD" || name == "UP") {
            double angle = 45.0;
            if (!args.empty()) {
                const std::optional<double> value = eval_number(args[0]);
                if (value.has_value()) {
                    angle = *value;
                }
            }
            if (!pose_.is_3d()) {
                pose_.enable_3d();
                turtle_.enable_3d();
            }
            pose_.pitch(angle);
            turtle_.pitch(angle);
            return {};
        }
        if (name == "TILTDOWN" || name == "DOWNWARD" || name == "DOWN") {
            double angle = 45.0;
            if (!args.empty()) {
                const std::optional<double> value = eval_number(args[0]);
                if (value.has_value()) {
                    angle = *value;
                }
            }
            if (!pose_.is_3d()) {
                pose_.enable_3d();
                turtle_.enable_3d();
            }
            pose_.pitch(-angle);
            turtle_.pitch(-angle);
            return {};
        }
    } catch (const BudgetStop&) {
        throw;
    } catch (const std::exception& error) {
        return std::string{"❌ "} + error.what() + "\n";
    }

    // A statement that is just an expression, which is how a template such as
    // `[:? * 2]` produces a value.
    if (!name.empty() && name.front() == ':') {
        std::string expression;
        for (const std::string& piece : words) {
            if (!expression.empty()) {
                expression.push_back(' ');
            }
            expression += piece;
        }
        try {
            return repr_value(eval(expression));
        } catch (const std::exception&) {
            // Falls through to the unknown-command message.
        }
    }
    return "❌ Unknown Logo command: " + name + "\n";
}

// -- motion ----------------------------------------------------------------

std::string LogoMachine::do_forward(const std::vector<std::string>& args) {
    const std::vector<std::string> consumed = consume_args(args, 1);
    if (consumed.size() != 1) {
        return "❌ FORWARD expects 1 argument(s), got " +
               std::to_string(consumed.size()) + "\n";
    }
    forward(eval_arg(consumed[0]));
    return {};
}

std::string LogoMachine::do_back(const std::vector<std::string>& args) {
    const std::vector<std::string> consumed = consume_args(args, 1);
    if (consumed.size() != 1) {
        return "❌ BACK expects 1 argument(s), got " + std::to_string(consumed.size()) +
               "\n";
    }
    back(eval_arg(consumed[0]));
    return {};
}

std::string LogoMachine::do_left(const std::vector<std::string>& args) {
    const std::vector<std::string> consumed = consume_args(args, 1);
    if (consumed.size() != 1) {
        return "❌ LEFT expects 1 argument(s), got " + std::to_string(consumed.size()) +
               "\n";
    }
    left(eval_arg(consumed[0]));
    return {};
}

std::string LogoMachine::do_right(const std::vector<std::string>& args) {
    const std::vector<std::string> consumed = consume_args(args, 1);
    if (consumed.size() != 1) {
        return "❌ RIGHT expects 1 argument(s), got " +
               std::to_string(consumed.size()) + "\n";
    }
    right(eval_arg(consumed[0]));
    return {};
}

std::string LogoMachine::do_setxy(const std::vector<std::string>& args) {
    const std::vector<std::string> consumed = consume_args(args, 2);
    if (consumed.size() != 2) {
        return "❌ SETXY expects 2 argument(s), got " + std::to_string(consumed.size()) +
               "\n";
    }
    const double x = eval_arg(consumed[0]);
    const double y = eval_arg(consumed[1]);
    go_to(x, y);
    return {};
}

std::string LogoMachine::do_setposition(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ SETPOSITION requires a list argument [x y]\n";
    }
    std::string joined;
    for (const std::string& piece : args) {
        if (!joined.empty()) {
            joined.push_back(' ');
        }
        joined += piece;
    }
    if (joined.front() == '[' && joined.back() == ']') {
        const std::vector<std::string> parts =
            split_ws(std::string_view{joined}.substr(1, joined.size() - 2));
        if (parts.size() != 2) {
            return "❌ SETPOSITION list must contain exactly 2 numbers\n";
        }
        const double x = eval_arg(parts[0]);
        const double y = eval_arg(parts[1]);
        go_to(x, y);
        return {};
    }
    if (args.size() == 2) {
        return do_setxy(args);
    }
    return "❌ SETPOSITION requires a list argument [x y]\n";
}

std::string LogoMachine::do_setx(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ SETX requires x coordinate\n";
    }
    std::string expression;
    for (const std::string& piece : args) {
        if (!expression.empty()) {
            expression.push_back(' ');
        }
        expression += piece;
    }
    go_to(eval(expression), pose_.y());
    return {};
}

std::string LogoMachine::do_sety(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ SETY requires y coordinate\n";
    }
    std::string expression;
    for (const std::string& piece : args) {
        if (!expression.empty()) {
            expression.push_back(' ');
        }
        expression += piece;
    }
    go_to(pose_.x(), eval(expression));
    return {};
}

std::string LogoMachine::do_setheading(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ SETHEADING requires angle\n";
    }
    std::string expression;
    for (const std::string& piece : args) {
        if (!expression.empty()) {
            expression.push_back(' ');
        }
        expression += piece;
    }
    set_heading(eval(expression));
    return {};
}

std::string LogoMachine::do_setpenwidth(const std::vector<std::string>& args) {
    std::string expression;
    for (const std::string& piece : args) {
        if (!expression.empty()) {
            expression.push_back(' ');
        }
        expression += piece;
    }
    if (expression.empty()) {
        return "❌ SETPENWIDTH requires a width argument\n";
    }
    const double value = eval(expression);
    const double clamped = std::min(100.0, std::max(1.0, std::trunc(value)));
    pen_width_ = clamped;
    turtle_.set_pen_width(clamped);
    return {};
}

// -- colour ----------------------------------------------------------------

std::string LogoMachine::do_setcolor(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ SETCOLOR requires color\n";
    }
    if (in_set(colour_reporters(), upper(args[0])) || args[0].front() == ':') {
        const std::string colour = resolve_colour_argument(args);
        if (colour.rfind("❌", 0) == 0) {
            return colour;
        }
        if (const std::optional<Rgb> named = named_colour(colour); named.has_value()) {
            turtle_.set_color(*named);
            return {};
        }
        // A hex colour or three numbers, spelled the way `pencolor` accepts.
        const std::string hex = strip_char(colour, '"');
        if (hex.size() == 7 && hex.front() == '#') {
            const auto channel = [&hex](std::size_t start) {
                return static_cast<int>(std::strtol(hex.substr(start, 2).c_str(), nullptr, 16));
            };
            turtle_.set_color(Rgb{channel(1), channel(3), channel(5)});
        }
        return {};
    }

    std::vector<std::string> working = args;
    std::string merged;
    for (const std::string& piece : working) {
        if (!merged.empty()) {
            merged.push_back(' ');
        }
        merged += piece;
    }
    if (merged.front() == '[' && merged.back() == ']') {
        std::string inner = merged.substr(1, merged.size() - 2);
        std::replace(inner.begin(), inner.end(), ',', ' ');
        working = split_ws(inner);
        if (working.size() != 3) {
            return "❌ SETCOLOR requires 1 color name/hex or 3 RGB values\n";
        }
    }

    if (working.size() == 1) {
        const std::string colour = strip_char(working[0], '"');
        if (const std::optional<Rgb> named = named_colour(colour); named.has_value()) {
            turtle_.set_color(*named);
        } else if (colour.size() == 7 && colour.front() == '#') {
            const auto channel = [&colour](std::size_t start) {
                return static_cast<int>(
                    std::strtol(colour.substr(start, 2).c_str(), nullptr, 16));
            };
            turtle_.set_color(Rgb{channel(1), channel(3), channel(5)});
        } else if (const std::optional<double> slot = eval_number(colour);
                   slot.has_value()) {
            // A bare number is a palette slot, which is what `SETPC 1` means in
            // the reference's own examples. Without this branch the statement was
            // accepted, printed nothing, and changed no colour -- a statement that
            // acknowledged work it had not done.
            turtle_.set_color(turtle_.resolve_color(static_cast<int>(*slot)));
        }
        return {};
    }
    if (working[0].front() == '"') {
        // A quoted colour name is complete in itself; trailing tokens belong to
        // whatever follows on the line.
        const std::string colour = strip_char(working[0], '"');
        if (const std::optional<Rgb> named = named_colour(colour); named.has_value()) {
            turtle_.set_color(*named);
        }
        return {};
    }
    if (working.size() >= 3) {
        const std::vector<std::string> rgb =
            working.size() == 3 ? working : group_rgb_arguments(working);
        if (rgb.size() != 3) {
            return "❌ SETCOLOR requires 1 color name/hex or 3 RGB values\n";
        }
        try {
            const int red = clamp_channel(eval_arg(rgb[0]));
            const int green = clamp_channel(eval_arg(rgb[1]));
            const int blue = clamp_channel(eval_arg(rgb[2]));
            turtle_.set_color(Rgb{red, green, blue});
        } catch (const std::exception&) {
            return "❌ SETCOLOR RGB values must be integers\n";
        }
        return {};
    }
    return "❌ SETCOLOR requires 1 color name/hex or 3 RGB values\n";
}

std::string LogoMachine::do_setbgcolor(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ SETBGCOLOR requires color\n";
    }
    if (in_set(colour_reporters(), upper(args[0])) || args[0].front() == ':') {
        const std::string colour = resolve_colour_argument(args);
        if (colour.rfind("❌", 0) == 0) {
            return colour;
        }
        if (const std::optional<Rgb> named = named_colour(colour); named.has_value()) {
            turtle_.set_background(*named);
            return {};
        }
        return "❌ SETBGCOLOR only supports named colors for now\n";
    }

    std::vector<std::string> working = args;
    std::string merged;
    for (const std::string& piece : working) {
        if (!merged.empty()) {
            merged.push_back(' ');
        }
        merged += piece;
    }
    if (merged.front() == '[' && merged.back() == ']') {
        std::string inner = merged.substr(1, merged.size() - 2);
        std::replace(inner.begin(), inner.end(), ',', ' ');
        working = split_ws(inner);
        if (working.size() != 3) {
            return "❌ SETBGCOLOR requires 1 color name or 3 RGB values\n";
        }
    }
    if (working.size() == 1) {
        const std::string colour = strip_char(working[0], '"');
        if (const std::optional<Rgb> named = named_colour(colour); named.has_value()) {
            turtle_.set_background(*named);
        } else {
            return "❌ SETBGCOLOR only supports named colors for now\n";
        }
        return {};
    }
    if (working.size() >= 3) {
        const std::vector<std::string> rgb =
            working.size() == 3 ? working : group_rgb_arguments(working);
        if (rgb.size() != 3) {
            return "❌ SETBGCOLOR requires 1 color name or 3 RGB values\n";
        }
        try {
            const int red = clamp_channel(eval_arg(rgb[0]));
            const int green = clamp_channel(eval_arg(rgb[1]));
            const int blue = clamp_channel(eval_arg(rgb[2]));
            turtle_.set_background(Rgb{red, green, blue});
        } catch (const std::exception&) {
            return "❌ SETBGCOLOR RGB values must be integers\n";
        }
        return {};
    }
    return "❌ SETBGCOLOR requires 1 color name or 3 RGB values\n";
}

// -- control ---------------------------------------------------------------

std::size_t matching_bracket(std::string_view text, std::size_t start) {
    int depth = 0;
    for (std::size_t index = start; index < text.size(); ++index) {
        if (text[index] == '[') {
            ++depth;
        } else if (text[index] == ']') {
            --depth;
            if (depth == 0) {
                return index;
            }
        }
    }
    return std::string_view::npos;
}

std::string LogoMachine::do_repeat(std::string_view command) {
    const std::size_t open = command.find('[');
    if (open == std::string_view::npos) {
        return "❌ REPEAT requires [commands]\n";
    }
    const std::size_t close = command.rfind(']');
    if (close == std::string_view::npos || close < open) {
        return "❌ REPEAT requires [commands]\n";
    }
    const std::string header = strip(command.substr(0, open));
    const std::string body = strip(command.substr(open + 1, close - open - 1));
    if (!starts_with_upper(header, "REPEAT")) {
        return "❌ Invalid REPEAT command\n";
    }
    const std::string count_text = strip(std::string_view{header}.substr(6));
    if (count_text.empty()) {
        return "❌ REPEAT requires count\n";
    }
    double count = 0.0;
    try {
        count = std::trunc(eval(count_text));
    } catch (const std::exception&) {
        return "❌ Invalid REPEAT count: " + count_text + "\n";
    }
    std::string output;
    for (long long step = 0; step < static_cast<long long>(count); ++step) {
        tick();
        set_number("REPCOUNT", static_cast<double>(step + 1));
        set_number("REPCOUNTER", static_cast<double>(step + 1));
        output += run_text(body);
    }
    return output;
}

std::string LogoMachine::do_if(std::string_view command) {
    const std::size_t open = command.find('[');
    if (open == std::string_view::npos) {
        return "❌ IF requires [commands]\n";
    }
    const std::size_t close = command.rfind(']');
    if (close == std::string_view::npos || close < open) {
        return "❌ IF requires [commands]\n";
    }
    const std::string header = strip(command.substr(0, open));
    const std::string body = strip(command.substr(open + 1, close - open - 1));
    if (!starts_with_upper(header, "IF")) {
        return "❌ Invalid IF command\n";
    }
    const std::string condition_text = strip(std::string_view{header}.substr(2));
    if (condition_text.empty()) {
        return "❌ IF requires condition\n";
    }
    double condition = 0.0;
    try {
        condition = eval(condition_text);
    } catch (const std::exception&) {
        return "❌ Invalid IF condition: " + condition_text + "\n";
    }
    if (condition != 0.0) {
        return run_text(body);
    }
    return {};
}

std::string LogoMachine::do_ifelse(std::string_view command) {
    const std::size_t first = command.find('[');
    if (first == std::string_view::npos) {
        return "❌ IFELSE requires [true] [false] blocks\n";
    }
    const std::string header = strip(command.substr(0, first));
    const std::vector<std::string> header_words = split_ws(header);
    if (header_words.size() < 2) {
        return "❌ IFELSE requires condition\n";
    }
    std::string condition_text;
    for (std::size_t index = 1; index < header_words.size(); ++index) {
        if (!condition_text.empty()) {
            condition_text.push_back(' ');
        }
        condition_text += header_words[index];
    }
    const std::size_t first_close = matching_bracket(command, first);
    if (first_close == std::string_view::npos) {
        return "❌ IFELSE first block malformed\n";
    }
    const std::string true_block = strip(command.substr(first + 1, first_close - first - 1));
    const std::size_t second = command.find('[', first_close + 1);
    if (second == std::string_view::npos) {
        return "❌ IFELSE requires second [false] block\n";
    }
    const std::size_t second_close = matching_bracket(command, second);
    if (second_close == std::string_view::npos) {
        return "❌ IFELSE second block malformed\n";
    }
    const std::string false_block =
        strip(command.substr(second + 1, second_close - second - 1));

    double condition = 0.0;
    const std::string shouted = upper(strip(condition_text));
    if (shouted == "TRUE") {
        condition = 1.0;
    } else if (shouted == "FALSE") {
        condition = 0.0;
    } else {
        try {
            condition = eval(condition_text);
        } catch (const std::exception&) {
            return "❌ Invalid IFELSE condition: " + condition_text + "\n";
        }
    }
    return condition != 0.0 ? run_text(true_block) : run_text(false_block);
}

std::vector<std::string> LogoMachine::parse_body(std::string_view body) const {
    const std::vector<std::string> lines = split_lines(body);
    std::vector<std::string> commands;
    std::size_t index = 0;
    while (index < lines.size()) {
        const std::string line = strip(lines[index]);
        if (line.empty()) {
            ++index;
            continue;
        }
        int bracket_count = static_cast<int>(std::count(line.begin(), line.end(), '[')) -
                            static_cast<int>(std::count(line.begin(), line.end(), ']'));
        if (bracket_count > 0) {
            std::string joined = line;
            ++index;
            while (index < lines.size() && bracket_count > 0) {
                const std::string next = strip(lines[index]);
                joined += "\n" + next;
                bracket_count += static_cast<int>(std::count(next.begin(), next.end(), '[')) -
                                 static_cast<int>(std::count(next.begin(), next.end(), ']'));
                ++index;
            }
            commands.push_back(joined);
        } else {
            commands.push_back(line);
            ++index;
        }
    }
    return commands;
}

std::string LogoMachine::do_while(std::string_view command, bool until_mode) {
    const char* const which = until_mode ? "UNTIL" : "WHILE";
    std::vector<std::string> blocks;
    std::size_t index = 0;
    while (index < command.size() && blocks.size() < 2) {
        if (command[index] != '[') {
            ++index;
            continue;
        }
        int depth = 0;
        std::size_t cursor = index;
        while (cursor < command.size()) {
            if (command[cursor] == '[') {
                ++depth;
            } else if (command[cursor] == ']') {
                --depth;
            }
            if (depth == 0) {
                blocks.emplace_back(strip(command.substr(index + 1, cursor - index - 1)));
                index = cursor;
                break;
            }
            ++cursor;
        }
        ++index;
    }
    if (blocks.size() < 2) {
        return std::string{"❌ "} + which + " requires [condition] [body]\n";
    }
    std::string output;
    for (int iteration = 0; iteration < 10000; ++iteration) {
        tick();
        if (!interpreter_.running) {
            break;
        }
        double condition = 0.0;
        try {
            condition = eval(blocks[0]);
        } catch (const std::exception&) {
            return std::string{"❌ "} + which + ": invalid condition '" + blocks[0] + "'\n";
        }
        if (until_mode ? condition != 0.0 : condition == 0.0) {
            break;
        }
        output += run_text(blocks[1]);
        if (!interpreter_.running) {
            break;
        }
    }
    return output;
}

std::string LogoMachine::do_forever(std::string_view command) {
    const std::size_t open = command.find('[');
    if (open == std::string_view::npos) {
        return "❌ FOREVER requires [commands]\n";
    }
    const std::size_t start = command.find('[', open) + 1;
    const std::size_t end = command.rfind(']');
    if (start >= end) {
        return "❌ FOREVER commands malformed\n";
    }
    const std::string body = strip(command.substr(start, end - start));
    std::string output;
    for (int iteration = 0; iteration < 10000; ++iteration) {
        tick();
        if (!interpreter_.running) {
            break;
        }
        set_number("REPCOUNT", static_cast<double>(iteration + 1));
        set_number("REPCOUNTER", static_cast<double>(iteration + 1));
        output += run_text(body);
        if (!interpreter_.running) {
            break;
        }
    }
    return output;
}

// -- output ----------------------------------------------------------------

std::string LogoMachine::do_output(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ OUTPUT requires value\n";
    }
    std::string expression;
    for (const std::string& piece : args) {
        if (!expression.empty()) {
            expression.push_back(' ');
        }
        expression += piece;
    }
    try {
        const double value = eval(expression);
        last_output_ = value;
        return repr_value(value) + "STOP_PROCEDURE";
    } catch (const std::exception& error) {
        return std::string{"❌ Invalid OUTPUT value: "} + error.what() + "\n";
    }
}

std::string LogoMachine::do_to(std::string_view command) {
    const std::string shouted = upper(command);
    std::size_t start = shouted.find("TO ");
    if (start == std::string::npos) {
        if (starts_with_upper(shouted, "TO ")) {
            start = 0;
        } else {
            return "❌ Invalid TO command\n";
        }
    }
    const std::size_t end = shouted.find(" END");
    if (end == std::string::npos) {
        return "❌ TO requires END\n";
    }
    const std::vector<std::string> lines = split_lines(command.substr(start, end - start));
    if (lines.empty()) {
        return "❌ Invalid TO command\n";
    }
    const std::vector<std::string> header = split_ws(strip(lines[0]));
    if (header.size() < 2) {
        return "❌ Invalid TO command\n";
    }
    const std::string name = header[1];
    std::vector<std::string> params;
    std::string body;
    if (lines.size() > 1) {
        for (std::size_t index = 2; index < header.size(); ++index) {
            if (header[index].front() == ':' && header[index].size() > 1) {
                params.push_back(header[index].substr(1));
            }
        }
        for (std::size_t index = 1; index < lines.size(); ++index) {
            if (!body.empty()) {
                body.push_back('\n');
            }
            body += lines[index];
        }
    } else {
        bool in_params = true;
        for (std::size_t index = 2; index < header.size(); ++index) {
            const std::string& part = header[index];
            if (in_params && part.front() == ':' && part.size() > 1) {
                params.push_back(part.substr(1));
            } else {
                in_params = false;
                if (!body.empty()) {
                    body.push_back(' ');
                }
                body += part;
            }
        }
    }
    procedure_params_[name] = params;
    procedures_[name] = body;
    return {};
}

std::string LogoMachine::call_procedure(const std::string& name,
                                        const std::vector<std::string>& args) {
    const auto body = procedures_.find(name);
    if (body == procedures_.end()) {
        return "❌ Procedure " + name + " not defined\n";
    }
    const std::vector<std::string> params = procedure_params_[name];
    constexpr int kMaxDepth = 128;

    const double depth = interpreter_.number("__RECURSION_DEPTH__");
    if (depth >= kMaxDepth) {
        return "❌ Procedure " + name + " exceeded maximum recursion depth (" +
               std::to_string(kMaxDepth) + ")\n";
    }
    tick();
    set_number("__RECURSION_DEPTH__", depth + 1.0);

    const std::vector<std::string> used = consume_args(args, params.size());
    if (used.size() < params.size()) {
        set_number("__RECURSION_DEPTH__", depth);
        return "❌ Procedure " + name + " requires " + std::to_string(params.size()) +
               " inputs\n";
    }

    std::vector<Saved> saved;
    saved.reserve(params.size());
    for (std::size_t index = 0; index < params.size(); ++index) {
        const std::string& param = params[index];
        const std::string& argument = used[index];
        Saved entry{param, std::nullopt, std::nullopt};
        const auto existing = interpreter_.variables().find(param);
        if (existing != interpreter_.variables().end()) {
            entry.number = existing->second;
        }
        const auto existing_word = words_.find(param);
        if (existing_word != words_.end()) {
            entry.word = existing_word->second;
        }
        saved.push_back(entry);

        if (!argument.empty() && argument.front() == '"') {
            set_word(param, argument.substr(1));
            continue;
        }
        try {
            set_number(param, eval(argument));
        } catch (const std::exception&) {
            // A bare word is a string, not an error.
            set_word(param, argument);
        }
    }

    std::string output;
    // The parameters and the recursion counter are put back on *every* exit,
    // including the STOP/OUTPUT ones in the middle of the body -- the reference
    // does this in a `finally`, and a `return` that skipped it leaked the
    // counter until a deep program hit the depth limit it should never have
    // reached.
    struct Guard {
        LogoMachine& machine;
        const std::vector<Saved>& saved;
        double depth;
        ~Guard() { machine.restore(saved, depth); }
    } guard{*this, saved, depth};

    for (const std::string& statement : parse_body(body->second)) {
        const std::string trimmed = strip(statement);
        if (trimmed.empty()) {
            continue;
        }
        const std::string result = run_text(trimmed);
        if (result.find("STOP_PROCEDURE") != std::string::npos) {
            std::string cleaned = result;
            std::string::size_type position = 0;
            while ((position = cleaned.find("STOP_PROCEDURE", position)) !=
                   std::string::npos) {
                cleaned.erase(position, std::string{"STOP_PROCEDURE"}.size());
            }
            return output + cleaned;
        }
        if (result.rfind("❌", 0) == 0) {
            // The reference abandons the procedure at the first error and
            // returns what the caller may print.
            return output + result;
        }
        output += result;
    }
    return output;
}

void LogoMachine::restore(const std::vector<Saved>& saved, double depth) {
    set_number("__RECURSION_DEPTH__", depth);
    for (const Saved& entry : saved) {
        if (entry.number.has_value()) {
            interpreter_.set_number(entry.name, entry.number->number());
        } else {
            interpreter_.remove_variable(entry.name);
        }
        if (entry.word.has_value()) {
            words_[entry.name] = *entry.word;
        } else {
            words_.erase(entry.name);
        }
    }
}

// -- variables and data ----------------------------------------------------

std::string LogoMachine::do_make(const std::vector<std::string>& args,
                                 std::string_view raw) {
    if (args.size() < 2) {
        return "❌ MAKE requires variable name and value\n";
    }
    const std::string name = upper(strip_char(args[0], '"'));
    std::string value_expr;
    {
        const std::vector<std::string> parts = split_ws_limit(raw, 1);
        if (parts.size() > 1) {
            value_expr = strip(parts[1]);
        }
    }
    if (value_expr.empty()) {
        value_expr.clear();
        for (std::size_t index = 1; index < args.size(); ++index) {
            if (!value_expr.empty()) {
                value_expr.push_back(' ');
            }
            value_expr += args[index];
        }
    }

    try {
        set_number(name, eval(value_expr));
        return {};
    } catch (const std::exception&) {
        // Not a number: a word or a list, stored the way the reference does.
    }
    if (!value_expr.empty() && value_expr.front() == '"' &&
        value_expr.find(' ') == std::string::npos) {
        set_word(name, rstrip(std::string_view{value_expr}.substr(1)));
    } else if (value_expr.size() >= 2 && value_expr.front() == '"' &&
               value_expr.back() == '"') {
        set_word(name, value_expr.substr(1, value_expr.size() - 2));
    } else if (value_expr.size() >= 2 && value_expr.front() == '[' &&
               value_expr.back() == ']') {
        lists_[name] = split_ws(
            std::string_view{value_expr}.substr(1, value_expr.size() - 2));
    } else {
        set_word(name, value_expr);
    }
    return {};
}

std::string LogoMachine::do_thing(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ THING requires variable name\n";
    }
    const std::string name = upper(strip_char(args[0], '"'));
    const auto number = interpreter_.variables().find(name);
    if (number != interpreter_.variables().end()) {
        return repr_value(number->second.number());
    }
    const auto word = words_.find(name);
    if (word != words_.end()) {
        return word->second;
    }
    const auto list = lists_.find(name);
    if (list != lists_.end()) {
        return repr_list(list->second);
    }
    return "❌ Variable " + name + " not found\n";
}

std::string LogoMachine::do_print(const std::vector<std::string>& args,
                                  std::string_view raw) {
    if (args.empty()) {
        return "\n";
    }
    std::string text;
    if (!raw.empty()) {
        text = std::string{raw};
    } else {
        for (const std::string& piece : args) {
            if (!text.empty()) {
                text.push_back(' ');
            }
            text += piece;
        }
    }

    if (text.size() >= 2 && text.front() == '[' && text.back() == ']') {
        const std::string inner = text.substr(1, text.size() - 2);
        // The reference collapses whitespace after tightening the brackets, so
        // `[a [ b ] c]` keeps the inner pair tight and loses the rest.
        std::string tightened;
        for (std::size_t index = 0; index < inner.size();) {
            if (inner[index] == '[') {
                tightened.push_back('[');
                ++index;
                while (index < inner.size() && is_space(inner[index])) {
                    ++index;
                }
                continue;
            }
            if (is_space(inner[index])) {
                std::size_t end = index;
                while (end < inner.size() && is_space(inner[end])) {
                    ++end;
                }
                if (end < inner.size() && inner[end] == ']') {
                    index = end;
                    continue;
                }
                tightened.push_back(' ');
                index = end;
                continue;
            }
            tightened.push_back(inner[index]);
            ++index;
        }
        return strip(tightened) + "\n";
    }
    if (text.front() == '"' && text.find(' ') == std::string::npos) {
        return text.substr(1) + "\n";
    }

    if (args.size() == 1 && args[0].front() == ':') {
        const std::string name = upper(std::string_view{args[0]}.substr(1));
        const auto number = interpreter_.variables().find(name);
        if (number != interpreter_.variables().end()) {
            return format_value(number->second.render(), number->second.is_text()) + "\n";
        }
        const auto word = words_.find(name);
        if (word != words_.end()) {
            return word->second + "\n";
        }
        const auto list = lists_.find(name);
        if (list != lists_.end()) {
            return repr_list(list->second) + "\n";
        }
    }

    // A user procedure that OUTPUTs a value is a reporter: `PRINT DOUBLE 21`.
    // The built-in reporters are all the expression evaluator knows, so without
    // this the call was printed back as if it were the answer.
    {
        const std::vector<std::string> tokens = split_ws(text);
        if (!tokens.empty()) {
            const std::string name = upper(tokens[0]);
            const auto params = procedure_params_.find(name);
            if (params != procedure_params_.end()) {
                const std::vector<std::string> call_args(tokens.begin() + 1,
                                                         tokens.end());
                const std::vector<std::string> used =
                    consume_args(call_args, params->second.size());
                std::size_t consumed = 0;
                for (const std::string& piece : used) {
                    consumed += split_ws(piece).size();
                }
                // Tokens left over are not this procedure's to read, so they
                // fall through to the ordinary expression path.
                if (consumed == call_args.size()) {
                    last_output_.reset();
                    const std::string result = call_procedure(name, call_args);
                    if (result.rfind("❌", 0) == 0) {
                        return result;
                    }
                    if (!last_output_.has_value()) {
                        return "❌ Procedure " + name +
                               " did not OUTPUT a value\n";
                    }
                    const Value value{*last_output_};
                    return format_value(value.render(), value.is_text()) + "\n";
                }
            }
        }
    }

    if (in_set(colour_reporters(), upper(args[0]))) {
        std::string joined;
        for (const std::string& piece : args) {
            if (!joined.empty()) {
                joined.push_back(' ');
            }
            joined += piece;
        }
        std::string result = run_text(joined);
        if (!result.empty() && result.back() == '\n') {
            result.pop_back();
        }
        double value = 0.0;
        if (looks_like_number(result, value)) {
            result = format_value(repr_real(value), false);
        }
        return result + "\n";
    }

    if (args.size() == 1) {
        const std::string reporter = upper(args[0]);
        if (reporter == "POS" || reporter == "XCOR" || reporter == "YCOR" ||
            reporter == "HEADING") {
            if (reporter == "POS") {
                return "[" + repr_real(pose_.x()) + " " +
                       repr_real(pose_.y()) + "]\n";
            }
            if (reporter == "XCOR") {
                return repr_real(pose_.x()) + "\n";
            }
            if (reporter == "YCOR") {
                return repr_real(pose_.y()) + "\n";
            }
            return repr_real(pose_.heading()) + "\n";
        }
    }

    try {
        const double value = eval(text);
        return format_value(repr_real(value), false) + "\n";
    } catch (const std::exception&) {
        return text + "\n";
    }
}

std::string LogoMachine::format_value(std::string_view text, bool as_text) const {
    if (as_text) {
        return std::string{text};
    }
    double value = 0.0;
    if (looks_like_number(text, value)) {
        return std::trunc(value) == value ? repr_integer(value) : general_text(value);
    }
    return std::string{text};
}

std::string LogoMachine::do_show(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ SHOW requires value\n";
    }
    std::string raw;
    for (const std::string& piece : args) {
        if (!raw.empty()) {
            raw.push_back(' ');
        }
        raw += piece;
    }
    std::string shown = do_print(args, raw);
    if (shown.empty() || shown.back() != '\n') {
        shown.push_back('\n');
    }
    return shown;
}

std::string LogoMachine::do_type(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ TYPE requires value\n";
    }
    std::string raw;
    for (const std::string& piece : args) {
        if (!raw.empty()) {
            raw.push_back(' ');
        }
        raw += piece;
    }
    return rstrip(do_print(args, raw));
}

std::string LogoMachine::do_word(const std::vector<std::string>& args) {
    if (args.size() < 2) {
        return "❌ WORD requires at least two arguments\n";
    }
    std::string result;
    for (const std::string& argument : args) {
        try {
            const double value = interpreter_.evaluate_expression(argument);
            result += std::trunc(value) == value ? repr_integer(value) : repr_real(value);
        } catch (const std::exception&) {
            result += strip_char(argument, '"');
        }
    }
    return result;
}

std::string LogoMachine::do_list(const std::vector<std::string>& args) {
    // The reference mixes both kinds in one Python list, so the repr has to be
    // rebuilt with floats keeping their `.0` and words quoted.
    std::vector<std::pair<bool, std::string>> items;
    items.reserve(args.size());
    for (const std::string& argument : args) {
        try {
            const double value = interpreter_.evaluate_expression(argument);
            items.emplace_back(true, repr_real(value));
        } catch (const std::exception&) {
            items.emplace_back(false, strip_char(argument, '"'));
        }
    }
    std::string repr = "[";
    for (std::size_t index = 0; index < items.size(); ++index) {
        if (index > 0) {
            repr += ", ";
        }
        repr += items[index].first ? items[index].second
                                   : "'" + items[index].second + "'";
    }
    repr += "]";
    lists_["_TEMP_LIST"] = {};
    for (const auto& item : items) {
        lists_["_TEMP_LIST"].push_back(item.second);
    }
    return repr;
}

std::string LogoMachine::do_sentence(const std::vector<std::string>& args) {
    std::vector<std::string> items;
    for (const std::string& argument : args) {
        if (argument.size() >= 2 && argument.front() == '[' && argument.back() == ']') {
            for (const std::string& piece :
                 split_ws(std::string_view{argument}.substr(1, argument.size() - 2))) {
                items.push_back(piece);
            }
            continue;
        }
        try {
            const double value = interpreter_.evaluate_expression(argument);
            items.push_back(repr_value(value));
        } catch (const std::exception&) {
            items.push_back(strip_char(argument, '"'));
        }
    }
    return repr_list(items);
}

std::string LogoMachine::do_first(const std::vector<std::string>& args, bool last) {
    const std::string label = last ? "LAST" : "FIRST";
    if (args.empty()) {
        return "❌ " + label + " requires argument\n";
    }
    const std::vector<std::string> consumed = consume_args(args, 1);
    const std::string& argument = consumed[0];

    if (argument.front() == ':') {
        const std::string name = upper(std::string_view{argument}.substr(1));
        const auto list = lists_.find(name);
        if (list != lists_.end()) {
            if (list->second.empty()) {
                return {};
            }
            return last ? list->second.back() : list->second.front();
        }
        const auto word = words_.find(name);
        if (word != words_.end()) {
            if (word->second.empty()) {
                return {};
            }
            return last ? std::string{word->second.back()}
                        : std::string{word->second.front()};
        }
        const auto number = interpreter_.variables().find(name);
        if (number != interpreter_.variables().end()) {
            const std::string text = number->second.render();
            if (text.empty()) {
                return {};
            }
            return last ? std::string{text.back()} : std::string{text.front()};
        }
    }

    if (argument.size() >= 2 && argument.front() == '[' && argument.back() == ']') {
        const std::vector<std::string> items =
            split_ws(std::string_view{argument}.substr(1, argument.size() - 2));
        if (items.empty()) {
            return {};
        }
        return last ? items.back() : items.front();
    }
    try {
        return repr_value(interpreter_.evaluate_expression(argument));
    } catch (const std::exception&) {
        const std::string word = strip_char(argument, '"');
        if (word.empty()) {
            return {};
        }
        return last ? std::string{word.back()} : std::string{word.front()};
    }
}

std::string LogoMachine::do_butfirst(const std::vector<std::string>& args, bool last) {
    const std::string label = last ? "BUTLAST" : "BUTFIRST";
    if (args.empty()) {
        return "❌ " + label + " requires argument\n";
    }
    const std::vector<std::string> consumed = consume_args(args, 1);
    const std::string& argument = consumed[0];

    const auto shorten = [last](const std::vector<std::string>& items) {
        if (items.size() <= 1) {
            return std::string{"[]"};
        }
        std::vector<std::string> kept(items.begin() + (last ? 0 : 1),
                                      items.end() - (last ? 1 : 0));
        return repr_list(kept);
    };
    const auto shorten_word = [last](const std::string& text) {
        if (text.size() <= 1) {
            return std::string{};
        }
        return last ? text.substr(0, text.size() - 1) : text.substr(1);
    };

    if (argument.front() == ':') {
        const std::string name = upper(std::string_view{argument}.substr(1));
        const auto list = lists_.find(name);
        if (list != lists_.end()) {
            return shorten(list->second);
        }
        const auto word = words_.find(name);
        if (word != words_.end()) {
            return shorten_word(word->second);
        }
        const auto number = interpreter_.variables().find(name);
        if (number != interpreter_.variables().end()) {
            return shorten_word(number->second.render());
        }
    }
    if (argument.size() >= 2 && argument.front() == '[' && argument.back() == ']') {
        return shorten(split_ws(std::string_view{argument}.substr(1, argument.size() - 2)));
    }
    try {
        return shorten_word(repr_real(interpreter_.evaluate_expression(argument)));
    } catch (const std::exception&) {
        return shorten_word(strip_char(argument, '"'));
    }
}

std::string LogoMachine::do_item(const std::vector<std::string>& args) {
    if (args.size() < 2) {
        return "❌ ITEM requires index and thing\n";
    }
    const std::vector<std::string> consumed = consume_args(args, 2);
    if (consumed.size() < 2) {
        return "❌ ITEM requires index and thing\n";
    }
    double position = 0.0;
    try {
        position = std::trunc(eval(consumed[0])) - 1.0;
    } catch (const std::exception&) {
        return "❌ Invalid index\n";
    }
    const long long index = static_cast<long long>(position);
    const std::string& thing = consumed[1];

    if (thing.front() == ':') {
        const std::string name = upper(std::string_view{thing}.substr(1));
        const auto list = lists_.find(name);
        if (list != lists_.end()) {
            if (index >= 0 && index < static_cast<long long>(list->second.size())) {
                return list->second[static_cast<std::size_t>(index)];
            }
            return {};
        }
        const auto word = words_.find(name);
        if (word != words_.end()) {
            if (index >= 0 && index < static_cast<long long>(word->second.size())) {
                return std::string{word->second[static_cast<std::size_t>(index)]};
            }
            return {};
        }
        const auto number = interpreter_.variables().find(name);
        if (number != interpreter_.variables().end()) {
            const std::string text = number->second.render();
            if (index >= 0 && index < static_cast<long long>(text.size())) {
                return std::string{text[static_cast<std::size_t>(index)]};
            }
            return {};
        }
    }
    if (thing.size() >= 2 && thing.front() == '[' && thing.back() == ']') {
        const std::vector<std::string> items =
            split_ws(std::string_view{thing}.substr(1, thing.size() - 2));
        if (index >= 0 && index < static_cast<long long>(items.size())) {
            return items[static_cast<std::size_t>(index)];
        }
        return {};
    }
    try {
        const std::string text = repr_value(interpreter_.evaluate_expression(thing));
        if (index >= 0 && index < static_cast<long long>(text.size())) {
            return std::string{text[static_cast<std::size_t>(index)]};
        }
    } catch (const std::exception&) {
        const std::string word = strip_char(thing, '"');
        if (index >= 0 && index < static_cast<long long>(word.size())) {
            return std::string{word[static_cast<std::size_t>(index)]};
        }
    }
    return {};
}

std::string LogoMachine::do_count(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ COUNT requires argument\n";
    }
    const std::string& argument = args[0];
    if (argument.size() >= 2 && argument.front() == '[' && argument.back() == ']') {
        return std::to_string(
            split_ws(std::string_view{argument}.substr(1, argument.size() - 2)).size());
    }
    try {
        return std::to_string(repr_value(interpreter_.evaluate_expression(argument)).size());
    } catch (const std::exception&) {
        return std::to_string(strip_char(argument, '"').size());
    }
}

std::string LogoMachine::do_arithmetic(const std::vector<std::string>& args,
                                       char operation) {
    const char* const label = operation == '+'   ? "SUM"
                              : operation == '-' ? "DIFFERENCE"
                              : operation == '*' ? "PRODUCT"
                                                 : "QUOTIENT";
    if (args.size() < 2) {
        return std::string{"❌ "} + label + " requires two numbers\n";
    }
    double first = 0.0;
    double second = 0.0;
    try {
        first = interpreter_.evaluate_expression(args[0]);
        second = interpreter_.evaluate_expression(args[1]);
    } catch (const std::exception&) {
        return std::string{"❌ Invalid numbers for "} + label + "\n";
    }
    switch (operation) {
        case '+':
            return repr_value(first + second);
        case '-':
            return repr_value(first - second);
        case '*':
            return repr_value(first * second);
        default:
            if (second == 0.0) {
                return "❌ Division by zero\n";
            }
            return repr_value(first / second);
    }
}

std::string LogoMachine::do_random(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ RANDOM requires limit\n";
    }
    double limit = 0.0;
    try {
        limit = std::trunc(interpreter_.evaluate_expression(args[0]));
    } catch (const std::exception&) {
        return "❌ Invalid limit for RANDOM\n";
    }
    if (limit <= 0.0) {
        return "0";
    }
    return std::to_string(random_.randint(0, static_cast<int>(limit) - 1));
}

// -- graphics extras -------------------------------------------------------

std::string LogoMachine::do_arc(const std::vector<std::string>& args) {
    if (args.size() < 2) {
        return "❌ ARC requires angle and radius\n";
    }
    double angle = 0.0;
    double radius = 0.0;
    try {
        angle = interpreter_.evaluate_expression(args[0]);
        radius = interpreter_.evaluate_expression(args[1]);
    } catch (const std::exception&) {
        return "❌ Invalid parameters for ARC\n";
    }
    turtle_.circle(radius, angle);
    return {};
}

std::string LogoMachine::do_bezier(const std::vector<std::string>& args) {
    // BEZIER cx1 cy1 cx2 cy2 x y -- a cubic from where the turtle stands to
    // (x, y), bending through the two control points.
    //
    // A native extension, like PENSTYLE: the canvas has drawn a bezier since
    // 19.0 and no language could ask for one. The turtle ends at the curve's end
    // point facing along its final tangent, which is what makes chaining two of
    // them produce a smooth curve rather than a corner.
    if (args.size() < 6) {
        return "❌ BEZIER requires cx1 cy1 cx2 cy2 x y\n";
    }
    double values[6] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    for (std::size_t index = 0; index < 6; ++index) {
        try {
            values[index] = interpreter_.evaluate_expression(args[index]);
        } catch (const std::exception&) {
            return "❌ Invalid parameters for BEZIER\n";
        }
    }
    const Point start{pose_.x(), pose_.y()};
    const Point first_control{values[0], values[1]};
    const Point second_control{values[2], values[3]};
    const Point finish{values[4], values[5]};
    if (pen_down_) {
        turtle_.bezier({start, first_control, second_control, finish},
                       turtle_.pen_colour(), pen_width_);
    }
    // The end tangent of a cubic is the direction from the last control point to
    // the end, so the heading follows the curve.
    const double dx = finish.x - second_control.x;
    const double dy = finish.y - second_control.y;
    pose_.go_to(finish.x, finish.y);
    if (dx != 0.0 || dy != 0.0) {
        constexpr double kDegrees = 57.29577951308232;
        pose_.set_heading(std::atan2(dx, dy) * kDegrees);
    }
    // The curve is already drawn; the host's `goto_xy` would draw a straight line
    // from where the turtle stood to where it now is, and heading it would leave
    // a second mark. The pen is lifted for the reposition only, which is also how
    // the reference moved a turtle without drawing.
    if (pen_down_) {
        turtle_.pen_up();
    }
    turtle_.goto_xy(finish.x, finish.y);
    // Heading 0 is north and grows clockwise, so a direction is atan2(dx, dy) in
    // degrees -- the same conversion every other motion word uses.
    turtle_.set_heading(pose_.heading());
    if (pen_down_) {
        turtle_.pen_down();
    }
    if (on_change_) {
        on_change_();
    }
    return {};
}

std::string LogoMachine::do_filled(std::string_view command) {
    const std::size_t open = command.find('[');
    if (open == std::string_view::npos) {
        return "❌ FILLED requires color and [commands]\n";
    }
    const std::vector<std::string> header = split_ws(strip(command.substr(0, open)));
    Rgb fill{255, 255, 255};
    std::size_t search_from = open;

    if (header.size() >= 2) {
        const std::vector<std::string> colour_tokens(header.begin() + 1, header.end());
        if (colour_tokens.size() == 1) {
            const std::string name = upper(strip_char(colour_tokens[0], '"'));
            if (const std::optional<Rgb> named = named_colour(name); named.has_value()) {
                fill = *named;
            } else {
                double grey = 0.0;
                fill = looks_like_number(name, grey)
                           ? Rgb{clamp_channel(grey), clamp_channel(grey), clamp_channel(grey)}
                           : Rgb{255, 255, 255};
            }
        } else if (colour_tokens.size() >= 3) {
            double red = 0.0;
            double green = 0.0;
            double blue = 0.0;
            if (looks_like_number(colour_tokens[0], red) &&
                looks_like_number(colour_tokens[1], green) &&
                looks_like_number(colour_tokens[2], blue)) {
                fill = Rgb{clamp_channel(red), clamp_channel(green), clamp_channel(blue)};
            } else {
                fill = Rgb{255, 255, 255};
            }
        }
    } else {
        // The colour is a bracketed triple, and the body follows it.
        const std::size_t close = command.find(']', open);
        if (close == std::string_view::npos) {
            return "❌ FILLED requires a color argument\n";
        }
        const std::vector<std::string> parts =
            split_ws(strip(command.substr(open + 1, close - open - 1)));
        if (parts.size() == 3) {
            double red = 0.0;
            double green = 0.0;
            double blue = 0.0;
            if (looks_like_number(parts[0], red) && looks_like_number(parts[1], green) &&
                looks_like_number(parts[2], blue)) {
                fill = Rgb{clamp_channel(red), clamp_channel(green), clamp_channel(blue)};
            } else {
                fill = Rgb{255, 255, 255};
            }
        } else if (parts.size() == 1) {
            double grey = 0.0;
            fill = looks_like_number(parts[0], grey)
                       ? Rgb{clamp_channel(grey), clamp_channel(grey), clamp_channel(grey)}
                       : Rgb{255, 255, 255};
        } else {
            fill = Rgb{255, 255, 255};
        }
        search_from = close + 1;
    }

    const std::string_view body_text = command.substr(search_from);
    const std::size_t body_open = body_text.find('[');
    const std::size_t body_close = body_text.rfind(']');
    if (body_open == std::string_view::npos || body_open + 1 >= body_close) {
        return "❌ FILLED commands malformed\n";
    }
    const std::string body = strip(body_text.substr(body_open + 1, body_close - body_open - 1));

    std::vector<Point> path{Point{pose_.x(), pose_.y()}};
    const std::function<void()> previous = on_change_;
    on_change_ = [this, &path]() {
        const Point current{pose_.x(), pose_.y()};
        if (path.empty() || path.back().x != current.x || path.back().y != current.y) {
            path.push_back(current);
        }
    };
    std::string result;
    try {
        result = run_text(body);
    } catch (...) {
        on_change_ = previous;
        throw;
    }
    on_change_ = previous;
    const Point final_point{pose_.x(), pose_.y()};
    if (path.empty() || path.back().x != final_point.x || path.back().y != final_point.y) {
        path.push_back(final_point);
    }
    if (path.size() >= 3) {
        turtle_.draw_polygon(path, turtle_.pen_colour(), fill, pen_width_, true);
    }
    return result;
}

std::string LogoMachine::do_label(std::string_view command) {
    std::string rest = strip(command);
    if (starts_with_upper(rest, "LABEL")) {
        rest = strip(std::string_view{rest}.substr(5));
    }
    if (rest.empty()) {
        return "❌ LABEL requires text\n";
    }
    const std::string text = strip_char(rest, '"');
    turtle_.draw_text(pose_.x(), pose_.y(), text.c_str(), font_size_);
    return {};
}

std::string LogoMachine::do_stamp() {
    constexpr double kSize = 10.0;
    const double radians = pose_.heading() * kPi / 180.0;
    const double left_angle = radians + 140.0 * kPi / 180.0;
    const double right_angle = radians - 140.0 * kPi / 180.0;
    std::vector<Point> triangle{
        Point{pose_.x() + kSize * std::sin(radians), pose_.y() - kSize * std::cos(radians)},
        Point{pose_.x() + kSize * 0.5 * std::sin(left_angle),
              pose_.y() - kSize * 0.5 * std::cos(left_angle)},
        Point{pose_.x() + kSize * 0.5 * std::sin(right_angle),
              pose_.y() - kSize * 0.5 * std::cos(right_angle)},
    };
    const Rgb colour = turtle_.pen_colour();
    // The reference stores a shape whose stroke width is TurtleShape's default,
    // not the pen width, so the stamp does not grow with SETPENWIDTH.
    turtle_.draw_polygon(triangle, colour, colour, 1.0, true);
    char buffer[64];
    std::snprintf(buffer, sizeof buffer, "🐢 Stamped at (%.1f, %.1f)\n", pose_.x(),
                  pose_.y());
    return buffer;
}

std::string LogoMachine::do_textsize(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ TEXTSIZE requires a size argument\n";
    }
    const std::optional<double> size = eval_number(args[0]);
    if (!size.has_value()) {
        return "❌ TEXTSIZE: invalid size '" + args[0] + "'\n";
    }
    const int rounded = static_cast<int>(*size);
    if (rounded < 1 || rounded > 200) {
        return "❌ TEXTSIZE must be between 1 and 200\n";
    }
    font_size_ = rounded;
    return {};
}

std::string LogoMachine::do_dot(const std::vector<std::string>& args) {
    double size = pen_width_ > 1.0 ? pen_width_ * 2.0 : 4.0;
    if (!args.empty()) {
        const std::optional<double> given = eval_number(args[0]);
        if (given.has_value()) {
            size = *given;
        }
    }
    const double radius = size / 2.0;
    constexpr int kSegments = 16;
    std::vector<Point> points;
    points.reserve(kSegments);
    for (int segment = 0; segment < kSegments; ++segment) {
        const double angle = static_cast<double>(segment) * 360.0 / kSegments * kPi / 180.0;
        points.push_back(Point{pose_.x() + radius * std::cos(angle),
                               pose_.y() + radius * std::sin(angle)});
    }
    const Rgb colour = turtle_.pen_colour();
    turtle_.draw_polygon(points, colour, colour, 1.0, true);
    return {};
}

// -- higher-order list primitives ------------------------------------------

std::string LogoMachine::run_template(std::string_view template_text,
                                      const std::map<std::string, Value>& bindings) {
    std::map<std::string, std::optional<Value>> saved;
    for (const auto& [name, value] : bindings) {
        const auto existing = interpreter_.variables().find(name);
        saved[name] = existing == interpreter_.variables().end()
                          ? std::optional<Value>{}
                          : std::optional<Value>{existing->second};
    }
    for (const auto& [name, value] : bindings) {
        interpreter_.variables()[name] = value;
    }
    std::string output;
    try {
        output = run_text(template_text);
    } catch (...) {
        for (const auto& [name, value] : saved) {
            if (value.has_value()) {
                interpreter_.variables()[name] = *value;
            } else {
                interpreter_.variables().erase(name);
            }
        }
        throw;
    }
    for (const auto& [name, value] : saved) {
        if (value.has_value()) {
            interpreter_.variables()[name] = *value;
        } else {
            interpreter_.variables().erase(name);
        }
    }
    return output;
}

Value template_value(std::string_view item) {
    double number = 0.0;
    if (looks_like_number(item, number)) {
        return Value{number};
    }
    return Value{std::string{item}};
}

std::string LogoMachine::do_foreach(std::string_view command) {
    const std::size_t found = upper(command).find("FOREACH");
    const std::string after = strip(command.substr(found + 7));
    std::vector<std::string> blocks;
    std::size_t index = 0;
    while (index < after.size() && blocks.size() < 2) {
        if (after[index] != '[') {
            ++index;
            continue;
        }
        int depth = 0;
        std::size_t cursor = index;
        while (cursor < after.size()) {
            if (after[cursor] == '[') {
                ++depth;
            } else if (after[cursor] == ']') {
                --depth;
                if (depth == 0) {
                    blocks.emplace_back(strip(after.substr(index + 1, cursor - index - 1)));
                    index = cursor;
                    break;
                }
            }
            ++cursor;
        }
        ++index;
    }
    if (blocks.size() < 2) {
        return "❌ FOREACH requires [list] [template]\n";
    }
    std::string output;
    for (const std::string& item : split_ws(blocks[0])) {
        tick();
        output += run_template(blocks[1], {{"?", template_value(item)}});
    }
    return output;
}

std::string LogoMachine::do_higher_order(std::string_view command, char which) {
    const std::string name = which == 'm' ? "MAP" : which == 'f' ? "FILTER" : "REDUCE";
    const std::size_t found = upper(command).find(name);
    const std::string after = strip(command.substr(found + name.size()));

    const std::size_t open = after.find('[');
    if (open == std::string::npos) {
        return "❌ " + name + (which == 'm'   ? " requires [template] [list]\n"
                                : which == 'f' ? " requires [predicate] [list]\n"
                                               : " requires [template] [list]\n");
    }
    const std::size_t close = matching_bracket(after, open);
    if (close == std::string::npos) {
        return "❌ " + name + " requires [template] [list]\n";
    }
    const std::string template_text = strip(after.substr(open + 1, close - open - 1));
    const std::string rest = strip(after.substr(close + 1));

    std::vector<std::string> items;
    if (!rest.empty() && rest.front() == '[') {
        if (rest.size() >= 2 && rest.back() == ']') {
            items = split_ws(std::string_view{rest}.substr(1, rest.size() - 2));
        } else {
            items = split_ws(rest);
        }
    } else {
        items = split_ws(rest);
    }

    if (which == 'r') {
        if (items.size() < 1) {
            return "❌ REDUCE requires a non-empty list\n";
        }
        Value accumulator = template_value(items[0]);
        for (std::size_t index = 1; index < items.size(); ++index) {
            tick();
            const std::string result =
                run_template(template_text, {{"?1", accumulator},
                                             {"?2", template_value(items[index])}});
            const std::string trimmed = strip(result);
            accumulator = template_value(trimmed);
        }
        return accumulator.render() + "\n";
    }

    std::vector<std::string> results;
    for (const std::string& item : items) {
        tick();
        const std::string result =
            run_template(template_text, {{"?", template_value(item)}});
        if (which == 'm') {
            results.push_back(strip(result));
            continue;
        }
        const std::string trimmed = strip(result);
        double value = 0.0;
        bool truthy = false;
        if (looks_like_number(trimmed, value)) {
            truthy = value != 0.0;
        } else {
            truthy = !trimmed.empty() && upper(trimmed) != "FALSE" && trimmed != "0";
        }
        if (truthy) {
            results.push_back(item);
        }
    }
    std::string joined;
    for (const std::string& piece : results) {
        if (!joined.empty()) {
            joined.push_back(' ');
        }
        joined += piece;
    }
    return "[" + joined + "]\n";
}

std::string LogoMachine::do_local(const std::vector<std::string>& args) {
    for (const std::string& argument : args) {
        std::size_t start = 0;
        while (start < argument.size() && argument[start] == ':') {
            ++start;
        }
        // The reference strips only `:` here, so `LOCAL "X` makes a variable
        // whose name really is `"X`. Reproduced rather than tidied up: the
        // parity harness compares variable names.
        const std::string name = upper(std::string_view{argument}.substr(start));
        if (interpreter_.variables().find(name) == interpreter_.variables().end() &&
            words_.find(name) == words_.end()) {
            interpreter_.set_number(name, 0.0);
        }
    }
    return {};
}

// -- the run ---------------------------------------------------------------

void LogoMachine::run(std::string_view source) {
    std::mt19937_64 seeder{std::random_device{}()};
    random_.seed_unsigned(seeder());
    load(source);

    for (std::size_t index = 0; index < statements_.size(); ++index) {
        // The debugger's pause point. A whole statement is the unit: the loader
        // joins bracketed lines into one, and a procedure call is one statement
        // whose body runs commands that have no line of their own.
        if (!interpreter_.pause_if_requested(statement_source_lines_[index])) {
            // Break rather than return: the words this run created are still
            // facts about it, and the Variables panel reads them after a stop.
            interpreter_.running = false;
            break;
        }
        try {
            tick();
        } catch (const BudgetStop&) {
            return;
        }
        std::string output;
        try {
            output = run_text(statements_[index]);
        } catch (const BudgetStop&) {
            return;
        } catch (const std::exception& error) {
            output = std::string{"❌ Error at line "} +
                     std::to_string(index + 1) + ": " + error.what() + "\n";
        }
        interpreter_.log_output(annotate(output, statement_lines_[index]));
        if (!interpreter_.running) {
            break;
        }
    }

    // The reference keeps its words in a second store that the Variables panel
    // and the differential harness both read, so they are folded into the one
    // map at the end of the run, words winning as they do there. Lists are not
    // folded: the reference keeps them in a third store that neither reads.
    for (const auto& [name, text] : words_) {
        interpreter_.set_text(name, text);
    }
}

std::string LogoMachine::run_one(std::string_view command) {
    std::string output;
    try {
        output = run_text(command);
    } catch (const BudgetStop&) {
        return output;
    } catch (const std::exception& error) {
        output = std::string{"❌ "} + error.what() + "\n";
    }
    for (const auto& [name, text] : words_) {
        interpreter_.set_text(name, text);
    }
    return output;
}

}  // namespace

std::string execute_logo(Interpreter& interpreter, std::string_view source,
                         TurtleHost& turtle) {
    LogoMachine machine{interpreter, turtle};
    machine.run(source);
    return {};
}

std::string execute_logo_command(Interpreter& interpreter, std::string_view command,
                                 TurtleHost& turtle) {
    LogoMachine machine{interpreter, turtle};
    return machine.run_one(command);
}

bool is_logo_command(std::string_view word) {
    return in_set(logo_commands(), upper(word));
}

}  // namespace timewarp::lang
