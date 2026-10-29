// SPDX-License-Identifier: MIT
//
// What Logo's translation units share.
//
// `logo.cpp` was 3,548 lines: the reference's quirks document, the preamble of
// string and number helpers, one `LogoMachine` class and every primitive it
// answers, all in one anonymous namespace. The class had already been written as
// declarations with out-of-line definitions, so the split is mostly a move: the
// declarations live here, in a named `logo_detail` namespace rather than an
// anonymous one, because a type that several translation units define methods for
// needs one definition that they all agree on.
//
// Internal to the Logo engine: nothing outside `src/lang/logo*.cpp` includes it.

#pragma once

#include "timewarp/core/charconv.hpp"

#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/mersenne_random.hpp"
#include "timewarp/lang/turtle_host.hpp"

#include <cstddef>
#include <functional>
#include <map>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace timewarp::lang::logo_detail {

// -- text, the way Python's str methods behave ------------------------------

bool is_space(char character);
std::string upper(std::string_view text);
std::string rstrip(std::string_view text);
std::string strip(std::string_view text);
std::string strip_char(std::string_view text, char character);
std::vector<std::string> split_ws(std::string_view text);
std::vector<std::string> split_ws_limit(std::string_view text, std::size_t limit);
std::vector<std::string> split_lines(std::string_view source);
bool starts_with_upper(std::string_view text, std::string_view prefix);

// -- numbers, rendered the way Python renders them --------------------------

std::string repr_real(double value);
std::string general_text(double value);
std::string repr_integer(double value);
std::string repr_value(double value);
std::string repr_list(const std::vector<std::string>& words);

// -- the word lists the reference keeps as module constants -----------------

bool in_set(const std::set<std::string_view>& words, std::string_view word);
const std::set<std::string_view>& logo_commands();
const std::set<std::string_view>& no_argument_commands();
const std::set<std::string_view>& logo_reporters();
const std::set<std::string_view>& colour_reporters();
const std::set<std::string_view>& math_prefix_functions();
const std::set<std::string_view>& spaced_call_functions();
std::optional<Rgb> named_colour(std::string_view name);
int clamp_channel(double value);
bool looks_like_number(std::string_view text, double& value);

/// The `]` that closes the `[` at *start*, so a bracketed body can be sliced.
std::size_t matching_bracket(std::string_view text, std::size_t start);

/// Thrown to unwind a run that has been stopped, timed out or run out of
/// steps. The reference raises out of `tick()` for the same reason.
struct BudgetStop {};

/// Puts the `[line N]` annotation on each `❌` line, as the reference's
/// `_inject_line_number` does. A Logo statement has no number of its own, so
/// `line` is the position the loader gave it.
std::string annotate(std::string_view text, int line);

class LogoMachine {
public:
    LogoMachine(Interpreter& interpreter, TurtleHost& turtle)
        : interpreter_{interpreter}, turtle_{turtle} {}

    /// Everything about the run that is not the loaded program: where it is, and
    /// the state a statement can change. The program itself is loaded from the
    /// source, so it is not carried -- only the position in it.
    ///
    /// This is what a snapshot of a Logo run holds, and what a rewind puts back.
    struct Checkpoint {
        std::size_t pc = 0;
        std::map<std::string, std::string> procedures;
        std::map<std::string, std::vector<std::string>> procedure_params;
        std::map<std::string, std::string> words;
        std::map<std::string, std::vector<std::string>> lists;
        std::optional<double> last_output;
        std::string pen_mode{"paint"};
        int font_size = 12;
        double pen_width = 2.0;
        bool pen_down = true;
        TurtlePose pose;
        MersenneRandom random;
    };

    [[nodiscard]] Checkpoint capture() const;
    void apply(const Checkpoint& state);

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

    /// The statement the run is on. Part of the checkpoint, so a rewind puts the
    /// run back on the statement it left.
    std::size_t pc_ = 0;
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

}  // namespace timewarp::lang::logo_detail
