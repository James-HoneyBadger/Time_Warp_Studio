// SPDX-License-Identifier: MIT
//
// The native interpreter: program storage, the execution loop, variables, I/O
// and the run budget.
//
// This is the C++ counterpart of `time_warp/core/interpreter.py`. The loop is
// the same shape on purpose -- the reference's behaviour is what the port is
// checked against, so a difference in the loop is a defect even when it is an
// improvement.
//
// Two deliberate differences from the reference:
//
//  * Input is synchronous. The Python engine stops at an ASK and is resumed by a
//    later `execute()` call; here the executor calls `request_input`, which
//    invokes the caller's callback and stores the answer before returning. The
//    visible behaviour is identical, and a blocking callback lets the IDE's
//    runner thread wait without a second state machine.
//  * Errors are caught in the loop and reported with the source line, as the
//    reference does, but a non-`ExecutionError` exception is not rethrown: one
//    bad statement ends that statement, not the run.

#pragma once

#include "timewarp/lang/executor.hpp"
#include "timewarp/lang/language.hpp"
#include "timewarp/lang/turtle_host.hpp"
#include "timewarp/lang/value.hpp"
#include "timewarp/expr/evaluator.hpp"

#include <any>
#include <atomic>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace timewarp::lang {

class DebugSession;

/// One stored line: the number a numbered language gave it, and the text.
///
/// `number` is empty for languages whose lines are not numbered (Logo, APL) and
/// for whole-program languages. It is *not* the execution index: a BASIC program
/// can start at line 100, and FOCAL's labels are not numbers at all.
struct ProgramLine {
    std::optional<int> number;
    std::string text;
};

/// One line's share of a profiled run.
///
/// `index` is the program-line index, which is what the executor jumps to;
/// `line` is the number a person reads -- a BASIC line number, or the 1-based
/// position for a language without them.
struct LineProfile {
    std::size_t index = 0;
    int line = 0;
    std::uint64_t hits = 0;
    double elapsed_ms = 0.0;
};

/// An error an executor reports to the learner. The message is rendered with
/// the ❌ prefix by the loop, so an executor never has to know how the IDE
/// decorates it.
class ExecutionError : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

class Interpreter {
public:
    using OutputCallback = std::function<void(const std::string& line)>;
    /// Runs one SQL statement and returns what the database should say. Supplied
    /// by the IDE, which owns the connection; a headless run leaves it unset and
    /// SQL lines report that no database is open.
    using SqlCallback = std::function<std::string(std::string_view)>;
    /// Returns the answer to a prompt. Called on the interpreter's thread; the
    /// IDE's adapter blocks until the user replies.
    using InputCallback = std::function<std::string(const std::string& prompt)>;

    static constexpr std::uint64_t kMaxSteps = 2'000'000;
    static constexpr std::uint64_t kMaxIterations = 100'000;
    static constexpr double kMaxSeconds = 10.0;

    Interpreter(Language language, TurtleHost& turtle);

    Interpreter(const Interpreter&) = delete;
    Interpreter(Interpreter&&) = delete;
    Interpreter& operator=(const Interpreter&) = delete;
    Interpreter& operator=(Interpreter&&) = delete;
    ~Interpreter() = default;

    // -- loading and running ---------------------------------------------

    /// Stores *source*, discarding any previous program and variable state.
    void load_program(std::string_view source);
    /// Executes from `current_line` to the end, or until a stop is requested.
    void run();

    /// Cooperative cancellation. Executors that loop internally call
    /// `check_budget()` so a runaway statement can still be stopped.
    void request_stop() noexcept;
    [[nodiscard]] bool stop_requested() const noexcept;

    /// The one budget check the loop and executors use. Returns false once the
    /// run should stop -- either because it was asked to or because it has run
    /// for too long.
    [[nodiscard]] bool check_budget();

    /// One step of work, for a whole-program executor that runs its own loop.
    ///
    /// The line loop counts its own iterations; an executor with its own
    /// statements (Logo) reports them here, so the telemetry HUD measures the
    /// run rather than reporting zero.
    void note_step() noexcept { ++steps_used_; }

    /// The debugging session this run honours, or null for an ordinary run.
    ///
    /// Set before `run()`. The loop asks it before executing each statement, so a
    /// breakpoint or a step blocks the runner thread rather than being checked
    /// after the fact -- a pause that arrives after the statement has run is an
    /// inspector, not a debugger.
    void set_debug_session(DebugSession* session) noexcept { debug_ = session; }
    [[nodiscard]] DebugSession* debug_session() const noexcept { return debug_; }

    /// A pause point for a whole-program executor, which runs its own loop and
    /// therefore calls this itself. `line` is 1-based, or 0 when the program has
    /// no source lines (a binary CHIP-8 ROM).
    ///
    /// This also checks the data breakpoints: a whole-program language has no
    /// statement loop to compare bindings after, so its own pause points are the
    /// only place the check can happen.
    ///
    /// Returns false when the session asked for the run to stop; the executor
    /// returns its output and the run ends cleanly.
    [[nodiscard]] bool pause_if_requested(int line);

    // -- the surface executors use ---------------------------------------

    /// Execution position: an index into `program_lines()`.
    ///
    /// Public because an executor may still assign it, but a jump should go
    /// through `jump_to`: the loop detects a jump by comparing the counter before
    /// and after a statement, and a jump to the line already executing is
    /// invisible to that comparison. `10 GOTO 10` used to fall through and end the
    /// program; it now loops until the budget stops it.
    std::size_t current_line = 0;

    /// Moves the program counter and records that a jump happened, which is the
    /// one thing an assignment cannot say when the target is the current line.
    void jump_to(std::size_t line) noexcept {
        current_line = line;
        jumped_ = true;
    }

    /// Cleared by QUIT/STOP and by the budget. An executor must not keep
    /// running once this is false.
    bool running = false;

    [[nodiscard]] Language language() const noexcept { return language_; }
    [[nodiscard]] TurtleHost& turtle() noexcept { return turtle_; }
    [[nodiscard]] const std::vector<ProgramLine>& program_lines() const noexcept {
        return program_lines_;
    }
    [[nodiscard]] const std::map<std::string, Value>& variables() const noexcept {
        return variables_;
    }
    [[nodiscard]] std::map<std::string, Value>& variables() noexcept { return variables_; }

    /// A variable's number; 0 when it is absent, which is the reference's
    /// answer for a never-assigned variable.
    [[nodiscard]] double number(const std::string& name) const;
    [[nodiscard]] bool has_variable(const std::string& name) const;
    void remove_variable(const std::string& name);
    void set_number(const std::string& name, double value);
    void set_text(const std::string& name, std::string value);

    /// Evaluates an arithmetic expression against this run's variables.
    ///
    /// The native `timewarp_expr` evaluator, so a statement's arithmetic is the
    /// same arithmetic the expression library is gated against -- not a second
    /// parser inside each executor. Throws `ExecutionError` on a bad
    /// expression; an executor turns that into its language's error line.
    [[nodiscard]] double evaluate_expression(std::string_view expression);

    /// Functions the executing language adds to expression evaluation.
    ///
    /// COMAL's `FUNC` is called from inside an expression, so the executor
    /// installs a resolver for the duration of its run and clears it after. The
    /// hook is consulted before the evaluator's builtins.
    expr::FunctionHook function_hook;

    /// Replaces `#VAR` and `*VAR*` with their values, the reference's
    /// `interpolate_text`. Unknown names are left as written.
    [[nodiscard]] std::string interpolate_text(std::string_view text) const;

    /// Jumps to a label the loader registered from `L: NAME` or `*NAME`.
    /// Throws when the label does not exist, with the reference's message.
    void jump_to_label(std::string_view label);

    /// The last answer read from the user, for PILOT's `M:` match.
    [[nodiscard]] const std::string& last_input() const noexcept { return last_input_; }

    /// PILOT's return stack: line indices a `U:`/`S:` call returns to.
    std::vector<std::size_t> subroutine_stack;
    /// PILOT's `M:` verdict, tested by `TY:`/`TN:`/`Y:`/`N:`.
    bool last_match_succeeded = false;

    /// The numbered-line index: `GOTO 100` names a source line, not an index.
    std::map<int, std::size_t> line_number_map;
    /// `DIM`-declared arrays, indexed from zero. BASIC and COMAL share this
    /// shape; a language that needs strings in an array gets its own store
    /// when it is ported.
    std::map<std::string, std::vector<double>> arrays;

    /// Per-run scratch state for an executor, created on first use.
    ///
    /// The reference hangs this off `interpreter.__dict__`; this is the typed
    /// equivalent. A fresh interpreter is built per run, so it needs no
    /// generation check.
    template <typename State>
    State& executor_state() {
        if (!executor_state_.has_value()) {
            executor_state_ = State{};
        }
        return std::any_cast<State&>(executor_state_);
    }

    /// Appends output. Multi-line text becomes one entry per line, trailing
    /// newline removed, and each line goes to the callback as it is stored --
    /// that is what makes the console stream rather than appear at the end.
    void log_output(std::string_view text);

    /// Prompts and stores the answer in *variable*.
    ///
    /// `numeric` mirrors the reference's typed store: a reply that is not a
    /// number becomes 0 rather than an error, because a learner typing a word
    /// at "How many?" should see the prompt again or a wrong count, not a
    /// traceback.
    void request_input(const std::string& prompt, const std::string& variable,
                       bool numeric);

    // -- I/O callbacks ----------------------------------------------------

    void set_output_callback(OutputCallback callback) { output_callback_ = std::move(callback); }
    void set_input_callback(InputCallback callback) { input_callback_ = std::move(callback); }
    void set_sql_callback(SqlCallback callback) { sql_callback_ = std::move(callback); }
    [[nodiscard]] bool sql_available() const noexcept { return static_cast<bool>(sql_callback_); }

    /// Runs SQL on the IDE's connection and returns what it says. Executors that
    /// own their statement loop call this rather than reaching the IDE directly.
    [[nodiscard]] std::string run_sql(std::string_view sql);

    /// Executes *source* on top of the current bindings, without clearing them.
    ///
    /// This is the REPL's run: `load_program` resets a run, which is right for a
    /// program and wrong for the second line someone types. The program text and
    /// position are replaced; variables, arrays and executor state survive.
    /// Returns the output produced, joined with newlines.
    [[nodiscard]] std::string run_snippet(std::string_view source);

    // -- results and telemetry -------------------------------------------

    [[nodiscard]] const std::vector<std::string>& output() const noexcept { return output_; }
    [[nodiscard]] std::uint64_t steps_used() const noexcept { return steps_used_; }
    [[nodiscard]] std::uint64_t max_steps() const noexcept { return kMaxSteps; }
    [[nodiscard]] double elapsed_ms() const noexcept;

    /// Turns per-line timing on for the next run. Off by default: reading a
    /// clock around every statement costs, and a run that is not being profiled
    /// should not pay for it.
    ///
    /// Only the shared line loop records anything. A whole-program language
    /// (Logo, Scheme, Smalltalk, CHIP-8) runs its own loop and reports an empty
    /// profile, which the UI says rather than showing an empty table.
    void set_profiling(bool on) noexcept { profiling_ = on; }
    [[nodiscard]] bool profiling() const noexcept { return profiling_; }

    /// Per-line totals for the last profiled run, longest first. Lines that
    /// never ran are left out; ties are ordered by source line so the report is
    /// stable between runs that measure the same times.
    ///
    /// The time is wall clock around the statement, so a program that waits for
    /// input shows that wait against the statement that asked. A debug pause is
    /// not included: it happens before the clock starts.
    [[nodiscard]] std::vector<LineProfile> line_profiles() const;
    [[nodiscard]] bool timed_out() const noexcept { return timed_out_; }
    [[nodiscard]] bool iteration_capped() const noexcept { return iteration_capped_; }
    /// The line the run stopped on, as a person reads it.
    [[nodiscard]] int display_line() const;
    [[nodiscard]] const std::string& error() const noexcept { return error_; }

    /// The source line a jump or breakpoint should name: a numbered line's own
    /// number, otherwise its 1-based position.
    [[nodiscard]] int source_line(std::size_t index) const;

private:
    void reset();
    void parse_lines(std::string_view source);
    /// Pauses when a watched variable changed since the last statement. Returns
    /// false when the user stopped the run instead of continuing.
    [[nodiscard]] bool check_data_breakpoints(int line);
    /// Recognises and runs a `SQL ...` statement. False when `text` is not one.
    [[nodiscard]] bool run_sql_statement(std::string_view text);
    [[nodiscard]] bool execute_statement(std::string_view text);
    void inject_line_number(std::string& text) const;

    Language language_;
    TurtleHost& turtle_;
    /// The source as loaded: whole-program executors receive it in one piece.
    std::string program_source_;
    std::vector<ProgramLine> program_lines_;
    std::map<std::string, std::size_t> labels_;
    std::map<std::string, Value> variables_;
    std::string last_input_;
    std::vector<std::string> output_;
    std::any executor_state_;
    /// Not owned: the runner owns the session and outlives the run.
    DebugSession* debug_ = nullptr;
    /// Bindings as of the previous statement, for data breakpoints. Only updated
    /// when a session has data breakpoints armed, so an ordinary run pays nothing.
    std::map<std::string, Value> data_snapshot_;
    SqlCallback sql_callback_;

    OutputCallback output_callback_;
    InputCallback input_callback_;

    std::atomic<bool> stop_requested_{false};
    std::chrono::steady_clock::time_point started_{};
    std::uint64_t steps_used_ = 0;
    double elapsed_ms_ = 0.0;
    bool profiling_ = false;
    std::map<std::size_t, LineProfile> profile_;
    bool timed_out_ = false;
    bool iteration_capped_ = false;
    /// Set by `jump_to`, cleared by the run loop before each statement.
    bool jumped_ = false;
    std::string error_;
};

}  // namespace timewarp::lang
