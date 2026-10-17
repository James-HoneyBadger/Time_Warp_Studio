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

#include <any>
#include <atomic>
#include <chrono>
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

/// One stored line: the number a numbered language gave it, and the text.
///
/// `number` is empty for languages whose lines are not numbered (Logo, APL) and
/// for whole-program languages. It is *not* the execution index: a BASIC program
/// can start at line 100, and FOCAL's labels are not numbers at all.
struct ProgramLine {
    std::optional<int> number;
    std::string text;
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

    // -- the surface executors use ---------------------------------------

    /// Execution position: an index into `program_lines()`.
    ///
    /// Public because an executor jumps by assigning it, exactly as the Python
    /// executors do. The loop re-tests it after each statement, and only
    /// advances the line itself when the executor did not move it.
    std::size_t current_line = 0;

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

    // -- results and telemetry -------------------------------------------

    [[nodiscard]] const std::vector<std::string>& output() const noexcept { return output_; }
    [[nodiscard]] std::uint64_t steps_used() const noexcept { return steps_used_; }
    [[nodiscard]] std::uint64_t max_steps() const noexcept { return kMaxSteps; }
    [[nodiscard]] double elapsed_ms() const noexcept;
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

    OutputCallback output_callback_;
    InputCallback input_callback_;

    std::atomic<bool> stop_requested_{false};
    std::chrono::steady_clock::time_point started_{};
    std::uint64_t steps_used_ = 0;
    double elapsed_ms_ = 0.0;
    bool timed_out_ = false;
    bool iteration_capped_ = false;
    std::string error_;
};

}  // namespace timewarp::lang
