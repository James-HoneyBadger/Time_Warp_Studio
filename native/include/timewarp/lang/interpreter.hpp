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
//  * Input is synchronous. The reference stopped at an ASK and was resumed by a
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

/// One entry of the running program's call chain, outermost first.
///
/// A whole-program executor keeps its own idea of where it is -- Smalltalk's
/// message sends, for instance -- and this is how it says so while a debugger
/// has it stopped. `procedure` is the machine's own name for the frame
/// (`Counter>>increment`) and `line` is the source line it is at, or 0 when the
/// frame has no line of its own.
struct CallFrame {
    std::string procedure;
    int line = 0;
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

    /// What a whole-program executor's pause point answers.
    struct WholeProgramStep {
        /// False when the session asked for the run to stop.
        bool go_on = true;
        /// Set when a rewind has been applied and the executor must adopt this
        /// checkpoint -- its own program position included -- before it goes on.
        /// The executor casts it back to the type it passed in.
        std::optional<std::any> restored;
    };

    /// The pause point for an executor that can be walked back.
    ///
    /// `checkpoint` is that executor's own position and scratch state, which is
    /// what a snapshot of a whole-program language has to carry: the statement
    /// loop that records one for the line-by-line languages belongs to the
    /// interpreter, and a language that runs its own loop has to hand its position
    /// over here. Nothing is copied when no history was asked for.
    ///
    /// A rewind is applied inside this call and answered in `restored`; an
    /// executor that passes an empty checkpoint is never walked back, because
    /// there would be no position to put it back on.
    [[nodiscard]] WholeProgramStep whole_program_pause(int line,
                                                       const std::any& checkpoint);

    /// Supplies the running program's call chain while it is stopped.
    ///
    /// Set by an executor that keeps one. It is called on the interpreter's
    /// thread from inside a pause -- the session's `on_paused` and
    /// `on_data_changed` run before the run blocks -- so a machine that has a
    /// stack pays nothing until it actually stops, and what the probe reads is
    /// the state at the pause rather than a copy taken earlier.
    ///
    /// The executor must clear it before it returns: the probe refers to objects
    /// that live only for that run.
    void set_call_stack_probe(std::function<std::vector<CallFrame>()> probe) {
        call_stack_probe_ = std::move(probe);
    }
    /// The running program's call chain, outermost first; empty when the
    /// language keeps none. Meaningful only while the run is paused, because that
    /// is the only time the executor is between frames rather than inside one.
    ///
    /// A line-by-line language answers from the frames its statements pushed
    /// (below) rather than through a probe: its executor is a function called once
    /// per statement, so a closure over the executor would not outlive the
    /// statement that made it, while the return stack it mirrors does.
    [[nodiscard]] std::vector<CallFrame> call_stack() const {
        if (call_stack_probe_) {
            return call_stack_probe_();
        }
        std::vector<CallFrame> frames;
        if (call_frames_.empty()) {
            return frames;
        }
        // The program's own frame is the one the executor never pushes: it waits at
        // the line its first call was made from. Each frame below it is the callee's
        // own position -- its call site while it waits, the statement it is on while
        // it runs -- which is the convention the Smalltalk chain reports.
        frames.reserve(call_frames_.size() + 1);
        frames.push_back(CallFrame{"PROGRAM", call_frames_.front().call_site});
        for (const LineCall& call : call_frames_) {
            frames.push_back(CallFrame{call.procedure, call.line});
        }
        return frames;
    }

    /// Pushes a frame a line-by-line executor has entered, outermost first.
    ///
    /// `procedure` is what the language calls the callee and `call_site` is the
    /// line it was called from -- a waiting frame's line is its call site, so a
    /// `GOSUB 1000` on line 100 reads as one frame at 100 until the run arrives at
    /// the subroutine's own statements. The executor pops at exactly the points it
    /// pops its own return stack, which is what keeps the two from disagreeing.
    void push_call_frame(std::string procedure, int call_site) {
        call_frames_.push_back(LineCall{std::move(procedure), call_site, call_site});
    }
    /// Moves the innermost frame to `line`, for an executor that runs statements
    /// itself: `FOCAL`'s `DO` group and the `FUNCTION` bodies of BASIC and COMAL
    /// never pass through the run loop, so the loop cannot advance their frame.
    void set_call_frame_line(int line) noexcept {
        if (!call_frames_.empty()) {
            call_frames_.back().line = line;
        }
    }
    /// Pops the innermost frame. A no-op on an empty chain, so a language whose
    /// return stack has already been unwound cannot pop the program's frame.
    void pop_call_frame() noexcept {
        if (!call_frames_.empty()) {
            call_frames_.pop_back();
        }
    }
    /// Forgets every frame, for a statement that restarts or ends the program.
    void clear_call_frames() noexcept { call_frames_.clear(); }

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

    /// Puts the program counter back on `line` *without* recording a jump.
    ///
    /// For an executor that ran another line's statements inside the one it was
    /// given -- BASIC's user `FUNCTION` does, because a function body is statements
    /// -- and has to leave the run loop believing nothing happened. `jump_to` would
    /// make it see a jump to the line already executing, which is how `10 GOTO 10`
    /// loops: the statement would be run again.
    void resume_line(std::size_t line) noexcept {
        current_line = line;
        jumped_ = false;
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

    /// The machine underneath the language: the address space `POKE` writes and
    /// `PEEK` reads, the cursor `LOCATE` moves, the ink and paper `COLOR` sets and
    /// the screen `CLS` clears.
    ///
    /// Without a bus -- the IDE editor, `time_warp_check`, the parity gates -- a
    /// program gets `State::memory`, the sparse map a language may carry, and the
    /// statements behave exactly as they did. With one, a statement reaches real
    /// hardware, which is the difference between the HB-1 and an editor tab.
    ///
    /// Each hook returns false when the machine refused: a `poke` into ROM is the
    /// case that matters, because the program has to be told.
    struct MachineBus {
        std::function<std::uint8_t(int address)> peek;
        std::function<bool(int address, std::uint8_t value)> poke;
        std::function<void(int row, int column)> set_cursor;
        std::function<void(int index)> set_ink;
        std::function<void(int index)> set_paper;
        std::function<void()> clear_screen;
        /// A line of program output, on the engine's thread and in order with the
        /// `POKE`s around it. A machine's teletype printed what the program said when
        /// the program said it; a queued callback to the GUI would print it later,
        /// after a program that pokes the screen had already overwritten the cells.
        std::function<void(const std::string& line)> write_text;
    };
    void set_machine_bus(MachineBus bus) { machine_bus_ = std::move(bus); }
    [[nodiscard]] bool machine_attached() const noexcept {
        return static_cast<bool>(machine_bus_.poke);
    }
    /// Reads `address` from the machine. An executor that wants the sparse map a
    /// language carries instead asks `machine_attached()` first, which is what keeps
    /// the editor and the parity gates on the behaviour they have always had.
    [[nodiscard]] std::uint8_t machine_peek(int address);
    /// Writes `value`. False when the machine refused the address.
    bool machine_poke(int address, std::uint8_t value);
    void machine_locate(int row, int column);
    void machine_ink(int index);
    void machine_paper(int index);
    void machine_clear_screen();
    /// Sends one output line to the machine, if there is one. The caller still emits
    /// through `output_callback_`: this is the screen, that is the console.
    void machine_write_text(const std::string& line);

    /// The directory a program's relative file names are confined to.
    ///
    /// Empty means the process's working directory, which is what `time_warp_check`,
    /// the corpora and a terminal run see. The IDE sets it per run -- a project's
    /// root when one is open, else the document's own folder -- so `OPEN "notes.txt"`
    /// writes beside the program rather than beside whatever directory the IDE
    /// happened to be launched from, which is a coin toss on a desktop launcher.
    void set_file_base(std::string base) { file_base_ = std::move(base); }
    [[nodiscard]] const std::string& file_base() const noexcept { return file_base_; }

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

    /// Prints `[line]` before each statement, which is what a 1982 machine's
    /// `TRON` did and the only way to see which arm of a `GOTO` a program took.
    ///
    /// It goes through `log_output`, so a trace arrives in the same stream as
    /// the program's own output -- no second channel, no second thread, and a
    /// caller that already streams output gets tracing for one flag. Off by
    /// default, and a whole-program language has no statements to trace.
    void set_tracing(bool on) noexcept { tracing_ = on; }
    [[nodiscard]] bool tracing() const noexcept { return tracing_; }

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

    // -- going back --------------------------------------------------------
    //
    // A run is a position, a set of bindings, the output it has printed and the
    // language's own scratch state. All four are copied here, so a caller can return
    // to a point in a program rather than only to a line a breakpoint stopped at.
    //
    // #### What a snapshot is not
    //
    // It is not a reset of the world outside the engine. Drawing already sent to the
    // canvas stays drawn (the canvas has a scrubber for that); an open file stays
    // open and stays where it was; a `SLEEP` that has passed is not re-waited. What
    // is restored is what the program can see through its own variables, plus where
    // it will execute next -- which is what makes "step back and try the other arm"
    // work. The limits are stated in `docs/ROADMAP.md` rather than discovered.

    struct Snapshot {
        /// The index into `program_lines()` the run would execute next.
        std::size_t line = 0;
        std::map<std::string, Value> variables;
        /// The output produced so far, so a transcript can be rebuilt rather than
        /// appended to twice.
        std::vector<std::string> output;
        /// The language's own scratch state -- BASIC's `FOR` stack, COMAL's blocks,
        /// REXX's frames. A copy, which is why every executor's state has to be
        /// copy-constructible (the `executor_state` slot already requires it).
        std::any executor;
        std::uint64_t steps = 0;
        /// How many drawing calls the run had made when the snapshot was taken. The
        /// canvas is not rewound by `restore()`; this is what the UI scrubs to.
        std::uint64_t drawing_calls = 0;
        /// The source line as a person reads it, for a report.
        int source_line = 0;
    };

    /// The run's state as it is now. Meaningful between statements: a snapshot taken
    /// from inside an executor would capture its half-finished work.
    [[nodiscard]] Snapshot snapshot() const;

    /// Makes the run that snapshot again: bindings, output, scratch state and
    /// position. The interpreter must not be running.
    void restore(const Snapshot& state);

    /// Keeps the last `depth` snapshots the run passes, one every `interval`
    /// statements. Zero -- the default -- keeps none and costs nothing: a run with no
    /// interest in going back must not pay a copy per statement.
    void set_history(std::size_t depth, std::size_t interval = 1);
    [[nodiscard]] std::size_t historyDepth() const noexcept { return history_depth_; }
    /// The snapshots kept, oldest first.
    [[nodiscard]] const std::vector<Snapshot>& history() const noexcept { return history_; }
    /// Steps back `steps` snapshots and returns the one it landed on, or nullopt when
    /// there is nothing that far back. The newest snapshot is the statement the run is
    /// at, so `rewind(1)` goes to the previous one -- the statement before it. Does not
    /// resume the run: the caller decides.
    std::optional<Snapshot> rewind(std::size_t steps = 1);

private:
    void reset();
    void parse_lines(std::string_view source);
    /// Records the state before a statement, when a caller asked for history.
    /// Shared by the line loop and `whole_program_pause`.
    void capture_history();
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
    /// Set by `rewind`: the statement the run has just been put back on must not
    /// push a second copy of itself, or the newest snapshot would be the statement
    /// it is at and going back from there would go nowhere.
    bool suppress_history_capture_ = false;
    /// The snapshots the run has passed, oldest first, and how often to take one.
    std::vector<Snapshot> history_;
    std::size_t history_depth_ = 0;
    std::size_t history_interval_ = 1;
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
    bool tracing_ = false;
    MachineBus machine_bus_;
    std::string file_base_;
    std::map<std::size_t, LineProfile> profile_;
    bool timed_out_ = false;
    bool iteration_capped_ = false;
    /// Set by `jump_to`, cleared by the run loop before each statement.
    bool jumped_ = false;
    /// The executor's call chain, asked only when a pause is published.
    std::function<std::vector<CallFrame>()> call_stack_probe_;
    /// One frame a line-by-line language is inside.
    struct LineCall {
        std::string procedure;
        /// The caller's line when the call was made: where that caller waits.
        int call_site = 0;
        /// Where this frame is now, which the run loop and a self-driving executor
        /// keep current.
        int line = 0;
    };
    /// The chain a line-by-line language's statements maintain themselves, through
    /// `push_call_frame` and `pop_call_frame`.
    std::vector<LineCall> call_frames_;
    std::string error_;
};

}  // namespace timewarp::lang
