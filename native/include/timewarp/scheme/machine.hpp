// SPDX-License-Identifier: MIT
//
// A tree-walking Scheme evaluator with no native recursion.
//
// Ported from the reference Scheme implementation in the Python engine
// (native/bridge/time_warp/languages/scheme.py).
//
// ---------------------------------------------------------------------------
// The central idea
// ---------------------------------------------------------------------------
//
// The reference implemented evaluation by recursing: `eval()` called itself for
// the operator, for each operand, for every form of a body, for every special
// form, and `apply()` called `eval()` again for the callee's body.  Evaluation
// depth was therefore native call-stack depth, and CPython's 1000-frame limit
// capped a Scheme program at roughly 200 calls.
//
// Here, every one of those recursive sites becomes a *frame* -- a record of
// "what to do with the value that is about to be produced" -- pushed on an
// explicit, heap-backed `std::vector<Frame>`.  No member of this class calls
// any other member recursively; the evaluator is a flat loop over two states,
// `Eval` and `Deliver`.
//
// A frame is popped before its handler runs, and a handler pushes it back only
// when further work is needed.  That single rule is what gives *proper tail
// calls* for free: when a handler has no more work, it simply does not push
// itself back, so the value flows to the enclosing frame and the machine's
// depth stays constant.  `(loop 1000000)` runs in constant space, where the
// reference aborted at ~200.
//
// ---------------------------------------------------------------------------
// Depth accounting
// ---------------------------------------------------------------------------
//
//   * Tail calls:        O(1) machine frames.
//   * Non-tail calls:    O(Scheme depth) machine frames on the heap, never on
//                        the native stack.
//   * Nested data:       O(1) native stack -- the reader, printer and `equal?`
//                        each walk an explicit work stack too.

#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "timewarp/scheme/host.hpp"
#include "timewarp/scheme/source_map.hpp"
#include "timewarp/scheme/value.hpp"

namespace timewarp::scheme {

// ---------------------------------------------------------------------------
// Suspension
// ---------------------------------------------------------------------------

/// Why the machine stopped.
enum class SuspendReason : std::uint8_t {
    Finished = 0,      ///< the program ran to completion
    QuantumExhausted,  ///< the caller's step budget ran out first
    Paused,            ///< the suspension controller declined to continue
};

[[nodiscard]] std::string_view to_string(SuspendReason reason) noexcept;

/// Where the machine is, reported to the controller at every compound form.
struct SuspendState {
    /// Source range of the form about to be evaluated.  Unknown when no source
    /// map was supplied, or when the form was built at run time.
    LineSpan span;
    /// Name of the procedure being applied, or empty at top level.
    std::string_view procedure;
    /// Call depth: 0 is the top level, 1 the first procedure entered.
    std::uint32_t depth = 0;
    /// Evaluation steps charged so far.
    std::uint64_t steps = 0;
};

/// Consulted at the start of every compound-form evaluation.
///
/// An interface rather than a `std::function` because it is called once per
/// compound form, and the indirection has to stay cheap enough that attaching a
/// debugger does not change the program's character.  A null controller means
/// zero overhead: the check is a single pointer test.
class SuspensionController {
public:
    virtual ~SuspensionController() = default;
    /// Return false to suspend.  The machine is resumable at this exact point:
    /// the form is re-tested rather than re-entered, so the step count is not
    /// double-charged either.
    [[nodiscard]] virtual bool on_form(const SuspendState& state) = 0;
};

/// One entry of the debugger's call stack, outermost first.
struct CallFrame {
    std::string procedure;
    LineSpan span;
};

/// A binding visible at the current position, already rendered for display.
struct VariableEntry {
    std::string name;
    std::string rendered;
    /// The value's type, as `type_name` reports it.
    std::string kind;
};

// ---------------------------------------------------------------------------
// Continuation frames
// ---------------------------------------------------------------------------

/// Bottom of the stack.  A value delivered here ends the program.
struct HaltFrame {};

/// An application: evaluate the operator, then each operand left to right, then
/// apply.  Corresponds to `proc = self.eval(head, env)` followed by the
/// `args = [self.eval(a, env) for a in expr[1:]]` list comprehension.
struct ApplicationFrame {
    Value proc;              ///< the operator, once evaluated
    Value cursor;            ///< operands still to evaluate
    std::vector<Value> args; ///< operands evaluated so far
    Env* env = nullptr;
    bool have_proc = false;
};

/// `begin`, and every procedure body.
///
/// Pops itself before the final form, so that form is in tail position.
struct SequenceFrame {
    Value forms;  ///< forms still to evaluate, excluding the current one
    Env* env = nullptr;
    /// Non-null on an *activation marker*: an empty sequence frame that exists
    /// only to delimit a procedure call, so a debugger can name it and measure
    /// its depth.  Markers are pushed only while a suspension controller is
    /// attached, so a normal run's frames and tail calls are untouched.
    const Procedure* owner = nullptr;
    /// The caller's tail-position base, saved when the marker is pushed.
    ///
    /// It has to be the caller's *saved* value rather than a height derived from
    /// this marker: the frames between the caller's base and this marker belong
    /// to the call site, and they are already gone by the time the marker is
    /// popped.  Restoring a derived height would leave `at_tail_position()`
    /// comparing against a stack shape that no longer exists, silently losing
    /// the tail call that followed.
    std::size_t saved_base = 0;
};

/// `if`.  Also carries the body of `when`/`unless` after their test passes.
struct IfFrame {
    Value consequent;
    Value alternative;
    Env* env = nullptr;
    bool has_consequent = false;
    bool has_alternative = false;
};

struct WhenFrame {
    Value forms;
    Env* env = nullptr;
    bool expect_truthy = true;  ///< false for `unless`
};

/// `and`: short-circuits on `#f`, otherwise yields the last value.
struct AndFrame {
    Value forms;
    Value last;
    Env* env = nullptr;
};

/// `or`: yields the first truthy value, else `#f`.
struct OrFrame {
    Value forms;
    Env* env = nullptr;
};

/// `cond`, positioned just after the current clause's test was evaluated.
struct CondFrame {
    Value clause;   ///< the clause whose test produced the delivered value
    Value clauses;  ///< clauses still to consider
    Env* env = nullptr;
};

/// `cond`'s `=>` form, waiting for the receiver expression to evaluate.
struct CondArrowFrame {
    Value argument;
    Env* env = nullptr;
};

/// `case`, waiting for its key expression.
struct CaseFrame {
    Value clauses;
    Env* env = nullptr;
};

/// `set!`, waiting for the new value.
struct SetFrame {
    Symbol* name = nullptr;
    Env* env = nullptr;
};

/// `define`, waiting for the value.  Yields the *symbol* when it completes,
/// which is the reference's (odd but observable) behaviour.
struct DefineFrame {
    Symbol* name = nullptr;
    Env* env = nullptr;
};

/// `let`, `let*` and `letrec`.
///
/// One frame serves all three: they differ only in when the local environment
/// is created and which environment the initialisers are evaluated in.
struct LetFrame {
    enum class Kind : std::uint8_t { Plain, Star, Rec };

    Kind kind = Kind::Plain;
    Value specs;  ///< binding specifications still to evaluate
    Value body;
    Env* outer = nullptr;
    Env* local = nullptr;       ///< already created for `let*` and `letrec`
    Symbol* name = nullptr;     ///< non-null for a named `let`
    std::vector<Symbol*> names;
    std::vector<Value> values;
};

/// `do`.
///
/// Phases are split into "about to evaluate" and "waiting for the result of
/// that evaluation", because on delivery the machine must know whether the
/// value is an initialiser (which *defines* a binding) or a step (which
/// *assigns* one).
struct DoFrame {
    enum class Phase : std::uint8_t {
        Init,
        AwaitInit,
        Steps,
        AwaitStep,
        AwaitTest,
        AwaitBody,
    };

    Phase phase = Phase::Init;
    Value specs;   ///< the whole spec list, re-walked for init and for steps
    Value cursor;  ///< position inside `specs`
    Value test;    ///< the test clause
    Value body;
    Env* local = nullptr;
    std::vector<Symbol*> names;
    std::size_t spec_index = 0;  ///< specs consumed, indexes into `names`
};

/// Resumes the quasiquote driver after an `unquote`/`unquote-splicing` has been
/// evaluated.  The driver's own state lives in `Machine::qq_jobs_`.
struct QuasiquoteFrame {};

/// `map` (`collect`) and `for-each` (`!collect`).
struct MapFrame {
    Value proc;
    std::vector<std::vector<Value>> columns;
    std::size_t index = 0;
    std::size_t count = 0;
    std::vector<Value> acc;
    bool collect = true;
};

/// `filter`.
struct FilterFrame {
    Value proc;
    std::vector<Value> source;
    std::size_t index = 0;
    std::vector<Value> acc;
};

/// `foldl`.
struct FoldFrame {
    Value proc;
    std::vector<Value> source;
    std::size_t index = 0;
    Value acc;
};

/// `apply`.
///
/// No frame is needed: all of `apply`'s arguments are already evaluated by the
/// time the builtin runs, so the trailing list is spliced immediately.
using Frame = std::variant<
    HaltFrame, ApplicationFrame, SequenceFrame, IfFrame, WhenFrame, AndFrame,
    OrFrame, CondFrame, CondArrowFrame, CaseFrame, SetFrame, DefineFrame,
    LetFrame, DoFrame, QuasiquoteFrame, MapFrame, FilterFrame, FoldFrame>;

// ---------------------------------------------------------------------------
// Special forms
// ---------------------------------------------------------------------------

enum class SpecialForm : std::uint8_t {
    None = 0,
    Quote, Quasiquote, If, Define, Lambda, Set, Begin, When, Unless, And, Or,
    Cond, Case, Let, LetStar, LetRec, Do,
};

/// Looks up a special form by name.  `defun` aliases `define`, matching the
/// reference's `_SPECIAL_FORMS` table.
[[nodiscard]] SpecialForm find_special_form(std::string_view name);

// ---------------------------------------------------------------------------
// The machine
// ---------------------------------------------------------------------------

class Machine {
public:
    Machine(Arena& arena, Host& host);

    /// Evaluates every top-level form and returns all emitted output.
    [[nodiscard]] std::string run(std::string_view source);

    /// Evaluates forms that have already been read.
    [[nodiscard]] std::string run_forms(const std::vector<Value>& forms);

    /// Runs `source` and renders failures exactly as `execute_scheme()` does:
    /// `"❌ Scheme error: <message>\n"`.  Never throws.
    [[nodiscard]] std::string execute(std::string_view source);

    [[nodiscard]] Env* global_env() const noexcept { return global_; }
    [[nodiscard]] std::uint64_t steps() const noexcept { return steps_; }

    /// Aborts the program once `limit` evaluation steps have been charged.
    /// Zero (the default) means unlimited.
    void set_step_limit(std::uint64_t limit) noexcept { step_limit_ = limit; }

    /// Deepest the continuation stack ever got.  Lets a test *prove* that a
    /// deeply recursive program ran in bounded space.
    [[nodiscard]] std::size_t peak_frame_depth() const noexcept {
        return peak_frames_;
    }

    // -- background execution and debugging -------------------------------
    //
    // `run`/`run_forms`/`execute` above are run-to-completion.  The entry points
    // below expose the *same* loop as a resumable state machine -- which the
    // machine already was.  `mode_` plus the frame stack fully describe where
    // execution has got to, so suspending is a matter of returning out of the
    // loop rather than unwinding anything, and there is no native stack to
    // preserve.

    /// Attaches a controller, or detaches with nullptr.
    ///
    /// While one is attached the machine also tracks procedure activations, so
    /// it can report a call stack; detaching restores plain behaviour.
    void set_suspension_controller(SuspensionController* controller) noexcept {
        controller_ = controller;
    }

    /// Reads `source` and prepares it without running anything.  Throws
    /// `ReaderError` on malformed input.
    ///
    /// When `source_map` is supplied it is attached and filled with form spans,
    /// which is what makes breakpoints and "current line" possible.
    void load(std::string_view source, SourceMap* source_map = nullptr);

    /// Prepares already-read forms.
    void load_forms(std::vector<Value> forms);

    /// Runs at most `max_steps` evaluation steps, or until the controller
    /// suspends.  Resumable: call again to carry on from where it stopped.
    SuspendReason step_quantum(std::uint64_t max_steps);

    /// Runs until the program finishes or the controller suspends it.
    SuspendReason run_loaded();

    [[nodiscard]] bool loaded() const noexcept { return program_loaded_; }
    [[nodiscard]] bool finished() const noexcept;

    /// The current position; meaningful while suspended.
    [[nodiscard]] SuspendState suspend_state() const;

    /// The call stack, outermost first.
    ///
    /// A call in tail position does not nest, which is *correct* for Scheme
    /// rather than a gap: tail calls genuinely do not grow the stack, and the
    /// caller's marker is removed so that stays true even while debugging.
    [[nodiscard]] std::vector<CallFrame> call_stack() const;

    /// Bindings visible at the current position, innermost frame first.
    /// Builtins are skipped: listing ~90 of them would drown the useful ones.
    [[nodiscard]] std::vector<VariableEntry> visible_variables(
        std::uint32_t max_frames = 4) const;

    /// Output emitted so far, so a background runner can stream it.
    [[nodiscard]] const std::string& output() const noexcept { return out_; }

private:
    enum class Mode : std::uint8_t { Eval, Deliver, Halt };

    // -- driver ----------------------------------------------------------
    void eval_step();
    bool deliver_step();
    void charge(std::uint64_t amount);
    void note_depth();
    /// Appends to the program's output.
    ///
    /// Deliberately *not* named `emit`: Qt defines that as a macro, so a member
    /// with that name is silently erased in any translation unit that also
    /// includes a Qt header -- which produced an inexplicable parse error here
    /// once a Qt-using target started including this header.
    void append_output(std::string_view text);

    // -- registers -------------------------------------------------------
    void evaluate(Value expression, Env* env);
    void deliver_value(Value value);

    /// Evaluates `forms` as a sequence.  A single form is evaluated in tail
    /// position; an empty sequence yields the unspecified value.
    ///
    /// `owner` is set only for a procedure body entered while a suspension
    /// controller is attached; it causes an activation marker to be pushed so a
    /// debugger can name this call and measure its depth.
    void begin_sequence(Value forms, Env* env, const Procedure* owner = nullptr);

    // -- application -----------------------------------------------------
    /// `tail_call` says the application occupied a tail position, so a debugger
    /// should replace the caller's activation rather than nest inside it.
    void start_apply(Value proc, std::vector<Value> args,
                     bool tail_call = false);
    void apply_primitive(PrimitiveId id, const std::vector<Value>& args);

    // -- frame handlers (one per continuation frame) ---------------------
    void handle(HaltFrame& frame);
    void handle(ApplicationFrame& frame);
    void handle(SequenceFrame& frame);
    void handle(IfFrame& frame);
    void handle(WhenFrame& frame);
    void handle(AndFrame& frame);
    void handle(OrFrame& frame);
    void handle(CondFrame& frame);
    void handle(CondArrowFrame& frame);
    void handle(CaseFrame& frame);
    void handle(SetFrame& frame);
    void handle(DefineFrame& frame);
    void handle(LetFrame& frame);
    void handle(DoFrame& frame);
    void handle(QuasiquoteFrame& frame);
    void handle(MapFrame& frame);
    void handle(FilterFrame& frame);
    void handle(FoldFrame& frame);

    // -- special forms ---------------------------------------------------
    void form_quote(Value operands);
    void form_quasiquote(Value operands, Env* env);
    void form_if(Value operands, Env* env);
    void form_define(Value operands, Env* env);
    void form_lambda(Value operands, Env* env);
    void form_set(Value operands, Env* env);
    void form_when(Value operands, Env* env, bool expect_truthy);
    void form_and(Value operands, Env* env);
    void form_or(Value operands, Env* env);
    void form_cond(Value clauses, Env* env);
    void form_case(Value operands, Env* env);
    void form_let(Value operands, Env* env, LetFrame::Kind kind);
    void form_do(Value operands, Env* env);

    /// Continues `cond` from `clauses`, evaluating a test if needed.
    void cond_advance(Value clauses, Env* env);
    /// Continues `case` from `clauses` with an already-evaluated key.
    void case_advance(Value clauses, Value key, Env* env);
    /// Walks the remaining binding specifications of a `let` form.
    void let_advance(LetFrame frame);
    /// Continues `do` in the requested phase.
    void do_advance(DoFrame frame);

    // -- quasiquote driver ------------------------------------------------
    void quasiquote_start(Value value, Env* env, int depth);
    void quasiquote_step();
    /// Hands a finished sub-template to its enclosing context.
    ///
    /// Returns true when the caller must stop stepping: either a further
    /// evaluation was requested, or the session completed and `value_` holds
    /// the result.  Returns false when the value was merely accumulated.
    [[nodiscard]] bool quasiquote_publish(Value result);

    // -- environment -----------------------------------------------------
    [[nodiscard]] Value lookup(Env* env, Symbol* name);
    /// Rebinds an existing binding; returns false when `name` is unbound, which
    /// is how `set!` falls back to defining it.
    [[nodiscard]] static bool assign(Env* env, Symbol* name, Value value);
    static void define_in(Env* env, Symbol* name, Value value);

    // -- helpers ---------------------------------------------------------
    [[nodiscard]] Value require_number(const Value& value,
                                       std::string_view who) const;
    [[nodiscard]] double as_real(const Value& value) const;
    [[nodiscard]] Value number_from_real(double value) const;
    [[nodiscard]] Value number_from_integer(std::int64_t value) const;
    void install_builtins();

    // -- state -----------------------------------------------------------
    Arena& arena_;
    Host& host_;

    Env* global_ = nullptr;
    std::vector<Frame> frames_;
    std::string out_;

    Mode mode_ = Mode::Halt;
    Value expr_;
    Env* env_ = nullptr;
    Value value_;

    std::uint64_t steps_ = 0;
    std::uint64_t step_limit_ = 0;
    std::size_t peak_frames_ = 0;

    // -- background execution state --------------------------------------

    /// Span of a procedure's body, for the debugger's call stack.  The body is a
    /// list, so its own recorded span is the enclosing definition; this reports
    /// where the body actually opens.
    [[nodiscard]] LineSpan body_span(const Procedure& procedure) const;

    /// Arms the next top-level form of `program_`, or leaves `mode_` at Halt
    /// when there is none left.
    void arm_next_form();

    /// True when nothing is pending above the current activation, which is
    /// exactly what tail position means in this machine.
    [[nodiscard]] bool at_tail_position() const noexcept {
        return frames_.size() == current_base_;
    }
    [[nodiscard]] static bool is_activation_marker(
        const SequenceFrame& frame) noexcept {
        return frame.owner != nullptr && frame.forms.type == ValueType::Nil;
    }

    std::vector<Value> program_;
    std::size_t next_form_ = 0;
    bool program_loaded_ = false;
    bool suspend_requested_ = false;
    SuspensionController* controller_ = nullptr;
    const SourceMap* source_map_ = nullptr;
    LineSpan current_span_;
    LineSpan top_level_span_;
    /// Live procedure activations, innermost last.  Maintained incrementally so
    /// depth is O(1) rather than a walk of the frame stack on every form.
    std::vector<const Procedure*> activations_;
    /// Frame count at which the current activation's body began.
    std::size_t current_base_ = 0;

    /// Quasiquote driver state.
    ///
    /// A template is walked with its own explicit work stack, because `_qq()`
    /// was the last recursive path in the reference's evaluator.  Sessions
    /// nest: an `unquote` may itself evaluate another `quasiquote`, so a nested
    /// session is pushed and, on completion, its result is published into the
    /// session beneath it.  A single flat driver would clobber the outer
    /// template's partially built accumulator.
    struct QqJob {
        enum class Kind : std::uint8_t { Enter, Finish, WrapQuasiquote, Splice };
        Kind kind = Kind::Enter;
        Value value;
        int depth = 1;
    };
    enum class QqPending : std::uint8_t { None, Replace, Splice };

    struct QqSession {
        std::vector<QqJob> jobs;
        std::vector<std::vector<Value>> accs;
        QqPending pending = QqPending::None;
        Env* env = nullptr;
    };

    std::vector<QqSession> qq_sessions_;
    std::vector<Value> qq_splice_scratch_;
};

}  // namespace timewarp::scheme
