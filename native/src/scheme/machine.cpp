// SPDX-License-Identifier: MIT
//
// The explicit-stack evaluator.
//
// Read `machine.hpp` first: it explains why every recursive site in the
// reference became a frame, and why "pop the frame, push it back only when more
// work is needed" yields proper tail calls.
//
// No function in this file calls itself, directly or indirectly.  Verified by
// inspection of every member, and by
// `Machine::peak_frame_depth()` staying flat across a million-deep tail loop.

#include "timewarp/scheme/machine.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <string>
#include <utility>
#include <vector>

#include "timewarp/scheme/printer.hpp"
#include "timewarp/scheme/reader.hpp"

namespace timewarp::scheme {
namespace {

struct SpecialFormEntry {
    std::string_view name;
    SpecialForm form;
};

/// Sorted by name so it can be binary searched.  `defun` is an alias, exactly
/// as in the reference's `_SPECIAL_FORMS` table.
constexpr SpecialFormEntry kSpecialForms[] = {
    {"and", SpecialForm::And},           {"begin", SpecialForm::Begin},
    {"case", SpecialForm::Case},         {"cond", SpecialForm::Cond},
    {"define", SpecialForm::Define},     {"defun", SpecialForm::Define},
    {"do", SpecialForm::Do},             {"if", SpecialForm::If},
    {"lambda", SpecialForm::Lambda},     {"let", SpecialForm::Let},
    {"let*", SpecialForm::LetStar},      {"letrec", SpecialForm::LetRec},
    {"or", SpecialForm::Or},             {"quasiquote", SpecialForm::Quasiquote},
    {"quote", SpecialForm::Quote},       {"set!", SpecialForm::Set},
    {"unless", SpecialForm::Unless},     {"when", SpecialForm::When},
};

static_assert(std::size(kSpecialForms) == 18);

/// Splits a parameter list into fixed parameters and an optional `. rest`.
///
/// The reference's `_parse_params()`: `.` is matched as a *symbol*, because the
/// reader has no dotted-pair syntax -- `int(".")` fails, so `_atom(".")` yields
/// a symbol.  That is why this works at all.
void parse_params(Value items, std::vector<Symbol*>& params, Symbol*& rest) {
    while (items.type == ValueType::Pair) {
        const Value item = items.pair->car;
        items = items.pair->cdr;
        if (is_symbol_named(item, ".")) {
            if (items.type == ValueType::Pair &&
                items.pair->car.type == ValueType::Symbol) {
                rest = items.pair->car.symbol;
            }
            return;
        }
        if (item.type != ValueType::Symbol) {
            throw SchemeError{"lambda: expected a symbol parameter"};
        }
        params.push_back(item.symbol);
    }
}

/// Numeric coercion shared by `eq?`/`eqv?`/`equal?` lives in value.cpp, so that
/// both this file and builtins.cpp share one definition of equality.

}  // namespace

SpecialForm find_special_form(std::string_view name) {
    const SpecialFormEntry* begin = std::begin(kSpecialForms);
    const SpecialFormEntry* end = std::end(kSpecialForms);
    const SpecialFormEntry* found = std::lower_bound(
        begin, end, name, [](const SpecialFormEntry& entry,
                             std::string_view key) { return entry.name < key; });
    if (found != end && found->name == name) {
        return found->form;
    }
    return SpecialForm::None;
}

// ---------------------------------------------------------------------------
// Construction and entry points
// ---------------------------------------------------------------------------

Machine::Machine(Arena& arena, Host& host) : arena_{arena}, host_{host} {
    global_ = arena_.make_env(nullptr);
    install_builtins();
}

std::string Machine::run(std::string_view source) {
    return run_forms(parse_program(arena_, source));
}

std::string_view to_string(SuspendReason reason) noexcept {
    switch (reason) {
        case SuspendReason::Finished:
            return "Finished";
        case SuspendReason::QuantumExhausted:
            return "QuantumExhausted";
        case SuspendReason::Paused:
            return "Paused";
    }
    return "Unknown";
}

// ---------------------------------------------------------------------------
// Loading and the resumable driver
// ---------------------------------------------------------------------------

void Machine::load(std::string_view source, SourceMap* source_map) {
    source_map_ = source_map;
    load_forms(parse_program(arena_, source, source_map));
}

void Machine::load_forms(std::vector<Value> forms) {
    program_ = std::move(forms);
    next_form_ = 0;
    program_loaded_ = true;
    suspend_requested_ = false;
    mode_ = Mode::Halt;
    frames_.clear();
    activations_.clear();
    current_base_ = 0;
    qq_sessions_.clear();
    // A fresh program starts from zero.  Without this the step count -- which is
    // what a debugger displays and what a step limit is checked against -- would
    // carry over from whatever ran before in the same machine.
    steps_ = 0;
}

std::string Machine::run_forms(const std::vector<Value>& forms) {
    load_forms(forms);
    (void)run_loaded();
    return out_;
}

void Machine::arm_next_form() {
    // Mirrors the per-form prologue the old run-to-completion loop used, so a
    // program behaves identically whether it is stepped or run straight through.
    frames_.clear();
    activations_.clear();
    qq_sessions_.clear();

    const Value form = program_[next_form_];
    top_level_span_ =
        source_map_ != nullptr ? source_map_->lookup(form) : LineSpan{};
    current_span_ = top_level_span_;

    // The Halt frame is the program's outermost continuation; everything else is
    // pushed and popped above it.
    evaluate(form, global_);
    current_base_ = frames_.size();
    ++next_form_;
}

SuspendReason Machine::step_quantum(std::uint64_t max_steps) {
    std::uint64_t budget = max_steps;

    for (;;) {
        if (mode_ == Mode::Halt) {
            if (next_form_ >= program_.size()) {
                return SuspendReason::Finished;
            }
            arm_next_form();
            continue;
        }
        if (suspend_requested_) {
            // Left exactly where the controller declined, so resuming re-tests
            // the same form rather than skipping or re-entering it.
            suspend_requested_ = false;
            return SuspendReason::Paused;
        }
        if (budget == 0) {
            return SuspendReason::QuantumExhausted;
        }
        --budget;
        if (mode_ == Mode::Eval) {
            eval_step();
        } else {
            (void)deliver_step();
        }
    }
}

SuspendReason Machine::run_loaded() {
    return step_quantum(std::numeric_limits<std::uint64_t>::max());
}

bool Machine::finished() const noexcept {
    if (!program_loaded_) {
        return true;
    }
    return mode_ == Mode::Halt && next_form_ >= program_.size();
}

SuspendState Machine::suspend_state() const {
    SuspendState state;
    state.span = current_span_;
    state.steps = steps_;
    state.depth = static_cast<std::uint32_t>(activations_.size());
    if (!activations_.empty() && activations_.back() != nullptr) {
        state.procedure = activations_.back()->name;
    }
    return state;
}

LineSpan Machine::body_span(const Procedure& procedure) const {
    // A body is the *list* of body forms, which is a `cdr` of the definition.  It
    // has a span -- the source map records every pair the reader built -- but
    // that span is the whole `define`, so reporting it would point a debugger at
    // the `define` line instead of at the body.  Report where the first body form
    // opens, which is the question a call-stack view is actually asking.
    if (source_map_ == nullptr || procedure.body.type != ValueType::Pair) {
        return LineSpan{};
    }
    return source_map_->lookup(procedure.body.pair->car);
}

std::vector<CallFrame> Machine::call_stack() const {
    std::vector<CallFrame> stack;
    stack.push_back(CallFrame{"<program>", top_level_span_});

    for (const Frame& frame : frames_) {
        const auto* sequence = std::get_if<SequenceFrame>(&frame);
        if (sequence == nullptr || !is_activation_marker(*sequence)) {
            continue;
        }
        const Procedure* owner = sequence->owner;
        CallFrame entry;
        entry.procedure = owner != nullptr ? owner->name : std::string{"?"};
        if (owner != nullptr) {
            entry.span = body_span(*owner);
        }
        stack.push_back(std::move(entry));
    }
    return stack;
}

std::vector<VariableEntry> Machine::visible_variables(
    std::uint32_t max_frames) const {
    std::vector<VariableEntry> entries;
    std::uint32_t visited = 0;

    for (Env* env = env_; env != nullptr && visited < max_frames;
         env = env->parent, ++visited) {
        for (const auto& binding : env->bindings) {
            const Value& value = binding.second;
            if (value.type == ValueType::Procedure &&
                value.procedure->prim != PrimitiveId::None) {
                // A builtin.  There are ~90 of them and they would bury the
                // program's own bindings, which is what the watch table wants.
                continue;
            }
            entries.push_back(VariableEntry{binding.first->name,
                                            write_to_string(value),
                                            std::string{type_name(value)}});
        }
    }
    return entries;
}

std::string Machine::execute(std::string_view source) {
    try {
        const std::string text = run(source);
        if (text.empty()) {
            return {};
        }
        return text.back() == '\n' ? text : text + "\n";
    } catch (const SchemeError& error) {
        return "❌ Scheme error: " + std::string{error.what()} + "\n";
    } catch (const std::exception& error) {
        return "❌ Scheme error: " + std::string{error.what()} + "\n";
    }
}

void Machine::evaluate(Value expression, Env* env) {
    frames_.push_back(HaltFrame{});
    expr_ = expression;
    env_ = env;
    mode_ = Mode::Eval;
    note_depth();
}

void Machine::deliver_value(Value value) {
    value_ = value;
    mode_ = Mode::Deliver;
}

// ---------------------------------------------------------------------------
// Driver loop
// ---------------------------------------------------------------------------

void Machine::eval_step() {
    // Consulted before the step is charged, so suspending and resuming does not
    // double-count the form in flight.  Only compound forms are reported: a leaf
    // has nothing to step into, and notifying on those would multiply the
    // controller's work for no benefit.
    if (controller_ != nullptr && expr_.type == ValueType::Pair) {
        current_span_ =
            source_map_ != nullptr ? source_map_->lookup(expr_.pair) : LineSpan{};
        if (!controller_->on_form(suspend_state())) {
            suspend_requested_ = true;
            return;
        }
    }

    charge(1);

    if (expr_.type == ValueType::Symbol) {
        value_ = lookup(env_, expr_.symbol);
        mode_ = Mode::Deliver;
        return;
    }

    if (expr_.type == ValueType::Pair) {
        const Value head = expr_.pair->car;
        if (head.type == ValueType::Symbol) {
            switch (find_special_form(head.symbol->name)) {
                case SpecialForm::Quote:
                    form_quote(expr_.pair->cdr);
                    return;
                case SpecialForm::Quasiquote:
                    form_quasiquote(expr_.pair->cdr, env_);
                    return;
                case SpecialForm::If:
                    form_if(expr_.pair->cdr, env_);
                    return;
                case SpecialForm::Define:
                    form_define(expr_.pair->cdr, env_);
                    return;
                case SpecialForm::Lambda:
                    form_lambda(expr_.pair->cdr, env_);
                    return;
                case SpecialForm::Set:
                    form_set(expr_.pair->cdr, env_);
                    return;
                case SpecialForm::Begin:
                    // A `begin` *is* a sequence: reusing the sequence frame
                    // makes the final form of a `begin` a tail position.
                    begin_sequence(expr_.pair->cdr, env_);
                    return;
                case SpecialForm::When:
                    form_when(expr_.pair->cdr, env_, true);
                    return;
                case SpecialForm::Unless:
                    form_when(expr_.pair->cdr, env_, false);
                    return;
                case SpecialForm::And:
                    form_and(expr_.pair->cdr, env_);
                    return;
                case SpecialForm::Or:
                    form_or(expr_.pair->cdr, env_);
                    return;
                case SpecialForm::Cond:
                    cond_advance(expr_.pair->cdr, env_);
                    return;
                case SpecialForm::Case:
                    form_case(expr_.pair->cdr, env_);
                    return;
                case SpecialForm::Let:
                    form_let(expr_.pair->cdr, env_, LetFrame::Kind::Plain);
                    return;
                case SpecialForm::LetStar:
                    form_let(expr_.pair->cdr, env_, LetFrame::Kind::Star);
                    return;
                case SpecialForm::LetRec:
                    form_let(expr_.pair->cdr, env_, LetFrame::Kind::Rec);
                    return;
                case SpecialForm::Do:
                    form_do(expr_.pair->cdr, env_);
                    return;
                case SpecialForm::None:
                    break;
            }
        }

        frames_.push_back(ApplicationFrame{unspecified_value(), expr_.pair->cdr,
                                           {}, env_, false});
        expr_ = head;
        note_depth();
        return;
    }

    // `()` is not self-evaluating: the reference rejects it explicitly.
    if (expr_.type == ValueType::Nil) {
        throw SchemeError{"cannot evaluate ()"};
    }

    value_ = expr_;
    mode_ = Mode::Deliver;
}

bool Machine::deliver_step() {
    if (frames_.empty()) {
        mode_ = Mode::Halt;
        return false;
    }
    Frame frame = std::move(frames_.back());
    frames_.pop_back();
    std::visit([this](auto& typed) { handle(typed); }, frame);
    return mode_ != Mode::Halt;
}

void Machine::charge(std::uint64_t amount) {
    steps_ += amount;
    if (step_limit_ != 0 && steps_ > step_limit_) {
        throw BudgetExhausted{steps_};
    }
    if (steps_ % 512 == 0 && !host_.tick(512)) {
        throw BudgetExhausted{steps_};
    }
}

void Machine::note_depth() {
    if (frames_.size() > peak_frames_) {
        peak_frames_ = frames_.size();
    }
}

void Machine::append_output(std::string_view text) { out_.append(text); }

// ---------------------------------------------------------------------------
// Environment
// ---------------------------------------------------------------------------

Value Machine::lookup(Env* env, Symbol* name) {
    for (Env* current = env; current != nullptr; current = current->parent) {
        const auto found = current->bindings.find(name);
        if (found != current->bindings.end()) {
            return found->second;
        }
    }
    throw SchemeError{"unbound variable: " + name->name};
}

bool Machine::assign(Env* env, Symbol* name, Value value) {
    for (Env* current = env; current != nullptr; current = current->parent) {
        const auto found = current->bindings.find(name);
        if (found != current->bindings.end()) {
            found->second = value;
            return true;
        }
    }
    return false;
}

void Machine::define_in(Env* env, Symbol* name, Value value) {
    env->bindings.insert_or_assign(name, value);
}

// ---------------------------------------------------------------------------
// Sequences and application
// ---------------------------------------------------------------------------

void Machine::begin_sequence(Value forms, Env* env, const Procedure* owner) {
    // While a debugger is attached, a procedure body gets an activation marker:
    // an empty sequence frame whose only job is to delimit the call, so the
    // debugger can name it and measure its depth.  Without a controller nothing
    // is pushed, so the frame layout -- and therefore tail behaviour -- is
    // exactly what a plain run has.
    if (owner != nullptr && controller_ != nullptr) {
        frames_.push_back(
            SequenceFrame{nil_value(), env, owner, current_base_});
        activations_.push_back(owner);
        current_base_ = frames_.size();
        note_depth();
    }

    if (forms.type == ValueType::Nil) {
        value_ = unspecified_value();
        mode_ = Mode::Deliver;
        return;
    }
    if (forms.type != ValueType::Pair) {
        throw SchemeError{"expected a proper list"};
    }

    const Value first = forms.pair->car;
    const Value rest = forms.pair->cdr;

    // A lone form is evaluated in tail position: no frame is pushed, so its
    // value returns straight to the enclosing continuation.
    if (rest.type == ValueType::Nil) {
        expr_ = first;
        env_ = env;
        mode_ = Mode::Eval;
        return;
    }

    frames_.push_back(SequenceFrame{rest, env});
    note_depth();
    expr_ = first;
    env_ = env;
    mode_ = Mode::Eval;
}

void Machine::start_apply(Value proc, std::vector<Value> args, bool tail_call) {
    if (proc.type != ValueType::Procedure) {
        throw SchemeError{"attempt to call non-procedure: " +
                          write_to_string(proc)};
    }

    Procedure* procedure = proc.procedure;
    if (procedure->prim != PrimitiveId::None) {
        apply_primitive(procedure->prim, args);
        return;
    }

    Env* local = arena_.make_env(procedure->env);
    const std::size_t fixed = procedure->params.size();
    for (std::size_t index = 0; index < fixed; ++index) {
        // Missing arguments bind to the empty list and surplus ones are
        // dropped, which is exactly what the reference's `apply()` does.
        define_in(local, procedure->params[index],
                  index < args.size() ? args[index] : nil_value());
    }
    if (procedure->rest != nullptr) {
        const std::size_t from = std::min(fixed, args.size());
        std::vector<Value> tail(args.begin() + static_cast<std::ptrdiff_t>(from),
                                args.end());
        define_in(local, procedure->rest, list_from(arena_, tail));
    }

    if (controller_ != nullptr && tail_call && !frames_.empty()) {
        const auto* top = std::get_if<SequenceFrame>(&frames_.back());
        if (top != nullptr && is_activation_marker(*top)) {
            // A tail call replaces the caller rather than nesting inside it.
            // Dropping the caller's marker is what keeps the continuation stack
            // from growing, so proper tail calls survive being debugged -- and
            // the reported stack stays semantically honest, because a tail call
            // really does not deepen the stack.
            //
            // The callee's marker then inherits the caller's saved base, so the
            // chain of "what am I inside of" stays intact across the splice.
            current_base_ = top->saved_base;
            frames_.pop_back();
            if (!activations_.empty()) {
                activations_.pop_back();
            }
        }
    }

    // The body runs as a sequence, so the last form is a tail call.  A closure
    // recursing in tail position therefore reuses this continuation instead of
    // growing the frame stack.
    begin_sequence(procedure->body, local, procedure);
}

// ---------------------------------------------------------------------------
// Frame handlers
// ---------------------------------------------------------------------------

void Machine::handle(HaltFrame&) { mode_ = Mode::Halt; }

void Machine::handle(ApplicationFrame& frame) {
    Env* const frame_env = frame.env;

    if (!frame.have_proc) {
        frame.proc = value_;
        frame.have_proc = true;
    } else {
        frame.args.push_back(value_);
    }

    if (frame.cursor.type != ValueType::Pair) {
        // Everything is evaluated: apply.  The frame is *not* pushed back, so
        // this call sits in the position its own application occupied.
        start_apply(frame.proc, std::move(frame.args), at_tail_position());
        return;
    }

    const Value next = frame.cursor.pair->car;
    frame.cursor = frame.cursor.pair->cdr;
    frames_.push_back(std::move(frame));
    note_depth();
    expr_ = next;
    env_ = frame_env;
    mode_ = Mode::Eval;
}

void Machine::handle(SequenceFrame& frame) {
    if (frame.forms.type == ValueType::Nil) {
        if (is_activation_marker(frame)) {
            // An activation marker: the call this delimits has returned.
            if (!activations_.empty()) {
                activations_.pop_back();
            }
            // Back to the caller's own tail position, restored rather than
            // recomputed -- see the note on `SequenceFrame::saved_base`.
            current_base_ = frame.saved_base;
        }
        return;  // value_ is the sequence's result
    }
    const Value next = frame.forms.pair->car;
    const Value rest = frame.forms.pair->cdr;

    if (rest.type == ValueType::Nil) {
        // Final form: evaluate it in tail position.
        expr_ = next;
        env_ = frame.env;
        mode_ = Mode::Eval;
        return;
    }

    frames_.push_back(SequenceFrame{rest, frame.env});
    note_depth();
    expr_ = next;
    env_ = frame.env;
    mode_ = Mode::Eval;
}

void Machine::handle(IfFrame& frame) {
    const bool taken = is_truthy(value_);
    if (taken ? !frame.has_consequent : !frame.has_alternative) {
        value_ = unspecified_value();
        return;
    }
    // The chosen branch is the value of the whole `if`, so it is in tail
    // position and no frame is pushed.
    expr_ = taken ? frame.consequent : frame.alternative;
    env_ = frame.env;
    mode_ = Mode::Eval;
}

void Machine::handle(WhenFrame& frame) {
    if (is_truthy(value_) == frame.expect_truthy) {
        begin_sequence(frame.forms, frame.env);
        return;
    }
    value_ = unspecified_value();
}

void Machine::handle(AndFrame& frame) {
    if (value_.type == ValueType::Boolean && !value_.boolean) {
        return;  // short circuit: #f is the result
    }
    if (frame.forms.type != ValueType::Pair) {
        return;  // value_ is the last form's result
    }
    Env* const frame_env = frame.env;
    const Value next = frame.forms.pair->car;
    frame.forms = frame.forms.pair->cdr;
    frames_.push_back(std::move(frame));
    note_depth();
    expr_ = next;
    env_ = frame_env;
    mode_ = Mode::Eval;
}

void Machine::handle(OrFrame& frame) {
    if (is_truthy(value_)) {
        return;  // first truthy value is the result
    }
    if (frame.forms.type != ValueType::Pair) {
        value_ = boolean_value(false);
        return;
    }
    Env* const frame_env = frame.env;
    const Value next = frame.forms.pair->car;
    frame.forms = frame.forms.pair->cdr;
    frames_.push_back(std::move(frame));
    note_depth();
    expr_ = next;
    env_ = frame_env;
    mode_ = Mode::Eval;
}

void Machine::handle(CondFrame& frame) {
    if (!is_truthy(value_)) {
        cond_advance(frame.clauses, frame.env);
        return;
    }

    const Value operands = frame.clause.pair->cdr;
    if (operands.type == ValueType::Nil) {
        return;  // `(cond (test))` yields the test's own value
    }
    if (operands.type == ValueType::Pair &&
        is_symbol_named(operands.pair->car, "=>") &&
        operands.pair->cdr.type == ValueType::Pair) {
        frames_.push_back(CondArrowFrame{value_, frame.env});
        note_depth();
        expr_ = operands.pair->cdr.pair->car;
        env_ = frame.env;
        mode_ = Mode::Eval;
        return;
    }
    begin_sequence(operands, frame.env);
}

void Machine::handle(CondArrowFrame& frame) {
    std::vector<Value> args;
    args.push_back(frame.argument);
    start_apply(value_, std::move(args));
}

void Machine::handle(CaseFrame& frame) {
    case_advance(frame.clauses, value_, frame.env);
}

void Machine::handle(SetFrame& frame) {
    // `set!` defines the variable when it is unbound, matching `_sf_set`.
    if (!assign(frame.env, frame.name, value_)) {
        define_in(frame.env, frame.name, value_);
    }
    value_ = unspecified_value();
}

void Machine::handle(DefineFrame& frame) {
    if (value_.type == ValueType::Procedure &&
        value_.procedure->name == "lambda") {
        value_.procedure->name = frame.name->name;
    }
    define_in(frame.env, frame.name, value_);
    value_ = symbol_value(frame.name);
}

void Machine::handle(LetFrame& frame) {
    frame.values.push_back(value_);
    const Symbol* const just_bound = frame.names[frame.values.size() - 1];

    if (frame.kind == LetFrame::Kind::Star) {
        // `let*` extends the local environment as it goes, so later
        // initialisers see earlier bindings.
        define_in(frame.local, const_cast<Symbol*>(just_bound), value_);
    } else if (frame.kind == LetFrame::Kind::Rec) {
        // `letrec` pre-bound every name, so this is a rebind.
        frame.local->bindings.insert_or_assign(const_cast<Symbol*>(just_bound),
                                               value_);
    }
    let_advance(std::move(frame));
}

void Machine::handle(DoFrame& frame) {
    Env* const local = frame.local;

    switch (frame.phase) {
        case DoFrame::Phase::AwaitInit:
            define_in(local, frame.names[frame.spec_index - 1], value_);
            frame.phase = DoFrame::Phase::Init;
            do_advance(std::move(frame));
            return;

        case DoFrame::Phase::AwaitStep:
            local->bindings.insert_or_assign(frame.names[frame.spec_index - 1],
                                             value_);
            frame.phase = DoFrame::Phase::Steps;
            do_advance(std::move(frame));
            return;

        case DoFrame::Phase::AwaitBody:
            // The body's value is discarded; move on to the step expressions.
            frame.phase = DoFrame::Phase::Steps;
            frame.cursor = frame.specs;
            frame.spec_index = 0;
            do_advance(std::move(frame));
            return;

        case DoFrame::Phase::AwaitTest:
            if (is_truthy(value_)) {
                if (frame.test.pair->cdr.type == ValueType::Pair) {
                    begin_sequence(frame.test.pair->cdr, local);
                    return;
                }
                value_ = unspecified_value();
                return;
            }
            if (frame.body.type != ValueType::Pair) {
                frame.phase = DoFrame::Phase::Steps;
                frame.cursor = frame.specs;
                frame.spec_index = 0;
                do_advance(std::move(frame));
                return;
            }
            {
                const Value body = frame.body;
                frame.phase = DoFrame::Phase::AwaitBody;
                frames_.push_back(std::move(frame));
                note_depth();
                begin_sequence(body, local);
            }
            return;

        case DoFrame::Phase::Init:
        case DoFrame::Phase::Steps:
            return;
    }
}

void Machine::handle(QuasiquoteFrame&) {
    // The topmost session is the one whose `unquote` produced this value.
    if (qq_sessions_.empty()) {
        throw SchemeError{"internal error: quasiquote session lost"};
    }
    if (quasiquote_publish(value_)) {
        return;  // an evaluation was requested, or the session finished
    }
    quasiquote_step();
}

void Machine::handle(MapFrame& frame) {
    if (frame.collect) {
        frame.acc.push_back(value_);
    }
    ++frame.index;
    if (frame.index >= frame.count) {
        value_ = frame.collect ? list_from(arena_, frame.acc) : unspecified_value();
        return;
    }

    const Value proc = frame.proc;
    std::vector<Value> row;
    row.reserve(frame.columns.size());
    for (const std::vector<Value>& column : frame.columns) {
        row.push_back(column[frame.index]);
    }
    frames_.push_back(std::move(frame));
    note_depth();
    start_apply(proc, std::move(row));
}

void Machine::handle(FilterFrame& frame) {
    if (is_truthy(value_)) {
        frame.acc.push_back(frame.source[frame.index]);
    }
    ++frame.index;
    if (frame.index >= frame.source.size()) {
        value_ = list_from(arena_, frame.acc);
        return;
    }

    const Value proc = frame.proc;
    std::vector<Value> args;
    args.push_back(frame.source[frame.index]);
    frames_.push_back(std::move(frame));
    note_depth();
    start_apply(proc, std::move(args));
}

void Machine::handle(FoldFrame& frame) {
    frame.acc = value_;
    if (frame.index >= frame.source.size()) {
        value_ = frame.acc;
        return;
    }

    const Value proc = frame.proc;
    std::vector<Value> args;
    args.push_back(frame.acc);
    args.push_back(frame.source[frame.index]);
    ++frame.index;
    frames_.push_back(std::move(frame));
    note_depth();
    start_apply(proc, std::move(args));
}

// ---------------------------------------------------------------------------
// Special forms
// ---------------------------------------------------------------------------

void Machine::form_quote(Value operands) {
    // The reference's `_quote()` rebuilt Python lists into pair chains.  Here
    // the reader already produced pairs, so quoting is the identity -- and the
    // vector/code conflation that broke `case` cannot arise.
    value_ = operands.type == ValueType::Pair ? operands.pair->car : nil_value();
    mode_ = Mode::Deliver;
}

void Machine::form_quasiquote(Value operands, Env* env) {
    if (operands.type != ValueType::Pair) {
        value_ = nil_value();
        mode_ = Mode::Deliver;
        return;
    }
    quasiquote_start(operands.pair->car, env, 1);
}

void Machine::form_if(Value operands, Env* env) {
    if (operands.type != ValueType::Pair) {
        throw SchemeError{"if: expected a test expression"};
    }
    IfFrame frame;
    frame.env = env;

    const Value rest = operands.pair->cdr;
    if (rest.type == ValueType::Pair) {
        frame.consequent = rest.pair->car;
        frame.has_consequent = true;
        if (rest.pair->cdr.type == ValueType::Pair) {
            frame.alternative = rest.pair->cdr.pair->car;
            frame.has_alternative = true;
        }
    }

    frames_.push_back(frame);
    note_depth();
    expr_ = operands.pair->car;
    env_ = env;
    mode_ = Mode::Eval;
}

void Machine::form_define(Value operands, Env* env) {
    if (operands.type != ValueType::Pair) {
        throw SchemeError{"define: missing target"};
    }
    const Value target = operands.pair->car;
    const Value rest = operands.pair->cdr;

    if (target.type == ValueType::Pair) {
        // `(define (f a b) body ...)` sugar.
        const Value head = target.pair->car;
        if (head.type != ValueType::Symbol) {
            throw SchemeError{"define: expected a name"};
        }
        std::vector<Symbol*> params;
        Symbol* rest_param = nullptr;
        parse_params(target.pair->cdr, params, rest_param);
        Procedure* procedure =
            arena_.make_procedure(params, rest_param, rest, env,
                                  PrimitiveId::None, head.symbol->name);
        define_in(env, head.symbol, procedure_value(procedure));
        value_ = symbol_value(head.symbol);
        mode_ = Mode::Deliver;
        return;
    }

    if (target.type != ValueType::Symbol) {
        throw SchemeError{"define: expected a name"};
    }

    if (rest.type != ValueType::Pair) {
        define_in(env, target.symbol, unspecified_value());
        value_ = symbol_value(target.symbol);
        mode_ = Mode::Deliver;
        return;
    }

    frames_.push_back(DefineFrame{target.symbol, env});
    note_depth();
    expr_ = rest.pair->car;
    env_ = env;
    mode_ = Mode::Eval;
}

void Machine::form_lambda(Value operands, Env* env) {
    if (operands.type != ValueType::Pair) {
        throw SchemeError{"lambda: missing parameter list"};
    }
    std::vector<Symbol*> params;
    Symbol* rest = nullptr;
    parse_params(operands.pair->car, params, rest);
    Procedure* procedure =
        arena_.make_procedure(params, rest, operands.pair->cdr, env,
                              PrimitiveId::None, "lambda");
    value_ = procedure_value(procedure);
    mode_ = Mode::Deliver;
}

void Machine::form_set(Value operands, Env* env) {
    if (operands.type != ValueType::Pair) {
        throw SchemeError{"set!: missing variable"};
    }
    const Value target = operands.pair->car;
    if (target.type != ValueType::Symbol) {
        throw SchemeError{"set!: expected a variable"};
    }
    if (operands.pair->cdr.type != ValueType::Pair) {
        throw SchemeError{"set!: missing value"};
    }
    frames_.push_back(SetFrame{target.symbol, env});
    note_depth();
    expr_ = operands.pair->cdr.pair->car;
    env_ = env;
    mode_ = Mode::Eval;
}

void Machine::form_when(Value operands, Env* env, bool expect_truthy) {
    if (operands.type != ValueType::Pair) {
        throw SchemeError{"when: expected a test expression"};
    }
    frames_.push_back(WhenFrame{operands.pair->cdr, env, expect_truthy});
    note_depth();
    expr_ = operands.pair->car;
    env_ = env;
    mode_ = Mode::Eval;
}

void Machine::form_and(Value operands, Env* env) {
    if (operands.type != ValueType::Pair) {
        value_ = boolean_value(true);
        mode_ = Mode::Deliver;
        return;
    }
    frames_.push_back(
        AndFrame{operands.pair->cdr, unspecified_value(), env});
    note_depth();
    expr_ = operands.pair->car;
    env_ = env;
    mode_ = Mode::Eval;
}

void Machine::form_or(Value operands, Env* env) {
    if (operands.type != ValueType::Pair) {
        value_ = boolean_value(false);
        mode_ = Mode::Deliver;
        return;
    }
    frames_.push_back(OrFrame{operands.pair->cdr, env});
    note_depth();
    expr_ = operands.pair->car;
    env_ = env;
    mode_ = Mode::Eval;
}

void Machine::cond_advance(Value clauses, Env* env) {
    while (clauses.type == ValueType::Pair) {
        const Value clause = clauses.pair->car;
        clauses = clauses.pair->cdr;

        // The reference skips anything that is not a non-empty list.
        if (clause.type != ValueType::Pair) {
            continue;
        }
        const Value test = clause.pair->car;
        if (is_symbol_named(test, "else")) {
            begin_sequence(clause.pair->cdr, env);
            return;
        }
        frames_.push_back(CondFrame{clause, clauses, env});
        note_depth();
        expr_ = test;
        env_ = env;
        mode_ = Mode::Eval;
        return;
    }
    value_ = unspecified_value();
    mode_ = Mode::Deliver;
}

void Machine::form_case(Value operands, Env* env) {
    if (operands.type != ValueType::Pair) {
        throw SchemeError{"case: expected a key expression"};
    }
    frames_.push_back(CaseFrame{operands.pair->cdr, env});
    note_depth();
    expr_ = operands.pair->car;
    env_ = env;
    mode_ = Mode::Eval;
}

void Machine::case_advance(Value clauses, Value key, Env* env) {
    while (clauses.type == ValueType::Pair) {
        const Value clause = clauses.pair->car;
        clauses = clauses.pair->cdr;

        if (clause.type != ValueType::Pair) {
            continue;
        }
        const Value datums = clause.pair->car;
        if (is_symbol_named(datums, "else")) {
            begin_sequence(clause.pair->cdr, env);
            return;
        }

        // In the reference this line was `_to_py_list(clause[0])`, applied to
        // *parsed code*, which is a Python list rather than a pair chain.  It
        // therefore always raised "expected a proper list", so `case` could
        // never run.  Because this port represents code as pairs throughout,
        // the same call now does the intended thing.
        const std::vector<Value> candidates = to_vector(datums);
        for (const Value& candidate : candidates) {
            if (equal_values(key, candidate)) {
                begin_sequence(clause.pair->cdr, env);
                return;
            }
        }
    }
    value_ = unspecified_value();
    mode_ = Mode::Deliver;
}

void Machine::form_let(Value operands, Env* env, LetFrame::Kind kind) {
    if (operands.type != ValueType::Pair) {
        throw SchemeError{"let: missing bindings"};
    }

    LetFrame frame;
    frame.kind = kind;
    frame.outer = env;

    Value bindings = operands.pair->car;
    if (kind == LetFrame::Kind::Plain && bindings.type == ValueType::Symbol) {
        // Named `let`: (let name ((n e) ...) body ...)
        frame.name = bindings.symbol;
        const Value rest = operands.pair->cdr;
        if (rest.type != ValueType::Pair) {
            throw SchemeError{"let: missing bindings"};
        }
        bindings = rest.pair->car;
        frame.body = rest.pair->cdr;
    } else {
        frame.body = operands.pair->cdr;
    }
    frame.specs = bindings;

    // Collect every name before evaluating any initialiser, so `letrec` can
    // pre-bind them all.
    for (Value cursor = bindings; cursor.type == ValueType::Pair;
         cursor = cursor.pair->cdr) {
        const Value spec = cursor.pair->car;
        if (spec.type != ValueType::Pair ||
            spec.pair->car.type != ValueType::Symbol) {
            throw SchemeError{"let: malformed binding specification"};
        }
        frame.names.push_back(spec.pair->car.symbol);
    }

    if (kind != LetFrame::Kind::Plain) {
        // `let*` and `letrec` evaluate initialisers in the new frame.
        frame.local = arena_.make_env(env);
    }
    if (kind == LetFrame::Kind::Rec) {
        for (Symbol* const name : frame.names) {
            define_in(frame.local, name, unspecified_value());
        }
    }

    let_advance(std::move(frame));
}

void Machine::let_advance(LetFrame frame) {
    if (frame.specs.type == ValueType::Pair) {
        const Value spec = frame.specs.pair->car;
        frame.specs = frame.specs.pair->cdr;

        const Value initialiser = spec.pair->cdr;
        if (initialiser.type != ValueType::Pair) {
            throw SchemeError{"let: binding has no initialiser"};
        }

        // A named `let` evaluates its initialisers in the *outer* environment,
        // like a plain `let`; only `let*`/`letrec` use the new frame.
        Env* const eval_env =
            (frame.kind == LetFrame::Kind::Plain || frame.name != nullptr)
                ? frame.outer
                : frame.local;

        const Value expression = initialiser.pair->car;
        frames_.push_back(std::move(frame));
        note_depth();
        expr_ = expression;
        env_ = eval_env;
        mode_ = Mode::Eval;
        return;
    }

    if (frame.name != nullptr) {
        // Named `let` is sugar for a `letrec`-style self-reference, except the
        // local frame is created after the initialisers are evaluated.
        Env* const local = arena_.make_env(frame.outer);
        Procedure* procedure = arena_.make_procedure(
            frame.names, nullptr, frame.body, local, PrimitiveId::None,
            frame.name->name);
        define_in(local, frame.name, procedure_value(procedure));
        start_apply(procedure_value(procedure), std::move(frame.values));
        return;
    }

    if (frame.kind == LetFrame::Kind::Plain) {
        Env* const local = arena_.make_env(frame.outer);
        for (std::size_t index = 0; index < frame.names.size(); ++index) {
            define_in(local, frame.names[index], frame.values[index]);
        }
        begin_sequence(frame.body, local);
        return;
    }

    begin_sequence(frame.body, frame.local);
}

void Machine::form_do(Value operands, Env* env) {
    if (operands.type != ValueType::Pair) {
        throw SchemeError{"do: missing variable specifications"};
    }
    const Value specs = operands.pair->car;
    const Value rest = operands.pair->cdr;
    if (rest.type != ValueType::Pair) {
        throw SchemeError{"do: missing test clause"};
    }

    DoFrame frame;
    frame.specs = specs;
    frame.cursor = specs;
    frame.test = rest.pair->car;
    frame.body = rest.pair->cdr;
    frame.local = arena_.make_env(env);

    for (Value cursor = specs; cursor.type == ValueType::Pair;
         cursor = cursor.pair->cdr) {
        const Value spec = cursor.pair->car;
        if (spec.type != ValueType::Pair ||
            spec.pair->car.type != ValueType::Symbol) {
            throw SchemeError{"do: malformed variable specification"};
        }
        frame.names.push_back(spec.pair->car.symbol);
    }

    do_advance(std::move(frame));
}

void Machine::do_advance(DoFrame frame) {
    Env* const local = frame.local;

    for (;;) {
        if (frame.phase == DoFrame::Phase::Init) {
            if (frame.cursor.type != ValueType::Pair) {
                frame.phase = DoFrame::Phase::AwaitTest;
                continue;
            }
            const Value spec = frame.cursor.pair->car;
            frame.cursor = frame.cursor.pair->cdr;
            ++frame.spec_index;

            const Value initialiser = spec.pair->cdr;
            if (initialiser.type != ValueType::Pair) {
                // `(do ((i)) ...)` -- no initialiser, so the reference binds the
                // empty list.
                define_in(local, frame.names[frame.spec_index - 1], nil_value());
                continue;
            }

            const Value expression = initialiser.pair->car;
            frame.phase = DoFrame::Phase::AwaitInit;
            frames_.push_back(std::move(frame));
            note_depth();
            expr_ = expression;
            env_ = local;
            mode_ = Mode::Eval;
            return;
        }

        if (frame.phase == DoFrame::Phase::Steps) {
            if (frame.cursor.type != ValueType::Pair) {
                frame.phase = DoFrame::Phase::AwaitTest;
                continue;
            }
            const Value spec = frame.cursor.pair->car;
            frame.cursor = frame.cursor.pair->cdr;
            ++frame.spec_index;

            // `len(spec) > 2` in the reference: the third element is the step.
            const Value after_init = spec.pair->cdr;
            if (after_init.type != ValueType::Pair) {
                continue;
            }
            const Value step = after_init.pair->cdr;
            if (step.type != ValueType::Pair) {
                continue;
            }

            const Value expression = step.pair->car;
            frame.phase = DoFrame::Phase::AwaitStep;
            frames_.push_back(std::move(frame));
            note_depth();
            expr_ = expression;
            env_ = local;
            mode_ = Mode::Eval;
            return;
        }

        if (frame.phase == DoFrame::Phase::AwaitTest) {
            const Value expression = frame.test.pair->car;
            frames_.push_back(std::move(frame));
            note_depth();
            expr_ = expression;
            env_ = local;
            mode_ = Mode::Eval;
            return;
        }

        return;  // AwaitInit / AwaitStep / AwaitBody are driven by handle()
    }
}

// ---------------------------------------------------------------------------
// Quasiquote
// ---------------------------------------------------------------------------

void Machine::quasiquote_start(Value value, Env* env, int depth) {
    QqSession session;
    session.env = env;
    session.pending = QqPending::None;

    QqJob job;
    job.kind = QqJob::Kind::Enter;
    job.value = value;
    job.depth = depth;
    session.jobs.push_back(std::move(job));

    qq_sessions_.push_back(std::move(session));
    quasiquote_step();
}

bool Machine::quasiquote_publish(Value result) {
    for (;;) {
        QqSession& session = qq_sessions_.back();

        if (session.pending == QqPending::Splice) {
            session.pending = QqPending::None;
            to_vector(result, qq_splice_scratch_);
            std::vector<Value>& acc = session.accs.back();
            acc.insert(acc.end(), qq_splice_scratch_.begin(),
                       qq_splice_scratch_.end());
            return false;  // accumulated; the caller keeps stepping
        }
        session.pending = QqPending::None;

        if (session.jobs.empty()) {
            // This template is complete.  Popping the session and delivering
            // the value is also the correct behaviour for a *nested* template:
            // the value becomes the result of the `(quasiquote ...)` form, which
            // is precisely what the enclosing `unquote` was waiting for.
            qq_sessions_.pop_back();
            value_ = result;
            mode_ = Mode::Deliver;
            return true;
        }
        if (session.jobs.back().kind == QqJob::Kind::WrapQuasiquote) {
            session.jobs.pop_back();
            Value parts[2];
            parts[0] = symbol_value(arena_.intern("quasiquote"));
            parts[1] = result;
            result = pair_value(arena_.list(parts, nil_value()));
            continue;
        }
        session.accs.back().push_back(result);
        return false;  // accumulated; the caller keeps stepping
    }
}

void Machine::quasiquote_step() {
    for (;;) {
        if (qq_sessions_.empty()) {
            return;  // the session completed and delivered its result
        }

        QqJob job = std::move(qq_sessions_.back().jobs.back());
        qq_sessions_.back().jobs.pop_back();
        Env* const session_env = qq_sessions_.back().env;

        if (job.kind == QqJob::Kind::WrapQuasiquote) {
            // Always consumed by `quasiquote_publish` before the driver gets
            // back here; skipping is a defensive no-op.
            continue;
        }

        if (job.kind == QqJob::Kind::Finish) {
            std::vector<Value> acc = std::move(qq_sessions_.back().accs.back());
            qq_sessions_.back().accs.pop_back();
            if (quasiquote_publish(list_from(arena_, acc))) {
                return;
            }
            continue;
        }

        if (job.kind == QqJob::Kind::Splice) {
            qq_sessions_.back().pending = QqPending::Splice;
            // The frame is what brings the driver back here: without it the
            // spliced list's value would be delivered to the caller instead.
            frames_.push_back(QuasiquoteFrame{});
            note_depth();
            expr_ = job.value;
            env_ = session_env;
            mode_ = Mode::Eval;
            return;
        }

        // Enter
        const Value value = job.value;
        if (value.type != ValueType::Pair) {
            if (quasiquote_publish(value)) {
                return;
            }
            continue;
        }

        const Value head = value.pair->car;
        if (head.type == ValueType::Symbol) {
            if (head.symbol->name == "unquote" && job.depth == 1) {
                qq_sessions_.back().pending = QqPending::Replace;
                frames_.push_back(QuasiquoteFrame{});
                note_depth();
                expr_ = value.pair->cdr.type == ValueType::Pair
                            ? value.pair->cdr.pair->car
                            : nil_value();
                env_ = session_env;
                mode_ = Mode::Eval;
                return;
            }
            if (head.symbol->name == "quasiquote") {
                QqJob wrap;
                wrap.kind = QqJob::Kind::WrapQuasiquote;
                QqJob inner;
                inner.kind = QqJob::Kind::Enter;
                inner.value = value.pair->cdr.type == ValueType::Pair
                                  ? value.pair->cdr.pair->car
                                  : nil_value();
                inner.depth = job.depth + 1;
                qq_sessions_.back().jobs.push_back(std::move(wrap));
                qq_sessions_.back().jobs.push_back(std::move(inner));
                continue;
            }
        }

        // A plain list template: build it item by item.
        QqJob finish;
        finish.kind = QqJob::Kind::Finish;
        qq_sessions_.back().jobs.push_back(std::move(finish));
        qq_sessions_.back().accs.emplace_back();

        std::vector<QqJob> items;
        for (Value cursor = value; cursor.type == ValueType::Pair;
             cursor = cursor.pair->cdr) {
            const Value item = cursor.pair->car;
            QqJob queued;
            if (job.depth == 1 && item.type == ValueType::Pair &&
                is_symbol_named(item.pair->car, "unquote-splicing")) {
                queued.kind = QqJob::Kind::Splice;
                queued.value = item.pair->cdr.type == ValueType::Pair
                                   ? item.pair->cdr.pair->car
                                   : nil_value();
            } else {
                queued.kind = QqJob::Kind::Enter;
                queued.value = item;
                queued.depth = job.depth;
            }
            items.push_back(std::move(queued));
        }
        for (auto it = items.rbegin(); it != items.rend(); ++it) {
            qq_sessions_.back().jobs.push_back(std::move(*it));
        }
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

Value Machine::require_number(const Value& value, std::string_view who) const {
    if (value.type == ValueType::Boolean || !is_number(value)) {
        throw SchemeError{std::string{who} + ": expected a number, got " +
                          std::string{type_name(value)}};
    }
    return value;
}

double Machine::as_real(const Value& value) const {
    return value.type == ValueType::Integer
               ? static_cast<double>(value.integer)
               : value.real;
}

Value Machine::number_from_real(double value) const {
    return real_value(value);
}

Value Machine::number_from_integer(std::int64_t value) const {
    return integer_value(value);
}

}  // namespace timewarp::scheme
