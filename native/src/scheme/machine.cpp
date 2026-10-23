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
