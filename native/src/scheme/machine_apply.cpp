// SPDX-License-Identifier: MIT
//
// Sequences and application: how a body's forms are run in order, and how a call
// pushes a frame instead of a C++ stack frame.

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

}  // namespace timewarp::scheme
