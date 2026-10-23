// SPDX-License-Identifier: MIT
//
// The special forms and quasiquote: one handler per form the reader can produce, and
// the `parse_params` they share.

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

}  // namespace timewarp::scheme
