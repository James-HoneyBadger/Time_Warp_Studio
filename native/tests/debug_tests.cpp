// SPDX-License-Identifier: MIT
//
// Tests for the debugging surface: source maps, resumable execution, the
// suspension controller, and the call-stack / variable inspection API.
//
// None of this touches Qt.  The debugging machinery lives in the evaluator and
// the Qt runner is a thin client of it, so the interesting logic is testable
// without an event loop.

#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <functional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "check.hpp"
#include "timewarp/scheme/host.hpp"
#include "timewarp/scheme/machine.hpp"
#include "timewarp/scheme/reader.hpp"
#include "timewarp/scheme/source_map.hpp"

namespace {

using timewarp::scheme::Arena;
using timewarp::scheme::CallFrame;
using timewarp::scheme::LineSpan;
using timewarp::scheme::Machine;
using timewarp::scheme::NullHost;
using timewarp::scheme::SourceMap;
using timewarp::scheme::SourceToken;
using timewarp::scheme::SuspendReason;
using timewarp::scheme::SuspendState;
using timewarp::scheme::SuspensionController;
using timewarp::scheme::Value;
using timewarp::scheme::VariableEntry;

constexpr std::uint64_t kUnlimited = ~std::uint64_t{0};
constexpr std::uint64_t kNever = ~std::uint64_t{0};

/// `(fact 5)` = 120, with the recursive call on line 4.
constexpr const char* kFact =
    "(define (fact n)\n"
    "  (if (= n 0)\n"
    "      1\n"
    "      (* n (fact (- n 1)))))\n"
    "(display (fact 5))\n";

void announce(const char* name) {
    std::printf("-- %s\n", name);
    std::fflush(stdout);
}

// ---------------------------------------------------------------------------
// A controller whose predicate means "suspend here"
// ---------------------------------------------------------------------------

/// Declines when the predicate matches, but at most once per step count.
///
/// That ceiling is not a test convenience, it is what a real debugger does:
/// resuming from a suspension leaves the form it was stopped at alone, so a
/// breakpoint cannot re-arm on the very instruction the user just continued
/// past.  Without it a predicate like "line 4" would suspend, resume, re-test
/// line 4, and suspend again -- forever.
class ProbeController final : public SuspensionController {
public:
    using Predicate = std::function<bool(const SuspendState&)>;

    explicit ProbeController(Predicate predicate)
        : predicate_(std::move(predicate)) {}

    /// Every form the machine reported, including re-tests.
    std::vector<SuspendState> all;
    /// The decisions to suspend, in order.
    std::vector<SuspendState> suspended;

    bool on_form(const SuspendState& state) override {
        all.push_back(state);
        if (state.steps == declined_at_steps_) {
            return true;  // already paused for this step; let it through
        }
        if (!predicate_(state)) {
            return true;
        }
        declined_at_steps_ = state.steps;
        suspended.push_back(state);
        return false;
    }

private:
    Predicate predicate_;
    std::uint64_t declined_at_steps_ = kNever;
};

/// Suspends everywhere the probe allows.
[[nodiscard]] ProbeController::Predicate always() {
    return [](const SuspendState&) { return true; };
}

/// Never suspends: the debugger is attached but idle.
[[nodiscard]] ProbeController::Predicate never() {
    return [](const SuspendState&) { return false; };
}

struct RunResult {
    std::string output;
    std::uint64_t steps = 0;
    std::size_t pauses = 0;
    std::size_t retests = 0;
    bool finished = false;
};

/// Runs to completion with no controller attached -- the ordinary path.
RunResult plain_run(std::string_view source) {
    Arena arena;
    NullHost host;
    Machine machine{arena, host};
    RunResult result;
    result.output = machine.run(source);
    result.steps = machine.steps();
    result.finished = true;
    return result;
}

/// Runs to completion with `probe` attached, in quanta of `quantum` steps.
///
/// A source map is always attached, because a breakpoint on a line is the thing
/// most of these tests are standing in for -- and without a map nothing reports
/// a position at all.
RunResult debug_run(std::string_view source, ProbeController& probe,
                    std::uint64_t quantum) {
    Arena arena;
    NullHost host;
    SourceMap map;
    Machine machine{arena, host};
    machine.set_suspension_controller(&probe);
    machine.load(source, &map);

    RunResult result;
    for (int guard = 0; guard < 2000000; ++guard) {
        const SuspendReason reason = machine.step_quantum(quantum);
        if (reason == SuspendReason::Finished) {
            break;
        }
        if (reason == SuspendReason::Paused) {
            ++result.pauses;
        }
    }
    result.output = machine.output();
    result.steps = machine.steps();
    result.retests = probe.all.size() - probe.suspended.size();
    result.finished = machine.finished();
    return result;
}

template <class Entry>
[[nodiscard]] bool has_entry(const std::vector<Entry>& entries,
                             const std::string& name,
                             const std::string& rendered) {
    for (const Entry& entry : entries) {
        if (entry.name == name && entry.rendered == rendered) {
            return true;
        }
    }
    return false;
}

template <class Entry>
[[nodiscard]] bool has_name(const std::vector<Entry>& entries,
                            const std::string& name) {
    for (const Entry& entry : entries) {
        if (entry.name == name) {
            return true;
        }
    }
    return false;
}

template <class Entry>
[[nodiscard]] bool has_kind(const std::vector<Entry>& entries,
                            const std::string& name, const std::string& kind) {
    for (const Entry& entry : entries) {
        if (entry.name == name && entry.kind == kind) {
            return true;
        }
    }
    return false;
}

// ---------------------------------------------------------------------------
// Source maps
// ---------------------------------------------------------------------------

void test_tokenizer_reports_start_lines() {
    announce("tokenize_with_lines");
    // A comment on line 1, a form split over two lines, a string spanning two
    // more, and a final form.  The line each token *starts* on is the contract.
    const std::string source =
        "; a comment\n"
        "(define x\n"
        "   1)\n"
        "(define s \"two\nlines\")\n"
        "(display s)\n";
    const std::vector<SourceToken> tokens =
        timewarp::scheme::tokenize_with_lines(source);

    TW_CHECK_EQ(tokens.size(), std::size_t{14});
    const auto line_of = [&tokens](std::size_t index) {
        return static_cast<int>(tokens[index].line);
    };

    TW_CHECK_EQ(line_of(0), 2);  // the comment produces no token at all
    TW_CHECK_EQ(tokens[0].text, std::string{"("});
    TW_CHECK_EQ(tokens[1].text, std::string{"define"});
    TW_CHECK_EQ(line_of(3), 3);  // `1` sits on the continuation line
    TW_CHECK_EQ(line_of(4), 3);  // ...and so does the closing paren
    TW_CHECK_EQ(line_of(5), 4);
    TW_CHECK_EQ(tokens[7].text, std::string{"s"});
    TW_CHECK_EQ(line_of(7), 4);

    // The literal spans lines 4-5, so it is attributed to the line it opens on
    // while the counter moves past it -- which is why the `)` that closes the
    // form lands on 5, not 6.
    TW_CHECK_EQ(tokens[8].text, std::string{"\"two\nlines\""});
    TW_CHECK_EQ(line_of(8), 4);
    TW_CHECK_EQ(line_of(9), 5);
    TW_CHECK_EQ(line_of(10), 6);
    TW_CHECK_EQ(tokens[13].text, std::string{")"});
    TW_CHECK_EQ(line_of(13), 6);
}

void test_source_map_records_form_spans() {
    announce("SourceMap spans");
    const std::string source =
        "(define (f x)\n"
        "  (* x 2))\n"
        "(display (f 21))\n";

    Arena arena;
    SourceMap map;
    const std::vector<Value> forms =
        timewarp::scheme::parse_program(arena, source, &map);

    TW_CHECK_EQ(forms.size(), std::size_t{2});

    const LineSpan outer = map.lookup(forms[0]);
    TW_CHECK(outer.known());
    TW_CHECK_EQ(static_cast<int>(outer.first_line), 1);
    TW_CHECK_EQ(static_cast<int>(outer.last_line), 2);

    const LineSpan second = map.lookup(forms[1]);
    TW_CHECK_EQ(static_cast<int>(second.first_line), 3);
    TW_CHECK_EQ(static_cast<int>(second.last_line), 3);

    // forms[0] = (define (f x) (* x 2)); the body lives in the cdr's cdr's car.
    const Value body = forms[0].pair->cdr.pair->cdr.pair->car;
    const LineSpan inner = map.lookup(body);
    TW_CHECK_EQ(static_cast<int>(inner.first_line), 2);
    TW_CHECK_EQ(static_cast<int>(inner.last_line), 2);

    // Only compound forms can carry a span: a bare atom has no Pair to key on.
    TW_CHECK(map.size() >= 3);
}

void test_source_map_is_optional() {
    announce("SourceMap optional");
    Arena arena;
    SourceMap map;
    const std::string source = "(display 1)\n";

    (void)timewarp::scheme::parse_program(arena, source);
    TW_CHECK_EQ(map.size(), std::size_t{0});

    // A machine loaded without a map must still run; it simply cannot report
    // positions, which is what the debugger's "no source" path relies on.
    NullHost host;
    Machine machine{arena, host};
    ProbeController probe{always()};
    machine.set_suspension_controller(&probe);
    machine.load(source);
    TW_CHECK_EQ(machine.step_quantum(kUnlimited), SuspendReason::Paused);
    TW_CHECK_EQ(machine.suspend_state().span.known(), false);
}

void test_line_span_contains_ignores_unknown() {
    announce("LineSpan::contains");
    const LineSpan span{3, 5};
    TW_CHECK(span.contains(3));
    TW_CHECK(span.contains(4));
    TW_CHECK(span.contains(5));
    TW_CHECK(!span.contains(2));
    TW_CHECK(!span.contains(6));

    // An unknown span must match nothing: otherwise a breakpoint on line 1
    // would fire on every run-time-constructed form.
    const LineSpan unknown;
    TW_CHECK(!unknown.known());
    TW_CHECK(!unknown.contains(0));
    TW_CHECK(!unknown.contains(1));
}

// ---------------------------------------------------------------------------
// Resumability
// ---------------------------------------------------------------------------

void test_stepping_converges_on_the_same_result() {
    announce("step_quantum resumable");
    const RunResult plain = plain_run(kFact);
    TW_CHECK_EQ(plain.output, std::string{"120"});
    TW_CHECK(plain.steps > 0);

    // (a) Fixed-size quanta, controller attached but idle.
    for (const std::uint64_t quantum : {std::uint64_t{1}, std::uint64_t{7},
                                        std::uint64_t{1000}}) {
        ProbeController probe{never()};
        const RunResult stepped = debug_run(kFact, probe, quantum);
        TW_CHECK_EQ(stepped.output, plain.output);
        TW_CHECK_EQ(stepped.steps, plain.steps);
        TW_CHECK_EQ(stepped.pauses, std::size_t{0});
        TW_CHECK(stepped.finished);
    }

    // (b) One step per quantum *and* a suspension at every opportunity: the
    // worst case for a resumable driver, and it must still land on the same
    // answer with the same step count.
    {
        ProbeController probe{always()};
        const RunResult churned = debug_run(kFact, probe, 1);
        TW_CHECK_EQ(churned.output, plain.output);
        TW_CHECK_EQ(churned.steps, plain.steps);
        // Only *compound* forms are reported, so this is the number of
        // compound-form eval steps -- comfortably more than a handful, and far
        // more than enough to expose a driver that double-counts.
        TW_CHECK(churned.pauses > 10);
        TW_CHECK(churned.finished);
    }
}

void test_zero_budget_does_no_work() {
    announce("step_quantum(0)");
    Arena arena;
    NullHost host;
    Machine machine{arena, host};
    machine.load(kFact);

    TW_CHECK(!machine.finished());
    // A zero budget must yield immediately rather than perform work anyway.
    TW_CHECK_EQ(machine.step_quantum(0), SuspendReason::QuantumExhausted);
    TW_CHECK_EQ(machine.steps(), std::uint64_t{0});
    TW_CHECK_EQ(machine.output(), std::string{});

    TW_CHECK_EQ(machine.run_loaded(), SuspendReason::Finished);
    TW_CHECK(machine.finished());
    TW_CHECK_EQ(machine.output(), std::string{"120"});
}

// ---------------------------------------------------------------------------
// Suspension
// ---------------------------------------------------------------------------

void test_suspending_everywhere_changes_nothing() {
    announce("suspension is observation-only");
    const RunResult plain = plain_run(kFact);

    // Suspend at every opportunity.  If suspending and resuming charged the form
    // in flight twice -- or re-entered it rather than re-testing it -- both the
    // output and the step count would drift.
    ProbeController probe{always()};
    const RunResult debugged = debug_run(kFact, probe, kUnlimited);

    TW_CHECK(debugged.pauses > 10);
    TW_CHECK_EQ(debugged.output, plain.output);
    TW_CHECK_EQ(debugged.steps, plain.steps);
    TW_CHECK(debugged.finished);

    // Each suspension is answered by re-testing the *same* form rather than
    // re-entering it, so the controller sees strictly more reports than
    // decisions.  If the form were re-entered instead, the step count would
    // grow and the equality above would fail.
    TW_CHECK(debugged.retests >= debugged.pauses);
    TW_CHECK_EQ(probe.suspended.size(), debugged.pauses);
}

void test_suspension_point_is_a_form_boundary() {
    announce("suspension at line 4");
    // Stand in for a breakpoint: stop wherever the machine reports a form on
    // line 4, which is where the recursive call in `fact` lives.
    ProbeController probe{[](const SuspendState& state) {
        return state.span.first_line == 4;
    }};
    Arena arena;
    NullHost host;
    SourceMap map;
    Machine machine{arena, host};
    machine.set_suspension_controller(&probe);
    machine.load(kFact, &map);

    std::size_t stops = 0;
    for (int guard = 0; guard < 100; ++guard) {
        const SuspendReason reason = machine.step_quantum(kUnlimited);
        if (reason == SuspendReason::Finished) {
            break;
        }
        TW_CHECK_EQ(reason, SuspendReason::Paused);
        ++stops;
    }
    TW_CHECK_EQ(machine.output(), std::string{"120"});

    // Three forms open on line 4 -- `(* n (fact (- n 1)))`, `(fact (- n 1))` and
    // `(- n 1)` -- and the recursion goes five deep, so the machine reports the
    // line fifteen times, three times per level.  The machine's job is to report
    // positions faithfully; deciding that three stops on one line is too many is
    // the runner's job, not the evaluator's.
    TW_CHECK_EQ(stops, std::size_t{15});
    TW_CHECK_EQ(probe.suspended.size(), std::size_t{15});
    for (std::size_t index = 0; index < probe.suspended.size(); ++index) {
        const SuspendState& state = probe.suspended[index];
        TW_CHECK_EQ(static_cast<int>(state.span.first_line), 4);
        TW_CHECK_EQ(static_cast<int>(state.span.last_line), 4);
        TW_CHECK_EQ(static_cast<int>(state.depth),
                    static_cast<int>(index / 3) + 1);
        TW_CHECK_EQ(std::string{state.procedure}, std::string{"fact"});
    }
    // The recursion really does descend: the first stop precedes the last.
    TW_CHECK(probe.suspended.front().steps < probe.suspended.back().steps);
}

void test_suspend_state_reports_the_form_about_to_run() {
    announce("suspend_state span");
    Arena arena;
    NullHost host;
    Machine machine{arena, host};
    SourceMap map;

    bool stopped = false;
    SuspendState captured;
    ProbeController probe{[&stopped, &captured](const SuspendState& state) {
        if (stopped || state.span.first_line != 4) {
            return false;
        }
        stopped = true;
        captured = state;
        return true;
    }};

    machine.set_suspension_controller(&probe);
    machine.load(kFact, &map);

    TW_CHECK_EQ(machine.step_quantum(kUnlimited), SuspendReason::Paused);
    TW_CHECK(!machine.finished());

    // What the machine reports while paused is the form that has not run yet,
    // not the one that just finished -- a debugger shows the line it is stopped
    // *at*, and "continue" has to execute it.
    const SuspendState live = machine.suspend_state();
    TW_CHECK_EQ(static_cast<int>(live.span.first_line), 4);
    TW_CHECK_EQ(static_cast<int>(live.span.first_line),
                static_cast<int>(captured.span.first_line));
    TW_CHECK_EQ(live.steps, captured.steps);

    // And carrying on from exactly there still finishes the program.
    TW_CHECK_EQ(machine.run_loaded(), SuspendReason::Finished);
    TW_CHECK_EQ(machine.output(), std::string{"120"});
}

void test_many_suspensions_do_not_leak_frames() {
    announce("repeated suspension keeps depth");
    // Suspend as often as possible while running one step at a time -- a
    // debugger being single-stepped hard.  The frame stack must not grow per
    // pause.
    ProbeController probe{always()};
    Arena arena;
    NullHost host;
    Machine machine{arena, host};
    machine.set_suspension_controller(&probe);
    machine.load(kFact);

    std::size_t worst = 0;
    for (int guard = 0; guard < 200000; ++guard) {
        const SuspendReason reason = machine.step_quantum(1);
        worst = std::max(worst, machine.call_stack().size());
        if (reason == SuspendReason::Finished) {
            break;
        }
    }
    TW_CHECK(machine.finished());
    // <program> plus the activations of `fact`: one per level from n = 5 down to
    // n = 0, so seven -- nothing proportional to the number of pauses.
    TW_CHECK(worst <= 7);
    TW_CHECK_EQ(machine.output(), std::string{"120"});
}

// ---------------------------------------------------------------------------
// Call stack
// ---------------------------------------------------------------------------

void test_call_stack_names_the_nesting() {
    announce("call_stack");
    const std::string source =
        "(define (inner x) (* x 2))\n"
        "(define (outer y) (+ 1 (inner (+ y 1))))\n"
        "(display (outer 5))\n";

    Arena arena;
    NullHost host;
    Machine machine{arena, host};
    SourceMap map;

    // Stop the first time we are inside `inner`, which is called from `outer`.
    // `(inner ...)` sits in argument position of `+`, so it really does nest.
    bool stopped = false;
    ProbeController probe{[&stopped](const SuspendState& state) {
        if (stopped || state.depth != 2) {
            return false;
        }
        stopped = true;
        return true;
    }};
    machine.set_suspension_controller(&probe);
    machine.load(source, &map);

    TW_CHECK_EQ(machine.step_quantum(kUnlimited), SuspendReason::Paused);

    const std::vector<CallFrame> stack = machine.call_stack();
    TW_CHECK_EQ(stack.size(), std::size_t{3});
    TW_CHECK_EQ(stack[0].procedure, std::string{"<program>"});
    TW_CHECK_EQ(stack[1].procedure, std::string{"outer"});
    TW_CHECK_EQ(stack[2].procedure, std::string{"inner"});

    // The program frame points at the top-level form being run.
    TW_CHECK_EQ(static_cast<int>(stack[0].span.first_line), 3);
    // `inner`'s frame points at its body, which is on line 1.
    TW_CHECK_EQ(static_cast<int>(stack[2].span.first_line), 1);

    TW_CHECK_EQ(machine.run_loaded(), SuspendReason::Finished);
    TW_CHECK_EQ(machine.output(), std::string{"13"});
}

void test_tail_calls_do_not_nest_while_debugging() {
    announce("tail calls stay flat");
    // 5000 iterations.  If attaching a debugger turned every call into a stack
    // frame this would be 5000 deep, and the call stack view would be useless.
    // A tail call genuinely does not nest, so it must stay flat.
    const std::string source =
        "(define (loop n acc)\n"
        "  (if (= n 0)\n"
        "      acc\n"
        "      (loop (- n 1) (+ acc 1))))\n"
        "(display (loop 5000 0))\n";

    Arena arena;
    NullHost host;
    Machine machine{arena, host};

    std::size_t deepest = 0;
    std::size_t deepest_depth = 0;
    ProbeController probe{[&machine, &deepest, &deepest_depth](
                              const SuspendState& state) {
        if (state.depth > 0) {
            deepest = std::max(deepest, machine.call_stack().size());
            deepest_depth =
                std::max(deepest_depth, static_cast<std::size_t>(state.depth));
        }
        return false;
    }};
    machine.set_suspension_controller(&probe);
    machine.load(source);

    TW_CHECK_EQ(machine.run_loaded(), SuspendReason::Finished);
    TW_CHECK_EQ(machine.output(), std::string{"5000"});

    // <program> + loop.  Constant, regardless of the iteration count.
    TW_CHECK_EQ(deepest, std::size_t{2});
    TW_CHECK_EQ(deepest_depth, std::size_t{1});
    TW_CHECK(machine.peak_frame_depth() < 20);
}

void test_non_tail_recursion_does_nest() {
    announce("non-tail recursion nests");
    const std::string source =
        "(define (sum n)\n"
        "  (if (= n 0)\n"
        "      0\n"
        "      (+ n (sum (- n 1)))))\n"
        "(display (sum 40))\n";

    Arena arena;
    NullHost host;
    Machine machine{arena, host};

    std::size_t deepest = 0;
    ProbeController probe{[&machine, &deepest](const SuspendState& state) {
        if (state.depth > 0) {
            deepest = std::max(deepest, machine.call_stack().size());
        }
        return false;
    }};
    machine.set_suspension_controller(&probe);
    machine.load(source);

    TW_CHECK_EQ(machine.run_loaded(), SuspendReason::Finished);
    TW_CHECK_EQ(machine.output(), std::string{"820"});
    // 41 nested activations -- sum(40) down to sum(0) -- plus the program frame.
    TW_CHECK_EQ(deepest, std::size_t{42});
}

// ---------------------------------------------------------------------------
// The activation-marker bookkeeping
// ---------------------------------------------------------------------------

void test_tail_position_survives_a_preceding_nested_call() {
    announce("tail call after a nested call");
    // Every iteration makes a *nested* call to `outer` (which nests again into
    // `inner`) and then loops in tail position.  The nested call has to restore
    // the caller's tail-position base on its way out; if it restored a height
    // derived from its own marker instead, the tail call that follows would stop
    // being recognised and this would recurse 300 deep.
    const std::string source =
        "(define (inner x) (* x 2))\n"
        "(define (outer y) (+ 100 (inner y)))\n"
        "(define (wrap n)\n"
        "  (if (= n 0)\n"
        "      (display \"done\")\n"
        "      (begin\n"
        "        (outer n)\n"
        "        (wrap (- n 1)))))\n"
        "(wrap 300)\n";

    Arena arena;
    NullHost host;
    Machine machine{arena, host};

    std::size_t deepest_stack = 0;
    std::size_t deepest_depth = 0;
    ProbeController probe{[&machine, &deepest_stack, &deepest_depth](
                              const SuspendState& state) {
        if (state.depth > 0) {
            deepest_stack = std::max(deepest_stack, machine.call_stack().size());
            deepest_depth =
                std::max(deepest_depth, static_cast<std::size_t>(state.depth));
        }
        return false;
    }};
    machine.set_suspension_controller(&probe);
    machine.load(source);

    TW_CHECK_EQ(machine.run_loaded(), SuspendReason::Finished);
    TW_CHECK_EQ(machine.output(), std::string{"done"});

    // <program>, wrap, outer, inner -- and it never gets deeper, no matter how
    // many iterations go by.  If the tail call stopped being recognised after
    // the nested one returned, this would be 300 deep.
    TW_CHECK_EQ(deepest_stack, std::size_t{4});
    // `wrap` tail-calls itself, so its own activation never deepens; the three
    // levels are wrap -> outer -> inner, which is what makes the nested call a
    // nested call in the first place.
    TW_CHECK_EQ(deepest_depth, std::size_t{3});
}

// ---------------------------------------------------------------------------
// Variable inspection
// ---------------------------------------------------------------------------

void test_visible_variables_render_bindings() {
    announce("visible_variables");
    const std::string source =
        "(define (area w h)\n"
        "  (let ((half (* w h)))\n"
        "    (* half 2)))\n"
        "(display (area 3 4))\n";

    Arena arena;
    NullHost host;
    Machine machine{arena, host};
    SourceMap map;

    bool stopped = false;
    ProbeController probe{[&stopped](const SuspendState& state) {
        if (stopped || state.span.first_line != 3) {
            return false;
        }
        stopped = true;
        return true;
    }};
    machine.set_suspension_controller(&probe);
    machine.load(source, &map);

    TW_CHECK_EQ(machine.step_quantum(kUnlimited), SuspendReason::Paused);

    const std::vector<VariableEntry> variables = machine.visible_variables();
    TW_CHECK(has_entry(variables, "half", "12"));
    TW_CHECK(has_entry(variables, "w", "3"));
    TW_CHECK(has_entry(variables, "h", "4"));
    // The kind is the machine's own `type_name`, not something the GUI infers from
    // the printed form -- `12` and `"12"` render differently, but a pair and a
    // symbol are easier to tell apart by kind than by eye.
    TW_CHECK(has_kind(variables, "half", "number"));
    TW_CHECK(has_kind(variables, "area", "procedure"));
    // The user's own global definition is useful; the ~90 builtins are not.
    TW_CHECK(has_name(variables, "area"));
    TW_CHECK(!has_name(variables, "display"));
    TW_CHECK(!has_name(variables, "car"));

    TW_CHECK_EQ(machine.run_loaded(), SuspendReason::Finished);
    TW_CHECK_EQ(machine.output(), std::string{"24"});
}

void test_visible_variables_respect_the_frame_limit() {
    announce("visible_variables depth limit");
    const std::string source =
        "(define global-ish 7)\n"
        "(define (area w h)\n"
        "  (let ((half (* w h)))\n"
        "    (* half 2)))\n"
        "(display (area 3 4))\n";

    Arena arena;
    NullHost host;
    SourceMap map;
    Machine machine{arena, host};
    bool stopped = false;
    ProbeController probe{[&stopped](const SuspendState& state) {
        if (stopped || state.span.first_line != 4) {
            return false;
        }
        stopped = true;
        return true;
    }};
    machine.set_suspension_controller(&probe);
    machine.load(source, &map);
    TW_CHECK_EQ(machine.step_quantum(kUnlimited), SuspendReason::Paused);

    // One frame: only the `let` bindings, so neither the parameter nor the
    // global should appear.
    const std::vector<VariableEntry> one = machine.visible_variables(1);
    TW_CHECK(has_name(one, "half"));
    TW_CHECK(!has_name(one, "w"));
    TW_CHECK(!has_name(one, "global-ish"));

    const std::vector<VariableEntry> all = machine.visible_variables(8);
    TW_CHECK(has_name(all, "half"));
    TW_CHECK(has_name(all, "w"));
    TW_CHECK(has_name(all, "global-ish"));
}

}  // namespace

int main() {
    test_tokenizer_reports_start_lines();
    test_source_map_records_form_spans();
    test_source_map_is_optional();
    test_line_span_contains_ignores_unknown();

    test_stepping_converges_on_the_same_result();
    test_zero_budget_does_no_work();

    test_suspending_everywhere_changes_nothing();
    test_suspension_point_is_a_form_boundary();
    test_suspend_state_reports_the_form_about_to_run();
    test_many_suspensions_do_not_leak_frames();

    test_call_stack_names_the_nesting();
    test_tail_calls_do_not_nest_while_debugging();
    test_non_tail_recursion_does_nest();

    test_tail_position_survives_a_preceding_nested_call();

    test_visible_variables_render_bindings();
    test_visible_variables_respect_the_frame_limit();
    return tw::test::summary("debug");
}
