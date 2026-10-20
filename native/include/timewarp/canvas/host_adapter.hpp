// SPDX-License-Identifier: MIT
//
// The interpreter-side half of the bridge: a `scheme::Host` that translates
// turtle calls into canvas commands.
//
// Header-only on purpose.  `timewarp_canvas` stays free of any dependency on
// `timewarp_scheme` -- a target only pulls the interpreter in if it includes
// this header.
//
// The translation is deliberately 1:1 and stateless.  The GUI keeps its own
// mirror of the turtle state and applies every command, including the state
// setters, so a drawing command never has to carry a copy of the current pen
// colour or heading.  That is both cheaper and more faithful: the mirror sees
// exactly the sequence the interpreter performed, in order.

#pragma once

#include <cstdint>
#include <functional>
#include <string>
#include <string_view>
#include <utility>

#include "timewarp/canvas/command.hpp"
#include "timewarp/canvas/command_queue.hpp"
#include "timewarp/scheme/host.hpp"

namespace timewarp::canvas {

/// Translates `scheme::Host` turtle calls into queued canvas commands.
///
/// Construct one per interpreter run, on the interpreter thread.  Pushing is
/// the only cross-thread interaction; the interpreter never touches a widget.
class QueuedTurtleHost final : public timewarp::scheme::Host {
public:
    /// Called after a push so the GUI can be woken.  Typically
    /// `bridge.wake`.  Cheap: the bridge collapses repeated calls.
    using Wakeup = std::function<void()>;
    /// Forwarded to `scheme::Host::tick`; returns false to abort the program.
    using Budget = std::function<bool(std::uint64_t)>;
    /// The IDE's database, called from the interpreter thread.
    using SqlRunner = std::function<std::string(std::string_view)>;

    explicit QueuedTurtleHost(CanvasCommandQueue& queue) : queue_{queue} {}

    void set_wakeup(Wakeup wakeup) { wakeup_ = std::move(wakeup); }
    void set_budget(Budget budget) { budget_ = std::move(budget); }
    void set_sql_runner(SqlRunner runner) { sql_runner_ = std::move(runner); }

    /// Publishes any coalesced state change.  Call whenever the interpreter
    /// pauses -- end of the run, a debugger break, a pause request -- so the
    /// final state is visible rather than held in the staging buffer.
    void flush() { queue_.flush(); }

    // -- accounting -------------------------------------------------------

    /// Commands the queue refused.  Only ever non-zero once the queue is closed
    /// (the window is going away) or for a record larger than the whole ring.
    [[nodiscard]] std::uint64_t rejected() const noexcept { return rejected_; }
    [[nodiscard]] std::uint64_t submitted() const noexcept { return submitted_; }
    [[nodiscard]] bool queue_closed() const noexcept { return queue_.closed(); }

    // -- scheme::Host ------------------------------------------------------
    //
    // `tick` is *not* overridden unless a budget is supplied, so the base
    // class's "always continue" default applies when the caller does not care.

    [[nodiscard]] bool tick(std::uint64_t steps) override {
        return budget_ ? budget_(steps) : true;
    }

    [[nodiscard]] std::string sql(std::string_view statement) override {
        if (!sql_runner_) {
            return "\u274C SQL: no database is open.\n";
        }
        return sql_runner_(statement);
    }

    void forward(double distance) override {
        submit(make_motion(CanvasCommandKind::Forward, distance));
    }
    void back(double distance) override {
        submit(make_motion(CanvasCommandKind::Back, distance));
    }
    void right(double degrees) override {
        submit(make_motion(CanvasCommandKind::Right, degrees));
    }
    void left(double degrees) override {
        submit(make_motion(CanvasCommandKind::Left, degrees));
    }
    void goto_xy(double px, double py) override {
        submit(make_position(px, py));
    }
    void set_heading(double degrees) override {
        submit(make_motion(CanvasCommandKind::SetHeading, degrees));
    }
    void set_pen_width(double width) override {
        submit(make_motion(CanvasCommandKind::SetPenWidth, width));
    }
    void set_color(int red, int green, int blue) override {
        submit(make_color(CanvasCommandKind::SetPenColor,
                          make_rgb(red, green, blue)));
    }
    void pen_up() override { submit(make_command(CanvasCommandKind::PenUp)); }
    void pen_down() override {
        submit(make_command(CanvasCommandKind::PenDown));
    }
    void home() override { submit(make_command(CanvasCommandKind::Home)); }
    void clear() override { submit(make_command(CanvasCommandKind::Clear)); }
    void circle(double radius) override { submit(make_circle(radius)); }
    void show_turtle() override {
        submit(make_command(CanvasCommandKind::ShowTurtle));
    }
    void hide_turtle() override {
        submit(make_command(CanvasCommandKind::HideTurtle));
    }

private:
    /// Not named `emit`: Qt defines that as a macro, which would silently erase
    /// the name and turn every call into a discarded expression.  This header is
    /// meant to be includable alongside Qt, so it avoids every Qt keyword macro.
    void submit(const CanvasCommand& command) {
        ++submitted_;
        const PushResult result = queue_.push(command);
        if (!accepted(result)) {
            ++rejected_;
            return;
        }
        if (wakeup_) {
            wakeup_();
        }
    }

    CanvasCommandQueue& queue_;
    Wakeup wakeup_;
    Budget budget_;
    SqlRunner sql_runner_;
    std::uint64_t submitted_ = 0;
    std::uint64_t rejected_ = 0;
};

}  // namespace timewarp::canvas
