// SPDX-License-Identifier: MIT
//
// Host services the evaluator needs from the surrounding application.
//
// The reference imported nothing for this, but reached into an `Interpreter`
// for its step budget and into a `TurtleState` for drawing, which is what made
// it impossible to run without booting the IDE.  Both are virtual here, so the
// same machine embeds in the Qt application and drives headless in a test.

#pragma once

#include <cstdint>

namespace timewarp::scheme {

/// Services the evaluator needs from its embedding application.
class Host {
public:
    virtual ~Host() = default;

    /// Charges `steps` against the host's allowance.
    ///
    /// Returns `false` to abort the program.  Called once per 512 evaluation
    /// steps, matching the reference's cadence so budget accounting is
    /// unchanged.
    virtual bool tick(std::uint64_t steps) {
        (void)steps;
        return true;
    }

    // -- turtle graphics -------------------------------------------------
    virtual void forward(double) {}
    virtual void back(double) {}
    virtual void right(double) {}
    virtual void left(double) {}
    virtual void goto_xy(double, double) {}
    virtual void set_heading(double) {}
    virtual void set_pen_width(double) {}
    virtual void set_color(int, int, int) {}
    virtual void pen_up() {}
    virtual void pen_down() {}
    virtual void home() {}
    virtual void clear() {}
    virtual void circle(double) {}
    virtual void show_turtle() {}
    virtual void hide_turtle() {}
};

/// A host that draws nothing and never expires.  Used by tests and the CLI.
class NullHost final : public Host {};

}  // namespace timewarp::scheme
