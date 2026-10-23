// SPDX-License-Identifier: MIT
//
// APL, ported from `time_warp/languages/apl.py`.
//
// Two features make APL unlike anything else in the studio: there is **no
// operator precedence** -- a line is evaluated right to left -- and most
// functions are **pervasive**, so they work on whole arrays. `1 2 3 + 10` is
// `11 12 13`. The value of the port is that it behaves like the reference,
// which is why every message here is the reference's message.
//
// The subset is bounded and says so: reduce and scan work along the outermost
// axis, there is no rank operator and no system function, and the turtle words
// (FD/BK/RT/LT/PU/PD/HOME/CS plus the read-only TX/TY/THETA) are this
// project's extension rather than APL's.

#include "timewarp/lang/apl.hpp"

#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/turtle_host.hpp"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <functional>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "timewarp/lang/apl.hpp"

#include "apl_internal.hpp"

#include <string>
#include <string_view>
#include <vector>

namespace timewarp::lang {

using namespace apl_detail;

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

std::string execute_apl(Interpreter& interpreter, std::string_view source,
                        TurtleHost& turtle) {
    AplState& state = interpreter.executor_state<AplState>();
    // The turtle's position and heading, under names that cannot collide with a
    // program's own: X, Y and H are ordinary APL variables.
    state.variables["TX"] = turtle_state_value(state.pose.x());
    state.variables["TY"] = turtle_state_value(state.pose.y());
    state.variables["THETA"] = turtle_state_value(state.pose.heading());

    const std::string text = trim(source);

    if (state.defining.has_value()) {
        if (text == "∇") {
            const Definition definition = std::move(*state.defining);
            state.defining.reset();
            if (definition.body.empty()) {
                return "❌ APL: a function needs a body between the ∇ lines\n";
            }
            state.functions[definition.header.name] =
                Function{definition.header.parameters, definition.body};
            return "✅ APL: " + definition.header.name + " defined\n";
        }
        if (starts_with(text, "∇")) {
            state.defining.reset();
            return "❌ APL: a definition was already open\n";
        }
        state.defining->body.push_back(trim(source));
        return "";
    }

    if (starts_with(text, "∇")) {
        try {
            const std::string header_text =
                trim(std::string_view{text}.substr(std::string("∇").size()));
            state.defining = Definition{parse_header(header_text), {}};
        } catch (const AplError& error) {
            return std::string{"❌ APL: "} + error.what() + "\n";
        }
        return "";
    }

    {
        const std::size_t comment = text.find("⍝");
        const std::string code =
            trim(comment == std::string::npos
                     ? std::string_view{text}
                     : std::string_view{text}.substr(0, comment));
        if (code.empty()) {
            return "";
        }
    }

    try {
        auto [value, assigned] = statement(text, state, &turtle);
        if (assigned) {
            return "";
        }
        if (state.quiet) {
            state.quiet = false;
            return "";
        }
        return display_text(value) + "\n";
    } catch (const AplError& error) {
        return std::string{"❌ APL: "} + error.what() + "\n";
    } catch (const std::exception& error) {
        return std::string{"❌ APL: "} + error.what() + "\n";
    }
}

}  // namespace timewarp::lang
