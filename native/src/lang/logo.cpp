// SPDX-License-Identifier: MIT
//
// Logo: the C++ port of `native/bridge/time_warp/languages/logo.py`.
//
// Logo is a whole-program language here, although the reference runs it line by
// line, because its *loader* is not the generic one: a `TO ... END` block
// defines a procedure rather than executing, and a command whose brackets span
// several lines is one statement. The reference does that work in
// `_parse_logo_program` before its loop starts, and then hands each joined
// statement to `execute_logo`. Doing both halves here means the statement
// boundaries come from the same code that knows what a procedure is.
//
// The output is streamed statement by statement (`log_output`), so the console
// fills while the program runs rather than at the end, and the `[line N]`
// annotation the IDE puts on an error is applied here with the same numbering
// the reference uses -- the statement's index, because a Logo line has no
// number of its own.
//
// Every quirk below is deliberate, because the reference has it and the
// differential harness compares against the reference. Where a behaviour looks
// like a bug (a list printed as `['1.0', '2.0']`, a `LOCAL "X` that makes a

#include "timewarp/lang/logo.hpp"

#include "logo_internal.hpp"

namespace timewarp::lang {

std::string execute_logo(Interpreter& interpreter, std::string_view source,
                         TurtleHost& turtle) {
    logo_detail::LogoMachine machine{interpreter, turtle};
    machine.run(source);
    return {};
}

std::string execute_logo_command(Interpreter& interpreter, std::string_view command,
                                 TurtleHost& turtle) {
    logo_detail::LogoMachine machine{interpreter, turtle};
    return machine.run_one(command);
}

bool is_logo_command(std::string_view word) {
    return logo_detail::in_set(logo_detail::logo_commands(),
                               logo_detail::upper(word));
}

}  // namespace timewarp::lang
