// SPDX-License-Identifier: MIT
//
// Forth: the eleventh language.
//
// Whole-program, like Scheme, Logo, Smalltalk and CHIP-8: a Forth program is a
// sequence of *words*, not statements, and `: SQUARE DUP * ;` is a definition that
// spans as many lines as it likes. A line-by-line dispatcher would have to decide
// where a definition ends before the language could, so this one receives the whole
// source.
//
// #### What this Forth is
//
// A tokenising Forth-83-shaped core: a data stack, a dictionary, colon definitions,
// the three control structures (`IF/ELSE/THEN`, `BEGIN/UNTIL` and `BEGIN/WHILE/REPEAT`,
// `DO/LOOP`/`+LOOP` with `I` and `LEAVE`), variables and constants, 16-bit `@`/`!` and
// byte `C@`/`C!` memory, and the usual stack and arithmetic words. It is a real Forth
// and not a subset dressed as one: everything it accepts, it executes, and everything
// it does not accepts it names in the error.
//
// #### What it deliberately is not
//
// No `CREATE ... DOES>`, no vocabularies, no `FIND`, no metacompilation, no pictured
// numeric output. Those are the parts of Forth that make it a language for writing
// Forths; a learner meeting Forth for the first time needs the stack, the dictionary
// and the loop, and the manual says where the line is drawn rather than leaving a
// reader to discover it.

#pragma once

#include <string>
#include <string_view>

namespace timewarp::lang {

class Interpreter;
class TurtleHost;

/// Runs a Forth program and returns everything it printed.
std::string execute_forth(Interpreter& interpreter, std::string_view source,
                          TurtleHost& turtle);

}  // namespace timewarp::lang
