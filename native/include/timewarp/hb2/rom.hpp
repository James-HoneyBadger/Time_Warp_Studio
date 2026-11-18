// SPDX-License-Identifier: MIT
//
// The ROM socket: a language, running on the machine's own hardware.
//
// The 1982 machine's socket was the eleven languages; the HB-2's is the same idea with
// a processor underneath it. A ROM here is a *language* -- BASIC, Logo, PILOT and the
// rest -- run by the same native engine the editor runs, with the machine's bus
// attached, which is the whole difference between running a program in a tab and
// running it on a computer:
//
//   * `POKE 1048576, 5` writes a pixel, because that address is the framebuffer;
//   * `LOCATE`, `COLOR`, `CLS` and `PRINT` reach the video chip;
//   * `SYS 4096` assembles nothing and runs the HB-16 code already at `$001000`,
//     stopping when it halts, and prints what it printed.
//
// The engines are not modified to allow this. `Interpreter::MachineBus` is the seam
// the HB-1 already uses, and this is the HB-2 on the other side of it.
//
// Scheme is not here: it is its own machine (`timewarp_scheme`) rather than an
// executor, and CHIP-8 is a different computer with its own window. The socket holds
// the nine engines that are an interpreter over a bus.

#pragma once

#include <cstdint>
#include <string>

#include "timewarp/hb2/machine.hpp"
#include "timewarp/lang/language.hpp"

namespace timewarp::hb2 {

class NativeRom {
public:
    NativeRom(Machine& machine, lang::Language language);

    [[nodiscard]] lang::Language language() const noexcept { return language_; }

    /// Runs `source` to completion on this ROM and returns everything it printed.
    /// A language that is not one of the nine is refused with a sentence.
    std::string run(const std::string& source, std::uint32_t instruction_budget = 2000000);

    /// The engine's own error, empty when the run finished.
    [[nodiscard]] const std::string& error() const noexcept { return error_; }
    /// Why a run could not start at all: the language has no executor, or the
    /// machine's console is already busy.
    [[nodiscard]] const std::string& refusal() const noexcept { return refusal_; }

    /// The languages the socket accepts, in the order a selector shows them.
    [[nodiscard]] static std::vector<lang::Language> languages();
    /// The one HB-2 service a language needs and no language has: run machine code.
    /// `SYS` in BASIC is spelled `sys(...)` here, so the statement stays a statement.
    [[nodiscard]] std::string sys(std::uint32_t address, std::uint32_t budget);

private:
    Machine& machine_;
    lang::Language language_;
    std::string error_;
    std::string refusal_;
};

}  // namespace timewarp::hb2
