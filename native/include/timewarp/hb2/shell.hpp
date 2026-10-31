// SPDX-License-Identifier: MIT
//
// The HB-2's prompt: the machine's own words.
//
// A 1992 computer was operated from a prompt, and the prompt is where its facts are
// visible: the disk's catalogue, the memory map, the registers, the tasks, the
// modules in the bay. This is that prompt. It is Qt-free and returns text, so the
// window shows the lines and a test reads them, and neither of them owns the
// behaviour.
//
// The words are deliberately the machine's rather than a shell's: `DIR`, `SAVE`,
// `RUN`, `REGS`, `DIS`, `TASKS`, `MODULE`. A machine whose prompt is made of words
// nobody documents is an appliance; a machine whose prompt is `HELP` and eleven lines
// is the kind of computer this project is about.

#pragma once

#include <string>
#include <vector>

#include "timewarp/hb2/dos.hpp"
#include "timewarp/hb2/machine.hpp"
#include "timewarp/hb2/modules.hpp"
#include "timewarp/hb2/monitor.hpp"

namespace timewarp::hb2 {

class Shell {
public:
    explicit Shell(Machine& machine);

    /// The disk, the monitor and the bay are optional: a machine with no disk still
    /// has a prompt, and a prompt with no monitor still runs programs.
    void setDos(Dos* dos) noexcept { dos_ = dos; }
    void setMonitor(Monitor* monitor) noexcept { monitor_ = monitor; }
    void setModules(Modules* modules) noexcept { modules_ = modules; }

    /// The listing the machine is holding, which is what `RUN` with no file assembles
    /// and what `SAVE` with no file writes. The window keeps it in step with its
    /// editor; the harness sets it before asking for a run.
    void setSource(std::string source) { source_ = std::move(source); }
    [[nodiscard]] const std::string& source() const noexcept { return source_; }

    void setDrive(int drive) noexcept { drive_ = drive == 1 ? 1 : 0; }
    [[nodiscard]] int drive() const noexcept { return drive_; }

    /// Runs one line and returns what the machine says. A blank line says nothing;
    /// an unknown word says what `HELP` would.
    std::vector<std::string> execute(std::string_view line);

    /// The words, one a line, for `HELP` and for the window's hint.
    [[nodiscard]] static std::vector<std::string> words();

    [[nodiscard]] std::string prompt() const;

private:
    std::vector<std::string> diskWords(const std::string& verb, const std::vector<std::string>& args);
    std::vector<std::string> runListing(const std::string& file, bool as_task);
    [[nodiscard]] bool requireDisk(std::vector<std::string>* out) const;

    Machine& machine_;
    Dos* dos_ = nullptr;
    Monitor* monitor_ = nullptr;
    Modules* modules_ = nullptr;
    std::string source_;
    int drive_ = 0;
};

}  // namespace timewarp::hb2
