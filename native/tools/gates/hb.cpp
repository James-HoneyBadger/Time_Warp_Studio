// SPDX-License-Identifier: MIT
//
// `time_warp_gates hb`: the machine's own harness.
//
// The counterpart of `time_warp_check` for a language program, and the thing every
// later proposal on the Model 92 page is verified with from a shell rather than by
// clicking: boot a machine, assemble and run a listing, and print the registers, the
// screen text and a hash of the framebuffer.
//
//     time_warp_gates hb Examples/hb2/palette.s --ticks 4 --screen
//     time_warp_gates hb --map
//
// A program that runs, halts and leaves the screen empty is reported as *drew
// nothing* rather than as a pass, because a machine that runs and shows nothing is
// the failure this project treats as the bug.

#include "gates.hpp"

#include "timewarp/hb2/asm.hpp"
#include "timewarp/hb2/machine.hpp"
#include "timewarp/hb2/map.hpp"

#include <QFile>
#include <QFileInfo>
#include <QStringList>

#include <algorithm>
#include <cstdio>
#include <optional>
#include <string>

namespace timewarp::gates {

namespace {

using timewarp::hb2::Assembly;
using timewarp::hb2::CpuStatus;
using timewarp::hb2::Machine;
using timewarp::hb2::Video;

/// Whether the machine drew anything: a pixel that is not the background in a bitmap
/// mode, or a cell that is not a space in the 1982 text mode.
bool drewSomething(Machine& machine) {
    if (machine.video().mode() == Video::Mode::Text) {
        for (const std::string& row : machine.video().screenText()) {
            if (!row.empty()) {
                return true;
            }
        }
        return false;
    }
    for (int y = 0; y < machine.video().height(); ++y) {
        for (int x = 0; x < machine.video().width(); ++x) {
            if (machine.video().pixel(x, y) != 0) {
                return true;
            }
        }
    }
    return false;
}

/// Reads a listing from the checkout.
std::optional<std::string> readListing(const QString& name) {
    const QString path = QFileInfo{name}.isAbsolute() ? name : repositoryRoot() + "/" + name;
    QFile file{path};
    if (!file.open(QIODevice::ReadOnly)) {
        return std::nullopt;
    }
    return file.readAll().toStdString();
}

/// HB-OS through the harness: several programs, a scheduler and a desktop. Every task
/// must have printed something and none may have died -- two programs making progress
/// at once is the claim, and a picture of one window is not evidence of it.
int runTasks(const QStringList& tasks, int ticks, bool screen) {
    if (tasks.isEmpty()) {
        std::fprintf(stderr, "hb: --tasks needs at least one listing\n");
        return 2;
    }
    Machine machine;
    for (const QString& name : tasks) {
        const std::optional<std::string> source = readListing(name);
        if (!source) {
            std::fprintf(stderr, "hb: cannot read %s\n", qPrintable(name));
            return 1;
        }
        if (machine.spawn(QFileInfo{name}.completeBaseName().toUpper().toStdString(), *source) == 0) {
            std::fprintf(stderr, "hb: %s did not start: %s\n", qPrintable(name),
                         machine.scheduler().lastError().c_str());
            return 1;
        }
    }
    machine.runFrames(ticks);

    std::fprintf(out(), "HB-OS: %llu frames, %zu processes\n",
                 static_cast<unsigned long long>(machine.video().frames()),
                 machine.scheduler().size());
    bool progress = true;
    for (const std::string& row : machine.scheduler().status()) {
        std::fprintf(out(), "  %s\n", row.c_str());
    }
    for (const timewarp::hb2::Task& task : machine.scheduler().tasks()) {
        const std::size_t lines = static_cast<std::size_t>(
            std::count(task.console.begin(), task.console.end(), '\n'));
        std::fprintf(out(), "  %s printed %zu line(s)\n", task.name.c_str(), lines);
        if (lines == 0 && task.failure.empty()) {
            progress = false;
        }
        if (!task.failure.empty()) {
            progress = false;
            std::fprintf(stderr, "hb: %s died: %s\n", task.name.c_str(), task.failure.c_str());
        }
    }
    std::fprintf(out(), "  desktop      %zu window(s), pointer at %d,%d\n", machine.desktop().count(),
                 machine.desktop().pointerX(), machine.desktop().pointerY());
    std::fprintf(out(), "  framebuffer  %016llX\n",
                 static_cast<unsigned long long>(machine.video().hash()));
    if (screen) {
        std::fprintf(out(), "  screen\n");
        for (const std::string& row : machine.video().screenText()) {
            if (row.empty()) {
                continue;
            }
            std::fprintf(out(), "    | %s\n", row.c_str());
        }
    }
    if (!progress) {
        std::fprintf(stderr, "hb: a process made no progress\n");
        return 1;
    }
    return 0;
}

void printMap(FILE* out) {
    std::fprintf(out, "%-12s %s\n", "region", "addresses");
    for (const std::string& row : timewarp::hb2::mapListing()) {
        std::fprintf(out, "%s\n", row.c_str());
    }
}

}  // namespace

int runHb(const QStringList& arguments) {
    QString program;
    QStringList taskList;
    int ticks = 4;
    bool screen = false;
    bool map = false;
    bool tasks = false;
    for (int index = 0; index < arguments.size(); ++index) {
        const QString& argument = arguments.at(index);
        if (argument == QLatin1String("--screen")) {
            screen = true;
        } else if (argument == QLatin1String("--map")) {
            map = true;
        } else if (argument == QLatin1String("--tasks")) {
            tasks = true;
        } else if (argument == QLatin1String("--ticks") && index + 1 < arguments.size()) {
            ticks = arguments.at(++index).toInt();
        } else if (argument.startsWith(QLatin1String("--ticks="))) {
            ticks = argument.section(QLatin1Char('='), 1).toInt();
        } else if (argument.startsWith(QLatin1String("--"))) {
            std::fprintf(stderr, "hb: unknown argument %s\n", qPrintable(argument));
            return 2;
        } else if (tasks) {
            taskList << argument;
        } else if (program.isEmpty()) {
            program = argument;
        } else {
            std::fprintf(stderr, "hb: one listing at a time, or --tasks then several\n");
            return 2;
        }
    }

    if (tasks) {
        return runTasks(taskList, ticks, screen);
    }
    if (ticks <= 0) {
        ticks = 4;
    }

    if (map) {
        printMap(out());
        if (program.isEmpty()) {
            return 0;
        }
    }

    if (program.isEmpty()) {
        if (!map) {
            std::fprintf(stderr, "hb: name a listing, or ask for --map\n");
            return 2;
        }
        return 0;
    }

    const QString path = QFileInfo{program}.isAbsolute() ? program : repositoryRoot() + "/" + program;
    QFile file{path};
    if (!file.open(QIODevice::ReadOnly)) {
        std::fprintf(stderr, "hb: cannot read %s\n", qPrintable(program));
        return 1;
    }
    const std::string source = file.readAll().toStdString();

    Machine machine;
    const Assembly assembly = machine.load(source);
    if (!assembly.ok) {
        std::fprintf(stderr, "hb: %s did not assemble\n", qPrintable(program));
        for (const std::string& error : assembly.errors) {
            std::fprintf(stderr, "     %s\n", error.c_str());
        }
        return 1;
    }

    machine.runFrames(ticks);

    const bool halted = machine.status() == CpuStatus::Halted;
    const bool faulted = machine.status() == CpuStatus::Faulted;
    const bool drew = drewSomething(machine);

    std::fprintf(out(), "%s\n", qPrintable(QFileInfo{program}.fileName()));
    std::fprintf(out(), "  origin       $%06X, %zu bytes\n", assembly.origin, assembly.bytes.size());
    std::fprintf(out(), "  status       %s\n", machine.statusText().c_str());
    std::fprintf(out(), "  registers    R0=%04X R1=%04X R2=%04X R3=%04X\n",
                 machine.cpu().registers().r[0], machine.cpu().registers().r[1],
                 machine.cpu().registers().r[2], machine.cpu().registers().r[3]);
    std::fprintf(out(), "  cycles       %llu in %llu instructions\n",
                 static_cast<unsigned long long>(machine.cpu().registers().cycles),
                 static_cast<unsigned long long>(machine.cpu().registers().instructions));
    std::fprintf(out(), "  video        mode %d, %dx%d\n", static_cast<int>(machine.video().mode()),
                 machine.video().width(), machine.video().height());
    std::fprintf(out(), "  framebuffer  %016llX\n",
                 static_cast<unsigned long long>(machine.video().hash()));
    if (!machine.console().empty()) {
        std::fprintf(out(), "  printed      \"%s\"\n", machine.console().c_str());
    }
    if (screen) {
        std::fprintf(out(), "  screen\n");
        for (const std::string& row : machine.video().screenText()) {
            if (row.empty()) {
                continue;
            }
            std::fprintf(out(), "    | %s\n", row.c_str());
        }
    }

    // The verdict. A fault is a failure, and so is a program that ran and left the
    // screen as it found it -- the bug the two machines in this tree exist to catch.
    if (faulted) {
        std::fprintf(stderr, "hb: %s faulted\n", qPrintable(program));
        return 1;
    }
    if (!halted) {
        std::fprintf(stderr, "hb: %s was still running after %d frames\n", qPrintable(program), ticks);
        return 1;
    }
    if (!drew) {
        std::fprintf(stderr, "hb: %s ran and drew nothing\n", qPrintable(program));
        return 1;
    }
    return 0;
}

}  // namespace timewarp::gates
