// SPDX-License-Identifier: MIT
//
// The gates, as one command with seven questions. See `gates.hpp` for why they are
// one binary and what each subcommand answers.
//
// The command line is `timewarp::cli`'s, which is what every tool in `tools/`
// speaks: `--root`, `--json PATH`, `--quiet` and `--help` in any position, and
// status 2 for a command line that cannot be used. `commands` writes a per-row
// report, because a gate that fails one row is more useful with the row's own
// record than with a single verdict; every other gate writes one object saying
// whether it passed, and its prose goes to stderr while that object is written.

#include "gates.hpp"

#include "timewarp/tools/cli.hpp"

#include <QCoreApplication>
#include <QStringList>

#include <array>
#include <cstdio>
#include <span>
#include <string>

namespace {

using timewarp::cli::Invocation;

/// The gate functions take a `QStringList` because their options are Qt values.
/// This is the one place the two vocabularies meet.
QStringList wordsOf(const Invocation& invocation) {
    QStringList words;
    for (const std::string& argument : invocation.arguments) {
        words << QString::fromStdString(argument);
    }
    return words;
}

/// `commands` has its own report: one record per row, with the program's source and
/// what was expected. The shared `--json` is forwarded to it in the spelling it
/// already takes.
QStringList commandsWords(const Invocation& invocation) {
    QStringList words = wordsOf(invocation);
    if (invocation.json_stdout) {
        words << QStringLiteral("--json") << QStringLiteral("-");
    } else if (!invocation.json.empty()) {
        words << QStringLiteral("--json")
              << QString::fromStdString(invocation.json.string());
    }
    return words;
}

int runSmoke(const Invocation& invocation) {
    return timewarp::gates::runSmoke(wordsOf(invocation));
}

int runCommands(const Invocation& invocation) {
    return timewarp::gates::runCommands(commandsWords(invocation));
}

int runRepl(const Invocation& invocation) {
    return timewarp::gates::runRepl(wordsOf(invocation));
}

int runGolden(const Invocation& invocation) {
    return timewarp::gates::runGolden(wordsOf(invocation));
}

int runTree(const Invocation& invocation) {
    return timewarp::gates::runTree(wordsOf(invocation));
}

int runRegistry(const Invocation& invocation) {
    return timewarp::gates::runRegistry(wordsOf(invocation));
}

int runStructure(const Invocation& invocation) {
    return timewarp::gates::runStructure(wordsOf(invocation));
}

int runQt5(const Invocation& invocation) {
    return timewarp::gates::runQt5(wordsOf(invocation));
}

constexpr std::array<timewarp::cli::Command, 8> kCommands{{
    {"smoke", "every Examples/** program through time_warp_check", runSmoke},
    {"commands", "every row of the command table, one program each", runCommands},
    {"repl", "every row again, through the REPL API", runRepl},
    {"golden", "the ports against the frozen reference corpora", runGolden},
    {"tree", "that the tree holds no Python", runTree},
    {"registry", "that the language table has one home", runRegistry},
    {"structure", "that no file has outgrown its budget", runStructure},
    {"qt5", "that no call site needs the Qt 6 spelling of a signal", runQt5},
}};

constexpr std::string_view kSynopsis{"<command> [--language NAME] [--verbose] [--explain]"};

}  // namespace

int main(int argc, char** argv) {
    QCoreApplication application{argc, argv};
    QCoreApplication::setApplicationName(QStringLiteral("time_warp_gates"));

    const Invocation invocation = timewarp::cli::parse("time_warp_gates", argc, argv);
    timewarp::gates::setQuiet(invocation.quiet);
    timewarp::gates::setProseToStderr(invocation.wants_json());

    if (invocation.help || !invocation.ok()) {
        return timewarp::cli::dispatch(invocation, kSynopsis, kCommands);
    }
    const int status = timewarp::cli::dispatch(invocation, kSynopsis, kCommands);
    if (!invocation.wants_json() || invocation.command == "commands") {
        // `commands` wrote the per-row array itself; anything else is one verdict.
        return status;
    }
    timewarp::cli::Report report{"time_warp_gates", invocation};
    report.check(invocation.command, status == 0, status == 0 ? "" : "the gate said no");
    return report.finish();
}
