// SPDX-License-Identifier: MIT
//
// The gates: the harnesses that ask the built binaries what each program did.
//
// They were five Python scripts until 22.0.0. Each one spawned a binary, read a
// line-delimited protocol and decided whether the answer was a pass. The reason
// they were Python was convenience, not necessity -- and it cost the project a
// second language in its own tree, an interpreter in CI and a virtual environment
// to document. This is the same set of questions asked in the language the rest of
// the repository is written in.
//
// One binary with subcommands rather than five executables: the parts share the
// protocol readers, the row data and the way they find the binaries they drive,
// and a single link step is a single place for those to live.
//
//     time_warp_gates smoke     every Examples/** program through `time_warp_check`
//     time_warp_gates commands  every row of the command table, one program each
//     time_warp_gates repl      every row again, this time through the REPL API
//     time_warp_gates golden    the ports against the frozen reference corpora
//     time_warp_gates tree      the tree holds no Python
//     time_warp_gates registry  the language table has one home
//     time_warp_gates structure the files are still the size they were designed to be
//
// Each returns 0 only when every question it asked was answered.

#pragma once

#include <QByteArray>
#include <QJsonObject>
#include <QString>
#include <QStringList>

#include <cstdio>
#include <optional>
#include <vector>

namespace timewarp::gates {

/// Where a gate writes what it found: `stdout`, unless the run is writing a machine
/// report, in which case the prose goes to stderr and stdout carries only the object.
[[nodiscard]] FILE* out();

/// Whether `--quiet` was given. Set once by `main` from the shared command line.
[[nodiscard]] bool quiet();
void setQuiet(bool quiet);
void setProseToStderr(bool to_stderr);

/// The checkout these tools belong to, found from the executable's own location.
/// A gate run from another directory must still find the repository it is about.
[[nodiscard]] QString repositoryRoot();

/// A built binary the gates drive: `TIME_WARP_<NAME>` wins, then the two build
/// trees the Makefile produces, then `PATH`. Null when it is nowhere, because "the
/// tool is missing" and "the tool said no" are different reports.
[[nodiscard]] std::optional<QString> locateBinary(const QString& name);

/// One command's smallest program and what should happen.
struct Row {
    QString language;
    QString command;
    QString source;
    /// any | prints:TEXT | draws | error | optional
    QString expect;
    /// Other commands this same source also exercises.
    QStringList also;
};

struct Waiver {
    QString language;
    QString command;
    QString reason;
};

/// `native/share/command_rows.json`: the rows, the waivers and the scratch name.
struct CommandTable {
    std::vector<Row> rows;
    std::vector<Waiver> waivers;
    /// The one file the file-channel rows write. The harness removes it, because
    /// the engine resolves a relative name against the working directory and a
    /// run from the repository root would otherwise leave it behind.
    QString scratch;
};

[[nodiscard]] std::optional<CommandTable> loadCommandTable(QString* error);

/// What one program did, as the two harnesses need to judge it.
struct Outcome {
    QString output;
    /// Ink: drawing commands that put something on the canvas.
    int drawings = 0;
    /// The turtle operations that change the drawing without adding to it. A row
    /// is "the command did something", and `RIGHT 90` did something; the example
    /// corpus counts ink alone, because "how many strokes" is a question about a
    /// picture rather than about activity.
    int actions = 0;
    QStringList errors;
    QJsonObject done;
    bool finished = false;
    QStringList malformed;
    /// Set when the program never produced a result at all.
    QString fatal;
};

/// Runs `source` through `time_warp_check --json -`.
[[nodiscard]] Outcome runProgram(const QString& check, const QString& language,
                                 const QString& source, int timeout_ms);

/// The verdict on one row, with the sentence a reader needs when it failed.
struct Verdict {
    bool ok = false;
    QString why;
};

/// The program harness's verdict (`--json` protocol from `time_warp_check`).
[[nodiscard]] Verdict judgeProgram(const Row& row, const Outcome& outcome);

/// A REPL answer: the `repl.eval` result, or a protocol failure.
struct ReplAnswer {
    QString output;
    QString error;
    int commands = 0;
    int actions = 0;
    bool protocol_error = false;
    QString protocol_message;
};

[[nodiscard]] Verdict judgeRepl(const Row& row, const ReplAnswer& answer);

/// Language names compared with case and punctuation removed, so a row's
/// "CHIP-8" and the inventory's "chip8" are one language.
[[nodiscard]] QString languageKey(const QString& name);

// -- subcommands -----------------------------------------------------------

[[nodiscard]] int runSmoke(const QStringList& arguments);
[[nodiscard]] int runCommands(const QStringList& arguments);
[[nodiscard]] int runRepl(const QStringList& arguments);
[[nodiscard]] int runGolden(const QStringList& arguments);
/// Fails if the tree has grown a `.py` back.
[[nodiscard]] int runTree(const QStringList& arguments);
/// Fails if a language's display name, key or extensions are written down anywhere
/// but `timewarp/lang/registry.hpp`.
[[nodiscard]] int runRegistry(const QStringList& arguments);
/// Fails if a source file or the build file has grown past its budget.
[[nodiscard]] int runStructure(const QStringList& arguments);
/// Fails if a call site names a Qt signal that Qt 5.15 overloads and Qt 6 does not.
[[nodiscard]] int runQt5(const QStringList& arguments);

}  // namespace timewarp::gates
