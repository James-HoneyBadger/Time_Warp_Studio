// SPDX-License-Identifier: MIT
//
// Runs one program through the native engines and reports what it did.
//
//   time_warp_check --language basic program.bas
//   time_warp_check --language logo  --json program.logo
//   time_warp_check --language basic -            (source on stdin)
//
// This is the tool the gates drive. It exists because the example, command and
// REPL harnesses used to spawn `python3 native/bridge/worker.py` to find out what
// a program did, which made a Python interpreter part of the *repository's* test
// suite long after the product stopped needing one. Every language has a native
// executor, so the answer was already in this build.
//
// What it reports is what the IDE would have received: the same `RecordingHost`
// the API and `lang_tests` use, so the drawing counted here is the drawing that
// reaches the canvas, not a second opinion about it.
//
// `--language` is required when the source comes from stdin and is never guessed
// from an extension: the extension table lives in the IDE (`ui/language.cpp`), and
// a copy of it here would be a third statement of a fact that already has two.
//
// Exit status: 0 the program ran, 1 it failed or timed out, 2 the arguments did
// not make a run. A program's own errors are reported in `--json` and on stdout;
// they are not exceptions.

#include <cstdio>
#include <iostream>
#include <iterator>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include <QFile>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QString>

#include "timewarp/graphics/recording_host.hpp"
#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/language.hpp"
#include "timewarp/scheme/machine.hpp"
#include "timewarp/scheme/source_map.hpp"

namespace {

struct Options {
    timewarp::lang::Language language = timewarp::lang::Language::Basic;
    bool json = false;
    bool quiet = false;
    std::string path;
};

int usage() {
    std::fprintf(stderr,
                 "usage: time_warp_check --language <key> [--json] [--quiet] <file>\n"
                 "       time_warp_check --language <key> [--json] -   (stdin)\n"
                 "\n"
                 "  --language  basic, apl, logo, pilot, focal, scheme, chip8,\n"
                 "              comal, rexx, smalltalk\n"
                 "  --json      line-delimited JSON, in the shape the harnesses read\n"
                 "  --quiet     print nothing when the program ran cleanly\n");
    return 2;
}

/// The answer as the canvas would receive it, counted by kind. A gate asks two
/// things about drawing -- did it draw anything, and did a clear take it away --
/// and both are counts, so counts are what the message carries.
QJsonObject canvasJson(const timewarp::graphics::RecordingHost& host) {
    std::size_t lines = host.segmentCount();
    std::size_t polygons = 0;
    std::size_t rects = 0;
    std::size_t ellipses = 0;
    std::size_t points = 0;
    std::size_t texts = 0;
    for (const auto& shape : host.shapes()) {
        using Kind = timewarp::graphics::RecordingHost::Shape::Kind;
        switch (shape.kind) {
            case Kind::Polygon: ++polygons; break;
            case Kind::Rect: ++rects; break;
            case Kind::Circle: ++ellipses; break;
            case Kind::Point: ++points; break;
            case Kind::Text: ++texts; break;
        }
    }
    QJsonObject kinds;
    kinds.insert(QStringLiteral("line"), static_cast<double>(lines));
    kinds.insert(QStringLiteral("poly"), static_cast<double>(polygons));
    kinds.insert(QStringLiteral("rect"), static_cast<double>(rects));
    kinds.insert(QStringLiteral("ellipse"), static_cast<double>(ellipses));
    kinds.insert(QStringLiteral("point"), static_cast<double>(points));
    kinds.insert(QStringLiteral("text"), static_cast<double>(texts));

    QJsonObject canvas;
    canvas.insert(QStringLiteral("t"), QStringLiteral("canvas"));
    canvas.insert(QStringLiteral("kinds"), kinds);
    // Ink is not the only observable: `RIGHT 90` in Logo, `DRAW` in FOCAL and a
    // COMAL pen change each change the drawing without adding to it. A gate that
    // asked only about ink called those rows silent.
    canvas.insert(QStringLiteral("actions"), static_cast<double>(host.actionCount()));
    if (host.screenWidth() > 0 && host.screenHeight() > 0) {
        QJsonArray screen;
        screen.append(host.screenWidth());
        screen.append(host.screenHeight());
        canvas.insert(QStringLiteral("screen"), screen);
    }
    return canvas;
}

QJsonArray variablesJson(const std::map<std::string, timewarp::lang::Value>& variables) {
    QJsonArray rows;
    for (const auto& [name, value] : variables) {
        QJsonObject row;
        row.insert(QStringLiteral("name"), QString::fromStdString(name));
        row.insert(QStringLiteral("type"), QString::fromStdString(value.type_name()));
        row.insert(QStringLiteral("value"), QString::fromStdString(value.render()));
        rows.append(row);
    }
    return rows;
}

bool readSource(const std::string& path, std::string* source) {
    if (path == "-") {
        *source = std::string{std::istreambuf_iterator<char>{std::cin},
                              std::istreambuf_iterator<char>{}};
        return true;
    }
    QFile file{QString::fromStdString(path)};
    if (!file.open(QIODevice::ReadOnly)) {
        std::fprintf(stderr, "time_warp_check: cannot read %s\n", path.c_str());
        return false;
    }
    const QByteArray bytes = file.readAll();
    source->assign(bytes.constData(), static_cast<std::size_t>(bytes.size()));
    return true;
}

}  // namespace

int main(int argc, char** argv) {
    // Qt Core only, and only for the JSON writer: a run needs no window and no
    // event loop, and a gate runs hundreds of these.

    Options options;
    for (int index = 1; index < argc; ++index) {
        const std::string_view argument{argv[index]};
        if (argument == "--json") {
            options.json = true;
        } else if (argument == "--quiet") {
            options.quiet = true;
        } else if (argument == "--language") {
            if (index + 1 >= argc) {
                return usage();
            }
            const auto language = timewarp::lang::language_from_key(argv[++index]);
            if (!language.has_value()) {
                std::fprintf(stderr, "time_warp_check: unknown language: %s\n", argv[index]);
                return 2;
            }
            options.language = *language;
        } else if (argument.rfind("--", 0) == 0) {
            return usage();
        } else if (options.path.empty()) {
            options.path = std::string{argument};
        } else {
            return usage();
        }
    }
    if (options.path.empty()) {
        return usage();
    }

    std::string source;
    if (!readSource(options.path, &source)) {
        return 2;
    }

    timewarp::graphics::RecordingHost host;
    timewarp::lang::Interpreter interpreter{options.language, host};
    std::vector<std::string> printed;
    // A program whose statement fails inside a loop prints that failure on every
    // iteration. Storing all of them turned a broken example into a 2GB vector
    // and then a segfault in JSON mode, so the record is capped and the count is
    // reported instead: what a reader needs is the first few, and what a gate
    // needs is that it was not one.
    constexpr std::size_t kMaxStoredLines = 2000;
    std::size_t printed_total = 0;
    interpreter.set_output_callback([&](const std::string& line) {
        ++printed_total;
        if (printed.size() < kMaxStoredLines) {
            printed.push_back(line);
        }
        if (!options.json) {
            std::printf("%s\n", line.c_str());
        }
    });
    // A program that keeps asking for input has to run out somewhere. The
    // reference harness resumes at most 500 times and stops where it stands, so
    // the native side stops at the same point rather than running a different
    // program from the same text.
    constexpr int kMaxInputRounds = 500;
    int rounds = 0;
    interpreter.set_input_callback([&interpreter, &rounds](const std::string&) {
        if (++rounds >= kMaxInputRounds) {
            interpreter.request_stop();
        }
        return std::string{};
    });

    std::string error;
    bool ok = true;
    std::uint64_t steps = 0;
    double elapsed = 0.0;

    if (options.language == timewarp::lang::Language::Scheme) {
        // Scheme is not one of the interpreters `executor_for` answers for: it is
        // the separate explicit-stack machine, and the IDE runs it through its
        // debugger runner for that reason. The same machine, run to completion,
        // answers the same question here -- and answers it as Scheme rather than
        // with `Interpreter`'s "not implemented natively" sentence, which would
        // have made this tool report a language the product runs as missing.
        timewarp::scheme::Arena arena;
        timewarp::scheme::SourceMap source_map;
        timewarp::scheme::Machine machine{arena, host};
        const std::string text = machine.execute(source);
        steps = machine.steps();
        std::size_t start = 0;
        while (start <= text.size()) {
            const std::size_t end = text.find('\n', start);
            const std::string line =
                end == std::string::npos ? text.substr(start) : text.substr(start, end - start);
            if (!line.empty()) {
                printed.push_back(line);
                if (!options.json) {
                    std::printf("%s\n", line.c_str());
                }
                if (line.rfind("❌", 0) == 0) {
                    error = line;
                }
            }
            if (end == std::string::npos) {
                break;
            }
            start = end + 1;
        }
        ok = error.empty();
    } else {
        interpreter.load_program(source);
        interpreter.run();
        error = interpreter.error();
        ok = error.empty() && !interpreter.timed_out();
        steps = interpreter.steps_used();
        elapsed = interpreter.elapsed_ms();
    }

    if (options.json) {
        const QJsonObject canvas = canvasJson(host);
        QJsonObject done;
        done.insert(QStringLiteral("t"), QStringLiteral("done"));
        done.insert(QStringLiteral("ok"), ok);
        if (!ok) {
            const std::string detail =
                error.empty() ? std::string{"the run hit its budget"} : error;
            done.insert(QStringLiteral("error"), QString::fromStdString(detail));
        }
        done.insert(QStringLiteral("steps"), static_cast<double>(steps));
        done.insert(QStringLiteral("elapsed_ms"), elapsed);
        done.insert(QStringLiteral("variables"), variablesJson(interpreter.variables()));
        done.insert(QStringLiteral("output_lines"), static_cast<double>(printed_total));

        // The worker's three message shapes, in the order a client applied them:
        // what was printed, what was drawn, then the verdict.
        for (const std::string& line : printed) {
            QJsonObject out;
            out.insert(QStringLiteral("t"), QStringLiteral("out"));
            out.insert(QStringLiteral("text"), QString::fromStdString(line));
            std::printf("%s\n", QJsonDocument{out}.toJson(QJsonDocument::Compact).constData());
        }
        std::printf("%s\n", QJsonDocument{canvas}.toJson(QJsonDocument::Compact).constData());
        std::printf("%s\n", QJsonDocument{done}.toJson(QJsonDocument::Compact).constData());
        std::fflush(stdout);
        return ok ? 0 : 1;
    }

    if (!options.quiet) {
        std::size_t ink = host.segmentCount() + host.shapeCount();
        std::printf("-- %zu line(s), %zu shape(s)%s\n", host.segmentCount(),
                    host.shapeCount(),
                    host.screenWidth() > 0 ? ", screen" : "");
        if (ink == 0 && host.actionCount() == 0) {
            std::printf("-- the program did not use the turtle\n");
        }
        if (printed_total > printed.size()) {
            std::printf("-- %zu output line(s), the first %zu kept\n", printed_total,
                        printed.size());
        }
        std::printf("-- %s\n", ok ? "ok" : (error.empty() ? "budget" : error.c_str()));
    }
    if (!ok && !error.empty()) {
        // A failed run says so on the error stream too, so a `make` target that
        // only prints stderr still shows the reason.
        std::fprintf(stderr, "time_warp_check: %s\n", error.c_str());
    }
    return ok ? 0 : 1;
}
