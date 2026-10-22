// SPDX-License-Identifier: MIT
//
// The gates, as one command with four questions. See `gates.hpp` for why they are
// one binary and what each subcommand answers.

#include "gates.hpp"

#include <QCoreApplication>
#include <QStringList>

#include <cstdio>

namespace {

void usage() {
    std::fprintf(stderr,
                 "usage: time_warp_gates <command> [options]\n"
                 "\n"
                 "  smoke     every Examples/** program through time_warp_check\n"
                 "  commands  every row of the command table, one program each\n"
                 "  repl      every row again, through the REPL API\n"
                 "  golden    the ports against the frozen reference corpora\n"
                 "  tree      that the tree holds no Python\n"
                 "  registry  that the language table has one home\n"
                 "  structure that no file has outgrown its budget\n"
                 "\n"
                 "Run with no command for this message. Each command returns 0 only\n"
                 "when every question it asked was answered, and prints what failed.\n");
}

}  // namespace

int main(int argc, char** argv) {
    QCoreApplication application{argc, argv};
    QCoreApplication::setApplicationName(QStringLiteral("time_warp_gates"));

    const QStringList arguments = QCoreApplication::arguments();
    if (arguments.size() < 2) {
        usage();
        return 2;
    }
    const QString command = arguments.at(1);
    const QStringList rest = arguments.mid(2);

    if (command == QLatin1String("smoke")) {
        return timewarp::gates::runSmoke(rest);
    }
    if (command == QLatin1String("commands")) {
        return timewarp::gates::runCommands(rest);
    }
    if (command == QLatin1String("repl")) {
        return timewarp::gates::runRepl(rest);
    }
    if (command == QLatin1String("golden")) {
        return timewarp::gates::runGolden(rest);
    }
    if (command == QLatin1String("tree")) {
        return timewarp::gates::runTree(rest);
    }
    if (command == QLatin1String("registry")) {
        return timewarp::gates::runRegistry(rest);
    }
    if (command == QLatin1String("structure")) {
        return timewarp::gates::runStructure(rest);
    }
    if (command == QLatin1String("--help") || command == QLatin1String("-h") ||
        command == QLatin1String("help")) {
        usage();
        return 0;
    }
    std::fprintf(stderr, "time_warp_gates: no such command: %s\n", qPrintable(command));
    usage();
    return 2;
}
