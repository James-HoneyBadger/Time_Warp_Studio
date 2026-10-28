// SPDX-License-Identifier: MIT
//
// The assistant from a shell.
//
// The IDE's panel is the feature; this is the same library with no window, which is
// what makes the assistant scriptable and what lets a person answer "does this key
// work?" in one command instead of a settings dialog and a click.
//
//     time_warp_ai check
//     time_warp_ai config
//     time_warp_ai explain --file Examples/logo/mandala.logo
//     time_warp_ai ask "why does this draw nothing?" --file prog.bas
//     time_warp_ai write "a guessing game" --language BASIC
//
// The answer goes to stdout alone -- no banner, no prefix -- so a pipeline can have
// it. Everything a person needs to read but a pipe does not goes to stderr.

#include "timewarp/ai/client.hpp"
#include "timewarp/ai/config.hpp"
#include "timewarp/ai/prompt.hpp"
#include "timewarp/lang/registry.hpp"
#include "timewarp/tools/cli.hpp"

#include <QCoreApplication>
#include <QEventLoop>
#include <QFile>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QFileInfo>
#include <QObject>
#include <QString>
#include <QStringList>
#include <QTextStream>

#include <array>
#include <cstdio>
#include <exception>
#include <iostream>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace {

using timewarp::ai::Config;
using timewarp::ai::Context;
using timewarp::ai::Conversation;
using timewarp::ai::Task;
using timewarp::cli::Command;
using timewarp::cli::Invocation;

constexpr std::string_view kSynopsis = "<command> [words...] [options]";

constexpr std::string_view kOptions =
    "\noptions:\n"
    "  --file PATH        the program to send ('-' reads standard input)\n"
    "  --language NAME    the language it is written in (BASIC, Logo, ...)\n"
    "  --no-context       send no program at all\n"
    "  --base-url URL     override ai/baseUrl for this run\n"
    "  --model NAME       override ai/model for this run\n"
    "  --protocol NAME    openai or anthropic\n"
    "  --max-tokens N     the reply ceiling\n"
    "  --timeout SECONDS  how long to wait\n"
    "  --json PATH        write the machine report to PATH ('-' for stdout), in\n"
    "                     which case stdout is the report rather than the answer\n"
    "  --quiet            only what failed\n"
    "  --help             this message\n"
    "\nThe key comes from TIME_WARP_AI_API_KEY when it is set, else from the\n"
    "settings file the IDE writes. Nothing here writes a key anywhere.\n";

/// The usage, in this tool's own words rather than the shared printer's: the shared
/// one also lists `--root`, which every tool accepts and this one has no use for. An
/// option in a usage message is a promise.
void print_help(std::ostream& out, const Invocation& invocation,
                std::span<const Command> commands) {
    out << "usage: " << invocation.tool << ' ' << kSynopsis << '\n';
    out << "\ncommands:\n";
    for (const Command& command : commands) {
        out << "  " << command.name;
        for (std::size_t pad = command.name.size(); pad < 14; ++pad) {
            out << ' ';
        }
        out << command.summary << '\n';
    }
    out << kOptions;
}

[[nodiscard]] int usage_error(const Invocation& invocation, const std::string& message,
                              std::span<const Command> commands) {
    std::cerr << invocation.tool;
    if (!invocation.command.empty()) {
        std::cerr << ' ' << invocation.command;
    }
    std::cerr << ": " << message << '\n';
    print_help(std::cerr, invocation, commands);
    return 2;
}

/// Writes the machine report where `--json` asked for it. The answer still goes to
/// stdout when the report went to a file; when the report *is* stdout there is one
/// stream and the report has it.
void write_report(const Invocation& invocation, const QJsonObject& report) {
    if (!invocation.wants_json()) {
        return;
    }
    const QByteArray json = QJsonDocument{report}.toJson(QJsonDocument::Compact);
    if (invocation.json_stdout) {
        std::cout << json.constData() << '\n';
        std::cout.flush();
        return;
    }
    QFile file{QString::fromStdString(invocation.json.string())};
    if (!file.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
        std::cerr << "\u274C " << invocation.json.string() << " could not be written\n";
        return;
    }
    file.write(json);
    file.write("\n");
    file.close();
}

[[nodiscard]] std::optional<int> integer_option(const Invocation& invocation,
                                                std::string_view name) {
    const std::optional<std::string> text = invocation.value(name);
    if (!text.has_value()) {
        return std::nullopt;
    }
    try {
        return std::stoi(*text);
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

/// The stored configuration with this run's overrides applied.
///
/// A command that asks a question turns `enabled` on: typing `time_warp_ai explain`
/// *is* asking, and a tool that refuses because a checkbox in a window is unticked
/// would be the tool disagreeing with the person using it. `check` and `config`
/// report the stored state instead, because their whole job is to say what it is.
[[nodiscard]] Config config_for(const Invocation& invocation, bool asking) {
    Config config = timewarp::ai::load();
    if (asking) {
        config.enabled = true;
    }
    if (const std::optional<std::string> value = invocation.value("--base-url")) {
        config.base_url = QString::fromStdString(*value);
    }
    if (const std::optional<std::string> value = invocation.value("--model")) {
        config.model = QString::fromStdString(*value);
    }
    if (const std::optional<std::string> value = invocation.value("--protocol")) {
        if (const auto protocol =
                timewarp::ai::protocol_from_key(QString::fromStdString(*value))) {
            config.protocol = *protocol;
        }
    }
    if (const std::optional<int> value = integer_option(invocation, "--max-tokens")) {
        config.max_tokens = *value;
    }
    if (const std::optional<int> value = integer_option(invocation, "--timeout")) {
        config.timeout_seconds = *value;
    }
    if (invocation.has("--no-context")) {
        config.send_document = false;
    }
    return config;
}

/// A language name as the table spells it, or the words the user gave when they are
/// not one of the eleven.
[[nodiscard]] QString display_language(const QString& words) {
    const QString wanted = words.trimmed().toLower();
    for (const timewarp::lang::LanguageRow& row : timewarp::lang::kLanguageTable) {
        const QString key =
            QString::fromLatin1(row.key.data(), static_cast<int>(row.key.size()));
        const QString display =
            QString::fromUtf8(row.display.data(), static_cast<int>(row.display.size()));
        if (wanted == key || wanted == display.toLower()) {
            return display;
        }
    }
    return words.trimmed();
}

/// The language of a file, from its extension, through the one table the rest of
/// the program uses.
[[nodiscard]] QString language_of(const QString& path) {
    const std::string extension = QFileInfo{path}.suffix().toLower().toStdString();
    if (extension.empty()) {
        return {};
    }
    if (const timewarp::lang::LanguageRow* row =
            timewarp::lang::language_row_for_extension(extension)) {
        return QString::fromUtf8(row->display.data(),
                                 static_cast<int>(row->display.size()));
    }
    return {};
}

/// Reads `--file`, or nothing when there is none. Reports a read failure rather than
/// sending an empty program as if it were the user's.
[[nodiscard]] std::optional<QString> read_program(const Invocation& invocation,
                                                  QString* error, QString* path) {
    const std::optional<std::string> given = invocation.value("--file");
    if (!given.has_value()) {
        return QString{};
    }
    const QString name = QString::fromStdString(*given);
    if (name == QLatin1String("-")) {
        QFile input;
        if (!input.open(stdin, QIODevice::ReadOnly)) {
            *error = QStringLiteral("standard input could not be read");
            return std::nullopt;
        }
        *path = QStringLiteral("stdin");
        return QString::fromUtf8(input.readAll());
    }
    QFile file{name};
    if (!file.open(QIODevice::ReadOnly)) {
        *error = QStringLiteral("%1 could not be read").arg(name);
        return std::nullopt;
    }
    *path = QFileInfo{name}.fileName();
    return QString::fromUtf8(file.readAll());
}

[[nodiscard]] Context context_for(const Invocation& invocation, QString* error) {
    Context context;
    const std::optional<std::string> file = invocation.value("--file");
    if (!file.has_value()) {
        if (const std::optional<std::string> language = invocation.value("--language")) {
            context.language = display_language(QString::fromStdString(*language));
        }
        return context;
    }
    QString path;
    const std::optional<QString> source = read_program(invocation, error, &path);
    if (!source.has_value()) {
        return context;
    }
    context.source = *source;
    context.file_name = path;
    if (const std::optional<std::string> language = invocation.value("--language")) {
        context.language = display_language(QString::fromStdString(*language));
    } else {
        context.language = language_of(path);
    }
    return context;
}

/// The words of the command that are not options, joined: a question with spaces in
/// it does not need quoting to be one argument. The value-taking options are named
/// here as well as in `--help`, or `--language Logo` would hand `Logo` back as a
/// word of the question.
[[nodiscard]] QString words_of(const Invocation& invocation) {
    QStringList parts;
    for (const std::string& word : invocation.positionals(
             {"--file", "--language", "--base-url", "--model", "--protocol",
              "--max-tokens", "--timeout"})) {
        parts << QString::fromStdString(word);
    }
    return parts.join(QLatin1Char(' ')).trimmed();
}

/// Sends one request and waits for it. The event loop is the whole point: the client
/// is asynchronous because the window must stay responsive, and a terminal tool
/// waits on it rather than the library growing a second, blocking path.
[[nodiscard]] int deliver(const Invocation& invocation, const Config& config,
                          const Conversation& conversation) {
    timewarp::ai::Client client;
    QEventLoop loop;
    int status = 1;
    QObject::connect(
        &client, &timewarp::ai::Client::answered, &loop,
        [&](const timewarp::ai::Reply& reply) {
            write_report(invocation,
                         QJsonObject{{QStringLiteral("ok"), true},
                                     {QStringLiteral("text"), reply.text.trimmed()},
                                     {QStringLiteral("model"), reply.model},
                                     {QStringLiteral("prompt_tokens"), reply.prompt_tokens},
                                     {QStringLiteral("completion_tokens"),
                                      reply.completion_tokens}});
            if (!invocation.json_stdout) {
                QTextStream out{stdout};
                out << reply.text.trimmed() << '\n';
                out.flush();
            }
            status = 0;
            loop.quit();
        });
    QObject::connect(&client, &timewarp::ai::Client::failed, &loop,
                     [&](const QString& message) {
                         write_report(invocation,
                                      QJsonObject{{QStringLiteral("ok"), false},
                                                  {QStringLiteral("error"), message}});
                         std::cerr << "❌ " << message.toStdString() << '\n';
                         status = 1;
                         loop.quit();
                     });
    client.send(config, conversation);
    if (client.busy()) {
        loop.exec();
    }
    return status;
}

/// Asks the endpoint what it can run. Its own function because it is the one
/// command that needs no program, and the answer is a list rather than text.
[[nodiscard]] int run_models(const Invocation& invocation) {
    const Config config = config_for(invocation, true);
    QString why;
    // A model name is not needed to ask what the models are.
    if (!timewarp::ai::reachable(config, &why)) {
        std::cerr << "\u274C " << why.toStdString() << '\n';
        return 1;
    }
    timewarp::ai::Client client;
    QEventLoop loop;
    int status = 1;
    QObject::connect(&client, &timewarp::ai::Client::modelsListed, &loop,
                     [&](const QStringList& models) {
                         QJsonArray array;
                         for (const QString& model : models) {
                             array.append(model);
                         }
                         write_report(invocation,
                                      QJsonObject{{QStringLiteral("ok"), true},
                                                  {QStringLiteral("models"), array}});
                         if (!invocation.json_stdout) {
                             if (models.isEmpty()) {
                                 std::cout << "the service named no models\n";
                             }
                             for (const QString& model : models) {
                                 std::cout << model.toStdString() << '\n';
                             }
                             std::cout.flush();
                         }
                         status = 0;
                         loop.quit();
                     });
    QObject::connect(&client, &timewarp::ai::Client::failed, &loop,
                     [&](const QString& message) {
                         write_report(invocation,
                                      QJsonObject{{QStringLiteral("ok"), false},
                                                  {QStringLiteral("error"), message}});
                         std::cerr << "\u274C " << message.toStdString() << '\n';
                         status = 1;
                         loop.quit();
                     });
    client.listModels(config);
    if (client.busy()) {
        loop.exec();
    }
    return status;
}

/// Everything a task without free text needs: the configuration, the program and
/// one message.
[[nodiscard]] int run_task(const Invocation& invocation, Task task) {
    const Config config = config_for(invocation, true);
    QString why;
    if (!timewarp::ai::ready(config, &why)) {
        std::cerr << "❌ " << why.toStdString() << '\n';
        return 1;
    }
    QString error;
    const Context context = context_for(invocation, &error);
    if (!error.isEmpty()) {
        std::cerr << "❌ " << error.toStdString() << '\n';
        return 1;
    }
    return deliver(invocation, config,
                   timewarp::ai::build_prompt(task, context, words_of(invocation)));
}

int run_check(const Invocation& invocation) {
    const Config config = config_for(invocation, false);
    QString why;
    const bool ok = timewarp::ai::ready(config, &why);
    write_report(invocation, QJsonObject{{QStringLiteral("ok"), ok},
                                         {QStringLiteral("summary"),
                                          timewarp::ai::summary(config)},
                                         {QStringLiteral("why"), why}});
    if (invocation.json_stdout) {
        return ok ? 0 : 1;
    }
    std::ostream& out = ok ? std::cout : std::cerr;
    out << (ok ? "✅ " : "❌ ")
        << timewarp::ai::summary(config).toStdString() << '\n';
    if (!ok) {
        out << "   " << why.toStdString() << '\n';
    }
    return ok ? 0 : 1;
}

int run_config(const Invocation& invocation) {
    const Config config = config_for(invocation, false);
    const bool key_from_environment = !timewarp::ai::environment_key().isEmpty();
    write_report(
        invocation,
        QJsonObject{{QStringLiteral("provider"), timewarp::ai::provider_name(config)},
                    {QStringLiteral("protocol"), timewarp::ai::protocol_key(config.protocol)},
                    {QStringLiteral("endpoint"), timewarp::ai::endpoint_url(config)},
                    {QStringLiteral("model"), config.model},
                    {QStringLiteral("enabled"), config.enabled},
                    {QStringLiteral("key_from_environment"), key_from_environment},
                    {QStringLiteral("key_saved"),
                     !key_from_environment && !config.api_key.isEmpty()},
                    {QStringLiteral("max_tokens"), config.max_tokens},
                    {QStringLiteral("timeout_seconds"), config.timeout_seconds}});
    if (invocation.json_stdout) {
        return 0;
    }
    std::cout << "provider: " << timewarp::ai::provider_name(config).toStdString() << '\n'
              << "protocol: " << timewarp::ai::protocol_key(config.protocol).toStdString()
              << '\n'
              << "endpoint: " << timewarp::ai::endpoint_url(config).toStdString() << '\n'
              << "model: " << config.model.toStdString() << '\n'
              << "enabled: " << (config.enabled ? "yes" : "no") << '\n'
              << "key: "
              << (timewarp::ai::environment_key().isEmpty()
                      ? (config.api_key.isEmpty() ? "none" : "saved in the settings file")
                      : "from " + std::string{timewarp::ai::kEnvApiKey})
              << '\n'
              << "max tokens: " << config.max_tokens << '\n'
              << "timeout: " << config.timeout_seconds << " s\n";
    return 0;
}

int run_explain(const Invocation& invocation) { return run_task(invocation, Task::Explain); }
int run_fix(const Invocation& invocation) { return run_task(invocation, Task::Fix); }
int run_comment(const Invocation& invocation) { return run_task(invocation, Task::Comment); }
int run_ask(const Invocation& invocation) { return run_task(invocation, Task::Question); }
int run_write(const Invocation& invocation) { return run_task(invocation, Task::Write); }
int run_translate(const Invocation& invocation) {
    return run_task(invocation, Task::Translate);
}

}  // namespace

int main(int argc, char** argv) {
    QCoreApplication app{argc, argv};
    // The same identity the IDE writes its settings under, so this reads the
    // configuration the user already made rather than an empty parallel one.
    QCoreApplication::setOrganizationName(QStringLiteral("TimeWarpStudio"));
    QCoreApplication::setApplicationName(QStringLiteral("TimeWarpStudio"));

    const Invocation invocation = timewarp::cli::parse("time_warp_ai", argc, argv);
    constexpr std::array<Command, 9> kCommands{{
        {"ask", "ask a question about a program", run_ask},
        {"explain", "explain what a program does", run_explain},
        {"fix", "find and correct what is wrong", run_fix},
        {"comment", "comment a program without changing it", run_comment},
        {"translate", "rewrite a program in another language", run_translate},
        {"write", "write a new program from a description", run_write},
        {"models", "list the models the endpoint can run", run_models},
        {"check", "say whether the assistant is configured", run_check},
        {"config", "print the configuration, never the key", run_config},
    }};

    if (invocation.help) {
        print_help(std::cout, invocation, kCommands);
        return 0;
    }
    if (!invocation.ok()) {
        return usage_error(invocation, invocation.error, kCommands);
    }
    for (const Command& command : kCommands) {
        if (command.name == invocation.command) {
            return command.run(invocation);
        }
    }
    return usage_error(invocation, "no such command: " + invocation.command, kCommands);
}
