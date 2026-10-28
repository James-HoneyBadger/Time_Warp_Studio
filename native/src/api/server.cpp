// SPDX-License-Identifier: MIT

#include "timewarp/api/server.hpp"

#include "timewarp/api/debug.hpp"

#include <QByteArray>
#include <QDir>
#include <QDirIterator>
#include <QFileInfo>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonParseError>
#include <QJsonValue>
#include <QString>
#include <QStringList>

#include <algorithm>
#include <cstddef>
#include <istream>
#include <map>
#include <ostream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "timewarp/graphics/recording_host.hpp"
#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/language.hpp"
#include "timewarp/lang/repl.hpp"
#include "timewarp/lang/watch.hpp"
#include "timewarp/project/project.hpp"
#include "timewarp/scheme/machine.hpp"
#include "timewarp/scheme/printer.hpp"
#include "timewarp/sql/database.hpp"

#if TIME_WARP_HAVE_AI
#include "timewarp/ai/client.hpp"
#include "timewarp/ai/config.hpp"
#include "timewarp/ai/prompt.hpp"

#include <QCoreApplication>
#include <QEventLoop>
#include <QTimer>
#endif

#ifndef TIME_WARP_VERSION
#define TIME_WARP_VERSION "0.0.0"
#endif

namespace timewarp::api {

namespace {

constexpr auto kApiVersion = "1.0";

struct ApiError {
    int code = -32603;
    std::string message;
    std::string detail;
};

[[noreturn]] void invalid(const std::string& message) {
    throw ApiError{-32602, message, {}};
}

[[noreturn]] void failed(const std::string& message) {
    throw ApiError{-32000, message, {}};
}

QString qstring(const QJsonObject& object, const char* key, bool required = true) {
    const QJsonValue value = object.value(QLatin1String{key});
    if (value.isUndefined() || value.isNull()) {
        if (required) {
            invalid(std::string{"missing string parameter '"} + key + "'");
        }
        return {};
    }
    if (!value.isString()) {
        invalid(std::string{"parameter '"} + key + "' must be a string");
    }
    return value.toString();
}

std::string text(const QJsonObject& object, const char* key, bool required = true) {
    return qstring(object, key, required).toStdString();
}

QJsonArray splitLines(const std::string& output) {
    QJsonArray lines;
    std::size_t start = 0;
    while (start < output.size()) {
        const std::size_t end = output.find('\n', start);
        const std::size_t length =
            end == std::string::npos ? output.size() - start : end - start;
        lines.append(QString::fromUtf8(output.data() + start,
                                       static_cast<int>(length)));
        if (end == std::string::npos) {
            break;
        }
        start = end + 1;
    }
    return lines;
}

QJsonArray variablesJson(const std::map<std::string, lang::Value>& variables) {
    QJsonArray rows;
    for (const auto& [name, value] : variables) {
        QJsonObject row;
        row.insert(QStringLiteral("name"), QString::fromStdString(name));
        row.insert(QStringLiteral("type"), QString::fromLatin1(value.type_name()));
        row.insert(QStringLiteral("value"), QString::fromStdString(value.render()));
        rows.append(row);
    }
    return rows;
}

QJsonArray schemeVariablesJson(const scheme::Machine& machine) {
    QJsonArray rows;
    for (const scheme::VariableEntry& entry : machine.visible_variables()) {
        QJsonObject row;
        row.insert(QStringLiteral("name"), QString::fromStdString(entry.name));
        row.insert(QStringLiteral("type"), QString::fromStdString(entry.kind));
        row.insert(QStringLiteral("value"), QString::fromStdString(entry.rendered));
        rows.append(row);
    }
    return rows;
}

QJsonObject drawingJson(const graphics::RecordingHost& host) {
    QJsonObject drawing;
    drawing.insert(QStringLiteral("commands"),
                   static_cast<double>(host.drawingCalls()));
    // Turns and pen changes have no ink but are still observable turtle actions;
    // a REPL test needs them to tell "accepted" from "ignored".
    drawing.insert(QStringLiteral("actions"),
                   static_cast<double>(host.actionCount()));
    // A program that drew nothing has no picture; a blank SVG would look like a
    // drawing that failed rather than a program that never used the turtle.
    drawing.insert(QStringLiteral("svg"), host.drew()
                                               ? QString::fromStdString(host.svg())
                                               : QString{});
    return drawing;
}

QJsonObject sqlResultJson(const sql::QueryResult& result) {
    QJsonObject object;
    QJsonArray columns;
    for (const QString& column : result.columns) {
        columns.append(column);
    }
    QJsonArray rows;
    for (const QStringList& row : result.rows) {
        QJsonArray values;
        for (const QString& value : row) {
            values.append(value);
        }
        rows.append(values);
    }
    object.insert(QStringLiteral("columns"), columns);
    object.insert(QStringLiteral("rows"), rows);
    object.insert(QStringLiteral("changes"), result.changes);
    object.insert(QStringLiteral("truncated"), result.truncated);
    object.insert(QStringLiteral("error"),
                  result.ok() ? QJsonValue{} : QJsonValue{result.error});
    return object;
}

std::string utf8Bytes(const QJsonObject& params, const char* key, const char* base64_key) {
    const QJsonValue encoded = params.value(QLatin1String{base64_key});
    if (encoded.isString()) {
        const QByteArray bytes =
            QByteArray::fromBase64(encoded.toString().toLatin1());
        return std::string{bytes.constData(), static_cast<std::size_t>(bytes.size())};
    }
    return text(params, key);
}

}  // namespace

struct Server::State {
    struct LangRepl {
        explicit LangRepl(lang::Language language) : session{language, host} {}
        graphics::RecordingHost host;
        lang::ReplSession session;
    };
    struct SchemeRepl {
        SchemeRepl() : machine{arena, host} {}
        graphics::RecordingHost host;
        scheme::Arena arena;
        scheme::Machine machine;
        std::size_t emitted = 0;
    };
    struct Repl {
        bool scheme = false;
        std::unique_ptr<LangRepl> lang;
        std::unique_ptr<SchemeRepl> scheme_state;
    };

    int next_session = 1;
    std::map<std::string, std::unique_ptr<Repl>> repls;
    std::map<std::string, std::unique_ptr<debug::Session>> debug_sessions;
    int next_debug = 1;
    sql::Database database;
    project::Project project;
    bool shutdown = false;

    QJsonValue dispatch(const QString& method, const QJsonObject& params);

    Repl& repl(const QJsonObject& params) {
        const std::string id = text(params, "session");
        const auto found = repls.find(id);
        if (found == repls.end()) {
            failed("no such REPL session: " + id);
        }
        return *found->second;
    }
};

Server::Server(std::istream& in, std::ostream& out)
    : state_{std::make_unique<State>()}, in_{in}, out_{out} {}

Server::~Server() = default;

namespace {

QJsonObject debugSnapshotJson(const std::string& id, const debug::Snapshot& snapshot) {
    QJsonObject object;
    object.insert(QStringLiteral("session"), QString::fromStdString(id));
    object.insert(QStringLiteral("paused"), snapshot.paused);
    object.insert(QStringLiteral("finished"), snapshot.finished);
    if (snapshot.paused) {
        object.insert(QStringLiteral("state"), QStringLiteral("paused"));
    } else if (snapshot.finished) {
        object.insert(QStringLiteral("state"), QStringLiteral("finished"));
    } else {
        object.insert(QStringLiteral("state"), QStringLiteral("timeout"));
    }
    object.insert(QStringLiteral("line"), snapshot.line);
    object.insert(QStringLiteral("depth"), snapshot.depth);
    object.insert(QStringLiteral("procedure"), QString::fromStdString(snapshot.procedure));
    QJsonArray stack;
    for (const debug::Frame& frame : snapshot.stack) {
        QJsonObject row;
        row.insert(QStringLiteral("procedure"),
                   QString::fromStdString(frame.procedure));
        row.insert(QStringLiteral("line"), frame.line);
        stack.append(row);
    }
    object.insert(QStringLiteral("stack"), stack);
    object.insert(QStringLiteral("reason"), QString::fromStdString(snapshot.reason));
    object.insert(QStringLiteral("changedVariable"),
                  QString::fromStdString(snapshot.changed_variable));
    object.insert(QStringLiteral("error"), QString::fromStdString(snapshot.error));
    object.insert(QStringLiteral("output"), QString::fromStdString(snapshot.output));
    QJsonArray variables;
    for (const debug::Variable& variable : snapshot.variables) {
        QJsonObject row;
        row.insert(QStringLiteral("name"), QString::fromStdString(variable.name));
        row.insert(QStringLiteral("type"), QString::fromStdString(variable.type));
        row.insert(QStringLiteral("value"), QString::fromStdString(variable.value));
        variables.append(row);
    }
    object.insert(QStringLiteral("variables"), variables);
    QJsonObject drawing;
    drawing.insert(QStringLiteral("commands"),
                   static_cast<double>(snapshot.drawing_commands));
    drawing.insert(QStringLiteral("svg"), QString::fromStdString(snapshot.svg));
    object.insert(QStringLiteral("drawing"), drawing);
    return object;
}

QJsonObject projectJson(const project::Project& project) {
    QJsonObject object;
    object.insert(QStringLiteral("name"), project.name());
    object.insert(QStringLiteral("root"), project.root());
    object.insert(QStringLiteral("filePath"), project.filePath());
    // What the IDE's Run Project runs: an absolute path, or null when the project
    // has no entry point. Reported so a client can run the same program rather
    // than guess which file the project means.
    object.insert(QStringLiteral("entry"),
                  project.hasEntry() ? QJsonValue{project.entry()} : QJsonValue{});
    QJsonArray groups;
    for (const QString& group : project.groups()) {
        groups.append(group);
    }
    object.insert(QStringLiteral("groups"), groups);

    QJsonArray files;
    for (const project::ProjectFile& file : project.files()) {
        QJsonObject entry;
        entry.insert(QStringLiteral("path"), file.path);
        entry.insert(QStringLiteral("group"), file.group);
        entry.insert(QStringLiteral("exists"), QFileInfo::exists(file.path));
        files.append(entry);
    }
    object.insert(QStringLiteral("files"), files);
    QJsonArray missing;
    for (const QString& path : project.missingFiles()) {
        missing.append(path);
    }
    object.insert(QStringLiteral("missing"), missing);
    return object;
}

QJsonObject languageInfo(lang::Language language) {
    QJsonObject object;
    const std::string_view key = lang::key(language);
    const std::string_view display = lang::display_name(language);
    object.insert(QStringLiteral("key"), QString::fromLatin1(key.data(),
                                                              static_cast<int>(key.size())));
    object.insert(QStringLiteral("display"),
                  QString::fromLatin1(display.data(), static_cast<int>(display.size())));
    object.insert(QStringLiteral("wholeProgram"), lang::is_whole_program(language));
    return object;
}

#if TIME_WARP_HAVE_AI
/// The API process has no event loop of its own -- `main` is stdio only -- so the one
/// method that speaks a network protocol makes what it needs.
///
/// Deliberately leaked rather than a static object: Qt's application object destroys
/// the network stack's process-wide state on the way out, and a `time_warp_api` that
/// then ran its own static destructors after it crashed at exit with a correct answer
/// already written. One allocation for the life of the process is the smaller cost.
QCoreApplication* coreApplication() {
    if (QCoreApplication::instance() != nullptr) {
        return QCoreApplication::instance();
    }
    static int argc = 1;
    static char program[] = "time_warp_api";
    static char* argv[] = {program, nullptr};
    static QCoreApplication* application = new QCoreApplication{argc, argv};
    return application;
}

/// `ai.ask`: one question to the configured assistant, answered before the response
/// line is written.
///
/// Synchronous because the protocol is: one request line in, one response line out.
/// The client is a Qt object that reports through signals, so this runs a local event
/// loop and takes the first of `answered` and `failed` -- the same two outcomes the
/// Assistant page's transcript is built from.
QJsonValue askAssistant(const QJsonObject& params) {
    const QString prompt = qstring(params, "prompt");
    if (prompt.trimmed().isEmpty()) {
        invalid("'prompt' must not be empty");
    }
    // The parameters are read before the configuration is: a caller that mistyped a
    // parameter learns that, rather than "the assistant is switched off", which
    // would send them looking at the wrong thing.
    ai::Conversation conversation;
    conversation.system = params.contains(QStringLiteral("system"))
                              ? qstring(params, "system")
                              : ai::system_prompt();
    const QJsonValue history_value = params.value(QStringLiteral("history"));
    if (!history_value.isUndefined() && !history_value.isNull() &&
        !history_value.isArray()) {
        invalid("'history' must be an array of {role, content} objects");
    }
    for (const QJsonValue& value : history_value.toArray()) {
        if (!value.isObject()) {
            invalid("'history' must be an array of {role, content} objects");
        }
        const QJsonObject turn = value.toObject();
        const QString role = qstring(turn, "role");
        if (role != QLatin1String("user") && role != QLatin1String("assistant")) {
            invalid("a history turn's role must be 'user' or 'assistant'");
        }
        conversation.messages.append(ai::Message{role, qstring(turn, "content")});
    }
    conversation.messages.append(ai::Message{QStringLiteral("user"), prompt});

    const ai::Config config = ai::load();
    QString why;
    if (!ai::ready(config, &why)) {
        failed(why.toStdString());
    }

    coreApplication();
    const QString key = ai::effective_key(config);
    ai::Client client;
    ai::Reply answer;
    QEventLoop loop;
    QObject::connect(&client, &ai::Client::answered, &loop,
                     [&answer, &loop](const ai::Reply& reply) {
                         answer = reply;
                         loop.quit();
                     });
    QObject::connect(&client, &ai::Client::failed, &loop,
                     [&answer, &loop](const QString& message) {
                         answer.error = message;
                         loop.quit();
                     });
    // The request carries its own transfer timeout; this is the second net, for a
    // transport that accepts the request and then reports nothing at all.
    QTimer::singleShot((config.timeout_seconds + 5) * 1000, &loop, [&loop] { loop.quit(); });
    client.send(config, conversation);
    loop.exec();

    if (!answer.ok) {
        const QString message = answer.error.isEmpty()
                                    ? QStringLiteral("the assistant did not answer")
                                    : answer.error;
        failed(ai::redact(message, key).toStdString());
    }
    QJsonObject result;
    result.insert(QStringLiteral("text"), answer.text);
    result.insert(QStringLiteral("model"), answer.model);
    result.insert(QStringLiteral("promptTokens"), answer.prompt_tokens);
    result.insert(QStringLiteral("completionTokens"), answer.completion_tokens);
    return result;
}
#endif

}  // namespace

QJsonValue Server::State::dispatch(const QString& method, const QJsonObject& params) {
    if (method == QLatin1String("studio.info")) {
        QJsonObject info;
        info.insert(QStringLiteral("apiVersion"), QLatin1String{kApiVersion});
        info.insert(QStringLiteral("version"), QLatin1String{TIME_WARP_VERSION});
        QJsonArray languages;
        for (const lang::Language language : lang::all_languages()) {
            languages.append(languageInfo(language));
        }
        info.insert(QStringLiteral("languages"), languages);
        QJsonObject features;
        features.insert(QStringLiteral("repl"), true);
        features.insert(QStringLiteral("sql"), true);
        features.insert(QStringLiteral("projects"), true);
        features.insert(QStringLiteral("watchExpressions"), true);
        features.insert(QStringLiteral("dataBreakpoints"), true);
        features.insert(QStringLiteral("debugger"), true);
        features.insert(QStringLiteral("compiledPrograms"), true);
#if TIME_WARP_HAVE_AI
        features.insert(QStringLiteral("assistant"), true);
#else
        features.insert(QStringLiteral("assistant"), false);
#endif
        info.insert(QStringLiteral("features"), features);
        return info;
    }
    if (method == QLatin1String("ping")) {
        return QStringLiteral("pong");
    }
    if (method == QLatin1String("studio.shutdown")) {
        shutdown = true;
        return true;
    }

    if (method == QLatin1String("ai.ask")) {
#if TIME_WARP_HAVE_AI
        return askAssistant(params);
#else
        // Present in every build so a client can tell "no assistant here" from
        // "unknown method": a script that asks should learn why it cannot.
        failed("this build has no AI assistant (Qt Network was not found)");
#endif
    }

    if (method == QLatin1String("language.run")) {
        const std::string key = text(params, "language");
        const std::string source_text = utf8Bytes(params, "source", "sourceBase64");
        const lang::Language language = key == "scheme"
                                            ? lang::Language::Scheme
                                            : lang::language_from_key(key).value_or(
                                                  lang::Language::Scheme);
        if (key != "scheme" && !lang::language_from_key(key).has_value()) {
            invalid("unknown language '" + key + "'");
        }
        if (key != "scheme" && lang::language_from_key(key) == lang::Language::Scheme) {
            invalid("use language 'scheme'");
        }

        QJsonArray inputs = params.value(QStringLiteral("inputs")).toArray();
        std::size_t next_input = 0;
        graphics::RecordingHost host;
        host.set_sql_runner([this](std::string_view sql) {
            return database.executeForOutput(sql);
        });

        QJsonObject result;
        if (language == lang::Language::Scheme) {
            scheme::Arena arena;
            scheme::Machine machine{arena, host};
            const std::string output = machine.execute(source_text);
            result.insert(QStringLiteral("output"), QString::fromStdString(output));
            result.insert(QStringLiteral("lines"), splitLines(output));
            result.insert(QStringLiteral("error"), output.rfind("\u274C", 0) == 0
                                                       ? QString::fromStdString(output)
                                                       : QJsonValue{});
            result.insert(QStringLiteral("variables"), schemeVariablesJson(machine));
            result.insert(QStringLiteral("drawing"), drawingJson(host));
            result.insert(QStringLiteral("telemetry"), QJsonObject{});
        } else {
            lang::Interpreter interpreter{language, host};
            interpreter.set_sql_callback([this](std::string_view sql) {
                return database.executeForOutput(sql);
            });
            interpreter.set_output_callback([](const std::string&) {});
            interpreter.set_input_callback(
                [&inputs, &next_input](const std::string&) {
                    if (next_input >= static_cast<std::size_t>(inputs.size())) {
                        return std::string{};
                    }
                    return inputs.at(static_cast<int>(next_input++)).toString().toStdString();
                });
            interpreter.load_program(source_text);
            interpreter.run();

            std::string output;
            for (const std::string& line : interpreter.output()) {
                output += line;
                output.push_back('\n');
            }
            result.insert(QStringLiteral("output"), QString::fromStdString(output));
            result.insert(QStringLiteral("lines"), splitLines(output));
            result.insert(QStringLiteral("error"),
                          QString::fromStdString(interpreter.error()));
            result.insert(QStringLiteral("variables"), variablesJson(interpreter.variables()));
            result.insert(QStringLiteral("drawing"), drawingJson(host));
            QJsonObject telemetry;
            telemetry.insert(QStringLiteral("steps"),
                             static_cast<double>(interpreter.steps_used()));
            telemetry.insert(QStringLiteral("ms"), interpreter.elapsed_ms());
            telemetry.insert(QStringLiteral("timedOut"), interpreter.timed_out());
            telemetry.insert(QStringLiteral("iterationCapped"),
                             interpreter.iteration_capped());
            result.insert(QStringLiteral("telemetry"), telemetry);
        }
        return result;
    }

    if (method == QLatin1String("repl.create")) {
        const std::string key = QString::fromStdString(text(params, "language"))
                                    .trimmed()
                                    .toLower()
                                    .toStdString();
        if (key != "scheme" && !lang::language_from_key(key).has_value()) {
            invalid("unknown language '" + key + "'");
        }
        auto session = std::make_unique<State::Repl>();
        if (key == "scheme" || lang::language_from_key(key) == lang::Language::Scheme) {
            session->scheme = true;
            session->scheme_state = std::make_unique<State::SchemeRepl>();
            session->scheme_state->host.set_sql_runner([this](std::string_view sql) {
                return database.executeForOutput(sql);
            });
        } else {
            session->lang = std::make_unique<State::LangRepl>(*lang::language_from_key(key));
            session->lang->host.set_sql_runner([this](std::string_view sql) {
                return database.executeForOutput(sql);
            });
            session->lang->session.set_sql_callback([this](std::string_view sql) {
                return database.executeForOutput(sql);
            });
        }
        const std::string id = std::to_string(next_session++);
        repls.emplace(id, std::move(session));
        QJsonObject result;
        result.insert(QStringLiteral("session"), QString::fromStdString(id));
        result.insert(QStringLiteral("language"), QString::fromStdString(key));
        return result;
    }

    if (method == QLatin1String("repl.eval")) {
        State::Repl& session = repl(params);
        const std::string source = utf8Bytes(params, "source", "sourceBase64");
        QJsonObject result;
        std::string output;
        std::string error;
        if (session.scheme) {
            State::SchemeRepl& machine = *session.scheme_state;
            const std::string returned = machine.machine.execute(source);
            const std::string& all = machine.machine.output();
            if (all.size() >= machine.emitted) {
                output = all.substr(machine.emitted);
                machine.emitted = all.size();
            }
            const bool bad = returned.rfind("\u274C", 0) == 0;
            if (output.empty() && bad) {
                output = returned;
            }
            const scheme::Value value = machine.machine.last_value();
            if (!bad && value.type != scheme::ValueType::Unspecified) {
                if (!output.empty() && output.back() != '\n') {
                    output.push_back('\n');
                }
                output += scheme::write_to_string(value);
                output.push_back('\n');
            }
            if (bad) {
                error = returned;
            }
            result.insert(QStringLiteral("variables"),
                          schemeVariablesJson(machine.machine));
        } else {
            output = session.lang->session.execute(source);
            error = session.lang->session.error();
            result.insert(QStringLiteral("variables"),
                          variablesJson(session.lang->session.variables()));
        }
        result.insert(QStringLiteral("output"), QString::fromStdString(output));
        result.insert(QStringLiteral("lines"), splitLines(output));
        result.insert(QStringLiteral("error"), QString::fromStdString(error));
        result.insert(QStringLiteral("drawing"),
                      drawingJson(session.scheme ? session.scheme_state->host
                                                 : session.lang->host));
        return result;
    }

    if (method == QLatin1String("repl.reset")) {
        State::Repl& session = repl(params);
        if (session.scheme) {
            session.scheme_state = std::make_unique<State::SchemeRepl>();
            session.scheme_state->host.set_sql_runner([this](std::string_view sql) {
                return database.executeForOutput(sql);
            });
        } else {
            session.lang->session.reset();
            session.lang->session.set_sql_callback([this](std::string_view sql) {
                return database.executeForOutput(sql);
            });
        }
        return true;
    }

    if (method == QLatin1String("repl.close")) {
        const std::string id = text(params, "session");
        if (repls.erase(id) == 0) {
            failed("no such REPL session: " + id);
        }
        return true;
    }

    if (method == QLatin1String("sql.open")) {
        QString path = qstring(params, "path");
        if (path != QLatin1String(":memory:") && !QFileInfo{path}.isAbsolute()) {
            // A relative path would be relative to whatever directory the API
            // process happens to have been started in, which a caller cannot see.
            path = QFileInfo{path}.absoluteFilePath();
        }
        QString error;
        if (!database.open(path, &error)) {
            failed(error.toStdString());
        }
        QJsonObject result;
        result.insert(QStringLiteral("path"), path);
        QJsonArray tables;
        for (const QString& table : database.tables()) {
            tables.append(table);
        }
        result.insert(QStringLiteral("tables"), tables);
        return result;
    }
    if (method == QLatin1String("sql.close")) {
        database.close();
        return true;
    }
    if (method == QLatin1String("sql.query")) {
        const QString sql = qstring(params, "sql");
        return sqlResultJson(database.execute(sql));
    }
    if (method == QLatin1String("sql.tables")) {
        QJsonArray tables;
        for (const QString& table : database.tables()) {
            tables.append(table);
        }
        return tables;
    }
    if (method == QLatin1String("sql.schema")) {
        return database.schema(qstring(params, "table"));
    }

    if (method == QLatin1String("debug.start")) {
        const std::string key = QString::fromStdString(text(params, "language"))
                                    .trimmed()
                                    .toLower()
                                    .toStdString();
        const std::optional<lang::Language> language =
            key == "scheme" ? std::optional<lang::Language>{lang::Language::Scheme}
                            : lang::language_from_key(key);
        if (!language.has_value() || key.empty()) {
            invalid("unknown language '" + key + "'");
        }
        std::set<int> breakpoints;
        for (const QJsonValue& value : params.value(QStringLiteral("breakpoints")).toArray()) {
            if (!value.isDouble()) {
                invalid("'breakpoints' must be an array of line numbers");
            }
            const int line = value.toInt();
            if (line > 0) {
                breakpoints.insert(line);
            }
        }
        std::set<std::string> data_breakpoints;
        for (const QJsonValue& value :
             params.value(QStringLiteral("dataBreakpoints")).toArray()) {
            if (!value.isString()) {
                invalid("'dataBreakpoints' must be an array of variable names");
            }
            const QString name = value.toString().trimmed();
            if (!name.isEmpty()) {
                data_breakpoints.insert(name.toStdString());
            }
        }
        const std::string source = utf8Bytes(params, "source", "sourceBase64");
        // How many snapshots the run keeps, so `debug.rewind` has somewhere to go.
        // Zero is the default and costs nothing: a client that does not ask for
        // history pays no copy per statement.
        std::size_t history = 0;
        if (params.contains(QStringLiteral("history"))) {
            const QJsonValue value = params.value(QStringLiteral("history"));
            if (!value.isDouble() || value.toDouble() < 0 || value.toDouble() > 10000) {
                invalid("'history' must be a number of snapshots between 0 and 10000");
            }
            history = static_cast<std::size_t>(value.toInt());
        }
        auto session = debug::Session::start(
            *language, source, breakpoints, data_breakpoints,
            [this](std::string_view sql) { return database.executeForOutput(sql); },
            history);
        const std::string id = std::to_string(next_debug++);
        QJsonObject result = debugSnapshotJson(id, session->snapshot());
        debug_sessions.emplace(id, std::move(session));
        return result;
    }

    if (method == QLatin1String("debug.snapshot")) {
        const std::string id = text(params, "session");
        const auto found = debug_sessions.find(id);
        if (found == debug_sessions.end()) {
            failed("no such debug session: " + id);
        }
        return debugSnapshotJson(id, found->second->snapshot());
    }

    if (method == QLatin1String("debug.continue")) {
        const std::string id = text(params, "session");
        const auto found = debug_sessions.find(id);
        if (found == debug_sessions.end()) {
            failed("no such debug session: " + id);
        }
        return debugSnapshotJson(id, found->second->resume());
    }

    if (method == QLatin1String("debug.step")) {
        const std::string id = text(params, "session");
        const auto found = debug_sessions.find(id);
        if (found == debug_sessions.end()) {
            failed("no such debug session: " + id);
        }
        const std::string kind = text(params, "kind", false);
        const int mode = kind.empty() || kind == "into" ? 0 : (kind == "over" ? 1 : 2);
        if (!kind.empty() && kind != "into" && kind != "over" && kind != "out") {
            invalid("'kind' must be into, over or out");
        }
        return debugSnapshotJson(id, found->second->step(mode));
    }

    if (method == QLatin1String("debug.rewind")) {
        const std::string id = text(params, "session");
        const auto found = debug_sessions.find(id);
        if (found == debug_sessions.end()) {
            failed("no such debug session: " + id);
        }
        const QJsonValue value = params.value(QStringLiteral("steps"));
        const double steps = value.isUndefined() ? 1.0 : value.toDouble();
        if (steps < 1 || steps > 10000) {
            invalid("'steps' must be between 1 and 10000");
        }
        const debug::Snapshot rewound =
            found->second->rewind(static_cast<std::size_t>(steps));
        QJsonObject result = debugSnapshotJson(id, rewound);
        if (!rewound.error.empty()) {
            // The session kept no history, or the run had already finished: the state
            // is unmoved and the answer says why rather than looking like a rewind
            // that happened to land where the run already was.
            result.insert(QStringLiteral("unmoved"), true);
        }
        return result;
    }

    if (method == QLatin1String("debug.stop")) {
        const std::string id = text(params, "session");
        const auto found = debug_sessions.find(id);
        if (found == debug_sessions.end()) {
            failed("no such debug session: " + id);
        }
        return debugSnapshotJson(id, found->second->stop());
    }

    if (method == QLatin1String("debug.close")) {
        const std::string id = text(params, "session");
        const auto found = debug_sessions.find(id);
        if (found == debug_sessions.end()) {
            failed("no such debug session: " + id);
        }
        static_cast<void>(found->second->stop());
        debug_sessions.erase(found);
        return true;
    }

    if (method == QLatin1String("project.create")) {
        const QString root = qstring(params, "root");
        const QString name = qstring(params, "name", false);
        const QString file = qstring(params, "filePath", false);
        project::Project created = project::Project::create(name, root, file);
        QString error;
        if (!created.save(&error)) {
            failed(error.toStdString());
        }
        project = std::move(created);
        return projectJson(project);
    }
    if (method == QLatin1String("project.open")) {
        project::Project loaded;
        QString error;
        if (!loaded.load(qstring(params, "path"), &error)) {
            failed(error.toStdString());
        }
        project = std::move(loaded);
        return projectJson(project);
    }
    if (method == QLatin1String("project.save")) {
        QString error;
        if (!project.save(&error)) {
            failed(error.toStdString());
        }
        return projectJson(project);
    }
    if (method == QLatin1String("project.info")) {
        if (!project.isValid()) {
            failed("no project is open");
        }
        return projectJson(project);
    }
    if (method == QLatin1String("project.addFile")) {
        if (!project.addFile(qstring(params, "path"), qstring(params, "group", false))) {
            failed("the file is already a member or the path is empty");
        }
        return projectJson(project);
    }
    if (method == QLatin1String("project.setEntry")) {
        if (!project.isValid()) {
            failed("no project is open");
        }
        if (!project.setEntry(qstring(params, "path"))) {
            failed("the entry point could not be set");
        }
        return projectJson(project);
    }
    if (method == QLatin1String("project.clearEntry")) {
        if (!project.clearEntry()) {
            failed("the project has no entry point");
        }
        return projectJson(project);
    }
    if (method == QLatin1String("project.addFiles")) {
        QStringList paths;
        for (const QJsonValue& value : params.value(QStringLiteral("paths")).toArray()) {
            if (value.isString()) {
                paths.append(value.toString());
            }
        }
        if (paths.isEmpty()) {
            invalid("'paths' must be a non-empty array of strings");
        }
        project.addFiles(paths, qstring(params, "group", false));
        return projectJson(project);
    }
    if (method == QLatin1String("project.addFolder")) {
        if (!project.isValid()) {
            failed("no project is open");
        }
        const QString folder = qstring(params, "path");
        const QFileInfo folder_info{folder};
        if (!folder_info.exists() || !folder_info.isDir()) {
            invalid("'path' must be an existing directory");
        }
        QStringList extensions;
        for (const QJsonValue& value : params.value(QStringLiteral("extensions")).toArray()) {
            if (value.isString()) {
                QString extension = value.toString().trimmed();
                if (extension.startsWith(QLatin1Char('.'))) {
                    extension.remove(0, 1);
                }
                if (!extension.isEmpty()) {
                    extensions.append(extension);
                }
            }
        }
        if (extensions.isEmpty()) {
            invalid("'extensions' must be a non-empty array of file suffixes");
        }
        const QString prefix = qstring(params, "group", false);
        const QString base = QDir::cleanPath(folder_info.absoluteFilePath());
        int added = 0;
        QDirIterator iterator{base, QDir::Files, QDirIterator::Subdirectories};
        while (iterator.hasNext()) {
            const QString path = iterator.next();
            if (!extensions.contains(QFileInfo{path}.suffix(), Qt::CaseInsensitive)) {
                continue;
            }
            QString group = QFileInfo{QDir{base}.relativeFilePath(path)}.path();
            if (group == QLatin1String(".")) {
                group.clear();
            }
            if (!prefix.isEmpty()) {
                group = group.isEmpty() ? prefix : prefix + QLatin1Char('/') + group;
            }
            if (project.addFile(path, group)) {
                ++added;
            }
        }
        QJsonObject result = projectJson(project);
        result.insert(QStringLiteral("added"), added);
        return result;
    }
    if (method == QLatin1String("project.removeFile")) {
        if (!project.removeFile(qstring(params, "path"))) {
            failed("the file is not a member of the project");
        }
        return projectJson(project);
    }
    if (method == QLatin1String("project.addGroup")) {
        if (!project.addGroup(qstring(params, "name"))) {
            failed("the group already exists or the name is empty");
        }
        return projectJson(project);
    }
    if (method == QLatin1String("project.renameGroup")) {
        if (!project.renameGroup(qstring(params, "from"), qstring(params, "to"))) {
            failed("the group does not exist or the new name is taken");
        }
        return projectJson(project);
    }
    if (method == QLatin1String("project.removeGroup")) {
        if (!project.removeGroup(qstring(params, "name"))) {
            failed("no such group");
        }
        return projectJson(project);
    }
    if (method == QLatin1String("project.moveFile")) {
        if (!project.setGroup(qstring(params, "path"), qstring(params, "group", false))) {
            failed("the file is not a member or is already in that group");
        }
        return projectJson(project);
    }

    if (method == QLatin1String("watch.evaluate")) {
        const QJsonArray expressions = params.value(QStringLiteral("expressions")).toArray();
        if (expressions.isEmpty()) {
            invalid("'expressions' must be a non-empty array of strings");
        }
        std::map<std::string, lang::Value> variables;
        const QJsonObject bindings = params.value(QStringLiteral("variables")).toObject();
        for (auto it = bindings.begin(); it != bindings.end(); ++it) {
            if (it.value().isBool()) {
                variables[it.key().toStdString()] =
                    lang::Value{it.value().toBool() ? 1.0 : 0.0};
            } else if (it.value().isString()) {
                variables[it.key().toStdString()] =
                    lang::Value{it.value().toString().toStdString()};
            } else if (it.value().isDouble()) {
                variables[it.key().toStdString()] = lang::Value{it.value().toDouble()};
            }
        }
        std::vector<std::string> wanted;
        for (const QJsonValue& value : expressions) {
            if (!value.isString()) {
                invalid("'expressions' must contain strings");
            }
            wanted.push_back(value.toString().toStdString());
        }
        QJsonArray results;
        for (const lang::WatchResult& watch : lang::evaluate_watches(wanted, variables)) {
            QJsonObject entry;
            entry.insert(QStringLiteral("expression"),
                         QString::fromStdString(watch.expression));
            entry.insert(QStringLiteral("value"), QString::fromStdString(watch.value));
            entry.insert(QStringLiteral("ok"), watch.ok);
            entry.insert(QStringLiteral("error"),
                         watch.ok ? QJsonValue{} : QJsonValue{QString::fromStdString(
                                         watch.error)});
            results.append(entry);
        }
        return results;
    }

    throw ApiError{-32601, "unknown method '" + method.toStdString() + "'", {}};
}

std::string Server::handleLine(const std::string& line) {
    const auto respond = [](QJsonValue id, QJsonValue result) {
        QJsonObject response;
        response.insert(QStringLiteral("jsonrpc"), QStringLiteral("2.0"));
        response.insert(QStringLiteral("id"), std::move(id));
        response.insert(QStringLiteral("result"), std::move(result));
        return QString::fromUtf8(QJsonDocument{response}.toJson(QJsonDocument::Compact))
            .toStdString();
    };
    const auto respondError = [](QJsonValue id, int code, const QString& message,
                                 const QString& detail) {
        QJsonObject error;
        error.insert(QStringLiteral("code"), code);
        error.insert(QStringLiteral("message"), message);
        if (!detail.isEmpty()) {
            error.insert(QStringLiteral("data"), detail);
        }
        QJsonObject response;
        response.insert(QStringLiteral("jsonrpc"), QStringLiteral("2.0"));
        response.insert(QStringLiteral("id"), std::move(id));
        response.insert(QStringLiteral("error"), error);
        return QString::fromUtf8(QJsonDocument{response}.toJson(QJsonDocument::Compact))
            .toStdString();
    };

    QJsonParseError parse_error{};
    const QJsonDocument document =
        QJsonDocument::fromJson(QByteArray::fromStdString(line), &parse_error);
    if (parse_error.error != QJsonParseError::NoError) {
        return respondError(QJsonValue{}, -32700,
                            QStringLiteral("Parse error"), parse_error.errorString());
    }
    if (document.isArray()) {
        const QJsonArray batch = document.array();
        if (batch.isEmpty()) {
            return respondError(QJsonValue{}, -32600, QStringLiteral("Invalid Request"),
                                QStringLiteral("a batch must contain at least one request"));
        }
        QJsonArray responses;
        for (const QJsonValue& value : batch) {
            if (!value.isObject()) {
                const std::string error =
                    respondError(QJsonValue{}, -32600, QStringLiteral("Invalid Request"),
                                 QStringLiteral("every batch entry must be an object"));
                responses.append(QJsonDocument::fromJson(QByteArray::fromStdString(error))
                                     .object());
                continue;
            }
            const std::string encoded =
                QString::fromUtf8(QJsonDocument{value.toObject()}.toJson(
                                      QJsonDocument::Compact))
                    .toStdString();
            const std::string response = handleLine(encoded);
            if (!response.empty()) {
                responses.append(
                    QJsonDocument::fromJson(QByteArray::fromStdString(response)).object());
            }
        }
        if (responses.isEmpty()) {
            return {};
        }
        return QString::fromUtf8(QJsonDocument{responses}.toJson(QJsonDocument::Compact))
            .toStdString();
    }
    if (!document.isObject()) {
        return respondError(QJsonValue{}, -32600, QStringLiteral("Invalid Request"),
                            QStringLiteral("the request must be a JSON object or array"));
    }
    const QJsonObject request = document.object();
    const QJsonValue id = request.value(QStringLiteral("id"));
    const bool notification = !request.contains(QStringLiteral("id"));
    if (!notification && !id.isNull() && !id.isString() && !id.isDouble()) {
        return respondError(QJsonValue{}, -32600, QStringLiteral("Invalid Request"),
                            QStringLiteral("id must be a string, number or null"));
    }
    if (request.value(QStringLiteral("jsonrpc")).toString() != QLatin1String("2.0")) {
        return notification
                   ? std::string{}
                   : respondError(id, -32600, QStringLiteral("Invalid Request"),
                                  QStringLiteral("jsonrpc must be \"2.0\""));
    }
    const QJsonValue method_value = request.value(QStringLiteral("method"));
    if (!method_value.isString()) {
        return notification ? std::string{}
                            : respondError(id, -32600, QStringLiteral("Invalid Request"),
                                           QStringLiteral("method must be a string"));
    }
    const QJsonValue params_value = request.value(QStringLiteral("params"));
    if (!params_value.isUndefined() && !params_value.isObject()) {
        return notification
                   ? std::string{}
                   : respondError(id, -32602, QStringLiteral("Invalid params"),
                                  QStringLiteral("params must be an object"));
    }
    const QJsonObject params = params_value.toObject();

    try {
        QJsonValue result = state_->dispatch(method_value.toString(), params);
        return notification ? std::string{} : respond(id, std::move(result));
    } catch (const ApiError& error) {
        if (notification) {
            return {};
        }
        return respondError(id, error.code, QString::fromStdString(error.message),
                            QString::fromStdString(error.detail));
    } catch (const std::exception& error) {
        if (notification) {
            return {};
        }
        return respondError(id, -32603, QStringLiteral("Internal error"),
                            QString::fromUtf8(error.what()));
    }
}

int Server::run() {
    std::string line;
    while (!state_->shutdown && std::getline(in_, line)) {
        if (line.empty()) {
            continue;
        }
        // A request is one line. A caller that writes a megabyte by accident gets
        // an error rather than an allocation the size of its mistake.
        if (line.size() > 8u * 1024u * 1024u) {
            out_ << R"({"jsonrpc":"2.0","id":null,"error":{"code":-32600,"message":"Request too large"}})"
                 << '\n';
            out_.flush();
            continue;
        }
        const std::string response = handleLine(line);
        if (!response.empty()) {
            out_ << response << '\n';
            out_.flush();
        }
    }
    return 0;
}

}  // namespace timewarp::api
