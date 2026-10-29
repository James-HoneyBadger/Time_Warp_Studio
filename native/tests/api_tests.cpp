// SPDX-License-Identifier: MIT
//
// The automation API: requests in, responses out, and the errors a caller has to
// be able to tell apart.
//
// The server is driven through a string stream rather than a process, so the
// transport is not what is under test -- the methods, their validation and the
// JSON-RPC envelope are.

#include <cstdio>

#include <QFileInfo>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QString>

#include <sstream>

#include "check.hpp"
#include "temp_dir.hpp"
#include "timewarp/api/server.hpp"

namespace {

using timewarp::api::Server;

QJsonObject request(const QString& method, const QJsonObject& params = {}) {
    QJsonObject object;
    object.insert(QStringLiteral("jsonrpc"), QStringLiteral("2.0"));
    object.insert(QStringLiteral("id"), 1);
    object.insert(QStringLiteral("method"), method);
    if (!params.isEmpty()) {
        object.insert(QStringLiteral("params"), params);
    }
    return object;
}

std::string lineOf(const QJsonObject& object) {
    return QString::fromUtf8(QJsonDocument{object}.toJson(QJsonDocument::Compact))
        .toStdString();
}

QJsonObject call(Server& server, const QString& method, const QJsonObject& params = {}) {
    const std::string line = server.handleLine(lineOf(request(method, params)));
    return QJsonDocument::fromJson(QByteArray::fromStdString(line)).object();
}

QJsonValue resultOf(Server& server, const QString& method, const QJsonObject& params = {}) {
    const QJsonObject response = call(server, method, params);
    if (response.contains(QStringLiteral("error"))) {
        std::printf("unexpected error: %s\n",
                    lineOf(response.value(QStringLiteral("error")).toObject()).c_str());
    }
    return response.value(QStringLiteral("result"));
}

void announce(const char* name) {
    std::printf("-- %s\n", name);
    std::fflush(stdout);
}

void test_info() {
    announce("studio.info");
    std::istringstream in;
    std::ostringstream out;
    Server server{in, out};
    const QJsonObject info = resultOf(server, QStringLiteral("studio.info")).toObject();
    TW_CHECK_EQ(info.value(QStringLiteral("apiVersion")).toString(),
                QStringLiteral("1.0"));
    TW_CHECK_EQ(info.value(QStringLiteral("languages")).toArray().size(), 11);
    const QJsonObject features = info.value(QStringLiteral("features")).toObject();
    TW_CHECK(features.value(QStringLiteral("sql")).toBool());
    TW_CHECK(features.value(QStringLiteral("projects")).toBool());
    TW_CHECK(features.value(QStringLiteral("repl")).toBool());
    TW_CHECK(features.value(QStringLiteral("debugger")).toBool());
}

void test_run_and_repl() {
    announce("language.run and repl.eval");
    std::istringstream in;
    std::ostringstream out;
    Server server{in, out};

    QJsonObject params;
    params.insert(QStringLiteral("language"), QStringLiteral("basic"));
    params.insert(QStringLiteral("source"),
                  QStringLiteral("10 LET X = 6 * 7\n20 PRINT X\n"));
    const QJsonObject run = resultOf(server, QStringLiteral("language.run"), params).toObject();
    TW_CHECK(run.value(QStringLiteral("output")).toString().contains(QStringLiteral("42")));
    TW_CHECK(run.value(QStringLiteral("error")).toString().isEmpty());
    const QJsonArray variables = run.value(QStringLiteral("variables")).toArray();
    TW_CHECK_EQ(variables.size(), 1);
    TW_CHECK_EQ(variables.at(0).toObject().value(QStringLiteral("name")).toString(),
                QStringLiteral("X"));

    QJsonObject create;
    create.insert(QStringLiteral("language"), QStringLiteral("basic"));
    const QString session =
        resultOf(server, QStringLiteral("repl.create"), create)
            .toObject()
            .value(QStringLiteral("session"))
            .toString();
    TW_CHECK(!session.isEmpty());

    QJsonObject first;
    first.insert(QStringLiteral("session"), session);
    first.insert(QStringLiteral("source"), QStringLiteral("LET X = 5"));
    TW_CHECK(resultOf(server, QStringLiteral("repl.eval"), first)
                 .toObject()
                 .value(QStringLiteral("error"))
                 .toString()
                 .isEmpty());
    QJsonObject second;
    second.insert(QStringLiteral("session"), session);
    second.insert(QStringLiteral("source"), QStringLiteral("PRINT X * 3"));
    const QJsonObject evaluated =
        resultOf(server, QStringLiteral("repl.eval"), second).toObject();
    TW_CHECK(evaluated.value(QStringLiteral("output")).toString().contains(
        QStringLiteral("15")));

    // Scheme is the same protocol with expression values echoed.
    // BASIC's NEW is a REPL command: it clears the session's bindings.
    QJsonObject new_command;
    new_command.insert(QStringLiteral("session"), session);
    new_command.insert(QStringLiteral("source"), QStringLiteral("new"));
    const QJsonObject cleared =
        resultOf(server, QStringLiteral("repl.eval"), new_command).toObject();
    TW_CHECK(cleared.value(QStringLiteral("output"))
                 .toString()
                 .contains(QStringLiteral("cleared")));
    QJsonObject after_new;
    after_new.insert(QStringLiteral("session"), session);
    after_new.insert(QStringLiteral("source"), QStringLiteral("PRINT X"));
    TW_CHECK(resultOf(server, QStringLiteral("repl.eval"), after_new)
                 .toObject()
                 .value(QStringLiteral("variables"))
                 .toArray()
                 .isEmpty());

    QJsonObject scheme;
    scheme.insert(QStringLiteral("language"), QStringLiteral("scheme"));
    const QString scheme_session =
        resultOf(server, QStringLiteral("repl.create"), scheme)
            .toObject()
            .value(QStringLiteral("session"))
            .toString();
    QJsonObject expression;
    expression.insert(QStringLiteral("session"), scheme_session);
    expression.insert(QStringLiteral("source"), QStringLiteral("(+ 20 22)"));
    const QJsonObject scheme_value =
        resultOf(server, QStringLiteral("repl.eval"), expression).toObject();
    TW_CHECK_EQ(scheme_value.value(QStringLiteral("output")).toString(),
                QStringLiteral("42\n"));

    // A definition survives into the next submission, and a reset drops it.
    QJsonObject define;
    define.insert(QStringLiteral("session"), scheme_session);
    define.insert(QStringLiteral("source"), QStringLiteral("(define y 4)"));
    resultOf(server, QStringLiteral("repl.eval"), define);
    QJsonObject use;
    use.insert(QStringLiteral("session"), scheme_session);
    use.insert(QStringLiteral("source"), QStringLiteral("(* y 5)"));
    TW_CHECK_EQ(resultOf(server, QStringLiteral("repl.eval"), use)
                    .toObject()
                    .value(QStringLiteral("output"))
                    .toString(),
                QStringLiteral("20\n"));

    QJsonObject reset;
    reset.insert(QStringLiteral("session"), scheme_session);
    TW_CHECK(resultOf(server, QStringLiteral("repl.reset"), reset).toBool());
    const QJsonObject after_reset =
        resultOf(server, QStringLiteral("repl.eval"), use).toObject();
    TW_CHECK(after_reset.value(QStringLiteral("error")).toString().contains(
        QStringLiteral("\u274C")));

    // Sessions do not share bindings.
    QJsonObject other;
    other.insert(QStringLiteral("language"), QStringLiteral("basic"));
    const QString other_session =
        resultOf(server, QStringLiteral("repl.create"), other)
            .toObject()
            .value(QStringLiteral("session"))
            .toString();
    QJsonObject inspect;
    inspect.insert(QStringLiteral("session"), other_session);
    inspect.insert(QStringLiteral("source"), QStringLiteral("PRINT X"));
    const QJsonObject isolated =
        resultOf(server, QStringLiteral("repl.eval"), inspect).toObject();
    TW_CHECK(isolated.value(QStringLiteral("variables")).toArray().isEmpty());

    QJsonObject close;
    close.insert(QStringLiteral("session"), other_session);
    TW_CHECK(resultOf(server, QStringLiteral("repl.close"), close).toBool());
    const QJsonObject gone = call(server, QStringLiteral("repl.eval"), inspect);
    TW_CHECK_EQ(gone.value(QStringLiteral("error"))
                    .toObject()
                    .value(QStringLiteral("code"))
                    .toInt(),
                -32000);
}

void test_drawing_reports_turtle_actions() {
    announce("drawing actions");
    std::istringstream in;
    std::ostringstream out;
    Server server{in, out};
    QJsonObject params;
    params.insert(QStringLiteral("language"), QStringLiteral("logo"));
    params.insert(QStringLiteral("source"), QStringLiteral("RIGHT 90"));
    const QJsonObject result =
        resultOf(server, QStringLiteral("language.run"), params).toObject();
    const QJsonObject drawing = result.value(QStringLiteral("drawing")).toObject();
    // A turn leaves no ink, but it is still a turtle operation the caller can
    // observe and a REPL test can use to tell acceptance from silence.
    TW_CHECK_EQ(drawing.value(QStringLiteral("commands")).toInt(), 0);
    TW_CHECK(drawing.value(QStringLiteral("actions")).toInt() > 0);
}

void test_sql() {
    announce("sql.*");
    std::istringstream in;
    std::ostringstream out;
    Server server{in, out};
    QJsonObject open;
    open.insert(QStringLiteral("path"), QStringLiteral(":memory:"));
    TW_CHECK(resultOf(server, QStringLiteral("sql.open"), open).isObject());

    QJsonObject create;
    create.insert(QStringLiteral("sql"),
                  QStringLiteral("CREATE TABLE t (name TEXT, score INTEGER);"));
    TW_CHECK(resultOf(server, QStringLiteral("sql.query"), create)
                 .toObject()
                 .value(QStringLiteral("changes"))
                 .toInt() == 0);
    QJsonObject insert;
    insert.insert(QStringLiteral("sql"),
                  QStringLiteral("INSERT INTO t VALUES ('Ada', 42);"));
    TW_CHECK_EQ(resultOf(server, QStringLiteral("sql.query"), insert)
                    .toObject()
                    .value(QStringLiteral("changes"))
                    .toInt(),
                1);

    QJsonObject select;
    select.insert(QStringLiteral("sql"), QStringLiteral("SELECT name, score FROM t;"));
    const QJsonObject result = resultOf(server, QStringLiteral("sql.query"), select).toObject();
    TW_CHECK_EQ(result.value(QStringLiteral("rows")).toArray().size(), 1);
    TW_CHECK_EQ(result.value(QStringLiteral("rows"))
                    .toArray()
                    .at(0)
                    .toArray()
                    .at(1)
                    .toString(),
                QStringLiteral("42"));

    QJsonObject bad;
    bad.insert(QStringLiteral("sql"), QStringLiteral("SELECT * FROM missing;"));
    TW_CHECK(!resultOf(server, QStringLiteral("sql.query"), bad)
                  .toObject()
                  .value(QStringLiteral("error"))
                  .toString()
                  .isEmpty());

    QJsonObject table;
    table.insert(QStringLiteral("table"), QStringLiteral("t"));
    TW_CHECK(resultOf(server, QStringLiteral("sql.schema"), table)
                 .toString()
                 .contains(QStringLiteral("CREATE TABLE")));

    // A program reaches the same connection: the API's database is what a `SQL`
    // statement in `language.run` sees.
    QJsonObject program;
    program.insert(QStringLiteral("language"), QStringLiteral("basic"));
    program.insert(QStringLiteral("source"), QStringLiteral("10 SQL SELECT name FROM t"));
    const QJsonObject run =
        resultOf(server, QStringLiteral("language.run"), program).toObject();
    TW_CHECK(run.value(QStringLiteral("output")).toString().contains(QStringLiteral("Ada")));
}

void test_project() {
    announce("project.*");
    tw::test::TempDir temp;
    if (!temp.valid()) {
        return;
    }
    std::istringstream in;
    std::ostringstream out;
    Server server{in, out};

    QJsonObject create;
    create.insert(QStringLiteral("name"), QStringLiteral("Class"));
    create.insert(QStringLiteral("root"), temp.path());
    const QJsonObject created =
        resultOf(server, QStringLiteral("project.create"), create).toObject();
    TW_CHECK_EQ(created.value(QStringLiteral("name")).toString(), QStringLiteral("Class"));

    QJsonObject group;
    group.insert(QStringLiteral("name"), QStringLiteral("Art"));
    TW_CHECK(resultOf(server, QStringLiteral("project.addGroup"), group)
                 .toObject()
                 .value(QStringLiteral("groups"))
                 .toArray()
                 .contains(QJsonValue{QStringLiteral("Art")}));

    const QString file = temp.write(QStringLiteral("one.logo"), "FD 10\n");
    QJsonObject add;
    add.insert(QStringLiteral("path"), file);
    add.insert(QStringLiteral("group"), QStringLiteral("Art"));
    const QJsonObject added =
        resultOf(server, QStringLiteral("project.addFile"), add).toObject();
    TW_CHECK_EQ(added.value(QStringLiteral("files")).toArray().size(), 1);
    TW_CHECK_EQ(added.value(QStringLiteral("files"))
                    .toArray()
                    .at(0)
                    .toObject()
                    .value(QStringLiteral("group"))
                    .toString(),
                QStringLiteral("Art"));

    // A folder adds every matching program, grouped by its subfolders, and does
    // not add the files that are not programs.
    temp.write(QStringLiteral("Art/two.bas"), "10 PRINT 1\n");
    temp.write(QStringLiteral("Art/deep/three.logo"), "FD 5\n");
    temp.write(QStringLiteral("notes.txt"), "not a program\n");
    QJsonObject folder;
    folder.insert(QStringLiteral("path"), temp.path());
    folder.insert(QStringLiteral("group"), QStringLiteral("Examples"));
    folder.insert(QStringLiteral("extensions"),
                  QJsonArray{QStringLiteral(".logo"), QStringLiteral("bas")});
    const QJsonObject imported =
        resultOf(server, QStringLiteral("project.addFolder"), folder).toObject();
    // `one.logo` is already a member, so the folder adds the two new programs.
    TW_CHECK_EQ(imported.value(QStringLiteral("added")).toInt(), 2);
    TW_CHECK_EQ(imported.value(QStringLiteral("files")).toArray().size(), 3);
    bool found_deep = false;
    for (const QJsonValue& value : imported.value(QStringLiteral("files")).toArray()) {
        if (value.toObject().value(QStringLiteral("group")).toString() ==
            QStringLiteral("Examples/Art/deep")) {
            found_deep = true;
        }
    }
    TW_CHECK(found_deep);

    // The entry point is the program Run Project runs, so it is reported -- and it
    // can be set and cleared over the same surface that sets everything else.
    TW_CHECK(added.value(QStringLiteral("entry")).isNull());
    QJsonObject set_entry;
    set_entry.insert(QStringLiteral("path"), file);
    const QJsonObject with_entry =
        resultOf(server, QStringLiteral("project.setEntry"), set_entry).toObject();
    TW_CHECK_EQ(with_entry.value(QStringLiteral("entry")).toString(),
                QFileInfo{file}.absoluteFilePath());
    TW_CHECK_EQ(resultOf(server, QStringLiteral("project.clearEntry"), {}).toObject()
                    .value(QStringLiteral("entry")),
                QJsonValue{});

    // A broken request is an error, not a crash or a silent success.
    QJsonObject missing;
    const QJsonObject response = call(server, QStringLiteral("project.addFile"), missing);
    TW_CHECK_EQ(response.value(QStringLiteral("error"))
                    .toObject()
                    .value(QStringLiteral("code"))
                    .toInt(),
                -32602);
}

void test_watch_and_errors() {
    announce("watch.evaluate and JSON-RPC errors");
    std::istringstream in;
    std::ostringstream out;
    Server server{in, out};

    QJsonObject bindings;
    bindings.insert(QStringLiteral("X"), 5);
    bindings.insert(QStringLiteral("NAME"), QStringLiteral("Ada"));
    QJsonObject params;
    params.insert(QStringLiteral("expressions"),
                  QJsonArray{QStringLiteral("X * 2"), QStringLiteral("NAME"),
                             QStringLiteral("MISSING")});
    params.insert(QStringLiteral("variables"), bindings);
    const QJsonArray watches =
        resultOf(server, QStringLiteral("watch.evaluate"), params).toArray();
    TW_CHECK_EQ(watches.size(), 3);
    TW_CHECK_EQ(watches.at(0).toObject().value(QStringLiteral("value")).toString(),
                QStringLiteral("10"));
    TW_CHECK_EQ(watches.at(1).toObject().value(QStringLiteral("value")).toString(),
                QStringLiteral("Ada"));
    TW_CHECK(!watches.at(2).toObject().value(QStringLiteral("ok")).toBool());

    const QJsonObject unknown = call(server, QStringLiteral("no.such.method"));
    TW_CHECK_EQ(unknown.value(QStringLiteral("error"))
                    .toObject()
                    .value(QStringLiteral("code"))
                    .toInt(),
                -32601);

    const std::string parse = server.handleLine("{not json");
    TW_CHECK(QJsonDocument::fromJson(QByteArray::fromStdString(parse))
                 .object()
                 .value(QStringLiteral("error"))
                 .toObject()
                 .value(QStringLiteral("code"))
                 .toInt() == -32700);

    // A notification gets no response.
    QJsonObject notification = request(QStringLiteral("ping"));
    notification.remove(QStringLiteral("id"));
    TW_CHECK(server.handleLine(lineOf(notification)).empty());

    // A batch is answered with an array in request order, and a notification
    // inside it is omitted rather than answered with null.
    QJsonArray batch;
    batch.append(request(QStringLiteral("ping")));
    batch.append(request(QStringLiteral("no.such.method")));
    batch.append(notification);
    const std::string batch_line =
        QString::fromUtf8(QJsonDocument{batch}.toJson(QJsonDocument::Compact))
            .toStdString();
    const QJsonArray answers = QJsonDocument::fromJson(
                                   QByteArray::fromStdString(server.handleLine(batch_line)))
                                   .array();
    TW_CHECK_EQ(answers.size(), 2);
    TW_CHECK_EQ(answers.at(0).toObject().value(QStringLiteral("result")).toString(),
                QStringLiteral("pong"));
    TW_CHECK_EQ(answers.at(1)
                    .toObject()
                    .value(QStringLiteral("error"))
                    .toObject()
                    .value(QStringLiteral("code"))
                    .toInt(),
                -32601);

    const std::string empty_batch = server.handleLine("[]");
    TW_CHECK_EQ(QJsonDocument::fromJson(QByteArray::fromStdString(empty_batch))
                    .object()
                    .value(QStringLiteral("error"))
                    .toObject()
                    .value(QStringLiteral("code"))
                    .toInt(),
                -32600);
}


void test_debug_sessions() {
    announce("debug.*");
    std::istringstream in;
    std::ostringstream out;
    Server server{in, out};

    QJsonObject start;
    start.insert(QStringLiteral("language"), QStringLiteral("basic"));
    start.insert(QStringLiteral("source"),
                 QStringLiteral("10 LET X = 1\n20 PRINT X\n30 LET X = 2\n"));
    start.insert(QStringLiteral("breakpoints"), QJsonArray{2});
    const QJsonObject paused =
        resultOf(server, QStringLiteral("debug.start"), start).toObject();
    TW_CHECK_EQ(paused.value(QStringLiteral("state")).toString(),
                QStringLiteral("paused"));
    TW_CHECK_EQ(paused.value(QStringLiteral("line")).toInt(), 20);
    TW_CHECK_EQ(paused.value(QStringLiteral("reason")).toString(),
                QStringLiteral("breakpoint"));
    const QString session = paused.value(QStringLiteral("session")).toString();
    TW_CHECK(!session.isEmpty());

    QJsonObject id;
    id.insert(QStringLiteral("session"), session);
    QJsonObject step;
    step.insert(QStringLiteral("session"), session);
    step.insert(QStringLiteral("kind"), QStringLiteral("over"));
    const QJsonObject stepped =
        resultOf(server, QStringLiteral("debug.step"), step).toObject();
    TW_CHECK_EQ(stepped.value(QStringLiteral("state")).toString(),
                QStringLiteral("paused"));
    TW_CHECK_EQ(stepped.value(QStringLiteral("line")).toInt(), 30);
    TW_CHECK_EQ(stepped.value(QStringLiteral("reason")).toString(),
                QStringLiteral("step"));

    const QJsonObject finished =
        resultOf(server, QStringLiteral("debug.continue"), id).toObject();
    TW_CHECK_EQ(finished.value(QStringLiteral("state")).toString(),
                QStringLiteral("finished"));
    TW_CHECK_EQ(finished.value(QStringLiteral("output")).toString(),
                QStringLiteral("1\n"));

    // A data breakpoint reports the variable that changed, before the statement
    // that would print or use the new value.
    QJsonObject data_start;
    data_start.insert(QStringLiteral("language"), QStringLiteral("basic"));
    data_start.insert(QStringLiteral("source"),
                      QStringLiteral("10 LET X = 1\n20 LET X = 2\n30 PRINT X\n"));
    data_start.insert(QStringLiteral("dataBreakpoints"), QJsonArray{QStringLiteral("X")});
    const QJsonObject first =
        resultOf(server, QStringLiteral("debug.start"), data_start).toObject();
    TW_CHECK_EQ(first.value(QStringLiteral("reason")).toString(), QStringLiteral("data"));
    TW_CHECK_EQ(first.value(QStringLiteral("changedVariable")).toString(),
                QStringLiteral("X"));
    TW_CHECK_EQ(first.value(QStringLiteral("line")).toInt(), 10);
    QJsonObject data_session;
    data_session.insert(QStringLiteral("session"),
                        first.value(QStringLiteral("session")).toString());
    const QJsonObject second =
        resultOf(server, QStringLiteral("debug.continue"), data_session).toObject();
    TW_CHECK_EQ(second.value(QStringLiteral("line")).toInt(), 20);
    const QJsonObject last =
        resultOf(server, QStringLiteral("debug.continue"), data_session).toObject();
    TW_CHECK_EQ(last.value(QStringLiteral("state")).toString(),
                QStringLiteral("finished"));
    TW_CHECK_EQ(last.value(QStringLiteral("output")).toString(), QStringLiteral("2\n"));

    // Scheme takes the same path through its own runner.
    QJsonObject scheme;
    scheme.insert(QStringLiteral("language"), QStringLiteral("scheme"));
    scheme.insert(QStringLiteral("source"),
                  QStringLiteral("(define x 1)\n(display x)\n"));
    scheme.insert(QStringLiteral("breakpoints"), QJsonArray{2});
    const QJsonObject scheme_pause =
        resultOf(server, QStringLiteral("debug.start"), scheme).toObject();
    TW_CHECK_EQ(scheme_pause.value(QStringLiteral("state")).toString(),
                QStringLiteral("paused"));
    TW_CHECK_EQ(scheme_pause.value(QStringLiteral("line")).toInt(), 2);
    QJsonObject scheme_id;
    scheme_id.insert(QStringLiteral("session"),
                     scheme_pause.value(QStringLiteral("session")).toString());
    const QJsonObject scheme_end =
        resultOf(server, QStringLiteral("debug.continue"), scheme_id).toObject();
    TW_CHECK_EQ(scheme_end.value(QStringLiteral("state")).toString(),
                QStringLiteral("finished"));
    TW_CHECK_EQ(scheme_end.value(QStringLiteral("reason")).toString(),
                QStringLiteral("finished"));
    TW_CHECK_EQ(scheme_end.value(QStringLiteral("output")).toString(),
                QStringLiteral("1"));

    // A whole-program language reports its own call chain, so a client can say
    // where a stop is rather than only which line it is on.
    QJsonObject smalltalk;
    smalltalk.insert(QStringLiteral("language"), QStringLiteral("smalltalk"));
    smalltalk.insert(QStringLiteral("source"),
                     QStringLiteral("Object subclass: #Thing\n"
                                    "  instanceVariableNames: ''\n"
                                    "\n"
                                    "Thing >> greet\n"
                                    "  ^1 + 2\n"
                                    "\n"
                                    "Thing new greet\n"));
    smalltalk.insert(QStringLiteral("breakpoints"), QJsonArray{5});
    const QJsonObject talk_pause =
        resultOf(server, QStringLiteral("debug.start"), smalltalk).toObject();
    TW_CHECK_EQ(talk_pause.value(QStringLiteral("state")).toString(),
                QStringLiteral("paused"));
    TW_CHECK_EQ(talk_pause.value(QStringLiteral("line")).toInt(), 5);
    const QJsonArray stack = talk_pause.value(QStringLiteral("stack")).toArray();
    TW_CHECK_EQ(stack.size(), 2);
    if (stack.size() == 2) {
        TW_CHECK_EQ(stack.at(0).toObject().value(QStringLiteral("procedure")).toString(),
                    QStringLiteral("DoIt"));
        TW_CHECK_EQ(
            stack.at(1).toObject().value(QStringLiteral("procedure")).toString(),
            QStringLiteral("Thing>>greet"));
        TW_CHECK_EQ(stack.at(1).toObject().value(QStringLiteral("line")).toInt(), 5);
    }
    QJsonObject talk_id;
    talk_id.insert(QStringLiteral("session"),
                   talk_pause.value(QStringLiteral("session")).toString());
    const QJsonObject talk_end =
        resultOf(server, QStringLiteral("debug.continue"), talk_id).toObject();
    TW_CHECK_EQ(talk_end.value(QStringLiteral("state")).toString(),
                QStringLiteral("finished"));

    // Going back: a session started with history can return to a statement it has
    // already run, and the state it lands in is the one it was in then -- which is what
    // "step back and try the other arm" needs.
    QJsonObject rewindable;
    rewindable.insert(QStringLiteral("language"), QStringLiteral("basic"));
    rewindable.insert(QStringLiteral("source"),
                      QStringLiteral("10 LET A = 1\n"
                                     "20 LET A = 2\n"
                                     "30 PRINT A\n"));
    rewindable.insert(QStringLiteral("breakpoints"), QJsonArray{3});
    rewindable.insert(QStringLiteral("history"), 8);
    const QJsonObject at_third =
        resultOf(server, QStringLiteral("debug.start"), rewindable).toObject();
    TW_CHECK_EQ(at_third.value(QStringLiteral("line")).toInt(), 30);
    TW_CHECK_EQ(at_third.value(QStringLiteral("reason")).toString(),
                QStringLiteral("breakpoint"));
    QJsonObject backward;
    backward.insert(QStringLiteral("session"),
                    at_third.value(QStringLiteral("session")).toString());
    backward.insert(QStringLiteral("steps"), 2);
    const QJsonObject gone_back =
        resultOf(server, QStringLiteral("debug.rewind"), backward).toObject();
    TW_CHECK_EQ(gone_back.value(QStringLiteral("reason")).toString(),
                QStringLiteral("rewind"));
    TW_CHECK_EQ(gone_back.value(QStringLiteral("line")).toInt(), 10);
    // The bindings are the ones that statement had not yet changed.
    bool saw_a = false;
    double a = 0.0;
    for (const QJsonValue& value : gone_back.value(QStringLiteral("variables")).toArray()) {
        const QJsonObject variable = value.toObject();
        if (variable.value(QStringLiteral("name")).toString() == QLatin1String("A")) {
            saw_a = true;
            a = variable.value(QStringLiteral("value")).toString().toDouble();
        }
    }
    TW_CHECK(!saw_a || a == 0.0);

    // Pressing back again at the oldest statement the run kept must not move it on:
    // the answer says unmoved and the line is the one it was already at.
    const QJsonObject again =
        resultOf(server, QStringLiteral("debug.rewind"), backward).toObject();
    TW_CHECK(again.value(QStringLiteral("unmoved")).toBool());
    TW_CHECK_EQ(again.value(QStringLiteral("line")).toInt(), 10);
    TW_CHECK(!again.value(QStringLiteral("error")).toString().isEmpty());

    // A whole-program language works the same way, because the position travels in
    // the checkpoint its executor hands to the pause point.
    QJsonObject logo;
    logo.insert(QStringLiteral("language"), QStringLiteral("logo"));
    logo.insert(QStringLiteral("source"),
                QStringLiteral("MAKE \"A 1\nPRINT :A\nMAKE \"A 2\nPRINT :A\n"));
    logo.insert(QStringLiteral("breakpoints"), QJsonArray{4});
    logo.insert(QStringLiteral("history"), 8);
    const QJsonObject logo_pause =
        resultOf(server, QStringLiteral("debug.start"), logo).toObject();
    TW_CHECK_EQ(logo_pause.value(QStringLiteral("line")).toInt(), 4);
    QJsonObject logo_back;
    logo_back.insert(QStringLiteral("session"),
                     logo_pause.value(QStringLiteral("session")).toString());
    logo_back.insert(QStringLiteral("steps"), 2);
    const QJsonObject logo_rewound =
        resultOf(server, QStringLiteral("debug.rewind"), logo_back).toObject();
    TW_CHECK_EQ(logo_rewound.value(QStringLiteral("reason")).toString(),
                QStringLiteral("rewind"));
    TW_CHECK_EQ(logo_rewound.value(QStringLiteral("line")).toInt(), 2);
    // The print that had already happened is gone with the state that made it.
    TW_CHECK(logo_rewound.value(QStringLiteral("output")).toString().isEmpty());

    // CHIP-8 hands over the whole machine rather than a transcript, so the same
    // request moves a run back to an earlier instruction -- the line is the
    // listing's, and the registers and pixels come with the counter.
    QJsonObject chip8;
    chip8.insert(QStringLiteral("language"), QStringLiteral("chip8"));
    chip8.insert(QStringLiteral("source"),
                 QStringLiteral("start: LD V0, 2\n"
                                "       LD I, dot\n"
                                "       DRW V0, V1, 1\n"
                                "       ADD V0, 1\n"
                                "       DRW V0, V1, 1\n"
                                "       HALT\n"
                                "dot:   DB 0xFF\n"));
    chip8.insert(QStringLiteral("breakpoints"), QJsonArray{5});
    chip8.insert(QStringLiteral("history"), 8);
    const QJsonObject chip8_pause =
        resultOf(server, QStringLiteral("debug.start"), chip8).toObject();
    TW_CHECK_EQ(chip8_pause.value(QStringLiteral("line")).toInt(), 5);
    QJsonObject chip8_back;
    chip8_back.insert(QStringLiteral("session"),
                      chip8_pause.value(QStringLiteral("session")).toString());
    chip8_back.insert(QStringLiteral("steps"), 2);
    const QJsonObject chip8_rewound =
        resultOf(server, QStringLiteral("debug.rewind"), chip8_back).toObject();
    TW_CHECK_EQ(chip8_rewound.value(QStringLiteral("reason")).toString(),
                QStringLiteral("rewind"));
    TW_CHECK_EQ(chip8_rewound.value(QStringLiteral("line")).toInt(), 3);

    // A session asked for none keeps none, and says so rather than looking unchanged.
    QJsonObject no_history;
    no_history.insert(QStringLiteral("session"), session);
    const QJsonObject refused =
        resultOf(server, QStringLiteral("debug.rewind"), no_history).toObject();
    TW_CHECK(refused.value(QStringLiteral("unmoved")).toBool());

    // A line-by-line language fills the same field from the frames its statements
    // push, so a client reads one chain shape whatever the engine.
    QJsonObject gosub;
    gosub.insert(QStringLiteral("language"), QStringLiteral("basic"));
    gosub.insert(QStringLiteral("source"),
                 QStringLiteral("10 GOSUB 100\n"
                                "20 PRINT \"END\"\n"
                                "30 END\n"
                                "100 LET X = 1\n"
                                "110 RETURN\n"));
    gosub.insert(QStringLiteral("breakpoints"), QJsonArray{4});
    const QJsonObject gosub_pause =
        resultOf(server, QStringLiteral("debug.start"), gosub).toObject();
    TW_CHECK_EQ(gosub_pause.value(QStringLiteral("line")).toInt(), 100);
    const QJsonArray gosub_stack = gosub_pause.value(QStringLiteral("stack")).toArray();
    TW_CHECK_EQ(gosub_stack.size(), 2);
    if (gosub_stack.size() == 2) {
        TW_CHECK_EQ(gosub_stack.at(0).toObject().value(QStringLiteral("procedure")).toString(),
                    QStringLiteral("PROGRAM"));
        TW_CHECK_EQ(gosub_stack.at(0).toObject().value(QStringLiteral("line")).toInt(), 10);
        TW_CHECK_EQ(gosub_stack.at(1).toObject().value(QStringLiteral("procedure")).toString(),
                    QStringLiteral("GOSUB"));
        TW_CHECK_EQ(gosub_stack.at(1).toObject().value(QStringLiteral("line")).toInt(), 100);
    }
    QJsonObject gosub_id;
    gosub_id.insert(QStringLiteral("session"),
                    gosub_pause.value(QStringLiteral("session")).toString());
    TW_CHECK_EQ(resultOf(server, QStringLiteral("debug.continue"), gosub_id)
                    .toObject()
                    .value(QStringLiteral("state"))
                    .toString(),
                QStringLiteral("finished"));

    QJsonObject missing;
    missing.insert(QStringLiteral("session"), QStringLiteral("no-such"));
    TW_CHECK_EQ(call(server, QStringLiteral("debug.continue"), missing)
                    .value(QStringLiteral("error"))
                    .toObject()
                    .value(QStringLiteral("code"))
                    .toInt(),
                -32000);
}

}  // namespace

void test_assistant_ask_validates_before_it_connects() {
    announce("ai.ask validates and reports an unconfigured assistant");
    std::istringstream in;
    std::ostringstream out;
    Server server{in, out};

    // `studio.info` says whether this build has an assistant at all, so the test
    // asserts the right sentence in either build.
    const QJsonObject features =
        resultOf(server, QStringLiteral("studio.info")).toObject()
            .value(QStringLiteral("features"))
            .toObject();
    TW_CHECK(features.contains(QStringLiteral("assistant")));

    const auto codeOf = [&server](const QJsonObject& params) {
        const QJsonObject response = call(server, QStringLiteral("ai.ask"), params);
        return response.value(QStringLiteral("error"))
            .toObject()
            .value(QStringLiteral("code"))
            .toInt();
    };

    // A missing or empty question is a parameter error, and a history turn that is
    // not a turn is too: the caller learns what to fix rather than being told the
    // assistant is off.
    TW_CHECK_EQ(codeOf(QJsonObject{}), -32602);
    QJsonObject empty;
    empty.insert(QStringLiteral("prompt"), QStringLiteral("   "));
    TW_CHECK_EQ(codeOf(empty), -32602);

    QJsonObject bad_role;
    bad_role.insert(QStringLiteral("prompt"), QStringLiteral("hi"));
    bad_role.insert(QStringLiteral("history"),
                    QJsonArray{QJsonObject{{QStringLiteral("role"), QStringLiteral("system")},
                                           {QStringLiteral("content"), QStringLiteral("x")}}});
    TW_CHECK_EQ(codeOf(bad_role), -32602);

    // With valid parameters and no configuration the assistant reports why it cannot
    // answer -- never "unknown method", so a script can tell the two apart.
    QJsonObject question;
    question.insert(QStringLiteral("prompt"), QStringLiteral("what is a turtle?"));
    TW_CHECK_EQ(codeOf(question), -32000);
}

int main() {
    test_info();
    test_run_and_repl();
    test_sql();
    test_drawing_reports_turtle_actions();
    test_project();
    test_watch_and_errors();
    test_debug_sessions();
    test_assistant_ask_validates_before_it_connects();
    return tw::test::summary("api_tests");
}
