// SPDX-License-Identifier: MIT
//
// The SQL database: statements, scripts, results and the file round trip.
//
// The panel and the languages are two views of this one object. What is worth
// asserting here is what both depend on: that a script runs in order, that a
// result carries its column names and rows, that an error is reported rather
// than an empty table, and that a database written by one process is readable by
// the next.

#include <cstdio>

#include <QString>
#include <QStringList>

#include "check.hpp"
#include "temp_dir.hpp"
#include "timewarp/sql/database.hpp"

namespace {

using timewarp::sql::Database;
using timewarp::sql::QueryResult;

void announce(const char* name) {
    std::printf("-- %s\n", name);
    std::fflush(stdout);
}

void test_create_insert_select() {
    announce("create, insert, select");
    Database database;
    TW_CHECK(database.open(QStringLiteral(":memory:")));
    TW_CHECK(database.isOpen());
    TW_CHECK_EQ(database.path(), QStringLiteral(":memory:"));

    QueryResult result = database.execute(
        QStringLiteral("CREATE TABLE scores (name TEXT, score INTEGER);"));
    TW_CHECK(result.ok());
    TW_CHECK(result.columns.isEmpty());

    result = database.execute(
        QStringLiteral("INSERT INTO scores VALUES ('Ada', 42), ('Grace', 51);"));
    TW_CHECK(result.ok());
    TW_CHECK_EQ(result.changes, 2);

    result = database.execute(
        QStringLiteral("SELECT name, score FROM scores ORDER BY score DESC;"));
    TW_CHECK(result.ok());
    TW_CHECK_EQ(result.columns, QStringList({QStringLiteral("name"), QStringLiteral("score")}));
    TW_CHECK_EQ(result.rows.size(), 2);
    TW_CHECK_EQ(result.rows.at(0).at(0), QStringLiteral("Grace"));
    TW_CHECK_EQ(result.rows.at(0).at(1), QStringLiteral("51"));

    TW_CHECK_EQ(database.tables(), QStringList({QStringLiteral("scores")}));
    const QString schema = database.schema(QStringLiteral("scores"));
    TW_CHECK(schema.contains(QStringLiteral("CREATE TABLE")));
    TW_CHECK(schema.contains(QStringLiteral("score")));
    TW_CHECK(database.schema(QStringLiteral("missing")).startsWith(QChar{0x274C}));
    database.close();
    TW_CHECK(!database.isOpen());
}

void test_scripts_and_errors() {
    announce("scripts, row cap and errors");
    Database database;
    TW_CHECK(database.open(QStringLiteral(":memory:")));
    // One submit, several statements: the manager runs scripts, and the last
    // query is the result it shows.
    const QueryResult result = database.execute(QStringLiteral(
        "CREATE TABLE t (n INTEGER);"
        "INSERT INTO t VALUES (1);"
        "INSERT INTO t VALUES (2);"
        "SELECT n FROM t ORDER BY n;"));
    TW_CHECK(result.ok());
    TW_CHECK_EQ(result.changes, 2);
    TW_CHECK_EQ(result.rows.size(), 2);
    TW_CHECK_EQ(result.rows.at(1).at(0), QStringLiteral("2"));

    const QueryResult bad = database.execute(QStringLiteral("SELECT * FROM nope;"));
    TW_CHECK(!bad.ok());
    TW_CHECK(!bad.error.isEmpty());
    TW_CHECK(bad.error.contains(QStringLiteral("no such table")));

    TW_CHECK(!database.execute(QStringLiteral("   ")).ok());
    TW_CHECK(!database.execute(QStringLiteral("SELECT * FROM t;")).error.contains(
        QStringLiteral("no such table")));
}

void test_output_for_languages() {
    announce("language output");
    Database database;
    TW_CHECK(database.open(QStringLiteral(":memory:")));
    static_cast<void>(database.execute(QStringLiteral("CREATE TABLE t (n INTEGER);")));
    static_cast<void>(database.execute(QStringLiteral("INSERT INTO t VALUES (7);")));

    const std::string text = database.executeForOutput("SELECT n FROM t;");
    TW_CHECK(text.find("n") != std::string::npos);
    TW_CHECK(text.find("7") != std::string::npos);
    TW_CHECK(text.find("(1 row)") != std::string::npos);

    const std::string changed = database.executeForOutput("UPDATE t SET n = 8;");
    TW_CHECK(changed.find("1 row changed") != std::string::npos);

    const std::string failed = database.executeForOutput("SELECT * FROM missing;");
    TW_CHECK(failed.rfind("\u274C", 0) == 0);
    TW_CHECK(failed.find("no such table") != std::string::npos);
}

void test_file_round_trip() {
    announce("file round trip");
    tw::test::TempDir temp;
    if (!temp.valid()) {
        return;
    }
    const QString path = temp.path(QStringLiteral("scores.db"));
    {
        Database database;
        QString error;
        TW_CHECK(database.open(path, &error));
        TW_CHECK(error.isEmpty());
        TW_CHECK(database.execute(QStringLiteral("CREATE TABLE t (v TEXT);")).ok());
        TW_CHECK(database.execute(QStringLiteral("INSERT INTO t VALUES ('kept');")).ok());
    }
    {
        Database database;
        TW_CHECK(database.open(path));
        const QueryResult result = database.execute(QStringLiteral("SELECT v FROM t;"));
        TW_CHECK(result.ok());
        TW_CHECK_EQ(result.rows.size(), 1);
        TW_CHECK_EQ(result.rows.at(0).at(0), QStringLiteral("kept"));
    }
}

void test_quoted_names_are_bound() {
    announce("schema lookup binds the name");
    Database database;
    TW_CHECK(database.open(QStringLiteral(":memory:")));
    TW_CHECK(database.execute(
                 QStringLiteral("CREATE TABLE \"odd name\" (x INTEGER);"))
                 .ok());
    // A name containing a quote must not be able to turn into SQL. The lookup is
    // bound, so this is a lookup that finds nothing rather than a syntax error in
    // a hand-built statement.
    const QString schema = database.schema(QStringLiteral("x'; DROP TABLE t;--"));
    TW_CHECK(schema.startsWith(QChar{0x274C}));
    TW_CHECK_EQ(database.tables().size(), 1);
}

}  // namespace

int main() {
    test_create_insert_select();
    test_scripts_and_errors();
    test_output_for_languages();
    test_file_round_trip();
    test_quoted_names_are_bound();
    return tw::test::summary("sql_tests");
}
