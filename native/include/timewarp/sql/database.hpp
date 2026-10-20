// SPDX-License-Identifier: MIT
//
// The Studio's SQL database: one SQLite connection, one lock, one result type.
//
// SQLite is an embedded database, which is the right shape for a teaching IDE:
// a learner's database is a file they can copy, back up or hand in, and there is
// no server to install before the first `SELECT`. `Database` wraps the handful
// of operations the database manager and the languages need, so neither one has
// to know the C API.
//
// The connection is guarded by a mutex because a language statement may run on a
// runner thread while the Database tab is querying the same file. A run and a
// query cannot corrupt each other, and neither blocks the other for longer than
// one statement.

#pragma once

#include <QList>
#include <QString>
#include <QStringList>

#include <memory>
#include <mutex>
#include <string>
#include <string_view>

namespace timewarp::sql {

/// One statement's answer.
struct QueryResult {
    QStringList columns;
    QList<QStringList> rows;
    /// Rows inserted, updated or deleted. SQLite reports it per statement; a
    /// script's totals are summed.
    int changes = 0;
    /// Empty on success. The message is SQLite's own, which names the syntax or
    /// constraint that failed.
    QString error;
    /// True when the result was truncated by the row cap.
    bool truncated = false;

    [[nodiscard]] bool ok() const noexcept { return error.isEmpty(); }
};

class Database {
public:
    /// The most rows one query returns. A `SELECT *` from a table a learner has
    /// just filled is fine; a cartesian join is not allowed to exhaust memory
    /// before the window can paint the first row.
    static constexpr int kMaxRows = 1000;

    Database();
    ~Database();

    Database(const Database&) = delete;
    Database(Database&&) = delete;
    Database& operator=(const Database&) = delete;
    Database& operator=(Database&&) = delete;

    /// Opens, creating the file when it does not exist. `":memory:"` gives a
    /// scratch database. False with `error` filled when the file cannot be
    /// opened; an already-open connection is closed first.
    [[nodiscard]] bool open(const QString& path, QString* error = nullptr);
    void close();
    [[nodiscard]] bool isOpen() const;
    [[nodiscard]] QString path() const;

    /// Runs every statement in `sql`, separated by semicolons. Returns the last
    /// statement that produced rows, with `changes` summed over the script.
    [[nodiscard]] QueryResult execute(const QString& sql) const;

    /// The user tables, in name order.
    [[nodiscard]] QStringList tables() const;
    /// The `CREATE` statement for `table`, or an error string starting with the
    /// ❌ prefix. Bound as a parameter, so a table name is never pasted into SQL.
    [[nodiscard]] QString schema(const QString& table) const;

    /// What a language's `SQL` statement prints: a readable table for a query, a
    /// one-line outcome for a statement that changed rows, and the database's
    /// error for a failure. Always newline-terminated.
    [[nodiscard]] std::string executeForOutput(std::string_view sql);

private:
    struct Impl;
    std::unique_ptr<Impl> impl_;
    mutable std::mutex mutex_;
};

}  // namespace timewarp::sql
