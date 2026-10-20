// SPDX-License-Identifier: MIT

#include "timewarp/core/qt_compat.hpp"
#include "timewarp/sql/database.hpp"

#include <QByteArray>
#include <QFileInfo>

#include <sqlite3.h>

#include <algorithm>
#include <utility>

namespace timewarp::sql {

namespace {

QString fromUtf8(const char* text) {
    return text == nullptr ? QString{} : QString::fromUtf8(text);
}

QString cellText(sqlite3_stmt* statement, int column) {
    if (sqlite3_column_type(statement, column) == SQLITE_NULL) {
        return QStringLiteral("NULL");
    }
    const auto* text = reinterpret_cast<const char*>(
        sqlite3_column_text(statement, column));
    return fromUtf8(text);
}

QString clipped(const QString& text, int width) {
    if (text.size() <= width) {
        return text;
    }
    if (width <= 1) {
        return text.left(width);
    }
    return text.left(width - 1) + QChar{0x2026};
}

QString renderRows(const QueryResult& result) {
    constexpr int kColumnCap = 48;
    QList<int> widths;
    widths.reserve(result.columns.size());
    for (const QString& column : result.columns) {
        widths.append(std::min(timewarp::core::intLength(column.size()), kColumnCap));
    }
    for (const QStringList& row : result.rows) {
        for (int column = 0; column < widths.size() && column < row.size(); ++column) {
            widths[column] =
                std::max(widths.at(column),
                         std::min(timewarp::core::intLength(row.at(column).size()), kColumnCap));
        }
    }

    const auto line = [&widths](const QStringList& cells) {
        QString text;
        for (int column = 0; column < widths.size(); ++column) {
            if (column > 0) {
                text += QStringLiteral(" | ");
            }
            text += clipped(column < cells.size() ? cells.at(column) : QString{},
                            widths.at(column))
                        .leftJustified(widths.at(column));
        }
        return text;
    };

    QString text = line(result.columns) + QLatin1Char('\n');
    QString rule;
    for (int column = 0; column < widths.size(); ++column) {
        if (column > 0) {
            rule += QStringLiteral("-+-");
        }
        rule += QString(widths.at(column), QLatin1Char('-'));
    }
    text += rule + QLatin1Char('\n');
    for (const QStringList& row : result.rows) {
        text += line(row) + QLatin1Char('\n');
    }
    text += QStringLiteral("(%1 %2%3)")
                .arg(result.rows.size())
                .arg(result.rows.size() == 1 ? QStringLiteral("row") : QStringLiteral("rows"))
                .arg(result.truncated ? QStringLiteral(", truncated") : QString{});
    return text;
}

}  // namespace

struct Database::Impl {
    sqlite3* handle = nullptr;
    QString path;
};

Database::Database() : impl_{std::make_unique<Impl>()} {}

Database::~Database() { close(); }

bool Database::open(const QString& path, QString* error) {
    const std::lock_guard<std::mutex> lock{mutex_};

    // Open the new file before dropping the old connection. A mistyped path must
    // leave the database the user already had open alone, rather than closing it
    // and then failing to open the replacement.
    const QByteArray utf8 = path.toUtf8();
    sqlite3* handle = nullptr;
    const int status = sqlite3_open_v2(
        utf8.constData(), &handle,
        SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE | SQLITE_OPEN_FULLMUTEX, nullptr);
    if (status != SQLITE_OK) {
        const QString message =
            handle != nullptr ? fromUtf8(sqlite3_errmsg(handle))
                              : QStringLiteral("Cannot open %1").arg(path);
        if (handle != nullptr) {
            sqlite3_close(handle);
        }
        if (error != nullptr) {
            *error = message;
        }
        return false;
    }
    // Two writers -- a program run and the Database tab -- should wait for each
    // other rather than fail with SQLITE_BUSY. Three seconds is longer than any
    // statement the IDE issues and short enough that a stuck lock is reported.
    sqlite3_busy_timeout(handle, 3000);
    sqlite3_exec(handle, "PRAGMA foreign_keys = ON", nullptr, nullptr, nullptr);
    if (impl_->handle != nullptr) {
        sqlite3_close(impl_->handle);
    }
    impl_->handle = handle;
    impl_->path = path;
    return true;
}

void Database::close() {
    const std::lock_guard<std::mutex> lock{mutex_};
    if (impl_->handle != nullptr) {
        sqlite3_close(impl_->handle);
        impl_->handle = nullptr;
    }
    impl_->path.clear();
}

bool Database::isOpen() const {
    const std::lock_guard<std::mutex> lock{mutex_};
    return impl_->handle != nullptr;
}

QString Database::path() const {
    const std::lock_guard<std::mutex> lock{mutex_};
    return impl_->path;
}

QueryResult Database::execute(const QString& sql) const {
    QueryResult result;
    const std::lock_guard<std::mutex> lock{mutex_};
    if (impl_->handle == nullptr) {
        result.error = QStringLiteral("No database is open");
        return result;
    }
    if (sql.trimmed().isEmpty()) {
        result.error = QStringLiteral("Nothing to run");
        return result;
    }

    const QByteArray bytes = sql.toUtf8();
    const char* tail = bytes.constData();
    while (tail != nullptr && *tail != '\0') {
        sqlite3_stmt* statement = nullptr;
        const int prepared = sqlite3_prepare_v2(impl_->handle, tail, -1, &statement, &tail);
        if (prepared != SQLITE_OK) {
            result.error = fromUtf8(sqlite3_errmsg(impl_->handle));
            if (statement != nullptr) {
                sqlite3_finalize(statement);
            }
            return result;
        }
        if (statement == nullptr) {
            continue;  // whitespace or a comment between statements
        }

        const int columns = sqlite3_column_count(statement);
        if (columns > 0) {
            QStringList names;
            names.reserve(columns);
            for (int column = 0; column < columns; ++column) {
                names.append(fromUtf8(sqlite3_column_name(statement, column)));
            }
            QList<QStringList> rows;
            int status = SQLITE_ROW;
            while ((status = sqlite3_step(statement)) == SQLITE_ROW) {
                if (rows.size() >= kMaxRows) {
                    result.truncated = true;
                    break;
                }
                QStringList row;
                row.reserve(columns);
                for (int column = 0; column < columns; ++column) {
                    row.append(cellText(statement, column));
                }
                rows.append(std::move(row));
                if (result.truncated) {
                    break;
                }
            }
            result.columns = std::move(names);
            result.rows = std::move(rows);
            if (status != SQLITE_DONE && status != SQLITE_ROW) {
                result.error = fromUtf8(sqlite3_errmsg(impl_->handle));
                sqlite3_finalize(statement);
                return result;
            }
        } else {
            const int status = sqlite3_step(statement);
            if (status != SQLITE_DONE && status != SQLITE_ROW) {
                result.error = fromUtf8(sqlite3_errmsg(impl_->handle));
                sqlite3_finalize(statement);
                return result;
            }
            result.changes += sqlite3_changes(impl_->handle);
        }
        sqlite3_finalize(statement);
    }
    return result;
}

QStringList Database::tables() const {
    QStringList names;
    const QueryResult result =
        execute(QStringLiteral("SELECT name FROM sqlite_master "
                              "WHERE type = 'table' AND name NOT LIKE 'sqlite_%' "
                              "ORDER BY name"));
    if (!result.ok()) {
        return names;
    }
    for (const QStringList& row : result.rows) {
        if (!row.isEmpty()) {
            names.append(row.front());
        }
    }
    return names;
}

QString Database::schema(const QString& table) const {
    const std::lock_guard<std::mutex> lock{mutex_};
    if (impl_->handle == nullptr) {
        return QStringLiteral("\u274C SQL: no database is open");
    }
    sqlite3_stmt* statement = nullptr;
    if (sqlite3_prepare_v2(impl_->handle,
                           "SELECT sql FROM sqlite_master WHERE type = 'table' "
                           "AND name = ?1",
                           -1, &statement, nullptr) != SQLITE_OK) {
        return QStringLiteral("\u274C SQL: %1").arg(fromUtf8(sqlite3_errmsg(impl_->handle)));
    }
    const QByteArray name = table.toUtf8();
    sqlite3_bind_text(statement, 1, name.constData(), -1, SQLITE_TRANSIENT);
    QString text;
    if (sqlite3_step(statement) == SQLITE_ROW) {
        text = fromUtf8(reinterpret_cast<const char*>(
            sqlite3_column_text(statement, 0)));
    }
    sqlite3_finalize(statement);
    if (text.isEmpty()) {
        return QStringLiteral("\u274C SQL: no such table: %1").arg(table);
    }
    return text;
}

std::string Database::executeForOutput(std::string_view sql) {
    const QString statement =
        QString::fromUtf8(sql.data(), static_cast<int>(sql.size()));
    const QueryResult result = execute(statement);
    if (!result.ok()) {
        return std::string{"\u274C SQL: "} + result.error.toStdString() + "\n";
    }
    std::string text;
    if (!result.columns.isEmpty()) {
        text = renderRows(result).toStdString();
        text.push_back('\n');
    }
    if (result.changes > 0) {
        text += "\u2705 " + std::to_string(result.changes) +
                (result.changes == 1 ? " row changed\n" : " rows changed\n");
    } else if (result.columns.isEmpty()) {
        text += "\u2139\uFE0F Statement executed\n";
    }
    return text;
}

}  // namespace timewarp::sql
