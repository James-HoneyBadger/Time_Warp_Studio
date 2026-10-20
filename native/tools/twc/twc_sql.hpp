// SPDX-License-Identifier: MIT
//
// The SQLite backing a compiled Studio program.
//
// A compiled program that uses `SQL` still needs a database. This is a small
// executor over the SQLite C API -- columns, rows, a change count, and the same
// 1000-row cap the IDE uses -- linked into the generated executable. `--db PATH`
// chooses the file at run time; without it the program uses `TIME_WARP_SQL_DB`,
// and without either it gets a scratch database that lasts for one run.
//
// Only plain SQLite is linked: no Qt, no Studio.

#pragma once

#include <algorithm>
#include <cstdio>
#include <string>
#include <string_view>
#include <vector>

#include <sqlite3.h>

class TwcDatabase {
public:
    TwcDatabase() = default;
    ~TwcDatabase() {
        if (handle_ != nullptr) {
            sqlite3_close(handle_);
        }
    }

    TwcDatabase(const TwcDatabase&) = delete;
    TwcDatabase& operator=(const TwcDatabase&) = delete;

    bool open(const std::string& path, std::string* error) {
        if (handle_ != nullptr) {
            sqlite3_close(handle_);
            handle_ = nullptr;
        }
        const int status = sqlite3_open_v2(path.c_str(), &handle_,
                                           SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE,
                                           nullptr);
        if (status != SQLITE_OK) {
            if (error != nullptr) {
                *error = handle_ != nullptr ? sqlite3_errmsg(handle_) : "cannot open database";
            }
            if (handle_ != nullptr) {
                sqlite3_close(handle_);
                handle_ = nullptr;
            }
            return false;
        }
        sqlite3_busy_timeout(handle_, 3000);
        return true;
    }

    /// Runs every statement and returns what a language's `SQL` statement prints.
    std::string run(std::string_view sql) {
        if (handle_ == nullptr) {
            return "\u274C SQL: no database is open.\n";
        }
        const std::string text{sql};
        const char* tail = text.c_str();
        std::string output;
        int changes = 0;
        while (tail != nullptr && *tail != '\0') {
            sqlite3_stmt* statement = nullptr;
            if (sqlite3_prepare_v2(handle_, tail, -1, &statement, &tail) != SQLITE_OK) {
                output += std::string{"\u274C SQL: "} + sqlite3_errmsg(handle_) + "\n";
                return output;
            }
            if (statement == nullptr) {
                continue;
            }
            const int columns = sqlite3_column_count(statement);
            if (columns > 0) {
                std::string table = render(statement, columns);
                output += table + "\n";
            } else {
                const int status = sqlite3_step(statement);
                if (status != SQLITE_DONE && status != SQLITE_ROW) {
                    output += std::string{"\u274C SQL: "} + sqlite3_errmsg(handle_) + "\n";
                    sqlite3_finalize(statement);
                    return output;
                }
                changes += sqlite3_changes(handle_);
            }
            sqlite3_finalize(statement);
        }
        if (changes > 0) {
            output += "\u2705 " + std::to_string(changes) +
                      (changes == 1 ? " row changed\n" : " rows changed\n");
        } else if (output.empty()) {
            output = "\u2139\uFE0F Statement executed\n";
        }
        return output;
    }

private:
    static std::string cell(sqlite3_stmt* statement, int column) {
        if (sqlite3_column_type(statement, column) == SQLITE_NULL) {
            return "NULL";
        }
        const auto* text =
            reinterpret_cast<const char*>(sqlite3_column_text(statement, column));
        return text != nullptr ? std::string{text} : std::string{};
    }

    static std::string render(sqlite3_stmt* statement, int columns) {
        constexpr std::size_t kMaxRows = 1000;
        std::vector<std::string> names;
        std::vector<std::size_t> widths;
        for (int column = 0; column < columns; ++column) {
            const auto* name = reinterpret_cast<const char*>(
                sqlite3_column_name(statement, column));
            names.emplace_back(name != nullptr ? name : "");
            widths.push_back(names.back().size());
        }

        std::vector<std::vector<std::string>> rows;
        for (;;) {
            const int status = sqlite3_step(statement);
            if (status != SQLITE_ROW) {
                break;
            }
            if (rows.size() >= kMaxRows) {
                break;
            }
            std::vector<std::string> row;
            for (int column = 0; column < columns; ++column) {
                row.push_back(cell(statement, column));
                widths[static_cast<std::size_t>(column)] =
                    std::max(widths[static_cast<std::size_t>(column)],
                             row.back().size());
            }
            rows.push_back(std::move(row));
        }

        const auto line = [&widths](const std::vector<std::string>& values) {
            std::string text;
            for (std::size_t column = 0; column < widths.size(); ++column) {
                if (column > 0) {
                    text += " | ";
                }
                std::string value = column < values.size() ? values[column] : std::string{};
                if (value.size() > widths[column]) {
                    value.resize(widths[column]);
                }
                text += value;
                text.append(widths[column] - value.size(), ' ');
            }
            return text;
        };

        std::string text = line(names) + "\n";
        for (std::size_t column = 0; column < widths.size(); ++column) {
            if (column > 0) {
                text += "-+-";
            }
            text.append(widths[column], '-');
        }
        text += "\n";
        for (const auto& row : rows) {
            text += line(row) + "\n";
        }
        text += "(" + std::to_string(rows.size()) +
                (rows.size() == 1 ? " row)" : " rows)");
        return text;
    }

    sqlite3* handle_ = nullptr;
};
